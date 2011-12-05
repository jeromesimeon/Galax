(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: planio_util.ml,v 1.6 2007/07/13 18:24:43 mff Exp $ *)

(* 
   Module: Planio_util
   Description:
     This module contains utilities for query plan parsing and
     serialization.
*)

open Cursor
open Debug
open Error

open Namespace_names
open Namespace_context
open Namespace_symbols

open Occurrence
open Planio_common

open Small_stream_ast
open Streaming_types

open Xquery_algebra_ast

(**********************)
(* Parsing Utilities  *)
(**********************)

let string_id x = x
let int_id x = int_of_string x

let rec parse_nested_substream level st =
  match Stream.peek st with
  | None -> 
      if (level = 0) then (cursor_empty())
      else raise (Query (Algebra_Parsing_Error ("In parse_nested_substream. Stream is empty")))
  | Some event->
      begin
	match event.se_desc  with
	| SAX_endDocument ->
	    if (level = 0) then (cursor_empty())
	    else raise (Query (Algebra_Parsing_Error ("In parse_nested_substream. Premature end-document.")))
	| SAX_endElement  -> 
	    if (level = 0) then (cursor_empty())
	    else
	      (Stream.junk st; cursor_cons event (parse_nested_substream (level-1) st))
   	| SAX_startElement _ -> 
	     (Stream.junk st; cursor_cons event (parse_nested_substream (level+1) st))
	| _ -> 
	    (Stream.junk st; cursor_cons event (parse_nested_substream (level) st))
      end
	
(* Apply a function multiple times to the stream *)
let get_multiple cardinality fn st =
  let rec get_multiple_helper cur accum =
    if (cur <= 0) then
      accum
    else
      get_multiple_helper (cur-1) ((fn st) :: accum)
  in
    List.rev (get_multiple_helper cardinality [])

(* Apply a function multiple times to the stream *)
let get_multiple_cursor cardinality fn st =
  let rec get_multiple_helper cur = 
    if (cur <= 0) then
      (Cursor.cursor_empty())
    else
      Cursor.cursor_cons (fn st) (get_multiple_helper (cur-1))
  in
    get_multiple_helper cardinality 

(* Helper functions for stream parsing *)
let rec start_typed_element_get_name_no_consume (st : Streaming_types.typed_xml_stream)=
  let v = Cursor.cursor_peek st in    
  match v with
  | Some event ->
      begin
	match event.se_desc with 
	  ((SAX_startElement (e, attrs, has, _, eo, et))) ->
	    let e =
	      match !eo with
	      | None -> raise (Query (Algebra_Parsing_Error ("Element hasn't been resolved")))
	      | Some (rel,_,_) -> rel
	    in
	    e, attrs
    (* Comments and characters are ignored *)
	| (SAX_comment _) 
	| (SAX_characters _) ->
	    Cursor.cursor_junk st;
	    start_typed_element_get_name_no_consume st
	| (SAX_endElement ) ->
	    raise (Query (Algebra_Parsing_Error ("Expecting Element start but found end element"))	)
	| _ -> 
	    raise (Query (Algebra_Parsing_Error ("Expecting Element start but did not find it")))
      end
  | _ -> 
      raise (Query (Algebra_Parsing_Error ("Expecting Element start but did not find it")))

let rec start_element_get_name_no_consume st =
  let v = Stream.peek st in    
  match v with
  | Some event ->
      begin
	match event.se_desc with 
	  ((SAX_startElement (_, attrs, has, _, eo, _))) ->
	    let e =
	      match !eo with
	      | None -> raise (Query (Algebra_Parsing_Error ("Element hasn't been resolved")))
	      | Some (rel,_,_) -> rel
	    in
	    e, attrs
    (* Comments and characters are ignored *)
	| (SAX_comment _) 
	| (SAX_characters _) ->
	    Stream.junk st;
	    start_element_get_name_no_consume st
	| (SAX_endElement ) ->
	    raise (Query (Algebra_Parsing_Error ("Expecting Element start but found end element"))	)
	| _ -> 
	    raise (Query (Algebra_Parsing_Error ("Expecting Element start but did not find it")))
      end
  | _ -> 
      raise (Query (Algebra_Parsing_Error ("Expecting Element start but did not find it")))

(* Start element and extract its name from the stream, consume it *)
let start_element_get_name st = 
  let res = start_element_get_name_no_consume st in
    Stream.junk st;
    res

(* Start element, with expected element name *)
let aux_start_element get_fn elem_name st = 
  let name, attrs = 
    try
      get_fn st     
    with Query (Algebra_Parsing_Error p) ->
      raise (Query (Algebra_Parsing_Error ("Named: " ^ (string_of_relem_symbol elem_name) ^ "|" ^ p ))	)
  in
    if not (relem_equal name elem_name) then      
      raise (Query (Algebra_Parsing_Error ("Expecting " ^ (string_of_relem_symbol elem_name) ^ " did not find it." ^
	    " Found: " ^ (string_of_relem_symbol name))))
    else
      attrs

let start_element elem_name st = 
  aux_start_element start_element_get_name elem_name st

let start_typed_element_get_name st = 
    let res = start_typed_element_get_name_no_consume st in
    Cursor.cursor_junk st;
    res

let start_typed_element elem_name st = 
  aux_start_element  start_typed_element_get_name elem_name st

let rec is_element_end st = 
  let v = Stream.peek st in
    match v with
    | Some event ->
	begin
	  match event.se_desc with 
	  | SAX_endElement -> true
	  | (SAX_comment _) 
	  | (SAX_characters _) ->
	      Stream.junk st;
	      is_element_end st
	  | _ -> false
	end
    | _ -> false

(* Assert there is an endtag and consume it *)
let rec consume_end_element (st:sax_event Stream.t) =
  let v = Stream.peek st in
    match v with
    | Some event ->
	begin
	  match event.se_desc with 
	  | (SAX_endElement) -> Stream.junk st
	  | (SAX_comment _) 
	  | (SAX_characters _) ->
	      Stream.junk st;
	      consume_end_element st 
	  | rse -> raise (Query (Algebra_Parsing_Error ("In consume_end_element: Expecting end tag but found "^(Streaming_util.string_of_resolved_sax_event_desc rse))))
	end
    | _ -> raise (Query (Algebra_Parsing_Error ("In consume_end_element: Expecting end tag but stream is empty")))


let rec consume_typed_end_element st = 
  let v = Cursor.cursor_peek st in
    match v with
    | Some event ->
	begin
	  match event.se_desc with 
	  | (SAX_endElement) -> Cursor.cursor_junk st
	  | (SAX_comment _) 
	  | (SAX_characters _) ->
	      Cursor.cursor_junk st;
	      consume_typed_end_element st 
	  | _ -> raise (Query (Algebra_Parsing_Error ("1. Expecting end tag - did not find it")))
	end
    | _ -> raise (Query (Algebra_Parsing_Error ("2. Expecting end tag - did not find it")))


(* Util to check if an optional element is present *)
let rec check_opt_element elem st =
  let relem = relem_symbol elem in
  let rec check_opt_element_helper () =
  let v = Stream.peek st in
    match v with
    | Some event ->
	begin
	  match event.se_desc with 
	  | (SAX_comment _) 
	  | (SAX_characters _) ->
	      Stream.junk st;
	      check_opt_element elem st
	  | ((SAX_startElement (_, attrs, has, _, eo, _))) ->
	      let e =
		match !eo with
		| None -> raise (Query (Algebra_Parsing_Error ("Element hasn't been resolved")))
		| Some (rel,_,_) -> rel
	      in
	      e = relem
	  | _ -> false
	end
      | _ -> false
  in
    check_opt_element_helper ()
(* 
   Consumes the start and end tags of elem_name
   To help make side-effects occur in as few places as possible 
*)
let element_parser elem_name fn error_msg st  =
  try
    let attrs = start_element (relem_symbol elem_name) st in
    let return_value = fn attrs st in
    let _ = consume_end_element st in
    return_value
  with (Query (Algebra_Parsing_Error q)) ->
    raise (Query (Algebra_Parsing_Error (q ^ " | " ^ error_msg)))

(* Attribute Parsing functions *)
(* coerce_fun: string -> 'a, empty_fn: unit -> 'a *)
let rec parse_attr_helper coerce_fn (attr_name:Namespace_symbols.rattr_symbol) empty_fn attr_list =
  let error_fn  () = 
     raise (Query (Algebra_Parsing_Error ("Looking for attribute")))
  in
  match attr_list with
      [] -> empty_fn ()
    | (_, value, special, ao, _) :: rest -> 
	if !special
	then parse_attr_helper coerce_fn attr_name empty_fn rest 
	else
	let aname =
	  match !ao with
	  | None -> error_fn ()
	  | Some rat -> rat
	in
	if (attr_name = aname)
	then coerce_fn value
	else parse_attr_helper coerce_fn attr_name empty_fn rest 

(* coerce_fun: string -> 'a *)
let get_attr_from_attr_list attr_list coerce_fn (a_name:Namespace_symbols.rattr_symbol) =
  let error_fn  () = 
     raise (Query (Algebra_Parsing_Error ("Looking for attribute")))
  in
  parse_attr_helper coerce_fn a_name error_fn attr_list
 
(* coerce_fun: string -> 'a *)
let parse_get_typed_attr_from_attr_list (attr_list:Streaming_types.sax_xml_attribute_forest) coerce_fn (attr_name:Namespace_symbols.rattr_symbol) =
  let error_fn () = 
     raise (Query (Algebra_Parsing_Error ("Looking for attribute")))
  in
  let rec parse_attr_helper coerce_fn attr_name empty_fn attr_list =
    match attr_list with
      [] -> empty_fn ()
    | (_, value, s, ao, _) :: rest -> 
	if !s
	then parse_attr_helper coerce_fn attr_name empty_fn rest 
	else
	  let aname =
	    match !ao with
	    | None -> error_fn ()
	    | Some rat -> rat
	  in
	  if (attr_name = aname)
	  then coerce_fn value
	  else parse_attr_helper coerce_fn attr_name empty_fn rest 
  in parse_attr_helper coerce_fn attr_name error_fn attr_list

let get_opt_attr_from_attr_list attr_list coerce_fn (a_name:Namespace_symbols.rattr_symbol) =
  parse_attr_helper (fun x -> Some (coerce_fn x)) a_name (fun () -> None) attr_list

let construct_element_top name attrs sub_elements =
  SElem(name, Some [], Namespace_context.empty_nsenv, attrs, Dm_atomic_util.default_no_uri_dm, sub_elements, ref None)

let construct_attribute_top attr_name str = (attr_name, str)

let construct_attribute attr_name str =
  let uq = Namespace_names.uqname_of_rqname attr_name in
  (uq,str,ref false,ref None, ref None)

let construct_element name attrs sub_elements =
  let attr_fun (an,ac) =
    let uq = Namespace_names.uqname_of_rqname an in
    (uq,ac,ref false,ref None, ref None)
  in
  let attrs = List.map attr_fun attrs in
  construct_element_top name attrs sub_elements

(* This code is to check when materializations occur in the decorated plan and
   generate a comment appropriately *)
let construct_comment msg x = [SComment(msg ^ x)];;

(****************************)
(* Serialization Utilities  *)
(****************************)

let string_of_occurrence occ =
  match occ with
  | None ->
      "None"
  | Some (l,h) ->
      begin
	match (l,h) with
	| (UP_INT 0, UNBOUNDED) ->
	    "*"
	| (UP_INT 1, UNBOUNDED) ->
	    "+"
	| (UP_INT 0, UP_INT 1) ->
	    "?"
	| _ ->
	    raise (Query (Malformed_Type("Cannot have complex minOccurs and maxOccurs in a Datatype")))
      end

let attribute_of_occurrence occ =
  construct_attribute occurrence_attr_name (string_of_occurrence occ)

(* Symbol (reelem/rattr) parsing functions *)
let box_rtype_symbol rtype = 
  let prefix = prefix_attr_name, (xml_string_of_prefix (rtype_prefix rtype)) in
  let name   = rqname_attr_name, (serializable_string_of_rqname (rtype_name rtype)) in
  let attrs = prefix :: name :: [] in
  construct_element rtype_elem_name attrs [] 

let box_relem_symbol rname = 
  let prefix = prefix_attr_name, (xml_string_of_prefix (relem_prefix rname)) in
  let name   = rqname_attr_name, (serializable_string_of_rqname (relem_name rname)) in
  let attrs = prefix :: name :: [] in
    
    construct_element relem_elem_name attrs [] 

let box_rattr_symbol rname = 
  let prefix = prefix_attr_name, (xml_string_of_prefix (rattr_prefix rname)) in
  let name   = rqname_attr_name, (serializable_string_of_rqname (rattr_name rname)) in
  let attrs  = prefix :: name :: [] in 
    
    construct_element rattr_elem_name attrs [] 

let box_element_test cet =
  match cet with
  | ASchemaElementTest nam ->
      let child1 = box_relem_symbol nam in
      let children = child1 :: [] in
      construct_element ait_schemaelementtype_elem_name [] children
  | AElementTest None ->
      let children = [] in
      construct_element ait_elementtype_elem_name [] children
  | AElementTest (Some (nam,None)) ->
      let child1 = box_relem_symbol nam in
      let children = child1 :: [] in
      construct_element ait_elementtype_elem_name [] children
  | AElementTest (Some (nam, Some typ)) ->
      let child1 = box_relem_symbol nam in
      let child2 = box_rtype_symbol typ in
      let children = child1 :: child2 :: [] in
      construct_element ait_elementtype_elem_name [] children

let box_attribute_test cet =
  match cet with
  | ASchemaAttributeTest nam ->
      let child1 = box_rattr_symbol nam in
      let children = child1 :: [] in
      construct_element ait_schemaattribute_elem_name [] children
  | AAttributeTest None ->
      let children = [] in
      construct_element ait_attributetype_elem_name [] children
  | AAttributeTest (Some (nam,None)) ->
      let child1 = box_rattr_symbol nam in
      let children = child1 :: [] in
      construct_element ait_attributetype_elem_name [] children
  | AAttributeTest (Some (nam, Some typ)) ->
      let child1 = box_rattr_symbol nam in
      let child2 = box_rtype_symbol typ in
      let children = child1 :: child2 :: [] in
      construct_element ait_attributetype_elem_name [] children

let box_kind_test ckt =
  match ckt with
  | AAttributeKind cet ->
      box_attribute_test cet
  | AElementKind cet ->
      box_element_test cet
  | AAnyKind ->
      construct_element ait_node_elem_name [] []
  | ATextKind ->
      construct_element ait_text_elem_name [] []
  | ACommentKind ->
      construct_element ait_comment_elem_name [] []
  | APIKind None ->
      construct_element ait_processing_instruction_elem_name [] []
  | APIKind (Some s) ->
      let attrs = [pi_arg_attr_name, s] in
      construct_element ait_processing_instruction_elem_name attrs []
  | ADocumentKind None ->
      construct_element ait_document_elem_name [] [] 
  | ADocumentKind (Some cet) ->
      let elem = box_element_test cet in
	construct_element ait_document_elem_name [] (elem :: [])


let box_aitemtype cdtk =
  match cdtk with
  | AITKindTest ckt ->
      let elem = box_kind_test ckt in
      construct_element ait_kindtest_elem_name [] (elem :: [])
  | AITTypeRef type_symbol ->
      let elem = box_rtype_symbol type_symbol in
      construct_element ait_typeref_elem_name [] (elem :: [])
  | AITItem ->
      construct_element ait_item_elem_name [] [] 
  | AITNumeric ->
      construct_element ait_numeric_elem_name [] [] 
  | AITAnyString ->
      construct_element ait_anystring_elem_name [] [] 
  | AITEmpty ->
      construct_element ait_empty_elem_name [] [] 
  | AITAtomic type_symbol ->
      let elem = box_rtype_symbol type_symbol in
      construct_element ait_atomic_elem_name [] (elem :: [])

let box_asequencetype cdt =
  let cdtk,occ = cdt.pasequencetype_desc in
  let attrs = [attribute_of_occurrence occ] in
  let elems = [box_aitemtype cdtk] in
  construct_element_top asequencetype_elem_name attrs elems

let box_optasequencetype ocdt = 
  match ocdt with 
      None -> []
    | Some cdt -> [box_asequencetype cdt]

let get_arity_attr attrs arity_attr_name algop_kind =
  match algop_kind with 
    | NoSub_n   -> 0
    | OneSub_n  -> 1
    | TwoSub_n  -> 2
    | ManySub_n -> get_attr_from_attr_list attrs int_of_string arity_attr_name 

let subexpr_attrs attr_name arity_attr_name sub =
  match sub with
      NoSub     -> [(Namespace_names.uqname_of_rqname attr_name, "No", ref false, ref (Some (Namespace_symbols.rattr_symbol attr_name)), ref None)] 
    | OneSub _  -> [(Namespace_names.uqname_of_rqname attr_name, "One", ref false, ref (Some (Namespace_symbols.rattr_symbol attr_name)), ref None)]
    | TwoSub _  -> [(Namespace_names.uqname_of_rqname attr_name, "Two", ref false, ref (Some (Namespace_symbols.rattr_symbol attr_name)), ref None)] 
    | ManySub x -> [(Namespace_names.uqname_of_rqname attr_name, "Many", ref false, ref (Some (Namespace_symbols.rattr_symbol attr_name)), ref None);(Namespace_names.uqname_of_rqname arity_attr_name, string_of_int (Array.length x), ref false, ref (Some (Namespace_symbols.rattr_symbol arity_attr_name)), ref None)] 

let dep_subexpr_attrs sub =
  subexpr_attrs dep_attr_name dep_arity_attr_name sub

let indep_subexpr_attrs sub =
  subexpr_attrs indep_attr_name indep_arity_attr_name sub

let get_dep_subexpr_kind_arity attrs k =
  get_arity_attr attrs (Namespace_symbols.rattr_symbol dep_arity_attr_name) k

let get_indep_subexpr_kind_arity attrs k =
  get_arity_attr attrs (Namespace_symbols.rattr_symbol indep_arity_attr_name) k

