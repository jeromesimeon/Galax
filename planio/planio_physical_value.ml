(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: planio_physical_value.ml,v 1.6 2007/10/16 01:25:35 mff Exp $ *)

(* Module: Planio_physical_value
   Description:
     This module parses and prints physical values.
*)

open Cursor
open Debug
open Error

open Namespace_names
open Namespace_context
open Namespace_symbols 

open Datatypes

open Dm_atomic
open Dm
open Dm_util

open Physical_xml_value
open Print_common
open Planio_common
open Planio_util

open Small_stream_ast
open Streaming_constructors
open Streaming_types

(******************)
(* Item Sequences *)
(******************)

(*------------*)
(*-- Boxing --*)
(*------------*)

(* Given an item sequence, produce a boxed, typed XML stream, in which
   only the top-level atomic values are boxed.  I.e., atomic values
   nested within elements are not boxed. *)

let rec aux_box_item_sequence_stream nsenv level st =
  match cursor_peek st with
  | None -> (cursor_junk st; cursor_empty())
  | Some typed_event ->
      begin
	cursor_junk st; 
	let (level', typed_events') = 
	  match typed_event.tse_desc with
	  | TSAX_atomicValue av ->
	      (level, 
	       if (level = 1) then 
		 let type_sym = elem_sym_of_atomic_type (av#getAtomicValueKind()) in
		 element_constructor Dm_atomic_util.default_no_uri_dm type_sym nsenv (Cursor.cursor_of_singleton typed_event)
	       else
		 cursor_of_singleton typed_event)
	  | TSAX_attribute _ ->
	      (level, 
	       if (level = 1) then 
		 element_constructor Dm_atomic_util.default_no_uri_dm closure_attribute_sym nsenv 
		   (Cursor.cursor_of_singleton typed_event)
	       else
		 cursor_of_singleton typed_event)
	  | TSAX_startElement _ -> (level+1, cursor_of_singleton typed_event)
	  | TSAX_endElement -> (level-1, cursor_of_singleton typed_event)
	  | _ -> (level, cursor_of_singleton typed_event)
	in
	cursor_append typed_events' (aux_box_item_sequence_stream nsenv level' st)
      end

(*------------*)
(*-- Unbox  --*)
(*------------*)

(* Typed XML Cursor ->  Typed XML Cursor *)
let rec aux_unbox_item_sequence_stream level match_level st = 
  match cursor_peek st with
  | None -> cursor_empty()
  | Some typed_event ->
      begin
	cursor_junk st; 
	let (level', typed_event') = 
	  match typed_event.tse_desc with
	  | TSAX_startElement (rsym,typed_sax_xml_attributes,has_element_content,base_uri,nsenv,_,_,_) -> 
	      begin
		Debug.print_dxq_debug(Format.sprintf "Unbox %s at level %d\n" (symbol_prefix_string rsym) level);
		if (level = match_level) then 
		  try 
		    (* Unbox an atomic-value *)
		    let (rtype_sym, at) = atomic_type_of_elem_sym rsym in 
		    match (cursor_next st).tse_desc with
		    | TSAX_characters text_desc ->
			begin
		    (* Instead of unboxing by hand, we should use schema validation...*)
			  consume_typed_end_element st;
			  Debug.print_dxq_debug("Cast "^text_desc^"\n");
			  let bv = new atomicUntyped text_desc in
			  let av = bv#cast_to nsenv rtype_sym at in 
			  (level, Streaming_util.fmktse_event (TSAX_atomicValue av) typed_event.tse_loc)
			end
		    | _ -> raise (Query(Internal_Error("Expected character data in boxed XML event")))
		  with
		  | Not_found -> 
		      begin
		    (* Unbox an attribute *)
			if (Namespace_symbols.symbol_equal rsym closure_attribute_sym) then 
			  let typed_event' = 
			    (Streaming_util.fmktse_event (TSAX_attribute(List.hd typed_sax_xml_attributes)) Finfo.bogus) in
			  consume_typed_end_element st;
			  (level, typed_event') 
			else
			  (level+1, typed_event)
		      end
		else 
		  (level+1, typed_event)
	      end
	  | TSAX_endElement -> (level-1, typed_event)
	  | _ -> (level, typed_event)
	in
	cursor_cons typed_event' (aux_unbox_item_sequence_stream level' match_level st)
      end

(*  Typed XML Cursor -> Typed XML Cursor *)
let unbox_and_load_item_sequence boxed_xml_stream = 
  try 
    let unboxed_xml_stream = aux_unbox_item_sequence_stream 1 1 boxed_xml_stream in 
    let nodeid_context = Nodeid_context.default_nodeid_context () in
    Physical_load.load_xml_value_from_typed_stream nodeid_context unboxed_xml_stream
  with
  | Query(Cursor_Error(msg)) -> raise (Query(DXQ_Error("In unbox_and_load_item_sequence: "^msg)))

(******************)
(* Physical Trees *)
(******************)

(* Resolved XML Cursor -> Resolved XML Stream -> Typed XML Cursor -> XML Value *)
let unbox_tree_result pc boxed_xml_stream = 
  try 
    let boxed_xml_stream = Cursor.stream_of_cursor boxed_xml_stream in
    (* Skip document node *)
    Stream.junk boxed_xml_stream;
    let (e, attrs) = start_element_get_name_no_consume boxed_xml_stream in 
    (* <alg:Result> *)
    if (Namespace_symbols.symbol_equal e closure_result_sym) then 
      begin
	Stream.junk boxed_xml_stream;  (* junk <alg:Result> *)
        (* <alg:Tree> *)
	Debug.print_dxq_debug("In aux_unbox_tree_value: <alg:Tree/>\n");
	let _ = start_element closure_tree_sym boxed_xml_stream in 
	let value_stream = Streaming_ops.typed_of_resolved_xml_stream(parse_nested_substream 0 boxed_xml_stream) in 
	let item_list = unbox_and_load_item_sequence value_stream in 
	consume_end_element boxed_xml_stream; (* </alg:Tree> *)
	consume_end_element boxed_xml_stream; (* </alg:Result> *)
	Stream.junk boxed_xml_stream; (* endDocument *)
	Physical_xml_value.xml_value_of_item_list item_list
      end
    (* <alg:Error> *)
    else if (Namespace_symbols.symbol_equal e closure_error_sym) then
      begin
	Debug.print_dxq_debug("In aux_unbox_tree_value: <a:Error/>\n");
	let boxed_xml_stream' = (parse_nested_substream 0 boxed_xml_stream) in
	let msg = Serialization.bserialize_resolved_xml_stream pc boxed_xml_stream' in  
	Debug.print_dxq_debug("Error is "^msg^"\n");
	raise (Query(DXQ_Error(msg)))
      end
    else 
      raise (Query(DXQ_Error("In aux_unbox_tree_value: Expected <alg:Result/> or <alg:Error/>.  Found "^
			     (Namespace_symbols.symbol_prefix_string e))))

  with
  | Query(Cursor_Error(msg)) -> raise (Query(DXQ_Error("In aux_unbox_tree_value: "^msg)))

(*
  <alg:Result><alg:Tree>Boxed value</alg:Tree><alg:Result/>     
*)
let box_tree_result xt xv = 
  let nsenv = Namespace_context.add_all_ns Namespace_context.empty_nsenv [(algebra_prefix, algebra_uri)]  in 
  print_dxq_debug("Before typed_xml_stream_of_datamodel\n");
  let typed_xml_stream = sax_value_of_xml_value xv in
  let tree_stream = 
    print_dxq_debug("Before box_event_stream\n");
    element_constructor Dm_atomic_util.default_no_uri_dm closure_tree_sym nsenv 
      (aux_box_item_sequence_stream nsenv 1 typed_xml_stream)
  in
  element_constructor Dm_atomic_util.default_no_uri_dm closure_result_sym nsenv tree_stream

(*********************)
(* Var/Value Binding *)
(*********************)
(*
   <alg:Bind Var="vname"> boxed_value </alg:Bind>
*)
let box_var_value nsenv (var, xml_value) =
  let items = Physical_xml_value.item_cursor_of_xml_value xml_value in 
  let typed_stream = Physical_export.typed_xml_stream_of_datamodel items in 
  let boxed_stream = aux_box_item_sequence_stream nsenv 1 typed_stream in
  let var_name_attr = attribute_constructor closure_var_sym 
      (Cursor.cursor_of_singleton 
	 (Streaming_util.fmktse_event (TSAX_atomicValue (new atomicString (serializable_string_of_rqname var))) Finfo.bogus))
  in
  (element_constructor Dm_atomic_util.default_no_uri_dm 
       closure_bind_sym nsenv (Cursor.cursor_append var_name_attr boxed_stream)) 

let unbox_var_value st = 
  let attrs = start_element (closure_bind_sym) st in
print_dxq_debug ("Saw <Bind>\n");
  let var = get_attr_from_attr_list attrs (parse_rqname_string) closure_var_elem_name in 
  let value_stream = parse_nested_substream 0 st in 
  (* Do we do validation of value stream here? *)
  let _ = consume_end_element st in 
print_dxq_debug ("Saw </Bind>\n");
  (var, value_stream)

(*
   Binding of N'th tuple field to boxed value: 

   <alg:N> boxed_value </alg:N>
*)
(*
 <alg:Result>	  
  <alg:Table cardinality="size-of-table" arity="width-of-tuple">
   <alg:Schema><alg:Field Name="field-name" Idx="integer"/></alg:Schema>
   <alg:Tup><alg:1> boxed_value </alg:1>...<alg:N> boxed_value </alg:N></alg:Tup><alg:Tup> boxed tuple </alg:Tup>...
  </alg:Table>	  
 <alg:Result/>   
*)
(*******************)
(* Physical Tables *)
(*******************)
(*
  This is a very heavy-weight representation of tables...

 <alg:Result>	  
  <alg:Table cardinality="size-of-table" arity="width-of-tuple">	  
   <alg:Tup> boxed tuple </alg:Tup><alg:Tup> boxed tuple </alg:Tup>...
  </alg:Table>	  
 <alg:Result/>   
*)
let box_table_result field_types tv = 
  let nsenv = Namespace_context.add_all_ns Namespace_context.empty_nsenv [(algebra_prefix, algebra_uri)]  in 
  let fields = List.map fst field_types in 
  let arity = (List.length field_types) in 
  let arity_attr = attribute_constructor (rtype_symbol arity_attr_name) 
      (Cursor.cursor_of_singleton 
	 (Streaming_util.fmktse_event 
	    (TSAX_atomicValue (new atomicInteger (Decimal._integer_of_int arity))) Finfo.bogus))
  in

  let tuple_list = Cursor.list_of_cursor "box_physical_table_value" tv in 
  let cardinality = (List.length tuple_list) in 
  let cardinality_attr = attribute_constructor (rtype_symbol arg_count_attr_name) 
      (Cursor.cursor_of_singleton 
	 (Streaming_util.fmktse_event 
	    (TSAX_atomicValue (new atomicInteger (Decimal._integer_of_int cardinality))) Finfo.bogus))
  in 
  let table_stream = 
    Cursor.cursor_list_fold
      (List.map
	 (fun tup -> 
	   let tuple_field_pairs = List.combine fields (Array.to_list tup) in
	   element_constructor Dm_atomic_util.default_no_uri_dm closure_tuple_sym nsenv 
	     (Cursor.cursor_list_fold (List.map (box_var_value nsenv) tuple_field_pairs))) tuple_list)
  in
  let table_stream = 
    element_constructor Dm_atomic_util.default_no_uri_dm closure_table_sym nsenv
      (Cursor.cursor_append arity_attr (Cursor.cursor_append cardinality_attr table_stream))
  in
  element_constructor Dm_atomic_util.default_no_uri_dm closure_result_sym nsenv table_stream

(*
  This is a very heavy-weight representation of tables...

 <alg:Result>	  
  <alg:Table cardinality="size-of-table" arity="width-of-tuple">	  
   <alg:Tup> boxed tuple </alg:Tup><alg:Tup> boxed tuple </alg:Tup>...
  </alg:Table>	  
 <alg:Result/>   
*)

(*-----------*)
(*-- Parse --*)
(*-----------*)

let tuple_parser arity st = 
print_dxq_debug ("In tuple_parser\n");
  let _ = start_element (closure_tuple_sym) st in
  let tuple_value_pairs = (get_multiple arity (unbox_var_value) st) in 
  let tuple_value_pairs = Array.of_list(List.map 
			     (fun (v,rs) -> 
			       (Physical_xml_value.xml_value_of_item_list(unbox_and_load_item_sequence(Streaming_ops.typed_of_resolved_xml_stream rs)))) tuple_value_pairs) in
  let _ = consume_end_element st in 
  tuple_value_pairs

let parse_serialized_table st = 
  let attrs = start_element closure_table_sym st in 
  let arity = get_attr_from_attr_list attrs (int_of_string) arity_attr_name in 
  let cardinality = get_attr_from_attr_list attrs (int_of_string) arg_count_attr_name in 
  let tuple_cursor = get_multiple_cursor cardinality (tuple_parser arity) st in
  consume_end_element st;
  tuple_cursor 

(* Resolved XML Cursor -> Resolved XML Stream -> ??? -> Typed XML Cursor -> XML Value *)
let unbox_table_result pc phys_tuple_type boxed_xml_stream = 
  try 
    let boxed_xml_stream = Cursor.stream_of_cursor boxed_xml_stream in
    (* Skip document node *)
    Stream.junk boxed_xml_stream;
    (* <alg:Result> *)
    let (e, attrs) = start_element_get_name_no_consume boxed_xml_stream in 
    if (Namespace_symbols.symbol_equal e closure_result_sym) then 
      begin
	Stream.junk boxed_xml_stream;
	let tuple_cursor = parse_serialized_table boxed_xml_stream in 
	Stream.junk boxed_xml_stream; (* </alg:Result> *)
	Stream.junk boxed_xml_stream; (* endDocument *)
	tuple_cursor
      end
    (* <alg:Error> *)
    else if (Namespace_symbols.symbol_equal e closure_error_sym) then
      begin
	Debug.print_dxq_debug("In aux_unbox_table_value: <alg:Error/>");
	let boxed_xml_stream = Cursor.cursor_of_stream boxed_xml_stream in
	let msg = Serialization.bserialize_resolved_xml_stream pc boxed_xml_stream in 
	raise (Query(DXQ_Error(msg)))
      end
    else 
      raise (Query(DXQ_Error("In aux_unbox_table_value: Expected <alg:Result/> or <alg:Error/>.  Found "^
			     (Namespace_symbols.symbol_prefix_string e))))

  with
  | Query(Cursor_Error(msg)) -> raise (Query(DXQ_Error("In aux_unbox_table_value: "^msg)))

