(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: streaming_ops.ml,v 1.26 2007/10/16 01:25:35 mff Exp $ *)

(* Module: Streaming_ops
   Description:
     Some basic operations on XML streams.
*)

open Error

open Namespace_names
open Namespace_symbols
open Namespace_resolve
open Namespace_context

open Xquery_common_ast

open Streaming_types
open Streaming_util


(********************)
(* The empty stream *)
(********************)

let empty_xml_stream          () = Cursor.cursor_of_list []
let empty_resolved_xml_stream () = Cursor.cursor_of_list [] 
let empty_typed_xml_stream    () = Cursor.cursor_of_list []

(***************************************)
(* Validity checks on stream contents  *)
(***************************************)

let check_valid_processing_instruction target pi_content = 
  let target'= Datatypes_util.ncname_of_untyped target in 
  if (String.lowercase(target') = "xml") then 
    raise (Query(Constructor_Error("Invalid processing-instruction target: contains 'xml'")))
  else if (Regularexp.matches pi_content "\\?>" "") then 
    raise (Query(Constructor_Error("Invalid processing-instruction content: contains '?>'")))
  else (target, pi_content)

let check_valid_comment comment_content = 
  if (Regularexp.matches comment_content "(--)|(-$)" "") then 
    raise (Query(Constructor_Error("Invalid comment content: contains '--' or '-' at end of comment")))
  else true

(***********************)
(* Discarding a stream *)
(***********************)

(* Discards the current XML subtree *)

let rec discard_xml_stream xml_stream =
  try
    while true do
      ignore(Cursor.cursor_next xml_stream)
    done
  with
  | Stream.Failure -> ()

(* Discards the current resolved XML subtree *)

let rec discard_resolved_xml_stream resolved_xml_stream =
  try
    while true do
      ignore(Cursor.cursor_next resolved_xml_stream)
    done
  with
  | Stream.Failure -> ()

(* Discards the current typed XML subtree *)

let rec discard_typed_xml_stream typed_xml_stream =
  try
    while true do
      ignore(Cursor.cursor_next typed_xml_stream)
    done
  with
  | Stream.Failure -> ()

 (***********************)
 (* Removing text nodes *)
 (***********************)

 (**
  * Code to merge text nodes in typed SAX streams
  **)
let merge_xml_text_nodes_in_typed_stream typed_stream = 
  let merge_text_nodes () = 
    (* function for merging adjacent text nodes *)
    let rec next_event_discard previous tsa fi =
      match (Cursor.cursor_peek typed_stream) with
      | Some {tse_desc = (TSAX_characters (s))} ->
	  begin
	    ignore(Cursor.cursor_next typed_stream);
	    next_event_discard (previous ^ s) tsa fi
	  end
      | _ ->
	  Some (fmkatse_event (TSAX_characters previous) tsa fi)
    in
    try
      let e = Cursor.cursor_next typed_stream in
      match e.tse_desc with
      | TSAX_characters s  -> next_event_discard s e.tse_annot e.tse_loc
      | _ -> Some e
    with
    | Stream.Failure ->
	None
  in
  Cursor.cursor_of_function merge_text_nodes


 (*******************************************************)
 (* Conversion between well-formed and resolved streams *)
 (*******************************************************)

 (* Turns a well-formed XML stream into an resolved one *)

let resolve_attribute ts_context attribute =
  let (nsenv,in_scope_nsenv) = Resolve_stream_context.get_nsenv ts_context in
  match attribute with
  | (attr_uqname, attr_content) ->
      let rattr_sym =
	Resolve_stream_context.resolve_attribute_name ts_context nsenv attr_uqname
      in
      (rattr_sym,attr_content)

let resolve_attributes ts_context attributes =
  List.map (resolve_attribute ts_context) attributes

let resolved_of_well_formed_event ts_context event =
  match event.se_desc with
  | SAX_startDocument (xmldecl,dtddecl,base_uri) ->
      fmkrse_event (RSAX_startDocument (xmldecl,dtddecl,base_uri)) event.se_loc
  | SAX_endDocument ->
      fmkrse_event RSAX_endDocument event.se_loc
  | SAX_startElement (elem_uqname,sax_attributes,has_element_content) ->
      (* Extract the namespace attributes and update the namespace environment *)
      let (ws_mode, new_nss, base_uri, other_sax_attributes) =
	Streaming_util.extract_special_attributes sax_attributes
      in
      let (nsenv,in_scope_nsenv) =
	begin
	  Resolve_stream_context.push_ns_bindings ts_context new_nss;
	  Resolve_stream_context.get_nsenv ts_context
	end
      in
      (* Resolve the element QName wrt namespaces *)
      let relem_sym,default =
	Resolve_stream_context.resolve_element_name ts_context nsenv elem_uqname
      in
      let in_scope_nsenv =
	if default then patch_bindings in_scope_nsenv Namespace_builtin.default_built_in_namespaces else in_scope_nsenv
      in
      let resolved_sax_xml_attributes =
	resolve_attributes ts_context other_sax_attributes
      in
      (* Checking for attribute duplicates here! *)
      let resolved_sax_xml_attributes =
	Streaming_util.check_duplicate_attributes resolved_sax_xml_attributes
      in
      fmkrse_event (RSAX_startElement (relem_sym,resolved_sax_xml_attributes,has_element_content,ref base_uri,in_scope_nsenv)) event.se_loc
  | SAX_endElement ->
      begin
	let _ = Resolve_stream_context.pop_nsenv ts_context in
	fmkrse_event RSAX_endElement event.se_loc
      end
  | SAX_processingInstruction pi_desc ->
      fmkrse_event (RSAX_processingInstruction pi_desc) event.se_loc
  | SAX_comment comment_desc ->
      fmkrse_event (RSAX_comment comment_desc) event.se_loc
  | SAX_characters text_desc ->
      fmkrse_event (RSAX_characters text_desc) event.se_loc
  | SAX_attribute sax_xml_attribute ->
      let typed_sax_xml_attribute =
	resolve_attribute ts_context sax_xml_attribute
      in
      fmkrse_event (RSAX_attribute typed_sax_xml_attribute) event.se_loc
  | SAX_atomicValue av ->
      fmkrse_event (RSAX_atomicValue av) event.se_loc
  | SAX_hole ->
      fmkrse_event RSAX_hole event.se_loc

let resolve_event_wrap ts_context xml_stream =
  try
    let next_event = Cursor.cursor_next xml_stream in
    Some (resolved_of_well_formed_event ts_context next_event)
  with
  | Stream.Failure ->
      None

let next_event_resolved_of_well_formed_xml_stream ts_context xml_stream n =
  resolve_event_wrap ts_context xml_stream

let resolve_xml_stream xml_stream =
  let ts_context = Resolve_stream_context.build_ts_context () in
  Cursor.cursor_of_function (next_event_resolved_of_well_formed_xml_stream ts_context xml_stream)


(* Turns a resolved XML stream back to one with prefixes *)

let prefix_attribute nsenv (rsym,content) =
  let uqname = rattr_uname nsenv rsym in
  (uqname,content)

let prefix_attributes nsenv resolved_attributes =
  List.map (prefix_attribute nsenv) resolved_attributes

let recreate_ns_binding new_bindings x =
  begin
    match x with
    | (NSDefaultFunctionPrefix,_) | (NSWildcardPrefix,_) | (_,NSWildcardUri) |
      (NSInterfacePrefix _,_) | (NSServerPrefix _,_) -> ()
    | (NSDefaultElementPrefix,NSUri uri) -> new_bindings := ((NSDefaultElementPrefix,"xmlns"),uri) :: !new_bindings
    | (NSPrefix ncname,NSUri uri) ->  new_bindings := ((NSPrefix "xmlns",ncname),uri) :: !new_bindings
  end

let recreate_ns_bindings delta_bindings =
  let new_bindings = ref [] in
  begin
    List.iter (recreate_ns_binding new_bindings) delta_bindings;
    !new_bindings
  end

let prefix_event prefix_context resolved_event =
  match resolved_event.rse_desc with
  | RSAX_startDocument (xmldecl,dtddecl,base_uri) ->
      Some (fmkse_event (SAX_startDocument (xmldecl,dtddecl,base_uri)) resolved_event.rse_loc)
  | RSAX_endDocument ->
      Some (fmkse_event SAX_endDocument resolved_event.rse_loc)
  | RSAX_startElement (rsym,resolved_sax_xml_attributes,has_element_content,base_uri,nsenv) ->
      let ns_bindings = Prefix_context.push_nsenv_in_prefix_context prefix_context nsenv in
      let uqname = relem_uname nsenv rsym in
      let sax_xml_attributes = prefix_attributes nsenv resolved_sax_xml_attributes in
      let ns_attributes = recreate_ns_bindings ns_bindings in
      Some (fmkse_event (SAX_startElement (uqname,ns_attributes @ sax_xml_attributes,has_element_content))  resolved_event.rse_loc)
  | RSAX_endElement ->
      Prefix_context.pop_nsenv_from_prefix_context prefix_context;
      Some (fmkse_event SAX_endElement resolved_event.rse_loc)
  | RSAX_processingInstruction pi_desc ->
      Some (fmkse_event (SAX_processingInstruction pi_desc) resolved_event.rse_loc)
  | RSAX_comment comment_desc ->
      Some (fmkse_event (SAX_comment comment_desc) resolved_event.rse_loc)
  | RSAX_characters text_desc ->
      Some (fmkse_event (SAX_characters text_desc) resolved_event.rse_loc)
  | RSAX_attribute resolved_sax_xml_attribute ->
      (* At the top-level, assume an empty namespace environment *)
      let nsenv = empty_nsenv in
      let sax_xml_attribute =
	prefix_attribute nsenv resolved_sax_xml_attribute
      in
      Some (fmkse_event (SAX_attribute sax_xml_attribute) resolved_event.rse_loc)
  | RSAX_atomicValue av ->
      Some (fmkse_event (SAX_atomicValue av) resolved_event.rse_loc)
  | RSAX_hole ->
      Some (fmkse_event SAX_hole resolved_event.rse_loc)

let rec prefix_xml_stream_next_event prefix_context  resolved_xml_stream =
   try
     let next_event = Cursor.cursor_next resolved_xml_stream in
     match prefix_event prefix_context next_event with
     | None ->
	 prefix_xml_stream_next_event prefix_context resolved_xml_stream
     | Some event ->
	 Some event
   with
   | Stream.Failure ->
       None

let next_event_for_prefix_xml_stream prefix_context resolved_xml_stream n =
  prefix_xml_stream_next_event prefix_context resolved_xml_stream

let prefix_xml_stream resolved_xml_stream =
  let prefix_context = Prefix_context.build_prefix_context () in
  Cursor.cursor_of_function (next_event_for_prefix_xml_stream prefix_context resolved_xml_stream)


 (*************************************************)
 (* Conversion between resolved and typed streams *)
 (*************************************************)

 (* Turns an untyped XML stream into an typed one *)

 (* Note:
      The following adds xs:untyped as type annotations for
      elements, and xs:untypedAtomic for attributes.
    - Jerome *)

let type_attribute attribute =
  match attribute with
  | (rattr_sym, attr_content) ->
      (rattr_sym,attr_content,untypedAtomicsym,[])

let type_attributes attributes =
  List.map type_attribute attributes

let typed_of_resolved_event event =
  match event.rse_desc with
  | RSAX_startDocument (xmldecl,dtddecl,base_uri) ->
      fmktse_event (TSAX_startDocument (xmldecl,dtddecl,base_uri)) event.rse_loc
  | RSAX_endDocument ->
      fmktse_event TSAX_endDocument event.rse_loc
  | RSAX_startElement (relem_sym,sax_attributes,has_element_content,base_uri,nsenv) ->
      let typed_sax_xml_attributes =
	type_attributes sax_attributes
      in
      fmktse_event (TSAX_startElement (relem_sym,typed_sax_xml_attributes,has_element_content,base_uri,nsenv,false,untypedsym,[])) event.rse_loc
  | RSAX_endElement ->
      fmktse_event TSAX_endElement event.rse_loc
  | RSAX_processingInstruction pi_desc ->
      fmktse_event (TSAX_processingInstruction pi_desc) event.rse_loc
  | RSAX_comment comment_desc ->
      fmktse_event (TSAX_comment comment_desc) event.rse_loc
  | RSAX_characters text_desc ->
      fmktse_event (TSAX_characters text_desc) event.rse_loc
  | RSAX_attribute sax_xml_attribute ->
      let typed_sax_xml_attribute =
	type_attribute sax_xml_attribute
      in
      fmktse_event (TSAX_attribute typed_sax_xml_attribute) event.rse_loc
  | RSAX_atomicValue av ->
      fmktse_event (TSAX_atomicValue av) event.rse_loc
  | RSAX_hole ->
      fmktse_event TSAX_hole event.rse_loc

let typed_event_wrap xml_stream =
  try
    let next_event = Cursor.cursor_next xml_stream in
    Some (typed_of_resolved_event next_event)
  with
  | Stream.Failure ->
      None

let next_event_typed_of_resolved_xml_stream xml_stream n =
  typed_event_wrap xml_stream

let typed_of_resolved_xml_stream xml_stream =
  let s1 =
    Cursor.cursor_of_function (next_event_typed_of_resolved_xml_stream xml_stream)
  in
  merge_xml_text_nodes_in_typed_stream s1

 (* Turns a typed XML stream into a resolved, untyped one *)

let erase_attribute (rsym,content,_,_) =
  (rsym,content)

let erase_attributes typed_attributes =
  List.map erase_attribute typed_attributes

let erase_event typed_event =
  match typed_event.tse_desc with
  | TSAX_startDocument (xmldecl,dtddecl,base_uri) ->
      Some (fmkrse_event (RSAX_startDocument (xmldecl,dtddecl,base_uri)) typed_event.tse_loc)
  | TSAX_endDocument ->
      Some (fmkrse_event (RSAX_endDocument) typed_event.tse_loc)
  | TSAX_startElement (rsym,typed_sax_xml_attributes,has_element_content,base_uri,nsenv,_,_,_) ->
      let sax_xml_attributes =
	erase_attributes typed_sax_xml_attributes
      in
      Some (fmkrse_event (RSAX_startElement (rsym,sax_xml_attributes,has_element_content,base_uri,nsenv)) typed_event.tse_loc)
  | TSAX_endElement ->
      Some (fmkrse_event (RSAX_endElement) typed_event.tse_loc)
  | TSAX_processingInstruction pi_desc ->
      Some (fmkrse_event (RSAX_processingInstruction pi_desc) typed_event.tse_loc)
  | TSAX_comment comment_desc ->
      Some (fmkrse_event (RSAX_comment comment_desc) typed_event.tse_loc)
  | TSAX_characters text_desc ->
      Some (fmkrse_event (RSAX_characters text_desc) typed_event.tse_loc)
  | TSAX_attribute typed_sax_xml_attribute ->
      let sax_xml_attribute =
	erase_attribute typed_sax_xml_attribute
      in
      Some (fmkrse_event (RSAX_attribute sax_xml_attribute) typed_event.tse_loc)
  | TSAX_atomicValue av ->
      Some (fmkrse_event (RSAX_atomicValue av) typed_event.tse_loc)
  | TSAX_hole ->
      Some (fmkrse_event (RSAX_hole) typed_event.tse_loc)
  | TSAX_startEncl 
  | TSAX_endEncl ->
      None

let rec erase_xml_stream_next_event typed_xml_stream =
  try
    let next_event = Cursor.cursor_next typed_xml_stream in
    match erase_event next_event with
    | None ->
	erase_xml_stream_next_event typed_xml_stream
    | Some event ->
	Some event
  with
  | Stream.Failure ->
      None

let next_event_for_erase_xml_stream typed_xml_stream n =
  erase_xml_stream_next_event typed_xml_stream

let erase_xml_stream typed_xml_stream =
  Cursor.cursor_of_function (next_event_for_erase_xml_stream typed_xml_stream)

 (* Turns a typed XML stream into a resolved, non-typed one, but also
    turns atomic values into text nodes according to the semantics in
    Section 3.7.1 of the XQuery 1.0 document, and rejects attribute
    events, since they should have been processed before-hand from the
    beginning of the stream. *)

let erase_atomic_event_section_3_7_1 typed_xml_stream av0 =
  let rec erase_atomic_event_section_3_7_1_aux typed_xml_stream =
    let typed_event = (Cursor.cursor_peek typed_xml_stream) in
    match typed_event with
      Some annot_tse ->
	begin
	  match annot_tse.tse_desc with
	    (TSAX_atomicValue av1) ->
	      begin
		ignore(Cursor.cursor_next typed_xml_stream);
		let avl = erase_atomic_event_section_3_7_1_aux typed_xml_stream in
		(av1 :: avl)
	      end
		(* Start/end enclosed expr are sentinels to end concatenation
		   of atomic values.  They are always consumed. *)
	  | (TSAX_startEncl) ->
	      raise (Query(Stream_Error("Start enclosed event cannot follow atomic value event\n")))
	  | (TSAX_endEncl) ->
	      ignore(Cursor.cursor_next typed_xml_stream);
	      []
	  | _ ->
	      []
	end
    | _ ->
	[]
  in
  let avl = erase_atomic_event_section_3_7_1_aux typed_xml_stream in
  fmkrse_event (RSAX_characters (Dm_atomic_util.erase_simple_value (av0 :: avl))) Finfo.bogus

let rec erase_event_section_3_7_1 typed_xml_stream typed_event =
  match typed_event.tse_desc with
  | TSAX_startDocument (xmldecl,dtddecl,base_uri) ->
      fmkrse_event (RSAX_startDocument (xmldecl,dtddecl,base_uri)) typed_event.tse_loc
  | TSAX_endDocument ->
      fmkrse_event (RSAX_endDocument) typed_event.tse_loc
  | TSAX_startElement (rsym,typed_sax_xml_attributes,has_element_content,base_uri,nsenv,_,_,_) ->
      let sax_xml_attributes = erase_attributes typed_sax_xml_attributes in
      fmkrse_event (RSAX_startElement (rsym,sax_xml_attributes,has_element_content,base_uri,nsenv)) typed_event.tse_loc
  | TSAX_endElement ->
      fmkrse_event (RSAX_endElement) typed_event.tse_loc
  | TSAX_processingInstruction pi_desc ->
      fmkrse_event (RSAX_processingInstruction pi_desc) typed_event.tse_loc
  | TSAX_comment comment_desc ->
      fmkrse_event (RSAX_comment comment_desc) typed_event.tse_loc
	(* From 3.7.1
	   Adjacent text nodes in the content sequence are merged into a
	   single text node by concatenating their contents, with no
	   intervening blanks. After concatenation, any text node whose
	   content is a zero-length string is deleted from the content
	   sequence.
	 *)
  | TSAX_characters text_desc ->
      if (text_desc = "") then 
	let next_event = Cursor.cursor_next typed_xml_stream in
	(erase_event_section_3_7_1 typed_xml_stream next_event)
      else
	fmkrse_event (RSAX_characters text_desc) typed_event.tse_loc
  | TSAX_attribute typed_sax_xml_attribute ->
      let sax_xml_attribute = erase_attribute typed_sax_xml_attribute in
      fmkrse_event (RSAX_attribute sax_xml_attribute) typed_event.tse_loc
  | TSAX_atomicValue av ->
      erase_atomic_event_section_3_7_1 typed_xml_stream av 
  | TSAX_hole ->
      fmkrse_event (RSAX_hole) typed_event.tse_loc
  | TSAX_startEncl ->
      let next_event = Cursor.cursor_next typed_xml_stream in
      (erase_event_section_3_7_1 typed_xml_stream next_event)
  | TSAX_endEncl ->
      let next_event = Cursor.cursor_next typed_xml_stream in
      (erase_event_section_3_7_1 typed_xml_stream next_event)

let erase_xml_stream_next_event_section_3_7_1 typed_xml_stream =
  try
    let next_event = Cursor.cursor_next typed_xml_stream in
    (Some (erase_event_section_3_7_1 typed_xml_stream next_event))
  with
  | Stream.Failure ->
      None

let next_event_for_erase_xml_stream_section_3_7_1 typed_xml_stream n =
  erase_xml_stream_next_event_section_3_7_1 typed_xml_stream

let erase_xml_stream_section_3_7_1 typed_xml_stream =
  Cursor.cursor_of_function (next_event_for_erase_xml_stream_section_3_7_1 typed_xml_stream)


(**********************************************)
(* Conversion between typed and ordered typed *)
(**********************************************)

let order_attribute streaming_ordered_context typed_sax_xml_attribute =
  let attributeid =
    Streaming_ordered_context.new_leaf_docorder streaming_ordered_context
  in
  (typed_sax_xml_attribute, attributeid)

let order_start_element streaming_ordered_context typed_element_desc : ordered_typed_element_desc =
  match typed_element_desc with
  | (relem_symbol,
     typed_sax_xml_attribute_forest,
     has_element_content,
     base_uri,
     nsenv,
     nilled_flag,
     type_annotation,
     simple_type_value) ->
       let typed_ordered_sax_xml_attribute_forest =
	 List.map (order_attribute streaming_ordered_context) typed_sax_xml_attribute_forest
       in
       (relem_symbol,
	typed_ordered_sax_xml_attribute_forest,
	has_element_content,
	base_uri,
	nsenv,
	nilled_flag,
	type_annotation,
	simple_type_value)

let ordered_event_of_typed_event streaming_ordered_context next_event =
  let next_ordered_event_desc =
    match next_event.tse_desc with
    | TSAX_startDocument document_desc ->
	let startdocid =
	  Streaming_ordered_context.new_preorderid streaming_ordered_context
	in
	OTSAX_startDocument (document_desc,startdocid)
    | TSAX_endDocument ->
	let enddocid =
	  Streaming_ordered_context.new_postorderid streaming_ordered_context
	in
	OTSAX_endDocument enddocid 
    | TSAX_startElement typed_element_desc ->
	let startelementid =
	  Streaming_ordered_context.new_preorderid streaming_ordered_context
	in
	let typed_ordered_element_desc =
	  order_start_element streaming_ordered_context typed_element_desc
	in
	OTSAX_startElement (typed_ordered_element_desc, startelementid)
    | TSAX_endElement ->
	let endelementid =
	  Streaming_ordered_context.new_postorderid streaming_ordered_context
	in
	OTSAX_endElement endelementid
    | TSAX_processingInstruction pi_desc ->
	let piid =
	  Streaming_ordered_context.new_leaf_docorder streaming_ordered_context
	in
	OTSAX_processingInstruction (pi_desc,piid)
    | TSAX_comment comment_desc ->
	let commentid =
	  Streaming_ordered_context.new_leaf_docorder streaming_ordered_context
	in
	OTSAX_comment (comment_desc,commentid)
    | TSAX_characters text_desc ->
	let textid =
	  Streaming_ordered_context.new_leaf_docorder streaming_ordered_context
	in
	OTSAX_characters (text_desc,textid)
	  (* Additions to the standard SAX events: *)
    | TSAX_attribute typed_sax_xml_attribute ->
	let tsxa =
	  order_attribute streaming_ordered_context typed_sax_xml_attribute
	in
	OTSAX_attribute tsxa
    | TSAX_atomicValue av ->
	OTSAX_atomicValue av
    | TSAX_hole ->
	raise (Query (Stream_Error "Cannot create node identifier in a stream with a [hole]")) 
    | TSAX_startEncl
    | TSAX_endEncl ->
	raise (Query (Stream_Error "1: Cannot create a node identified in an XML stream with start/end enclosed expression"))
  in
  fmkotse_event next_ordered_event_desc next_event.tse_annot next_event.tse_loc

let next_ordered_typed_event streaming_ordered_context typed_xml_stream =
  try
    let next_event = Cursor.cursor_next typed_xml_stream in
    (Some (ordered_event_of_typed_event streaming_ordered_context next_event))
  with
  | Stream.Failure ->
      None

let next_ordered_typed_event_token streaming_ordered_context typed_xml_stream n =
  next_ordered_typed_event streaming_ordered_context typed_xml_stream

let ordered_typed_of_typed_stream_for_docid docid nodeid_context typed_xml_stream =
  let streaming_ordered_context =
    Streaming_ordered_context.build_streaming_ordered_context docid nodeid_context
  in
  Cursor.cursor_of_function (next_ordered_typed_event_token streaming_ordered_context typed_xml_stream)

let ordered_typed_of_typed_stream docid_gen nodeid_context typed_xml_stream =
  let docid = Nodeid.new_docid docid_gen in
  ordered_typed_of_typed_stream_for_docid docid nodeid_context typed_xml_stream


(**********************)
(* Stream composition *)
(**********************)

(* Composes a well-formed stream with holes with a list of well-formed streams *)

let rec next_event_of_compose_xml_streams s0 current_top additional_streams =
  match !current_top with
  | None ->
      begin
	try
	  let event = (Cursor.cursor_next s0) in
	  match event.se_desc with
	  | SAX_hole ->
	      begin
		match !additional_streams with
		| [] ->
		    raise (Query (Stream_Error "Stream composition: less streams than holes in the main stream"))
		| s1 :: sl ->
		    begin
		      current_top := Some s1;
		      additional_streams := sl;
		      next_event_of_compose_xml_streams s0 current_top additional_streams
		    end
	      end
	  | _ ->
	      Some event
	with
	| Stream.Failure ->
	    begin
	      match !additional_streams with
	      | [] ->
		  None
	      | _ ->
		  raise (Query (Stream_Error "Stream composition: more streams than holes in the main stream"))
	    end
      end
  | Some s ->
      try
	Some (Cursor.cursor_next s)
      with
      |	Stream.Failure ->
	  begin
	    (* Switch back to the top-level stream *)
	    current_top := None;
	    next_event_of_compose_xml_streams s0 current_top additional_streams
	  end

let next_event_of_compose_xml_streams_aux s0 current_top additional_streams n =
  next_event_of_compose_xml_streams s0 current_top additional_streams

let compose_xml_streams s0 sl =
  let current_top = ref None in
  let additional_streams = ref sl in
  Cursor.cursor_of_function (next_event_of_compose_xml_streams_aux s0 current_top additional_streams)


(* Composes a resolved stream with holes with a list of resolved streams *)

let rec next_event_of_compose_resolved_xml_streams s0 current_top additional_streams =
  match !current_top with
  | None ->
      begin
	try
	  let event = (Cursor.cursor_next s0) in
	  match event.rse_desc with
	  | RSAX_hole ->
	      begin
		match !additional_streams with
		| [] ->
		    raise (Query (Stream_Error "Stream composition: less streams than holes in the main stream"))
		| s1 :: sl ->
		    begin
		      current_top := Some s1;
		      additional_streams := sl;
		      next_event_of_compose_resolved_xml_streams s0 current_top additional_streams
		    end
	      end
	  | _ ->
	      Some event
	with
	| Stream.Failure ->
	    begin
	      match !additional_streams with
	      | [] ->
		  None
	      | _ ->
		  raise (Query (Stream_Error "Stream composition: more streams than holes in the main stream"))
	    end
      end
  | Some s ->
      try
	Some (Cursor.cursor_next s)
      with
      |	Stream.Failure ->
	  begin
	    (* Switch back to the top-level stream *)
	    current_top := None;
	    next_event_of_compose_resolved_xml_streams s0 current_top additional_streams
	  end

let next_event_of_compose_resolved_xml_streams_aux s0 current_top add_streams n =
  next_event_of_compose_resolved_xml_streams s0 current_top add_streams

let compose_resolved_xml_streams s0 sl =
  let current_top = ref None in
  let additional_streams = ref sl in
  Cursor.cursor_of_function (next_event_of_compose_resolved_xml_streams_aux s0 current_top additional_streams)

    (* Composes a typed stream with holes with a list of typed streams *)

let rec next_event_of_compose_typed_xml_streams s0 current_top additional_streams =
  match !current_top with
  | None ->
      begin
	try
	  let event = (Cursor.cursor_next s0) in
	  match event.tse_desc with
	  | TSAX_hole ->
	      begin
		match !additional_streams with
		| [] ->
		    raise (Query (Stream_Error "Stream composition: less streams than holes in the main stream"))
		| s1 :: sl ->
		    begin
		      current_top := Some s1;
		      additional_streams := sl;
		      
		      next_event_of_compose_typed_xml_streams s0 current_top additional_streams
		     end
	       end
	   | _ ->
	       Some event
	 with
	 | Stream.Failure ->
	     begin
	       match !additional_streams with
	       | [] ->
		   None
	       | s::l -> 
		   raise (Query (Stream_Error "4: Stream composition: more streams than holes in the main stream"))
	     end
       end
   | Some s ->
       try
	  Some (Cursor.cursor_next s)
       with
       |	Stream.Failure ->
	   begin
	     (* Switch back to the top-level stream *)
	     current_top := None; 
	     next_event_of_compose_typed_xml_streams s0 current_top additional_streams
	   end

 let next_event_of_compose_typed_xml_streams_aux s0 current_top additional_streams n =
   next_event_of_compose_typed_xml_streams s0 current_top additional_streams

 let compose_typed_xml_streams s0 sl =
   let current_top = ref None in
   let additional_streams = ref sl in
   Cursor.cursor_of_function (next_event_of_compose_typed_xml_streams_aux s0 current_top additional_streams)

 (* Builds a typed XML stream with just one SAX comment event. *)

 let rec simple_value_of_xml_stream input_stream =
   String.concat "" (simple_value_of_xml_stream_aux input_stream)
 and simple_value_of_xml_stream_aux input_stream =
   match Cursor.cursor_peek input_stream with
   | None -> []
   | Some { tse_desc = TSAX_atomicValue av; tse_annot = _; tse_loc = _ } -> 
       begin
	 Cursor.cursor_junk input_stream;
	 (av#string_value()) :: 
	 ((match Cursor.cursor_peek input_stream with 
	 | Some ({ tse_desc = TSAX_atomicValue _; tse_annot = _; tse_loc = _ }) -> " "
	 | _ -> "") :: 
	  (simple_value_of_xml_stream_aux input_stream))
       end
   | Some { tse_desc = TSAX_startEncl ; tse_annot = _; tse_loc = _ } 
   | Some { tse_desc = TSAX_endEncl ; tse_annot = _; tse_loc = _ } ->
       Cursor.cursor_junk input_stream;
       (simple_value_of_xml_stream_aux input_stream)
   | _ ->
       raise (Query (Stream_Error "Not a stream of atomic values"))


 (* Builds an XML stream with just one TSAX_attribute event, from a
    stream of atomic values, adding one character whitespace between
    each atomic value. *)

 let resolved_xml_stream_of_attribute sym text =
   Cursor.cursor_of_list [fmkrse_event(RSAX_attribute(sym,text)) Finfo.bogus]

(***************************)
(* Simple stream accessors *)
(***************************)

(* Returns true is the stream is empty *)

let is_empty_xml_stream xml_stream =
  match Cursor.cursor_peek xml_stream with
  | None -> true
  | Some _ -> false


(* Returns true is the resolved stream is empty *)

let is_empty_resolved_xml_stream xml_stream =
  match Cursor.cursor_peek xml_stream with
  | None -> true
  | Some _ -> false


(* Returns true is the typed stream is empty *)

let is_empty_typed_xml_stream xml_stream =
  match Cursor.cursor_peek xml_stream with
  | None -> true
  | Some _ -> false


(* Return all of the leading attributes in the stream *)

let rec consume_leading_attribute_events resolved_xml_stream =
  match (Cursor.cursor_peek resolved_xml_stream) with
  | Some { rse_desc = RSAX_attribute attribute_desc; rse_loc = _; } ->
      begin
	ignore(Cursor.cursor_next resolved_xml_stream);
	let next_attributes = consume_leading_attribute_events resolved_xml_stream in
	attribute_desc :: next_attributes
      end
  | _ ->
      []
