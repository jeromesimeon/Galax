(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_validation.ml,v 1.32 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_validation
   Description:
     This module implements XML Schema validation directly on a SAX
     stream.
*)

open Error

open Xquery_common_ast
open Xquery_type_core_ast

open Streaming_types

open Dm_atomic

open Schema_builtin
open Schema_util
open Schema_judge

open Schema_validation_context


(******************)
(* Simple content *)
(******************)

let validate_atomic_kind valid_ctxt ctname content =
  let cxschema = get_cxschema valid_ctxt in
  let dt = atomic_type_of_typename cxschema ctname in 
  try
    let nsenv = get_namespace_env valid_ctxt in
    let bv = new atomicUntyped content in
    bv#cast_to nsenv ctname dt
  with
  | exn ->
      let dts = Datatypes_util.string_of_atomic_type dt in
      raise (Query (Validation ("Cannot validate content \"" ^ content ^ "\" against atomic type " ^ dts)))

let rec validate_union_kind valid_ctxt dtl content =
  match dtl with
  | [] ->
      raise (Query (Validation ("None of the atomic types in union validates content \"" ^ content ^ "\"")))
  | dt :: dtl ->
      begin
	try
	  validate_atomic_kind valid_ctxt dt content
	with
	| _ ->
	    validate_union_kind valid_ctxt dtl content
      end

let validate_simple_kind valid_ctxt simple_kind content =
  match simple_kind with
  | AtomicKind dt ->
      [validate_atomic_kind valid_ctxt dt content]
  | UnionKind dtl ->
      [validate_union_kind valid_ctxt dtl content]
  | ListKind dtl ->
      begin
	let whitespace_separated_content_items = Whitespace.whitespace_separate content in
	try
	  match dtl with
	  | [dt] ->
	      List.map (validate_atomic_kind valid_ctxt dt) whitespace_separated_content_items
	  | _ ->
	      List.map (validate_union_kind valid_ctxt dtl) whitespace_separated_content_items
	with
	| Query (Validation msg) ->
	    raise (Query (Validation ("Cannot validate content \"" ^ content ^ "\" against list type [" ^ msg ^"]")))
	| _ ->
	    raise (Query (Validation ("Cannot validate content \"" ^ content ^ "\" against list type")))
      end

let rec gather_simple_content valid_ctxt buffer =
  let event =
    try
      next_validation_event valid_ctxt
    with
    | Stream.Failure ->
	raise (Query (Stream_Error "SAX Stream finished in the middle of a simple content element"))
  in
  try
    match event.rse_desc with
    | RSAX_startDocument _
    | RSAX_endDocument ->
	raise (Query (Schema_Internal "Stream 'start' in simple type"))
    | RSAX_startElement _ ->
	raise (Query (Validation "Opening element within an element with simple content"))
    | RSAX_endElement ->
	(Streaming_util.fmkrse_event RSAX_endElement event.rse_loc) :: []
    | RSAX_processingInstruction _
    | RSAX_comment _ ->
	let following_events = gather_simple_content valid_ctxt buffer in
	event :: following_events
    | RSAX_characters text_desc ->
	begin
	  Buffer.add_string buffer text_desc;
	  let following_events = gather_simple_content valid_ctxt buffer in
	  event :: following_events
	end
    | RSAX_attribute _ ->
	raise (Query (Schema_Internal "'attribute' event in element with simple content"))
    | RSAX_atomicValue _ ->
	raise (Query (Schema_Internal "'atomic value' event in element with simple content"))
    | RSAX_hole ->
	raise (Query (Schema_Internal "'hole' event in simple type"))
  with
    exn -> raise(error_with_file_location event.rse_loc exn)

let process_simple_element valid_ctxt simple_kind =
  let buffer = Buffer.create 10 in
  begin
    let buffered_events = gather_simple_content valid_ctxt buffer in
    let content = Buffer.contents buffer in
    let atomic_value = validate_simple_kind valid_ctxt simple_kind content in
    (buffered_events,atomic_value)
  end

(**********************)
(* Special attributes *)
(**********************)

let rec find_attribute caname sax_attributes =
  match sax_attributes with
  | [] ->
      None
  | (rattr_sym,content) :: sax_attributes' ->
      if Namespace_symbols.symbol_equal rattr_sym caname
      then
	Some content
      else
	find_attribute caname sax_attributes'

let extract_special_attributes valid_ctxt sax_attributes =
  let xsi_nil_attribute =
    match find_attribute Namespace_symbols_builtin.xsi_nil sax_attributes with
    | None -> false
    | Some content ->
	let b = validate_atomic_kind valid_ctxt Namespace_symbols_builtin.xs_boolean (* Datatypes.ATBoolean *) content in
	b#getAtomicBoolean()
  in
  let xsi_type_attribute =
    match find_attribute Namespace_symbols_builtin.xsi_type sax_attributes with
    | None -> None
    | Some content ->
	let qname = validate_atomic_kind valid_ctxt Namespace_symbols_builtin.xs_QName (* Datatypes.ATQName *) content in
	Some (qname#getAtomicQName())
  in
  (xsi_nil_attribute,xsi_type_attribute)

(**************)
(* Attributes *)
(**************)

let rec validate_attributes valid_ctxt cxtype sax_attributes =
  let cxschema = get_cxschema valid_ctxt in
  match sax_attributes with
  | [] ->
      if type_contains_empty cxtype
      then
	[]
      else
	raise (Query (Validation "Missing attributes in element"))
  | (rattr_sym,content) :: sax_attributes' ->
      let (sibling_cxtype,type_annotation,simple_kind) = attribute_transition_final cxschema cxtype rattr_sym in
      let simple_value = validate_simple_kind valid_ctxt simple_kind content in
      let new_attribute = (rattr_sym,content,type_annotation,simple_value) in
      new_attribute :: (validate_attributes valid_ctxt sibling_cxtype sax_attributes')

(***********)
(* Element *)
(***********)

(* has element content ? *)

(* Note:
     The element has element content iff its original event has
     element content or its content model is not mixed.

     In case the element has element content, the final data model
     will not have text nodes after validation (i.e., whitespace only
     text nodes will have been removed, or validation will have failed
     if some non whitespace only text nodes are encountered.

   - Jerome
 *)

let compute_has_element_content valid_ctxt cmixed has_element_content =
  if has_element_content
  then
    true
  else
    match cmixed with
    | Mixed -> true
    | NonMixed -> true


(***********************)
(* Validate each event *)
(***********************)

let rec validate_event valid_ctxt event =
  try
    match event.rse_desc with
    | RSAX_startDocument (xmldecl,dtddecl,base_uri) ->
	begin
	  push_document_event valid_ctxt;
	  Some (Streaming_util.fmktse_event(TSAX_startDocument (xmldecl,dtddecl,base_uri)) event.rse_loc)
	end
    | RSAX_endDocument ->
	begin
	  pop_document_event valid_ctxt;
	  Some (Streaming_util.fmktse_event(TSAX_endDocument) event.rse_loc)
	end
    | RSAX_startElement (relem_sym,sax_attributes,has_element_content,baseuri,nsenv) ->
	let _ = push_nsenv valid_ctxt nsenv in
	let cxschema = get_cxschema valid_ctxt in
	let cxtype = get_current_content_model valid_ctxt in
	let (xsi_nil_attribute,xsi_type_attribute) = extract_special_attributes valid_ctxt sax_attributes in
	let (sibling_cxtype,type_annotation,cmixed,cxtype_attribute,cxtype_kind,nilled) = 
	  element_transition_final cxschema cxtype relem_sym xsi_nil_attribute xsi_type_attribute in
	let validated_sax_xml_attributes = validate_attributes valid_ctxt cxtype_attribute sax_attributes in
	let validated_has_element_content = compute_has_element_content valid_ctxt cmixed has_element_content in
	begin
	  let simple_value =
	    match cxtype_kind with
	    | ComplexKind children_cxtype ->
		begin
		  push_complex_element_event
		    valid_ctxt
		    sibling_cxtype
		    cmixed
		    xsi_nil_attribute
		    children_cxtype;
		  [] (* For complex content, there is no simple value attached to the element - Jerome *)
		end
	    | SimpleKind simple_kind ->
		begin
		  let (buffered_events,simple_value) = process_simple_element valid_ctxt simple_kind in
		  begin
		    push_simple_element_event valid_ctxt sibling_cxtype buffered_events;
		    simple_value
		  end
		end
	  in
	  Some (Streaming_util.fmktse_event(TSAX_startElement (relem_sym,validated_sax_xml_attributes,validated_has_element_content,baseuri,nsenv,nilled,type_annotation,simple_value)) event.rse_loc)
	end
    | RSAX_endElement ->
	begin
	  pop_element_event valid_ctxt;
	  Some (Streaming_util.fmktse_event(TSAX_endElement) event.rse_loc)
	end
    | RSAX_processingInstruction pi_desc ->
	Some (Streaming_util.fmktse_event(TSAX_processingInstruction pi_desc) event.rse_loc)
    | RSAX_comment comment_desc ->
	Some (Streaming_util.fmktse_event(TSAX_comment comment_desc) event.rse_loc)
    | RSAX_characters text_desc ->
	if has_been_nilled valid_ctxt
	then
	  raise (Query (Validation ("Text \"" ^ text_desc^ "\" appeared in an element which has been nilled")))
	else
	  if has_mixed_content valid_ctxt
	  then
	    Some (Streaming_util.fmktse_event(TSAX_characters text_desc) event.rse_loc)
	  else
	    if (Whitespace.whitespace_only text_desc)
	    then
	      None
	    else
	      raise (Query (Validation "Text appeared in an element which does not have mixed content"))
    | RSAX_attribute sax_xml_attribute ->
	let (rattr, _) = sax_xml_attribute in 
	raise (Query (Schema ("Cannot validate a stand-alone attribute: "^(Namespace_symbols.rattr_string rattr))))
    | RSAX_atomicValue av ->
	Some (Streaming_util.fmktse_event(TSAX_atomicValue av) event.rse_loc)
    | RSAX_hole ->
	raise (Query (Schema_Internal "Cannot validate an incomplete stream (hole found)"))
  with
    exn -> raise(error_with_file_location event.rse_loc exn)

(************************************)
(* Wrapping of the top-level stream *)
(************************************)

let rec validate_event_wrap valid_ctxt =
  try
    let next_validated_event =
      validate_event valid_ctxt (next_validation_event valid_ctxt)
    in
    begin
      match next_validated_event with
      | Some event -> Some event
      | None ->
	  validate_event_wrap valid_ctxt
    end
  with
  | Stream.Failure ->
      None

let next_event_validate valid_ctxt n =
  validate_event_wrap valid_ctxt


(********)
(* Main *)
(********)

let validate cxschema xml_stream =
  let nsenv = Namespace_context.default_xml_nsenv in
  let valid_ctxt = build_validation_context nsenv cxschema xml_stream in
  Cursor.cursor_of_function (next_event_validate valid_ctxt)
