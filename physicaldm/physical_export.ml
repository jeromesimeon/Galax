(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_export.ml,v 1.7 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_export
   Description:
     This module exports a data model instance into an XML stream.
*)

open Error

open Namespace_symbols

open Cursor

open Dm_types
open Dm_atomic
open Dm

open Physical_value
open Physical_item
open Physical_export_context


(*********************************************)
(* Build a stream from a data model instance *)
(*********************************************)

(* Note:
     The following function turns a data model instance back into an
     XML stream.
   - Jerome *)

let typed_sax_attribute_of_attribute_node attribute =
  let attr_name    = attribute#attrName() in
  let attr_content = attribute#string_value() in
  let attr_value   = attribute#export_typed_value() in
  let attr_type_annotation = attribute#node_type() in
  (attr_name,attr_content,attr_type_annotation,attr_value)

let typed_sax_attributes_of_attribute_nodes attributes =
  List.map typed_sax_attribute_of_attribute_node attributes

let next_datamodel_event export_context =
  let current_cursor = get_current_cursor export_context in
  if cursor_is_empty current_cursor
  then
    pop_node_from_export_context export_context
  else
    (* Note:
	   WARNING: the following call now has a side effect on the
	   current cursor.
	 - Jerome
       *)
    let item = cursor_next current_cursor in
    begin
      match item_kind item with
      | NodeKind ->
	  let node = getNode item in
	  begin
	    match node#node_kind() with
	    | DocumentNodeKind ->
		let dnode = node#getDocumentNode() in
		begin
		  let doc_children = (dnode#children None) in
		  let doc_base_uri = (dnode#base_uri()) in
		  let sax_doc_children =
		    cursor_map (fun x -> Item_Node x) doc_children
		  in
                  push_node_to_export_context export_context node sax_doc_children;
		  Some (Streaming_util.mktse_event(Streaming_types.TSAX_startDocument (None,None, doc_base_uri)))
		end
	    | ElementNodeKind ->
		let enode = node#getElementNode() in
		begin
		  let elem_name = enode#elemName() in
		  let elem_children = enode#children None in
		  let elem_base_uri = (enode#base_uri()) in
		  
		  (* Note:

  		       Here we use export_typed_value, which only
  		       returns the original PSVI information on the
  		       typed value. Notably, it does not apply
  		       string_value, etc.

                       This is ok since, when doing export, we do not
                       want to add redundant information to the typed
                       stream. We just need the typed value if it is
                       really resulting from previous XML Schema
                       validation.

		     - Jerome
		     *)
		  let elem_value = 
		    try
		      enode#export_typed_value()
		    with
		    | _ -> []
		  in
		  let elem_type_annotation = enode#node_type() in
		  let sax_elem_children =
		    cursor_map (fun x -> Item_Node x) elem_children
		  in

		  let elem_attributes = list_of_cursor "Dm_export.elem_attributes" (enode#attributes None) in
		  let sax_elem_attributes =
		    typed_sax_attributes_of_attribute_nodes elem_attributes
		  in
		  let nsenv = enode#namespace_environment() in
		  let has_element_content = enode#has_element_content() in
		  let nilled_flag = enode#nilled() in
		  let sax_elem =
		    (elem_name,sax_elem_attributes,has_element_content,elem_base_uri,nsenv,nilled_flag,elem_type_annotation,elem_value)
		  in
		  push_node_to_export_context export_context node sax_elem_children;
		  Some (Streaming_util.mktse_event (Streaming_types.TSAX_startElement sax_elem))
		end
	    | AttributeNodeKind ->
		let anode = node#getAttributeNode() in
		let sax_attribute = typed_sax_attribute_of_attribute_node anode in
		Some (Streaming_util.mktse_event(Streaming_types.TSAX_attribute sax_attribute))
	    | TextNodeKind ->
		let tnode = node#getTextNode() in
		let sax_text = tnode#string_value() in
		Some (Streaming_util.mktse_event(Streaming_types.TSAX_characters sax_text))
	    | ProcessingInstructionNodeKind ->
		let pinode = node#getProcessingInstructionNode() in
		let pi_target = (pinode#target())#getAtomicString() in
		let pi_value  = (pinode#content())#getAtomicString() in
		Some (Streaming_util.mktse_event(Streaming_types.TSAX_processingInstruction (pi_target,pi_value)))
	    | CommentNodeKind ->
		let cnode = node#getCommentNode() in
		let comment_value = cnode#string_value() in
		Some (Streaming_util.mktse_event(Streaming_types.TSAX_comment comment_value))
	  end
      | AtomicValueKind ->
	  let atomic = getAtomicValue item in
	  Some (Streaming_util.mktse_event(Streaming_types.TSAX_atomicValue atomic))
    end

let next_event export_context n =
  next_datamodel_event export_context

let typed_xml_stream_of_datamodel dmv =
  incr Conf.countexpo;
  let export_context = build_export_context dmv in
  (Cursor.cursor_of_function (next_event export_context))

let is_already_wrapped_result (dmv : item cursor) =
  if not(cursor_is_singleton dmv)
  then
    false
  else
    let item = cursor_get_singleton dmv in
    match item_kind item with
    | AtomicValueKind -> false
    | NodeKind ->
	begin
	  let node = getNode item in
	  match node#node_kind() with
	  | DocumentNodeKind -> true
	  | ElementNodeKind -> true
	  | _ -> false
	end

let resolved_xml_stream_of_datamodel dmv =
  incr Conf.countexpo;
  (* 1. Export the data model *)
  let typed_xml_stream = typed_xml_stream_of_datamodel dmv in
  (* 2. Erase the stream *)
  Streaming_ops.erase_xml_stream typed_xml_stream

let resolved_wrapped_xml_stream_of_datamodel dmv =
  incr Conf.countexpo;
  let typed_xml_stream =
   (* Two cases here. *)
    if (is_already_wrapped_result dmv)
    then
      (* First Case: the result is a single element or document node *)
      (* 1. Export the data model as a stream *)
      typed_xml_stream_of_datamodel dmv
    else
      (* Second Case: the result must be wrapped into an 'glx:result' element *)
      (* 1. Export the data model as a stream *)
      let typed_xml_stream = typed_xml_stream_of_datamodel dmv in
      (* 2. Wrap the stream in the glx:result element *)
      Streaming_constructors.glx_result_serialization typed_xml_stream
  in
  (* 2. Erase the stream *)
  Streaming_ops.erase_xml_stream typed_xml_stream

