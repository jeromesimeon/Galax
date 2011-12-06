(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: streaming_constructors.ml,v 1.19 2007/08/17 18:28:54 simeon Exp $ *)

(* Module: Stream_constructors
   Description:
     Construction operations over XML streams.
*)

open Error

open Namespace_names
open Namespace_symbols
open Namespace_resolve
open Namespace_context

open Streaming_types
open Streaming_util

open Small_stream_ast

(* 

   untyped_value_of_xml_stream is the physical implementation of
   fs:item-sequence-to-untypedAtomic().

   It constructs an untyped value from a stream of atomic values,
   adding one character whitespace between each atomic value.  

   It is used in the implementation of attribute, comment,
   processing-instruction, and text nodes.
*)
let rec untyped_value_of_xml_stream input_stream =
  String.concat "" (untyped_value_of_xml_stream_aux input_stream)
and untyped_value_of_xml_stream_aux input_stream =
  match Cursor.cursor_peek input_stream with
  | None -> []
  | Some { se_desc = SAX_atomicValue av; se_annot = _; se_loc = _ } -> 
      begin
	Cursor.cursor_junk input_stream;
	(av#string_value()) :: 
	((match Cursor.cursor_peek input_stream with 
	| Some ({ se_desc = SAX_atomicValue _; se_annot = _; se_loc = _ }) -> " "
	| _ -> "") :: 
	 (untyped_value_of_xml_stream_aux input_stream))
      end
  | Some { se_desc = SAX_startEncl ; se_annot = _; se_loc = _ } 
  | Some { se_desc = SAX_endEncl ; se_annot = _; se_loc = _ } ->
      Cursor.cursor_junk input_stream;
      (untyped_value_of_xml_stream_aux input_stream)
  | _ ->
      raise (Query (Stream_Error "Not a stream of atomic values"))

(* Attribute constructor 

   The second step should do implicit validation if necessary.
*)
let attribute_constructor sym nsenv input_stream =
  let uqname = Namespace_symbols.rattr_uname nsenv sym in
  let resolved_stream =
    begin
      match sym with
      | (prefix,uri,ncname) ->
	  if (uri = Namespace_symbols_builtin.xmlns_uri) || (ncname = Namespace_symbols_builtin.xmlns_local)
	  then
	    raise (Query (Constructor_Error "Attribute name is a reserved xmlns name"))
	  else
	    ()
    end;
    let text = (untyped_value_of_xml_stream input_stream) in
    let text = Whitespace.whitespace_normalize text in
    let text =
      if symbol_equal sym Namespace_symbols_builtin.xml_id
      then
	Whitespace.whitespace_id_normalize text
      else
	text
    in
    Cursor.cursor_of_list [fmkse_event(SAX_attribute (uqname,text,(ref (Some sym)),ref None)) Finfo.bogus]
  in
  Streaming_ops.typed_of_resolved_xml_stream resolved_stream

(* Comment constructor *)
let comment_constructor input_stream =
  let comment = (untyped_value_of_xml_stream input_stream) in
  let _ = Streaming_ops.check_valid_comment comment in
  Cursor.cursor_of_list [fmkse_event (SAX_comment comment) Finfo.bogus]

(* Processing instruction constructor *)
let pi_constructor leading target input_stream =
  let content = (untyped_value_of_xml_stream input_stream) in
  let content =
    if leading then Whitespace.remove_leading_whitespace content else content
  in
  let (target', content') = Streaming_ops.check_valid_processing_instruction target content in
  Cursor.cursor_of_list [fmkse_event (SAX_processingInstruction (target',content')) Finfo.bogus]

(* XQuery 3.7.3.4 Text Node Constructors

   The content expression of a text node constructor is processed as
   follows:

   1. Atomization is applied to the value of the content expression,
      converting it to a sequence of atomic values.
      (Handled by normalization)

   2. If the result of atomization is an empty sequence, no text node
      is constructed. Otherwise, each atomic value in the atomized
      sequence is cast into a string.

   3. The individual strings resulting from the previous step are
      merged into a single string by concatenating them with a single
      space character between each pair. The resulting string becomes
      the content property of the constructed text node.
Note:
    It is possible for a text node constructor to construct a text
    node containing a zero-length string. However, if used in the
    content of a constructed element or document node, such a text
    node will be deleted or merged with another text node.
*)

let text_constructor input_stream =
  match Cursor.cursor_peek input_stream with
  | None -> Cursor.cursor_of_list []
  | _ -> 
      let text = (untyped_value_of_xml_stream input_stream) in 
      Cursor.cursor_of_list [fmkse_event (SAX_characters text) Finfo.bogus]
	
let charref_constructor i =
  let text = Galax_camomile.utf8_string_of_code_point i in
  Cursor.cursor_of_list [fmkse_event (SAX_characters text) Finfo.bogus]

(* Element constructor *)
let element_content_stream xml_stream =
   let rec next_element_content_event xml_stream () =
    try
      let event = (Cursor.cursor_next xml_stream) in
      match event.se_desc with
      | SAX_startDocument _
      | SAX_endDocument ->
	  next_element_content_event xml_stream ()
      | SAX_attribute a ->
	  raise (Query (Stream_Error "Content of the constructor contains attributes not at the beginning"))
      | _ ->
	  Some event
    with
    | Stream.Failure ->
	None
  in
  Cursor.cursor_of_function (next_element_content_event xml_stream)

let prefix_attribute nsenv (uqname,content,orsym,rtype) (prev_atts,prev_bindings,required_bindings) =
  let rsym =
    match !orsym with
    | Some rsym -> rsym
    | None ->raise (Query (Datamodel ("Attribute hasn't been resolved")))
  in
  let (uqname,opt_binding,required_binding) = rattr_name_with_binding nsenv rsym in
  match opt_binding with
  | None ->
      let ns_attributes = prev_bindings in
      ((uqname,content,rsym) :: prev_atts, ns_attributes,required_binding :: required_bindings)
  | Some new_binding ->
      let ns_attributes =  new_binding :: prev_bindings in
      ((uqname,content,rsym) :: prev_atts, ns_attributes,required_bindings)

let prefix_attributes nsenv resolved_attributes =
  List.fold_right (prefix_attribute nsenv) resolved_attributes ([],[],[])

let fix_in_scope_namespaces nsenv rsym atts =
  let (uqname,opt_binding,required_binding) = relem_name_with_binding nsenv rsym in
  (* Printf.printf "UQNAME:%s\n" (Namespace_names.string_of_uqname uqname);
  flush stdout; *)
  let (_,new_bindings,required_attribute_prefixes) = prefix_attributes nsenv atts in
  let required_bindings =
    match opt_binding with
    | None ->
	let required_bindings = required_binding :: required_attribute_prefixes in
	cleanup_out_bindings new_bindings required_bindings
    | Some created_binding ->
	let rb = created_binding :: new_bindings in
	cleanup_out_bindings rb []
  in
  (* Namespace_context.print_binding_table "" Format.std_formatter required_bindings; *)
  (required_bindings,Namespace_context.patch_bindings nsenv required_bindings)

let element_constructor_of_resolved base_uri rsym nsenv resolved_input_stream =
  let sym = Namespace_symbols.relem_name rsym in
(*  let rsym = Namespace_symbols.relem_symbol sym in *)
  (* 2. Get the leading attributes in the resolved stream *)
  let leading_attributes =
    Streaming_ops.consume_leading_attribute_events resolved_input_stream
  in
  (* 2.b. Check that attributes are not duplicated *)
  let _ =
    Streaming_util.check_duplicate_attributes leading_attributes
  in
  (* 3. Make sure that the rest of the stream contains proper nodes
        (no attributes, and returns the children of document nodes) *)
  let resolved_element_content_stream =
    element_content_stream resolved_input_stream
  in
  (* 4. Compute the in-scope namespaces (see XQuery Section 3.7.4
  In-scope Namespaces of a Constructed Element) *)
  let (bindings,in_scope_nsenv) = fix_in_scope_namespaces nsenv rsym leading_attributes in
(*  Namespace_context.print_binding_table "" Format.std_formatter bindings;
  Namespace_context.print_binding_table "" Format.std_formatter (Namespace_context.flatten_bindings in_scope_nsenv); *)
  (* 5. Builds a small stream to construct the element *)
  let small_expr = SElem(sym,Some bindings,in_scope_nsenv,leading_attributes,base_uri,[SHole],ref (Some rsym)) in
  let small_stream = Small_stream_context.resolved_xml_stream_of_sexpr small_expr in
  (* 6. Compose the small stream with the resolved input stream *)
  let new_xml_stream =
    Streaming_ops.compose_xml_streams small_stream [resolved_element_content_stream]
  in new_xml_stream

let element_constructor base_uri sym nsenv input_stream =
  try
    (* 0. Clean up the labels, possibly buffering subtrees with nested labels *)
    let input_stream =
      Streaming_conv.typed_of_typed_labeled_xml_stream input_stream
	(* input_stream *)
    in
    (* 1. Do the erasure *)
    let resolved_input_stream = Streaming_ops.erase_xml_stream_section_3_7_1 input_stream in
    let new_xml_stream = element_constructor_of_resolved base_uri sym nsenv resolved_input_stream 
    in
    (* 7. Add type annotations *)
    (* 7.a NOTE THAT THIS MERGES TEXT NODES NOW - Philippe and Jerome *)
    Streaming_ops.typed_of_resolved_xml_stream new_xml_stream
  with
  | Query(Cursor_Error(msg)) -> raise(Query(Constructor_Error("In element_constructor "^msg)))

(* Document constructor *)

let document_constructor baseuri input_stream =
try
  (* 0. Clean up the labels, possibly buffering subtrees with nested labels *)
  let input_stream =
    Streaming_conv.typed_of_typed_labeled_xml_stream input_stream
  in
  (* 1. Do the erasure *)
  let resolved_input_stream = Streaming_ops.erase_xml_stream_section_3_7_1 input_stream in

  (* 2. Get the leading attributes in the resolved stream *)
  let leading_attributes =
    Streaming_ops.consume_leading_attribute_events resolved_input_stream
  in

  (* 2.b. Check that there is no attribute *)
  let _ =
    if not(leading_attributes = []) then
      raise (Query (Stream_Error "Document nodes should not have attribute content"))
    else
      ()
  in
  (* 3. Make sure that the rest of the stream contains proper nodes
        (no attributes, and returns the children of document nodes) *)
  let resolved_document_content_stream =
    try
      element_content_stream resolved_input_stream
    with
    | (Query (Stream_Error "Content of the constructor contains attributes not at the beginning")) ->
	raise (Query (Stream_Error "Document nodes should not have attribute content"))
  in

  (* 2. Builds a small stream to construct the element *)
  let small_expr = SDocument(baseuri,[SHole]) in
  let small_stream = Small_stream_context.resolved_xml_stream_of_sexpr small_expr in
  (* 3. Compose the small stream with the resolved input stream *)
  let new_xml_stream =
    Streaming_ops.compose_xml_streams small_stream [resolved_document_content_stream]
  in
  (* 4. Add type annotations *)
  Streaming_ops.typed_of_resolved_xml_stream new_xml_stream
with
| Query(Cursor_Error(msg)) -> raise(Query(Constructor_Error("In document_constructor "^msg)))

let sequence_constructor t1 t2 =
  Cursor.cursor_append t1 t2

(**************************)
(* Serialization wrapping *)
(**************************)

(* Note:
     Essentially, this calls element construction with the proper
     parameters to build a <glx:result> top-level element.
 - Jerome *)

let glx_result_serialization input_stream =
  let sym = Namespace_symbols.relem_symbol Namespace_builtin.glx_result in
  let nsenv = 
    let export_orig_nsenv = Namespace_context.default_xml_out_nsenv () in
    let delta_bindings = [(Namespace_builtin.glx_prefix,Namespace_builtin.glx_uri)] in
    Namespace_context.add_all_ns export_orig_nsenv delta_bindings
  in
  element_constructor Dm_atomic_util.default_no_uri_dm sym nsenv input_stream

let flatten_document_nodes input_stream =
  let no_doc () =
    match Cursor.cursor_peek input_stream with
    | Some event ->
	begin
	  match event.se_desc with
	  | SAX_startDocument _ ->
	      Cursor.cursor_junk input_stream;
	      Some (Cursor.cursor_next input_stream)
	  | SAX_endDocument ->
	      Cursor.cursor_junk input_stream;
	      Some (Cursor.cursor_next input_stream)
	  | _ -> Some (Cursor.cursor_next input_stream)
	end
    | None -> None
  in
  Cursor.cursor_of_function no_doc

let dburi =
  Dm_atomic_util.default_no_uri_dm

let sequence_normalization input_stream =
  document_constructor dburi (flatten_document_nodes input_stream)

