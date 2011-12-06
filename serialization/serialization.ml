(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: serialization.ml,v 1.30 2008/03/12 22:30:58 simeon Exp $ *)

(* Module: Serialization
   Description:
     This module takes an XML stream and serializes it to a given
     output formatter.
*)

open Format

open Error

open Namespace_names
open Namespace_symbols

open Datatypes

open Streaming_types

open Serialization_context


(***********************************)
(* String serialization operations *)
(***********************************)

let serialize_markup_string serial_context s =
  let target_encoding = get_target_encoding serial_context in
  let internal_encoding = get_internal_encoding serial_context in
  Encoding.write_markup_string internal_encoding target_encoding s

let serialize_data_string serial_context s =
  let target_encoding = get_target_encoding serial_context in
  let internal_encoding = get_internal_encoding serial_context in
  Encoding.write_data_string internal_encoding target_encoding s

(**************)
(* XML header *)
(**************)

let fserialize_xml_header serial_context ff =
  let target_encoding = get_target_encoding serial_context in
  if (get_serialization_kind serial_context != Processing_context.Serialize_As_Canonical) && (get_serialization_kind serial_context != Processing_context.Serialize_As_Standard)
  then
    begin
      let enc_string = Encoding.string_of_encoding target_encoding in
      match target_encoding with
	(* Special headers for UTF-16 *)
      | `Enc_utf16_le -> 
	  fprintf ff "\255\254"
      | `Enc_utf16_be ->
	  fprintf ff "\254\255"
	    (* Or build a standard XML header with the right encoding declaration *)
      | `Enc_iso88591
      | `Enc_usascii
      | `Enc_utf8
      | `Enc_java     ->
	  let header = "<?xml version=\"1.0\" encoding=\"" ^ enc_string ^ "\"?>" in
	  let header_string = serialize_markup_string serial_context header in
	  fprintf ff "%s" header_string
      | _ ->
	  raise (Query (Prototype ("Serialization to output encoding " ^ enc_string ^ " not supported")))
    end

(*****************)
(* Atomic values *)
(*****************)

(* Note:
     The serialization should use the notion of XML Schema normalized
     values for atomic types.

     This is implemented using the 'erase' operations which are part
     of the Simple module in ./base.

   - Jerome
*)

let fserialize_atomic_value serial_context ff vn =
  let s = serialize_data_string serial_context (vn#erase_atomic_value()) in
  fprintf ff "%s" s

let fserialize_atomic_value_xquery serial_context ff (vn : Dm_atomic.atomicValue) =
  let dt = vn#atomic_type() in
  if (Namespace_symbols.symbol_equal dt Namespace_symbols_builtin.xs_string)
  then
    fprintf ff "\"%s\"" (vn#erase_atomic_value())
  else if
    ((Namespace_symbols.symbol_equal dt Namespace_symbols_builtin.xs_integer) || 
    (Namespace_symbols.symbol_equal dt Namespace_symbols_builtin.xs_decimal) || 
    (Namespace_symbols.symbol_equal dt Namespace_symbols_builtin.xs_double))
  then
    fprintf ff "%s" (vn#erase_atomic_value())
  else if (Namespace_symbols.symbol_equal dt Namespace_symbols_builtin.xs_boolean)
  then
    if vn#getAtomicBoolean()
    then fprintf ff "true()"
    else fprintf ff "false()"
  else if (Namespace_symbols.symbol_equal dt Namespace_symbols_builtin.xs_anyAtomicType)
  then
    raise (Query(Serialization("Dynamic type of value is xs:anyAtomicType")))
  else
    let rsqname = rtype_prefix_string dt in
    fprintf ff "%s(\"%s\")" rsqname (vn#erase_atomic_value())

let rec fserialize_simple_value serial_context ff vs =
  let rec fserialize_atomic_value_list ff vl =
    match vl with
    | [] ->
	()
    | x :: [] ->
	fserialize_atomic_value serial_context ff x
    | x :: vl' ->
	fprintf ff "%a@ %a" (fserialize_atomic_value serial_context) x fserialize_atomic_value_list vl'
  in
  fserialize_atomic_value_list ff vs


(************)
(* PI nodes *)
(************)

let open_pi serial_context =
  serialize_markup_string serial_context "<?"

let close_pi serial_context =
  serialize_markup_string serial_context "?>"

let make_space serial_context =
  serialize_data_string serial_context " "

let fserialize_pi serial_context ff (pit,piv) =
  let pi_target =
    serialize_markup_string serial_context pit
  in
  let pi_content = piv in
  if pi_content = ""
  then
    fprintf ff "%s%s%s" (open_pi serial_context) pi_target (close_pi serial_context)
  else
    let pi_space =
      make_space serial_context
    and pi_value =
      serialize_markup_string serial_context pi_content
    in
    fprintf ff "%s%s%s%s%s" (open_pi serial_context) pi_target pi_space pi_value (close_pi serial_context)


(*****************)
(* Comment nodes *)
(*****************)

let open_comment serial_context =
  serialize_markup_string serial_context "<!--"

let close_comment serial_context =
  serialize_markup_string serial_context "-->"

let fserialize_comment serial_context ff cvalue =
  if (get_serialization_kind serial_context != Processing_context.Serialize_As_Canonical)
  then
    begin
      let comment_value =
	serialize_data_string serial_context cvalue
      in
      fprintf ff "%s%s%s" (open_comment serial_context) comment_value (close_comment serial_context)
    end


(***************************)
(* Main node serialization *)
(***************************)

let open_start_element serial_context =
  serialize_markup_string serial_context "<"

let close_start_element serial_context =
  serialize_markup_string serial_context ">"

let close_empty_element serial_context =
  serialize_markup_string serial_context "/>"

let open_end_element serial_context =
  serialize_markup_string serial_context "</"

let close_end_element serial_context =
  serialize_markup_string serial_context ">"

(* Prints attributes *)

let fserialize_one_attribute serial_context ff (uqname, content,_,_) =
  let attribute_name_string = string_of_uqname uqname in
  let attribute_name = serialize_markup_string serial_context attribute_name_string in
  let attribute_content = serialize_data_string serial_context content in
  let equal_sign = serialize_markup_string serial_context "=" in
  let quote = serialize_markup_string serial_context "\"" in
  fprintf ff "%s%s%s%s%s" attribute_name equal_sign quote attribute_content quote

let fserialize_attributes serial_context ff specialattributes attributes =
  let rec fserialize_attributes_aux serial_context ff attributes =
    match attributes with
    | [] ->
	()
    | x :: attributes ->
	begin
	  pp_print_break ff 1 1;
	  fprintf ff "%a%a" (fserialize_one_attribute serial_context) x (fserialize_attributes_aux serial_context) attributes
	end
  in
(*  Namespace_context.print_special_attributes "serialization time" Format.std_formatter !specialattributes; *)
  let specialattributes = List.map (fun (x,y) -> (x,y,ref None,ref None)) !specialattributes  in
  let attributes = (specialattributes@attributes) in
  match attributes with
  | [] ->
      ()
  | x :: attributes ->
      begin
	pp_open_hvbox ff 0;
	fprintf ff " %a%a" (fserialize_one_attribute serial_context) x (fserialize_attributes_aux serial_context) attributes;
	pp_close_box ff ()
      end

let fserialize_text_attributes serial_context ff specialattributes attributes =
  let rec fserialize_text_attributes_aux serial_context ff attributes =
    match attributes with
    | [] ->
	()
    | x :: attributes ->
	fprintf ff " %a%a" (fserialize_one_attribute serial_context) x (fserialize_text_attributes_aux serial_context) attributes
  in
(*  Namespace_context.print_special_attributes "serialization time" Format.std_formatter !specialattributes; *)
  let specialattributes = List.map (fun (x,y) -> (x,y,ref None,ref None)) !specialattributes  in
  let attributes = (specialattributes@attributes) in
  match attributes with
  | [] ->
      ()
  | _ ->
      fserialize_text_attributes_aux serial_context ff attributes

let fserialize_stand_alone_attribute serial_context ff (uqname, content,_,_) =
  let attribute_name_string = string_of_uqname uqname in
  let attribute_name = serialize_markup_string serial_context attribute_name_string in
  let attribute_content = serialize_data_string serial_context content in
  fprintf ff "@[<hv 2>attribute %s {@,\"%s\"@;<0 -2>}@]" attribute_name attribute_content

(* Prints a start document event *)

let fserialize_start_document serial_context ff () =
  fserialize_xml_header serial_context ff

(* Prints an end document event *)

let fserialize_end_document serial_context ff () =
  ()

(* Prints a start element event *)

let fserialize_open_tag serial_context ff elem_name specialattributes attributes outer_element_kind =
  begin
    pp_print_string ff (open_start_element serial_context);
    pp_print_string ff elem_name;
    fserialize_attributes serial_context ff specialattributes attributes;
    pp_print_string ff (close_start_element serial_context)
  end

let fserialize_text_open_tag serial_context ff elem_name specialattributes attributes outer_element_kind =
  begin
    pp_print_string ff (open_start_element serial_context);
    pp_print_string ff elem_name;
    fserialize_text_attributes serial_context ff specialattributes attributes;
    pp_print_string ff (close_start_element serial_context)
  end

let fserialize_empty_tag serial_context ff elem_name specialattributes attributes outer_element_kind =
  begin
    pp_print_string ff (open_start_element serial_context);
    pp_print_string ff elem_name;
    pp_open_hvbox ff 0;
    fserialize_attributes serial_context ff specialattributes attributes;
    pp_close_box ff ();
    pp_print_string ff (close_empty_element serial_context)
  end

let fserialize_text_empty_tag serial_context ff elem_name specialattributes attributes outer_element_kind =
  begin
    pp_print_string ff (open_start_element serial_context);
    pp_print_string ff elem_name;
    fserialize_text_attributes serial_context ff specialattributes attributes;
    pp_print_string ff (close_empty_element serial_context)
  end

let fserialize_open_tag_top serial_context ff elem_name specialattributes attributes =
  begin
    pp_print_string ff (open_start_element serial_context);
    pp_print_string ff elem_name;
    fserialize_attributes serial_context ff specialattributes attributes;
    pp_print_string ff (close_start_element serial_context)
  end

let fserialize_text_open_tag_top serial_context ff elem_name specialattributes attributes =
  begin
    pp_print_string ff (open_start_element serial_context);
    pp_print_string ff elem_name;
    fserialize_text_attributes serial_context ff specialattributes attributes;
    pp_print_string ff (close_start_element serial_context)
  end

let fserialize_empty_tag_top serial_context ff elem_name specialattributes attributes =
  begin
    pp_print_string ff (open_start_element serial_context);
    pp_print_string ff elem_name;
    pp_open_hvbox ff 0;
    fserialize_attributes serial_context ff specialattributes attributes;
    pp_close_box ff ();
    pp_print_string ff (close_empty_element serial_context)
  end

let fserialize_text_empty_tag_top serial_context ff elem_name specialattributes attributes =
  begin
    pp_print_string ff (open_start_element serial_context);
    pp_print_string ff elem_name;
    fserialize_text_attributes serial_context ff specialattributes attributes;
    pp_print_string ff (close_empty_element serial_context)
  end

let fserialize_start_element serial_context ff (uqname,attributes,has_element_content,specialattributes,_,_,_) =
  let element_name_string = string_of_uqname uqname in
  let outer_element_kind = get_current_element_kind serial_context in
  let current_element_kind = new_current_element_kind outer_element_kind has_element_content in
  let is_toplevel = is_toplevel serial_context in
  let elem_name =
    serialize_markup_string serial_context element_name_string
  in
  push_element serial_context (uqname, current_element_kind);
  match current_element_kind with
  | ElementContentInTextElement ->
      begin
	fserialize_open_tag serial_context ff elem_name specialattributes attributes outer_element_kind;
	pp_force_newline ff ();
	pp_open_hvbox ff 2
      end
  | ElementContentInContentElement ->
      begin
	if not(is_toplevel)
	then
	  pp_print_cut ff ();
	pp_open_hvbox ff 2;
	fserialize_open_tag serial_context ff elem_name specialattributes attributes outer_element_kind
      end
  | TextElementInElementContent ->
      begin
	if not(is_toplevel)
	then
	  pp_print_cut ff ();
	fserialize_open_tag serial_context ff elem_name specialattributes attributes outer_element_kind
      end
  | TextElementInTextContent ->
      begin
	fserialize_text_open_tag serial_context ff elem_name specialattributes attributes outer_element_kind
      end
  | TopElement ->
      raise (Query (Serialization "Should not get a TopElement descriptor during serialization"))

let fserialize_empty_element serial_context ff (uqname,attributes,has_element_content,specialattributes,_,_,_) =
  let element_name_string = string_of_uqname uqname in
  let outer_element_kind = get_current_element_kind serial_context in
  let current_element_kind = new_current_element_kind outer_element_kind has_element_content in
  let is_toplevel = is_toplevel serial_context in
  let elem_name =
    serialize_markup_string serial_context element_name_string
  in
  push_element serial_context (uqname, current_element_kind);
  match current_element_kind with
  | ElementContentInTextElement ->
      begin
	fserialize_empty_tag serial_context ff elem_name specialattributes attributes outer_element_kind
      end
  | ElementContentInContentElement ->
      begin
	if not(is_toplevel)
	then
	  pp_print_cut ff ();
	pp_open_hvbox ff 2;
	fserialize_empty_tag serial_context ff elem_name specialattributes attributes outer_element_kind;
	pp_close_box ff ()
      end
  | TextElementInElementContent ->
      begin
	if not(is_toplevel)
	then
	  pp_print_cut ff ();
	fserialize_text_empty_tag serial_context ff elem_name specialattributes attributes outer_element_kind
      end
  | TextElementInTextContent ->
      begin
	fserialize_text_empty_tag serial_context ff elem_name specialattributes attributes outer_element_kind
      end
  | TopElement ->
      raise (Query (Serialization "Should not get a TopElement descriptor during serialization"))

let fserialize_empty_element_top serial_context ff (uqname,attributes,has_element_content,specialattributes,_,_,_) =
  let element_name_string = string_of_uqname uqname in
  let outer_element_kind = get_current_element_kind serial_context in
  let current_element_kind = new_current_element_kind outer_element_kind has_element_content in
  let is_toplevel = is_toplevel serial_context in
  let elem_name =
    serialize_markup_string serial_context element_name_string
  in
  push_element serial_context (uqname, current_element_kind);
  match current_element_kind with
  | ElementContentInTextElement ->
      fserialize_empty_tag_top serial_context ff elem_name specialattributes attributes
  | ElementContentInContentElement ->
      begin
	if not(is_toplevel)
	then
	  pp_print_cut ff ();
	pp_open_hvbox ff 2;
	fserialize_empty_tag_top serial_context ff elem_name specialattributes attributes;
	pp_close_box ff ()
      end
  | TextElementInElementContent ->
      begin
	if not(is_toplevel)
	then
	  pp_print_cut ff ();
	fserialize_text_empty_tag_top serial_context ff elem_name specialattributes attributes
      end
  | TextElementInTextContent ->
      fserialize_text_empty_tag_top serial_context ff elem_name specialattributes attributes
  | TopElement ->
      raise (Query (Serialization "Should not get a TopElement descriptor during serialization"))

(* Prints a close element event *)

let fserialize_close_tag serial_context ff elem_name =
  begin
    pp_print_string ff (open_end_element serial_context);
    pp_print_string ff elem_name;
    pp_print_string ff (close_end_element serial_context)
  end

let fserialize_end_element serial_context ff () =
  let (uqname, current_element_kind) = pop_element serial_context in
  let element_name_string = string_of_uqname uqname in
  let elem_name =
    serialize_markup_string serial_context element_name_string
  in
  match current_element_kind with
  | ElementContentInTextElement ->
      begin
	pp_print_break ff 0 (-2);
	pp_close_box ff ();
	pp_force_newline ff ();
	fserialize_close_tag serial_context ff elem_name
      end
  | ElementContentInContentElement ->
      begin
	pp_print_break ff 0 (-2);
	fserialize_close_tag serial_context ff elem_name;
	pp_close_box ff ()
      end
  | TextElementInElementContent ->
      begin
	fserialize_close_tag serial_context ff elem_name;
      end
  | TextElementInTextContent ->
      begin
	fserialize_close_tag serial_context ff elem_name
      end
  | TopElement ->
      raise (Query (Serialization "Should not get a TopElement descriptor during serialization"))

(* Prints a character event *)

let fserialize_characters_top serial_context ff content =
  let target_content = serialize_data_string serial_context content in
  fprintf ff "@[<hv 2>text {@,\"%s\"@;<0 -2>}@]" target_content

let fserialize_characters serial_context ff content =
  let target_content = serialize_data_string serial_context content in
  pp_print_string ff target_content

(* Dispatching events to printing functions *)

let serialize_xml_event serial_context ff =
  let event = get_next_event serial_context in
  match event.se_desc with
  | SAX_startDocument _ ->
      if not(get_serialization_kind serial_context = Processing_context.Serialize_As_Canonical) && not(get_serialization_kind serial_context = Processing_context.Serialize_As_Standard)
      then
	begin
	  pp_open_vbox ff 0;
	  fserialize_start_document serial_context ff ();
	  push_document serial_context
	end
  | SAX_endDocument ->
      if not(get_serialization_kind serial_context = Processing_context.Serialize_As_Canonical) && not(get_serialization_kind serial_context = Processing_context.Serialize_As_Standard)
      then
	begin
	  fserialize_end_document serial_context ff ();
	  pp_close_box ff ();
	  pop_document serial_context
	end
  | SAX_startElement elem_desc ->
      if not(get_serialization_kind serial_context = Processing_context.Serialize_As_Canonical)
      then
	begin
	  match peek_next_event serial_context with
	  | None ->
	      raise (Query (Stream_Error "Non-terminated element in SAX Stream"))
	  | Some { se_desc = SAX_endElement; se_loc = _ } ->
	      begin
		fserialize_empty_element serial_context ff elem_desc;
		ignore(get_next_event serial_context);
		ignore(pop_element serial_context)
	      end
	  | Some _ ->
	      fserialize_start_element serial_context ff elem_desc
	end
      else
	(* Always serialize as open-close tag in the canonical form *)
	fserialize_start_element serial_context ff elem_desc
  | SAX_endElement ->
      fserialize_end_element serial_context ff ()
  | SAX_processingInstruction pi_desc ->
      fserialize_pi serial_context ff pi_desc
  | SAX_comment comment_desc ->
      fserialize_comment serial_context ff comment_desc
  | SAX_characters content ->
      fserialize_characters serial_context ff content
  | SAX_attribute sax_xml_attribute ->
      fserialize_stand_alone_attribute serial_context ff sax_xml_attribute
  | SAX_atomicValue atomicvalue ->
      fserialize_atomic_value serial_context ff atomicvalue
  | SAX_endEncl
  | SAX_startEncl
  | SAX_hole ->
      raise (Query (Serialization "Cannot serialize a stream with holes"))

let fserialize_start_element_top serial_context ff (uqname,attributes,has_element_content,specialattributes,_,_,_) =
  let element_name_string = string_of_uqname uqname in
  let outer_element_kind = get_current_element_kind serial_context in
  let current_element_kind = new_current_element_kind outer_element_kind has_element_content in
  let is_toplevel = is_toplevel serial_context in
  let elem_name =
    serialize_markup_string serial_context element_name_string
  in
  push_element serial_context (uqname, current_element_kind);
  match current_element_kind with
  | ElementContentInTextElement ->
      begin
	fserialize_open_tag_top serial_context ff elem_name specialattributes attributes;
	pp_force_newline ff ();
	pp_open_hvbox ff 2
      end
  | ElementContentInContentElement ->
      begin
	if not(is_toplevel)
	then
	  pp_print_cut ff ();
	pp_open_hvbox ff 2;
	fserialize_open_tag_top serial_context ff elem_name specialattributes attributes
      end
  | TextElementInElementContent ->
      begin
	if not(is_toplevel)
	then
	  pp_print_cut ff ();
	fserialize_open_tag_top serial_context ff elem_name specialattributes attributes
      end
  | TextElementInTextContent ->
      begin
	fserialize_text_open_tag_top serial_context ff elem_name specialattributes attributes
      end
  | TopElement ->
      raise (Query (Serialization "Should not get a TopElement descriptor during serialization"))

let serialize_xml_event_top serial_context ff =
  begin
    begin
      let event = get_next_event serial_context in
      match event.se_desc with
      | SAX_startDocument _ ->
	  begin
	    pp_open_vbox ff 2;
	    pp_print_string ff "document {";
	    pp_print_cut ff ();
	    push_document serial_context
	  end
      | SAX_endDocument ->
	  begin
	    pp_print_break ff 0 (-2);
	    pp_print_string ff "}";
	    pp_close_box ff ();
	    pop_document serial_context
	  end
      | SAX_startElement elem_desc ->
	  begin
	    match peek_next_event serial_context with
	    | None ->
		raise (Query (Stream_Error "Non-terminated element in SAX Stream"))
	    | Some { se_desc = SAX_endElement; se_loc = _ } ->
		begin
		  fserialize_empty_element_top serial_context ff elem_desc;
		  ignore(get_next_event serial_context);
		  ignore(pop_element serial_context)
		end
	    | Some _ ->
		fserialize_start_element_top serial_context ff elem_desc
	  end
      | SAX_endElement ->
	  fserialize_end_element serial_context ff ()
      | SAX_processingInstruction pi_desc ->
	  fserialize_pi serial_context ff pi_desc
      | SAX_comment comment_desc ->
	  fserialize_comment serial_context ff comment_desc
      | SAX_characters content ->
	  if is_toplevel serial_context
	  then
	    fserialize_characters_top serial_context ff content
	  else
	    fserialize_characters serial_context ff content
      | SAX_attribute sax_xml_attribute ->
	  fserialize_stand_alone_attribute serial_context ff sax_xml_attribute
      | SAX_atomicValue atomicvalue ->
	  fserialize_atomic_value_xquery serial_context ff atomicvalue
      | SAX_startEncl
      | SAX_endEncl
      | SAX_hole ->
	  raise (Query (Serialization "Cannot serialize a stream with holes"))
    end;
    if not(is_empty serial_context) && (is_toplevel serial_context)
    then
      begin
	pp_print_string ff ",";
	pp_print_space ff ()
      end
  end

(************************************)
(* Namespace bindings serialization *)
(************************************)

let fserialize_node_content serial_context ff () =
  try
    while true do
      serialize_xml_event serial_context ff
    done
  with
  | Stream.Failure ->
      ()

let rec fserialize_node_top serial_context ff () =
  try
    while true do
      serialize_xml_event_top serial_context ff
    done
  with
  | Stream.Failure ->
      ()
	
let rec fserialize_value_top serial_context ff () =
  if (is_empty serial_context)
  then
    fprintf ff "()"
  else
    begin
      pp_open_hvbox ff 0;
      fserialize_node_top serial_context ff ();
      pp_close_box ff ()
    end

let setup_formatter_for_encoding serial_context ff =
  let (out, flush, newline, spaces) =
    pp_get_all_formatter_output_functions ff ()
  in
  let new_newline () =
    let newline_chars =
      serialize_markup_string serial_context "\n"
    in
    let new_line_size =
      String.length newline_chars
    in
    out newline_chars 0 new_line_size
  in
  let new_spaces n =
    let spaces =
      let some_spaces_chars = (String.make n ' ') in
      serialize_markup_string serial_context some_spaces_chars
    in
    let spaces_size =
      String.length spaces
    in
    out spaces 0 spaces_size
  in
  pp_set_all_formatter_output_functions ff out flush new_newline new_spaces


(************************************)
(* Serialization from an XML stream *)
(************************************)

(* Serialization from an XML stream to an arbitrary formatter *)

let fserialize_xml_stream proc_ctxt ff stream =
  (* Note:
       Those variables take top-level values, but should eventually be
       passed as input parameters somehow.
     - Jerome
   *)
  let target_encoding = (Encoding.get_output_encoding ()) in
  let serialization_kind = proc_ctxt.Processing_context.serialization_kind in
  let internal_encoding = (Encoding.get_internal_encoding ()) in
  let whitespace_mode = Whitespace.Default in
  let serial_context =
    build_serialization_context
      serialization_kind
      internal_encoding
      target_encoding
      whitespace_mode
      stream
  in
  begin
    setup_formatter_for_encoding serial_context ff;
    match serialization_kind with
    | Processing_context.Serialize_As_Standard ->
	fprintf ff "%a@?" (fserialize_node_content serial_context) ()
    | Processing_context.Serialize_As_Well_Formed
    | Processing_context.Serialize_As_Canonical ->
	fprintf ff "%a@?" (fserialize_node_content serial_context) ()
    | Processing_context.Serialize_As_XQuery ->
	fprintf ff "%a@?" (fserialize_value_top serial_context) ()
    | Processing_context.Serialize_As_Text ->
	fprintf ff "%a@?" (fserialize_value_top serial_context) ()
  end

(* Serialization to the standard formatter (stdout) *)

let serialize_xml_stream proc_ctxt stream =
  fserialize_xml_stream proc_ctxt Format.std_formatter stream

(* Serialization to a string buffer *)

let bserialize_xml_stream proc_ctxt stream =
  let buff = Buffer.create 50 in
  bprintf buff "%a@?" (fserialize_xml_stream proc_ctxt) stream;
  let result = Buffer.contents buff in
  Buffer.reset buff;
  result


(********************************************)
(* Serialization from a resolved XML stream *)
(********************************************)

(* Serialization from a resolved XML stream to an arbitrary formatter *)

let fserialize_resolved_xml_stream proc_ctxt ff resolved_stream =
  fserialize_xml_stream proc_ctxt ff (Streaming_ops.prefix_xml_stream resolved_stream)

(* Serialization to the standard formatter (stdout) *)

let serialize_resolved_xml_stream proc_ctxt resolved_stream =
  fserialize_resolved_xml_stream proc_ctxt Format.std_formatter resolved_stream

(* Serialization to a string buffer *)

let bserialize_resolved_xml_stream proc_ctxt resolved_stream =
  let buff = Buffer.create 50 in
  bprintf buff "%a@?" (fserialize_resolved_xml_stream proc_ctxt) resolved_stream;
  let result = Buffer.contents buff in
  Buffer.reset buff;
  result


(*****************************************)
(* Serialization from a typed XML stream *)
(*****************************************)

(* Serialization from a typed XML stream to an arbitrary formatter *)

let fserialize_typed_xml_stream proc_ctxt ff typed_stream =
  let typed_stream = Streaming_conv.typed_of_typed_labeled_xml_stream typed_stream in
  fserialize_resolved_xml_stream proc_ctxt ff (Streaming_ops.erase_xml_stream typed_stream)

(* Serialization to the standard formatter (stdout) *)

let serialize_typed_xml_stream proc_ctxt typed_stream =
  fserialize_typed_xml_stream proc_ctxt Format.std_formatter typed_stream

(* Serialization to a string buffer *)

let bserialize_typed_xml_stream proc_ctxt typed_stream =
  let buff = Buffer.create 50 in
  bprintf buff "%a@?" (fserialize_typed_xml_stream proc_ctxt) typed_stream;
  let result = Buffer.contents buff in
  Buffer.reset buff;
  result


(*******************************************)
(* Serialization from a datamodel instance *)
(*******************************************)

(* Note:
     Serialization from a datamodel instance is only a convenience
     short-cut. It proceeds by first exporting the data model instance
     to a typed XML stream, them turning the typed XML stream to an
     untyped one, then applying serialization on the XML stream.
   - Jerome *)

(* Serialization from an XML stream to an arbitrary formatter *)

let fserialize_datamodel proc_ctxt ff dmv =
  let resolved_xml_stream =
    match proc_ctxt.Processing_context.serialization_kind with
    | Processing_context.Serialize_As_Standard ->
	Streaming_ops.erase_xml_stream
	  (Streaming_constructors.sequence_normalization
	     (Physical_export.typed_xml_stream_of_datamodel dmv))
    | Processing_context.Serialize_As_Well_Formed
    | Processing_context.Serialize_As_Canonical ->
	Physical_export.resolved_wrapped_xml_stream_of_datamodel dmv
    | Processing_context.Serialize_As_XQuery ->
	let typed_xml_stream = Physical_export.typed_xml_stream_of_datamodel dmv in
	Streaming_ops.erase_xml_stream typed_xml_stream
    | Processing_context.Serialize_As_Text ->
	let typed_xml_stream = Physical_export.typed_xml_stream_of_datamodel dmv in
	Streaming_ops.erase_xml_stream typed_xml_stream
  in
  fserialize_resolved_xml_stream proc_ctxt ff resolved_xml_stream
  

(* Serialization to the standard formatter (stdout) *)

let serialize_datamodel proc_ctxt dmv =
  fserialize_datamodel proc_ctxt Format.std_formatter dmv

(* Serialization to a string buffer *)

let bserialize_datamodel proc_ctxt dmv =
  let buff = Buffer.create 50 in
  bprintf buff "%a@?" (fserialize_datamodel proc_ctxt) dmv;
  let result = Buffer.contents buff in
(* print_string ("bserialize_datamodel "^(string_of_int(String.length result))^"\n");  *)
  Buffer.reset buff;
  result

