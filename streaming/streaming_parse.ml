(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: streaming_parse.ml,v 1.13 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Streaming_parse
   Description:
     This module implements a SAX parser for Galax.
*)

open Error
open Finfo

open Pxp_types
open Pxp_lexer_types

open Namespace_names

open Streaming_types
open Streaming_util

type context =
    { mutable odtd : Pxp_dtd.dtd option;
      mutable reused_lexobj : lexer_obj option;
      io : string option;
      fileid : int }

let fileid = get_file_id "Unknown File"
let cur_finfo () = ref (make_finfo_line_col_id fileid 0 0)

let make_file_id gio_string =
  match gio_string with
  | None -> fileid
  | Some f -> get_file_id f

let default_context gio_string =
  { odtd = None;
    reused_lexobj = None;
    io = gio_string;
    fileid = make_file_id gio_string }

let sax_raise_parsing_error pp msg =
  begin
    Galax_io.close_pull_parser (snd pp);
    raise (Query (Parsing (Finfo.bogus, msg)))
  end

let set_dtd_in_context c dtd =
  begin
    let lfactory = dtd # lexer_factory in
    let reused_lexobj = lfactory # open_string "" in
    c.odtd <- Some dtd;
    c.reused_lexobj <- Some reused_lexobj
  end

let glx_expand_attvalue c str =
  let dtd =
    match c.odtd with
    | None -> raise (Query (Internal_Error "Missing DTD in parsing"))
    | Some dtd -> dtd
  in
  let reused_lexobj =
    match c.reused_lexobj with
    | None -> raise (Query (Internal_Error "Missing DTD in parsing"))
    | Some reused_lexobj -> reused_lexobj
  in
  Pxp_aux.expand_attvalue reused_lexobj dtd str false

let glx_normalized_public_id s =
  Pxp_aux.normalize_public_id s

let make_document_desc c xmlversion dtd =
  let uri =
    match c.io with
    | None -> Dm_atomic_util.default_no_uri_dm
    | Some uri -> Dm_atomic_util.uri_dm_of_uri (Some (AnyURI._kinda_uri_of_string uri))
  in
  match dtd#root with
  | None ->
      (Some (xmlversion,None,None), None, uri)
  | Some _ ->
      (Some (xmlversion,None,None), Some dtd, uri)

let make_att c (name,content) =
  let qname = uqname_element_of_string name in
(*  let fixed_content = glx_expand_attvalue c content in *)
  let fixed_content = content in
  (qname,fixed_content, ref None, ref None)

let make_element_desc c name attlist scope_opt entid =
  let qname = uqname_element_of_string name in
  let sax_xml_attribute_forest = List.rev (List.map (make_att c) attlist) in
  (* Extract the namespace attributes *)
  let (ws_mode, special_attributes, base_uri, other_attributes) =
    Streaming_util.extract_special_attributes sax_xml_attribute_forest
  in
  (qname,other_attributes,ref false,ref special_attributes, ref base_uri, ref None, ref None)

let make_text_desc data =
  data

let make_pi_desc target value =
  (target,value)

let make_comment_desc content =
  content

let rec map_event fi_ref c pp event =
  match event with
  | E_start_doc (xmlversion,dtd) ->
      begin
	if xmlversion = "1.0"
	then
	  ()
	else
	  sax_raise_parsing_error pp ("XML version: " ^ xmlversion ^ " not supported");
	set_dtd_in_context c dtd;
	Some (fmkse_event (SAX_startDocument (make_document_desc c xmlversion dtd)) (!fi_ref))
      end
  | E_end_doc lit_name ->
      Some (fmkse_event (SAX_endDocument) !fi_ref)
  | E_start_tag (name, attlist, scope_opt, entid) ->
      Some (fmkse_event (SAX_startElement (make_element_desc c name attlist scope_opt entid)) (!fi_ref))
  | E_end_tag (name, entid) ->
      Some (fmkse_event (SAX_endElement) (!fi_ref))
  | E_char_data data ->
      Some (fmkse_event (SAX_characters (make_text_desc data)) (!fi_ref))
  | E_pinstr (target,value,entid) ->
      Some (fmkse_event (SAX_processingInstruction (make_pi_desc target value)) (!fi_ref))
  | E_comment content ->
      Some (fmkse_event (SAX_comment (make_comment_desc content)) (!fi_ref))
  (* Silently ignoring super root event *)
  | E_start_super ->
      next_pxp_event fi_ref c pp
  | E_end_super ->
      next_pxp_event fi_ref c pp
  | E_position (entity,line,col) ->
    (*
     * These events are only created if the next event will be
     * E_start_tag, E_pinstr, or E_comment, and if the configuration option
     * store_element_position is true.
     *)
      fi_ref := make_finfo_line_col_id c.fileid line col;
      next_pxp_event fi_ref c pp
  | E_error exn ->
      begin
	Galax_io.close_pull_parser (snd pp);
	raise exn
      end
  | E_end_of_stream ->
      begin
	Galax_io.close_pull_parser (snd pp);
	None
      end

and next_pxp_event fi_ref c pp =
  begin
    match (fst pp) () with
    | None ->
	begin
	  Galax_io.close_pull_parser (snd pp);
	  None
	end
    | Some event ->
	map_event fi_ref c pp event
  end

 

(************************************)
(* The top level stream constructor *)
(************************************)

let get_schema_of_stream stream =
  let eo = Cursor.cursor_peek stream in
  match eo with
  | None -> None
  | Some e ->
      match e.se_desc with
      | SAX_startDocument (_,schema,_) -> schema
      | _ -> None

let open_pxp_stream_from_io gio entity_kind =
  let s = Galax_io.uri_string_of_gio gio in
  (Galax_io.pull_parser_from_input_spec gio  Galax_io.Document_entity,s)

let open_xml_stream_from_pxp_stream (pp,s) =
  let fi_ref = cur_finfo () in
  let c = default_context s in
  let glx_token_from_pxp_event x = next_pxp_event fi_ref c pp in
  let stream = Cursor.cursor_of_function glx_token_from_pxp_event in
  let schema = get_schema_of_stream stream in
  (schema, stream)

let open_xml_stream_from_io gio =
  let pp,s = open_pxp_stream_from_io gio Galax_io.Document_entity in
  open_xml_stream_from_pxp_stream (pp,s)

let open_xml_entity_stream_from_io gio entity_kind =
  let pp,s = open_pxp_stream_from_io gio entity_kind in
  open_xml_stream_from_pxp_stream (pp,s)

(* Parse a stand-alone DTD *)

let get_dtd gio =
  Galax_io.dtd_from_input_spec gio

let parse_standalone_dtd gio =
  get_dtd gio

