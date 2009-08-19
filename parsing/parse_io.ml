(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parse_io.ml,v 1.24 2007/09/07 19:03:40 mff Exp $ *)

open Error

open Pxp_ev_parser
open Pxp_types

(* Module: Io
   Description:
     This module contains some basic operations to manipulate I/O's
     from Galax.
*)


(************************)
(* Smart lexing buffers *)
(************************)

(* Internal stuff used to set-up Pxp resolvers *)

class warner =
  object 
    method warn w =
      print_endline ("WARNING: " ^ w)
  end

(* Resolvers *)

type resolver =
    Pxp_reader.resolver

let resolve_as_file () =
  new Pxp_reader.resolve_as_file()

let resolve_as_string s =
  let is = new Netchannels.input_string s in
  new Pxp_reader.resolve_to_this_obj_channel is

let resolve_as_channel ic =
  new Pxp_reader.resolve_read_this_channel ic

let init_resolver resolver encoding =
  begin
    resolver # init_rep_encoding encoding;
    let w = new warner in
    resolver # init_warner None w
  end

let close_resolver resolver =
  resolver # close_in

let change_encoding resolver enc_string =
  try
    resolver # change_encoding enc_string
  with
  | _ -> 
      raise (Query (Undefined ("Undefined character encoding: " ^ enc_string)))

let open_resolver resolver fname =
  try
    let start_url =
      Pxp_reader.make_file_url fname in
    let ext_id = 
      Pxp_types.System (Neturl.string_of_url start_url) in
    let resolver_id = Pxp_types.resolver_id_of_ext_id ext_id in
    let lexer_source = resolver # open_rid resolver_id in
    Lazy.force lexer_source.Pxp_reader.lsrc_lexbuf
  with
  | _ ->
      raise (Query (Undefined ("file: " ^ fname ^ " not found")))

let open_resolver_no_name resolver =
  try
    let pid = Pxp_types.allocate_private_id() in
    let ext_id = Pxp_types.Private pid in
    let resolver_id = Pxp_types.resolver_id_of_ext_id ext_id in
    let lexer_source = resolver # open_rid resolver_id in
    Lazy.force lexer_source.Pxp_reader.lsrc_lexbuf
  with
  | _ ->
      raise (Query (Undefined ("Resolver initialization failure")))

(**************************)
(* Galax Input Operations *)
(**************************)

(* Note:
     galax_input's include a ready-to use lexing buffer, and must be
     closed after use.
*)

type galax_input = (Lexing.lexbuf * resolver * in_channel option)

let galax_lexbuf_from_file file =
  (* Make sure the finfo locator know which file is processed *)
  Finfo.set_current_file file;
  (* This is where the lexing buffer is created using the nice PXP reader *)
  let resolver = resolve_as_file () in
  init_resolver resolver (Encoding.get_internal_encoding ());
  (open_resolver resolver file,resolver, None)

let galax_lexbuf_from_string s =
  let resolver = resolve_as_string s in
  init_resolver resolver (Encoding.get_internal_encoding ());
  (open_resolver_no_name resolver,resolver, None)

let galax_lexbuf_from_netbuffer nb =
  let (in_channel,close) = Netchannels.create_input_netbuffer nb in
  close ();
  let resolver = new Pxp_reader.resolve_to_this_obj_channel in_channel in
  init_resolver resolver (Encoding.get_internal_encoding ());
  (open_resolver_no_name resolver,resolver, None)

let galax_lexbuf_from_http uri =
  match Galax_url.glx_decode_url uri with
  | Galax_url.Http (host,port,local) ->
      (* Note: This is pretty inefficient here. - Jerome *)
      galax_lexbuf_from_string (Http.HTTP.get uri)
  | Galax_url.File local ->
      galax_lexbuf_from_file local
  | Galax_url.ExternalSource _ ->
      raise (Query (Prototype "Cannot parse external source"))

let galax_lexbuf_from_channel ic =
  let resolver = resolve_as_channel ic in
  init_resolver resolver (Encoding.get_internal_encoding ());
  (open_resolver_no_name resolver,resolver,Some ic)


(* Takes a galax input and creates an appropriate lexbuf *)

let galax_input_from_input_spec proc_ctxt gio =
  let res =
  match gio with
  | Galax_io.File_Input s ->
      galax_lexbuf_from_file s
  | Galax_io.String_Input s ->
      galax_lexbuf_from_string s
  | Galax_io.Buffer_Input s ->
      galax_lexbuf_from_netbuffer s
  | Galax_io.Http_Input uri ->
      let base_uri = AnyURI.default_galax_base_uri () in
      let relative_uri = AnyURI._kinda_uri_of_string uri in
      let _ = AnyURI._uri_resolve base_uri relative_uri in
      galax_lexbuf_from_http uri
  | Galax_io.Channel_Input ic ->
      galax_lexbuf_from_channel ic
  in
  res

let lexbuf_from_galax_input (lexbuf,_,_) = lexbuf

let set_galax_input_encoding (_,resolver,_) s =
  change_encoding resolver s

let galax_utf8_input_from_input_spec proc_ctxt gio =
  let lb = galax_input_from_input_spec proc_ctxt gio in 
  (* M & J: PXP interface requires that input encoding be set
     explicitly after reading the first 2 bytes of the document or
     after reading the XML declaration; The encoding is necessary
     to populate the lex buffer more than one character at a
     time. *)
  set_galax_input_encoding lb "";
  lb

let parse_error_msg gio =
  match gio with
  | Galax_io.File_Input fname ->
      ("Parse error in file " ^ fname)
  | Galax_io.String_Input _ ->
      "Parse error in string"
  | Galax_io.Buffer_Input _ ->
      "Parse error in buffer"
  | Galax_io.Http_Input uri ->
      ("Parse error in resource at: " ^ uri)
  | Galax_io.Channel_Input _ ->
      ("Parse error in input channel")

let name_of_input_spec gio = 
  match gio with
  | Galax_io.File_Input s -> s
  | Galax_io.String_Input s -> "String_Input"
  | Galax_io.Buffer_Input s -> "Buffer_Input"
  | Galax_io.Http_Input uri -> uri
  | Galax_io.Channel_Input uri -> "Channel_Input"

let close_galax_input (_,resolver,gin) =
  Finfo.set_current_file ""; (* Reset the current file *)
  close_resolver resolver;  (* Close the resolver *)
  match gin with            (* Close the input channel if it was given *)
  | None -> ()
  | Some c ->
      close_in c

(***************************)
(* Galax Output Operations *)
(***************************)

(* Note:
     galax_output's may include output file descriptors and must be
     closed after use.
*)

type galax_output =
  | File_Output_Internal of out_channel
  | Buffer_Output_Internal of Buffer.t
  | Channel_Output_Internal of out_channel
  | Formatter_Output_Internal of Format.formatter


(* Creates an internal I/O from an external, Galax I/O *)

let galax_output_from_output_spec output_spec =
  match output_spec with
  | Galax_io.File_Output fname ->
      File_Output_Internal (open_out fname)
  | Galax_io.Buffer_Output buff ->
      Buffer_Output_Internal buff
  | Galax_io.Channel_Output oc ->
      Channel_Output_Internal oc
  | Galax_io.Formatter_Output f ->
      Formatter_Output_Internal f


(* Creates an output formatter out of a galax output *)

let formatter_of_galax_output gout =
  match gout with
  | File_Output_Internal oc ->
      Format.formatter_of_out_channel oc
  | Buffer_Output_Internal buff ->
      Format.formatter_of_buffer buff
  | Channel_Output_Internal oc ->
      Format.formatter_of_out_channel oc
  | Formatter_Output_Internal f ->
      f


(* Closes a Galax output *)

let close_galax_output gout =
  match gout with
  | File_Output_Internal oc ->
      close_out oc
  | Buffer_Output_Internal buff -> ()
(*      Buffer.reset buff *)
  | Channel_Output_Internal oc ->
      () (* Do not close if the original output spec was already an
            output channel -- let the user do it in this case. -
            Jerome *)
  | Formatter_Output_Internal oc ->
      () (* Do not close if the original output spec was already a
            formatter -- let the user do it in this case. - Jerome *)



