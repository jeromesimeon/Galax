(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax-parse.ml,v 1.36 2007/02/01 22:08:55 simeon Exp $ *)

(* Module: Galax-parse
   Description:
     This module implements a simple front-end for Galax XML
     parser/validator and serialization.
 *)

open Format

open Error

open Xquery_ast

open Processing_context
open Monitoring_context

open Top_util
open Top_options
open Top_config


(*****************)
(* Schema import *)
(*****************)

(* Imports an XML Schema *)

let get_schema () =
  match !schemafile with
  | None ->
      failwith "Could not load the schema for validation"
  | Some uri ->
      (* Create a fresh processing context for the schema resource *)
      let proc_ctxt         	 = Processing_context.default_processing_context() in
      (* SAX parsing *)
      let (_, xml_schema_stream) = Streaming_parse.open_xml_stream_from_io (Galax_io.Http_Input uri) in
      (* Followed by namespace resolution *)
      let resolved_schema_stream = Streaming_ops.resolve_xml_stream xml_schema_stream in
      (* Now schema import *)
      let imported_schema        = Schema_import.import_schema_document proc_ctxt None resolved_schema_stream in
      (* Finally, schema normalization *)
      let normalized_schema      = Schema_norm.normalize (Namespace_context.default_xml_nsenv) imported_schema in
      normalized_schema


(******************)
(* Parsing phases *)
(******************)

exception DONE of int

let consume_xml_stream xml_stream =
  let i = ref 0 in
  try
    while (ignore(Cursor.cursor_next xml_stream);true) do incr i
    done;
    raise (DONE !i)
  with
  | Stream.Failure ->
      raise (DONE !i)

let consume_resolved_xml_stream = consume_xml_stream

(* Serialization *)

let serialize proc_ctxt stream =
  if !Conf.print_xml
  then
    begin
      Serialization.serialize_xml_stream proc_ctxt stream;
      raise (DONE 0)
    end
  else
    consume_xml_stream stream

(* Prefix *)

let prefix proc_ctxt resolved_stream =
  if !prefix
  then
    let stream = Streaming_ops.prefix_xml_stream resolved_stream in
    serialize proc_ctxt stream
  else
    consume_xml_stream resolved_stream

(* Erase *)

let erase proc_ctxt typed_stream =
  if !erase
  then
    let resolved_stream = Streaming_ops.erase_xml_stream typed_stream in
    prefix proc_ctxt resolved_stream
  else
    consume_xml_stream typed_stream

(* Parsing *)

let parse_pxp proc_ctxt uri_string =
  let gio = Galax_io.Http_Input uri_string in
  let (pxp_stream,s) = Streaming_parse.open_pxp_stream_from_io gio Galax_io.Document_entity in
  pxp_stream

let consume_pxp pp =
  let e = ref None in
  let i = ref 0 in
  while (e := (fst pp) ()); (!e != None) do incr i
  done;
  raise (DONE !i)

let parse_galax proc_ctxt uri_string =
  (* Parsing, followed by namespace resolution *)
  let gio = Galax_io.Http_Input uri_string in
  let (dtdopt, xml_stream) = Streaming_parse.open_xml_stream_from_io gio in
  let sopt =
    match dtdopt with
    | None -> None
    | Some dtd -> Some (Schema_dtd_import.import_dtd dtd)
  in
  (sopt, xml_stream)

let parse proc_ctxt uri_string =
  if (!pxp)
  then
    if (!stream)
    then
      parse_galax proc_ctxt uri_string
    else
      consume_pxp (parse_pxp proc_ctxt uri_string)
  else
    raise (DONE 0)

let resolve proc_ctxt xml_stream =
  if (!resolve)
  then
    Streaming_ops.resolve_xml_stream xml_stream
  else
    serialize proc_ctxt xml_stream

(* Validation *)

let validate proc_ctxt opt_cxschema resolved_xml_stream =
  if (!annotate)
  then
    if (!validation)
    then
      begin
	match opt_cxschema with
	| None ->
	    failwith "Cannot validate without a schema"
	| Some cxschema ->
	    Schema_validation.validate cxschema resolved_xml_stream
      end
    else
      Streaming_ops.typed_of_resolved_xml_stream resolved_xml_stream
  else
    prefix proc_ctxt resolved_xml_stream

(* Export *)

let export proc_ctxt dm =
  if !export
  then
    let typed_stream = Physical_export.typed_xml_stream_of_datamodel dm in
    erase proc_ctxt typed_stream
  else
    raise (DONE 0)

(* Data model loading *)

let load proc_ctxt uri typed_xml_stream =
  if (!load)
  then
    begin
      let apply_load_document () =
	let nodeid_context      = Nodeid_context.default_nodeid_context () in
	Physical_load.load_xml_document_from_typed_stream nodeid_context typed_xml_stream
      in
      begin
	let res =
	  Monitor.wrap_monitor proc_ctxt (Document_ParsingLoading_Phase (AnyURI._string_of_uri uri)) apply_load_document ()
	in
	export proc_ctxt (Cursor.cursor_of_list res)
      end
    end
  else
    erase proc_ctxt typed_xml_stream

(*************************)
(* Process a single file *)
(*************************)

let process_file_aux proc_ctxt uri_string =
  let uri = AnyURI._actual_uri_of_string uri_string in
  (* Parsing -- check for well-formedness *)
  let (dtdopt, xml_stream) = parse proc_ctxt uri_string in
  let resolved_xml_stream = resolve proc_ctxt xml_stream in
  (* Validation -- if requested *)
  let opt_cxschema =
    if (!validation)
    then
      if (!dtd) then 
	match dtdopt with
	| Some xschema -> Some (Schema_norm.normalize (Namespace_context.default_xml_nsenv) xschema)
	| None -> raise (Query(Error("No DTD found in "^uri_string)))
      else Some (get_schema ())
    else
      None
  in
  let typed_xml_stream = validate proc_ctxt opt_cxschema resolved_xml_stream in
  (* Data model loading and serialization -- if requested *)
  let _ = load proc_ctxt uri typed_xml_stream in
  ()

let process_file proc_ctxt uri_string =
  try
    process_file_aux proc_ctxt uri_string
  with
  | DONE i -> Printf.printf "Processed %i events!\n" i

(**************************)
(* Command line arguments *)
(**************************)

let process_args proc_ctxt =
  let args =
    make_options
      proc_ctxt
      usage_galax_parse
      [ GalaxParse_Options;Misc_Options;Monitoring_Options;Encoding_Options;DataModel_Options;Serialization_Options;PrintParse_Options ]
  in
  match args with
  | [] -> failwith "Input file(s) not specified"
  | fnames ->
      List.rev fnames


(********)
(* Main *)
(********)

let main proc_ctxt input_files =
  List.iter (process_file proc_ctxt) input_files



(*************)
(* Let's go! *)
(*************)


let parse_typed_xml_stream_from_io pc gio =
	  (* 1. Open a SAX cursor on the input document *)
  let (diff_opt, xml_stream) = Streaming_parse.open_xml_stream_from_io gio  in
	  (* 2. Resolve namespaces *)
  let resolved_xml_stream = Streaming_ops.resolve_xml_stream xml_stream in
	  (* 3. Apply type annotations *)
  let typed_xml_stream = Streaming_ops.typed_of_resolved_xml_stream resolved_xml_stream in
  typed_xml_stream


let go () =
  let proc_ctxt = Processing_context.default_processing_context() in
  let input_files = process_args proc_ctxt in
  (* Test if -diff is passed *)

  if (!Top_config.diff) then 
    let output_file = List.hd input_files in
    let expected_file = List.hd (List.tl input_files) in
    let t1 = parse_typed_xml_stream_from_io proc_ctxt (Galax_io.File_Input output_file) in
    let t2 = parse_typed_xml_stream_from_io proc_ctxt (Galax_io.File_Input expected_file) in
    let b = Streaming_diff.stream_boolean_diff t1 t2 in
    if (not b) then print_string (output_file^" and "^expected_file^" differ\n")
    else ()
  else
    exec main proc_ctxt input_files

let _ =
  low_exec go ()
