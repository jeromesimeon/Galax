(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module: Top_schema
   Description:
     This module contains the main function for the "galax xmlschema"
     command. This command takes an XML Schema and produces the
     proprietary XQuery system syntax supported by Galax.
 *)

open Format
open Error
open Galax_io

open Top_util
open Top_options
open Top_config

let usage_msg = 
  sprintf "Usage: %s [-prefix namespaceprefix] [-import-type on/off] [-normalize-type on/off] [-print-type on/off] [-print-normalized-type on/off] [-output-type file] [-output-normalized-type file] [-verbose on/off] [-dtd] xmlschema-or-dtd(s)" Sys.argv.(0)

let process_args proc_ctxt gargs =
  let args =
    make_options_argv
      proc_ctxt
      (usage_galax_schema ())
      [ GalaxParse_Options;Misc_Options;Monitoring_Options;Encoding_Options;DataModel_Options;Serialization_Options;PrintParse_Options ]
      gargs
  in
  match args with
    [] -> failwith ("Input file not specified\n" ^ usage_msg)
  | fnames ->
      fnames

let prefix_opt () =
  match !prefix_opt with
  | None -> None
  | Some f -> Some (Namespace_names.NSPrefix f)

let process_file proc_ctxt schema_file =
  if !import_type
  then
    begin
      let xschema = 
	if (!dtd) then
	  Schema_dtd_import.import_dtd (Streaming_parse.parse_standalone_dtd (File_Input schema_file))
	else
	  let (_, raw_xml_stream) = Streaming_parse.open_xml_stream_from_io (File_Input schema_file) in
	  let resolved_xml_stream = Streaming_ops.resolve_xml_stream raw_xml_stream in 
	  let schema = Schema_import.import_schema_document proc_ctxt (prefix_opt()) resolved_xml_stream  in
	  schema
      in
	begin
	if !print_type
	then
	  begin
	    if !Conf.verbose
	    then
	      begin
		printf "Imported XQuery type\n";
		printf "--------------------\n\n";
		flush stdout;
	      end;
	    let oc = 
	      match !output_type with
	      | None -> stdout
	      | Some f -> open_out f
	    in
	    let ff = Format.formatter_of_out_channel oc in
	    begin
	      let printed_form = Print_type.bprintf_xschema "" xschema in 
	      fprintf ff "%s" printed_form;
	      match !output_type with
	      | None ->  ()
	      | Some _ -> close_out oc
	    end;
	    if !Conf.verbose
	    then
	      begin
		printf "%s" !separator;
		flush stdout;
	      end
	  end
	else
	  ()
      end;
      if !normalize_type
      then
	let cxschema = Schema_norm.normalize Namespace_context.default_xml_nsenv xschema in
	begin
	  if !print_normalized_type
	  then
	    begin
	      if !Conf.verbose
	      then
		begin
		  printf "Normalized XQuery type\n";
		  printf "----------------------\n\n";
		  flush stdout;
		end;
	      let oc = 
		match !output_normalized_type with
		| None -> stdout
		| Some f -> open_out f
	      in
	      let ff = Format.formatter_of_out_channel oc in
	      begin
		let printed_form = Print_top.bprintf_cxschema "" cxschema in
		fprintf ff "%s" printed_form;
		match !output_normalized_type with
		| None ->  ()
		| Some _ -> close_out oc
	      end;
	      if !Conf.verbose
	      then
		begin
		  printf "%s" !separator;
		  flush stdout;
		end
	    end
	  else
	    ()
	end
      else
	()
    end
  else
    if !normalize_type
    then
      raise (Query(Wrong_Args("Type normalization requires type import")))
    else
      ()

let wrap_process_file proc_ctxt mod_ctxt schema_file =
  if !Conf.verbose
  then
    begin
      printf "---------------------------------------\n";
      if (!dtd) then 
	printf "Mapping DTD: %s\n" schema_file
      else
	printf "Mapping XML Schema: %s\n" schema_file;
      printf "---------------------------------------\n";
      printf "%s" !separator;
      flush stdout;
    end;
  begin
    try
      process_file proc_ctxt schema_file
    with
    | e ->
	begin
	  eprintf_error "  " e;
	  fprintf err_formatter "@."
	end
  end

let main proc_ctxt input_files =
  let mod_ctxt = init_all proc_ctxt in
  List.iter (wrap_process_file proc_ctxt mod_ctxt) input_files;
  close_channel_ref(Conf.glx_stderr);
  close_channel_ref(Conf.xml_output);
  close_channel_ref(Conf.projection_output);
  close_channel_ref(Conf.projected_file_output);
  if not(!output_all) then
    begin
      close_channel_ref(Conf.expr_output);
      close_channel_ref(Conf.type_output);
      close_channel_ref(Conf.core_expr_output);
      close_channel_ref(Conf.optimized_expr_output);
      close_channel_ref(Conf.factorized_expr_output);
    end

let go gargs =
  (* 1. First get the default processing context for galax-run *)
  let proc_ctxt = Processing_context.default_processing_context () in

  (* 2. Parses the command-line arguments *)
  let input_files = process_args proc_ctxt gargs in

  (* 3. Execute the input queries *)
  let ret = exec main proc_ctxt input_files in

  (* 4. Call the close handlers *)
  let ()  = Register_handlers.call_close_handlers () in
  begin
    flush stdout;
    if (Debug.default_debug())
    then
      begin
	Debug.sprintf_default_debug "EXPORT CALLED %i TIMES!\n" !Conf.countexpo;
	Debug.sprintf_default_debug "NEXT CALLED %i TIMES!\n" !Conf.countnext
      end;
    ret
  end

