(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax-mapschema.ml,v 1.21 2007/02/01 22:08:55 simeon Exp $ *)

(* Module: Galax-compile
   Description:
     This module contains the main function for the galax-mapschema
     command. This command takes an XML Schema and produces the
     proprietary XQuery system syntax supported by Galax.
 *)

open Format
open Error
open Galax_io

open Top_util
open Top_config

let prefix_opt 		   = ref None 
let verbose    		   = ref false

(* XML Schema - XQuery types processing phases *)
let map_dtd                = ref false
let import_type            = ref true
let normalize_type         = ref false
let print_type             = ref true
let print_normalized_type  = ref false
let output_type		   = ref None  (* Set to a filename *)
let output_normalized_type = ref None  (* Set to a filename *)

let usage_msg = 
  sprintf "Usage: %s [-prefix namespaceprefix] [-import-type on/off] [-normalize-type on/off] [-print-type on/off] [-print-normalized-type on/off] [-output-type file] [-output-normalized-type file] [-verbose on/off] [-dtd] xmlschema-or-dtd(s)" Sys.argv.(0)

let process_args proc_ctxt =
  let args = ref [] in
  Arg.parse
    [ "-prefix", Arg.String (fun f  -> prefix_opt := Some (Namespace_names.NSPrefix f)), "Namespace prefix";
      "-verbose", Arg.String (fun onoff -> verbose := bool_of_onoff(onoff)), " Set printing to verbose";
      "-dtd", Arg.Set map_dtd, " Map a DTD";
      "-import-type", Arg.String (fun onoff -> import_type := bool_of_onoff(onoff)), " Set XML Schema import";
      "-normalize-type", Arg.String (fun onoff -> normalize_type := bool_of_onoff(onoff)), " Set XQuery type normalization";
      "-print-type", Arg.String (fun onoff -> print_type := bool_of_onoff(onoff)), " Set printing ofXQuery type";
      "-print-normalized-type", Arg.String (fun onoff -> print_normalized_type := bool_of_onoff(onoff)), " Set printing of normalized XQuery type";
      "-output-type" , Arg.String (fun x -> output_type := Some x), "Writing XQuery type in file";
      "-output-normalized-type" , Arg.String (fun x -> output_normalized_type := Some x), "Writing normalized XQuery type in file"; ]
      (fun arg -> args := arg :: !args) usage_msg;
  match !args with
    [] -> failwith ("Input file not specified\n" ^ usage_msg)
  | fnames ->
      fnames

let process_file proc_ctxt schema_file =
  if !import_type
  then
    begin
      let xschema = 
	if (!map_dtd) then
	  Schema_dtd_import.import_dtd (Streaming_parse.parse_standalone_dtd (File_Input schema_file))
	else
	  let (_, raw_xml_stream) = Streaming_parse.open_xml_stream_from_io (File_Input schema_file) in
	  let resolved_xml_stream = Streaming_ops.resolve_xml_stream raw_xml_stream in 
	  let schema = Schema_import.import_schema_document proc_ctxt !prefix_opt resolved_xml_stream  in
	  schema
      in
	begin
	if !print_type
	then
	  begin
	    if !verbose
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
	    if !verbose
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
	let cxschema = Schema_norm.normalize (Namespace_context.default_xml_nsenv) xschema in
	begin
	  if !print_normalized_type
	  then
	    begin
	      if !verbose
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
	      if !verbose
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

let wrap_process_file proc_ctxt schema_file =
  if !verbose
  then
    begin
      printf "---------------------------------------\n";
      if (!map_dtd) then 
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


let main () =
  let proc_ctxt = Processing_context.default_processing_context () in
  let input_files = process_args proc_ctxt in
  List.iter (wrap_process_file proc_ctxt) input_files

let exec func arg =
  try
    func arg
  with
  | e ->
      begin
	eprintf_error "  " e;
	fprintf err_formatter "@."
      end

let _ =
  exec main()

