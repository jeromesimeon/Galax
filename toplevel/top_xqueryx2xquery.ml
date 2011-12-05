(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xqueryx2xquery.ml,v 1.3 2007/08/09 20:21:22 simeon Exp $ *)

(* Module: Top_xqueryx2xquery
   Description:
     Maps an XQueryX into an XQuery.
 *)

open Format
open Print_top
open Error
open Dm

open Processing_context
open Monitoring_context

open Top_util
open Top_config
open Top_options

open Procmod_compiler


(************************)
(* Command-line options *)
(************************)

let process_args proc_ctxt =
  let args =
    make_options
      proc_ctxt
      (usage_galax_compile ())
      [ Misc_Options;Monitoring_Options;Context_Options;Behavior_Options;ProcessingPhases_Options;Printing_Options;Optimization_Options;CodeSelection_Options;XQueryX_Options ]
  in
  match args with
  | [] -> failwith "Input file(s) not specified"
  | fnames ->
      List.rev fnames

let override_args proc_ctxt args =
  make_options_argv
    proc_ctxt
    (usage_galax_compile ())
    [ Misc_Options;Monitoring_Options;Context_Options;Behavior_Options;ProcessingPhases_Options;Printing_Options;Optimization_Options;CodeSelection_Options;XQueryX_Options ]
    args

(*********************************************************************************)
(* NOTO BENE!                                                                    *)
(*                                                                               *)
(* For accurate monitoring of execution time, Galax-run MUST delegate            *)
(* to the Galax O'Caml API, which ensures that every API function is monitored.  *)
(*                                                                               *)
(* Do NOT call the compiler directly, as this circumvents the monitor            *)
(* and will yield imprecise results.  If some functionality is missing           *)
(* from the API, then let Mary know.                                             *)
(*********************************************************************************)

open Streaming_types

let check_doc s =
  match (Cursor.cursor_next s).se_desc with
  | SAX_startDocument _ -> ()
  | _ -> raise Not_found

let check_end_doc s =
  match (Cursor.cursor_next s).se_desc with
  | SAX_endDocument -> ()
  | _ -> raise Not_found

let rec conv_atts atts =
  match atts with
  | [] -> []
  | (aname,atext) :: atts' ->
      (Xquery_ast_util.fmkexpr
	 (Xquery_ast.EAttrFixed(aname,
				[(Xquery_ast_util.fmkexpr (Xquery_ast.EText atext) Finfo.bogus)])) Finfo.bogus)
      :: (conv_atts atts')

let check_elem s =
  let nsenv = Namespace_context.default_xml_nsenv in
  match (Cursor.cursor_next s).se_desc with
  | SAX_startElement (name,atts,_,_,_,_) ->
      begin
	let atts = List.map (fun (x,y,_,_,_) -> (x,y)) atts in
	let (new_nss, other_atts) = Xquery_ast_util.get_ns_attributes (conv_atts atts) in
	let nsenv' = Namespace_context.add_all_ns nsenv new_nss in
	let cename = Namespace_resolve.resolve_element_qname nsenv' name in
	if (Namespace_names.rqname_equal Namespace_builtin.xqx_xquery cename)
	then
	  (name,other_atts)
	else
	  raise Not_found
      end
  | _ -> raise Not_found

let rec check_content_aux s =
  match (Cursor.cursor_next s).se_desc with
  | SAX_processingInstruction _ | SAX_comment _ -> check_content_aux s
  | SAX_characters t ->
      t ^ (check_content_aux s)
  | SAX_endElement -> check_end_doc s; ""
  | _ -> raise Not_found

let check_content s =
  let text = check_content_aux s in
  [Xquery_ast_util.fmkexpr (Xquery_ast.EText text) Finfo.bogus]

let ast_of_xml file =
  let (_,s) = Streaming_parse.open_xml_stream_from_io (Galax_io.File_Input file) in
  begin
    check_doc s;
    let (name,attlist) = check_elem s in
    let childlist = check_content s in
    (name,attlist,childlist)
  end

let print_mm mod_file mm =
  begin
    let s = Print_top.bprintf_main_module "" mm in
    Print_top.print_escaped_output !Conf.expr_formatter !Conf.expr_header s !Conf.expr_footer;
    flush stdout
  end

let print_lm mod_file mm =
  begin
    let s = Print_top.bprintf_library_module "" mm in
    Print_top.print_escaped_output !Conf.expr_formatter !Conf.expr_header s !Conf.expr_footer;
    flush stdout
  end

let print_im mod_file mm =
  begin
    let s = Print_top.bprintf_interface "" mm in
    Print_top.print_escaped_output !Conf.expr_formatter !Conf.expr_header s !Conf.expr_footer;
    flush stdout
  end

let get_newname oldname =
  match Gmisc.split_right_on_char oldname '.' with
  | (p,"xqx") -> p ^ ".xq"
  | _ ->  oldname ^ ".xq"

let eprint_mm mod_file mm =
  let newname = get_newname mod_file in
  let oc = open_out newname in
  let newformatter = Format.formatter_of_out_channel oc in
  begin
    Print_top.fprintf_main_module newformatter "" mm;
    Format.pp_print_flush newformatter ();
    close_out oc
  end

let eprint_lm mod_file lm =
  let newname = get_newname mod_file in
  let oc = open_out newname in
  let newformatter = Format.formatter_of_out_channel oc in
  begin
    Print_top.fprintf_library_module newformatter "" lm;
    Format.pp_print_flush newformatter ();
    close_out oc
  end

let eprint_im mod_file lm =
  let newname = get_newname mod_file in
  let oc = open_out newname in
  let newformatter = Format.formatter_of_out_channel oc in
  begin
    Print_top.fprintf_interface newformatter "" lm;
    Format.pp_print_flush newformatter ();
    close_out oc
  end

let process_library_module proc_ctxt mod_ctxt mod_file print_lm =
  begin
    print_processing_file mod_file;
    let ast =
      let (_,al,el) = ast_of_xml mod_file in
      Parse_xqueryx.normalize_xqx_xquery_library_module al el Finfo.bogus
	(*
	  let gio = Galax_io.File_Input mod_file in
	  let pac = Parse_context.build_xquery_parse_context proc_ctxt in
	  let ast = (snd (Parse_xqueryx.parse_xqx_xquery_library_module_from_io pac gio)) in
	 *)
      in
      print_lm mod_file ast
  end

let process_interface_module proc_ctxt mod_ctxt mod_file print_im =
  begin
    print_processing_file mod_file;
    let ast =
      let (_,al,el) = ast_of_xml mod_file in
      Parse_xqueryx.normalize_xqx_xquery_interface_module al el Finfo.bogus
	(*
	  let gio = Galax_io.File_Input mod_file in
	  let pac = Parse_context.build_xquery_parse_context proc_ctxt in
	  let ast = (snd (Parse_xqueryx.parse_xqx_xquery_library_module_from_io pac gio)) in
	 *)
      in
      print_im mod_file ast
  end

let process_main_module proc_ctxt mod_ctxt mod_file print_mm =
  begin
    print_processing_file mod_file;
    let ast = 
      let (_,al,el) = ast_of_xml mod_file in
      Parse_xqueryx.normalize_xqx_xquery_main_module al el Finfo.bogus
(*      
    let gio = Galax_io.File_Input mod_file in
    let proc_ctxt = Processing_context.default_processing_context() in
    let pac = Parse_context.build_xquery_parse_context proc_ctxt in
    snd (Parse_xqueryx.parse_xqx_xquery_main_module_from_io pac gio)
*)
    in
    print_mm mod_file ast
  end

let process_one_module proc_ctxt mod_ctxt mod_file (p1,p2,p3)=
  try
    try process_main_module proc_ctxt mod_ctxt mod_file p1 with
    | _ ->
	process_library_module proc_ctxt mod_ctxt mod_file p2
  with
  | _ ->
      process_interface_module proc_ctxt mod_ctxt mod_file p3

let process_module proc_ctxt mod_ctxt mod_file =
  let p1,p2,p3 =
  if !Conf.batch_xqueryx
  then (eprint_mm,eprint_lm,eprint_im)
  else (print_mm,print_lm,print_im)
  in
  process_one_module proc_ctxt mod_ctxt mod_file (p1,p2,p3)

let main proc_ctxt module_files =
  let mod_ctxt = init_all proc_ctxt in
  Monitor.start_monitor_external_call proc_ctxt "Galax-compile.main";
  List.iter (process_module proc_ctxt mod_ctxt) module_files;
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
    end


(*************)
(* Let's go! *)
(*************)

let go gargs =
  (* 1. First get the default processing context for galax-compile *)
  let proc_ctxt = galax_compile_proc_ctxt () in
  Conf.print_prolog := true;

  (* 2. Force proper options for that top-level *)
  let options =
    [| Sys.argv.(0); "-optimization"; "off" |]
  in

  (* 3. Parses the command-line arguments *)
  let module_files = override_args proc_ctxt options in

  (* 4. Compile the input queries *)
  exec main proc_ctxt module_files

