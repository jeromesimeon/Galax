(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery2xmlplan.ml,v 1.1 2007/02/12 21:15:35 simeon Exp $ *)

(* Module: Top_xquery2xmlplan
   Description:
     Compiles and XQuery into a logical plan in XML form.
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

let override_args proc_ctxt gargs =
  let args =
    make_options_argv
      proc_ctxt
      (usage_galax_compile ())
      [ Misc_Options;Monitoring_Options;Context_Options;Behavior_Options;ProcessingPhases_Options;Printing_Options;Optimization_Options;CodeSelection_Options ]
      gargs
  in
  match args with
  | [] -> failwith "Input file(s) not specified"
  | fnames ->
      List.rev fnames

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

let process_main_module proc_ctxt mod_ctxt mod_file =
  begin
    print_processing_file mod_file;
    let (ext_ctxt_item,ext_ctxt) = set_up_external_context proc_ctxt in
    let _ = 
      match !context_file with
      | None ->
	  compile_main_module_helper ext_ctxt_item mod_ctxt (Galax_io.File_Input mod_file) 
      | Some f ->
	  let compiled_prolog = Galax.import_prolog_only mod_ctxt ext_ctxt_item (Galax_io.File_Input f) in
	  compile_main_module_helper false compiled_prolog (Galax_io.File_Input mod_file)
    in ()
  end

let main proc_ctxt module_files =
  let mod_ctxt = init_all proc_ctxt in
  Monitor.start_monitor_external_call proc_ctxt "Galax-compile.main";
  List.iter (process_main_module proc_ctxt mod_ctxt) module_files;
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

  (* 2. Force proper options for that top-level *)
  let options = [| Sys.argv.(0); "-optimization"; "off"; "-print-plan-kind"; "xml" |] in

  (* 3. Parses the command-line arguments *)
  let module_files = override_args proc_ctxt options in

  (* 4. Compile the input queries *)
  exec main proc_ctxt module_files

