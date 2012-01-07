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

(* Module: Top_tokenize
   Description:
     The "galax tokenize" command can be used to produce a stream of
     tokens from the query through the lexer.
 *)

open Format
open Print_top
open Error
open Dm

open Galax_server

open Processing_context
open Monitoring_context

open Top_util
open Top_config
open Top_options

open Procmod_compiler


(************************)
(* Command-line options *)
(************************)

let process_args proc_ctxt gargs =
  let args =
    make_options_argv
      proc_ctxt
      (usage_galax_compile ())
      [ Misc_Options;Monitoring_Options;Context_Options;Behavior_Options;ProcessingPhases_Options;Printing_Options ]
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
    let tokens = Parse_top.tokens_from_file mod_file in
    Parse_top.print_tokens true Format.std_formatter tokens
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

  (* 2. Parses the command-line arguments *)
  let module_files = process_args proc_ctxt gargs in

  (* 3. Compile the input queries *)
  exec main proc_ctxt module_files

