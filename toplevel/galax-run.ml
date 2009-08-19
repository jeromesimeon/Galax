(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax-run.ml,v 1.92 2007/10/26 15:49:33 simeon Exp $ *)

(* Module: Galax-run
   Description:
     This module contains the main function for the galax-run command.
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

module DummyServerKind : SERVERKIND =
  struct
    type http_request = (in_channel * out_channel * Http.HTTP.http_method * Http.HTTP.header list * string option)
    let async_eval (b:bool) eh (f: unit -> unit) =
      raise(Query(DXQ_Error("Distributed XQuery async_eval not supported")))
    let delay f  =
      raise(Query(DXQ_Error("Distributed XQuery delay not supported")))
    let http_tcp_server (b:bool) eh f s =
      raise(Query(DXQ_Error("Distributed XQuery tcp_server not supported")))
    let udp_server (b:bool) eh f s =
      raise(Query(DXQ_Error("Distributed XQuery udp_server not supported")))
  end

module DummyServer = Galax_server.Server(DummyServerKind)

(************************)
(* Command-line options *)
(************************)

let process_args proc_ctxt =
  let args =
    make_options
      proc_ctxt
      usage_galax_run
      [ Misc_Options;Monitoring_Options;Encoding_Options;Context_Options;DataModel_Options;Serialization_Options;Behavior_Options;ProcessingPhases_Options;Printing_Options;Optimization_Options;CodeSelection_Options;Runtime_Options]
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
    (* Load main module *)
    let (mod_ctxt'', stmts) =
      match !context_file with
      | None ->
	  compile_main_module_helper ext_ctxt_item mod_ctxt (Galax_io.File_Input mod_file)
      | Some f ->
	  let compiled_program = Galax.import_prolog_only mod_ctxt ext_ctxt_item (Galax_io.File_Input f) in
	  compile_main_module_helper false compiled_program (Galax_io.File_Input mod_file)
    in

    (* Evaluate all global variables *)
    let mod_ctxt''' = Galax.prepare_program mod_ctxt'' (Some ext_ctxt) in

    (* Evaluate all statements in module and serialize, if necessary *)


    (********************************************************************************)
    (* Hack! Serializes the query result from a stream iff treejoins are turned on. *)
    (* - Michael                                                                    *)
    (********************************************************************************)

    let eval_serialize_stream stmt =
      let stream = Galax.eval_compiled_statement_as_sax mod_ctxt''' stmt in
	if !Conf.print_xml then
	  Galax.serialize_as_sax proc_ctxt (Galax_io.Channel_Output !Conf.xml_output) stream
	else
	  Streaming_ops.discard_typed_xml_stream stream
    in

      if (proc_ctxt.treejoin_log = TreeJoin && proc_ctxt.treejoin_phys = Streaming)
      then
	List.iter eval_serialize_stream stmts
      else
	(* proceed as usual *)
	List.iter (fun stmt ->
		     let v = Galax.eval_compiled_statement mod_ctxt''' stmt in
		       if !Conf.print_xml then
			 Galax.serialize proc_ctxt (Galax_io.Channel_Output !Conf.xml_output) v
		       else ()) stmts;
  end


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

let main proc_ctxt module_files =
  let mod_ctxt = init_all proc_ctxt in

  Monitor.start_monitor_external_call proc_ctxt (Conf.system ^ "-run.main");
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
      close_channel_ref(Conf.factorized_expr_output);
    end;
  Monitor.end_monitor_external_call proc_ctxt


(*************)
(* Let's go! *)
(*************)

let go () =
  (* 1. First get the default processing context for galax-run *)
  let proc_ctxt = galax_run_proc_ctxt () in

  (* 2. Parses the command-line arguments *)
  let module_files = process_args proc_ctxt in

  Processing_context.set_dxq_server proc_ctxt 
    (DummyServer.evaluate_closure, DummyServer.evaluate_remote_query, DummyServer.async_eval, DummyServer.interpret_hostport_string);

  (* 3. Execute the input queries *)
  let ret = exec main proc_ctxt module_files in

  (* 4. Call the close handlers *)
  let ()  = Register_handlers.call_close_handlers () in
  begin
(*     Printf.printf "LOAD CALLED %i TIMES!\n" !Conf.countload; *)
    flush stdout;
    if (Debug.default_debug())
    then
      begin
	Debug.sprintf_default_debug "EXPORT CALLED %i TIMES!\n" !Conf.countexpo;
	Debug.sprintf_default_debug "NEXT CALLED %i TIMES!\n" !Conf.countnext
      end;
    ret
  end

let _ =
  low_exec go ()
