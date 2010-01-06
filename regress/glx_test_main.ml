(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax-test.ml,v 1.26 2007/08/17 18:28:54 simeon Exp $ *)

(* Module: Galax-test
   Description:
     The galax-test command can be used to process tests following the
     W3C XML Query test suite format.
 *)

open Galax
open Galax_io

open Error
open Processing_context

open Datatypes
open Dm_atomic
open Dm

open Streaming_types
open Streaming_util
open Streaming_ops
open Streaming_constructors
open Streaming_diff

open Top_options
open Test_core


(********)
(* Main *)
(********)

let run_unit all_bugs name version unit =
  begin
    Printf.printf "Running %s unit\n" unit.unit_id;
    Printf.printf "--------------------\n\n";
    make_catalog unit ();
    let result_file = unit.result_file in
    Conf.warning := false;
    let all_groups = run_groups unit all_bugs in
    let all = get_all_results all_groups in
    Printf.printf "Producing result file...";flush stdout;
    let r1 = total_result (name,version,all) in
    Printf.printf "Serializing result file ...";flush stdout;
    let oc = open_out result_file in
    begin
      serialize_as_sax proc_ctxt (Channel_Output oc) r1;
      Printf.printf "done.\n\n";flush stdout;
      close_out oc
    end
  end

let run_tests test_config () =
  try
    let all_bugs = get_all_bugs test_config in
    List.iter
      (fun (name,unit) -> run_unit all_bugs name !(unit.unit_version) unit)
      test_config.units
  with
  | exn -> Error.printf_error "ERROR!" exn 

let usage_galax_test = "Usage: %s configfile"

let _ =
  try
    let args =
      make_options
	proc_ctxt
	usage_galax_test
	[ Misc_Options;Encoding_Options;DataModel_Options;Behavior_Options;ProcessingPhases_Options;Optimization_Options;CodeSelection_Options;Runtime_Options;Testing_Options ]
    in
    let file =
      match args with
      | [] -> failwith "Input file(s) not specified"
      | [fname] ->
	  fname
      | _ -> failwith "Cannot have several config files"
    in
    Printf.printf "\n\n";
    Printf.printf "===========================\n";
    Printf.printf " Running XQuery test suite\n";
    Printf.printf "===========================\n\n";
    let test_config = top_make_test_config file in
    run_tests test_config ()
  with
  | e ->
      begin
	eprintf_error "  " e;
	Format.fprintf (!Conf.glx_err_formatter) "@.";
	exit 1; 
      end


