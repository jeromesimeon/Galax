(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2009.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module: Galax_main
   Description:
     Main for the galax executable.
 *)


(*************)
(* Let's go! *)
(*************)

open Top_args
open Top_util

let rec dispatch_go ek gargs =
  match ek with
  | ExecHelp ->
      (* Makes sure it maps glx help <subcommand> back to glx <subcommand> -help *)
      begin
	match Top_args.exec_help_go gargs with
	| None -> ()
	| Some (rek,rgargs) ->
	    dispatch_go rek rgargs 
      end
  | ExecXQuery ->
      Top_run.go gargs
  | ExecXQueryCompile ->
      Top_compile.go gargs
  | ExecXML ->
      Top_parse.go gargs
  | ExecXMLSchema ->
      Top_schema.go gargs
  | ExecProject ->
      Top_project.go gargs

let go() =
  (* 1. Pre-process the arguments *)
  let ek,gargs = dispatch_args () in

  (* 2. Call the proper dispatcher *)
  dispatch_go ek gargs

let _ =
  low_exec go ()
