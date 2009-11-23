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

let dispatch_go ek gargs =
  match ek with
  | XQueryExec ->
      Top_run.go gargs
  | XQueryCompileExec ->
      Top_compile.go gargs
  | XMLExec ->
      Top_parse.go gargs

let go() =
  (* 1. Pre-process the arguments *)
  let ek,gargs = dispatch_args () in

  (* 2. Call the proper dispatcher *)
  dispatch_go ek gargs

let _ =
  low_exec go ()
