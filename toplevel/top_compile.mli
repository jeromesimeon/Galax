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

(* Module: Top_compile
   Description:
     The "galax compile" command can be used to process the query
     through successive compilation phases.
 *)

val go: Top_args.gargs -> unit

