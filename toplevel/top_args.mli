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

(* Module: Top_args
   Description:
     This module contains code for generic processing of galax
     executable command-line arguments.
 *)

type executable_kind =
  | XQueryExec
  | XQueryCompileExec
  | XMLExec

type gargs = string array

val dispatch_args: unit -> executable_kind * gargs

