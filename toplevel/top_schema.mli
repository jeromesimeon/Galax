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

(* Module: Top_schema
   Description:
     This module contains the main function for the "galax xmlschema"
     command. This command takes an XML Schema and produces the
     proprietary XQuery system syntax supported by Galax.
 *)

val go: Top_args.gargs -> unit

