(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: print_type.mli,v 1.2 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Print_type
   Description:
     This module implements pretty-printing for the XQuery type
     system.
*)

val print_xschema   : Format.formatter -> Xquery_type_ast.xschema -> unit
val bprintf_xschema : string -> Xquery_type_ast.xschema -> string

