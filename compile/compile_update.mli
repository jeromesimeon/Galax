(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: compile_update.mli,v 1.6 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Compile_update
   Description:
     This module compiles an XQuery core update into the XQuery
     algebra.
*)

open Xquery_core_ast
open Xquery_algebra_ast

val compile_cupdate :
    ('a option, unit) Compile_context.compile_context ->
      Xquery_core_ast.acupdate -> ('a option, unit) Xquery_algebra_ast.aalgop_expr

