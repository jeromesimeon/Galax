(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_top.mli,v 1.8 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Optimization_top
   Description:
     This module contains top-level operation for the algebraic
     optimizer.
*)

open Ast_logical_algebra_types

val optimize_statement :
    Compile_context.logical_compile_context -> logical_algop_expr -> logical_algop_expr

val optimize_prolog :
    Compile_context.logical_compile_context ->
      logical_algop_prolog -> Compile_context.logical_compile_context * logical_algop_prolog

val optimize_library_module :
    Compile_context.logical_compile_context ->
      logical_algop_xmodule -> Compile_context.logical_compile_context * logical_algop_xmodule

val optimize_main_module :
    Compile_context.logical_compile_context ->
      logical_algop_xmodule -> Compile_context.logical_compile_context * logical_algop_xmodule

