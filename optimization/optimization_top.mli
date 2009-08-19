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

open Logical_algebra_types

val optimize_statement :
    logical_compile_context -> logical_algop_expr -> logical_algop_expr

val optimize_prolog :
    logical_compile_context ->
      logical_algop_prolog -> logical_compile_context * logical_algop_prolog

val optimize_library_module :
    logical_compile_context ->
      logical_algop_xmodule -> logical_compile_context * logical_algop_xmodule

val optimize_main_module :
    logical_compile_context ->
      logical_algop_xmodule -> logical_compile_context * logical_algop_xmodule

