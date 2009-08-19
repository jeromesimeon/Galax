(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cs_annotate.mli,v 1.6 2007/05/23 22:12:59 simeon Exp $ *)

(* Module: Cs_annotate
   Description:
     This module implements algebraic annotation for the physical
     layer.
*)

open Logical_algebra_types
open Algebra_type

(* NOTE:
   Annotation is really a separate 'sub-phase', that deals with
   annotations needed for physical operators, notably it decides on
   which physical types should be used for each operator.
*)

val annotate_context   : logical_compile_context -> alg_compile_context
val annotate_statement : alg_compile_context -> logical_algop_expr -> algop_expr
val annotate_expr      : alg_compile_context -> logical_algop_expr -> algop_expr
val annotate_prolog    :
    logical_compile_context -> logical_algop_prolog ->
      algop_prolog * alg_compile_context
val annotate_prolog_with_bindings :
    alg_compile_context -> logical_algop_prolog ->
      algop_prolog * alg_compile_context
val annotate_module :
    logical_compile_context -> logical_algop_xmodule ->
      algop_xmodule * alg_compile_context
val annotate_module_with_bindings :
    alg_compile_context -> logical_algop_xmodule ->
      algop_xmodule * alg_compile_context

val make_temp_annotated_expr :
  Xquery_algebra_ast.algop_expr_name ->
    algop_sub_exprs -> algop_sub_exprs -> Finfo.finfo -> algop_expr

