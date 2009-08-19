(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_functional_ops.mli,v 1.7 2007/08/27 18:49:03 simeon Exp $ *)

(* Module: Code_functional_ops
   Description:
     This module contains code building for operators that implement
     "functional" code (if..then..else, let bindings, etc.).
*)

open Xquery_algebra_ast
open Xquery_common_ast

val build_if_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_while_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
	  
val build_let_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	(asequencetype option * cvname) ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_var_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	cvname->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
	  
val build_set_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	cvname->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
	  


