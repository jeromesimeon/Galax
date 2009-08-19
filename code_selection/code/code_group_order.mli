(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_group_order.mli,v 1.6 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_group_order
   Description:
     This module contains code building for operators that implement
     group-by.
*)

open Algebra_type
open Code_selection_context
open Execution_context
open Physical_value
open Xquery_common_ast

type select_single_op_fn =   
    Code_selection_context.code_selection_context -> algop_expr -> 
      Code_selection_context.code_selection_context

val build_order_by_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	(stablekind * (sortkind * emptysortkind) list * Xquery_algebra_ast.aoverloaded_signature_table) ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_group_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	select_single_op_fn ->
	  Xquery_algebra_ast.group_desc list -> 
	    (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
  
