(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_tuple.mli,v 1.7 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_tuple
   Description:
     This is the code for basic tuple operations.
*)

open Algebra_type
open Code_selection_context
open Execution_context
open Namespace_names
open Namespace_symbols
open Physical_value
open Xquery_algebra_ast
open Xquery_common_ast

val build_input_tuple_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
	  
val build_create_tuple_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	(asequencetype option * crname) array -> 
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_access_tuple_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	crname ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_tuple_concat_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_tuple_product_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_tuple_select_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	predicate_desc ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)


