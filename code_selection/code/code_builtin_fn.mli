(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_builtin_fn.mli,v 1.7 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_builtin_fn
   Description:
     This module contains code building for built-in functions.
*)

open Xquery_algebra_ast
open Xquery_common_ast

val build_builtin_fn_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	(cfname * int) * asequencetype option array * asequencetype ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
	  

val build_builtin_fn_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	(cfname * int) * asequencetype option array * asequencetype ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
	  
val build_builtin_fn_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	(cfname * int) * asequencetype option array * asequencetype ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
	  
val build_convert_simple_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	Datatypes.atomic_type ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_promote_numeric_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	Datatypes.atomic_type ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_promote_anystring_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_unsafe_promote_numeric_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	Datatypes.atomic_type ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val select_physical_op : 
    Code_selection_context.code_selection_context ->
      Xquery_algebra_ast.input_signature ->
       Algebra_type.algop_expr -> 
         (cfname * int) * asequencetype option array * asequencetype ->
	   Xquery_physical_algebra_ast.physop_expr_name
