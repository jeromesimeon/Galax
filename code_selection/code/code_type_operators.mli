(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_type_operators.mli,v 1.4 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_type_operators
   Description:
     This module contains code building for operators on types.
*)

open Code_selection_context
open Xquery_algebra_ast
open Xquery_common_ast

val build_treat_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	asequencetype -> 
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_validate_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	validation_mode ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_cast_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Namespace_context.nsenv * asequencetype) ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)


val build_castable_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Namespace_context.nsenv * asequencetype) ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)


