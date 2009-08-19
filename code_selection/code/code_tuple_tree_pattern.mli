(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_tuple_tree_pattern.mli,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_tuple_tree_pattern
   Description:
     This module contains code building for the TTP operator.
*)

open Xquery_algebra_ast
open Xquery_common_ast

val build_tuple_tree_pattern_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	(crname * twig_pattern) ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
	  

val select_physical_op :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	(crname * twig_pattern) ->
	  Xquery_physical_algebra_ast.physop_expr_name
	  
	  

