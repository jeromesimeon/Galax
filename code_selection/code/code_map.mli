(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_map.mli,v 1.4 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_map
   Description:
     This module contains code building for operators that implement
     maps.
*)

open Algebra_type
open Xquery_algebra_ast
open Xquery_common_ast

val build_tuple_map_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_tuple_map_index_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	cvname ->
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_null_map_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	cvname ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

(* Concat code *)
val build_tuple_map_concat_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_outer_tuple_map_concat_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	crname ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

