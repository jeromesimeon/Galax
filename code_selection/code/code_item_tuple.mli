(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_item_tuple.mli,v 1.4 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_item_tuple
   Description:
     This module contains code building for operators that implement
     item maps.
*)

open Algebra_type
open Code_selection_context 
open Xquery_algebra_ast
open Xquery_common_ast

val build_tuple_to_item_map_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_item_to_tuple_map_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	cvname ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_some_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	(asequencetype option * cvname) ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_every_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	(asequencetype option * cvname) ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

