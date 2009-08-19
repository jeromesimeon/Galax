(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_update.mli,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Cs_code_update
   Description:
     This module contains the evaluation code for XML Query update
     operations. Additionally it contains the implementation of snaps
     and deltas.
*)

open Xquery_algebra_ast
open Xquery_common_ast

val build_copy_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
	  
val build_delete_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
	  
val build_insert_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	algop_insert_location ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
	  
val build_rename_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	Namespace_context.nsenv ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
	  
val build_replace_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	value_of_flag ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
	  
val build_snap_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	snap_modifier ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
	  





	  

