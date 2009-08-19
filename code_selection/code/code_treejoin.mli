(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_treejoin.mli,v 1.8 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_tree_join
   Description:
     This module contains code building for the TreeJoin operator.
*)

val build_treejoin_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Xquery_common_ast.axis * Xquery_algebra_ast.anode_test) ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
	  

val select_physical_op : 
    Code_selection_context.code_selection_context ->
      Xquery_algebra_ast.input_signature ->
       Algebra_type.algop_expr -> 
	 (Xquery_common_ast.axis * Xquery_algebra_ast.anode_test) ->
	   Xquery_physical_algebra_ast.physop_expr_name
