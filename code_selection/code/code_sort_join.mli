(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_sort_join.mli,v 1.4 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_sc_join
   Description:
     This module contains code building for operators that implement
     sort-joins.
*)

open Xquery_common_ast
open Algebra_type
open Code_selection_context

val build_sort_join :
    Code_util_join.outer_kind -> code_selection_context -> 
      Code_util_predicates.predicate_functions ->
	Xquery_algebra_ast.predicate_desc ->
	  alg_eval_code_dep  * code_selection_context

val build_sort_join_with_btree_index :
    Code_util_join.outer_kind -> code_selection_context -> Physical_name_index.name_index_handler ->
      Code_util_predicates.predicate_functions ->
	Xquery_algebra_ast.predicate_desc -> 
	  alg_eval_code_dep  * code_selection_context

