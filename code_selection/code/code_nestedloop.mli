(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_nestedloop.mli,v 1.7 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_nestedloop
   Description:
     This module contains code building for operators that implement
     nested loops.
*)

open Algebra_type
open Xquery_common_ast
open Xquery_algebra_ast

open Physical_value
open Execution_context
open Code_selection_context


val build_tuple_nodup_code :
    code_selection_context -> crname ->
      (algebra_context -> tuple_unit Cursor.cursor -> tuple_unit Cursor.cursor)

val build_distinct_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	crname ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_default_tuple_tree_pattern_code  :
    code_selection_context -> crname -> Xquery_algebra_ast.twig_pattern ->
      (unit -> eval_fun -> algebra_context ->  tuple_unit Cursor.cursor -> tuple_unit Cursor.cursor)

(* fixme, move this *)
val effective_boolean_value : Physical_value.item Cursor.cursor -> Datatypes.xs_boolean

(* fime move this to ast util *)
val get_treejoin_attrs :
    Xquery_algebra_ast.twig_pattern ->
      Xquery_common_ast.axis * int ->
	Namespace_names.rqname * Xquery_common_ast.axis *  Xquery_algebra_ast.anode_test

