(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_prune.mli,v 1.6 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_parse
   Description:
     This module contains code building for operators that implement
     pruning of sequences of XML items.
*)

open Algebra_type
open Xquery_algebra_ast
open Xquery_common_ast

(* XPath evaluation *)

val build_prune_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	(crname * axis) -> 
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_default_prune_code : 
    Code_selection_context.code_selection_context ->
      Xquery_common_ast.axis ->
	Xquery_common_ast.crname ->
	  'a ->
	    Physical_value.tuple_unit Cursor.cursor ->
	      Physical_value.tuple_unit Cursor.cursor

