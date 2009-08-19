(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_sc_join.mli,v 1.4 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_sc_join
   Description:
     This module contains code building for operators that implement
     StairCase Joins.
*)

open Algebra_type
open Xquery_common_ast
open Xquery_algebra_ast

open Physical_value
open Execution_context
open Code_selection_context

val build_staircase_join_code :
    code_selection_context -> crname -> Xquery_algebra_ast.twig_pattern ->
      (unit -> eval_fun -> algebra_context ->  tuple_unit Cursor.cursor -> tuple_unit Cursor.cursor)
