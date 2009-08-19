(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_tj_twigstack.mli,v 1.4 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_tj_twigstack
   Description:
     This is the code for the TwigStack variant of the TwigJoin.
*)


open Algebra_type
open Xquery_common_ast
open Xquery_algebra_ast

open Physical_value
open Execution_context
open Code_selection_context

val build_holistic_tuple_tree_pattern_code :
    code_selection_context -> crname -> Xquery_algebra_ast.twig_pattern ->
      (unit -> eval_fun -> algebra_context ->  tuple_unit Cursor.cursor -> tuple_unit Cursor.cursor)
