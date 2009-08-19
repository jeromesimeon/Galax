(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_hash_join.mli,v 1.4 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_hash_join
   Description:
     This module contains code building for operators that implement
     hash joins.
*)

open Algebra_type
open Xquery_common_ast
open Code_selection_context

val build_hash_join :
    Code_util_join.outer_kind -> code_selection_context -> 
      Code_util_predicates.predicate_functions ->
	Xquery_algebra_ast.predicate_desc -> 
	  alg_eval_code_dep  * code_selection_context

