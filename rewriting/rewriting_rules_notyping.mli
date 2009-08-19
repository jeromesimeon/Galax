(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: rewriting_rules_notyping.mli,v 1.7 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Rewriting_rules_notyping
   Description:
     This module contains the rewriting rules that are independant of
     typing information.
*)

open Xquery_core_ast
open Ast_walker_rewrite_context

(**************************************************************************)
(* Various sets of rewriting rules : Rules are composed in procmod_phases *)
(**************************************************************************)

(* Generic rewriting rules that are independent of other optimization & typing options *)
val generic_rule_set          : Typing_context.static_context rewrite_rule_set
val generic_toplevel_rule_set : Typing_context.static_context rewrite_rule_set

type clause_rewriter =
  Typing_context.static_context rewrite_context ->
  acfl_expr list -> acfl_expr ->
  acexpr -> acfl_expr list * acexpr * bool

(* A generic function for rewriting FLWOR clauses.  Takes one list of
   clause_rewriters for LET and one for FOR. *)
val flwr_rewrite :
    clause_rewriter -> clause_rewriter -> 
      Typing_context.static_context rewrite_context ->
	acexpr -> acexpr * bool

val call_rewrite :
    Xquery_common_ast.cfname * int *
    (Typing_context.static_context rewrite_context -> acexpr -> acexpr list -> acexpr * bool) ->
      Typing_context.static_context rewrite_context -> acexpr -> acexpr * bool

