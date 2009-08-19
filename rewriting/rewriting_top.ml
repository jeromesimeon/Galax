(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: rewriting_top.ml,v 1.4 2007/08/01 21:10:07 simeon Exp $ *)

(* Module: Rewriting_top
   Description:
     This module contains the top-level operations for the rewriting
     processing phase.
*)

open Xquery_core_ast

open Ast_walker_rewrite_context
open Ast_walker_rewrite

(* Top-level rewriting operations *)

(* Note:
     The toplevel rewriting operations here always apply to an
     annotated core expressions (with types), and always returns an
     annotated core expression (with types).
   - Jerome
   *)

type statement_rule_sets =
    Typing_context.static_context rewrite_rule_set
      * Typing_context.static_context rewrite_rule_set
type module_rule_sets =
    Typing_context.static_context rewrite_rule_set
      * Typing_context.static_context rewrite_prolog_rule_set
      * Typing_context.static_context rewrite_rule_set

let rewriting_cexpr (toplevel_rule_set, recurse_rule_set) static_ctxt acexpr =
  let recurse_rewrite_ctxt = build_rewrite_context static_ctxt recurse_rule_set [] in
  let toplevel_rewrite_ctxt = build_rewrite_context static_ctxt toplevel_rule_set [] in
  rewrite_cexpr (toplevel_rewrite_ctxt, recurse_rewrite_ctxt) acexpr

let rewriting_cstatement (toplevel_rule_set, recurse_rule_set) static_ctxt acstatement =
  let recurse_rewrite_ctxt = build_rewrite_context static_ctxt recurse_rule_set [] in
  let toplevel_rewrite_ctxt = build_rewrite_context static_ctxt toplevel_rule_set [] in
  rewrite_cstatement (toplevel_rewrite_ctxt, recurse_rewrite_ctxt) acstatement

let rewriting_cprolog (toplevel_rule_set, toplevel_prolog_set, recurse_rule_set) static_ctxt acprolog =
  let recurse_rewrite_ctxt =
    build_rewrite_context static_ctxt recurse_rule_set toplevel_prolog_set
  in
  let toplevel_rewrite_ctxt =
    build_rewrite_context static_ctxt toplevel_rule_set toplevel_prolog_set
  in
  (static_ctxt,rewrite_cprolog (toplevel_rewrite_ctxt, recurse_rewrite_ctxt) acprolog)

let rewriting_cxmodule (toplevel_rule_set, toplevel_prolog_set, recurse_rule_set) static_ctxt acxmodule =
  let recurse_rewrite_ctxt =
    build_rewrite_context static_ctxt recurse_rule_set toplevel_prolog_set
  in
  let toplevel_rewrite_ctxt =
    build_rewrite_context static_ctxt toplevel_rule_set toplevel_prolog_set
  in
  (static_ctxt,rewrite_cxmodule (toplevel_rewrite_ctxt, recurse_rewrite_ctxt) acxmodule)

