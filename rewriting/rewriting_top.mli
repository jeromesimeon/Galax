(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: rewriting_top.mli,v 1.3 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Rewriting_top
   Description:
     This module contains the top-level operations for the rewriting
     processing phase.
*)

open Xquery_core_ast

open Typing_context
open Ast_walker_rewrite_context

(* Top-level rewriting operations *)

(* Note:
     The toplevel rewriting operations here always apply to a typed
     core expressions, and always return a typed core expressions.
   - Jerome
   *)

(* 
   Each top-level rewriting operator takes two sets of rules:
  
   The first set is applied only to the top-level AST node in the
   input expr/statement.

   The second set is applied recursively to every AST node in the
   input expr/statement.
*)

type statement_rule_sets =
    static_context rewrite_rule_set
      * static_context rewrite_rule_set
type module_rule_sets =
    static_context rewrite_rule_set
      * static_context rewrite_prolog_rule_set
      * static_context rewrite_rule_set

val rewriting_cstatement :
    statement_rule_sets -> static_context -> acstatement -> acstatement
val rewriting_cprolog    :
    module_rule_sets -> static_context -> acprolog  -> static_context * acprolog
val rewriting_cxmodule   :
    module_rule_sets -> static_context -> acxmodule -> static_context * acxmodule

