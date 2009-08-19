(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ast_walker_rewrite_context.ml,v 1.2 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Ast_walker_rewrite_context
   Description:
     Context for the rewriting engine.
*)

open Xquery_core_ast
open Xquery_type_core_ast


(* Type for the rewriting rules *)

(* Note:
     A rule takes a context (including rules), an acexpr, and returns
     an acexpr, plus a boolean indicating if any of the rules has been
     triggered, or whether the default behavior has been applied.

     The default behavior is to copy the expression as is. 

   - Jerome *)

type 'a rewrite_rule =
    'a rewrite_context -> acexpr -> acexpr * bool

and 'a rewrite_prolog_rule =
    'a rewrite_context -> acvar_decl -> 'a rewrite_context * acvar_decl * bool

(* Type for the rewriting context *)

and 'a rewrite_context =
    { rewrite_context : 'a;
      rewrite_rules   : 'a rewrite_rule list;
      rewrite_prolog_rules  : 'a rewrite_prolog_rule list }

(* Operations on the rewriting context *)

let build_rewrite_context ctxt rules prolog_rules =
  { rewrite_context = ctxt;
    rewrite_rules   = rules; 
    rewrite_prolog_rules   = prolog_rules }

let get_context        rewrite_ctxt = rewrite_ctxt.rewrite_context
let get_rewrite_rules  rewrite_ctxt = rewrite_ctxt.rewrite_rules
let get_rewrite_prolog_rules  rewrite_ctxt = rewrite_ctxt.rewrite_prolog_rules

type 'a rewrite_rule_set  = 'a rewrite_rule list 
type 'a rewrite_prolog_rule_set  = 'a rewrite_prolog_rule list 

