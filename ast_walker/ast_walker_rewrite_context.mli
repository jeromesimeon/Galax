(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ast_walker_rewrite_context.mli,v 1.3 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Ast_walker_rewrite_context
   Description:
     Context for the rewriting engine.
*)

open Xquery_core_ast
open Xquery_type_core_ast

(* Type for the rewriting rules *)

(* Note:
     A rule takes a context (including 'a rules), an 'a cexpr, and
     returns a 'a cexpr, plus a boolean indicating if any of the rules
     has been triggered, or whether the default behavior has been
     applied.

     The default behavior is to copy the expression as is. The output
     expression must be of the same type for the fix-point to work.

   - Jerome *)

type 'a rewrite_context

and 'a rewrite_rule = 'a rewrite_context -> acexpr -> acexpr * bool

and 'a rewrite_prolog_rule = 'a rewrite_context -> acvar_decl -> 'a rewrite_context * acvar_decl * bool

type 'a rewrite_rule_set         = 'a rewrite_rule list 
type 'a rewrite_prolog_rule_set  = 'a rewrite_prolog_rule list 

(* Type for the rewriting context *)

(* Operations on the rewriting context *)

val build_rewrite_context :
    'a -> 'a rewrite_rule_set -> 'a rewrite_prolog_rule_set -> 'a rewrite_context

val get_context        	     : 'a rewrite_context -> 'a
val get_rewrite_rules  	     : 'a rewrite_context -> 'a rewrite_rule list
val get_rewrite_prolog_rules : 'a rewrite_context -> 'a rewrite_prolog_rule list

