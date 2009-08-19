(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ast_walker_rewrite.mli,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Ast_walker_rewrite
   Description:

     This module implements a generic tree walker, which recursively
     applies rewrite rules until a fix-point is reached.

     This module is purely *functional* : it takes annotated cexprs as
     input and produces annotated cexprs as output.  It does not
     destructively modify any AST nodes.
*)

open Xquery_common_ast
open Xquery_core_ast

open Ast_walker_rewrite_context

(* Exception raised if a given rewriting rule does not apply *)

exception Not_applied

(* General application of rewriting rules on an XQuery Core AST *)

val generic_cexpr   : 'a rewrite_context -> acexpr -> (acexpr * bool)
val children_cexpr  : 'a rewrite_context -> acexpr -> (acexpr * bool)
val fix_point_cexpr : 'a rewrite_context -> acexpr -> acexpr

(* Note:
     The toplevel rewriting operations here always apply to annotated core
     expressions, and always return annotated core expressions.
   - Jerome/Mary
*)

(* 
   Each top-level rewrite operator takes two sets of rules:
  
   The first set is applied only to the top-level AST node in the
   input expr/statement.

   The second set is applied recursively to every AST node in the
   input expr/statement.
*)
val rewrite_cexpr      : ('a rewrite_context * 'a rewrite_context) -> acexpr      -> acexpr
val rewrite_cstatement : ('a rewrite_context * 'a rewrite_context) -> acstatement -> acstatement
val rewrite_cprolog    : ('a rewrite_context * 'a rewrite_context) -> acprolog    -> acprolog
val rewrite_cxmodule   : ('a rewrite_context * 'a rewrite_context) -> acxmodule   -> acxmodule

(***************************)
(* Specific rewriting rule *)
(***************************)

(* force_substitute_var substitutes E1 for vname in E2, 
   and does not check for variable capture in E2 *)
val force_substitute_var : acexpr -> cvname -> acexpr -> (acexpr * bool)
(* safe_substitute_var substitutes E1 for vname in E2, 
   and does check for variable capture in E2 *)
val safe_substitute_var : acexpr -> cvname -> acexpr -> (acexpr * bool)

(* added by Philippe to enable free variable calc *)
val free_variables : acexpr -> cvname list

