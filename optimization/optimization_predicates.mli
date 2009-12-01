(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_predicates.mli,v 1.7 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Optimization_predicates
   Description:
     This module is used to process predicates during optimization.
*)

open Xquery_algebra_ast
open Ast_logical_algebra_types

val is_simple_conjunct         : logical_algop_expr -> bool

(* Get the predicate description from an op that has one *)
val extract_pred_desc          : logical_algop_expr -> predicate_desc

(* Construct a select *)
val construct_singleton_select :
    logical_algop_expr -> logical_algop_expr ->
      Xquery_ast.expr_handle -> Finfo.finfo -> logical_algop_expr

val conjunctive_merge_select :
    logical_algop_expr -> logical_algop_expr -> logical_algop_expr

(* Remove a conjunct from an op with a predicate
   If this conjunct is the only predicate in a select, return None
   else return Some op modified (Notice this should turn a Join into a product *)
val remove_conjunct : logical_algop_expr -> int -> logical_algop_expr option

