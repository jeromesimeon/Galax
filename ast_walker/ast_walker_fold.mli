(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ast_walker_fold.mli,v 1.3 2007/08/15 18:53:48 mgreenberg Exp $ *)

(* Module: Ast_walker_fold
   Description:
     This module implements a generic tree walker, which recursively
     computes a value over the XQuery core AST.
*)

open Xquery_core_ast

val fold_over_cexpr      : (acexpr -> 'b) -> ('a -> 'b -> 'a) -> 'a -> acexpr -> 'a

(* [fold_over_cexpr loc agg v e] recursively walks down e, applying the
   loc function to every node in the expression AST, then accumulating the result of
   applying the agg function to every value in the tree, from the seed
    value 'a. *)

open Xquery_type_core_ast

val fold_over_cxtype      : (cxtype -> 'b) -> ('a -> 'b -> 'a) -> 'a -> cxtype -> 'a

(* [fold_over_cxtype loc agg v e] recursively walks down e, applying the
   loc function to every node in the type AST, then accumulating the result of
   applying the agg function to every value in the tree, from the seed
    value 'a. *)

