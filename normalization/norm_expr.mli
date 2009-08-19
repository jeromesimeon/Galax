(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: norm_expr.mli,v 1.7 2007/05/18 15:32:13 mff Exp $ *)

(* Module: Norm_expr
   Description:
     This module implements normalization for XQuery expressions.
*)

open Xquery_ast
open Xquery_core_ast

open Norm_context


(* Normalization of expressions *)
val normalize_expr     : norm_context -> expr  -> (acfunction_def list * acexpr)

