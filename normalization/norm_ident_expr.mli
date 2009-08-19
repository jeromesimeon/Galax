(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: norm_ident_expr.mli,v 1.4 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Norm_ident_expr
   Description:
     This module implements an identity normalization for XQuery
     expressions which are already in the core.
*)

open Xquery_ast
open Xquery_core_ast

open Norm_context



(* Normalization of expressions *)

val normalize_ident_expr     : norm_context -> expr        -> (acfunction_def list * acexpr)

