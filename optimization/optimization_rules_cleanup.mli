(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_rules_cleanup.mli,v 1.4 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Optimization_rules_cleanup
   Description:
     This module contains optimization cleanup rules.
*)

open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_algebra_ast_annotation_util

open Processing_context
open Compile_context

open Optimization_util
open Optimization_walker
open Ast_logical_algebra_types

val cleanup_rules:
    ((bool ref -> logical_compile_context ->
      logical_algop_expr ->
      (logical_algop_expr * sub_expr_kind * int) option ->
      logical_algop_expr ->
      logical_algop_expr) * string) list
