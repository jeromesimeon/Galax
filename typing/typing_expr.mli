(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_expr.mli,v 1.2 2007/02/01 22:08:55 simeon Exp $ *)

(* Module: Typing_expr
   Description:
     This module implements the static typing feature for expressions.
 *)

open Typing_context

open Xquery_core_ast
open Xquery_type_core_ast



(*******************************)
(* main type-checking function *)
(*******************************)

val type_cexpr              : static_context -> acexpr -> cxtype
val type_cexpr_with_replace : static_context -> acexpr -> cxtype -> cxtype

