(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_top.mli,v 1.5 2007/02/01 22:08:55 simeon Exp $ *)

(* Module: Typing_top
   Description:
     This module implements the static typing feature.
 *)

open Typing_context

open Xquery_core_ast
open Xquery_type_core_ast



(*******************************)
(* main type-checking function *)
(*******************************)

val typing_type_cexpr      : static_context -> acexpr      -> static_context
val typing_type_cstatement : static_context -> acstatement -> static_context
val typing_type_cprolog    : static_context -> acprolog    -> static_context
val typing_type_cxmodule   : static_context -> acxmodule   -> static_context

