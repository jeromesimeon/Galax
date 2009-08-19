(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_notyping.mli,v 1.4 2007/02/01 22:08:55 simeon Exp $ *)

(* Module: Typing_notyping
   Description:
     This module contains operations to add dummy type annotations,
     when static typing is off. Eventually, this should be replaced by
     a 'non-safe' static typing analysis phase.
 *)

open Typing_context
open Xquery_core_ast
open Xquery_type_core_ast


(*******************************)
(* main type-checking function *)
(*******************************)

val notyping_type_cexpr      : static_context -> acexpr      -> static_context
val notyping_type_cstatement : static_context -> acstatement -> static_context
val notyping_type_cprolog    : static_context -> acprolog    -> static_context
val notyping_type_cxmodule   : static_context -> acxmodule   -> static_context

