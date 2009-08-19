(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util_matching.mli,v 1.7 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_util_matching
   Description:
     This module contains some auxiliary evaluation code for type
     matching.
*)

open Namespace_context
open Typing_context

open Xquery_common_ast
open Xquery_algebra_ast

open Datatypes
open Cursor

open Physical_value

(*****************************************)
(* Auxiliary functions for type matching *)
(*****************************************)

val dynamic_type_check : static_context -> asequencetype -> item cursor -> item cursor
val dynamic_opttype_check : static_context -> asequencetype option -> item cursor -> item cursor

val dynamic_type_check_item    : static_context -> asequencetype -> item -> unit
val dynamic_opttype_check_item : static_context -> asequencetype option -> item -> unit
val boolean_dynamic_type_check : static_context -> asequencetype -> item list -> bool


