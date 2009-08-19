(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_fn.mli,v 1.5 2007/02/01 22:08:55 simeon Exp $ *)

(* Module: Typing_fn
   Description:
     This module implements static typing for F&O functions.
*)

open Xquery_common_ast
open Xquery_type_core_ast

(* Typing for built-in functions types *)

type bltin_type_rule = Typing_context.static_context -> Finfo.finfo -> cxtype list -> cxtype -> cxtype

val lookup_bltin_fctn_type_rule : cfname_arity -> bltin_type_rule
val has_special_type_rule       : cfname_arity -> bool

