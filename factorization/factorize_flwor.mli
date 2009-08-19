(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_flwor.mli,v 1.4 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Factorize_flwor
   Description:
     This module factorizes out code from FLWOR expression. This is to
     help the optimizer pick up logical optimizations.
*)


open Xquery_core_ast

val factorize_flwor : Typing_context.static_context -> acexpr -> acexpr

