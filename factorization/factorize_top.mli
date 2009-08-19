(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_top.mli,v 1.4 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Factorize_top
   Description:
    This module will put statements into a normal form before their
    are compiled into the algbera. This is to help the optimizer pick
    up logical optimizations.

*)

open Xquery_core_ast
open Typing_context

val factorize_statement : static_context -> acstatement -> acstatement
val factorize_prolog    : static_context -> acprolog    -> static_context * acprolog
val factorize_xmodule   : static_context -> acxmodule   -> static_context * acxmodule

