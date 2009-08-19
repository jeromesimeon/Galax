(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_update.mli,v 1.2 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Factorize_update
   Description:
     This module factorizes out code from update expression. This is
     to help the optimizer pick up logical optimizations.
*)

open Xquery_core_ast

(***************************)
(* Normal form for updates *)
(***************************)

val update_in_normal_form : acfl_expr list -> acexpr -> acfl_expr list * acexpr

