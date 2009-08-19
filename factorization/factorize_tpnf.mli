(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_tpnf.mli,v 1.2 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Factorize_tpnf
   Description:

   This module is a factorization plugin for the TPNF approach
   for normalizing XQuery Core expressions.

   -- Philippe
*)

val factorize_tpnf : Typing_context.static_context -> Xquery_core_ast.acexpr -> Xquery_core_ast.acexpr 

