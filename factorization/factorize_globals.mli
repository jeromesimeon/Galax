(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_globals.mli,v 1.3 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Factorize_globals
   Description:
     This module takes those expressions which are global variables
     (for expressions) and makes them let bindings. This lets the
     compiler know that the dependence is lest restrictive.
*)

(* Factor bindings into global expressions *)
val factor_global_expression : Typing_context.static_context -> Xquery_core_ast.acexpr -> Xquery_core_ast.acexpr 
