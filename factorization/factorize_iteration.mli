(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_iteration.mli,v 1.5 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Factorize_iteration
   Description:
     This module attempts to put expressions in a normal form. For a
     fragment of the language it moves expressions close as close to
     the iteration they depend on as possible.
*)

val factorize_expression : Typing_context.static_context -> Xquery_core_ast.acexpr -> Xquery_core_ast.acexpr 
