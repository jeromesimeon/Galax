(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_free_var.mli,v 1.3 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Factorize_free_var
   Description:
     Given a core expression, recursively fill out the free variable
     annotation.  Additionally return the list of free variables for
     the expression.
*)

val compute_free_vars : Xquery_core_ast.acexpr -> Xquery_common_ast.cvname list

val annotate_free_vars : Typing_context.static_context -> Xquery_core_ast.acexpr -> Typing_context.static_context

