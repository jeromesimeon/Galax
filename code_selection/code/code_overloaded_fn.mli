(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_overloaded_fn.mli,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_overloaded_fn
   Description:
     This module contains code building for operators that implement
     overloaded functions.
*)

open Xquery_algebra_ast
open Xquery_common_ast


val build_default_overloaded_fn_code :
    Code_selection_context.code_selection_context -> 
      cfname -> int -> aoverloaded_signature_table ->
	(Execution_context.algebra_context ->
    Physical_value.item list array -> Physical_value.item list)

val build_overloaded_fn_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	(cfname * int) * aoverloaded_signature_table ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
