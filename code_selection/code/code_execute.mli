(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_execute.mli,v 1.7 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Code_execute
   Description:
     This module contains code to execute query plans.
*)

open Code_selection_context
open Xquery_algebra_ast
open Xquery_common_ast

val build_execute_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	(bool * Namespace_names.ncname * string) ->
	  (Algebra_type.alg_eval_code_dep * Code_selection_context.code_selection_context)

val build_server_implements_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Namespace_names.ncname * string) ->
	  (Algebra_type.alg_eval_code_dep * Code_selection_context.code_selection_context) 

val build_server_close_code :
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Namespace_names.ncname * string) ->
	  (Algebra_type.alg_eval_code_dep * Code_selection_context.code_selection_context) 

val build_eval_closure_code :
    string -> (* URI of module in which closure is evaluated *)
      Code_selection_context.code_selection_context -> 
	Algebra_type.algop_expr -> 
	  (Algebra_type.alg_eval_code_dep * Code_selection_context.code_selection_context) 


