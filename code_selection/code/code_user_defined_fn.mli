(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_user_defined_fn.mli,v 1.6 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_user_defined_fn
   Description:
     This module contains code building operations for user-defined
     functions.
*)

open Xquery_algebra_ast
open Xquery_common_ast

val build_user_defined_fn_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	(cfname * int) * asequencetype option array * asequencetype ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context) 

