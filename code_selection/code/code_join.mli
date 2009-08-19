(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_join.mli,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_join
   Description:
     This module contains code building for operators that implement
     joins.
*)

open Code_selection_context

type default_code_selection_type = (code_selection_context -> Algebra_type.algop_expr -> code_selection_context)

val build_join_code : 
    code_selection_context -> 
	Algebra_type.algop_expr ->
	  default_code_selection_type ->
	    (Algebra_type.alg_eval_code_dep  * code_selection_context)
	  

