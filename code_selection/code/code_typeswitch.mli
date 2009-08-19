(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_typeswitch.mli,v 1.4 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_typeswitch
   Description:
     This module contains code building for the typeswitch operator.
*)

open Xquery_algebra_ast
open Xquery_common_ast

val build_typeswitch_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr ->
	(apattern * cvname option) array ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)
	  

