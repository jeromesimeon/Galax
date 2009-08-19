(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cs_code_selection_expr.mli,v 1.3 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Cs_code_selection_expr
   Description:
     This module performs code selection for a logical plan.
*)

open Code_selection_context
open Compiled_program_units

val default_code_selection :
    string (* Module URI *) -> code_selection_context -> Algebra_type.algop_expr -> code_selection_context

val sub_expr_default_code_selection :
    string (* Module URI *) -> code_selection_context -> Algebra_type.algop_sub_exprs -> code_selection_context

