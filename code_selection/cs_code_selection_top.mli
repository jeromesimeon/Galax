(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cs_code_selection_top.mli,v 1.7 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Cs_code_selection_top
   Description:
     This module is the entry-point to code-selection.
*)

(* The main interface to compilation
   Takes in typed core expressions and allocates code for each
   operation in the algebraic plan. *)

val code_selection_statement :
    (Compiled_program_units.compiled_program * 
    string (* Module URI *) *
    Code_selection_context.code_selection_context) ->
    Ast_logical_algebra_types.logical_algop_expr ->
    Code_selection_context. code_selection_context *
    Algebra_type.algop_expr

val code_selection_prolog :
    (Compiled_program_units.compiled_program *
    string (* Module URI *) *
    Code_selection_context.code_selection_context) ->
    Ast_logical_algebra_types.logical_algop_prolog ->
    Code_selection_context. code_selection_context *
    Algebra_type.algop_prolog

val code_selection_module :
    (Compiled_program_units.compiled_program *
    string (* Module URI *) *
    Code_selection_context.code_selection_context) ->
    Ast_logical_algebra_types.logical_algop_xmodule ->
    Code_selection_context.code_selection_context *
    Algebra_type.algop_xmodule

