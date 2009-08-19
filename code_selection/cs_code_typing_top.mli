(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cs_code_typing_top.mli,v 1.2 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Cs_code_typing_top.
   Description:
     This module implements the physical typing and physical-operator
     assignment phase.
*)

(* 
   Code typing is really a separate 'sub-phase', that deals with
   assigning physical operators to logical operators. 
*)

open Code_typing_context
open Code_selection_context

(* For now, we are passing the entire code selection context because
   typing and physical operator selection depend upon the processing
   context. *)
val code_typing_statement : code_selection_context -> Algebra_type.algop_expr    -> code_type_context 
val code_typing_prolog    : code_selection_context -> Algebra_type.algop_prolog  -> code_type_context 
val code_typing_module    : code_selection_context -> Algebra_type.algop_xmodule -> code_type_context 
