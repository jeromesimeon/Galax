(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_rules_sbdo_automaton.mli,v 1.2 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Rewriting_ddo_automaton
   Description:
     This module implements the 3 automata that annotate the AST with
     DDO annotations for the benefits of the corresponding rewriting
     rules.
*)

open Processing_context


(* The first integer is the state, the second integer is the substate i *)

type state_number = int
type state        = state_number * (int * int)

type action_needed =
| Nothing
| Docorder
| Distinct
| DistinctDocorder

type axis_number = int

val axis_child : axis_number
val axis_descendant : axis_number
val axis_descendant_or_self : axis_number
val axis_following : axis_number
val axis_following_sibling : axis_number
val axis_parent : axis_number
val axis_ancestor : axis_number
val axis_ancestor_or_self : axis_number
val axis_preceding : axis_number
val axis_preceding_sibling : axis_number
val axis_self : axis_number

val axis_adapter : Xquery_common_ast.axis -> axis_number

val do_transition : sbdo_kind -> state -> axis_number -> bool -> (state * action_needed)

val init_state : sbdo_kind -> state
val sink_state : sbdo_kind -> state
val undefined_state : sbdo_kind -> state

val needs_sorting : action_needed -> bool
val needs_dupelim : action_needed -> bool

val print_action : action_needed -> string
val print_state : state -> string

