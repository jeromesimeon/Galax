(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: rewriting_rules_typing.mli,v 1.5 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Rewriting_rules_typing
   Description:
     This module contains the rewriting rules that rely on typing
     information.
*)

open Ast_walker_rewrite_context

(**************************************************************************)
(* Various sets of rewriting rules : Rules are composed in procmod_phases *)
(**************************************************************************)

(* Additional rules that are applicable when either weak or strong typing is enabled. *)
val any_typing_rule_set : Typing_context.static_context rewrite_rule_set

