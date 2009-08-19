(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: sxp_context.mli,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Sxp_context
   Description:
     This module provides data structures and accessors that together
     constitute the context for streaming XPath evaluation over
     labeled XML token streams.
   - Michael *)

type action = {
  let_pass : bool;
  increase_outermost_match_depth : bool;
  push_label_depth_stack : bool;
  set_flag : bool;
}

type sxp_context


val default_sxp_context :
  unit -> sxp_context


(******************************************)
(* Stack of label-relative depth counters *)
(******************************************)

val push_label_depth_stack :
  sxp_context -> unit

val pop_label_depth_stack :
  sxp_context -> int

val decrease_topmost_label_depth :
  sxp_context -> unit

val increase_topmost_label_depth :
  sxp_context -> unit

val topmost_label_depth_is_one :
  sxp_context -> bool

val topmost_label_depth_ge_one :
  sxp_context -> bool


(*************************************)
(* Depth relative to outermost match *)
(*************************************)

val decrease_outermost_match_depth :
  sxp_context -> unit

val increase_outermost_match_depth :
  sxp_context -> unit

val is_inside_another_match :
  sxp_context -> bool


(********************)
(* Action recording *)
(********************)

val record_action :
  action -> sxp_context -> unit

val get_recorded_action :
  sxp_context -> action
