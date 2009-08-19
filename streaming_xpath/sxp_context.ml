(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: sxp_context.ml,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

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

type sxp_context = {
  mutable outermost_match_depth : int;
  label_depth_stack : int Stack.t;
  action_stack: action Stack.t;
}


(***************************)
(* Streaming XPath context *)
(***************************)

let default_sxp_context () = 
  let stack = Stack.create () in
    Stack.push 0 stack;
    {
      outermost_match_depth = 0;
      label_depth_stack = stack;
      action_stack = Stack.create ();
    }


(******************************************)
(* Stack of label-relative depth counters *)
(******************************************)

(* In principle, each depth counter represents the depth of the current event
   with respect to all the labels affecting that event, i.e., those corresponding
   to ancestors of the event. As a simplification, only the topmost depth counter
   is to be in/decreased in reaction to start/end events. *)

let push_label_depth_stack sxp_context =
  Stack.push 1 sxp_context.label_depth_stack

let pop_label_depth_stack sxp_context =
  try
    Stack.pop sxp_context.label_depth_stack
  with Stack.Empty ->
    begin
      (*print_string "pop_label_depth_stack: Stack.empty\n";*)
      raise Stack.Empty
    end


(* validate sideeffect behaviour *)
let decrease_topmost_label_depth sxp_context =
  let depth_stack = sxp_context.label_depth_stack in
  let topmost_depth = 
    try
      Stack.pop depth_stack
    with Stack.Empty ->
      begin
	(*print_string "decrease_topmost_label_depth: Stack.empty\n";*)
	raise Stack.Empty
      end
  in
    Stack.push (topmost_depth - 1) depth_stack

let increase_topmost_label_depth sxp_context =
  let depth_stack = sxp_context.label_depth_stack in
  let topmost_depth =
    try
      Stack.pop depth_stack
    with Stack.Empty ->
      begin
	(*print_string "increase_topmost_label_depth: Stack.empty\n";*)
	raise Stack.Empty
      end
  in
    Stack.push (topmost_depth + 1) depth_stack

let topmost_label_depth_is_one sxp_context =
  ((
     try
       Stack.top sxp_context.label_depth_stack
     with Stack.Empty ->
       begin
	 (*print_string "topmost_label_depth_is_one: Stack.empty\n";*)
	 raise Stack.Empty
       end
   ) = 1)

let topmost_label_depth_ge_one sxp_context =
  ((
     try
       Stack.top sxp_context.label_depth_stack
     with Stack.Empty ->
       begin
	 (*print_string "topmost_label_depth_is_one: Stack.empty\n";*)
	 raise Stack.Empty
       end
   ) >= 1)


(*************************************)
(* Depth relative to outermost match *)
(*************************************)

let decrease_outermost_match_depth sxp_context =
  sxp_context.outermost_match_depth <- (sxp_context.outermost_match_depth - 1)

let increase_outermost_match_depth sxp_context =
  sxp_context.outermost_match_depth <- (sxp_context.outermost_match_depth + 1)

let is_inside_another_match sxp_context =
  (sxp_context.outermost_match_depth > 0)


(********************)
(* Action recording *)
(********************)

let record_action action sxp_context =
  Stack.push action sxp_context.action_stack

let get_recorded_action sxp_context =
  try
    Stack.pop sxp_context.action_stack
  with Stack.Empty ->
    begin
      print_string "get_recorded_action: Stack.empty\n";
      raise Stack.Empty
    end

