(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cursor_context.ml,v 1.3 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Cursor_context
   Description:
     This module implements the context used iteration over the data
     model.
*)

open Error

open Dm_types
open Cursor


(**********************)
(* The cursor context *)
(**********************)

type 'a cursor_context =
    { mutable current_cursor : 'a cursor;
      stacked_nodes : ('a cursor) Stack.t}


(********************************)
(* Creates a new cursor context *)
(********************************)

let build_cursor_context il =
  { current_cursor = il;
    stacked_nodes = Stack.create () }


(************************************)
(* Operations on the cursor context *)
(************************************)

(* Access the current cursor to stream *)

let get_current_cursor cursor_context =
  cursor_context.current_cursor

(* Push a context (getting inside of an element/document node) *)

let push_node_to_cursor_context cursor_context new_current_cursor =
  Stack.push (cursor_context.current_cursor) cursor_context.stacked_nodes;
  cursor_context.current_cursor <- new_current_cursor

(* Pop a context (getting outside of an element/document node) *)

let pop_node_from_cursor_context cursor_context =
  try
    let (replacement_cursor) = Stack.pop cursor_context.stacked_nodes in
    begin
      cursor_context.current_cursor <- replacement_cursor;
      Some ()
    end
  with
  | Stack.Empty ->
      None

