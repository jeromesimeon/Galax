(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cursor_context.mli,v 1.3 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Cursor_context
   Description:
     This module implements the context used iteration over the data
     model.
*)

open Error


(**********************)
(* The cursor context *)
(**********************)

type 'a cursor_context


(********************************)
(* Creates a new cursor context *)
(********************************)

val build_cursor_context : 'a Cursor.cursor -> 'a cursor_context


(************************************)
(* Operations on the cursor context *)
(************************************)

val get_current_cursor           : 'a cursor_context -> 'a Cursor.cursor
val push_node_to_cursor_context  : 'a cursor_context -> 'a Cursor.cursor -> unit
val pop_node_from_cursor_context : 'a cursor_context -> unit option


