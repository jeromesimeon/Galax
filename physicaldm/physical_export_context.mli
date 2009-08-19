(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_export_context.mli,v 1.4 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Export_context
   Description:
     This module implements the context used during export from the
     data model to an XML stream.
*)

open Error

open Dm


(**********************)
(* The export context *)
(**********************)

type export_context


(********************************)
(* Creates a new export context *)
(********************************)

val build_export_context : Physical_value.item Cursor.cursor -> export_context


(************************************)
(* Operations on the export context *)
(************************************)

val get_current_cursor           : export_context -> Physical_value.item Cursor.cursor
val push_node_to_export_context  : export_context -> Dm.node -> Physical_value.item Cursor.cursor -> unit
val pop_node_from_export_context : export_context -> Streaming_types.typed_annotated_sax_event option


