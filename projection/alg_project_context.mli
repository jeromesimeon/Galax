(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: alg_project_context.mli,v 1.2 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Alg_project_context
   Description:
     This module implements the context used during projection.
*)

open Alg_path_struct


(* Type used for the projection context *)

type project_context


(* Builds a new projection context *)

val build_project_context :
    Streaming_types.typed_xml_stream ->
      path_fragment_sequence ->
	Streaming_types.typed_annotated_sax_event list -> project_context


(* Accessors to the projection context *)

val get_pfs            : project_context -> path_fragment_sequence option
val get_local_buffer   : project_context -> Streaming_types.typed_annotated_sax_event list
val get_next_xml_event : project_context -> Streaming_types.typed_annotated_sax_event

(* Added accessor function to underlying input stream.
   - Michael *)
val project_stream_is_empty : project_context -> bool
val get_stream               : project_context -> Streaming_types.typed_xml_stream

val get_next_buffered_sax_event  : project_context -> Streaming_types.typed_annotated_sax_event option
val peek_next_buffered_sax_event : project_context -> Streaming_types.typed_annotated_sax_event option


(* Change the local_buffer *)

val refill_local_buffer  : project_context -> Streaming_types.typed_annotated_sax_event list -> unit

val push_project_context_get_subtree :
    project_context -> Streaming_types.typed_annotated_sax_event -> unit

val push_project_context_keep_moving_skip_node :
    project_context -> Streaming_types.typed_annotated_sax_event -> path_fragment_sequence -> unit

val push_project_context_keep_moving_preserve_node :
    project_context -> Streaming_types.typed_annotated_sax_event -> path_fragment_sequence -> unit

val push_project_context_preserve_node :
    project_context -> Streaming_types.typed_annotated_sax_event list -> unit

val push_project_context_skip_node :
    project_context -> unit

val pop_project_context  : project_context -> Streaming_types.typed_annotated_sax_event list -> unit
