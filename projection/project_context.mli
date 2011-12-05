(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: project_context.mli,v 1.5 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Project_context
   Description:
     This module implements the context used during projection.
*)


(* Type used for the projection context *)

type project_context


(* Builds a new projection context *)

val build_project_context :
    Streaming_types.xml_stream ->
      Path_struct.path_fragment_sequence ->
	Streaming_types.sax_event list -> project_context


(* Accessors to the projection context *)

val get_pfs            : project_context -> Path_struct.path_fragment_sequence option
val get_local_buffer   : project_context -> Streaming_types.sax_event list
val get_next_xml_event : project_context -> Streaming_types.sax_event

(* Added accessor function to underlying input stream.
   - Michael *)
val project_stream_is_empty : project_context -> bool
val get_stream         : project_context -> Streaming_types.xml_stream

val get_next_buffered_sax_event  : project_context -> Streaming_types.sax_event option
val peek_next_buffered_sax_event : project_context -> Streaming_types.sax_event option


(* Change the local_buffer *)

val refill_local_buffer  : project_context -> Streaming_types.sax_event list -> unit

val push_project_context_get_subtree :
    project_context -> Streaming_types.sax_event -> unit

val push_project_context_keep_moving_skip_node :
    project_context -> Streaming_types.sax_event -> Path_struct.path_fragment_sequence -> unit

val push_project_context_keep_moving_preserve_node :
    project_context -> Streaming_types.sax_event -> Path_struct.path_fragment_sequence -> unit

val push_project_context_preserve_node :
    project_context -> Streaming_types.sax_event list -> unit

val push_project_context_skip_node :
    project_context -> unit

val pop_project_context  : project_context -> Streaming_types.sax_event list -> unit
