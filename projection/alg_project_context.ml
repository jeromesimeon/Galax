(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: alg_project_context.ml,v 1.2 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Alg_project_context
   Description:
     This module implements the context used during projection.
*)

(* Type used for the projection context *)

type project_context =
    { project_pfs                  : (Alg_path_struct.path_fragment_sequence option) Stack.t;
      mutable project_local_buffer : Streaming_types.typed_annotated_sax_event list;
      mutable project_temp_buffer  : Streaming_types.typed_annotated_sax_event list;
      project_stream               : Streaming_types.typed_xml_stream}

(* Builds a new projection context *)

let build_project_context xml_stream pfs init_local_buffer =
  let init_pfs_stack = Stack.create () in
  begin
    Stack.push (Some pfs) init_pfs_stack;
    { project_pfs           = init_pfs_stack;
      project_local_buffer  = init_local_buffer;
      project_temp_buffer   = [];
      project_stream 	    = xml_stream }
  end

(* Accessors to the projection context *)

let get_pfs project_context =
    Stack.top project_context.project_pfs

let get_local_buffer project_context =
  project_context.project_local_buffer

let get_temp_buffer project_context =
  project_context.project_temp_buffer

let get_stream project_context =
  project_context.project_stream

let get_next_xml_event project_context =
  Cursor.cursor_next project_context.project_stream

(* Added accessor function to underlying input stream.
   - Michael *)
let project_stream_is_empty project_context =
  Cursor.cursor_is_empty project_context.project_stream

let get_next_buffered_sax_event project_context =
  match project_context.project_local_buffer with
  | [] -> None
  | xml_event :: rest ->
      begin
	project_context.project_local_buffer <- rest;
	Some xml_event
      end

let peek_next_buffered_sax_event project_context =
  match project_context.project_local_buffer with
  | [] -> None
  | xml_event :: _ ->
      Some xml_event

(* Cleans up the temporary buffer *)

let return_temp_buffer project_context =
  let current_temp_buffer = project_context.project_temp_buffer in
  begin
    project_context.project_temp_buffer <- [];
    List.rev current_temp_buffer
  end

(* Operations on the temporary buffer *)

let refill_local_buffer project_context refill_buffer =
  let previous_local_buffer = project_context.project_local_buffer in
  let temp_buffer = return_temp_buffer project_context in
  project_context.project_local_buffer <- previous_local_buffer @ temp_buffer @ refill_buffer

let push_event_in_temp_buffer project_context xml_event =
  (* Note: This stores the buffered events in reverse order! - Jerome *)
  let current_temp_buffer = project_context.project_temp_buffer in
  project_context.project_temp_buffer <- xml_event :: current_temp_buffer

let pop_event_in_temp_buffer project_context =
  match project_context.project_temp_buffer with
  | [] ->
      None
  | event :: rest ->
      begin
	project_context.project_temp_buffer <- rest;
	Some event
      end

let push_project_context_get_subtree project_context xml_event =
  let refill_buffer = [ xml_event ] in
  begin
    Stack.push None project_context.project_pfs;
    refill_local_buffer project_context refill_buffer
  end

let push_project_context_keep_moving_preserve_node project_context xml_event pfs =
  let refill_buffer = [ xml_event ] in
  begin
    Stack.push (Some pfs) project_context.project_pfs;
    refill_local_buffer project_context refill_buffer
  end
  
let push_project_context_keep_moving_skip_node project_context sax_event pfs =
  begin
    Stack.push (Some pfs) project_context.project_pfs;
    push_event_in_temp_buffer project_context sax_event
  end

let push_project_context_preserve_node project_context refill_buffer =
  begin
    Streaming_ops.discard_typed_xml_stream project_context.project_stream;
    refill_local_buffer project_context refill_buffer
  end

let push_project_context_skip_node project_context =
  Streaming_ops.discard_typed_xml_stream project_context.project_stream

let pop_project_context project_context refill_buffer =
  (* Note: This is called whenever a close element is encountered,
     pulling back to the context for the parent element. - Jerome *)
  match pop_event_in_temp_buffer project_context with
  | Some _ ->
      ignore(Stack.pop project_context.project_pfs);
  | None ->
      begin
	ignore(Stack.pop project_context.project_pfs);
	refill_local_buffer project_context refill_buffer
      end

