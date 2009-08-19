(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cursor_descendant.ml,v 1.4 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Cursor_descendant
   Description:
     This module cursors a data model instance into an XML stream.
*)

open Error

open Cursor
open Cursor_context

open Dm_types


(***************************)
(* Build a cursor of nodes *)
(***************************)

let rec next_descendant access_ops cursor_context =
  let current_cursor = get_current_cursor cursor_context in
  if cursor_is_empty current_cursor
  then
    match pop_node_from_cursor_context cursor_context with
    | Some () -> next_descendant access_ops cursor_context
    | None -> None
  else
    let node = cursor_next current_cursor in
    begin
      begin
	match access_ops.get_node_kind node with
	| DocumentNodeKind ->
	    begin
	      let doc_children = access_ops.get_document_node_children node in
	      push_node_to_cursor_context cursor_context doc_children
	    end
	| ElementNodeKind ->
	    begin
	      let elem_children = access_ops.get_element_node_children node in
	      push_node_to_cursor_context cursor_context elem_children
	    end
	| AttributeNodeKind
	| TextNodeKind
	| ProcessingInstructionNodeKind
	| CommentNodeKind -> ()
      end;
      Some node
    end

let next_event access_ops cursor_context n =
  next_descendant access_ops cursor_context

let cursor_descendant access_ops input_cursor =
  let cursor_context = build_cursor_context input_cursor in
  Cursor.cursor_of_function (next_event access_ops cursor_context)

