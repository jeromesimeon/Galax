(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_export_context.ml,v 1.4 2007/02/01 22:08:51 simeon Exp $ *)

open Error

open Cursor

open Dm_types
open Physical_value


(**********************)
(* The export context *)
(**********************)

type export_context =
    { mutable current_cursor : item cursor;
      stacked_nodes : (Streaming_types.typed_annotated_sax_event * item cursor) Stack.t}


(********************************)
(* Creates a new export context *)
(********************************)

let build_export_context il =
  { current_cursor = il;
    stacked_nodes = Stack.create () }


(************************************)
(* Operations on the export context *)
(************************************)

(* Access the current cursor to stream *)

let get_current_cursor export_context =
  export_context.current_cursor

(* Push a context (getting inside of an element/document node) *)

let push_node_to_export_context export_context node new_current_cursor =
  let pop_event =
    match node#node_kind() with
    | DocumentNodeKind -> Streaming_util.fmktse_event Streaming_types.TSAX_endDocument Finfo.bogus
    | ElementNodeKind -> Streaming_util.fmktse_event Streaming_types.TSAX_endElement Finfo.bogus
    | AttributeNodeKind
    | TextNodeKind
    | ProcessingInstructionNodeKind
    | CommentNodeKind ->
	raise (Query (Stream_Error "Should never push a non-document and non-element node during datamodel to SAX streaming"))
  in
  Stack.push (pop_event, export_context.current_cursor) export_context.stacked_nodes;
  export_context.current_cursor <- new_current_cursor

(* Pop a context (getting outside of an element/document node) *)

let pop_node_from_export_context export_context =
  try
    let (event,replacement_cursor) = Stack.pop export_context.stacked_nodes in
    begin
      export_context.current_cursor <- replacement_cursor;
      Some event
    end
  with
  | Stack.Empty ->
      None

