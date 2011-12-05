(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: serialization_context.ml,v 1.13 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Serialization_context
   Description:
     This module implements a the serialization context.
*)

open Error

(* A type for the serialization context *)

type serial_element_kind =
  | ElementContentInTextElement
  | ElementContentInContentElement
  | TextElementInElementContent
  | TextElementInTextContent
  | TopElement

type serialization_context =
    { serial_kind : Processing_context.serialization_kind;
      serial_internal_encoding : Encoding.rep_encoding;
      serial_target_encoding : Encoding.encoding;
      serial_whitespace : Whitespace.mode;
      serial_stream : Streaming_types.xml_stream;
      serial_stack :  (Namespace_names.uqname * serial_element_kind) Stack.t;
      serial_inside_document : bool ref }

(* Creates a new loading context *)

let build_serialization_context kind inenc outenc wsmode stream =
  { serial_kind = kind;
    serial_internal_encoding = inenc;
    serial_target_encoding = outenc;
    serial_whitespace = wsmode;
    serial_stream = stream;
    serial_stack = Stack.create ();
    serial_inside_document = ref false }

let get_serialization_kind serial_context =
  serial_context.serial_kind

let get_internal_encoding serial_context =
  serial_context.serial_internal_encoding

let get_target_encoding serial_context =
  serial_context.serial_target_encoding

let get_next_event serial_context =
  Cursor.cursor_next serial_context.serial_stream

let peek_next_event serial_context =
  Cursor.cursor_peek serial_context.serial_stream

let get_current_element_kind serial_context =
  try
    snd (Stack.top serial_context.serial_stack)
  with
  | Stack.Empty ->
      TopElement

let new_current_element_kind ek hec =
  match ek with
  | ElementContentInTextElement
  | ElementContentInContentElement ->
      if !hec then ElementContentInContentElement else TextElementInElementContent
  | TextElementInElementContent
  | TextElementInTextContent ->
      TextElementInTextContent
      (* We should not try to be too smart... The following doesnt work well.
	 if hec then ElementContentInTextElement else TextElementInTextContent
	 *)
  | TopElement ->
      (* Note:
	   The top-level elements are treated as within nodes with
	   element content.
	  - Jerome
       *)
      (* TURNING PRETTY-PRINTING OFF *)
      (* if (hec) then ElementContentInContentElement else *)
      TextElementInElementContent

let push_element serial_context x =
  Stack.push x serial_context.serial_stack

let push_document serial_context =
  serial_context.serial_inside_document := true

let pop_element serial_context =
  try
    Stack.pop serial_context.serial_stack
  with
  | Stack.Empty ->
      raise (Query (Serialization "Empty stack during serialization"))

let pop_document serial_context =
  serial_context.serial_inside_document := false

let is_toplevel serial_context =
  not(!(serial_context.serial_inside_document))
    &&
  (try
    ignore(Stack.top serial_context.serial_stack);
    false
  with
  | Stack.Empty ->
      true)

let is_empty serial_context =
  match Cursor.cursor_peek serial_context.serial_stream with
  | None ->
      true
  | Some _ ->
      false

