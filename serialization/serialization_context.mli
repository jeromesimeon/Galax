(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: serialization_context.mli,v 1.8 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Serialization_context
   Description:
     This module implements a the serialization context.
*)


(* Kinds of elements *)

type serial_element_kind =
  | ElementContentInTextElement
  | ElementContentInContentElement
  | TextElementInElementContent
  | TextElementInTextContent
  | TopElement

(* A type for the serialization context *)

type serialization_context

(* Creates a new loading context *)

val build_serialization_context :
    Processing_context.serialization_kind ->
      Encoding.rep_encoding ->
	Encoding.encoding ->
	  Whitespace.mode ->
	    Streaming_types.xml_stream ->
	      serialization_context

val get_serialization_kind : serialization_context -> Processing_context.serialization_kind

val get_internal_encoding  : serialization_context -> Encoding.rep_encoding
val get_target_encoding    : serialization_context -> Encoding.encoding

val get_next_event         : serialization_context -> Streaming_types.sax_event
val peek_next_event        : serialization_context -> Streaming_types.sax_event option

val get_current_element_kind : serialization_context -> serial_element_kind
val new_current_element_kind : serial_element_kind -> Streaming_types.has_element_content -> serial_element_kind

val push_element : serialization_context -> (Namespace_names.uqname * serial_element_kind) -> unit
val pop_element  : serialization_context -> (Namespace_names.uqname * serial_element_kind)

val push_document : serialization_context -> unit
val pop_document  : serialization_context -> unit

val is_toplevel : serialization_context -> bool
val is_empty    : serialization_context -> bool

