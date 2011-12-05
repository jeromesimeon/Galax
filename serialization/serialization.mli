(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: serialization.mli,v 1.8 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Serialization
   Description:
     This module supports serialization of XML values either from the
     XML Query data model or directly from an XML
     stream. Serialization can occur directly on any given output
     formatter. (See the OCaml Format library).
*)

(* Serialization from an XML stream *)

val fserialize_xml_stream          : Processing_context.processing_context -> Format.formatter -> Streaming_types.xml_stream -> unit
val serialize_xml_stream  	   : Processing_context.processing_context -> Streaming_types.xml_stream -> unit
val bserialize_xml_stream 	   : Processing_context.processing_context -> Streaming_types.xml_stream -> string

(* Serialization from a resolved stream *)

val fserialize_resolved_xml_stream : Processing_context.processing_context -> Format.formatter -> Streaming_types.xml_stream -> unit
val serialize_resolved_xml_stream  : Processing_context.processing_context -> Streaming_types.xml_stream -> unit
val bserialize_resolved_xml_stream : Processing_context.processing_context -> Streaming_types.xml_stream -> string

(* Serialization from a typed stream *)

val fserialize_typed_xml_stream    : Processing_context.processing_context -> Format.formatter -> Streaming_types.typed_xml_stream -> unit
val serialize_typed_xml_stream     : Processing_context.processing_context -> Streaming_types.typed_xml_stream -> unit
val bserialize_typed_xml_stream    : Processing_context.processing_context -> Streaming_types.typed_xml_stream -> string

(* Serialization from an XML data model *)

val fserialize_datamodel  	   : Processing_context.processing_context -> Format.formatter -> Physical_value.item Cursor.cursor -> unit
val serialize_datamodel   	   : Processing_context.processing_context -> Physical_value.item Cursor.cursor -> unit
val bserialize_datamodel  	   : Processing_context.processing_context -> Physical_value.item Cursor.cursor -> string

