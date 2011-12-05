(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_export.mli,v 1.4 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_export
   Description:
     This module exports a data model instance into an XML stream.
*)

(* Creates a stream representing the data model instance directly *)

val typed_xml_stream_of_datamodel : Physical_value.item Cursor.cursor -> Streaming_types.typed_xml_stream

(* Creates a stream representing the data model instance possibly
   wrapped into an element node to make it a well-formed XML if
   necessary. *)

val resolved_xml_stream_of_datamodel : Physical_value.item Cursor.cursor -> Streaming_types.xml_stream

val resolved_wrapped_xml_stream_of_datamodel : Physical_value.item Cursor.cursor -> Streaming_types.xml_stream

