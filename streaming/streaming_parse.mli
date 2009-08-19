(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: streaming_parse.mli,v 1.5 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Streaming_parse
   Description:
     This module implements a SAX parser for Galax.
*)

val glx_normalized_public_id : string -> string


(* Creates an XML stream from a string URI buffer and if a DTD exists
  and was parsed successfully, returns the XQuery type representing
  that DTD and the corresponding XML stream *)


val open_pxp_stream_from_io :
    Galax_io.input_spec -> Galax_io.entity_kind -> Galax_io.pxp_stream * string option

val open_xml_stream_from_pxp_stream : Galax_io.pxp_stream * string option -> (Pxp_dtd.dtd option * Streaming_types.xml_stream)

(* Opens a document entity or a document fragment *)
val open_xml_entity_stream_from_io : Galax_io.input_spec -> Galax_io.entity_kind -> (Pxp_dtd.dtd option * Streaming_types.xml_stream)

(* Opens a document entity *)
val open_xml_stream_from_io : Galax_io.input_spec -> (Pxp_dtd.dtd option * Streaming_types.xml_stream)

val parse_standalone_dtd : Galax_io.input_spec -> Pxp_dtd.dtd

