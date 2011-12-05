(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: stream_project.mli,v 1.5 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Stream_project
   Description:
     This module implements document projection on an XML stream.
*)


(* Applies projection on the XML stream *)

val project_xml_stream_from_document :
    Datatypes.xs_anyURI ->
	Path_struct.rooted_path_sequence ->
	  Streaming_types.xml_stream -> Streaming_types.xml_stream

val project_xml_stream_from_variable :
    Xquery_common_ast.cvname ->
	Path_struct.rooted_path_sequence ->
	  Streaming_types.xml_stream -> Streaming_types.xml_stream

