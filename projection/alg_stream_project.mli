(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: alg_stream_project.mli,v 1.2 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Alg_stream_project
   Description:
     This module implements document projection on an XML stream.
*)


(* Applies projection on the XML stream *)

val project_xml_stream_from_document :
    Datatypes.xs_anyURI ->
	Alg_path_struct.rooted_path_sequence ->
	  Streaming_types.typed_xml_stream -> Streaming_types.typed_xml_stream
(*
val project_xml_stream_from_variable :
    Xquery_common_ast.cvname ->
	Alg_path_struct.rooted_path_sequence ->
	  Streaming_types.resolved_xml_stream -> Streaming_types.resolved_xml_stream
*)
