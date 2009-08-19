(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: planio_physical_value.mli,v 1.2 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Planio_physical_value
   Description:
     This module parses and prints physical values.
*)


val box_tree_result :
    Xquery_physical_type_ast.physical_xml_type -> 
      Physical_value.xml_value -> 
	Streaming_types.typed_xml_stream

val unbox_tree_result :
    Processing_context.processing_context ->
      Streaming_types.resolved_xml_stream ->
	Physical_value.xml_value

val box_table_result :
    Xquery_physical_type_ast.physical_tuple_type -> 
      Physical_value.tuple Cursor.cursor -> 
	Streaming_types.typed_xml_stream

val unbox_table_result :
    Processing_context.processing_context ->
      Xquery_physical_type_ast.physical_tuple_type -> 
	Streaming_types.resolved_xml_stream ->
	  Physical_value.tuple Cursor.cursor

val unbox_and_load_item_sequence :  
    Streaming_types.typed_xml_stream ->  
      Physical_value.item list

val box_var_value :  
    Namespace_context.nsenv ->
      Namespace_names.rqname * Physical_value.xml_value ->
	Streaming_types.typed_xml_stream

val unbox_var_value : 
    Streaming_types.resolved_sax_event Stream.t ->
      Namespace_names.rqname * Streaming_types.resolved_xml_stream

