(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_load.mli,v 1.5 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_load
   Description:
     This contains operations for loading streams as in-memory
     representation.
*)

open Streaming_types

open Nodeid
open Nodeid_context

open Physical_value

type document_id_generator = unit -> docid
type load_xml_value_function = ordered_typed_xml_stream -> item list
type load_node_sequence_function = ordered_typed_xml_stream -> Dm.node list
type load_xml_document_function = ordered_typed_xml_stream -> item list

val register_load_functions :
    document_id_generator -> load_xml_value_function -> load_node_sequence_function -> load_xml_document_function -> unit

(* Load a data model instance from an XML stream *)

val load_xml_value_from_typed_stream :
    nodeid_context -> typed_xml_stream -> item list
val load_xml_node_sequence_from_typed_stream :
    nodeid_context -> typed_xml_stream -> Dm.node list
val load_xml_document_from_typed_stream  :
    nodeid_context -> typed_xml_stream -> item list
(* Note:
     The following function can be used to apply loading inside an
     existing document. This is use to support updates, notably.
   - Jerome *)

val load_xml_node_sequence_from_typed_stream_for_docid :
    nodeid_context -> docid -> typed_xml_stream -> Dm.node list

