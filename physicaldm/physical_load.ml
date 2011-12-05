(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_load.ml,v 1.7 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_load
   Description:
     This contains operations for loading streams as in-memory
     representation.
*)

open Error

open Streaming_types
open Nodeid
open Nodeid_context
open Dm

open Physical_value
open Physical_name_index

type document_id_generator = unit -> docid
type load_xml_value_function = ordered_xml_stream -> item list
type load_node_sequence_function = ordered_xml_stream -> Dm.node list
type load_xml_document_function = ordered_xml_stream -> item list

let new_docid =
  (fun () -> (raise (Query (Internal_Error "No main-memory data model support compiled!!"))))

let load_xml_value_from_typed_ordered_stream =
  (fun typed_ordered_xml_stream -> (raise (Query (Internal_Error "No main-memory data model support compiled!!"))))

let load_xml_node_sequence_from_typed_ordered_stream =
  (fun typed_ordered_xml_stream -> (raise (Query (Internal_Error "No main-memory data model support compiled!!"))))

let load_xml_document_from_typed_ordered_stream =
  (fun typed_ordered_xml_stream -> (raise (Query (Internal_Error "No main-memory data model support compiled!!"))))

type load_functions =
    { mutable load_docid_gen : document_id_generator;
      mutable load_xml_value : load_xml_value_function;
      mutable load_node_sequence : load_node_sequence_function;
      mutable load_document : load_xml_document_function; }

let actual_load_funs =
  { load_docid_gen = new_docid;
    load_xml_value = load_xml_value_from_typed_ordered_stream;
    load_node_sequence = load_xml_node_sequence_from_typed_ordered_stream;
    load_document = load_xml_document_from_typed_ordered_stream }
  
let register_load_functions load_fun0 load_fun1 load_fun2 load_fun3 =
  actual_load_funs.load_docid_gen <- load_fun0;
  actual_load_funs.load_xml_value <- load_fun1;
  actual_load_funs.load_node_sequence <- load_fun2;
  actual_load_funs.load_document <- load_fun3

(* Load a data model instance from an XML stream *)

let load_xml_document_from_typed_stream nodeid_context doc_stream =
  incr Conf.countload;
  try
    let docid = actual_load_funs.load_docid_gen () in
    let typed_ordered_xml_stream = 
      Streaming_ops.ordered_typed_of_typed_stream_for_docid docid nodeid_context doc_stream
    in
    actual_load_funs.load_document typed_ordered_xml_stream
  with exn -> 
    (printf_error_safe "In load_xml_document_from_typed_stream: " exn; raise exn)

let load_xml_node_sequence_from_typed_stream nodeid_context doc_stream =
  incr Conf.countload;
  let docid = actual_load_funs.load_docid_gen () in
  let typed_ordered_xml_stream = 
    Streaming_ops.ordered_typed_of_typed_stream_for_docid docid nodeid_context doc_stream
  in
  actual_load_funs.load_node_sequence typed_ordered_xml_stream

let load_xml_value_from_typed_stream nodeid_context doc_stream =
  incr Conf.countload;
  let docid = actual_load_funs.load_docid_gen () in
  let typed_ordered_xml_stream =
    Streaming_ops.ordered_typed_of_typed_stream_for_docid docid nodeid_context doc_stream
  in
  actual_load_funs.load_xml_value typed_ordered_xml_stream

let load_xml_node_sequence_from_typed_stream_for_docid nodeid_ctxt docid doc_stream =
  incr Conf.countload;
  let typed_ordered_xml_stream = 
    Streaming_ops.ordered_typed_of_typed_stream_for_docid docid nodeid_ctxt doc_stream
  in
  actual_load_funs.load_node_sequence typed_ordered_xml_stream

let load_xml_document_from_typed_stream_for_docid nodeid_ctxt docid doc_stream =
  incr Conf.countload;
  let typed_ordered_xml_stream = 
    Streaming_ops.ordered_typed_of_typed_stream_for_docid docid nodeid_ctxt doc_stream
  in
  actual_load_funs.load_document typed_ordered_xml_stream

