(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_index_load.ml,v 1.6 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_index_load
   Description:
     This module contains support for registering different loading
     operations with the system.
*)

open Error

open Streaming_types
open Nodeid
open Nodeid_context
open Dm

open Physical_name_index
open Physical_value

type load_function0 = unit -> docid
type load_function1 = name_indices_handler -> ordered_typed_xml_stream -> item list
type load_function2 = name_indices_handler -> ordered_typed_xml_stream -> node list
type load_function3 = name_indices_handler -> ordered_typed_xml_stream -> item list

let new_docid =
  (fun () -> (raise (Query (Internal_Error "No main-memory data model support compiled!!"))))

let load_xml_value_from_typed_ordered_stream =
  (fun index_names typed_ordered_xml_stream -> (raise (Query (Internal_Error "No main-memory data model support compiled!!"))))

let load_xml_node_sequence_from_typed_ordered_stream =
  (fun index_names typed_ordered_xml_stream -> (raise (Query (Internal_Error "No main-memory data model support compiled!!"))))

let load_xml_document_from_typed_ordered_stream =
  (fun index_names typed_ordered_xml_stream -> (raise (Query (Internal_Error "No main-memory data model support compiled!!"))))

type load_functions =
    { mutable load_docid_gen : load_function0;
      mutable load_xml_value : load_function1;
      mutable load_node_sequence : load_function2;
      mutable load_document : load_function3 }

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

let load_xml_document_from_typed_stream_for_docid nodeid_ctxt docid index_names doc_stream =
  let typed_ordered_xml_stream = 
    Streaming_ops.ordered_typed_of_typed_stream_for_docid docid nodeid_ctxt doc_stream
  in
  actual_load_funs.load_document index_names typed_ordered_xml_stream

