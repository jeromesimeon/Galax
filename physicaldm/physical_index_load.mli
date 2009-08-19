(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_index_load.mli,v 1.6 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_index_load
   Description:
     This module contains support for registering different loading
     operations with the system.
*)

open Streaming_types
open Nodeid
open Nodeid_context

open Physical_name_index
open Physical_value

type load_function0 = unit -> docid
type load_function1 = name_indices_handler -> ordered_typed_xml_stream -> item list
type load_function2 = name_indices_handler -> ordered_typed_xml_stream -> Dm.node list
type load_function3 = name_indices_handler -> ordered_typed_xml_stream -> item list

val register_load_functions :
    load_function0 -> load_function1 -> load_function2 -> load_function3 -> unit

val load_xml_document_from_typed_stream_for_docid :
    nodeid_context -> docid -> name_indices_handler ->  typed_xml_stream -> item list

