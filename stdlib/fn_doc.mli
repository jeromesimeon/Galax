(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: fn_doc.mli,v 1.17 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Fn_doc
   Description:
     This module implements the fn:doc() function.
*)

(*************************)
(* Back-end registration *)
(*************************)

(* Note:
     The fn:doc() function supports files and HTTP natively, but can
     also be used to retrieve data in back-end stores (e.g., in
     Jungle). Such back-end stores must register themselves to the
     system using the following function.
   - Jerome
 *)

type back_end_call =
    Processing_context.processing_context -> string * string option * string -> Physical_value.item list

val register_back_end : string -> back_end_call -> unit

(* Alive documents *)

type alive_documents

val build_alive_documents_table : unit -> alive_documents
val build_dummy_alive_documents_table : unit -> alive_documents

val alive_documents_table_mem : alive_documents -> Datatypes.xs_string -> bool
val alive_documents_table_get : alive_documents -> Datatypes.xs_string -> Physical_value.item list
val alive_documents_table_put : alive_documents -> Datatypes.xs_string -> Physical_value.item list -> unit

val merge_alive_documents : alive_documents -> alive_documents -> alive_documents

(*********)
(* doc() *)
(*********)

(* Note:
     The main doc() function below takes an input URI and returns a
     function which evaluates to a data model value, which is the
     result of calling the fn:doc() function.
   - Jerome
*)

val lookup_document_from_io :
    Galax_io.input_spec -> alive_documents option -> Processing_context.processing_context -> Physical_value.item list

val lookup_doc_function :
    Datatypes.xs_string -> alive_documents option -> Processing_context.processing_context -> Physical_value.item list

val lookup_doc_function_no_table :
    Datatypes.xs_string -> Processing_context.processing_context -> Physical_value.item list

val lookup_doc_function_with_index :
    Datatypes.xs_string -> Physical_name_index.name_indices_handler -> alive_documents option -> Processing_context.processing_context -> Physical_value.item list



(* Note:
     The main collection() function below takes an input URI and returns a
     function which evaluates to a data model value, which is the
     result of calling the fn:collection() function.
   - Jerome
*)

val lookup_collection_function :
    Datatypes.xs_string -> alive_documents option -> Processing_context.processing_context -> Physical_value.item list

