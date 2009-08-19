(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_validation_context.mli,v 1.8 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_validation_context
   Description:
     This modules implements the context used during XML Schema
     validation.
*)

open Xquery_common_ast
open Xquery_type_core_ast


(* The validation context *)

type validation_context

val build_validation_context  : Namespace_context.nsenv -> cxschema -> Streaming_types.resolved_xml_stream -> validation_context

(* Namespace environment handling *)

val push_nsenv        : validation_context -> Namespace_context.nsenv -> unit
val get_namespace_env : validation_context -> Namespace_context.nsenv

val get_current_content_model : validation_context -> cxtype
val get_cxschema              : validation_context -> cxschema
val has_mixed_content         : validation_context -> bool
val has_been_nilled           : validation_context -> bool

(* Events *)

val next_validation_event     : validation_context -> Streaming_types.resolved_sax_event

(* Document events *)

val push_document_event : validation_context -> unit
val pop_document_event  : validation_context -> unit

(* Element events *)

(* Validation context should also include name of current element *)
val push_complex_element_event : validation_context -> cxtype -> mixed -> bool -> cxtype -> unit
val push_simple_element_event  : validation_context -> cxtype -> Streaming_types.resolved_sax_event list -> unit

val pop_element_event          : validation_context -> unit


