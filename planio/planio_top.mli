(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: planio_top.mli,v 1.2 2007/02/01 22:08:51 simeon Exp $ *)

(* 
   Module: Planio_top
   Description:
     This module parses the algebraic plans which are serialized by
     algebra_print_xml.ml so that they may be run again after
     modification
*)

open Xquery_algebra_ast
  
(****************************)
(* STATEMENTS/EXPRESSIONS   *)
(****************************)

val box_logical_algebra_statement  : 
    Namespace_context.nsenv -> 
      Algebra_type.algop_expr ->
	Streaming_types.resolved_xml_stream

val serialize_logical_algebra_statement : 
    Processing_context.processing_context -> 
      Galax_io.output_spec -> 
	Algebra_type.algop_expr -> 
	  unit

val parse_logical_algebra_statement :
    Processing_context.processing_context ->
      Logical_algebra_types.logical_compile_context ->
	Galax_io.input_spec -> 
	  Logical_algebra_types.logical_algop_expr

(******************)
(* MODULES        *)
(******************)

val box_logical_algebra_module : 
    Namespace_context.nsenv -> 
      ('a,'b, 'c) Xquery_algebra_ast.aalgop_xmodule -> 
	Streaming_types.resolved_xml_stream

val parse_logical_algebra_module :
    Processing_context.processing_context -> 
      Logical_algebra_types.logical_compile_context -> 
	Galax_io.input_spec ->
	  (Logical_algebra_types.logical_algop_xmodule * Logical_algebra_types.logical_compile_context)

(******************)
(* CLOSURES       *)
(******************)

val box_closure_environment : 
  Namespace_context.nsenv ->
    (Xquery_common_ast.cvname * Physical_value.xml_value) list ->
      (Xquery_common_ast.cvname * Physical_value.xml_value) list ->
	Streaming_types.resolved_xml_stream

val box_closure : 
    Namespace_context.nsenv ->
    (* environment stream *)              
      Streaming_types.resolved_xml_stream -> 
      (* plan stream *) 
	Streaming_types.resolved_xml_stream ->       
        (* closure stream *)
	  Streaming_types.resolved_xml_stream

val parse_closure:
    Processing_context.processing_context -> 
      Logical_algebra_types.logical_compile_context -> 
	Galax_io.input_spec ->
    (* ((Variable, Value) list * (Tuple-field Value) list) * Algebraic plan *)
	  ((Namespace_names.rqname * Streaming_types.resolved_xml_stream) list * 
	     (Namespace_names.rqname * Streaming_types.resolved_xml_stream) list) *
	    Logical_algebra_types.logical_algop_expr

