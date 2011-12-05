(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: streaming_constructors.mli,v 1.10 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Stream_constructors
   Description:
     Construction operations over XML streams.
*)

open Finfo

open Streaming_types


(***********************)
(* Stream constructors *)
(***********************)

val text_constructor      : typed_xml_stream -> typed_xml_stream
val charref_constructor   : int -> typed_xml_stream
val pi_constructor        : bool -> Namespace_names.ncname -> typed_xml_stream -> typed_xml_stream
val comment_constructor   : typed_xml_stream -> typed_xml_stream
val attribute_constructor : Namespace_symbols.rattr_symbol -> Namespace_context.nsenv -> typed_xml_stream -> typed_xml_stream
val element_constructor   : Dm_atomic.atomicAnyURI option ref -> Namespace_symbols.relem_symbol -> Namespace_context.nsenv -> typed_xml_stream -> typed_xml_stream
val document_constructor  : Dm_atomic.atomicAnyURI option ref -> typed_xml_stream -> typed_xml_stream
val sequence_constructor  : typed_xml_stream -> typed_xml_stream -> typed_xml_stream

val element_constructor_of_resolved   : Dm_atomic.atomicAnyURI option ref -> Namespace_symbols.relem_symbol -> Namespace_context.nsenv -> xml_stream -> xml_stream


(****************************)
(* Serialization operations *)
(****************************)

val glx_result_serialization : typed_xml_stream -> typed_xml_stream

val sequence_normalization : typed_xml_stream -> typed_xml_stream

