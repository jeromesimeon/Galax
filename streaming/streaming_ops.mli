(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: streaming_ops.mli,v 1.8 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Streaming_ops
   Description:
     Some basic operations on XML streams.
*)


(********************)
(* The empty stream *)
(********************)

val empty_xml_stream                     : unit -> Streaming_types.xml_stream
    (* Discards the current XML subtree *)

val empty_typed_xml_stream               : unit -> Streaming_types.typed_xml_stream
    (* Discards the current typed XML subtree *)


(***************************************)
(* Validity checks on stream contents  *)
(***************************************)
val check_valid_processing_instruction : string -> string -> string * string
val check_valid_comment : string -> bool 

(***********************)
(* Discarding a stream *)
(***********************)

val discard_xml_stream                     : Streaming_types.xml_stream -> unit
    (* Discards the current XML subtree *)

val discard_typed_xml_stream               : Streaming_types.typed_xml_stream -> unit
    (* Discards the current typed XML subtree *)


(*******************************************************)
(* Conversion between well-formed and resolved streams *)
(*******************************************************)

val resolve_xml_stream : Streaming_types.xml_stream -> Streaming_types.xml_stream
    (* Resolves namespaces in the original well-formed stream *)

val prefix_xml_stream : Streaming_types.xml_stream -> Streaming_types.xml_stream
    (* Turns a resolved XML stream back into one with prefixes *)


(*************************************************)
(* Conversion between resolved and typed streams *)
(*************************************************)

val typed_of_resolved_xml_stream : Streaming_types.xml_stream -> Streaming_types.typed_xml_stream
    (* Treats a resolved XML stream as 'typed'. I.e., adds
       xs:untypedAtomic and xs:untyped at the right places. *)

val erase_xml_stream : Streaming_types.typed_xml_stream -> Streaming_types.xml_stream
    (* Turns a typed XML stream into a resolved, non-typed one *)

val erase_xml_stream_section_3_7_1 : Streaming_types.typed_xml_stream -> Streaming_types.xml_stream
    (* Turns a typed XML stream into a resolved, non-typed one, but
       also turns atomic values into text nodes according to the
       semantics in Section 3.7.1 of the XQuery 1.0 document, and
       rejects attribute events, since they should have been processed
       before-hand from the beginning of the stream. *)


(**********************************************)
(* Conversion between typed and ordered typed *)
(**********************************************)

val ordered_typed_of_typed_stream_for_docid :
    Nodeid.docid -> Nodeid_context.nodeid_context -> Streaming_types.typed_xml_stream -> Streaming_types.ordered_xml_stream
val ordered_typed_of_typed_stream :
    Nodeid.docid_gen -> Nodeid_context.nodeid_context -> Streaming_types.typed_xml_stream -> Streaming_types.ordered_xml_stream

(**********************)
(* Stream composition *)
(**********************)

val compose_xml_streams : Streaming_types.xml_stream -> Streaming_types.xml_stream list -> Streaming_types.xml_stream
    (* Compose XML streams together.
       [compose_xml_streams s0 [s1;...;sk]] builds a stream where s1,
       ..., sk are inserted in the k "holes" found in stream s0. *)

val compose_typed_xml_streams : Streaming_types.typed_xml_stream -> Streaming_types.typed_xml_stream list -> Streaming_types.typed_xml_stream
    (* Compose typed XML streams together.
       [compose_typed_xml_streams s0 [s1;...;sk]] builds a stream where s1,
       ..., sk are inserted in the k "holes" found in stream s0. *)


(***************************)
(* Simple stream accessors *)
(***************************)

val is_empty_xml_stream : Streaming_types.xml_stream -> bool
    (* Returns true is the stream is empty *)

val is_empty_typed_xml_stream : Streaming_types.typed_xml_stream -> bool
    (* Returns true is the typed stream is empty *)

val consume_leading_attribute_events :
    Streaming_types.xml_stream -> Streaming_types.sax_xml_attribute_forest
    (* Return all of the leading attributes in the stream *)
