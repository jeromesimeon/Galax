(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: streaming_conv.mli,v 1.7 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Stream_conv
   Description:
     This module provides data structures, accessor and conversion
     functions for an extended XML token stream model, labeled
     streams, namely.
*)

open Streaming_types


type typed_labeled_sax_event = sax_event

type typed_labeled_xml_stream = xml_stream


(*****************)
(* Stream access *)
(*****************)

val get_flag :
  typed_labeled_sax_event -> bool

val set_flag :
  typed_labeled_sax_event -> unit

val unset_flag :
  typed_labeled_sax_event -> unit


(*********************)
(* Stream conversion *)
(*********************)

val typed_labeled_of_typed_xml_stream : typed_xml_stream -> typed_xml_stream

val typed_of_typed_labeled_xml_stream : typed_xml_stream -> typed_xml_stream


(******************)
(* Stream slicing *)
(******************)

(* Chopping a stream into separate sub-streams is needed for
   operators that need to access a stream item-at-a-time,
   such as an item to tuple map. - Michael *)
val slice_typed_xml_stream : typed_xml_stream -> typed_xml_stream Cursor.cursor

(* Chops a stream into slices, discarding each of them individually.
   This is useful whenever the item-cardinality of a stream is needed
   without actually accessing these items. - Michael *)
val slice_discard_typed_xml_stream : typed_xml_stream -> unit Cursor.cursor

val item_count_typed_labeled_xml_stream : typed_xml_stream -> int

val item_range_typed_labeled_xml_stream : typed_xml_stream -> int -> int -> typed_xml_stream
val nth_item_typed_labeled_xml_stream   : typed_xml_stream -> int -> typed_xml_stream
val first_item_typed_labeled_xml_stream : typed_xml_stream -> typed_xml_stream


