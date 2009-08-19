(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_xml_value.ml,v 1.8 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_xml_value
   Description:
     This modules defines physical representations of XML values.
*)

open Physical_value
open Physical_item
open Physical_sequence


(* Conversions *)

let dom_value_of_sax_value sv =
  let nodeid_context = Nodeid_context.default_nodeid_context () in
  materialized_of_list
    (Physical_load.load_xml_value_from_typed_stream nodeid_context sv)

let sax_value_of_dom_value xv =
  Physical_export.typed_xml_stream_of_datamodel (cursor_of_sequence xv)

let materialize_xml_value xv =
  match xv with
  | DomValue s -> DomValue (materialize_sequence s)
  | SaxValue s ->
      let nodeid_context = Nodeid_context.default_nodeid_context () in
      (DomValue
	 (LSeq (Physical_load.load_xml_value_from_typed_stream nodeid_context s)))

let dom_value_of_xml_value xv =
  match xv with
  | DomValue mv -> mv
  | SaxValue sv -> dom_value_of_sax_value sv

let sax_value_of_xml_value xv =
  match xv with
  | DomValue mv -> sax_value_of_dom_value mv
  | SaxValue sv -> sv

let xml_value_of_dom_value mv = DomValue mv
let xml_value_of_sax_value sv = SaxValue sv

let is_empty_xml_value xv =
  match xv with
  | DomValue mv -> sequence_is_empty mv
  | SaxValue sv -> Streaming_ops.is_empty_typed_xml_stream sv

let item_cursor_of_xml_value pv =
  cursor_of_sequence (dom_value_of_xml_value pv)

let item_list_of_xml_value pv =
  list_of_sequence (dom_value_of_xml_value pv)

let xml_value_of_item_list v     = (DomValue (LSeq v))
let xml_value_of_item_cursor v   = (DomValue (CSeq v))


