(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_value_util.ml,v 1.8 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_value_util
   Description:
     This modules defines various utilities (notably boxing/unboxing)
     for physical values.
*)

(*
   Values manipulated include :
       - XML Trees
       - SAX streams
       - Tuples
*)

open Error

open Physical_value
open Physical_sequence
open Physical_item
open Physical_xml_value
open Physical_table


(* Local exceptions *)

let raise_not_a_table_error () =
  raise (Query (Physical_DM_Error "Not an homogeneous table"))

let raise_unsafe_xml_value () =
  raise (Query (Physical_DM_Error "[Physical_value] Unsafely accessing a SAX-XMLValue from within a tuple"))

let raise_not_an_xml_value_error () =
  raise (Query (Physical_DM_Error "Not an homogeneous XML value"))

let raise_hybrid_sequence_error () =
  raise (Query (Physical_DM_Error "Appying map-concat on an hybrid sequence of tuples and values!"))


(*******************************************)
(* From specific values to physical values *)
(*******************************************)

let physical_value_of_xml_value v     = PXMLValue v
let physical_value_of_dom_value v     = PXMLValue (DomValue v)
let physical_value_of_item v          = PXMLValue (DomValue (LSeq [v]))
let physical_value_of_item_list v     = PXMLValue (DomValue (LSeq v))
let physical_value_of_item_cursor v   = PXMLValue (DomValue (CSeq v))
let physical_value_of_sax_value sax   = PXMLValue (SaxValue sax)

let physical_value_of_tuple t         = PTable (table_of_singleton t)
let physical_value_of_tuple_cursor ts = PTable ts


(*************************************)
(* Basic physical value constructors *)
(*************************************)

let empty_xml_value () = physical_value_of_item_list []
let empty_sax_value () = physical_value_of_sax_value (Streaming_ops.empty_typed_xml_stream())

let is_empty_physical_value pv =
  match pv with
  | PXMLValue xv -> is_empty_xml_value xv
  | PTable t     -> table_is_empty t

(*******************************************)
(* From physical values to specific values *)
(*******************************************)

(* Cast from/to tables/xml_values/sax_values *)

(* Note:
     The casting operations are such that they should treat the empty
     sequence as polymorphic.
   - Jerome 03/26/2004
 *)

let dom_value_of_table t =
  if table_is_empty t
  then sequence_empty()
  else raise_not_an_xml_value_error()

let xml_value_of_table t =
  dom_value_of_table t

let table_of_dom_value xv =
  if sequence_is_empty xv
  then table_empty ()
  else raise_not_a_table_error()

let table_of_sax_value sv =
  if Streaming_ops.is_empty_typed_xml_stream sv
  then table_empty()
  else raise_not_a_table_error()

let sax_value_of_table t =
  if table_is_empty t
  then Streaming_ops.empty_typed_xml_stream()
  else raise_not_an_xml_value_error()


(* Actual operations physical values *)

let xml_value_of_physical_value pv =
  match pv with
  | PXMLValue (DomValue xv) -> (DomValue xv)
  | PXMLValue (SaxValue xv) -> raise_unsafe_xml_value()
  | PTable t                -> (DomValue (xml_value_of_table t))

let unsafe_xml_value_of_physical_value pv =
  match pv with
  | PXMLValue xv -> xv
  | PTable t     -> (DomValue (xml_value_of_table t))
let xml_value_of_physical_value pv =
  unsafe_xml_value_of_physical_value pv

let table_of_physical_value pv =
  match pv with
  | PXMLValue (DomValue xv) -> table_of_dom_value xv
  | PXMLValue (SaxValue sv) -> table_of_sax_value sv
  | PTable t 	            -> t

let tuple_cursor_of_physical_value pv =
  table_of_physical_value pv

let sax_value_of_physical_value pv =
  match pv with
  | PXMLValue xv -> sax_value_of_xml_value xv
  | PTable t     -> sax_value_of_table t

let dom_value_of_physical_value pv =
  match pv with
  | PXMLValue xv -> dom_value_of_xml_value xv
  | PTable t     -> dom_value_of_table t

let item_cursor_of_physical_value pv =
  cursor_of_sequence (dom_value_of_physical_value pv)

let item_list_of_physical_value pv =
  list_of_sequence (dom_value_of_physical_value pv)


(********************************************)
(* Materialize/Streaming of physical values *)
(********************************************)

let stream_physical_value pv =
  match pv with
  | PXMLValue (DomValue is) -> PXMLValue (DomValue (stream_sequence is))
  | PXMLValue (SaxValue s)  ->
      let nodeid_context = Nodeid_context.default_nodeid_context () in
      PXMLValue (DomValue (streamed_of_list (Physical_load.load_xml_value_from_typed_stream nodeid_context s)))
  | PTable ts               -> PTable ts

let materialize_physical_value ev =
  match ev with
  | PXMLValue xv -> PXMLValue (materialize_xml_value xv)
  | PTable s        -> raise (Query (Physical_DM_Error "Cannot materialize a table (tuple cursor) from the functional physical DM interface"))


(**************************************************************)
(* Special operation used in the evaluation code of for loops *)
(**************************************************************)

let concat_physical_value_cursor pvs =
  let cleaned_cursor =
    Cursor.cursor_filter (fun x -> not (is_empty_physical_value x)) pvs
  in
  match Cursor.cursor_peek cleaned_cursor with
  | None -> empty_xml_value ()
  | Some (PTable _) ->
      begin
	try
	  let concat_cursor = Cursor.cursor_map_concat tuple_cursor_of_physical_value cleaned_cursor in
	  physical_value_of_tuple_cursor concat_cursor
	with
	| _ ->
	    raise_hybrid_sequence_error ()
      end
  | Some (PXMLValue _) ->
      begin
	try
	  let concat_cursor = Cursor.cursor_map_concat item_cursor_of_physical_value cleaned_cursor in
	  physical_value_of_item_cursor concat_cursor
	with
	| _ ->
	    raise_hybrid_sequence_error ()
      end

(* Checks homogeneity of input xml_values (SaxValue vs. MatValue). - Michael *)
let concat_xml_value_cursor xml_values =
  let non_empty_xml_values =
    Cursor.cursor_filter (fun xml_value -> not (is_empty_xml_value xml_value)) xml_values
  in
  match Cursor.cursor_peek non_empty_xml_values with
    (* TODO: check the implications of returning a stream here! - Michael *)
    | None -> SaxValue (Streaming_ops.empty_typed_xml_stream ())
  | Some (SaxValue _) ->
      let sax_value_of_xml_value xml_value =
	match xml_value with
	| SaxValue token_cursor_value -> token_cursor_value
	| _ -> raise_hybrid_sequence_error ()
      in
      let concat_cursor = Cursor.cursor_map_concat sax_value_of_xml_value non_empty_xml_values in
      xml_value_of_sax_value concat_cursor
	
  | Some (DomValue _) ->
      let item_cursor_of_xml_value xml_value =
	match xml_value with
	| DomValue mat_value -> cursor_of_sequence mat_value
	| _ -> raise_hybrid_sequence_error ()
      in
      let xml_value_of_item_cursor item_cursor =
	DomValue (CSeq item_cursor)
      in
      let concat_cursor = Cursor.cursor_map_concat item_cursor_of_xml_value non_empty_xml_values in
      xml_value_of_item_cursor concat_cursor

let slice_xml_value xml_value =
  let wrap_item_sax typed_xml_stream = SaxValue typed_xml_stream in
  let wrap_item_mat mat_value = DomValue (CSeq (Cursor.cursor_of_singleton mat_value)) in
    match xml_value with
      | SaxValue typed_xml_stream ->
	  Cursor.cursor_map wrap_item_sax (Streaming_conv.slice_typed_xml_stream typed_xml_stream)
      | DomValue mat_value ->
	  match mat_value with
	      
	    (* Check the implications of converting to cursors here! - Michael *)
	    | LSeq item_list ->
		Cursor.cursor_map wrap_item_mat (Cursor.cursor_of_list item_list)
	    | CSeq item_cursor ->
		Cursor.cursor_map wrap_item_mat item_cursor

let slice_sax_value typed_xml_stream =
  Streaming_conv.slice_typed_xml_stream typed_xml_stream

let slice_discard_sax_value typed_xml_stream =
  Streaming_conv.slice_discard_typed_xml_stream typed_xml_stream

let slice_item_cursor item_cursor =
  let wrap_item item = Cursor.cursor_of_singleton item in
    Cursor.cursor_map wrap_item item_cursor

let slice_item_list item_list =
  let wrap_item item = item :: [] in
    (* This is different from slice_xml_value! We do not convert to item cursors here.- Michael *)
    Cursor.cursor_map wrap_item (Cursor.cursor_of_list item_list)


let is_nan i =
  match item_kind i with
  | AtomicValueKind ->
      let av = getAtomicValue i in
      begin
	match av#getAtomicValueKind() with
	| Datatypes.ATFloat ->
	    let f = av#getAtomicFloat() in
	    Decimal.is_nan f
	| Datatypes.ATDouble ->
	    let d = av#getAtomicDouble() in
	    Decimal.is_nan d
	| _ -> false
      end
  | NodeKind -> false
