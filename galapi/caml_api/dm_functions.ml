(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm_functions.ml,v 1.30 2007/02/01 22:08:47 simeon Exp $ *)

(* Module: Dm_functions
   Description:

   This module implements a number of short-cut functions to construct
   and access the XQuery 1.0 and XPath 2.0 data model.

*)

open Error

open Dm_atomic
open Dm

open Physical_value
open Physical_item
open Physical_item_util
open Physical_sequence

open Galax_dm


(***********************************************************)
(* Load context & node ids for dynamically allocated nodes *)
(***********************************************************)

let load_context () = Nodeid_context.default_nodeid_context ()

(***************************)
(* Data model constructors *)
(***************************)

let atomicString s       = new atomicString s
let atomicBoolean b      = new atomicBoolean b
let atomicInteger i      = new atomicInteger i
let atomicDecimal i      = new atomicDecimal i
let atomicFloat d        = new atomicFloat d
let atomicDouble d       = new atomicDouble d
let atomicAnyURI b       = new atomicAnyURI b
let atomicUntyped s      = new atomicUntyped s
let atomicQName(nsenv,s) = new atomicQName(Datatypes_util.qname_of_untyped nsenv s)

let local_proc_ctxt = Processing_context.default_processing_context ()

(* Dates, times, and durations are parsed from strings *)
let atomicDate              s = new atomicDate(Datatypes_lexer.parse_date (Parse_io.lexbuf_from_galax_input(Parse_io.galax_input_from_input_spec local_proc_ctxt (Galax_io.String_Input s))))
let atomicTime              s = new atomicTime(Datatypes_lexer.parse_time (Parse_io.lexbuf_from_galax_input(Parse_io.galax_input_from_input_spec local_proc_ctxt (Galax_io.String_Input s))))
let atomicDateTime          s = new atomicDateTime(Datatypes_lexer.parse_dateTime (Parse_io.lexbuf_from_galax_input(Parse_io.galax_input_from_input_spec local_proc_ctxt (Galax_io.String_Input s))))
let atomicDayTimeDuration   s = new atomicDayTimeDuration(Datatypes_lexer.parse_dayTimeDuration(Parse_io.lexbuf_from_galax_input(Parse_io.galax_input_from_input_spec local_proc_ctxt (Galax_io.String_Input s))))
let atomicYearMonthDuration s = new atomicYearMonthDuration(Datatypes_lexer.parse_yearMonthDuration(Parse_io.lexbuf_from_galax_input(Parse_io.galax_input_from_input_spec local_proc_ctxt (Galax_io.String_Input s))))

(*********************)
(* Node constructors *)
(*********************)

let documentNode(s, nl) =
  let base_uri         	   = ref (Some (new atomicAnyURI (AnyURI._kinda_uri_of_string s))) in
  let pv                   = Physical_value_util.physical_value_of_item_cursor (Physical_util._node_list nl) in
  let typed_xml_stream 	   = Physical_value_util.sax_value_of_physical_value  pv in
  let doc_typed_xml_stream = Streaming_constructors.document_constructor base_uri typed_xml_stream in
  let new_galax_node = 
    Physical_load.load_xml_value_from_typed_stream (load_context ()) doc_typed_xml_stream
  in
  (getNode (List.nth new_galax_node 0))#getDocumentNode()

let elementNode(q, al, nl, _) = (* Type is ignored !! - Jerome *)
  (* This name business is a mess -- element & attribute should take atomicQNames as arguments *)
  let enname = (q#getAtomicQName()) in
  let aln = List.map (fun x -> (x :> node)) al in
  let pv = Physical_value_util.physical_value_of_item_cursor (Physical_util._node_list (aln@nl)) in
  let typed_xml_stream 	   = Physical_value_util.sax_value_of_physical_value pv in
  (* Note: eventually, we may want to give the user the control over
     the namespace environment from the API. - Jerome *)
  let init_nsenv = Namespace_context.default_xml_nsenv in
  let baseuri = Dm_atomic_util.default_no_uri_dm in
  let elem_typed_xml_stream = Streaming_constructors.element_constructor baseuri enname init_nsenv typed_xml_stream in
  let new_galax_node = 
    Physical_load.load_xml_value_from_typed_stream (load_context ()) elem_typed_xml_stream
  in
  (getNode (List.nth new_galax_node 0))#getElementNode()

let attributeNode(q, s, _) = (* Type is ignored !! - Jerome *)
  (* This name business is a mess -- element & attribute should take atomicQNames as arguments *)
  let atname = (q#getAtomicQName()) in
  let pv = Physical_value_util.physical_value_of_item_cursor (Physical_util._atomic_value (s :> atomicValue)) in
  let typed_xml_stream 	    = Physical_value_util.sax_value_of_physical_value pv in
  let attr_typed_xml_stream = Streaming_constructors.attribute_constructor atname typed_xml_stream in
  let new_galax_node =
    Physical_load.load_xml_value_from_typed_stream (load_context ()) attr_typed_xml_stream
  in
  (getNode (List.nth new_galax_node 0))#getAttributeNode()

let textNode s =
  let pv = Physical_value_util.physical_value_of_item_cursor (Physical_util._atomic_value (s :> atomicValue)) in
  let typed_xml_stream 	   = Physical_value_util.sax_value_of_physical_value  pv in
  let text_typed_xml_stream = Streaming_constructors.text_constructor typed_xml_stream in
  let new_galax_node =
    Physical_load.load_xml_value_from_typed_stream (load_context ()) text_typed_xml_stream
  in
  (getNode (List.nth new_galax_node 0))#getTextNode()

let commentNode s =
  let pv = Physical_value_util.physical_value_of_item_cursor (Physical_util._atomic_value (s :> atomicValue)) in
  let typed_xml_stream 	   = Physical_value_util.sax_value_of_physical_value  pv in
  let comment_typed_xml_stream = Streaming_constructors.comment_constructor typed_xml_stream in
  let new_galax_node =
    Physical_load.load_xml_value_from_typed_stream (load_context ()) comment_typed_xml_stream
  in
  (getNode (List.nth new_galax_node 0))#getCommentNode()

let processingInstructionNode (s1,s2) =
  let pv = Physical_value_util.physical_value_of_item_cursor (Physical_util._atomic_value (s2 :> atomicValue)) in
  let typed_xml_stream 	   = Physical_value_util.sax_value_of_physical_value  pv in
  let pi_typed_xml_stream = Streaming_constructors.pi_constructor false (s1#getAtomicString()) typed_xml_stream in
  let new_galax_node =
    Physical_load.load_xml_value_from_typed_stream (load_context ()) pi_typed_xml_stream
  in
  (getNode (List.nth new_galax_node 0))#getProcessingInstructionNode()


(**********************)
(* Accessors on items *)
(**********************)

let string_value item =
  (string_value item)

let item_kind item =
  Physical_item_util.string_of_item_kind(item_kind item)

let get_node item =
  getNode item

let get_atomicValue item =
  getAtomicValue item

let get_element item =
  (getNode item)#getElementNode()

let get_attribute item =
  (getNode item)#getAttributeNode()


(**********************)
(* Accessors on nodes *)
(**********************)

let parent node =
  begin
    match node#parent None with
    | None -> []
    | Some p -> [p]
  end

let children node =
  Cursor.list_of_cursor "Dm_functions.children" (node#children None)

let base_uri node =
  begin
    match !(node#base_uri())
    with
    | None -> []
    | Some uri -> [uri]
  end

let node_kind node =
  Dm_util.string_of_node_kind(node#node_kind())

let node_name node =
  begin
    match (node#node_name()) with
    | None -> []
    | Some qname -> [qname]
  end

let typed_value node =
  Cursor.list_of_cursor "Dm_functions.typed_value" (node#typed_value())

let attributes node =
  Cursor.list_of_cursor "Dm_functions.attributes" (node#attributes None)

let namespace node =
  Cursor.list_of_cursor "Dm_functions.namespace" (node#namespaces())


(************************)
(* Conversion functions *)
(************************)

(* "Up-cast" functions *)

let to_node n = (n :> node)
let to_node_item n = Item_Node (to_node n)
let to_atomicValue av = (av :> atomicValue)
let to_atomicValue_item av  = Item_Atomic (to_atomicValue av)
let to_node_item_list nl  = List.map to_node_item nl
let to_atomicValue_item_list avl = List.map to_atomicValue_item avl

(* "Down cast" functions *)

let string_of_atomicValue  a =
  a#erase_atomic_value()

let boolean_of_atomicBoolean b =
  b#getAtomicBoolean()

let int_of_atomicInteger i =
  Decimal._int_of_integer (i#getAtomicInteger())

let integer_of_atomicInteger i =
  i#getAtomicInteger()

let decimal_of_atomicDecimal d =
  d#getAtomicDecimal()

let float_of_atomicFloat f =
  f#getAtomicFloat()

let float_of_atomicDouble d =
  d#getAtomicDouble()


