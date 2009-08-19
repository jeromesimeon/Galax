(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_value.mli,v 1.11 2007/02/01 22:08:51 simeon Exp $ *)


(* Module: Physical_value
   Description:
     This modules defines physical representations of XML values.
*)

(*************)
(* Sequences *)
(*************)

type 'a sequence =
  | CSeq of 'a Cursor.cursor
  | LSeq of 'a list
      (* Data model sequences are represented as a Caml stream with a
         possible buffer. *)


(******************)
(* Physical items *)
(******************)

type item =
  | Item_Atomic of Dm_atomic.atomicValue
  | Item_Node of Dm.node

(* There are 2 kinds of items *)

type _ItemKind = 
  | AtomicValueKind
  | NodeKind

(***********************)
(* Physical XML values *)
(***********************)

type dom_value = item sequence
type sax_value = Streaming_types.typed_xml_stream

type xml_value =
  | DomValue of dom_value
  | SaxValue of sax_value

(* Those are the tuples really accessed within the code *)

type tuple_unit = unit
val empty_tuple : tuple_unit

type tuple     = xml_value array
type dom_tuple = dom_value array

type table_unit = tuple_unit Cursor.cursor
type table = tuple array
type dom_table = dom_tuple array

type physical_value =
  | PXMLValue of xml_value                     (* XML value *)
  | PTable of table_unit                       (* Table *)


