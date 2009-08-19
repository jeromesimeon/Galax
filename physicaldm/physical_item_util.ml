(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_item_util.ml,v 1.6 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_item_util
   Description:
     This module implements utility functions on the XQuery 1.0 and
     XPath 2.0 data model.
*)

open Error

open Namespace_names
open Namespace_builtin
open Namespace_symbols

open Datatypes

open Dm_types
open Dm_atomic

open Physical_value
open Physical_item


(*************************)
(* Testing the node kind *)
(*************************)

let isNode i          		  = item_kind i = NodeKind
let isAtomicValue i   		  = item_kind i = AtomicValueKind


(*****************)
(* Some printing *)
(*****************)

let string_of_item_kind k = 
  match k with
  | AtomicValueKind -> "atomicValue"
  | NodeKind -> "node"



(*********************)
(* Item constructors *)
(*********************)

(* Constants *)

let _true      = Item_Atomic (new atomicBoolean(true))
let _false     = Item_Atomic (new atomicBoolean(false))

(* Atomic values *)

let _integer i = Item_Atomic (new atomicInteger(i))
let _decimal i = Item_Atomic (new atomicDecimal(i))
let _boolean b = Item_Atomic (new atomicBoolean(b))
let _float i   = Item_Atomic (new atomicFloat(i))
let _double i  = Item_Atomic (new atomicDouble(i))
let _string s  = Item_Atomic (new atomicString(s))
let _NCName s  = Item_Atomic (new atomicString ~ta:Namespace_symbols_builtin.xs_NCName s)
let _untyped s = Item_Atomic (new atomicUntyped(s))
let _QName q   = Item_Atomic (new atomicQName(q))
let _anyURI u  = Item_Atomic (new atomicAnyURI(u))
let _date d    = Item_Atomic (new atomicDate(d))
let _time t    = Item_Atomic (new atomicTime(t))
let _dateTime dt  = Item_Atomic (new atomicDateTime(dt))
let _yearMonthDuration ymd  = Item_Atomic (new atomicYearMonthDuration(ymd))
let _dayTimeDuration dtd    = Item_Atomic (new atomicDayTimeDuration(dtd))


