(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_item_util.mli,v 1.7 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_item_util
   Description:
     This module implements utility functions on the XQuery 1.0 and
     XPath 2.0 data model.
*)

open Datatypes

open Namespace_symbols

open Physical_value


(*************************)
(* Testing the node kind *)
(*************************)

val isNode     	 	        : item -> bool
val isAtomicValue  		: item -> bool


(*****************)
(* Some printing *)
(*****************)

val string_of_item_kind 	: _ItemKind -> string


(*********************)
(* Item constructors *)
(*********************)

(* Constants *)

val _true    : item
val _false   : item

(* Atomic values *)

val _integer : xs_integer -> item
val _decimal : xs_decimal -> item
val _boolean : xs_boolean -> item
val _float   : xs_float   -> item
val _double  : xs_double  -> item
val _string  : xs_string  -> item
val _NCName  : xs_string  -> item
val _QName   : xs_QName   -> item
val _anyURI  : xs_anyURI  -> item
val _untyped : xs_untyped -> item
val _date    : xs_date     -> item
val _time    : xs_time     -> item
val _dateTime : xs_dateTime     -> item
val _yearMonthDuration : xs_yearMonthDuration  -> item
val _dayTimeDuration : xs_dayTimeDuration      -> item


