(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: datatypes.mli,v 1.22 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Datatypes
   Description:
     This module provides Caml types that implement XQuery/XML Schema
     atomic values and atomic types.
 *)

open Decimal
open AnyURI
open DateTime


(*****************)
(* Atomic values *)
(*****************)

(* Note:
     Each of the following Caml types implements the *value space* of
     one the 19 XML Schema primitive atomic types.
   - Jerome
 *)

type xs_ncname       = string
type xs_string       = string
type xs_boolean      = bool
type xs_decimal      = Decimal._decimal
type xs_float        = float
type xs_double       = float
type xs_duration     = DateTime.xs_duration
type xs_dateTime     = DateTime.xs_dateTime
type xs_time         = DateTime.xs_time
type xs_date         = DateTime.xs_date
type xs_gYearMonth   = DateTime.xs_gYearMonth
type xs_gYear        = DateTime.xs_gYear
type xs_gMonthDay    = DateTime.xs_gMonthDay
type xs_gDay         = DateTime.xs_gDay
type xs_gMonth       = DateTime.xs_gMonth
type xs_hexBinary    = string
type xs_base64Binary = string
type xs_anyURI       = _uri                          (* Defined in the AnyUri module *)
type xs_QName        = Namespace_symbols.anon_symbol (* This is a *resolved* QName *)
type xs_NOTATION     = string

(* The two built-in XML Schema derived types treated as primitive
   types *)

type xs_integer      = Decimal._integer

(* 'xs' XQuery types *)

type xs_untyped           = string
type xs_yearMonthDuration = DateTime.xs_yearMonthDuration
type xs_dayTimeDuration   = DateTime.xs_dayTimeDuration


(****************)
(* Atomic types *)
(****************)

type atomic_type =
      (* 19 primitive XML Schema atomic types *)
  | ATString
  | ATBoolean
  | ATDecimal
  | ATFloat
  | ATDouble
  | ATDuration
  | ATDateTime
  | ATTime
  | ATDate
  | ATGYearMonth
  | ATGYear
  | ATGMonthDay
  | ATGDay
  | ATGMonth
  | ATHexBinary
  | ATBase64Binary
  | ATAnyURI
  | ATQName
  | ATNOTATION

      (* Derived atomic types *)
  | ATInteger

      (* xs data types *)
  | ATYearMonthDuration
  | ATDayTimeDuration
  | ATUntypedAtomic

      (* built-in Ur atomic type *)
  | ATAnyAtomic


(****************)
(* Atomic value *)
(****************)

