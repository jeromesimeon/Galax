(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm_atomic_util.ml,v 1.5 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Dm_atomic_util
   Description:
     This module implements a number of short-cut functions to
     construct and access atomic values in the XQuery 1.0 and XPath
     2.0 data model.
*)

open Error

open Datatypes
open Dm_atomic


(***********)
(* Erasure *)
(***********)

(* Erasure of simple values *)

let erase_simple_value vl =
  String.concat " " (List.map (fun v -> (v#erase_atomic_value())) vl)

let string_of_atomic_value av =
  match av#getAtomicValueKind() with
  | ATString   	   	-> "\"" ^ (av#getAtomicString()) ^ "\""
  | ATBoolean  	   	-> string_of_bool (av#getAtomicBoolean())
  | ATDecimal  	   	-> Decimal._string_of_decimal (av#getAtomicDecimal())
  | ATFloat    	   	-> string_of_float (av#getAtomicFloat())
  | ATDouble   	   	-> string_of_float (av#getAtomicDouble())
  | ATDuration 	   	->  "\"" ^ (av#getAtomicDuration()) ^ "\""
  | ATDateTime 	   	->  "\"" ^ (DateTime.string_of_dateTime (av#getAtomicDateTime())) ^ "\""
  | ATTime     	   	->  "\"" ^ (DateTime.string_of_time (av#getAtomicTime())) ^ "\""
  | ATDate     	   	->  "\"" ^ (DateTime.string_of_date (av#getAtomicDate())) ^ "\""
  | ATGYearMonth   	->  "\"" ^ (av#getAtomicGYearMonth())^ "\""
  | ATGYear        	->  "\"" ^ (av#getAtomicGYear())^ "\""
  | ATGMonthDay    	->  "\"" ^ (av#getAtomicGMonthDay())^ "\""
  | ATGDay   	   	->  "\"" ^ (av#getAtomicGDay())^ "\""
  | ATGMonth 	   	->  "\"" ^ (av#getAtomicGMonth())^ "\""
  | ATHexBinary    	->  "\"" ^ (av#getAtomicHexBinary())^ "\""
  | ATBase64Binary 	->  "\"" ^ (av#getAtomicBase64Binary())^ "\""
  | ATAnyURI   	   	-> AnyURI._string_of_uri (av#getAtomicAnyURI())
  | ATQName    	   	-> "\"" ^ (Namespace_symbols.anon_prefix_string (av#getAtomicQName())) ^ "\""
  | ATNOTATION 	   	-> "\"" ^ (av#getAtomicNotation()) ^ "\""
  | ATInteger  	   	-> Decimal._string_of_integer (av#getAtomicInteger())
  | ATYearMonthDuration -> "\"" ^ (DateTime.string_of_yearMonthDuration (av#getAtomicYearMonthDuration())) ^ "\""
  | ATDayTimeDuration   -> "\"" ^ (DateTime.string_of_dayTimeDuration (av#getAtomicDayTimeDuration())) ^ "\""
  | ATUntypedAtomic     -> "\"" ^ (av#getAtomicUntyped()) ^ "\""
  | ATAnyAtomic         -> raise (Query(Malformed_Type("Dynamic type of value is xs:anyAtomicType")))


(* Normalization stuff *)

let integer_one = ((new atomicInteger Decimal._integer_one) :> atomicValue)

(************************************)
(* Total Ordering over atomic types *)
(************************************)

let total_order_compare v v' =
  let cmp = Datatypes_util.compare_types (v#getAtomicValueKind ())
             (v'#getAtomicValueKind ()) in
    if cmp = 0 then
      v#atomic_value_compare v'
    else cmp

let uri_dm_of_uri uriopt =
  let dbu =
    match uriopt with
    | None -> None
    | Some uri -> Some (new Dm_atomic.atomicAnyURI uri)
  in
  ref dbu

let default_no_uri_dm =
  uri_dm_of_uri (AnyURI.default_base_uri ())

