(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: datatypes_util.mli,v 1.26 2007/07/05 08:35:53 simeon Exp $ *)

(* Module Datatypes_util
   Description:
     This module contains basic operations on atomic values and atomic
     types, at the Caml level.
 *)

open Decimal
open AnyURI
open DateTime

open Datatypes


(*******************************)
(* Parsing from text to values *)
(*******************************)

val ncname_of_untyped       	   : xs_untyped -> xs_ncname
val string_of_untyped       	   : xs_untyped -> xs_string
val boolean_of_untyped      	   : xs_untyped -> xs_boolean
val decimal_of_untyped      	   : xs_untyped -> xs_decimal
val float_of_untyped        	   : xs_untyped -> xs_float
val double_of_untyped       	   : xs_untyped -> xs_double
val dateTime_of_untyped     	   : xs_untyped -> xs_dateTime
val time_of_untyped         	   : xs_untyped -> xs_time
val date_of_untyped         	   : xs_untyped -> xs_date
val gYearMonth_of_untyped   	   : xs_untyped -> xs_gYearMonth
val gYear_of_untyped        	   : xs_untyped -> xs_gYear
val gMonthDay_of_untyped    	   : xs_untyped -> xs_gMonthDay
val gDay_of_untyped         	   : xs_untyped -> xs_gDay
val gMonth_of_untyped       	   : xs_untyped -> xs_gMonth
val hexBinary_of_untyped    	   : xs_untyped -> xs_hexBinary
val base64Binary_of_untyped 	   : xs_untyped -> xs_base64Binary
val anyURI_of_untyped       	   : xs_untyped -> xs_anyURI
val qname_of_untyped        	   : Namespace_context.nsenv -> xs_untyped -> xs_QName
val notation_of_untyped     	   : xs_untyped -> xs_NOTATION
val integer_of_untyped      	   : Namespace_symbols.rtype_symbol -> xs_untyped -> xs_integer
val duration_of_untyped     	   : xs_untyped -> xs_duration
val yearMonthDuration_of_untyped   : xs_untyped -> xs_yearMonthDuration
val dayTimeDuration_of_untyped     : xs_untyped -> xs_dayTimeDuration

(***************)
(* Comparisons *)
(***************)

val string_equal : xs_string -> xs_string -> bool
val string_lteq  : xs_string -> xs_string -> bool
val string_lt    : xs_string -> xs_string -> bool
val string_gteq  : xs_string -> xs_string -> bool
val string_gt    : xs_string -> xs_string -> bool

val bool_equal : xs_boolean -> xs_boolean -> bool
val bool_lteq  : xs_boolean -> xs_boolean -> bool
val bool_lt    : xs_boolean -> xs_boolean -> bool
val bool_gteq  : xs_boolean -> xs_boolean -> bool
val bool_gt    : xs_boolean -> xs_boolean -> bool

val float_equal : xs_float -> xs_float -> bool
val float_lteq  : xs_float -> xs_float -> bool
val float_lt    : xs_float -> xs_float -> bool
val float_gteq  : xs_float -> xs_float -> bool
val float_gt    : xs_float -> xs_float -> bool

val double_equal : xs_double -> xs_double -> bool
val double_lteq  : xs_double -> xs_double -> bool
val double_lt    : xs_double -> xs_double -> bool
val double_gteq  : xs_double -> xs_double -> bool
val double_gt    : xs_double -> xs_double -> bool

val duration_equal : xs_duration -> xs_duration -> bool
val duration_lteq  : xs_duration -> xs_duration	-> bool
val duration_lt    : xs_duration -> xs_duration	-> bool
val duration_gteq  : xs_duration -> xs_duration	-> bool
val duration_gt    : xs_duration -> xs_duration	-> bool

val dateTime_equal : xs_dayTimeDuration option -> xs_dateTime -> xs_dateTime -> bool
val dateTime_lteq  : xs_dayTimeDuration option -> xs_dateTime -> xs_dateTime -> bool
val dateTime_lt    : xs_dayTimeDuration option -> xs_dateTime -> xs_dateTime -> bool
val dateTime_gteq  : xs_dayTimeDuration option -> xs_dateTime -> xs_dateTime -> bool
val dateTime_gt    : xs_dayTimeDuration option -> xs_dateTime -> xs_dateTime -> bool

val time_equal : xs_dayTimeDuration option -> xs_time -> xs_time -> bool
val time_lteq  : xs_dayTimeDuration option -> xs_time -> xs_time -> bool
val time_lt    : xs_dayTimeDuration option -> xs_time -> xs_time -> bool
val time_gteq  : xs_dayTimeDuration option -> xs_time -> xs_time -> bool
val time_gt    : xs_dayTimeDuration option -> xs_time -> xs_time -> bool

val date_equal : xs_dayTimeDuration option -> xs_date -> xs_date -> bool
val date_lteq  : xs_dayTimeDuration option -> xs_date -> xs_date -> bool
val date_lt    : xs_dayTimeDuration option -> xs_date -> xs_date -> bool
val date_gteq  : xs_dayTimeDuration option -> xs_date -> xs_date -> bool
val date_gt    : xs_dayTimeDuration option -> xs_date -> xs_date -> bool

val gYearMonth_equal : xs_dayTimeDuration option -> xs_gYearMonth -> xs_gYearMonth -> bool
val gYearMonth_lteq  : xs_dayTimeDuration option -> xs_gYearMonth -> xs_gYearMonth -> bool
val gYearMonth_lt    : xs_dayTimeDuration option -> xs_gYearMonth -> xs_gYearMonth -> bool
val gYearMonth_gteq  : xs_dayTimeDuration option -> xs_gYearMonth -> xs_gYearMonth -> bool
val gYearMonth_gt    : xs_dayTimeDuration option -> xs_gYearMonth -> xs_gYearMonth -> bool

val gYear_equal : xs_dayTimeDuration option -> xs_gYear	-> xs_gYear -> bool
val gYear_lteq  : xs_dayTimeDuration option -> xs_gYear	-> xs_gYear -> bool
val gYear_lt    : xs_dayTimeDuration option -> xs_gYear	-> xs_gYear -> bool
val gYear_gteq  : xs_dayTimeDuration option -> xs_gYear	-> xs_gYear -> bool
val gYear_gt    : xs_dayTimeDuration option -> xs_gYear	-> xs_gYear -> bool

val gMonthDay_equal : xs_dayTimeDuration option -> xs_gMonthDay -> xs_gMonthDay -> bool
val gMonthDay_lteq  : xs_dayTimeDuration option -> xs_gMonthDay -> xs_gMonthDay -> bool
val gMonthDay_lt    : xs_dayTimeDuration option -> xs_gMonthDay -> xs_gMonthDay -> bool
val gMonthDay_gteq  : xs_dayTimeDuration option -> xs_gMonthDay -> xs_gMonthDay -> bool
val gMonthDay_gt    : xs_dayTimeDuration option -> xs_gMonthDay -> xs_gMonthDay -> bool

val gMonth_equal : xs_dayTimeDuration option -> xs_gMonth -> xs_gMonth -> bool
val gMonth_lteq  : xs_dayTimeDuration option -> xs_gMonth -> xs_gMonth -> bool
val gMonth_lt    : xs_dayTimeDuration option -> xs_gMonth -> xs_gMonth -> bool
val gMonth_gteq  : xs_dayTimeDuration option -> xs_gMonth -> xs_gMonth -> bool
val gMonth_gt    : xs_dayTimeDuration option -> xs_gMonth -> xs_gMonth -> bool

val gDay_equal : xs_dayTimeDuration option -> xs_gDay -> xs_gDay -> bool
val gDay_lteq  : xs_dayTimeDuration option -> xs_gDay -> xs_gDay -> bool
val gDay_lt    : xs_dayTimeDuration option -> xs_gDay -> xs_gDay -> bool
val gDay_gteq  : xs_dayTimeDuration option -> xs_gDay -> xs_gDay -> bool
val gDay_gt    : xs_dayTimeDuration option -> xs_gDay -> xs_gDay -> bool

val hexBinary_equal    : xs_hexBinary    -> xs_hexBinary    -> bool
val hexBinary_lteq     : xs_hexBinary    -> xs_hexBinary    -> bool
val hexBinary_lt       : xs_hexBinary    -> xs_hexBinary    -> bool
val hexBinary_gteq     : xs_hexBinary    -> xs_hexBinary    -> bool
val hexBinary_gt       : xs_hexBinary    -> xs_hexBinary    -> bool

val base64Binary_equal : xs_base64Binary -> xs_base64Binary -> bool
val base64Binary_lteq  : xs_base64Binary -> xs_base64Binary -> bool
val base64Binary_lt    : xs_base64Binary -> xs_base64Binary -> bool
val base64Binary_gteq  : xs_base64Binary -> xs_base64Binary -> bool
val base64Binary_gt    : xs_base64Binary -> xs_base64Binary -> bool

val anyURI_equal       : xs_anyURI   	 -> xs_anyURI 	    -> bool

val qname_equal        : xs_QName    	 -> xs_QName  	    -> bool
val qname_lteq         : xs_QName     	 -> xs_QName        -> bool
val qname_lt           : xs_QName      	 -> xs_QName  	    -> bool
val qname_gteq         : xs_QName      	 -> xs_QName   	    -> bool
val qname_gt           : xs_QName      	 -> xs_QName   	    -> bool

val notation_equal     : xs_NOTATION     -> xs_NOTATION     -> bool
val notation_lteq      : xs_NOTATION     -> xs_NOTATION     -> bool
val notation_lt        : xs_NOTATION     -> xs_NOTATION     -> bool
val notation_gteq      : xs_NOTATION     -> xs_NOTATION     -> bool
val notation_gt        : xs_NOTATION     -> xs_NOTATION     -> bool


val yearMonthDuration_equal : xs_yearMonthDuration -> xs_yearMonthDuration -> bool
val yearMonthDuration_lteq  : xs_yearMonthDuration -> xs_yearMonthDuration -> bool
val yearMonthDuration_lt    : xs_yearMonthDuration -> xs_yearMonthDuration -> bool
val yearMonthDuration_gteq  : xs_yearMonthDuration -> xs_yearMonthDuration -> bool
val yearMonthDuration_gt    : xs_yearMonthDuration -> xs_yearMonthDuration -> bool


val dayTimeDuration_equal : xs_dayTimeDuration -> xs_dayTimeDuration -> bool
val dayTimeDuration_lteq  : xs_dayTimeDuration -> xs_dayTimeDuration -> bool
val dayTimeDuration_lt    : xs_dayTimeDuration -> xs_dayTimeDuration -> bool
val dayTimeDuration_gteq  : xs_dayTimeDuration -> xs_dayTimeDuration -> bool
val dayTimeDuration_gt    : xs_dayTimeDuration -> xs_dayTimeDuration -> bool

val untyped_equal : xs_untyped -> xs_untyped -> bool
val untyped_lteq  : xs_untyped -> xs_untyped -> bool
val untyped_lt    : xs_untyped -> xs_untyped -> bool
val untyped_gteq  : xs_untyped -> xs_untyped -> bool
val untyped_gt    : xs_untyped -> xs_untyped -> bool


(*******************************)
(* Operations on atomic values *)
(*******************************)

val serialize_float        : xs_float  -> string
val serialize_double       : xs_double -> string
val serialize_base64Binary : xs_base64Binary -> string
val serialize_hexBinary    : xs_hexBinary -> string

(******************************)
(* Operations on atomic types *)
(******************************)

val atomic_is_numeric        : atomic_type -> bool
val atomic_is_anyURI         : atomic_type -> bool
val atomic_is_anystring      : atomic_type -> bool
val atomic_type_subsumes     : atomic_type -> atomic_type -> bool
val untyped_atomic_type      : atomic_type
val lookup_bltin_type        : Namespace_symbols.rtype_symbol -> atomic_type
val symbol_of_primitive_type : atomic_type -> Namespace_symbols.rtype_symbol
val unit_symbol_of_base_type : atomic_type -> Namespace_symbols.rtype_symbol
val can_be_promoted_to       : atomic_type -> atomic_type list
val bt_can_be_promoted_to    : atomic_type -> atomic_type -> (bool * bool)
val string_of_atomic_type : Datatypes.atomic_type -> string
val compare_types : Datatypes.atomic_type -> Datatypes.atomic_type -> int
val base64_of_hex : xs_base64Binary -> xs_hexBinary
val hex_of_base64 : xs_hexBinary -> xs_base64Binary

