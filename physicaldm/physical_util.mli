(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_util.mli,v 1.22 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_util
   Description:
     This module supports utility functions on the abstract
     implementation of XQuery 1.0 and XPath 2.0 data model sequence.

*)

open Namespace_names
open Datatypes
open Dm_atomic

open Physical_value

(* Some useful value accessors *)

val get_string 	       	  : item Cursor.cursor -> xs_string
val get_integer	       	  : item Cursor.cursor -> xs_integer
val get_decimal	       	  : item Cursor.cursor -> xs_decimal
val get_float	       	  : item Cursor.cursor -> xs_float
val get_double	       	  : item Cursor.cursor -> xs_double
val get_anyURI         	  : item Cursor.cursor -> xs_anyURI
val get_QName          	  : item Cursor.cursor -> xs_QName
val get_boolean        	  : item Cursor.cursor -> xs_boolean
val get_date           	  : item Cursor.cursor -> xs_date
val get_gYearMonth     	  : item Cursor.cursor -> xs_gYearMonth
val get_gYear     	  : item Cursor.cursor -> xs_gYear
val get_gMonthDay     	  : item Cursor.cursor -> xs_gMonthDay
val get_gMonth     	  : item Cursor.cursor -> xs_gMonth
val get_gDay     	  : item Cursor.cursor -> xs_gDay
val get_time           	  : item Cursor.cursor -> xs_time
val get_dateTime       	  : item Cursor.cursor -> xs_dateTime
val get_yearMonthDuration : item Cursor.cursor -> xs_yearMonthDuration
val get_ymd_from_duration : item Cursor.cursor -> xs_yearMonthDuration
val get_dayTimeDuration   : item Cursor.cursor -> xs_dayTimeDuration
val get_dtd_from_duration : item Cursor.cursor -> xs_dayTimeDuration
val get_duration          : item Cursor.cursor -> xs_duration
val get_hexBinary         : item Cursor.cursor -> xs_hexBinary
val get_base64Binary      : item Cursor.cursor -> xs_base64Binary

val get_string_cursor  	  : item Cursor.cursor -> xs_string Cursor.cursor
val get_integer_cursor 	  : item Cursor.cursor -> xs_integer Cursor.cursor
val get_decimal_cursor 	  : item Cursor.cursor -> xs_decimal Cursor.cursor
val get_float_cursor   	  : item Cursor.cursor -> xs_float Cursor.cursor
val get_double_cursor  	  : item Cursor.cursor -> xs_double Cursor.cursor
val get_dayTimeDuration_cursor   : item Cursor.cursor -> xs_dayTimeDuration Cursor.cursor
val get_yearMonthDuration_cursor : item Cursor.cursor -> xs_yearMonthDuration Cursor.cursor
val get_date_cursor       : item Cursor.cursor -> xs_date Cursor.cursor
val get_time_cursor       : item Cursor.cursor -> xs_time Cursor.cursor
val get_dateTime_cursor   : item Cursor.cursor -> xs_dateTime Cursor.cursor


val get_node   :  item -> Dm.node
val get_atomic :  item -> atomicValue

val get_singleton_node   : item Cursor.cursor -> Dm.node
val get_singleton_atomic : item Cursor.cursor -> atomicValue
val get_item             : item Cursor.cursor -> item
val get_optional_item    : item Cursor.cursor -> item option
val get_optional_atomic  : item Cursor.cursor -> atomicValue option
val get_optional_string  : item Cursor.cursor -> xs_string option
val get_optional_double  : item Cursor.cursor -> xs_double option
val get_optional_float   : item Cursor.cursor -> xs_float option
val get_optional_decimal : item Cursor.cursor -> xs_decimal option
val get_optional_integer : item Cursor.cursor -> xs_integer option
val get_optional_date    : item Cursor.cursor -> xs_date option
val get_optional_time    : item Cursor.cursor -> xs_time option

val get_atomic_cursor    : item Cursor.cursor -> atomicValue Cursor.cursor
val get_node_cursor      : item Cursor.cursor -> Dm.node Cursor.cursor

val get_node_list_of_item_list   : item list -> Dm.node list
val get_atomic_list_of_item_list : item list -> atomicValue list

val _integer_cursor : xs_integer Cursor.cursor -> item Cursor.cursor

val _node_cursor    : Dm.node Cursor.cursor        -> item Cursor.cursor
val _node_list      : Dm.node list                 -> item Cursor.cursor

val _atomic_cursor  : atomicValue Cursor.cursor -> item Cursor.cursor
val _atomic_list    : atomicValue list          -> item Cursor.cursor
val _atomic_value   : atomicValue               -> item Cursor.cursor

