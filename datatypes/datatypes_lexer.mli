(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module: Datatypes_lexer
   Description:
     This module is the interface for the datatype lexer.
 *)

val parse_ncname: Lexing.lexbuf -> Datatypes.xs_string
val parse_boolean: Lexing.lexbuf -> Datatypes.xs_boolean
val parse_decimal: Lexing.lexbuf -> Datatypes.xs_decimal
val parse_float: Lexing.lexbuf -> Datatypes.xs_float
val parse_double: Lexing.lexbuf -> Datatypes.xs_double
val parse_yearMonthDuration: Lexing.lexbuf -> Datatypes.xs_yearMonthDuration
val parse_dayTimeDuration: Lexing.lexbuf -> Datatypes.xs_dayTimeDuration
val parse_dateTime: Lexing.lexbuf -> Datatypes.xs_dateTime
val parse_time: Lexing.lexbuf -> Datatypes.xs_time
val parse_date: Lexing.lexbuf -> Datatypes.xs_date
val parse_gYearMonth: Lexing.lexbuf -> Datatypes.xs_gYearMonth
val parse_gYear: Lexing.lexbuf -> Datatypes.xs_gYear
val parse_gMonthDay: Lexing.lexbuf -> Datatypes.xs_gMonthDay
val parse_gDay: Lexing.lexbuf -> Datatypes.xs_gDay
val parse_gMonth: Lexing.lexbuf -> Datatypes.xs_gMonth
val parse_integer: Lexing.lexbuf -> Datatypes.xs_integer
