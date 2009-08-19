(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: encoding.mli,v 1.6 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Encoding
   Description:
     Configuration of character encoding operations.
*)

(* Character encoding *)

val utf8_string : string

type encoding = Pxp_types.encoding
      (* All possible external encodings *)

type rep_encoding = Pxp_types.rep_encoding
      (* All possible internal encodings *)

val encoding_of_string : string -> encoding
val string_of_encoding : encoding -> string

val encoding_of_rep_encoding : rep_encoding -> encoding
val rep_encoding_of_encoding : encoding -> rep_encoding

(* Character encoding *)

val set_internal_encoding 	: rep_encoding -> unit
val get_internal_encoding 	: unit -> rep_encoding

val set_output_encoding         : encoding -> unit
val set_default_output_encoding : unit -> unit
val get_output_encoding         : unit -> encoding

(* String / character conversions *)

val write_data_string   : rep_encoding -> encoding -> string -> string
val write_markup_string : rep_encoding -> encoding -> string -> string

val character           : rep_encoding -> int -> string


