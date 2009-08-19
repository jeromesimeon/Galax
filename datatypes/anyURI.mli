(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: anyURI.mli,v 1.10 2007/02/01 22:08:46 simeon Exp $ *)

(* Module AnyURI
   Description:
     This module provides support for the anyURI atomic type.
 *)

type _uri

val default_base_uri       : unit -> _uri option
val default_galax_base_uri : unit -> _uri

val default_collation_uri  : unit -> _uri

val _kinda_uri_of_string : string -> _uri
val _actual_uri_of_string  : string -> _uri

val _string_of_uri : _uri -> string

val _uri_resolve : _uri -> _uri -> _uri

val _uri_eq  : _uri -> _uri -> bool


val encode_string_for_uri : string -> string
val encode_iri_to_uri     : string -> string
val encode_html_uri       : string -> string

