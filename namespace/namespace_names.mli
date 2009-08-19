(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_names.mli,v 1.9 2007/09/19 20:01:50 mff Exp $ *)

(* Module: Namespace_names
   Description:
     This module implements basic operations on XML names.
*)

(**********)
(* QNames *)
(**********)

(* Local names *)
type ncname = string

(* Namespace prefixes *)
type prefix =
  | NSDefaultElementPrefix
  | NSDefaultFunctionPrefix
  | NSWildcardPrefix
  | NSPrefix of ncname
  | NSInterfacePrefix of ncname (* A module interface prefix *) 
  | NSServerPrefix of ncname (* A server implementation prefix *)

(* Creates a new prefix *)
val new_prefix : string -> prefix

(* Prints a prefix *)
val string_of_prefix : prefix -> string

(* Namespace URIs *)
type uri =
  | NSUri of string
  | NSWildcardUri

(* String representation for URIs *)
val string_of_uri        : uri -> string
val quoted_string_of_uri : uri -> string
val curly_string_of_uri  : uri -> string

(*********************)
(* Unresolved QNames *)
(*********************)

type uqname = prefix * ncname

(* takes a string value containing a ':' and extract prefix and local name *)
val uqname_element_of_string          : string -> uqname
val uqname_element_of_raw_string      : string -> uqname
val uqname_function_of_string         : string -> uqname
val wildcard_uqname_of_string 	      : string -> uqname
val escaped_uqname_of_string  	      : string -> uqname

(* string value for an unresolved QName *)
val string_of_uqname : uqname -> string


(*******************)
(* Resolved QNames *)
(*******************)

type rqname = prefix * uri * ncname
      (* Now implements the "triple" proposal - Jerome 08/18/2004 *)

(* Resolved QNames equality *)
val rqname_compare   : rqname -> rqname -> int
val rqname_equal     : rqname -> rqname -> bool
val rqname_int_equal : rqname * int -> rqname * int -> bool

(* From a resolved QName back into an unresolved QName *)
val uqname_of_rqname        : rqname -> uqname

(* string representation for resolved QNames *)
val prefixed_string_of_rqname   : rqname -> string
val quoted_uri_string_of_rqname : rqname -> string
val curly_uri_string_of_rqname  : rqname -> string

(* Parses back a string representation for a QName *)
val parse_curly_uri_string      : string -> rqname


(* Hash functions *)

val prefix_hash : prefix -> int
val uri_hash    : uri -> int
val ncname_hash : ncname -> int

