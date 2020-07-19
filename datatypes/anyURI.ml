(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: anyURI.ml,v 1.18 2007/02/01 22:08:46 simeon Exp $ *)

(* Module AnyURI
   Description:
     This module provides support for the anyURI atomic type.
 *)

open Neturl

open Error

(* URI's are implemented based on the Netstring package *)

type _uri = url

(* URI syntax used *)

let url_syntax = ip_url_syntax

(* Resolves an absolute URI with a given URI scheme *)

let process_uri uri_string =
  try
    let uri_string = Whitespace.remove_whitespace uri_string in
    let uri_string = Whitespace.whitespace_id_normalize uri_string in
    url_of_string url_syntax uri_string 
  with
  | _ ->
      raise (Query (URI_Error ("Malformed URI " ^ uri_string))) 

let parse_uri uri_string =
  try
    let uri_string = Whitespace.remove_whitespace uri_string in
    let uri_string = Whitespace.whitespace_id_normalize uri_string in
    parse_url uri_string 
  with
  | _ ->
      raise (Query (URI_Error ("Malformed URI " ^ uri_string))) 

(* Default base URI and URI scheme *)

let is_absolute_uri uri =
  ensure_absolute_url uri

let _uri_is_absolute uri =
  try
    ignore(is_absolute_uri uri);
    true
  with
  | Malformed_URL -> false

let default_scheme = "file"

let default_base_uri () = None
let default_galax_base_uri () =
  try
    let default_base_uri_lead = default_scheme ^ "://" in
    let default_base_uri_string = (Gmisc.rename_dir (Filename.concat (default_base_uri_lead) (Filename.concat (Sys.getcwd ()) ""))) in
    process_uri default_base_uri_string
  with
  | _ ->
      process_uri ""

let base_uri_scheme =
  ref default_scheme

let base_uri_content =
  ref (default_base_uri ())

let default_collation_uri () =
  process_uri Conf.collns

let _kinda_uri_of_string uri_string =
  process_uri uri_string

let _actual_uri_of_string uri_string =
  parse_uri uri_string

let _string_of_uri uri =
  string_of_url uri

let _uri_resolve base_uri relative_uri =
  ensure_absolute_url
    (apply_relative_url base_uri relative_uri)

let _uri_resolve_opt bu ru =
  match bu with
  | None -> ru
  | Some bu0 -> _uri_resolve bu0 ru

  (* Equality based on string values for now ? - Jerome *)
let _uri_eq uri1 uri2 =
  string_of_url uri1 = string_of_url uri2


(* from I/O *)

let _uri_of_io base_uri gio =
  match gio with
  | Galax_io.File_Input fname ->
      let relative_uri = _kinda_uri_of_string fname in
      let absolute_uri = _uri_resolve base_uri relative_uri in
      Some absolute_uri
  | Galax_io.String_Input _ ->
      None
  | Galax_io.Buffer_Input _ ->
      None
  | Galax_io.Http_Input uri ->
      let relative_uri = _kinda_uri_of_string uri in
      let absolute_uri = _uri_resolve base_uri relative_uri in
      Some absolute_uri
  | Galax_io.Channel_Input uri ->
      None

(* Encode URIs *)

let hex_digits =
  [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
     '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' |]

let to_hex2 k =
  (* Converts k to a 2-digit hex string *)
  let s = Bytes.create 2 in
  Bytes.set s 0 hex_digits.( (k lsr 4) land 15 );
  Bytes.set s 1 hex_digits.( k land 15 );
  Bytes.to_string s

(*

  From F&O  7.4.10 fn:encode-for-uri:

  All characters are escaped except those identified as "unreserved"
  by [RFC 3986], that is 
   the upper- and lower-case letters A-Z, 
   the digits 0-9, 
   HYPHEN-MINUS ("-"), 
   LOW LINE ("_"), 
   FULL STOP ".", and
   TILDE "~".

*)

let uri_encoding_re =
  Netstring_pcre.regexp "[^A-Za-z0-9_.~-]"
(*  Netstring_pcre.regexp "[^A-Za-z0-9#_.!~*'()-]" *)

(*

  All characters are escaped other than the lower case letters a-z,
  the upper case letters A-Z, the digits 0-9, the NUMBER SIGN "#" and
  HYPHEN-MINUS ("-"), LOW LINE ("_"), FULL STOP ".", EXCLAMATION MARK
  "!", TILDE "~", ASTERISK "*", APOSTROPHE "'", LEFT PARENTHESIS "(",
  and RIGHT PARENTHESIS ")", SEMICOLON ";", SOLIDUS "/", QUESTION MARK
  "?", COLON ":", COMMERCIAL AT "@", AMPERSAND "&", EQUALS SIGN "=",
  PLUS SIGN "+", DOLLAR SIGN "$", COMMA ",", LEFT SQUARE BRACKET "[",
  RIGHT SQUARE BRACKET "]", and the PERCENT SIGN "%".

*)

let iri_encoding_re =
  Netstring_pcre.regexp "[^A-Za-z0-9#_.!~*'();////?:@&=//+//$,\\[\\]%-]"

let html_encoding_re =
  Netstring_pcre.regexp "[^\032-\126]"

let encode_string re s =
  Netstring_pcre.global_substitute
    re
    (fun r _ ->
      let x = Netstring_pcre.matched_string r s in
      let k = Char.code(x.[0]) in
      "%" ^ to_hex2 k
    )
    s

let encode_string_for_uri s = encode_string uri_encoding_re s
let encode_iri_to_uri s     = encode_string iri_encoding_re s
let encode_html_uri s       = encode_string html_encoding_re s

