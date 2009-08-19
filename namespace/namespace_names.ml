(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_names.ml,v 1.13 2007/10/25 00:08:42 mff Exp $ *)

(* Module: Namespace_names
   Description:
     This module implements basic operations on XML names.
*)

open Error


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
let prefix_counter = Id.create 0

let new_prefix nc =
  (NSPrefix (nc ^ "_newprefix_" ^ (string_of_int (Id.next prefix_counter))))

(* Prints a prefix *)
let string_of_prefix pr =
  match pr with
  | (NSDefaultElementPrefix) -> ""
  | (NSDefaultFunctionPrefix) -> ""
(* (Error.eprintf_warning "No string representation for Default Function Namespace prefix";"") *)
(* raise (Query (URI_Error ("No string representation for Default Function Namespace prefix" ))) *)
  | (NSWildcardPrefix) -> "*"
  | (NSPrefix ns) -> ns
  | (NSInterfacePrefix ns) -> "(:int:)"^ns
  | (NSServerPrefix ns) -> "(:server:)"^ns

(* Namespace URIs *)
type uri =
  | NSUri of string
  | NSWildcardUri

(* String representation for URIs *)
let string_of_uri uri =
  match uri with
  | NSWildcardUri -> "*"
(*      raise (Query (Namespace_Internal "Casting the internal wildcard uri to a string")) *)
  | NSUri uri ->
      uri

let quoted_string_of_uri uri =
  match uri with
  | NSWildcardUri ->
      "\"*\""
  | NSUri uri ->
      "\"" ^ uri ^ "\""

let curly_string_of_uri uri =
  match uri with
  | NSWildcardUri ->
      "{*}"
  | NSUri uri ->
      "{" ^ uri ^ "}"


(*********************)
(* Unresolved QNames *)
(*********************)

type uqname = prefix * ncname

(* takes a string value containing a ':' and extract prefix and local name *)

let parse_qname s =
  match Gmisc.split_on_char s ':' with
  | [] -> raise (Query (Namespace_Internal "Invalid qualified name"))
  | s :: [] -> (None,s)
  | s1 :: s2 :: [] -> (Some s1,s2)
  | _ -> raise (Query (Namespace_Internal "Invalid qualified name"))

let uqname_element_of_string s =
  match (parse_qname s) with
  | (None,s2) ->
      (NSDefaultElementPrefix,s2)
  | (Some s1,s2) ->
      (NSPrefix s1,s2)

let uqname_element_of_raw_string s =
  match (Gmisc.wrap_lexer Qname_lexer.parse_qname s) with
  | (None,s2) ->
      (NSDefaultElementPrefix,s2)
  | (Some s1,s2) ->
      (NSPrefix s1,s2)

let uqname_function_of_string s =
  match (parse_qname s) with
  | (None,s2) ->
      (NSDefaultFunctionPrefix,s2)
  | (Some s1,s2) ->
      (NSPrefix s1,s2)

(* takes a string value containing a ':' and extract prefix and local name *)

let wildcard_uqname_of_string s =
  match (parse_qname s) with
  | (None,s2) ->
      (NSDefaultElementPrefix,s2)
  | (Some "*", s2) ->
      (NSWildcardPrefix, s2)
  | (Some s1, s2) ->
      (NSPrefix s1,s2)

(* takes a string value containing a ':' and extract prefix and local name *)

let escaped_uqname_of_string s =
  if (String.get s 0) = ':'
  then
    uqname_element_of_string (String.sub s 1 ((String.length s)-1))
  else
    uqname_element_of_string s

(* string value for an unresolved QName *)

let string_of_uqname qn =
  match qn with
  | (NSDefaultElementPrefix, nc) -> nc
  | (NSDefaultFunctionPrefix, nc) -> nc
  | (NSWildcardPrefix, "*") -> "*"
  | (NSWildcardPrefix, nc) -> "*:" ^ nc
  | (NSPrefix ns, nc) -> if (ns = "") then nc else ns ^ ":" ^ nc
  | (NSInterfacePrefix ns, nc) -> "(:int:)"^ ns ^ ":" ^ nc
  | (NSServerPrefix ns, nc) -> "(:server:)"^ ns ^ ":" ^ nc

(*******************)
(* Resolved QNames *)
(*******************)

type rqname = prefix * uri * ncname
      (* Now implements the "triple" proposal - Jerome 08/18/2004 *)

(* Resolved QNames equality *)
(* When comparing two resolved _server_ QNames, the prefix is signifcant. *)
let rqname_compare (prefix1,uri1,ncname1) (prefix2,uri2,ncname2) =
  compare (uri1,ncname1) (uri2,ncname2)

let rqname_equal (prefix1,uri1,ncname1) (prefix2,uri2,ncname2) =
  match prefix1, prefix2 with 
  | NSInterfacePrefix _, NSInterfacePrefix _ 
  | (NSPrefix _ | NSDefaultElementPrefix | NSDefaultFunctionPrefix | NSWildcardPrefix), 
    (NSPrefix _ | NSDefaultElementPrefix | NSDefaultFunctionPrefix | NSWildcardPrefix) ->
      (uri1 = uri2) && (ncname1 = ncname2)
  | NSServerPrefix p1, NSServerPrefix p2 when (p1 = p2) ->
      (uri1 = uri2) && (ncname1 = ncname2)
  | _ -> false

let rqname_int_equal ((prefix1,uri1,ncname1),int1) ((prefix2,uri2,ncname2),int2) =
  match prefix1, prefix2 with 
  | NSInterfacePrefix _, NSInterfacePrefix _
  | (NSPrefix _ | NSDefaultElementPrefix | NSDefaultFunctionPrefix | NSWildcardPrefix), 
    (NSPrefix _ | NSDefaultElementPrefix | NSDefaultFunctionPrefix | NSWildcardPrefix) ->
      (uri1 = uri2) && (ncname1 = ncname2) && (int1 = int2)
  | NSServerPrefix p1, NSServerPrefix p2 when (p1 = p2) ->
      (uri1 = uri2) && (ncname1 = ncname2) && (int1 = int2)
  | _ -> false

(* From a resolved QName back into an unresolved QName *)

let uqname_of_rqname qn =
  match qn with
  | (prefix,uri,nc) ->
      (prefix,nc)

(* string value for a resolved qname *)

let prefixed_string_of_rqname qn =
  string_of_uqname (uqname_of_rqname qn)

let quoted_uri_string_of_rqname qn =
  let (prefix,uri,nc) = qn in
  (quoted_string_of_uri uri) ^ ":" ^ nc

(* I've extended the "curly" syntax of qnames to include the prefix type for servers: 
     [server-prefix]{uri}nc for server prefixes
   Otherwise:
     {uri}nc
*)
let square_string_of_prefix prefix = 
  match prefix with
  | NSServerPrefix p -> "[" ^ p ^ "]"
  | _ -> ""

let curly_uri_string_of_rqname qn =
  let (prefix,uri,nc) = qn in
  (square_string_of_prefix prefix)^(curly_string_of_uri uri) ^ nc

let parse_square_prefix_string s =
  let (s1,s2) = Gmisc.split_left_on_char s '[' in
  if (s1 = "") then 
    try
      let (p, rest) = Gmisc.split_left_on_char s2 ']' in
      if (p = "") then 
	raise (Query (Namespace_Internal ("Cannot re-parse string : \"" ^ s ^ "\" as a RQNAME")))
      else (NSServerPrefix p, rest)
    with
    | _ ->
	raise (Query (Namespace_Internal ("Cannot re-parse string : \"" ^ s ^ "\" as a RQNAME")))
  else (NSPrefix "unknown", s)

let parse_curly_uri_string s =
(* Debug.print_default_debug ("Parse :"^s^"\n"); *)
  let (p,s) = parse_square_prefix_string s in 
  let (s1,s2) =
    try
      Gmisc.split_left_on_char s '{'
    with
    | _ ->
	raise (Query (Namespace_Internal ("Cannot re-parse string : \"" ^ s ^ "\" as a RQNAME")))
  in
  match s1 with
  | "" ->
      begin
	let (uri,lname) =
	  try
	    Gmisc.split_left_on_char s2 '}'
	  with
	  | _ ->
	      raise (Query (Namespace_Internal ("Cannot re-parse string : \"" ^ s ^ "\" as a RQNAME")))
	in
(* Debug.print_default_debug ("Parsed as :"^uri^":"^lname^"\n"); *)
	match uri with
	| "*" ->
	    (NSWildcardPrefix,NSWildcardUri, lname)
	| "" ->
	    (NSDefaultElementPrefix, NSUri uri, lname)
	| _ ->
	    (p, NSUri uri, lname)
      end
  | _ ->
      raise (Query (Namespace_Internal ("Cannot re-parse string : \"" ^ s ^ "\" as a RQNAME")))


(* Hash functions *)

let prefix_hash prefix =
  match prefix with
  | NSDefaultElementPrefix -> 1
  | NSDefaultFunctionPrefix -> 2
  | NSWildcardPrefix -> 3
  | NSPrefix ncname -> Gmisc.string_hash ncname
  | NSInterfacePrefix ncname -> Gmisc.string_hash ("I"^ncname)
  | NSServerPrefix ncname -> Gmisc.string_hash ("S"^ncname)

let uri_hash    uri    =
  match uri with
  | NSUri u -> Gmisc.string_hash u
  | NSWildcardUri -> 1

let ncname_hash ncname = Gmisc.string_hash ncname

