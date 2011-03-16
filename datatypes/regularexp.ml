(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: regularexp.ml,v 1.6 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Regularexp
   Description:
     This module implements regular expressions on strings in F&O functions.
*)

open Error
open Pcre
let rec getflags flags index =
  let len = String.length flags in
  if index = len then [] 
  else 
    let curflag =
      let curchar = String.get flags index in
      match curchar with
      |	's' -> `DOTALL 
      |	'm' -> `MULTILINE
      |	'i' -> `CASELESS
      |	'x' -> `EXTENDED 
      |	_ -> raise (Query (Wrong_Args ("Invalid flag. Only valid characters are s, m, i, and x"))) in
    curflag :: (getflags flags (index + 1))

let make_regex flaglist pattern =
  try
    Pcre.regexp ~iflags:flaglist pattern
  with
  | _ ->
      raise (Query (Wrong_Args ("Invalid regular expression: \"" ^pattern^ "\"")))
  

let matches input pattern flags =
  let flaglist = Pcre.cflags (getflags flags 0) in
  let regex = make_regex flaglist pattern in
  let res = Pcre.pmatch ~rex:regex input in
  res

let check1 = Pcre.regexp "\\$[^0-9]"
let check2 = Pcre.regexp "\\$$"
let check3 = Pcre.regexp "\\\\[^\\\\]"
let check4 = Pcre.regexp "\\\\$"
let checkrep t =
  if (Pcre.pmatch ~rex:check1 t) || (Pcre.pmatch ~rex:check2 t)
  then
    raise (Query (Wrong_Args "'$' character not followed by a digit [0-9] in replacement text"));
  if (Pcre.pmatch ~rex:check3 t) || (Pcre.pmatch ~rex:check4 t)
  then
    raise (Query (Wrong_Args "'\\' character not written as '\\\\'"))

let replace input pattern replacement flags =
  let flaglist = Pcre.cflags (getflags flags 0) in
  let regex = make_regex flaglist pattern in
  let _ = checkrep replacement in
  let res =
    try
      Pcre.replace ~rex:regex ~templ:replacement input
    with
    | Failure msg ->
	raise (Query (Wrong_Args (msg)))
  in
  res

let check_pattern pattern flags =
  if (matches "" pattern flags)
  then
    raise (Query (Wrong_Args ("Invalid regular expression: \"" ^pattern^ "\", matches \"\" in tokenize")))

let tokenize input pattern flags =
  let _ = check_pattern pattern flags in
  let flaglist = Pcre.cflags (getflags flags 0) in
  let regex = make_regex flaglist pattern in
  let full_split = (Pcre.full_split ~rex:regex input) in
  let rec separate sp empty =
    match sp with
    | [] -> if empty then "" :: [] else []
    | Pcre.Text s :: spr -> s :: (separate spr false)
    | Pcre.Delim _ :: spr -> if empty then "" :: (separate spr true) else (separate spr true)
    | _ :: spr -> (separate spr empty)
  in
  separate full_split true

