(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: whitespace.ml,v 1.15 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Whitespace
   Description:
     Manipulation of whitespace within strings. Used during whitespace
     handling in XML/XQuery.
*)

(**************************)
(* Processing white space *)
(**************************)

let rec get_whitespace_gen isspace s b =
  try
    let c =
      Stream.next s
    in
    if (isspace c) then
      begin
	Buffer.add_char b c;
	get_whitespace_gen isspace s b
      end
    else
      let newb = Buffer.create 50 in
      begin
	Buffer.add_char newb c;
	let result = ((Buffer.contents b), (get_non_whitespace_gen isspace s newb)) in
	begin
	  Buffer.reset newb;
	  Buffer.clear newb;
	  result
	end
      end
  with
  | Stream.Failure ->
      ("","")

and get_non_whitespace_gen isspace s b =
  try
    let c =
      Stream.next s
    in
    if not(isspace c) then
      begin
	Buffer.add_char b c;
	get_non_whitespace_gen isspace s b
      end
    else
      let newb = Buffer.create 50 in
      begin
	Buffer.add_char newb c;
	let (lead,trail) = (get_whitespace_gen isspace s newb) in
	let result = (Buffer.contents b) ^ lead ^ trail in
	begin
	  Buffer.reset newb;
	  Buffer.clear newb;
	  result
	end
      end
  with
  | Stream.Failure ->
      (Buffer.contents b)

let get_whitespace s b =
  let isspace c = (c = ' ' || c = '\n' || c = '\t' || c = '\r') in
  get_whitespace_gen isspace s b

let whitespace_only text =
  let b = Buffer.create 50 in
  let s = Stream.of_string text in
  match (get_whitespace s b) with
  | (_,"") ->  true
  | (_,trail) -> false

let remove_whitespace_and_empty text =
  let b = Buffer.create 50 in
  let s = Stream.of_string text in
  let result =
    match (get_whitespace s b) with
    | (_,"") ->
	None
    | (_,trail) ->
	Some trail
  in
  begin
    Buffer.reset b;
    Buffer.clear b;
    result
  end

let remove_whitespace text =
  let b = Buffer.create 50 in
  let s = Stream.of_string text in
  match (get_whitespace s b) with
  | (_,trail) ->
      trail

let re = Netstring_str.regexp "[ \n\t\r]*"

let remove_leading_whitespace text =
  Netstring_str.replace_first re "" text

let get_newlines s b =
  let isspace c = (c = '\n' || c = '\r') in
  get_whitespace_gen isspace s b

let remove_newlines text =
  let b = Buffer.create 50 in
  let s = Stream.of_string text in
  match (get_newlines s b) with
  | (_,trail) ->
      trail

let remove_all_whitespace text =
  Netstring_str.global_replace re "" text

(* Whitespace or not whitespace ? That is the question... *)

let white c =
  (c = ' ') || (c = '\n') || (c = '\t') || (c = '\r')

(* Whitespace preserving mode *)

type mode =
  | Preserve
  | Default


(* Split whitespace-separated strings *)

let whitespace_regexp    = Str.regexp "[ \t\n]+"
let whitespace_pair      = Str.regexp "\013\n"
let whitespace_single    = Str.regexp "\013"
let whitespace_onespace  = Str.regexp "[ \t\n]"

let whitespace_separate str =
  Str.split whitespace_regexp str

let whitespace_normalize str =
  let str = Str.global_replace whitespace_pair "\n" str in
  let str = Str.global_replace whitespace_single "\n" str in
  let str = Str.global_replace whitespace_onespace " " str in
  str

let whitespace_id_normalize str =
  let str = Str.global_replace whitespace_regexp " " str in
  remove_whitespace str

let remove_trailing_spaces str =
  let str = Str.global_replace whitespace_pair "\n" str in
  let str = Str.global_replace whitespace_single "\n" str in
  let i = ref ((String.length str) - 1) in
  while
    if !i >= 0 then ((String.get str !i) = '\n' || (String.get str !i) = ' ')
    else false
  do
    decr i
  done;
  Str.string_before str (!i+1)

