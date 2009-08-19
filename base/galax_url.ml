(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* Module: Galax_url
   Description: This module contains functions for decoding URLs.
*)

open Neturl
open Error

type decoded_url =
  | File of string
  | Http of (string * int * string)
  | ExternalSource of (string * string * string option * string)

let registered_methods =
  ref []

let is_registered b =
  List.exists (fun x -> x=b) !registered_methods

let register_method b =
  if (is_registered b)
  then
    raise (Query (Toplevel_Error ("URL method : " ^ b ^ " already registered")))
  else
    registered_methods := b :: !registered_methods


(* fix_local worked with Http.decode_url, and fix_local2 works with decode_url2 below *)

let fix_local r =
  match r with
  | None -> "/"
  | Some x -> "/" ^ x

let fix_local2 r =
  match r with
  | None -> "/"
  | Some x -> x

let decodeaux s =
      try 
	let scheme = 
	  extract_url_scheme s in
	let newurl = 
	  if is_registered scheme then
	    url_of_string ip_url_syntax s
	  else
	    url_of_string (Hashtbl.find common_url_syntax scheme) s in
	let urlhost = 
	  try 
	    url_host newurl
	  with Not_found -> "" in
	let urlportopt = 
	  try 
	    Some(string_of_int (url_port newurl))
	  with Not_found -> None in
	let urlpath = 
	  try 
	    join_path (norm_path (url_path newurl)) ^
	    if is_registered scheme then
	  (* If it is an external registered source then the url fragment
	     needs to be appended on to the path *)
	      try
		"#" ^ url_fragment newurl 
	      with
	      | Not_found -> ""

	    else
	      ""
	  with Not_found -> "" in
	Some((url_scheme newurl), urlhost, urlportopt, Some urlpath)
      with _ -> None


let decode_url2 s =
  try
    if((String.compare (String.sub s 0 6) ("file:\\")) = 0) 
    then
      (
       (* Indicating a windows file uri *)
       (* Netstring does not parse them correctly so the old parsing*)
       (* routine has to be used *)
      Http.decode_url s
	)
    else
      decodeaux s
  with 
  | Invalid_argument _ -> decodeaux s
  | x -> raise x


let glx_decode_url s =
  match decode_url2 s with
  | None ->
      (File s)
  | Some ("file", host, port, s) ->
      if host = "" && port = None then
	let local = fix_local2 s in
	(File local)
      else
	raise (Query (URI_Error "File URL should not have a host or port number"))
  | (Some ("http", host, port, s)) ->
      let local = fix_local2 s in
      let new_port =
	match port with
	| None -> 80   (* Default port *)
	| Some portnum ->
	    begin
	      try
		(int_of_string portnum)
	      with
	      | _ ->
		  raise (Query (URI_Error (portnum ^ " is not a valid port number")))
	    end
      in 
      (Http (host, new_port, local))
  | (Some (me, host, port, s)) ->
      try
	if (is_registered me)
	then
	  let local = fix_local2 s in
	  ExternalSource (me,host,port,local)
	else
	  raise Not_found
      with
      | _ ->
	  raise (Query (URI_Error ("Method: " ^ me ^ " not supported in URL")))

