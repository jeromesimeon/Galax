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
   Description:  This module contains functions for decoding URLs.
*)

type decoded_url =
  | File of string
  | Http of (string * int * string)
  | ExternalSource of (string * string * string option * string)

val register_method : string -> unit

val glx_decode_url : string -> decoded_url

