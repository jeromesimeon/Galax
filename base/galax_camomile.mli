(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_camomile.mli,v 1.5 2007/09/27 19:19:28 simeon Exp $ *)

(* Module: Glx_camomile
   Description:
     This module is a wrapper over Camomile operations.
*)

val nfc : string -> string
val nfd : string -> string
val nfkc : string -> string
val nfkd : string -> string

val utf8_string_of_code_point : int -> string
val utf8_code_points_of_string : string -> int list
val utf8_string_of_code_points : int list -> string

val utf8_codepoint_compare : string -> string -> int

