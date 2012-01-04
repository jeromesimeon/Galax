(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: whitespace.mli,v 1.14 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Whitespace
   Description:
     Manipulation of whitespace within strings. Used during whitespace
     handling in XML/XQuery.
*)

(* Whitespace operations *)

val remove_newlines   : string -> string

(* Remove leading and trailing whitespace characters *)
val remove_whitespace : string -> string
val remove_leading_whitespace : string -> string
val remove_all_whitespace : string -> string

val whitespace_only : string -> bool


(* Whitespace or not whitespace ? That is the question... *)

val white : char -> bool

type mode =
  | Preserve
  | Default


(* Split whitespace-separated strings *)

val whitespace_separate : string -> string list

val whitespace_normalize    : string -> string
val whitespace_id_normalize : string -> string

val remove_trailing_spaces : string -> string

val normalize_space : string -> string

