(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: regularexp.mli,v 1.3 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Regularexp
   Description:
     This module implements regular expressions on strings in F&O functions.
*)

val matches : string -> string -> string -> bool

val replace : string -> string -> string -> string -> string

val tokenize : string -> string -> string -> string list
