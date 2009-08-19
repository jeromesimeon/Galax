(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: id.mli,v 1.7 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Id
   Description:
     Generation of unique id's, which, in this version are Caml
     integers.
*)

(* id generators *)

type id_gen

val create : int -> id_gen

val init : id_gen -> int -> unit
val next : id_gen -> int
val top  : id_gen -> int

