(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: id.ml,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Id
   Description:
     Generation of unique id's, which, in this version are Caml
     integers.
*)

(* id generators *)

type id_gen = int ref

let create i = ref i

let init ig i =
  ig := i

let next ig =
  let current = !ig in
  ig := current + 1;
  current

let top ig=
  !ig
