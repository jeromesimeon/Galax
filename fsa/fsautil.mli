(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: fsautil.mli,v 1.2 2007/02/01 22:08:46 simeon Exp $ *)

(* Module Fsautil
   Description:
     This module contains a few function used in the rest of the FSA
     package.
*)

(* Utilities used in the FSA package *)

val list_of_self : int -> 'a -> 'a list

val cross_product : 'a list -> 'b list -> ('a * 'b) list

(* Create a new ordered type whose base type is a pair of a given
   original ordered type *)

module MakeOrderedSetPair (Ordered : Set.OrderedType) :
  Set.OrderedType with type t = (Ordered.t * Ordered.t)

