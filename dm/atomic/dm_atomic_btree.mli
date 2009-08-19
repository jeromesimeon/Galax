(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm_atomic_btree.mli,v 1.3 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Dm_atomic_btree
   Description:
     This module contains support for BTrees over XML items.
*)

(* Stuff like a btree implemented on top of arrays *)


open Dm_atomic

(*******************)
(* Data structures *)
(*******************)

type key       = atomicValue
type 'a pair   = (key * 'a)

type position = int
type 'a btree  = ('a pair) array 

val min_typed_value : 'a btree -> key -> position
val max_typed_value : 'a btree -> key -> position

val find_low_point  : 'a btree -> key -> position
val find_high_point : 'a btree -> key -> position

val previous_highest_value : 'a btree -> key -> position -> position
val next_lowest_value      : 'a btree -> key -> position -> position

val max_position   : 'a btree -> position
val min_position   : 'a btree -> position

val position_before  : position -> position -> bool
val position_after   : position -> position -> bool

val first : 'a btree -> key
val last  : 'a btree -> key

val sub     : 'a btree -> position -> position -> 'a btree
val to_list : 'a btree -> 'a list

type 'a bulk_loader

val init       : ('a -> 'a -> int) -> 'a bulk_loader
val single_add : 'a bulk_loader -> key * 'a -> unit
val bulk_add   : 'a bulk_loader -> (key * 'a) list -> unit
val finalize   : 'a bulk_loader -> 'a btree
val finalize_sorted : 'a bulk_loader -> 'a btree
val reset      : 'a bulk_loader -> unit

