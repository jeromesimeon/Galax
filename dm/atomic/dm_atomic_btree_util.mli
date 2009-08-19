(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm_atomic_btree_util.mli,v 1.3 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Dm_atomic_btree_util
   Description:
     This module contains utilities used for BTrees over XML items.
*)

(***************)
(* BTree types *)
(***************)

(* BTree Handlers *)

type 'a full_btree_handler

(* Final BTrees *)

type int_btree           = int Dm_atomic_btree.btree
type 'a full_btree_index = 'a array * int_btree array



(********************************)
(* Operations on Btree handlers *)
(********************************)

(* Create a new btree handler *)
val create_full_btree_handler : int -> 'a full_btree_handler

(* Get the main array from a btree handler *)
val get_main_array : 'a full_btree_handler -> 'a array

(* Set the main array in a btree handler *)
val set_main_array : 'a full_btree_handler -> 'a array -> unit

(* Load a btree with a given index *)

val single_add_to_btree : 'a full_btree_handler -> int -> Dm_atomic_btree.key * int -> unit
val bulk_add_to_btree   : 'a full_btree_handler -> int -> (Dm_atomic_btree.key * int) list -> unit


(* Get a given loaded btree on indes *)
val finalize_btree_on_index           : 'a full_btree_handler -> int -> unit
val get_loaded_btree_on_index         : 'a full_btree_handler -> int -> int_btree
val full_btree_index_of_btree_handler : 'a full_btree_handler -> 'a full_btree_index
val full_btree_index_of_sorted_btree_handler : 'a full_btree_handler -> 'a full_btree_index

val full_btree_index_add_to_main_array : 'a full_btree_handler -> 'a -> int
val full_btree_index_update_in_main_array : 'a full_btree_handler -> int -> 'a -> unit
