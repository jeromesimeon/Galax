(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_main_memory_btree.mli,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Shredded_main_memory_btree
   Description:
     This module describes a functorial interface for in memory
     btrees. The btree is paramterized by its key and value modules.
     The naming conventions (and the semantics they imply) are present
     in shredded_store_sigs.mli.
*)

module type Main_Memory_Btree_Functor_Sig =
  functor (Key:Shredded_store_sigs.Shredded_OrderedType) ->
    functor (Value:Shredded_store_sigs.Shredded_OrderedType) ->
sig 

  type btree_handle
  type btree_key   = Key.t
  type btree_value = Value.t
  type btree_cursor

  (* Empty tree *)
  val empty_tree : unit -> btree_handle

  (* Get *)
  val btree_get_set        : btree_handle -> btree_key -> (btree_key * btree_value) option
  val btree_get_set_range  : btree_handle -> btree_key -> (btree_key * btree_value) option

  val btree_get_both       : btree_handle -> (btree_key * btree_value) -> (btree_key * btree_value) option
  val btree_get_both_range : btree_handle -> (btree_key * btree_value) -> (btree_key * btree_value) option

  val btree_get_all        : btree_handle -> btree_key -> btree_value Cursor.cursor

  (* put *)
  val btree_put            : btree_handle -> btree_key -> btree_value -> unit

  (* Delete *)
  val btree_delete         : btree_handle -> btree_key -> btree_value -> unit
  val btree_delete_all     : btree_handle -> btree_key -> unit  

  val btree_sync           : btree_handle -> unit
  val btree_close          : btree_handle -> unit
  val btree_get            : btree_handle -> btree_key -> btree_value option
  val btree_open           : string -> int -> bool -> btree_handle

  (* Cursor *)

  (* Cursor operations *)
  type cursor_direction = Next | Prev
  val btree_cursor_open      : btree_handle -> btree_cursor
  val btree_cursor_to_cursor : btree_cursor -> cursor_direction -> (btree_key * btree_value) Cursor.cursor 

  val btree_cursor_put       : btree_cursor -> btree_key -> btree_value -> unit 
  val btree_cursor_get_first : btree_cursor -> (btree_key * btree_value) option 
  val btree_cursor_get_last  : btree_cursor -> (btree_key * btree_value) option 

  val btree_cursor_get_next  : btree_cursor -> (btree_key * btree_value) option
  val btree_cursor_get_prev  : btree_cursor -> (btree_key * btree_value) option

  val btree_cursor_get_set : btree_cursor -> btree_key -> (btree_key * btree_value) option
  val btree_cursor_get_both      : btree_cursor -> btree_key * btree_value -> (btree_key * btree_value) option 
  val btree_cursor_get_set_range : btree_cursor -> btree_key -> (btree_key * btree_value) option
  val btree_cursor_get_both_range: btree_cursor -> btree_key * btree_value -> (btree_key * btree_value) option 

  val btree_cursor_del   : btree_cursor -> unit 
  val btree_cursor_close : btree_cursor -> unit
end
  
module Main_Memory_Btree_Functor : Main_Memory_Btree_Functor_Sig
