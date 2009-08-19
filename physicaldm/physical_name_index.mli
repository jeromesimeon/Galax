(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_name_index.mli,v 1.10 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_name_index
   Description:
     This module contains support for building name indexes at
     document loading time.
*)

open Dm_atomic_btree_util

(* Name indices *)

type name_index = Physical_value.dom_tuple full_btree_index
type name_index_handler = Physical_value.dom_tuple full_btree_handler

(* Collection of name indices *)

type name_indices = (Namespace_symbols.relem_symbol * name_index) list
type name_indices_handler = (Namespace_symbols.relem_symbol * name_index_handler) list

type name_indices_hash
val init_name_indices_hash  : unit -> name_indices_hash
val build_name_indices_hash : name_indices_handler -> name_indices_hash
val get_name_index : name_indices_hash -> Namespace_symbols.relem_symbol -> name_index_handler
val get_opt_name_index : name_indices_hash -> Namespace_symbols.relem_symbol -> name_index_handler option
val get_all_name_indices : name_indices_hash -> name_indices_handler

(* Create a name index *)

val create_name_index : unit -> name_index_handler

val add_item_to_name_index :
    name_index_handler -> Dm.node -> Nodeid.prepostint_docorder -> unit

val add_pre_order_item_to_name_index :
    name_index_handler -> int -> int

val add_post_order_item_to_name_index :
    name_index_handler -> int -> Dm.node -> int -> int

(* Empty name_indices *)

val no_name_indices : name_indices_handler

val add_new_name_index :
    name_indices_hash -> Namespace_symbols.relem_symbol -> name_index_handler -> unit

val merge_name_indices :
    name_indices_hash -> name_indices_hash -> name_indices_hash

(* sequential scan of the index *)

val pre_cursor_of_name_index_at_pos :
      name_index -> int -> (Physical_value.item Physical_value.sequence) Cursor.cursor
val pre_cursor_of_name_index_from_post :
      name_index -> int -> (Physical_value.item Physical_value.sequence) Cursor.cursor
val pre_cursor_of_name_index_from_window :
      name_index -> int -> int -> (Physical_value.item Physical_value.sequence) Cursor.cursor

