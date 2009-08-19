(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_value_index.mli,v 1.4 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Dm_value_index
   Description:
     Support for value indices.
*)

(* Value index *)

type value_index

val create_value_index        : unit -> value_index
val add_item_to_value_index   : value_index -> string -> Physical_value.item list -> unit
val get_item_from_value_index : value_index -> string -> Physical_value.item list list


(* Collection of value indices *)

type value_indices_hash

val init_value_indices_hash : unit -> value_indices_hash

val build_value_indices_hash : unit -> value_indices_hash
val add_value_index    	     : value_indices_hash -> string -> value_index -> unit
val get_value_index    	     : value_indices_hash -> string -> value_index


val merge_value_indices :
    value_indices_hash -> value_indices_hash -> value_indices_hash

