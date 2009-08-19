(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_value_index.ml,v 1.4 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Dm_value_index
   Description:
     Support for value indices.
*)

open Error

(* Value indices *)

type value_index = (string, Physical_value.item list) Hashtbl.t

let create_value_index () = Hashtbl.create 167

let add_item_to_value_index vi s il =
  Hashtbl.add vi s il

let get_item_from_value_index vi s =
  Hashtbl.find_all vi s

(* Collection of value indices *)

type value_indices_hash =
    (string,value_index) Hashtbl.t


let init_value_indices_hash () = Hashtbl.create 17

let build_value_indices_hash () =
  Hashtbl.create 17

let add_value_index vih s vi =
  if Hashtbl.mem vih s
  then ()
  else
    Hashtbl.add vih s vi

let get_value_index vih s =
  if Hashtbl.mem vih s
  then
    Hashtbl.find vih s
  else
    raise (Query (Wrong_Args ("No existing value index with name" ^ s)))

let merge_value_indices value_indices1 value_indices2 =
  Gmisc.merge_hashtable value_indices1 value_indices2

