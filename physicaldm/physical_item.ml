(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_item.ml,v 1.4 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_item
   Description:
     Physical representation of XDM items.
*)

open Error

open Dm_atomic
open Dm

open Physical_value


let item_kind item =
  match item with
  | Item_Atomic _ -> AtomicValueKind
  | Item_Node _ -> NodeKind

let string_value item =
  match item with
  | Item_Atomic a -> a#string_value()
  | Item_Node n -> n#string_value()
  
let getAtomicValue item =
  match item with
  | Item_Atomic a -> a
  | Item_Node _ -> raise (Query (Datamodel "Not an atomicValue"))

let getNode item =
  match item with
  | Item_Atomic _ -> raise (Query (Datamodel( "Not a node")))
  | Item_Node n -> n


