(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_item.mli,v 1.5 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_item
   Description:
     Physical representation of XDM items.
*)


open Physical_value

val item_kind    : item -> _ItemKind

val string_value : item -> Datatypes.xs_string

val getAtomicValue : item -> Dm_atomic.atomicValue
val getNode        : item -> Dm.node

