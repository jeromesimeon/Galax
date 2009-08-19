(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_table.mli,v 1.7 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_table
   Description:
     This modules defines physical representations for tuples and
     tables.
*)

open Physical_value

(**********)
(* Tuples *)
(**********)

(* Tuples passed functionally are always empty *)

(* Note:
     Tuples here are actual always empty. This type is only used on
     the interface for cursor operations.
*)

(**********)
(* Tables *)
(**********)

val empty_tuple : tuple_unit

val table_of_singleton : tuple_unit -> table_unit
val table_is_empty     : table_unit -> bool
val table_empty        : unit -> table_unit


