(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util_ridlist.mli,v 1.4 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Cs_ridlist
   Description:
     This module contains operations on record-id lists (ridlists).
*)

type rid = int

type rid_list = (* This should change *)
  | Full_RidList
  | Empty_RidList
  | Regular_RidList of rid list

val is_empty_rid_list  : rid_list -> bool
val is_full_rid_list   : rid_list -> bool
val intersect_rid_list : rid_list -> rid_list -> rid_list
val union_rid_list     : rid_list -> rid_list -> rid_list

val rid_compare : rid -> rid -> int

val cursor_of_rid : rid_list -> int -> int Cursor.cursor

val unwrap_rid_list_cursor :
    'a array -> ('a -> unit) -> rid_list -> Physical_value.tuple_unit Cursor.cursor

