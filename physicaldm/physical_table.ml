(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_table.ml,v 1.8 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_table
   Description:
     This modules defines physical representations for tuples and
     tables.
*)


open Dm
open Error

open Physical_value


(**********)
(* Tables *)
(**********)

let empty_tuple = ()

let table_of_singleton t = Cursor.cursor_of_singleton t
let table_is_empty ta    = Cursor.cursor_is_empty ta
let table_empty ()       = Cursor.cursor_empty ()


