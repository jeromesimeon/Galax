(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_value_util.mli,v 1.5 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_value_util
   Description:
     This modules defines various utilities (notably boxing/unboxing)
     for physical values.
*)

open Error

open Physical_value
open Streaming_types


(* Basic physical value constructors *)

val empty_xml_value                   : unit -> physical_value
val empty_sax_value                   : unit -> physical_value


(* From specific values to physical values *)

val physical_value_of_xml_value       : xml_value      		   -> physical_value
val physical_value_of_dom_value       : dom_value   	           -> physical_value
val physical_value_of_item            : item                       -> physical_value
val physical_value_of_item_list       : item list                  -> physical_value
val physical_value_of_item_cursor     : item Cursor.cursor         -> physical_value

val physical_value_of_tuple           : tuple_unit     		   -> physical_value
val physical_value_of_tuple_cursor    : tuple_unit Cursor.cursor   -> physical_value

val physical_value_of_sax_value       : Streaming_types.typed_xml_stream -> physical_value

(* From physical values to specific values *)

val xml_value_of_physical_value       : physical_value -> xml_value
val dom_value_of_physical_value       : physical_value -> dom_value
val item_cursor_of_physical_value     : physical_value -> item Cursor.cursor
val item_list_of_physical_value       : physical_value -> item list

val tuple_cursor_of_physical_value    : physical_value -> tuple_unit Cursor.cursor

val sax_value_of_physical_value       : physical_value -> Streaming_types.typed_xml_stream


(* Materialize/Streaming of physical values *)

val materialize_physical_value        : physical_value  -> physical_value
val stream_physical_value             : physical_value  -> physical_value


(* Special operation used in the evaluation code of for loops *)

val concat_physical_value_cursor      : physical_value Cursor.cursor -> physical_value
val concat_xml_value_cursor           : xml_value Cursor.cursor -> xml_value

(* Wraps each item contained in an xml_value in its own,
   fresh xml_value. - Michael *)
val slice_xml_value                   : xml_value -> xml_value Cursor.cursor
val slice_sax_value                   : typed_xml_stream -> typed_xml_stream Cursor.cursor
val slice_discard_sax_value           : typed_xml_stream -> unit Cursor.cursor
val slice_item_cursor                 : item Cursor.cursor -> item Cursor.cursor Cursor.cursor
val slice_item_list                   : item list -> item list Cursor.cursor

val is_nan : item -> bool


