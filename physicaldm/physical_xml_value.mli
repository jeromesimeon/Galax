(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_xml_value.mli,v 1.6 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_xml_value
   Description:
     This modules defines physical representations of XML values.
*)

open Cursor
open Physical_value

(* Conversions *)

val dom_value_of_sax_value : sax_value -> dom_value
val sax_value_of_dom_value : dom_value -> sax_value

val materialize_xml_value  : xml_value -> xml_value

val dom_value_of_xml_value : xml_value -> dom_value
val sax_value_of_xml_value : xml_value -> sax_value

val xml_value_of_dom_value : dom_value -> xml_value
val xml_value_of_sax_value : sax_value -> xml_value

val is_empty_xml_value : xml_value -> bool

val item_cursor_of_xml_value     : xml_value -> item Cursor.cursor
val item_list_of_xml_value       : xml_value -> item list

val xml_value_of_item_list    : item list   -> xml_value 
val xml_value_of_item_cursor  : item cursor -> xml_value 


