(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_sequence.mli,v 1.6 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_sequence
   Description:
     This module implements sequences in the XQuery 1.0 and XPath 2.0
     data model.
*)

(* Note:

     In this module, none of the operations, except forced
     materialization are destructive of the stream.

     This means that to use the cursor operations, one must first cast
     the sequence to a cursor and use the Cursor module. This makes
     sure that we can easily track the destructive operations in the
     rest of the code.

     The "forced" materialization functions are:
	 materialized_of_cursor
	 list_of_sequence
	 materialize_sequence

 - Jerome 03/19/2004
*)

open Physical_value

(*************************)
(* Sequence Constructors *)
(*************************)

(* GENERAL *)

val materialized_of_list   : 'a list -> 'a sequence
val streamed_of_list       : 'a list -> 'a sequence

val materialized_of_cursor : 'a Cursor.cursor -> 'a sequence
val streamed_of_cursor     : 'a Cursor.cursor -> 'a sequence

val list_of_sequence       : 'a sequence -> 'a list
val cursor_of_sequence     : 'a sequence -> 'a Cursor.cursor


(* BASIC *)

val sequence_empty         : unit -> 'a sequence
val sequence_of_singleton  : 'a -> 'a sequence
val sequence_of_option     : 'a option -> 'a sequence


(* Switched from/to materialized and streamed sequences *)

val materialize_sequence : 'a sequence -> 'a sequence
val stream_sequence      : 'a sequence -> 'a sequence


(**********************)
(* Sequence Accessors *)
(**********************)

(* GENERAL *)

val sequence_peek          : 'a sequence -> 'a option
val sequence_npeek         : int -> 'a sequence -> 'a list


(* BASIC / NON DESTRUCTIVE *)

val sequence_is_empty      : 'a sequence -> bool

val sequence_get_singleton : 'a sequence -> 'a
val sequence_is_singleton  : 'a sequence -> bool

val sequence_get_optional  : 'a sequence -> 'a option
val sequence_is_optional   : 'a sequence -> bool


