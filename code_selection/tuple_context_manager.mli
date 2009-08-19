(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: tuple_context_manager.mli,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Tuple_context_manager
     This module eals with variable context during runtime.  It also
     holds the build functions to access and store variables
 *)


type tuple_context_manager
type tuple_ref = tuple_context_manager * int

val build_context_manager : unit -> tuple_context_manager 
val instantiate_tuple_context_manager : tuple_context_manager -> unit

val string_of_tuple_ref   : tuple_ref -> string

val build_tuple_enter_context : tuple_context_manager -> (unit -> unit)
val build_tuple_exit_context  : tuple_context_manager -> (unit -> unit)
val build_tuple_store_code    : tuple_ref -> (Physical_value.xml_value -> unit)
val build_tuple_retrieve_code : tuple_ref -> (unit -> Physical_value.xml_value)

val get_new_tuple_slot                : tuple_context_manager -> tuple_ref
