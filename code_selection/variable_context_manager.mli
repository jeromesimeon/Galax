(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: variable_context_manager.mli,v 1.7 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Update_ordering 
     This module deals with variable context during runtime.  It also
     holds the build functions to access and store variables
*)

type variable_context_manager
type variable_ref 

val build_context_manager : unit -> variable_context_manager
(* This must be called before the manager can be used - 
   it allocates space for the variables.
   
   After it is called no variable references can be added to the context manager
*)
val instantiate_variable_context_manager : variable_context_manager -> unit


val build_parameter_insert_code : variable_context_manager -> int -> (Physical_value.xml_value -> unit)

val build_variable_enter_context : variable_context_manager -> (unit -> unit)
val build_variable_exit_context  : variable_context_manager -> (unit -> unit)
val build_variable_store_code    : variable_ref -> (Physical_value.xml_value -> unit)
val build_variable_retrieve_code : variable_ref -> (unit -> Physical_value.xml_value)
val build_variable_assign_code   : variable_ref -> (unit -> Physical_value.xml_value) -> (unit -> Physical_value.xml_value)


val get_new_variable_slot       : variable_context_manager -> variable_ref

val string_of_variable_ref      : variable_ref -> string
