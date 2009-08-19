(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: execution_context.mli,v 1.11 2007/05/16 15:32:09 mff Exp $ *)

(* Module: Execution_context
   Description:
     This module implements the XQuery algebra context.
*)

open Dm_atomic

open Physical_value

open Xquery_common_ast

(*******************************************************
 PROGRAM's run-time, DYNAMIC  context: 

     The context item +
     The context tuple +
     Current dateTime + 
     Snap semantic + 
     Update cache + 
     Local timezone +
     Index tables for the declared keys + 
     Alive documents

  Shared by all program components
*******************************************************)
type algebra_context

(******************)
(* Initialization *)
(******************)

(* Creates a new algebra context *)

val build_algebra_context   : unit -> algebra_context
val copy_algebra_context    : algebra_context -> algebra_context
val default_algebra_context : unit -> algebra_context


(*************************************)
(* Support for current date and time *)
(*************************************)

val get_current_datetime :
    algebra_context -> atomicDateTime
val get_timezone :
    algebra_context -> atomicDayTimeDuration
val set_timezone :
    algebra_context -> atomicDayTimeDuration ->  unit

(*****************************)
(* Support for declared keys *)
(*****************************)

(* Add a key and its value to algebra context *)

val add_key_to_algebra_context :
    algebra_context -> (string * string) -> item list ->  algebra_context

(* Lookup a key's value in algebra context *)

val key_from_algebra_context :
    algebra_context -> (string * string) -> item list


val copy_algebra_context : algebra_context -> algebra_context

(********************************)
(* Support for update semantics *)
(********************************)
(* make this more opaque *)
val get_current_snap_semantic     : algebra_context -> Xquery_common_ast.snap_modifier
val set_current_snap_semantic     : algebra_context-> Xquery_common_ast.snap_modifier -> algebra_context

val get_ordering_structure     : algebra_context -> Update_ordering.update_holder
val allocate_update_holder     : algebra_context -> algebra_context

val enter_snap                    : algebra_context -> Xquery_common_ast.snap_modifier -> algebra_context

(* Alive documents *)
val alive_documents_from_algebra_context : algebra_context -> Fn_doc.alive_documents
