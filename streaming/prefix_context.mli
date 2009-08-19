(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: prefix_context.mli,v 1.3 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Prefix_context
   Description:
     This module implements the context necessary to turn a resolved
     SAX stream back into an unresolved one with the proper xmlns
     attributes.
*)


(*********************************)
(* A type for the prefix context *)
(*********************************)

type prefix_context


(********************************)
(* Creates a new prefix context *)
(********************************)

val build_prefix_context : unit -> prefix_context



(*********************************)
(* Operations on prefix contexts *)
(*********************************)

val push_nsenv_in_prefix_context : prefix_context -> Namespace_context.nsenv -> Namespace_context.binding_table
val pop_nsenv_from_prefix_context : prefix_context -> unit

