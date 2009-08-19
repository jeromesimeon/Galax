(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: update_ordering.mli,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Update_ordering 
     Description: This module contains a structure to assist with
     ordering for the deterministic order semantic.
*)

type update_holder 


(******************************************************)
(* Delta type (atomic update) as in the semantic note *)
(******************************************************)
type delta = 
  | Insert       of Dm.node * Dm.node option * Dm.node list
  | Delete       of Dm.node * Dm.node
  | ReplaceValue of Dm.node * Dm.text
  | Replace      of Dm.node * Dm.node * Dm.node list
  | Rename       of Dm.node * Datatypes.xs_QName

(* Returns the newly allocated update_holder, and side-effects the original *)
val allocate_update_holder : update_holder -> update_holder

val add_concrete_update    : update_holder -> delta -> unit

val collect_all_updates    : update_holder -> delta list

val make_fresh_place_holder : unit -> update_holder
