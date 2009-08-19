(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: nodeid_context.mli,v 1.2 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Nodeid_creation_context
   Description:
     This module contains context_information used to generate
     appropriate node identity during data model loading.
*)


(********************************)
(* Type for the node id context *)
(********************************)

type nodeid_context

(* Creates a nodeid context *)

val build_nodeid_context   : int -> int -> nodeid_context
val default_nodeid_context : unit -> nodeid_context
    (* default node id context uses min_preorder and min_postorder as
       seeds. *)

(************************)
(* Creates new node ids *)
(************************)

(* new pre order *)

val new_pre  : nodeid_context -> Nodeid.preorder

(* new post order *)

val new_post : nodeid_context -> Nodeid.postorder

(* Convenience function to create a pre/post pair for a leaf node *)

val new_leaf_pre_post : nodeid_context -> Nodeid.preorder * Nodeid.postorder


