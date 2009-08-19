(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: nodeid_context.ml,v 1.2 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Nodeid_creation_context
   Description:
     This module contains context_information used to generate
     appropriate node identity during data model loading.
*)

open Gmisc
open Error


(*********************************)
(* Generic node ids construction *)
(*********************************)

type nodeid_context =
    { preorder_gen  : Id.id_gen;
      postorder_gen : Id.id_gen;
      pre_increment_function  : Id.id_gen -> int;
      post_increment_function : Id.id_gen -> int }


(*********************************)
(* Document and node id creation *)
(*********************************)

(* create a new node id generator for a given document *)

let build_nodeid_context pre post =
  { preorder_gen = Id.create pre;
    postorder_gen = Id.create post;
    pre_increment_function = Id.next;
    post_increment_function = Id.next }


let default_nodeid_context () =
  build_nodeid_context Nodeid.min_preorder Nodeid.min_postorder

(* new prefix *)

let new_pre nodeid_context =
  nodeid_context.pre_increment_function nodeid_context.preorder_gen


(* new postfix *)

let new_post nodeid_context =
  nodeid_context.post_increment_function nodeid_context.postorder_gen


(* Convenience function to create ids for leaf nodes *)

let new_leaf_pre_post nodeid_context =
  (new_pre nodeid_context, new_post nodeid_context)


