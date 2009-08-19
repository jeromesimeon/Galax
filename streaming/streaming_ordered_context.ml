(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: streaming_ordered_context.ml,v 1.5 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Streaming_ordered_context
   Description:
     This module implements the context necessary to add identity
     information to an input typed stream.
*)

open Nodeid
open Nodeid_context


(********************************************)
(* A type for the streaming ordered context *)
(********************************************)

type streaming_ordered_context =
    { streaming_ordered_context_docid   : docid;
      streaming_ordered_context_idgen   : nodeid_context }


(*******************************************)
(* Creates a new streaming ordered context *)
(*******************************************)

let build_streaming_ordered_context docid nodeid_context =
    { streaming_ordered_context_docid   = docid;
      streaming_ordered_context_idgen   = nodeid_context }


(*******************************)
(* Operations on node identity *)
(*******************************)

let get_docid streaming_ordered_context =
  streaming_ordered_context.streaming_ordered_context_docid

let new_preorderid streaming_ordered_context =
  let docid = get_docid streaming_ordered_context in
  let preorder = new_pre streaming_ordered_context.streaming_ordered_context_idgen in
  (docid,preorder)

let new_postorderid streaming_ordered_context =
  let docid = get_docid streaming_ordered_context in
  let postorder = new_post streaming_ordered_context.streaming_ordered_context_idgen in
  (docid,postorder)

let new_leaf_docorder streaming_ordered_context =
  let docid = get_docid streaming_ordered_context in
  let (preorder,postorder) = new_leaf_pre_post streaming_ordered_context.streaming_ordered_context_idgen in
  (docid,preorder,postorder)

