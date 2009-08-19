(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_nodeid.ml,v 1.7 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Galax_nodeid
   Description:
     This module implements specifics of the Galax node ids.
*)

open Nodeid
open Nodeid_context


(* Galax's implementation id *)

let galax_implemid = new_implemid "Galax"

(* Galax node id's are always pre-order integers *)

type galax_nodeid = prepostint_docorder


(* Doc id generator for Galax only *)

let galax_docid_gen =
  build_docid_gen ()

let new_docid () =
  new_docid galax_docid_gen


(* Pre-post generation *)

type galax_nodeid_gen = (docid * nodeid_context)

let build_galax_nodeid_gen nodeid_context =
  (new_docid (), nodeid_context)

let build_galax_nodeid_gen_for_docid nodeid_context docid =
  (docid, nodeid_context)

let new_galax_pre (_,pre_post_gen) =
  new_pre pre_post_gen

let new_galax_post (_,pre_post_gen) =
  new_post pre_post_gen

let new_galax_nodeid (docid,_) pre post =
  (docid,pre,post)

let new_galax_leaf_nodeid (docid,pre_post_gen) =
  let pre = new_galax_pre (docid,pre_post_gen) in
  let post = new_galax_post (docid,pre_post_gen) in
  (docid,pre,post)

