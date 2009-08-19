(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: nodeid.ml,v 1.4 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Nodeid
   Description:
     This module implements node-id support as a basic service
     available to all implementations connected to Galax. It also
     implements the main nodeid type which is used withing the Galax
     query processor.
*)

open Gmisc
open Error


(******************************)
(* Generic node id structures *)
(******************************)

(* Common components *)

type implemid = int
type docid    = int

(* gId = group identifier, mId = member identifier. *)
type int64_pair = {gId: int64; mId: int64}

type preorder  = int
type postorder = int
type large_preorder = int64_pair

(* Document order *)

type prepostint_docorder  = docid * preorder * postorder
type preint_docorder      = docid * preorder
type postint_docorder     = docid * postorder
type pre_intpair_docorder = docid * large_preorder

type partial_docorder =
  | PrePostInt of prepostint_docorder
  | PreInt of preint_docorder
  | PreIntPair of pre_intpair_docorder

type docorder = (implemid * partial_docorder)

(* Node identity *)

type partial_nodeid =
  | IntId of docid * int
  | IntPairId of docid * int64_pair

type nodeid = (implemid * partial_nodeid)


(*****************************************)
(* Generic access operations on node-ids *)
(*****************************************)

let get_implemid  (implemid,_)       = implemid
let get_docid     (_,partialid)      =
  match partialid with
  | PrePostInt (docid,_,_) -> docid
  | PreInt (docid,_) -> docid
  | PreIntPair(docid,_) -> docid
let get_partialid (_,partial_docorder) = partial_docorder


(******************************)
(* Node comparison operations *)
(******************************)

let partial_nodeid_is pid1 pid2 =
  match (pid1,pid2) with
  | (IntId (docid1,pre1), IntId (docid2,pre2)) ->
      (docid1 = docid2) && (pre1 = pre2)
  | (IntPairId (docid1,pre1), IntPairId (docid2,pre2)) ->
      (docid1 = docid2) && (pre1.gId = pre2.gId) && (pre1.mId = pre2.mId)
  | _ ->
      raise (Query (Datamodel "Two nodes from the same implementation must have same node-id scheme"))

let nodeid_is (implemid1,partial_nodeid1) (implemid2,partial_nodeid2) = 
  if not(implemid1 = implemid2)
  then
    false
  else
    partial_nodeid_is partial_nodeid1 partial_nodeid2

let partial_docorder_precedes pid1 pid2 =
  match (pid1,pid2) with
  | PrePostInt(docid1,pre1,_), PrePostInt(docid2,pre2,_) ->
      if not(docid1 = docid2)
      then
	docid1 < docid2
      else
	(pre1 < pre2)
  | PreInt(docid1,pre1), PreInt(docid2,pre2) ->
      if not(docid1 = docid2)
      then
	docid1 < docid2
      else
	(pre1 < pre2)
  | PreIntPair(docid1,pre1), PreIntPair(docid2,pre2) ->
      (docid1 < docid2) ||
      ((docid1 = docid2) && (pre1.gId < pre2.gId)) ||
      ((docid1 = docid2) && (pre1.gId = pre2.gId) && (pre1.mId < pre2.mId))
  | _ ->
      raise (Query (Datamodel "Two nodes from the same implementation must have same doc-order scheme"))

let docorder_precedes (implemid1,partial_docorder1) (implemid2,partial_docorder2) = 
  if not(implemid1 = implemid2)
  then
    implemid1 < implemid2
  else
    partial_docorder_precedes partial_docorder1 partial_docorder2

let docorder_follows nid1 nid2 = docorder_precedes nid2 nid1

let nodeid_of_docorder docorder =
  match docorder with
  | (implemid, PrePostInt (docid,preorder,_)) ->
      (implemid, IntId (docid,preorder))
  | (implemid, PreInt (docid,preorder)) ->
      (implemid, IntId (docid,preorder))
  | (implemid, PreIntPair(docid,preorder)) ->
      (implemid, IntPairId (docid,preorder))
      

let docorder_compare  nid1 nid2 =
  if nodeid_is (nodeid_of_docorder nid1) (nodeid_of_docorder nid2) then 0
  else if docorder_precedes nid1 nid2 then -1
  else 1

let is_preceding_xpath pid1 pid2 =
  match (pid1,pid2) with
  |(implemid1, PrePostInt(docid1,pre1,post1)), (implemid2,PrePostInt(docid2,pre2,post2)) ->
      if not(implemid1 = implemid2)
      then
	implemid1 < implemid2
      else if not(docid1 = docid2)
      then
	docid1 < docid2
      else
	((pre1 < pre2) && (post1 < post2))
  | _ ->
      raise (Query (Datamodel "Is_preceding_xpath requires pre/post doc-order schema."))

let is_following_xpath pid1 pid2 =
  match (pid1,pid2) with
  | (implemid1,PrePostInt(docid1,pre1,post1)), (implemid2,PrePostInt(docid2,pre2,post2)) ->
      if not(implemid1 = implemid2)
      then
	implemid1 > implemid2
      else if not(docid1 = docid2)
      then
	docid1 > docid2
      else
	((pre1 > pre2) && (post1 > post2))
  | _ ->
      raise (Query (Datamodel "Is_preceding_xpath requires pre/post doc-order schema."))

let is_ancestor_xpath pid1 pid2 =
  match (pid1,pid2) with
  | (implemid1,PrePostInt(docid1,pre1,post1)), (implemid2,PrePostInt(docid2,pre2,post2)) ->
      if not(implemid1 = implemid2) || not(docid1 = docid2)
      then false
      else
	((pre1 < pre2) && (post1 > post2))
  | _ ->
      raise (Query (Datamodel "Is_preceding_xpath requires pre/post doc-order schema."))

let is_descendant_xpath pid1 pid2 =
  match (pid1,pid2) with
  | (implemid1,PrePostInt(docid1,pre1,post1)), (implemid2,PrePostInt(docid2,pre2,post2)) ->
      if not(implemid1 = implemid2) || not(docid1 = docid2)
      then false
      else
	((pre1 > pre2) && (post1 < post2))
  | _ ->
      raise (Query (Datamodel "Is_preceding_xpath requires pre/post doc-order schema."))


(**********************************)
(* Builds a new implementation id *)
(**********************************)

let implem_name_table = Hashtbl.create 167

let current_implemid = Id.create 0

let new_implemid implem_name =
  let newid = Id.next current_implemid in
  begin
    Hashtbl.add implem_name_table newid implem_name;
    newid
  end

let get_implem_name iid =
  try
    Hashtbl.find implem_name_table iid
  with
  | _ ->
      raise (Query (Datamodel ("Implementation id " ^ (string_of_int iid) ^ " not registered!")))


(************)
(* Printing *)
(************)

let string_of_partial_docorder pid =
  match pid with
  | PrePostInt (docid,pre,post) ->
      ( "; docid: "
	^ (string_of_int docid)
	^ ";"
	^ "prefix: "
	^ (string_of_int pre)
	^ "; postfix: "
	^ (string_of_int post))
  | PreInt (docid,pre) ->
      ("; docid: "
       ^ (string_of_int docid)
       ^ ";"
       ^ "prefix: "
       ^ (string_of_int pre))
  | PreIntPair(docid,pre) ->
      ("; docid: "
       ^ (string_of_int docid)
       ^ ";"
       ^ "prefix : ("
       ^ (Int64.to_string pre.gId)
       ^ ":"
       ^ (Int64.to_string pre.mId)
       ^ ")")

let string_of_docorder (implemid,pid) =
  ("{ implementation: "
   ^ (get_implem_name implemid)
   ^ "implemid: "
   ^ (string_of_int implemid)
   ^ (string_of_partial_docorder pid)
   ^ " }")


(*******************************)
(* Generic doc id constructors *)
(*******************************)

type docid_gen = Id.id_gen

let build_docid_gen () = Id.create 1

let new_docid docid_gen = Id.next docid_gen


(*********************************)
(* Generic node ids construction *)
(*********************************)

type pre_post_gen = (Id.id_gen * Id.id_gen)


(*********************************)
(* Document and node id creation *)
(*********************************)

(* create a new node id generator for a given document *)

let build_pre_post_gen () =
  (Id.create 1, Id.create 1)


(* new prefix *)

let new_pre (pre_gen,_) =
  Id.next pre_gen


(* new postfix *)

let new_post (_,post_gen) =
  Id.next post_gen


(* Convenience function to create ids for leaf nodes *)

let new_leaf_pre_post pre_post_gen =
  (new_pre pre_post_gen, new_post pre_post_gen)



(* build node id *)

let build_docorder_from_pre_post implemid docid (pre, post) =
  (implemid,(PrePostInt (docid,pre,post)))

let merge_docorder_from_pre_post (docid,pre) (_,post) = (docid,pre,post)


(* Special node ids *)

(* eventually this should become: ?
let min_preorder  = min_int
let min_postorder = min_int

let max_preorder  = max_int
let max_postorder = max_int
*)

let min_preorder  = 1
let min_postorder = 1

let max_preorder  = max_int
let max_postorder = max_int



let large_preorder_to_big_int  pre =
  let x = Gmisc.big_int_of_int64 pre.gId in
  let shifted = Big_int.mult_big_int x (Gmisc.big_int_of_int64 Int64.max_int) in
    Big_int.add_big_int shifted (Gmisc.big_int_of_int64 pre.mId)


let big_int_to_large_preorder bi =
  let (q, r) = Big_int.quomod_big_int bi (Gmisc.big_int_of_int64 Int64.max_int) in
  {gId= Gmisc.int64_of_big_int q; mId = Gmisc.int64_of_big_int r}

let big_int_pair_of_docorder ord =
  let (implemid, pord) = ord in
  match pord with
   | PrePostInt (docid, pre, _)
   | PreInt (docid, pre) -> 
         (Big_int.big_int_of_int docid, Big_int.big_int_of_int pre)
   | PreIntPair (docid, largepre) -> 
         (Big_int.big_int_of_int docid, large_preorder_to_big_int largepre)
