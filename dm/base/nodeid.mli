(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: nodeid.mli,v 1.4 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Nodeid
   Description:
     This module implements the node-id and document order support
     that must be common to all implementations connected to Galax. It
     also implements the main nodeid type which is used within the
     Galax query processor.
*)

(*************************)
(* Generic id structures *)
(*************************)

(* Common components *)

type implemid  = int
type docid     = int

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


val get_implemid  : docorder -> implemid          (* Get the implementation id *)
val get_docid     : docorder -> docid             (* Get the document id *)
val get_partialid : docorder -> partial_docorder  (* Get the node-id internal *)


(******************************)
(* Node comparison operations *)
(******************************)

val nodeid_is         : nodeid -> nodeid -> bool      (* node equality *)

val docorder_precedes : docorder -> docorder -> bool  (* preceding *)
val docorder_follows  : docorder -> docorder -> bool  (* following *)
val docorder_compare  : docorder -> docorder -> int   (* Document order compare function *)

(* XPath semantics of foll/prec/anc/desc *)
val is_descendant_xpath : docorder -> docorder -> bool
val is_ancestor_xpath   : docorder -> docorder -> bool
val is_preceding_xpath  : docorder -> docorder -> bool
val is_following_xpath  : docorder -> docorder -> bool


(************)
(* Printing *)
(************)

val string_of_docorder : docorder -> string
val string_of_nodeid   : nodeid -> string


(*************************************)
(* Generates a new implementation id *)
(*************************************)

val new_implemid  : string -> implemid
    (* The input string should be a short-name for the implementation *)


(*****************************)
(* Generic doc id generation *)
(*****************************)

type docid_gen

val build_docid_gen : unit -> docid_gen

val new_docid : docid_gen -> docid

(* Build node id from a pre/post pair *)

val build_docorder_from_pre_post : implemid -> docid -> (preorder * postorder) -> docorder
val merge_docorder_from_pre_post : preint_docorder -> postint_docorder -> prepostint_docorder

(* Special node ids *)

val min_preorder  : preorder
val min_postorder : postorder

val max_preorder  : preorder
val max_postorder : postorder

val large_preorder_to_big_int : large_preorder -> Big_int.big_int 
val big_int_to_large_preorder : Big_int.big_int -> large_preorder

val big_int_pair_of_docorder : docorder -> (Big_int.big_int * Big_int.big_int)
