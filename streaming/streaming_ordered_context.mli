(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: streaming_ordered_context.mli,v 1.5 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Streaming_ordered_context
   Description:
     This module implements the context necessary to add identity
     information to an input typed stream.
*)


(********************************************)
(* A type for the streaming ordered context *)
(********************************************)

type streaming_ordered_context


(*******************************************)
(* Creates a new streaming ordered context *)
(*******************************************)

val build_streaming_ordered_context :
    Nodeid.docid -> Nodeid_context.nodeid_context -> streaming_ordered_context



(*******************************)
(* Operations on node identity *)
(*******************************)

val get_docid          : streaming_ordered_context -> Nodeid.docid

val new_preorderid     : streaming_ordered_context -> Nodeid.preint_docorder
val new_postorderid    : streaming_ordered_context -> Nodeid.postint_docorder
val new_leaf_docorder  : streaming_ordered_context -> Nodeid.prepostint_docorder

