(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_nodeid.mli,v 1.7 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Galax_nodeid
   Description:
     This module implements specifics of the Galax node ids.
*)

open Nodeid


(* Galax implementation id *)

val galax_implemid : implemid

(* Galax node id's are always Nodeid.PreInt *)

type galax_nodeid = prepostint_docorder


(* Doc id generator for Galax *)

val new_docid : unit -> docid


(* Galax docorder generation *)

type galax_nodeid_gen

val build_galax_nodeid_gen           : Nodeid_context.nodeid_context -> galax_nodeid_gen
val build_galax_nodeid_gen_for_docid : Nodeid_context.nodeid_context -> docid -> galax_nodeid_gen

val new_galax_pre    	  : galax_nodeid_gen -> preorder

val new_galax_nodeid      : galax_nodeid_gen -> preorder -> postorder -> galax_nodeid

val new_galax_leaf_nodeid : galax_nodeid_gen -> galax_nodeid

