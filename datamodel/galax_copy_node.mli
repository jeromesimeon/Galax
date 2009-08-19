(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_copy_node.mli,v 1.3 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Galax_copy_node
   Description:
     This module implements deep copy of Galax nodes.
*)

(*************)
(* Deep copy *)
(*************)

val deep_copy : Nodeid_context.nodeid_context -> Nodeid.docid -> Dm.node Cursor.cursor -> Dm.node list

