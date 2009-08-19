(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_load_context.mli,v 1.12 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Load_context
   Description:
     This module implements a the loading context.
*)


(**********************************)
(* A type for the loading context *)
(**********************************)

type load_context


(*********************************)
(* Creates a new loading context *)
(*********************************)

val build_load_context : 
    Physical_name_index.name_indices_handler -> 
	load_context

val add_element_to_name_index :
    load_context -> Namespace_symbols.relem_symbol -> Dm.node -> Nodeid.prepostint_docorder -> unit

val add_pre_order_to_name_index :
    load_context -> Namespace_symbols.relem_symbol -> int -> unit

val add_post_order_to_name_index :
    load_context -> Namespace_symbols.relem_symbol -> Dm.node -> int -> int -> unit

(********************************)
(* Accesses the loading context *)
(********************************)

val set_postorder   : load_context -> Nodeid.postint_docorder -> unit
val unset_postorder : load_context -> unit

val get_postorder   : load_context -> Nodeid.postint_docorder

val push_non_labeled_event : load_context -> unit
val push_labeled_event     : load_context -> unit

val pop_event              : load_context -> Dm.node -> unit
val get_result             : load_context -> Dm.node list

