(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_load_context.mli,v 1.6 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Shredded_load_context
   Description:
     This module implements a the loading context for the shredded.
*)

open Nodeid
(* open Shredded_store *)


(****************************************)
(* Creates a new shredded loading context *)
(****************************************)
module type Shredded_Load_Context_Functor_Sig = 
  functor (Shredded_store : Shredded_load_sigs.Shredded_Load_Store) ->
sig
(*****************************************)
(* A type for the shredded loading context *)
(*****************************************)

  type shredded_load_context

  val build_load_context : Nodeid_context.nodeid_context -> string -> string -> 
    Shredded_renumber.Cell_As_Int64.Generator.handle -> int -> shredded_load_context      

  val build_load_update_context : Shredded_store.shredded_store -> shredded_load_context      

  val get_shredded_store : shredded_load_context -> Shredded_store.shredded_store
  val get_current_parent   : shredded_load_context -> Nodeid.nodeid
    
  val new_preorder : shredded_load_context -> Nodeid.large_preorder

  val become_next_parent : shredded_load_context -> Nodeid.nodeid -> unit 
  val no_longer_parent   : shredded_load_context -> unit

  val get_nodeids : shredded_load_context -> Nodeid.nodeid list
    (* Faster *)
  val get_nodeids_rev : shredded_load_context -> Nodeid.nodeid list 

  val update_current_parent_with_child : shredded_load_context -> Nodeid.nodeid -> unit
  val get_current_children : shredded_load_context -> Nodeid.nodeid list 

  val get_namespace_id : shredded_load_context -> Shredded_store.namespaceid

  val get_namespace_env : shredded_load_context -> Namespace_context.nsenv 

  val add_namespace_env : shredded_load_context -> Shredded_store.namespaceid -> Namespace_context.nsenv -> unit

  val remove_namespace_env : shredded_load_context -> unit
    
  (******************************************************************************)
  (*	Old Functions							      *)
  (******************************************************************************)

  (* Note:
     What to do with base URI is still unclear. *)

  val push_base_uri  : shredded_load_context -> Dm_atomic.atomicAnyURI -> unit
  val pop_base_uri   : shredded_load_context -> unit

  val get_base_uri   : shredded_load_context -> Dm_atomic.atomicAnyURI option
  val initial_namespaceid : Shredded_store.namespaceid


  val update_attributes : shredded_load_context -> Nodeid.nodeid list -> unit
end

module Shredded_Load_Context_Functor : Shredded_Load_Context_Functor_Sig
