(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_load_context.ml,v 1.7 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Shredded_load_context
   Description:
     This module implements a the loading context for the shredded.
*)

open Namespace_names
open Datatypes
(* open Shredded_store *)
exception Shredded_load_context of string

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

(* A type for the loading Shredded context *)
module  Shredded_Load_Context_Functor
  (Shredded_store : Shredded_load_sigs.Shredded_Load_Store) = struct
    
    type parent_record = {
      current_id              : Nodeid.nodeid;
      mutable stored_children : Nodeid.nodeid list;
    }
    let mk_parent_record id children = {
      current_id = id;
      stored_children = children
    } 
    type generator = 
      | Invalid (* Always return an invalid preorder, for updates *)
      | Valid of Shredded_renumber.Cell_As_Int64.Generator.handle
      
    type shredded_load_context =
	{ shredded_store_handle	: Shredded_store.shredded_store;
	  parent_stack          : parent_record Stack.t;
	  namespace_id		: Shredded_store.namespaceid	Stack.t; (* Stores the namespace environment *)
	  namespace_env		: Namespace_context.nsenv	Stack.t; (* Stores the namespace environment *)

	  (* This is <> None if we are updating *)
	  (* Also this is in reverse order *)
	  mutable updated_nodeid_list   : Nodeid.nodeid list option;
	  preorder_generator            : generator;

	  shredded_load_base_uri: Dm_atomic.atomicAnyURI Stack.t }


    (* Creates a new loading context *)

    let build_load_context nodeid_context directory name gen buff_size =
      { shredded_store_handle  = Shredded_store.create_shredded_store nodeid_context directory name buff_size;
	parent_stack           = Stack.create ();
	namespace_id	       = Stack.create ();
	namespace_env	       = Stack.create ();
	preorder_generator     = Valid gen;
	updated_nodeid_list    = None;
	shredded_load_base_uri = Stack.create ()}

    let build_load_update_context store  =
      { shredded_store_handle  = store;
	parent_stack           = Stack.create ();
	namespace_id	       = Stack.create ();
	namespace_env	       = Stack.create ();
	preorder_generator     = Invalid;
	updated_nodeid_list    = Some [];
	shredded_load_base_uri = Stack.create ()}

    (******************************************************************************)
    (*								New Functions     *)
    (******************************************************************************)

    let get_shredded_store slc = slc.shredded_store_handle

    let get_parent_stack slc   = slc.stored_children
    let get_current_parent slc = (Stack.top slc.parent_stack).current_id

    let new_preorder slc       = 
      match slc.preorder_generator with
	| Invalid -> Shredded_renumber.Cell_As_Int64.invalid_preorder
	| Valid gen -> Shredded_renumber.Cell_As_Int64.Generator.next gen


    let get_nodeids_rev slc = 
      match slc.updated_nodeid_list with
	| None -> (raise (Shredded_load_context "Getting updated node list during Loading"))
	| Some l -> l 

    let get_nodeids slc = List.rev (get_nodeids_rev slc)


    let become_next_parent slc id = 
      Stack.push (mk_parent_record id []) slc.parent_stack 

    let no_longer_parent slc     = ignore(Stack.pop slc.parent_stack)

    let update_current_parent_with_child slc child = 
      let current_parent   = Stack.top slc.parent_stack in
      let current_children = current_parent.stored_children in 
      let () = 
	match slc.updated_nodeid_list with
	  | None -> ()
	  | Some unl -> slc.updated_nodeid_list <- Some (child :: unl)
      in
	current_parent.stored_children <- child :: current_children

    let update_attributes slc attrs = 
      let attrs            = List.rev attrs in (* Just to be consistent *)
      let current_parent   = Stack.top slc.parent_stack in
      let _ = current_parent.stored_children in 
      match slc.updated_nodeid_list with
      | None -> ()
      | Some unl -> slc.updated_nodeid_list <- Some (attrs @ unl)

    let get_current_children slc = 
      (Stack.top slc.parent_stack).stored_children

    let get_namespace_env slc = 
      Stack.top slc.namespace_env

    let get_namespace_id slc = 
      Stack.top slc.namespace_id

    let add_namespace_env slc nsid nsenv = 
      Stack.push nsenv slc.namespace_env;
      Stack.push nsid  slc.namespace_id

    let remove_namespace_env slc = 
      (* Question for Avinash:
	 Should the result of those pop really be discarded? - Jerome *)
      ignore(Stack.pop slc.namespace_env);
      ignore(Stack.pop slc.namespace_id);
      ()	

    (******************************************************************************)
    (*	Old Functions	    						      *)
    (******************************************************************************)

    (* Accesses the loading context *)

    let push_base_uri slc base_uri =
      Stack.push base_uri slc.shredded_load_base_uri

    let pop_base_uri slc =
      try
	ignore(Stack.pop slc.shredded_load_base_uri)
      with
	| Stack.Empty ->
	    ()

    let get_base_uri slc =
      try
	Some (Stack.top slc.shredded_load_base_uri)
      with
	| Stack.Empty ->
	    None

    let initial_namespaceid = Shredded_store.namespaceid_seed
  end
