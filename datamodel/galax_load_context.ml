(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_load_context.ml,v 1.16 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Load_context
   Description:
     This module implements a the loading context.
*)

open Error
open Datatypes


(* A type for the loading context *)


(* Note:
     The nodes resulting from loading are all the labelled nodes.

     The labeled_node_refs are used to keep a pointer to the node
     which will have to be returned eventually. Those have to be added
     in the list in pre-order, but we only obtain the node in
     post-order. So the stack keeps track of the pointers in pre-order
     so we can associate the node in post-order... sigh.
   - Philippe & Jerome *)

type labeled_node_ref = Dm.node option ref

(* Note:
     The the reason we need the stack to contain both labeled and
     non-labeled events is because the closing events do not have the
     labels. so this is the only way to know if the closing event
     corresponds to an opening event which had a label.
   - Philippe & Jerome *)


type labeled_event =
  | NotLabeled
  | Labeled of labeled_node_ref

type load_context =
    { mutable load_postorder : Nodeid.postint_docorder option;
      load_stack             : labeled_event Stack.t;
      mutable load_result    : labeled_node_ref list;
      mutable load_indices   : Physical_name_index.name_indices_hash;
      name_index_stack       : (int * int) Stack.t }

(* Index access *)

let add_element_to_name_index load_context relem_sym new_elem_node (docid,pre,post) =
  match Physical_name_index.get_opt_name_index load_context.load_indices relem_sym with
  | Some btree_handle ->
      (* add this node to the index *)
      Physical_name_index.add_item_to_name_index btree_handle new_elem_node (docid,pre,post)
  | None -> ()


let add_pre_order_to_name_index load_context relem_sym pre =
  match Physical_name_index.get_opt_name_index load_context.load_indices relem_sym with
  | Some btree_handle ->
      begin
	let rid = Physical_name_index.add_pre_order_item_to_name_index btree_handle pre in
	Stack.push (pre,rid) load_context.name_index_stack 
      end
  | None -> ()

let add_post_order_to_name_index load_context relem_sym new_elem_node pre post =
  match Physical_name_index.get_opt_name_index load_context.load_indices relem_sym with
  | Some btree_handle ->
      (* add this node to the index *)
      let (pre_verify, rid) = Stack.pop load_context.name_index_stack in
      if pre_verify = pre then
	ignore(Physical_name_index.add_post_order_item_to_name_index btree_handle rid new_elem_node post)
      else
	raise (Query (Internal_Error "Name_index stack misconfiguration. Pre on stack does not match current post"))
  | None -> ()

(* Creates a new loading context *)

let build_load_context name_indices =
  let n_indices = Physical_name_index.build_name_indices_hash name_indices in
  { load_postorder = None;
    load_stack     = Stack.create ();
    load_result    = [];
    load_indices   = n_indices;
    name_index_stack = Stack.create () }


(* Accesses the loading context *)

let set_postorder load_context postorder =
  load_context.load_postorder <- (Some postorder)

let unset_postorder load_context =
  load_context.load_postorder <- None

let get_postorder load_context =
  match load_context.load_postorder with
  | None -> raise (Query (Internal_Error "Accessing unset post-order during loading"))
  | Some postorder ->
      unset_postorder load_context ; postorder

let push_non_labeled_event load_context =
  Stack.push NotLabeled load_context.load_stack

let push_labeled_event load_context =
  let labeled_node_ref = ref None in
  Stack.push (Labeled labeled_node_ref) load_context.load_stack;
  load_context.load_result <- labeled_node_ref :: load_context.load_result

let pop_event load_context node =
  match Stack.pop load_context.load_stack with
  | NotLabeled -> ()
  | Labeled labeled_node_ref ->
      labeled_node_ref := Some node

let acess_node_result x =
  match !x with
  | None -> raise (Query (Internal_Error "Accessing empty result node ref after loading"))
  | Some node ->
      node

let get_result load_context =
  let result = List.rev_map acess_node_result load_context.load_result in
  begin
    Stack.clear load_context.load_stack;
    load_context.load_result <- [];
    result
  end

