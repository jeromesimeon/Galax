(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_update.ml,v 1.15 2007/08/23 21:01:32 simeon Exp $ *)

(* Module: Cs_code_update
   Description:
     This module contains the evaluation code for XML Query update
     operations. Additionally it contains the implementation of snaps
     and deltas.
*)

open Cs_util_coercion
open Datatypes

open Xquery_ast
open Xquery_common_ast
open Xquery_core_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util

open Dm_types
open Dm_util
open Physical_value
open Physical_item
open Physical_sequence

open Execution_context

open Physical_value_util
open Physical_util
open Update_ordering


(******************)
(* Some utilities *)
(******************)

(* Get first child *)

let get_last_child parent_node =
  try
    let cursor = parent_node#children None in
    let first = ref (Cursor.cursor_next cursor) in
    begin
      (try
	while true do
	  let next = Cursor.cursor_next cursor in
	  first := next
	done;
      with
      | _ -> ());
      Some (!first)
    end
  with
  | _ ->
      None

(* Get next sibling *)

let get_next_sibling parent_node afterNode =
  try
    let cursor = parent_node#children None in
    let _ = Cursor.cursor_find (fun x -> node_equal x afterNode) cursor in
    Some (Cursor.cursor_next cursor)
  with
  | _ ->
      None

(* Filtering attributes from values *)

let rec filterValue_aux il =
  match il with
  | [] -> ([], [])
  | i :: il' -> 
      begin 
	match (item_kind i) with
	| NodeKind ->
	    let (fn,fv) = filterValue_aux il' in
	    ((getNode i) :: fn, fv)
	| AtomicValueKind -> 
	    let (fn,fv) = filterValue_aux il' in
	    (fn, (getAtomicValue i) :: fv)
      end

and filterValue il =
  let (ns, avs) = filterValue_aux il
  in (materialized_of_list ns, materialized_of_list avs)


let rec filterAttributesFromValue_aux il =
  match il with
  | [] -> ([], [])
  | i :: il' ->
      begin
	match item_kind i with
	| NodeKind -> 
	    begin 
	      let x = getNode i in 
	      match x#node_kind() with
	      |	AttributeNodeKind -> 
		  let (atts,others) = filterAttributesFromValue_aux il' in
		  ((x#getAttributeNode()) :: atts, others)
	      |	_ ->
		  let (atts,others) = filterAttributesFromValue_aux il' in
		  if (not(atts = [])) then 
		    raise (Error.Query(Error.Type_Error(("Attributes must precede nodes in element content."))))
		  else
		    (atts, i :: others)
	    end
	| AtomicValueKind ->
	    begin
	      let (atts,others) = filterAttributesFromValue_aux il' in
	      if (not(atts = [])) then 
		raise (Error.Query(Error.Type_Error(("Attributes must precede atomic values in element content."))))
	      else
		(atts, i :: others)
	    end
      end

and filterAttributesFromValue il = 
  let (ats, v) = filterAttributesFromValue_aux il in
  (materialized_of_list ats, materialized_of_list v)


(* Auxilliary functions for replace *)

(* A ContentSequence is any sequence of zero or more element, PI,
comment nodes and atomic values - Gargi *)

let cstest n = 
  if (isElementNode n || isProcessingInstructionNode n || isCommentNode n)
  then true
  else false
      
let rec checkContentSequence node_list = 
  match node_list with 
  | [] -> true
  | n :: node_list' ->
	cstest n && checkContentSequence node_list'

let isContentSequence n = 
  let (nodes, avs) = filterValue n in
  checkContentSequence (list_of_sequence nodes)

let check_content_sequence node n =
  match node#node_kind() with
  | DocumentNodeKind -> ()
  | ElementNodeKind
  | TextNodeKind
  | ProcessingInstructionNodeKind
  | CommentNodeKind ->
      if (isContentSequence n)
      then ()
      else raise (Error.Query (Error.Update_Error ("Replace not over a proper content")))
  | AttributeNodeKind ->
      let (attrs, nodes) = filterAttributesFromValue n in
      if (sequence_is_empty nodes) 
      then ()
      else raise (Error.Query (Error.Update_Error ("Attribute node can only be replaced with other attribute nodes")))

let check_content_atomic_sequence node new_content =
  let (nodes, avs) = filterValue new_content in 
  if (sequence_is_empty nodes)
  then ()
  else raise (Error.Query (Error.Update_Error ("Content in [replace value of] not a sequence of atomic values")))

let check_insert_target_node t =
  match t with
  | [] -> raise (Error.Query (Error.Update_Error ("Insert target is empty")))
  | [pn] -> (* to check whether the TargetLocation is a single node *)
      begin
	match item_kind pn with
	| NodeKind -> 
	    getNode pn
	| AtomicValueKind ->
	    raise (Error.Query (Error.Update_Error ("Insert target is not a node")))
      end
  | _ ->
      raise (Error.Query (Error.Update_Error ("Insert target is not a single node")))

let rec expand_doc_nodes (source_list: Dm.node list) = 
  List.rev (
      List.fold_left 
        (fun res nd -> 
	      match nd#node_kind() with
	        | DocumentNodeKind -> List.rev_append res (Cursor.list_of_cursor "Code_update.expand_doc_nodes" (nd#children None))
            | _ -> nd::res
        )
        [] source_list
    )

let rec expand_doc_nodes_in_item_list (source_list: Physical_value.item list) = 
  List.rev (
      List.fold_left 
        (fun res itm -> 
          match itm with
            | Item_Node nd ->
                begin
	              match nd#node_kind() with
	                | DocumentNodeKind -> 
                        List.rev_append res 
                          (List.map (fun n -> Item_Node n) 
                              (Cursor.list_of_cursor "Code_update.expand_doc_nodes_in_item_list" 
                                  (nd#children None)))
                    | _ -> itm::res
                end
            | _ -> itm::res
        )
        [] source_list
    )


let get_parent target_node =
  match (target_node#parent None) with
  | Some p -> p
  | None ->
      raise (Error.Query (Error.Update_Error ("The target node does not have a parent")))


(***************)
(* Delete code *)
(***************)

let eval_single_delete node =
  let node = get_node node in
  let parent_node = get_parent node in
    Delete (parent_node, node)

let eval_delete v1 =
  List.map eval_single_delete v1


(***************)
(* Detach code *)
(***************)

(* let eval_single_detach node = *)
(*   let node = get_node node in *)
(*   let parent_node = get_parent node in *)
(*     Detach(parent_node, node) *)

(* let eval_detach v1 = *)
(*   List.map eval_single_detach v1 *)


(***************)
(* Rename code *)
(***************)

let check_rename_target_node t =
  match t with
  | [] -> raise (Error.Query (Error.Update_Error ("Rename target is empty")))
  | [pn] -> (* to check whether the Target is a single node *)
      begin
	match item_kind pn with
	| NodeKind -> 
	    getNode pn
	| AtomicValueKind ->
	    raise (Error.Query (Error.Update_Error ("Rename target is not a node")))
      end
  | _ ->
      raise (Error.Query (Error.Update_Error ("Rename target is not a single node")))

let check_rename_newname nsenv t = 
  let pv1 = Cursor.cursor_of_list t in
  Code_util.get_computed_node_name nsenv pv1

let eval_rename nsenv target name = 
  let tn = check_rename_target_node target in
  let newn = check_rename_newname nsenv name in
  Rename (tn, newn)
      
(***************)
(* Insert code *)
(***************)

let insert_into_location par =
  let parent_node = check_insert_target_node par in
  (parent_node,None)

let insert_lastinto_location par =
  let parent_node = check_insert_target_node par in
  (parent_node,get_last_child parent_node)

let insert_before_location insert_loc = 
  let insert_location = check_insert_target_node insert_loc in
  let parent_node = get_parent insert_location in
  (parent_node, Some insert_location)

let insert_after_location insert_loc =
  let after_node = check_insert_target_node insert_loc in
  let parent_node = get_parent after_node in
  let insert_location = get_next_sibling parent_node after_node in
  (parent_node, insert_location)

(* Function to evaluate a single concrete update *)

let eval_single_insert parent_node update_content sibling_node =
  Insert (parent_node, sibling_node, update_content)

  

let eval_insert insert_location_flag raw_content raw_parent =
  let update_content = expand_doc_nodes( get_node_list_of_item_list raw_content ) in
(*   begin   (\* What should we do with attributes?? *\) *)
(*     let (attrs, nodes) = filterAttributesFromValue raw_content in  *)
(*     let attrs_list = list_of_sequence attrs in *)
(*     match attrs_list with  *)
(*     | [] -> () *)
(*     | _ -> *)
(* 	raise (Error.Query (Error.Update_Error ("Insert content cannot have attributes"))) *)
(*   end; *)
  let (parent_node,insert_location) =
    match insert_location_flag with
    | AOUInto ->
	insert_into_location raw_parent
    | AOUAsLastInto -> 
	insert_lastinto_location raw_parent
    | AOUAsFirstInto ->
	insert_into_location raw_parent
    | AOUBefore ->
	insert_before_location raw_parent
    | AOUAfter ->
	insert_after_location raw_parent
  in
  eval_single_insert parent_node update_content insert_location


(****************)
(* Replace code *)
(****************)

let eval_concrete_replace parent_node target_node replace_content = 
  Replace (parent_node, target_node, replace_content)

	
let eval_concrete_replace_value target_node replace_content =
  ReplaceValue (target_node, replace_content) 


let check_replace_value_with_node t =
  match t with
  | [] -> raise (Error.Query (Error.Update_Error ("Replace value content is empty")))
  | [pn] -> (* to check whether the Target is a single node *)
      begin
	match item_kind pn with
	| NodeKind -> 
	    (getNode pn)#getTextNode()          
	| AtomicValueKind ->
	    raise (Error.Query (Error.Update_Error ("Replace value content cannot be treated as the content of a text node constructor")))
      end
  | _ ->
	  raise (Error.Query (Error.Update_Error ("Replace value content cannot be treated as the content of a text node constructor")))


let eval_replace vol target_node new_content =
  let target_node = check_insert_target_node target_node in
  match vol with 
  | Normal_Replace ->
      let parent_node = get_parent target_node in 
      let new_content' = expand_doc_nodes_in_item_list new_content in
      let replace_content =
	    check_content_sequence target_node new_content';
	    get_node_list_of_item_list new_content'
      in
        eval_concrete_replace parent_node target_node replace_content
  | Value_Of_Replace ->
      let replace_content =
        check_replace_value_with_node new_content
          (* 	    check_content_atomic_sequence target_node new_content; *)
          (* 	    get_atomic_list_of_item_list new_content  *)
      in
        eval_concrete_replace_value target_node replace_content


(**************)
(* Delta Code *)
(**************)
type delta_stack = Update_ordering.delta list ref Stack.t
(* 'global' stack *)
let (snap_stack:delta_stack)   = Stack.create ()

let enter_unordered_snap () =
  (* Format.printf "Enter snap: (depth: %d)@." (Stack.length snap_stack); *)
  let (empty_delta_list:delta list ref) = ref [] in 
    Stack.push empty_delta_list snap_stack

let exit_unordered_snap ()    = 
  try
    (* Format.printf "Exiting snap: (depth: %d)@." (Stack.length snap_stack); *)
    List.rev  !(Stack.pop snap_stack)
  with Stack.Empty ->
    raise (Error.Query (Error.Update_Error "Attempt to leave snap which was never opened!"))

let add_delta alg_ctxt delta = 
  match get_current_snap_semantic alg_ctxt with
    | Snap_Nondeterministic
    | Snap_Unordered_Deterministic   -> 
	begin
	  try 
	    let dl = Stack.top snap_stack in 
	      dl := !dl @ delta
	  with Stack.Empty ->
	    raise (Error.Query (Error.Update_Error "Attempting to add delta updates to non-existent delta"))
	end
    | Snap_Ordered_Deterministic -> 
	let uh = Execution_context.get_ordering_structure alg_ctxt in 
	  List.iter (Update_ordering.add_concrete_update uh) delta


let add_delta_single alg_ctxt delta =
  match get_current_snap_semantic alg_ctxt with
    | Snap_Nondeterministic
    | Snap_Unordered_Deterministic   -> 
	begin
	  try 
	    let dl = Stack.top snap_stack in 
	      dl := !dl @ [delta]
	  with Stack.Empty ->
	    raise (Error.Query (Error.Update_Error "Attempting to add delta updates to non-existent delta"))
	end
    | Snap_Ordered_Deterministic -> 
	let uh = Execution_context.get_ordering_structure alg_ctxt in 
	  Update_ordering.add_concrete_update uh delta


(********************************)
(* Evaluation of Simple Updates *)
(********************************)

(* Actual code wrappers *)
let empty_seq = []
let build_default_delete_code code_ctxt =
  (fun alg_ctxt ae1 ->
    add_delta alg_ctxt (eval_delete ae1);
    empty_seq)

let build_default_insert_code code_ctxt insert_location_flag =
  (fun alg_ctxt ae1 ae2 ->
    add_delta_single alg_ctxt (eval_insert insert_location_flag ae1 ae2);
    empty_seq)

let build_default_rename_code code_ctxt nsenv =
  (fun alg_ctxt ae1 ae2 ->
    add_delta_single alg_ctxt (eval_rename nsenv ae1 ae2);
    empty_seq)

let build_default_replace_code code_ctxt value_of_flag =
  (fun alg_ctxt ae1 ae2 ->
    add_delta_single alg_ctxt (eval_replace value_of_flag ae1 ae2);
    empty_seq)

(*****************)
(* Apply a delta *)
(*****************)
let apply_delta delta = 
  match delta with 
  | Delete (parent_node, node) ->
      parent_node#delete node
  | Insert (parent_node, Some sibling_node, update_content) ->
      parent_node#insert (Cursor.cursor_of_list update_content) sibling_node
  | Insert (parent_node, None, update_content) ->
      parent_node#insert_first (Cursor.cursor_of_list update_content)
  | Rename (target_node, newName) ->
      target_node#rename newName
  | ReplaceValue (target_node, replace_content) ->
      target_node#replace_value replace_content
  | Replace (parent_node,target_node, replace_content ) ->
      parent_node#replace (Cursor.cursor_of_list replace_content) target_node

(*************)
(* Snap Code *)
(*************)

let is_delete_delta v = 
  match v with 
  | Delete _  -> true
  | _ -> false

let is_insert_delta v = 
  match v with
  | Insert _ -> true
  | _ -> false

let is_rename_delta v = 
  match v with
  | Rename _ -> true
  | _ -> false

let is_replacenode_delta v = 
  match v with
  | Replace _ -> true
  | _ -> false

let is_replacevalue_delta v = 
  match v with
  | ReplaceValue _ -> true
  | _ -> false


(*************************************)	  
(* move all deletes to the last spot *)
(*************************************)
let delete_insert_conflict_resolve deltas = 
  let deletes, non_deletes = List.partition is_delete_delta deltas in 
  non_deletes @ deletes

module Nodeid_Module = struct
  type t = Nodeid.nodeid
  let compare = compare
end

module Nodeid_Set = Set.Make(Nodeid_Module)

(* Does not change the order just, checks for insert_insert conflicts *)
let check_insert_insert_conflicts deltas =
  let parent_set  = Nodeid_Set.empty in 
  let sibling_set = Nodeid_Set.empty in 
  let unwrap_inserts i = 
    match i with
      | Insert (parent,sibling_opt,_) -> (parent,sibling_opt)
      | _ -> raise (Error.Query (Error.Update_Error ("Unwrapping Inserts - partition should prevent this!")))
  in
  let rec check_conflicts_fold (t,parent_set,sibling_set) (parent,sibling_opt) =
    let check_and_add_node set node =
      let nodeid = node#nodeid () in 
	if Nodeid_Set.mem nodeid set
	then (true, set)
	else (false, (Nodeid_Set.add nodeid set))
    in
      if t 
      then (t,parent_set, sibling_set)
      else
	match sibling_opt with
	  | None ->
	      let (t, parent_set) = check_and_add_node parent_set parent in 		  
		(t,parent_set, sibling_set)		  

	  | Some node -> 
	      let (t, sibling_set) = check_and_add_node sibling_set node in 		  
		(t,parent_set, sibling_set)		  	      
  in
  let inserts, _     = List.partition is_insert_delta deltas in 
  let unwrapped      = List.map unwrap_inserts inserts in 
  let conflict, _, _ = List.fold_left check_conflicts_fold (false, parent_set, sibling_set) unwrapped  in 
    if conflict
    then raise (Error.Query (Error.Update_Error ("Insert-Insert Error")))
    else deltas
      
let rec check_target_conflicts_fold (t,target_set) node =
  if t 
  then (t,target_set)
  else 
    let nodeid = node#nodeid () in 
	  if Nodeid_Set.mem nodeid target_set
	  then (true, target_set)
	  else (false, (Nodeid_Set.add nodeid target_set))           	      
        
let check_rename_rename_conflicts deltas =
  let unwrap_renames i = 
    match i with
      | Rename (target,_) -> target
      | _ -> raise (Error.Query (Error.Update_Error ("Unwrapping Renames - partition should prevent this!")))
  in
  let renames, _     = List.partition is_rename_delta deltas in 
  let unwrapped      = List.map unwrap_renames renames in 
  let conflict, _ = List.fold_left check_target_conflicts_fold (false, Nodeid_Set.empty) unwrapped  in 
    if conflict
    then raise (Error.Query (Error.Update_Error ("Rename-Rename Error")))
    else deltas

let check_replacenode_replacenode_conflicts deltas =
  let unwrap_replaces i = 
    match i with
      | Replace (_,target,_) -> target
      | _ -> raise (Error.Query (Error.Update_Error ("Unwrapping Replaces - partition should prevent this!")))
  in
  let replaces, _     = List.partition is_replacenode_delta deltas in 
  let unwrapped      = List.map unwrap_replaces replaces in 
  let conflict, _ = List.fold_left check_target_conflicts_fold (false, Nodeid_Set.empty) unwrapped  in 
    if conflict
    then raise (Error.Query (Error.Update_Error ("Replace-Replace Error")))
    else deltas

let check_replacevalue_replacevalue_conflicts deltas =
  let unwrap_replaces i = 
    match i with
      | ReplaceValue (target,_) -> target
      | _ -> raise (Error.Query (Error.Update_Error ("Unwrapping Replaces - partition should prevent this!")))
  in
  let replaces, _     = List.partition is_replacevalue_delta deltas in 
  let unwrapped      = List.map unwrap_replaces replaces in 
  let conflict, _ = List.fold_left check_target_conflicts_fold (false, Nodeid_Set.empty) unwrapped  in 
    if conflict
    then raise (Error.Query (Error.Update_Error ("ReplaceValue-ReplaceValue Error")))
    else deltas
      
(* check that:
  - 2 renames don't have the same target
  - 2 replace-nodes don't have the same target
  - 2 replace-values don't have the same target
*)
let check_conflicts deltas = 
(*   check_insert_insert_conflicts deltas *)
  ignore(check_rename_rename_conflicts deltas);
  ignore(check_replacenode_replacenode_conflicts deltas);
  check_replacevalue_replacevalue_conflicts deltas
    
let build_default_snap_code code_ctxt snap_modifier  =
  (fun dep eval alg_ctxt () ->
     (* In the unordered cases, we maintain a stack of updates
	locally, rather than in the evaluation context *)
     let () = 
       match snap_modifier with
	 | Snap_Unordered_Deterministic -> enter_unordered_snap ()
	 | Snap_Nondeterministic -> enter_unordered_snap ()	     
	 | Snap_Ordered_Deterministic -> ()
     in

     (* Set the semantic of the snap and whatever setup is needed, for
	example Ordered Deterministic requires some state to be setup.
     *)
     let alg_ctxt    = Execution_context.enter_snap alg_ctxt snap_modifier in  

     (* Materialize the result *)
     let ret = item_list_of_physical_value  
		 (eval alg_ctxt dep) in 
     (* Apply the deltas *)
     let deltas = 
       match snap_modifier with
	 | Snap_Nondeterministic -> 
	     let deltas = exit_unordered_snap () in
	     delete_insert_conflict_resolve deltas
	 | Snap_Unordered_Deterministic   -> 
	     let deltas = exit_unordered_snap () in
	       delete_insert_conflict_resolve deltas
	 | Snap_Ordered_Deterministic ->
	     Update_ordering.collect_all_updates 
	       (Execution_context.get_ordering_structure alg_ctxt)
     in
     ignore(check_conflicts deltas);
     (* Apply deltas, here we have already resolved all potential delta conflicts *)
     List.iter apply_delta deltas;
     ret
  )

let build_default_copy_code code_ctxt =
  (fun alg_ctxt ae1 -> ae1)

let build_copy_code code_ctxt algop = 
  let fn = build_default_copy_code code_ctxt in
  let _ = access_nosub algop.pdep_sub_expression in
  (coerce_nodep fn coerce_unary_sax_to_sax), code_ctxt

let build_delete_code code_ctxt algop = 
  let fn = build_default_delete_code code_ctxt in
  let _ = access_nosub algop.pdep_sub_expression in
  (coerce_nodep fn coerce_unary_item_list_to_item_list), code_ctxt

let build_insert_code code_ctxt algop insert_location =
  let fn = build_default_insert_code code_ctxt insert_location in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_binary_item_list_to_item_list), code_ctxt

let build_rename_code code_ctxt algop nsenv =
  let fn = build_default_rename_code code_ctxt nsenv in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_binary_item_list_to_item_list), code_ctxt

let build_replace_code code_ctxt algop value_flag =
  let fn = build_default_replace_code code_ctxt value_flag in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_binary_item_list_to_item_list), code_ctxt

let build_snap_code code_ctxt algop sm =
  let fn  = build_default_snap_code code_ctxt sm in
  let dep = access_onesub algop.pdep_sub_expression in
    (coerce_onedep fn dep coerce_unit_to_item_list), code_ctxt







