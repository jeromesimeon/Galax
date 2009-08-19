(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_sc_join.ml,v 1.19 2007/05/23 22:12:59 simeon Exp $ *)

(* Module: Code_sc_join
   Description:
     This module contains code building for operators that implement
     StairCase Joins.
*)

open Error
open Dynamic_stack
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_common_ast

open Code_selection_context
open Execution_context

open Datatypes

open Dm_types
open Dm_atomic
open Dm
open Dm_util

open Physical_value
open Physical_item
open Physical_item_util
open Physical_sequence
open Physical_value_util
open Physical_table
open Physical_name_index

open Code_util
open Code_util_xpath

open Algebra_type

(*******************************************************)
(* ALGORITHM: Staircase Join                           *)
(* Accelerating XPath Evaluation in Any RDMSMatching   *)
(* Grust, Van Keulen, Teubner                          *)
(* ACM TODS, Vol 29 No 1, March 2004                   *) 
(*******************************************************)

(* Some error defs *)
let leaf_code_error = 
  (Query 
     (Code_Selection 
	"[Staircase Join] Attempt to generate code for leaf node."))
and node_test_error =
  (Query
     (Code_Selection 
	"[Staircase Join] No node test for non-root pattern node (None)."))
and out_field_error =
  (Query 
     (Code_Selection 
	"[Staircase Join] No output field for pattern node (None)."))
and not_supported_err =
  (Query 
     (Code_Selection 
	"[Staircase Join] Only name tests supported in SC-Join."))
and no_index_found_err =
  (Query 
     (Code_Selection 
	"[Staircase Join] No index found for node test."))


type compare_op = 
  | Less 
  | Greater 
  | Equal 

(* fetches the set of BTrees matching  the node test *)
let nameindex_of_node_test code_ctxt nt =
  match nt with
  | APNameTest symbol ->
      begin
	match Code_selection_context.get_name_index code_ctxt symbol with
	| Some index -> symbol, index
	| None -> raise no_index_found_err
      end
  | _ -> raise not_supported_err


let get_pre_post_for_item item =
  let n = getNode (Physical_util.get_item (cursor_of_sequence ( item ))) in
  match n#docorder() with
  | (_,Nodeid.PrePostInt(_,pre,post)) -> pre, post
  | _ -> raise (Query (Internal_Error "No full pre/post order description available (sc_join.get_pre_post_from_item)"))

let restore_fun restore_code item =
  let actual_item = Physical_util.get_item (cursor_of_sequence item) in
  let _ = restore_code (materialized_of_list [actual_item]) in
  empty_tuple

let eval_node_test_single_node stat_ctxt nt node =
  let c = eval_axis_node_test stat_ctxt Self nt node in
  not (Cursor.cursor_is_empty c) 

(* *** DESCENDANT *** DESCENDANT-OR-SELF ****************************)
(* Descendant can be evaluated best by identifying the index window *)
(* that contains all the descendants. A lookup in the name index    *)
(* will define the boundaries of this window, so that we can return *)
(* a cursor that scans the window.                                  *)
(* **************************************************************** *)
let sc_join_descendant code_ctxt input output nt include_self =
  let restore_code = build_create_dom_tuple_code code_ctxt output in
  let retrieve_code =  build_retrieve_dom_tuple_code code_ctxt input in 
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
 
  let item_fun index tu =
    let item = retrieve_code () in
    let pre, post = get_pre_post_for_item item in
    (* Note: following implements scanning AND skipping at a cost of log(|doc|) *)
    (* Note: for desc-or-self, include self here *)
    let curs = pre_cursor_of_name_index_from_window index pre post in
    let curs' =
      if include_self then
	let node =  getNode (Physical_util.get_item (cursor_of_sequence ( item ))) in
	if eval_node_test_single_node stat_ctxt nt node then
	  Cursor.cursor_append (Cursor.cursor_of_singleton item) curs
	else curs
      else curs
    in
    Cursor.cursor_map (restore_fun restore_code) curs'
  in

  fun () eval alg_ctxt input_cursor ->
    let sym, index = nameindex_of_node_test code_ctxt nt in
    let axis = Xquery_common_ast.Descendant in
    let prune_code =
      Code_prune.build_default_prune_code code_ctxt axis input
    in
    Cursor.cursor_map_concat (item_fun index) (prune_code alg_ctxt input_cursor)

(* *** CHILD ***************************************** *)
(* - This simply boils down to the pathstack algorithm *)
(*   for a single step                                 *)
(* *************************************************** *)
let get_min_source indices_curs () =
  let min = ref (-1) in
  let current = ref None in

  for i = 0 to (Array.length indices_curs) -1 do
    begin
      let next_item = Cursor.cursor_peek indices_curs.(i) in
       match next_item with
       | Some item ->
	   begin
	     let node = getNode (Physical_util.get_item (cursor_of_sequence ( item ))) in
	     match !current with
	     | Some node' -> 
		 if node_precedes node node' then
		   ignore(current := Some node; min := i)
	     | _ ->
		 ignore(current := Some node; min := i)
	   end
       | _ -> ()
     end
  done;
  if !min < 0 
  then None
  else Some !min

let get_parent_pre node =
  match node#parent None with
  | Some p -> 
      begin
	match p#docorder() with
	| (_,Nodeid.PrePostInt(_,pre,_)) ->  pre
	| _ -> -1
      end
  | None -> -1

let sc_join_child code_ctxt input output nt =
  let retrieve_code = build_retrieve_dom_tuple_code code_ctxt input in
  let restore_code = build_create_dom_tuple_code code_ctxt output in
  let dummy   = (sequence_empty(), (-1), (-1)) in

  fun () eval alg_ctxt curs  ->
    let stack  = Dynamic_stack.make 16  dummy in
    let sym, index = nameindex_of_node_test code_ctxt nt in
    let input_source = Code_util_tj.common_cursor_of_input_cursor curs retrieve_code in
    let win_pre, win_post = Code_util_tj.get_index_window input_source in
    let index_source =
      if win_pre < 0 || win_post < 0 then
	pre_cursor_of_name_index_at_pos index 1
      else
	pre_cursor_of_name_index_from_window index win_pre win_post
    in
    let sources = [| input_source; index_source |] in
    let source_cursor = Cursor.cursor_of_function (get_min_source sources) in
    

    let item_fun src =
      begin
	let item = Cursor.cursor_next sources.(src) in
	let node = getNode (Physical_util.get_item (cursor_of_sequence item)) in
	let pre, post = get_pre_post_for_item item in
	let _ = if Debug.default_debug() then 
	  let msg =
	    Format.sprintf "Next node from source %i is (%i;%i)\n" src pre post 
	  in
	  Debug.print_default_debug msg
	in
	(* clean stack *)
	let get_top_post s = let (_,_,post') = top s in post' in
	let _ =
	  while not(empty stack) && ((get_top_post stack) < post) do
	    ignore(pop stack)
	  done
	in
	
	if src == 0 then
	  let _ = push stack (item, pre, post) in
	  Cursor.cursor_empty ()
	else if not(empty stack) then
	  begin
	    (* parent-child test here *)
	    let _, pre', _  = top stack in
	    if (get_parent_pre node) = pre' then
	      Cursor.cursor_of_singleton 
		 (restore_fun restore_code item)
	    else
	      Cursor.cursor_empty ()
	  end
	else
	  (* if stack is empty, jump over desc nodes *)
	  let _ =
	    match Cursor.cursor_peek sources.(0) with
	    | Some item ->
		let jump_to, _ = get_pre_post_for_item item in
		let new_cursor =
		  if win_post < 0 
		  then pre_cursor_of_name_index_at_pos index (jump_to +1)
		  else pre_cursor_of_name_index_from_window index (jump_to +1) win_post
		in
		sources.(1) <- new_cursor 
	    | None -> ()
	  in
	  Cursor.cursor_empty ()
      end
    in
    Cursor.cursor_map_concat item_fun source_cursor


(* *** ANCESTOR *** ANCESTOR-OR-SELF *** PARENT ********************* *)
(* Not really SC-Join.                                                *)
(* By evaluating this axis nested-loop style, but with intermediate   *)
(* pruning, we obtain the best performance. Once again, pure SC join  *)
(* is inefficient due to our inability to directly access elements    *)
(* based on their pre/post numbers                                    *)
(* ****************************************************************** *)

let sc_join_ancestor code_ctxt input output nt include_self =
  let restore_code = build_create_dom_tuple_code code_ctxt output in
  let retrieve_code =  build_retrieve_dom_tuple_code code_ctxt input in  

  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let eval_node_test_fun = eval_axis_node_test stat_ctxt Self nt in
  
  let prev_pre  = ref (-1) in
  let prev_post = ref (-1) in
  let prev_ctxt = ref [] in

  let scan_partition curs item  =
    let pre, post = get_pre_post_for_item item in
    (* get all ancestors -- using the DOM-way  *)
    (* conditions: (1) match node test         *)
    (*             (2) remain within partition *)
    let rec get_parents item' = 
      let node = getNode (Physical_util.get_item (cursor_of_sequence ( item' ))) in
      match node#parent None with
      | Some p ->
	  let item_seq = LSeq [Item_Node p] in
	  let pre', post' = get_pre_post_for_item item_seq in
	  if pre' = !prev_pre && pre > !prev_pre && post < !prev_post 
             || pre' > !prev_pre
	  then
	    begin
	      let x = eval_node_test_fun p in
	      if  not(Cursor.cursor_is_empty x)
	      then 
		get_parents item_seq @ [item_seq]
	      else 
		get_parents item_seq
	    end
	  else []
      | None -> []
    in

    let _ =
      if Debug.default_debug() then
	let msg =
	begin
	  (Format.sprintf "Conext: (%i;%i), prev: (%i;%i)\n" pre post !prev_pre !prev_post) ^
	  (Format.sprintf "   pre > !prev_pre = %b\n"  (pre > !prev_pre)) ^
	  (Format.sprintf "   post > !prev_post = %b\n"  (post > !prev_post)) ^
	  (Format.sprintf "   Cursor.cursor_is_empty curs = %b\n"  (Cursor.cursor_is_empty curs))
	end
	in
	Debug.print_default_debug msg
    in

    let include_prev_context = pre > !prev_pre && post > !prev_post && include_self in
    let include_current_context = Cursor.cursor_is_empty curs && include_self in
    let ancs   = get_parents item in
    let ancs'  = 
      if include_prev_context && List.length !prev_ctxt = 1 then 
	let n = getNode (Physical_util.get_item (cursor_of_sequence ( List.hd !prev_ctxt ))) in
	if eval_node_test_single_node stat_ctxt nt n then !prev_ctxt @ ancs else ancs
      else 
	ancs 
    in
    let ancs'' = 
      let curnode = getNode (Physical_util.get_item (cursor_of_sequence ( item ))) in
      if include_current_context then 
	if eval_node_test_single_node stat_ctxt nt curnode then ancs' @ [item] else ancs'
      else 
	ancs' 
    in
    let _ = prev_ctxt := [item] in
    let _ = prev_pre := pre; prev_post := post in
    let anc_cursor = (Cursor.cursor_of_list ancs'') in
    Cursor.cursor_map  (fun x -> restore_fun restore_code x) anc_cursor
  in

  let item_fun curs tu =
    let item = retrieve_code () in
    scan_partition curs item
  in

  fun () eval alg_ctxt input_cursor ->
    (* let sym, index = nameindex_of_node_test code_ctxt nt in *)
    (* Note: pruning inlined in algo *)
    let _ = prev_pre := -1; prev_post := -1 in
    Cursor.cursor_map_concat (item_fun input_cursor) input_cursor

(* *** FOLLOWING *** ********************************************** *)
(* Priciple:                                                        *)
(*    - pruning should leave us exactly one context node            *)
(*      namely, the deepest descendant of the first context, or the *)
(*      first context itself if no descendant is in the list        *)
(*    - Next, we estimate the first follower of that node           *)
(*    - we scan the index until we reach the first actual follower  *)
(*    - everything from that point on is output                     *)
(* **************************************************************** *)
let rec fast_forward_to_foll curs pre post =
  match Cursor.cursor_peek curs with
  | Some item ->
      let pre', post' = get_pre_post_for_item item in
      if pre' > pre && post' > post then
	curs
      else
	let _ = Cursor.cursor_next curs in
	fast_forward_to_foll curs pre post
  | None -> curs

let sc_join_following code_ctxt input output nt =
  let restore_code = build_create_dom_tuple_code code_ctxt output in
  let retrieve_code =  build_retrieve_dom_tuple_code code_ctxt input in  

   fun () eval alg_ctxt input_cursor ->
    let sym, index = nameindex_of_node_test code_ctxt nt in

    let prev_pre = ref (-1) in
    let prev_post = ref (-1) in
    let past_descendant = ref false in

    let item_fun i =
      let item = retrieve_code () in
      let pre, post = get_pre_post_for_item item in

      let _ = if Debug.default_debug()
      then
	let msg =
	  Format.sprintf "- Cur ctxt: (%i;%i), prev (%i, %i)"
	    pre post !prev_pre !prev_post
	in
	Debug.print_default_debug msg
      in

      let out_curs =
	if Cursor.cursor_is_empty input_cursor && not(!past_descendant) then
(*let _ = Printf.printf "ctxt: (%i;%i): Empty input cursor, continuing at post\n" pre post  in*)
	  let curs = pre_cursor_of_name_index_at_pos index post  in
	  let _ = fast_forward_to_foll curs pre post in
	  Cursor.cursor_map (restore_fun restore_code) curs
	else if (pre > !prev_pre && post > !prev_post 
	    && !prev_pre > 0 && !prev_post > 0)
	then
	  begin
	    if not(!past_descendant) then
	      let _ = past_descendant := true in
	      let curs = pre_cursor_of_name_index_at_pos index !prev_post  in
	      let _ = fast_forward_to_foll curs !prev_pre !prev_post in
(*let _ = Printf.printf "ctxt: (%i;%i): Past deepest desc, continuing at post\n" pre post !prev_post  in*)
	      Cursor.cursor_map (restore_fun restore_code) curs
	    else
	      Cursor.cursor_empty ()
	  end
	else 
	  (* implicit pruning, skip to deepest decendant *)
	  Cursor.cursor_empty ()
      in
      let _ = prev_pre := pre; prev_post := post in
      out_curs
    in
    Cursor.cursor_map_concat item_fun input_cursor

(* *** PRECEDING *** ******************************************* *)
(* Modus operandus:                                              *)
(*  if a node a' is a preceding of a node b' and there exists    *)
(*  a node c' such that b' << c', then                           *)
(*  a' is also a preceding of c'                                 *)
(* Use this property to simultaniously scan the document from    *)
(* the starting position (0) and consume the context list        *)
(* ************************************************************* *)
let sc_join_preceding code_ctxt input output nt =
  let restore_code = build_create_dom_tuple_code code_ctxt output in
  let retrieve_code =  build_retrieve_dom_tuple_code code_ctxt input in  
  let prev_pre = ref (-1) in
  let prev_post = ref (-1) in

  let rec scan_reg sc pre post () =
    match Cursor.cursor_peek sc with
    | Some item ->
	let pre', post' = get_pre_post_for_item item in
	let _ =
	  if Debug.default_debug()
	  then
	    let msg =
	      Format.sprintf "*** PREC Scanning item (%i,%i)" pre' post'
	    in
	    Debug.print_default_debug msg
	in
	if pre' >= pre then
	  let _ =
	    if Debug.default_debug()
	    then
	      Debug.print_default_debug "*** PREC Reached context (stop)"
	  in
	  (* reached context item, continue with next one *)
	  None
	else if pre' < pre && post' < post then
	  let _ =
	    if Debug.default_debug()
	    then Debug.print_default_debug "*** PREC output"
	  in
	  let _ = Cursor.cursor_next sc in
	  Some (restore_fun restore_code item)
	else
	  let _ =
	    if Debug.default_debug()
	    then Debug.print_default_debug "*** PREC Ancestor (skip)"
	  in
	  let _ = (Cursor.cursor_next sc) in
	  scan_reg sc pre post ()
    | None -> None
  in
(*
  let item_fun in_curs scanner i = 
    let item = retrieve_code () in
    let pre, post = get_pre_post_for_item item in
   let _ =
   if Debug.default_debug() then Printf.printf 
	"PREC Current ctxt item: (%i;%i), previous (%i,%i)\n" 
	pre post !prev_pre !prev_post 
    in
    match Cursor.cursor_peek in_curs with
    | None ->
	let _ = if Debug.misc_debug() then 
	  Printf.printf "PREC END: Scanning to: (%i;%i)\n" pre post in
	Cursor.cursor_of_function (scan_reg scanner pre post)
    | _ ->
        (* implicit pruning *)
	if !prev_pre < 0 || pre < !prev_pre || post < !prev_post then
	  let _ = if Debug.misc_debug() then 
	    Printf.printf "PREC Skipping (%i;%i)\n"  !prev_pre !prev_post  in
	  let _ = prev_pre := pre in
	  let _ = prev_post := post in 
	  Cursor.cursor_empty ()
	else
	  let _ = if Debug.misc_debug() then 
	    Printf.printf "Scanning to: (%i;%i)\n" !prev_pre !prev_post in
	  let curs = Cursor.cursor_of_function (scan_reg scanner !prev_pre !prev_post) in      
	  let _ = prev_pre := pre in
	  let _ = prev_post := post in 
	  curs
   in
*)
  let item_fun in_curs scanner i =
    match Cursor.cursor_peek in_curs with
    | None ->
	let item = retrieve_code () in
	let pre, post = get_pre_post_for_item item in
	Cursor.cursor_of_function (scan_reg scanner pre post)
    | Some _ -> Cursor.cursor_empty()
  in
  fun () eval alg_ctxt input_cursor ->
    let _ = prev_pre := -1 in
    let _ = prev_post := -1 in
    let sym, index = nameindex_of_node_test code_ctxt nt in
    let sc = pre_cursor_of_name_index_at_pos index 0 in
    Cursor.cursor_map_concat (item_fun input_cursor sc) input_cursor

(* ***************************** *)
(* Build code for a single step  *)
(* ***************************** *)
let build_single_step_sc_join code_ctxt input output axis nt =
  match axis with
  | Xquery_common_ast.Self -> 
      fun () eval alg_ctxt input_cursor -> 
	let restore_code = build_create_dom_tuple_code code_ctxt output in
	let retrieve_code =  build_retrieve_dom_tuple_code code_ctxt input in
	let stat_ctxt = static_context_from_code_selection_context code_ctxt in
	
	let item_fun tu = 
	  let item = retrieve_code ()  in
	  let node = getNode (Physical_util.get_item (cursor_of_sequence ( item ))) in
	  let c = eval_axis_node_test stat_ctxt Self nt node in
	  if not (Cursor.cursor_is_empty c) then
	    Cursor.cursor_of_singleton (restore_fun restore_code item)
	  else
	    Cursor.cursor_empty ()
	in
	Cursor.cursor_map_concat item_fun input_cursor 

  | Xquery_common_ast.Descendant ->
      sc_join_descendant code_ctxt input output nt false
  | Xquery_common_ast.Descendant_or_self ->
      sc_join_descendant code_ctxt input output nt true
  | Xquery_common_ast.Ancestor ->
      sc_join_ancestor code_ctxt input output nt false 
  | Xquery_common_ast.Ancestor_or_self ->
      sc_join_ancestor code_ctxt input output nt true 
  | Xquery_common_ast.Following ->
      sc_join_following code_ctxt input output nt
  | Xquery_common_ast.Preceding ->
      sc_join_preceding code_ctxt input output nt
  | Xquery_common_ast.Child ->
      sc_join_child code_ctxt input output nt

  (* fall back to NL-style evaluation here ?? *)
  | Xquery_common_ast.Parent ->
       raise (Query (Prototype 
		      ("The parent axis is not supported by the staicase join")))
  | Xquery_common_ast.Following_sibling ->
       raise (Query (Prototype 
		      ("The following-sibling axis is not supported by the staicase join")))
  | Xquery_common_ast.Preceding_sibling ->
      raise (Query (Prototype 
		      ("The preceding-sibling axis is not supported by the staircase join")))
  | Xquery_common_ast.Attribute ->
      raise (Query (Prototype 
		      ("The attribute axis is not supported by the staircase join")))



  
(* ***************************************** *)
(* Recursive walk of the tree pattern with   *)
(* on the fly code generation                *)
(* ***************************************** *)
let rec build_default_twig_code 
    (code_ctxt:code_selection_context) 
    (input:Namespace_names.rqname) 
    (step_data: (Namespace_names.rqname * Xquery_common_ast.axis * anode_test) option) 
    (pattern:twig_pattern) 
    (index:int)
    :(unit -> eval_fun -> algebra_context ->  tuple_unit Cursor.cursor -> tuple_unit Cursor.cursor)
    = 
  let input' = 
    if index = 0 then input
    else
      match pattern.(index).out with
      | Some o -> o
      | None -> raise out_field_error
  in
  (* step 1 : generate code for a single step -- unless we are in the root *)
  let step_code = 
    match step_data with
    | None -> (fun () eval alg_ctxt curs -> curs) (* root -> end of recursion *)
    | Some (output, axis, nt) ->
	build_single_step_sc_join code_ctxt input output axis nt

  in

  (* step 2 : generate predicate code *)
  let step_pred_code = 
    let filter_fun = build_predicates code_ctxt input' pattern index in 
    (fun dep eval alg_ctxt cursor ->
      Cursor.cursor_filter (filter_fun () eval alg_ctxt) (step_code () eval alg_ctxt cursor)) 
  in

  (* step 3 : generate code for subsequent steps *)
  match pattern.(index).child_twig with
  | None -> step_pred_code
  | Some (typ', index') ->
      (* fixme: factorize to ast_util *)
      let (output',axis',nt') = Code_nestedloop.get_treejoin_attrs pattern (typ', index') in
      let nested_step_code = 
	build_default_twig_code code_ctxt input' (Some (output',axis',nt')) pattern index' 
      in
      fun () eval alg_ctxt cursor -> 
	(nested_step_code () eval alg_ctxt 
	   (step_pred_code () eval alg_ctxt cursor))

(* ************************************************* *)
(* Build the code for a list of chained predicates   *)
(* rooted at 'index'                                 *)
(* ************************************************* *)
and build_predicates code_ctxt input pattern index =
  let predicate_list = pattern.(index).pred_twigs in
  let build_single_predicate (typ, pred_index)  =
    begin
      let (output, axis, nt) = Code_nestedloop.get_treejoin_attrs pattern (typ, pred_index) in
      let predicate_twig_code = 
	build_default_twig_code code_ctxt input (Some (output, axis, nt)) pattern pred_index
      in
      let leaf_node = 
	Xquery_algebra_ast_util.get_leaf_twig_node pattern pred_index
      in
      match leaf_node.out with
      | Some o ->
	  let retrieve_code = build_retrieve_dom_tuple_code code_ctxt o in
	  fun () eval alg_ctxt tuple ->
	    begin
	      (* evaluate the predicate *)
	      let new_curs = predicate_twig_code () eval alg_ctxt 
		  (Cursor.cursor_of_singleton tuple) 
	      in
	      
	      Code_nestedloop.effective_boolean_value 
		(Cursor.cursor_map_concat
		   (fun x -> cursor_of_sequence (retrieve_code ())) new_curs) 
	      
	    end
      | None -> raise out_field_error
    end
  in
  let pred_funs = List.map build_single_predicate predicate_list in
  fun () eval alg_ctxt tuple ->
    List.for_all (fun p -> p () eval alg_ctxt tuple) pred_funs


(* Exposed in interface *)
let build_staircase_join_code code_ctxt input_field pattern =
  build_default_twig_code code_ctxt input_field None pattern 0



