(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_tj_twigstack.ml,v 1.13 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_tj_twigstack
   Description:
     This is the code for the TwigStack variant of the TwigJoin.
*)

open Error
open Dynamic_stack
open Code_util_tj
open Xquery_algebra_ast
open Xquery_algebra_ast_util

open Code_selection_context
open Execution_context

open Datatypes

open Dm_types
open Dm_atomic
open Dm
open Dm_util

open Physical_item
open Physical_item_util
open Physical_sequence
open Physical_value_util
open Physical_table
open Physical_name_index

open Code_util
open Code_util_xpath


(*******************************************************)
(* ALGORITHM: TwigStack                                *)
(* Holistic Twig Joins, Optimal XML Pattern Matching   *)
(* Bruno, Koudas, Srivastava                           *)
(* SIGMOD '02 Madison - USA                            *)
(*******************************************************)

(*
 * Checks if some of the sources belonging to one of the 
 * sub nodes of the twignode at pattern.(src) is empty   
 *)
(*
let some_desc_sources_empty all_sources pattern src =
  let desc_sources = get_sub_node_indices pattern pattern.(src) in
  let is_empty b s =
    match Cursor.cursor_peek all_sources.(s) with
    | Some _ -> b
    | None -> true
   in
  List.fold_left is_empty false desc_sources
*)

let some_desc_sources_empty esf  pattern src =
  let desc_sources = get_sub_node_indices pattern pattern.(src) in
  let is_empty b s = 
    if esf.(s) 
    then true 
    else b
  in
  let b = List.fold_left is_empty false desc_sources in
  let _ = if Debug.default_debug() then Debug.sprintf_default_debug "Some desc source empty? -> %b \n" b in
  b

(*
 * Peeks on a source and gets the node out of the wrapping sequence
 *)
let peek all_sources source esf =
    match Cursor.cursor_peek all_sources.(source) with
    | Some i -> Some(getNode (Physical_util.get_item (cursor_of_sequence i)))
    | None   -> let _ = esf.(source) <- true in  None

let get_min_and_max_source all_sources child_node_indices esf =
  let source_empty = ref false in
  let min, max = ref (-1), ref (-1) in
  for i = 0 to (Array.length child_node_indices) -1 do
    let cur = child_node_indices.(i) in
    match peek all_sources cur esf with
    | Some n ->
	begin
	  if !min < 0 && !max < 0 then
	    (min := cur; max := cur)
	  else
	    let _ = 
	      match peek all_sources !min esf with
	      | Some m when node_precedes n m -> min := cur
	      | _ -> ()
	    in
	    match peek all_sources !max esf with
	    | Some m when node_follows n m -> max := cur
	    | _ -> ()
	end
    | None -> source_empty := true
  done;
  (!source_empty, !min, !max)

(************************************************)
(* PROCEDURE clean_stacks -- part of TwigStack  *)
(************************************************)
let clean_stack stacks index act =
  try
    while not(empty(stacks.(index))) 
	&& (node_preceding_xpath (get_top_item_from_stack stacks.(index)) act) 
    do
      ignore(pop(stacks.(index)))
    done
  with _ ->
    raise (Query(Internal_Error ("Failed during stack cleaning")))

(**************************************************************************)
(* PROCEDURE get_next -- part of TwigStack                                *)
(* To avoid that nodes get pushed on the stack while they are             *)
(* not part of the result, getNext makes sure that when a node            *)
(* n gets pushed on a stack, it holds that:                               *)
(* (i) n has a descendant in each of the streams of the child-twig nodes  *)
(* (ii) the same recursively holds for the nodes on the stacks of the     *)
(*      children of those child-twig nodes                                *)
(**************************************************************************)
let rec get_next pattern all_sources source esf () =
  begin
    let _ = if Debug.default_debug() then Debug.sprintf_default_debug "Entering getNext() for src %i" source in 
    let eoc = ref false in
    if is_leaf_node pattern pattern.(source) then Some source
    else 
      let _ = if Debug.default_debug() then Debug.print_default_debug "Not a leaf node: recursion" in 
      let chldrn = Array.of_list (get_child_node_indices pattern pattern.(source)) in
      let result = ref (-1) in

      let i = ref 0 in
      while !i < (Array.length chldrn) && !result < 0 do
	begin
	  (
	  match get_next pattern all_sources chldrn.(!i) esf () with
	  | Some nxt ->
	      if nxt <> chldrn.(!i) then result := nxt;
	      chldrn.(!i) <- nxt;
	  | None -> eoc := true
	  );
	  incr(i)
	end
      done;
      

      if !result >= 0 then 
	 let _ = if Debug.default_debug() then Debug.sprintf_default_debug "Short circuiting with %i\n" !result in 
	Some !result
      else
	begin
	  let eoc, n_min, n_max = get_min_and_max_source all_sources chldrn esf in
	  let _ =  if Debug.default_debug() then
	    let msg = Format.sprintf "n_min = %i, n_max = %i\n" n_min n_max in
	    Debug.print_default_debug msg
	  in
	  if n_min < 0 || n_max < 0
	  then 
	    begin
	      if Debug.default_debug()
	      then Debug.print_default_debug "End of cursor and end of story!";
	      None (* End of cursor and end of story! *)
	    end
	  else
	    (* Advance cursor (unless it's the first one) *)
	    let npx source n_max' =
	      match (peek all_sources source esf), (peek all_sources n_max' esf) with
	      | Some x, Some y -> node_preceding_xpath x y
	      | _ -> false
	    in
	    while npx source n_max && source <> 0 do 
	      let _ = if Debug.default_debug()
	      then Debug.sprintf_default_debug "*** WARN *** Discarding item on source %i\n" source in 
	      ignore(Cursor.cursor_next all_sources.(source)) 
	    done;

	    let peek_src = peek all_sources source esf in
	    let peek_min = peek all_sources n_min esf in

	    match peek_src, peek_min with
	    | Some x, Some y -> 
		if pre x < pre  y then 
		  let _ =
		    (if Debug.default_debug() then
		      let msg = Format.sprintf "%i < %i --> src\n" (pre x) (pre  y)
		      in Debug.print_default_debug msg)
		  in Some source
		else 
		  let _ = 
		    (if Debug.default_debug()
		    then
		      let msg = Format.sprintf "%i >= %i --> n_min\n" (pre x) (pre  y)
		      in Debug.print_default_debug msg)
		  in  Some n_min		    
	    | None, Some y -> Some n_min
	    | _ ->  None 
	end
  end


let get_node_test pattern src =
  match pattern.(src).node_test with
  | Some nt -> nt
  | _ -> 
      raise (Query (Internal_Error ("Expecting node test in twig node (twigstack)")))


let initialize_sources input_source indices pre post  =
  
  if pre < 0 || post < 0 then
    let sources = Array.map (fun index -> pre_cursor_of_name_index_at_pos index 1) indices in
    Array.append [|input_source|] sources
  else
    let sources = Array.map (fun index -> pre_cursor_of_name_index_from_window index pre post) indices in
    Array.append [|input_source|] sources

(**************************************************************************)
(*                        ALGORITHM TwigStack                             *)
(**************************************************************************)
let twigstack input code_ctxt pattern =

  let len = Array.length pattern in
  let dummy   = (sequence_empty(), (-1), (-1), (-1)) in
  let retrieve_code = build_retrieve_dom_tuple_code code_ctxt input in
  let restore_array = build_restore_array pattern code_ctxt in
  let axes = get_axis_array pattern in

  let _ = get_first_leaf_node_index pattern 0 in 
  let sp = get_select_path pattern in
  let select_leaf = sp.(Array.length sp -1) in

  fun () eval alg_ctxt curs ->
    begin
      (*  create the stack structure *)
      let stacks  = Array.init len  (fun i -> Dynamic_stack.make 16  dummy) in
      let empty_source_flags = Array.make len false in

      let indices = get_name_indices_array code_ctxt pattern in
      let input_source = common_cursor_of_input_cursor curs retrieve_code in 
      let win_pre, win_post = get_index_window input_source in
      let all_sources = initialize_sources input_source indices win_pre win_post in
      let source_cursor = Cursor.cursor_of_function (get_next pattern all_sources 0 empty_source_flags) in 
     
      let item_fun src =
	begin
	  if Cursor.cursor_is_empty all_sources.(0) then empty_source_flags.(0) <- true;
	  if Debug.default_debug() then print_stack_config stacks;
	  let parent_node_index = get_parent_node_index pattern pattern.(src) in
	  let item = Cursor.cursor_next all_sources.(src) in
	  let node = getNode (Physical_util.get_item (cursor_of_sequence item)) in
	  let nextL, nextR = (pre node), (post node) in
	  let _ =
	    if Debug.default_debug()
	    then
	      let msg =
		Format.sprintf "next item = %i-%i from source %i\n" nextL nextR src
	      in
	      Debug.print_default_debug msg
	      in 
	  let _ =
	    if Debug.default_debug()
	    then
	      let msg =
		Format.sprintf "src empty? %b" (Cursor.cursor_is_empty all_sources.(0))
	      in
	      Debug.print_default_debug msg
	  in 
	  let solutions = ref [] in

	  if not (is_root_node pattern pattern.(src)) then
	    clean_stack stacks (parent_node_index) node;
	   
	  if ((is_root_node pattern pattern.(src)) 
	      ||  not(empty stacks.(parent_node_index))) 
	      && not(some_desc_sources_empty empty_source_flags pattern src) 
	  then
	    begin
	      let _ = clean_stack stacks src node in
	      
              (* This check is for distinguishing parent/child and anc/desc relations -- Philippe *)
	      let _ =
		if Debug.default_debug()
		then Debug.sprintf_default_debug "Checking axis for src %i\n" src
	      in
	      let axis_check = check_axis code_ctxt node axes pattern src in 
	      let _ =
		if Debug.default_debug()
		then
		  let msg =
		    Format.sprintf "Parent node index is %i, axis check is %b\n" parent_node_index axis_check
		  in
		  Debug.print_default_debug msg
	      in
	      if axis_check then 
		begin
		  try
		    if parent_node_index < 0 then 
		      push stacks.(src) (item,-1,nextL,nextR)
		    else
		      push stacks.(src) (item,(stacks.(parent_node_index).size),nextL,nextR);
		  with _ -> raise (Query(Internal_Error("Attempt to push failed")))
		end;
	      
	      if Debug.default_debug() then print_stack_config stacks;
	      if src = select_leaf && axis_check then
		begin
		  let tmp = show_solutions pattern axes stacks src (* 0 *) (stacks.(src).size -1) in
		  solutions := [List.hd tmp];
		  try
		    ignore ( pop(stacks.(src)) );
		  with _ -> raise (Query(Internal_Error("Attempt to pop for select-leaf failed")))
		end
	      
	      else if (is_leaf_node pattern pattern.(src)) then 
		begin

		  (* leaf node bahavior is now moved to the selecting xpath-node -- philippe *)
(*		  let tmp = show_solutions pattern axes stacks src 0 in *)
(*		  if src = output_src then solutions := tmp; *)
		  try
		    ignore(pop(stacks.(src)));
		  with _ -> raise (Query(Internal_Error("Attempt to pop for leaf failed")))
		end
	    end
	  else 
	    begin
	      if parent_node_index >= 0 then
		match Cursor.cursor_peek all_sources.(parent_node_index) with
		| Some item ->
		    let _ = if Debug.default_debug() then 
		      let msg = Format.sprintf "Jumping on source %i - window:%b\n" src (not(win_post < 0)) in
		      Debug.print_default_debug msg
		    in
		    let jump_to = pre (getNode (Physical_util.get_item (cursor_of_sequence item))) in
		    let new_cursor =
		      if win_post < 0 
		      then pre_cursor_of_name_index_at_pos indices.(src -1) (jump_to +1)
		      else pre_cursor_of_name_index_from_window indices.(src -1) (jump_to +1) win_post
		    in
		    all_sources.(src) <- new_cursor 
		| None -> () (* discarding *) 
	    end;

	  !solutions
	end
      in
      let restore_intermed list =
	Cursor.cursor_map (restore_tuple restore_array) (Cursor.cursor_of_list list)
     in
      Cursor.cursor_map_concat restore_intermed (Cursor.cursor_map item_fun source_cursor)
   end

let build_holistic_tuple_tree_pattern_code code_ctxt input pattern =
  (*  create field accessor and field restore functions *)
  let output_fields = get_restored_outputs_from_twig_pattern pattern in
  let _ = List.map (add_tuple_reference code_ctxt) output_fields in 
  (*  the algorithm *)
  twigstack input code_ctxt pattern


