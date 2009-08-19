(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_tj_pathstack.ml,v 1.6 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_tj_pathstack
   Description:
     This is the code for the PathStack variant of the TwigJoin.
*)

open Error
open Dynamic_stack
open Code_util_tj


open Code_selection_context
open Execution_context
open Xquery_algebra_ast
open Xquery_algebra_ast_util

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

open Cs_util
open Code_util_materialize



 (*******************************************************)
 (* ALGORITHM: PathStack                                *)
 (* Holistic Twig Joins, Optimal XML Pattern Matching   *)
 (* Bruno, Koudas, Srivastava                           *)
 (* SIGMOD '02 Madison - USA                            *)
 (*******************************************************)

(* PROCEDURE get_min_source -- PART OF PathStack                   *)
(* Get the index or input whose first node has a minimal pre-order *)
(* Returns the index of that source in the source array.           *)
let get_min_source indices_curs () =
  let min = ref (-1) in
  let current = ref None in

  for i = 0 to (Array.length indices_curs) -1 do
    begin
      let next_item = Cursor.cursor_peek indices_curs.(i) in
       match next_item with
       | Some node_arr ->
	   begin
	     let node = getNode (Physical_util.get_item (cursor_of_sequence ( node_arr ))) in
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

 
 (******************************************************************************)
 (*                            ALGORITHM PathStack                             *)
 (******************************************************************************)
 let pathstack input code_ctxt pattern stacks =
   let len = Array.length stacks in
   let retrieve_code = build_retrieve_dom_tuple_code code_ctxt input in
   let restore_array = build_restore_array pattern code_ctxt in
   let axes = get_axis_array pattern in

   fun () eval alg_ctxt curs ->
   begin
     let indices = get_name_indices_array code_ctxt pattern in
     let indices_curs = Array.map (fun index -> pre_cursor_of_name_index_at_pos index 1) indices in
     let all_sources = Array.append [| common_cursor_of_input_cursor curs retrieve_code |] indices_curs in	  
     let source_cursor = Cursor.cursor_of_function (get_min_source all_sources) in
     
     let item_fun src =
       begin
	 let item = Cursor.cursor_next all_sources.(src) in
	 let node = getNode (Physical_util.get_item (cursor_of_sequence item)) in
	 let nextL = pre  node in
	 let nextR = post node in

	 (* clean stacks *)
	 for i = 0 to len -1 do
	   while not(empty stacks.(i)) && ((get_top_post_from_stack stacks.(i)) < nextR) do
	     ignore(pop stacks.(i))
	   done
	 done;

	 (* move stream to stack *)
	 (* Note: possible optimization, short circuit if not all parent stacks non-empty *)
	 let parent_node_index = get_parent_node_index pattern pattern.(src) in
	 if parent_node_index < 0 
	 then push stacks.(src) (item, -1, nextL, nextR)
	 else push stacks.(src) (item, (stacks.(parent_node_index).size), nextL, nextR);

	(* print_stack_config stacks;*)
	 let output = ref [] in
	 if (is_leaf_node pattern pattern.(src)) then 
	   begin
	     output := show_solutions pattern axes stacks src 0;
	     ignore(pop stacks.(src)); 
	   end;

	 !output 
       end
     in
     let restore_intermed list =
       (* Warning, this XPath hack prevents duplicates to occur in the *)
       (*  output. It only works -for now- if the output corresponds   *)
       (*  with the leaf node of a straight-line twig pattern          *)
       (*  replace by duplicat elim over the output fields - Philippe  *)
       let list' = 
	 match list with
	 | hd::tl -> [hd]
	 | _ -> list
       in
       Cursor.cursor_map (restore_tuple restore_array) (Cursor.cursor_of_list list')
     in
     Cursor.cursor_map_concat restore_intermed (Cursor.cursor_map item_fun source_cursor)
   end


let build_holistic_tuple_tree_pattern_code code_ctxt input pattern =

  (* 1. create field accessor and field restore functions *)
  let output_fields = get_restored_outputs_from_twig_pattern pattern in
  let _ = List.map (add_tuple_reference code_ctxt) output_fields in 

  (* 2. create the stack structure *)
  let dummy   = (sequence_empty(), (-1), (-1), (-1)) in
  let stacks  = Array.init (Array.length pattern) (fun i -> Dynamic_stack.make 16  dummy) in

  (* 3. the algorithm *)
  pathstack input code_ctxt pattern stacks 


