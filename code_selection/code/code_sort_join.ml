(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_sort_join.ml,v 1.9 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_sc_join
   Description:
     This module contains code building for operators that implement
     sort-joins.
*)

(* *** Warning: ***
   This module does not correctly handle type promotion.
   Promotion should be done per-type.
*)

open Error

open Xquery_core_ast
open Xquery_common_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_physical_type_ast
open Algebra_type

open Norm_context
open Typing_context
open Compile_context
open Code_selection_context
open Execution_context

open Dm_atomic_btree
open Dm
open Physical_value
open Physical_sequence
open Physical_value_util 
open Physical_table

open Code_update
open Cs_util

open Algebra_type
open Cs_util_coercion

open Compile_context
open Code_selection_context   
open Optimization_util

open Code_util_materialize
open Code_util_pattern_matcher
open Code_util_predicates
open Code_util_ridlist

type sort_join_index =
    table * (Code_util_ridlist.rid Dm_atomic_btree.btree) array


(*************************************************************)
(*   This section of the code deals with searching the table *)
(*   with respsect to different operators                    *)
(*************************************************************)

(* probe_sorted *)

let probe_sorted op_type tbl (b : Dm_atomic.atomicValue)  =
  let max_table_value = max_position tbl in
  let min_table_value = min_position tbl in

  (* Return inclusive intervals
     [lo, hi]
  *)
  let lo, hi =
    (* *****
       The op_type refers to what the compiled op was. So 
       we need to find all ys such that :
       b op_type ys.
       
       So Greater_Than entails finding all strictly smaller ys *)
    match op_type with
    | Invalid_Scan -> 
	raise (Query (Code_Selection ("Invalid Scan of sorted index")))

	  (* Inclusive as above *)
    | Value_Equality ->
	let lo_point = find_low_point tbl b in
	let hi_point = find_high_point tbl b in
	lo_point, hi_point

	  (* Greater than *)
	  (* b > y *)
    | Greater_Than ->
	(* Find the lowest equal point to b. This is the lowest index 
	   b or is already less than b. Find out which and return it 
	 *)
	let hi_point = previous_highest_value tbl b (find_low_point tbl b) in
	let lo_point = min_typed_value tbl b in
	lo_point, hi_point

    | Greater_Than_or_Equal ->
	let hi_point = find_high_point tbl b in
	let lo_point = min_typed_value tbl b in
	lo_point, hi_point	      

    | Less_Than ->
	let lo_point = next_lowest_value tbl b (find_high_point tbl b) in
	let hi_point = max_typed_value tbl b in	      
	lo_point, hi_point

    | Less_Than_or_Equal ->
	let lo_point = find_low_point tbl b in
	let hi_point = max_typed_value tbl b in
	lo_point, hi_point
  in
  (* Empty conditions *)
  if (position_after lo hi) || (position_after hi max_table_value) || (position_before lo min_table_value)
  then []
  else to_list (sub tbl lo hi)


(*************************)
(* COMMON SORT JOIN CODE *)
(*************************)

let build_common_join_code nsenv eval alg_ctxt built_cond left_cursor pred_desc get_btrees null_handling array_restore_fun =
  let (is_outer_join,null_function,not_null_fn,empty_right) = null_handling in
  let materialized_array,btrees = get_btrees () in
  let built_predicates =
    Array.mapi (fun i -> fun (outer, inner, op) ->
      btrees.(i), (outer, inner, op)) built_cond
  in

  (************************)
  (* The probing function *)
  (************************)
  let sort_probe current_sort sort_op_type current_outer_predicate_branch =
    let atomic_values = Code_util_predicates.evaluate_predicate_branch 
	current_outer_predicate_branch eval alg_ctxt nsenv in
    (* Now probe each one *)
    List.concat
      (List.map
	 (probe_sorted sort_op_type current_sort)
	 atomic_values)	       
  in
  (* probe to rid lists *)
  let sort_probe_to_rid_list (sorted_array, (outer_op,inner_op, sort_op_type)) =
    let ret_list = 
      (Gmisc.sort_and_remove_duplicates_revcompare rid_compare
	 (sort_probe sorted_array sort_op_type outer_op))
    in
    if List.length ret_list = 0 then
      Empty_RidList
    else
      Regular_RidList ret_list
  in
  let get_rid_list_for_tuple () = 
    Code_util_predicates.eval_predicate_desc_to_rid_list 
      sort_probe_to_rid_list
      built_predicates 
      pred_desc
  in

  (*********************************)
  (* Sort it and remove duplicates *)
  (*********************************)
  let ci = ref 0 in
  let eval_fn tup = 	 
    incr ci;
    let rid_list = get_rid_list_for_tuple () in
    (* Unwrap code *)
    let ret =
      unwrap_rid_list_cursor materialized_array array_restore_fun rid_list
    in
    if (is_outer_join) then
      begin 
	if Cursor.cursor_is_empty ret then
	  begin
	    null_function ();
	    array_restore_fun empty_right;
	    table_of_singleton empty_tuple
	  end
	else
	  begin
	    not_null_fn ();
	    ret
	  end
      end
    else
      ret
  in
  Cursor.cursor_map_concat eval_fn left_cursor

let make_binary_sort_join built_cond outer_kind code_ctxt pred_desc =
  (* 0. Set up the necessary environments *)
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let norm_ctxt = norm_context_from_stat_context stat_ctxt in
  let nsenv     = nsenv_from_norm_context norm_ctxt in
  let annot     = retrieve_annotation "make_binary_sort_join" code_ctxt in 

  (* 1. Set up support for materialization *)
  let array_materialize_fun, array_restore_fun, needed_names =
    materialize_cursor_to_dom_value_array code_ctxt annot () in

  (* 2. Set up support for null-values *)
  let null_handling =
    Code_util_join.get_null_functions code_ctxt outer_kind needed_names
  in

  (* 3. Build the actual join code ! *)
  let fn =
    (fun () eval alg_ctxt left_cursor right_cursor ->
      let get_btrees () =
	let mat =
	  array_materialize_fun eval alg_ctxt right_cursor
	in
	let btrees =
	  Array.map (fun (_,inner,_) -> 
	    let sort_materialize =
	      materialize_array_to_sorted_array_index
		code_ctxt annot
		array_restore_fun
		(inner,nsenv)
	    in
	    sort_materialize eval alg_ctxt mat)
	    built_cond
	in
	mat,btrees
      in
      build_common_join_code nsenv eval alg_ctxt built_cond left_cursor pred_desc get_btrees null_handling array_restore_fun)
  in
  fn

let make_unary_sort_join built_cond is_outer_join outer_kind code_ctxt pred_desc (btrees:Physical_name_index.name_index_handler) =
  (* 0. Set up the necessary environments *)
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let norm_ctxt = norm_context_from_stat_context stat_ctxt in
  let nsenv     = nsenv_from_norm_context norm_ctxt in
  let annot     = retrieve_annotation "make_unary_sort_join" code_ctxt in 

  (* 1. Set up support for materialization *)
  let array_materialize_fun, array_restore_fun, needed_names =
    materialize_cursor_to_dom_value_array code_ctxt annot () in

  (* 2. Set up support for null-values *)
  let null_handling =
    Code_util_join.get_null_functions code_ctxt outer_kind needed_names
  in

  (* 3. Build the actual join code ! *)
  let fn =
    (fun () eval alg_ctxt left_cursor ->
      let get_btrees () = 
	let _ = 
	  if Debug.default_debug() then
	     Debug.print_default_debug "WARNING, finalizing btree handler into full index (make_unary_sort_join)!";
	in
	Dm_atomic_btree_util.full_btree_index_of_btree_handler btrees 

      in
      build_common_join_code nsenv eval alg_ctxt built_cond left_cursor pred_desc get_btrees null_handling array_restore_fun)
  in
  fn



(****************************************************************)
(* SORT JOIN WITH RIGHT BTREE INDEX *)
(****************************************************************)

let build_sort_join_with_btree_index outer_kind code_ctxt index built_cond pred_desc =
  (* 1. call the common join code constructor *)
  let fn =
    make_unary_sort_join built_cond is_outer_join outer_kind code_ctxt pred_desc index
  in
  (* 2. wrap the resulting code *)
  (coerce_unitdep fn () coerce_unary_tuple_cursor_to_tuple_cursor), code_ctxt


(****************************************************************)
(* SORT JOIN WITH INPUT RIGHT BRANCH *)
(****************************************************************)

let build_sort_join outer_kind code_ctxt built_cond pred_desc =
  (* 1. call the common join code constructor *)
  let fn =
    make_binary_sort_join built_cond outer_kind code_ctxt pred_desc
  in
  (* 2. wrap the resulting code *)
  (coerce_unitdep fn () coerce_binary_tuple_cursor_to_tuple_cursor), code_ctxt


