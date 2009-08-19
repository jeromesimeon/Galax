(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_group_order.ml,v 1.13 2007/08/23 21:01:32 simeon Exp $ *)

(* Module: Code_group_order
   Description:
     This module contains code building for operators that implement
     group-by.
*)

open Cs_util_coercion 

open Error

open Physical_value_util 
open Physical_table (* Create_Tuple *)
open Physical_sequence (* materialized_of_list *)

open Xquery_common_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_algebra_ast_annotation_util

open Algebra_type
open Execution_context
open Cs_util

open Code_selection_context
open Code_util_materialize
(**************************************************)
(* Some of this section could probably reside in other 
  locations (dm.mli) but
   we haven't used the yet *)
(**************************************************)

(* AtomicValue Hash *)
module AtomicValueHashType = 
 struct 
   type t = Dm_atomic.atomicValue
   let equal x y = x#atomic_value_eq y
   let hash x = 
     let x_str = x#string_value () in
     let ret   = (Hashtbl.hash x_str) 
		 * (Hashtbl.hash (x#getAtomicValueKind ())) in	
       ret 
 end

module AtomicValueHash = Hashtbl.Make(AtomicValueHashType)
(* There are potentially many sorts and many group desc *)
(* Right now we implement them one after the other as decomposed operators *)

(**************************************************)
type select_single_op_fn =   
    Code_selection_context.code_selection_context -> algop_expr -> 
      Code_selection_context.code_selection_context

  (* Note:
     All of the evaluations of e2 must yield values (or nodes) of
     the same type.
     - Jerome *)
let eval_e2 (eval:eval_fun)
    (alg_ctxt:algebra_context) 
    (dep:algop_expr) =
    (* Evaluates the sortspec expression *)
    (* This would be nice to get rid of... *)
    item_cursor_of_physical_value
      (eval alg_ctxt dep)
  
(* Compare function.
   This can be updated since we have very particular semantics (no
   need to evaluate the functions).
*)
let compare_function restore_fn sk_esk_list eval alg_ctxt compare_deps input_value1 input_value2 =
  let atomic_comp = Code_util.make_atomic_gt in
  let rec compare_fun deps sk_esk_list =
    match (deps,sk_esk_list) with
      | [],[] -> 0
      | (dep::deps',(sk,esk) :: sk_esk_list') ->
	  let eval_e2' x = 
	    restore_fn x; (* setup evaluation tuple fields *)
	    eval_e2 eval alg_ctxt dep
	  in
	  let comp1 =	      
	    (Code_util.forest_compare sk esk 
	       (eval_e2' input_value1)
	       (eval_e2' input_value2))
	      atomic_comp
	  in
	    if comp1 = 0
	    then
	      compare_fun deps' sk_esk_list'
	    else
	      comp1
      | _ ->
	  raise (Query (Code_Selection "Incompatible number of sub-exprs and order by modifiers in one order-by expression"))
  in
    (* Tail because the first dep is to be applied, not grouped by
    *)
    
    compare_fun compare_deps sk_esk_list

(* Comparison operation *)

(* Helper function to find the index of a given variable name *)
let name_to_index needed_names vn =
  let n_needed_names = Array.length needed_names in
  let rec name_to_index_helper index vn = 
    if index < n_needed_names then
      if needed_names.(index) = vn then
	index
      else
	name_to_index_helper (index+1) vn
    else
      raise (Query (Code_Selection ("name not found during grouping: " ^ 
				  (Namespace_names.prefixed_string_of_rqname vn))))
  in
    name_to_index_helper 0 vn

(* Aggregate Cursor Semantic:
   Assume this is sorted by the group names in order.   
*)
let aggregate_cursor nsenv code_ctxt (store_aggregate_value: Physical_value.dom_value -> unit)
  (restore_fn:Code_util_materialize.restore_function)
  needed_names group_names odt valid_names
  group_create_op 

  (* These are the typical runtime parameters *)
  eval alg_ctxt sorted_input =
  let stat_ctxt      = static_context_from_code_selection_context code_ctxt in
  let start_offset   = ref 0 in
  let current_offset = ref 0 in

  let input_length   = Array.length sorted_input in
  (* First lookup the indexes of the names *)
  let group_indexes  =
    Array.of_list 
      (List.map (name_to_index needed_names)
	 group_names) in
  let n_indexes        = Array.length group_indexes in
  let required_indexes = Array.of_list
			   (List.map (name_to_index needed_names)
			      valid_names) in
  
  let required_length = Array.length required_indexes in
  (* Helper function determine if row1 = row2 *)
  let named_contents_equal row1 row2 =
    let is_equal = ref true in
      for i = 0 to n_indexes - 1 do
	let cur_index = group_indexes.(i) in
	  is_equal := !is_equal &&
	  (sorted_input.(row1).(cur_index) = 
	     sorted_input.(row2).(cur_index))
      done;
      !is_equal
  in

  (* Helper function to see if any grouping attribute
     is empty *)
  let exists_empty_name rown = 
    let is_empty = ref false in 
    let i = ref 0 in
      while (!i < required_length) && 
	(not (!is_empty)) do
	let cur_index = required_indexes.(!i) in
	  if (sorted_input.(rown).(cur_index)
	     = empty_dom_sequence) then
	    begin
	      is_empty := true
	    end
	  ;
	  incr i
      done;
      !is_empty
  in
  (****************************)
  (* Actual Cursor Definition *)
  (****************************)

  (* If we have input length = 0
     - we clear the aggregate value 
     o On empty input
     the apply clause is the empty
     list to the name.
     
     This to prevent dirty output
   *)
  if (input_length = 0) then
    begin
      (fun () ->	       	     
	begin
	  store_aggregate_value (materialized_of_list []);
	  None
	end
      )
    end
  else
    (* Typical grouping *)
    (fun () ->	   
      (* In this case, we have reached the end,
	 so we return None *)
      if (!start_offset >= input_length) then	    
	begin
	  None	       
	end     
      else
	begin
	  let return_sequence = ref [] in
	  let start_off = !start_offset in
	  
	  (* Restore the current tuple *)
	  restore_fn sorted_input.(start_off);
	  
	  (* Special Case: 
             There exists x in GroupingNames = () =>
             The aggregate should be set to () and
             we advance *)		 
	  if (exists_empty_name start_off) then
	    begin
	      store_aggregate_value empty_dom_sequence;
	      incr start_offset;
	      empty_tuple_opt			     
	    end
	  else
	    begin
	      (* General Case: 
		 This is an actual grouping *)
	      
	      (* Start counting and looping *)
	      (* Could be faster to split this into two loops
		 Loop 1 gets the bounds in the array
		 -- and --  
		 Loop 2 does the evaluation and storest the results in *)
	      
	      (* For performance we should really have two different 
		 functions a duplicate removing
		 one and a non-duplicate removing version *)
	      
	      current_offset := start_off;
	      while !current_offset < input_length && (* still in bounds *)
		(named_contents_equal start_off !current_offset) do
		(* restore the current value for evaluation *)
		restore_fn sorted_input.(!current_offset);
		(* retrieve the current return *)
		let current_ungrouped_value = item_list_of_physical_value
		    (eval alg_ctxt group_create_op) in
		
		return_sequence := (!return_sequence @ current_ungrouped_value);			     
		incr current_offset
	      done;		 
	      
	      start_offset := !current_offset; (* update the new start offset *)
	      
	      begin
		match odt with
		| None ->
		    store_aggregate_value
		      (materialized_of_list !return_sequence);
		| Some dt ->
		    let type_matched_cursor = Code_util_matching.dynamic_type_check stat_ctxt dt (Cursor.cursor_of_list !return_sequence) in
		    store_aggregate_value (Physical_sequence.materialized_of_cursor type_matched_cursor)
	      end;
	      empty_tuple_opt (* bogus return *)
	    end
	end)

(*******************************************)
(* This does not select the code to do so! *)
(*******************************************)
let build_comparison_dep code_selection code_ctxt g_names =
  let (comp_ctxt:Algebra_type.alg_compile_context) = Code_selection_context.annotated_compile_context_from_code_selection_context code_ctxt in 
  let build_op gn =    
    Cs_annotate.annotate_statement comp_ctxt
      (logical_aalgop_mkop (AOEAccessTuple gn) NoSub NoSub None None Finfo.bogus)
  in
  let return = List.map build_op g_names in
    List.iter (fun x -> ignore(code_selection code_ctxt x)) return;
    return
      

(* GROUPING CODE TAKES THE OP SELECTION FUNCTION
   SO THAT IT CAN BUILD COMPARISON OPERATORS *)
(* This build for a single grouping *)
(* This is a really slow way to do this. We materialize PER
   group. Each group consumes the previous.  There is no code to share
   the sort orders (in most cases we just require one
   sort/materialization and several uses of it) Also, there are some
   cases when we could have no materialization at all (if we are just
   sorting on doc order).  *)

let append_aggregate_return code_ctxt agg =
  let annot = retrieve_annotation "append_aggregate_return" code_ctxt in 
  let rf    = agg :: (get_returned_fields annot) in 
  let annot =   mk_annotation (get_use_counts annot) (get_bound_use_counts annot)
                  (get_accessed_fields annot) rf (get_tuple_field_use_counts annot) in
      store_annotation code_ctxt (Some annot)

let build_group_code_aux op_selection code_ctxt group_desc index =
  let agg_name  = get_aggregate_name group_desc in
  let agg_type  = get_aggregate_type group_desc in 
    (* Store the new aggregate so the next cursor materializes it *)
    (* Retrieve before append so that we don't materialize our own
    aggregate (no need too) *)
  let annot     = retrieve_annotation "build_group_code_aux" code_ctxt in 
  let code_ctxt = append_aggregate_return code_ctxt agg_name in
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  let nsenv     = Norm_context.nsenv_from_norm_context norm_ctxt in

  let sort_function = Array.stable_sort in
  let store_aggregate_value = build_create_dom_tuple_code code_ctxt agg_name in

  let group_names = get_group_names    group_desc in
  let valid_names = get_valid_names    group_desc in 

  let sk_esk_list = List.map (fun x -> (Ascending, EmptyGreatest)) group_names in

  let materialize_fun, restore_fn, needed_names = materialize_cursor_to_dom_value_array code_ctxt annot () in
  let built_cursor = aggregate_cursor nsenv code_ctxt store_aggregate_value restore_fn needed_names group_names agg_type valid_names in
  let compare_deps = build_comparison_dep op_selection code_ctxt group_names in

    (fun (deps:algop_expr array) 
	 (eval:eval_fun)
	 (alg_ctxt: algebra_context) 
	 input_table ->     
	   (* Then sort !! *)
	   Debug.print_join_debug ">>> In GroupBy, materialization";
	   let materialized = materialize_fun eval alg_ctxt input_table in
	     (* Now sorts it in place *)
	   Debug.print_join_debug ">>> In GroupBy, AFTER materialization";
	   Debug.print_join_debug ">>> In GroupBy, sorting";
	   sort_function
	       (compare_function restore_fn sk_esk_list eval alg_ctxt compare_deps) materialized;
	   Debug.print_join_debug ">>> In GroupBy, AFTER sorting";
	   Debug.print_join_debug ">>> In GroupBy, NOW creating output_cursor";
	     Cursor.cursor_of_function (built_cursor deps.(index) eval alg_ctxt materialized)
    ), code_ctxt

(* Order by *)
(* Maybe should make actual tuples array based *)
let build_default_tuple_order_by_code code_ctxt stable sk_esk_list gt_table =
  (* The following function needs to be called to get the value of the
     sorting expression. *)
  let annot = retrieve_annotation "build_default_tuple_order_by_code" code_ctxt in
  let materialize_fun, restore_fn, needed_names = materialize_cursor_to_dom_value_array code_ctxt annot () in

   (* Note:
       All of the evaluations of e2 must yield values (or nodes) of
       the same type.
     - Jerome *)
  let eval_e2 (eval:eval_fun)
      (alg_ctxt:algebra_context) 
      (dep:algop_expr) =
    (* Evaluates the sortspec expression *)
    (* This would be nice to get rid of... *)
    item_cursor_of_physical_value
      (eval alg_ctxt dep)
  in

  (* Here is the local comparison function *)

  (* Note:
       This applies both evaluation of the sorting expression, and
       actual comparison for each resulting value taking all the
       necessary parameters into account.
     - Jerome *)

  let op_gt alg_ctxt arg1 arg2 =
    let arg1 = [arg1] in
    let arg2 = [arg2] in
    let result =
      Code_overloaded_fn.build_default_overloaded_fn_code
	code_ctxt
	Namespace_builtin.op_gt
	2
	gt_table
	alg_ctxt
	[|arg1;arg2|]
    in
    Physical_util.get_boolean (Cursor.cursor_of_list result)
  in
  
  let compare_function eval alg_ctxt deps input_value1 input_value2 =
    let deps = Array.to_list deps in
    let rec compare_fun deps sk_esk_list =
      match (deps,sk_esk_list) with
      | [],[] -> 0
      | (dep::deps',(sk,esk) :: sk_esk_list') ->
	  let eval_e2' = (fun x ->
	    restore_fn x; (* setup evaluation tuple fields *)
	    eval_e2 eval alg_ctxt dep) in
	  let comp1 =
	    (Code_util.forest_compare sk esk 
	       (eval_e2' input_value1)
	       (eval_e2' input_value2))
	      (op_gt alg_ctxt)
	  in
	  if comp1 = 0
	  then
	    compare_fun deps' sk_esk_list'
	  else
	    comp1
      | _ ->
	  raise (Query (Code_Selection "Incompatible number of sub-exprs and order by modifiers in one order by expression"))
    in
    compare_fun deps sk_esk_list
  in

  (* The sort function *)
  let sort_function =
    match stable with
    | Stable ->
	Array.stable_sort
    | NonStable ->
	Array.sort
  in
  (* Should be a common operation.. *)
  let our_cursor m = 
    let offset = ref 0 in
    let len = Array.length m in
    
    (fun () ->
      let res = 
	if !offset < len then	   
	  begin
	    restore_fn m.(!offset);
	    empty_tuple_opt
	  end
	else
	  None
      in
      incr offset;
      res)
  in
  (fun (deps:algop_expr array) 
      (eval:eval_fun)
      (alg_ctxt: algebra_context) 
      input_table ->     
	let materialized = materialize_fun eval alg_ctxt input_table in
	(* Now sorts it in place *)
	sort_function (compare_function eval alg_ctxt deps) materialized;
	Cursor.cursor_of_function (our_cursor materialized))

let build_order_by_code code_ctxt algop (stablekind, sort_spec_list, gt_table) =
  let _   = access_onesub algop.psub_expression in
  let dep = access_manysub algop.pdep_sub_expression in
  let fn = build_default_tuple_order_by_code code_ctxt stablekind sort_spec_list gt_table in
  (coerce_manydep fn dep coerce_unary_tuple_cursor_to_tuple_cursor), code_ctxt

(**************************)
(* Exposed build function *)
(**************************)

let build_default_group_code op_select code_ctxt gd_list =
  let build_helper code_ctxt index gd =
    build_group_code_aux op_select code_ctxt gd index 
  in
  let build_fold gd (cursors, code_ctxt, index) =
    let cursor, code_ctxt = build_helper code_ctxt index gd  in
      (cursor :: cursors, code_ctxt, (index-1))
  in
  (* These must be built in reverse order so that the annotation for
     the new aggregate is kept. This annotation tells cursor{i+1} to materialize
     the aggregates of the previous aggregates
  *)
  let cursor_list, code_ctxt, _ = 
    List.fold_right build_fold gd_list ([], code_ctxt, ((List.length gd_list)-1) ) 
  in
  let cursor_array = Array.of_list cursor_list in
  let gd_length    = Array.length cursor_array in
  (fun deps eval alg_ctxt input_table ->
    (* The first are applied last since this is the semantic of the operand
       new ones can be appended *)
    (* apply in reverse order *)
    let rec apply_cursor index input = 
      match index with
      | -1 ->
	  input
      | i  -> 
	  apply_cursor (index-1) (cursor_array.(index) deps eval alg_ctxt input)
    in
    apply_cursor (gd_length - 1) input_table)

let build_group_code code_ctxt algop single_op_default_code_selection gd_list =
  (* Add in the created tuples in the return *)
  let add_aggs  = List.map Xquery_algebra_ast_util.get_aggregate_name gd_list in
  let code_ctxt = List.fold_left add_tuple_reference code_ctxt add_aggs in
  let indep = access_onesub algop.psub_expression in 	
  let code_ctxt = store_annotation code_ctxt indep.compile_annotations in
  (* THIS IS AWFUL - we are passing the function inside the grouping code...*)
  (* We do this to do code selection for the sort operands over sequences *)
  let fn    = build_default_group_code (single_op_default_code_selection) code_ctxt gd_list in
  let dep   = access_manysub algop.pdep_sub_expression in 
  (coerce_manydep fn dep coerce_unary_tuple_cursor_to_tuple_cursor), code_ctxt

