(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_treejoin.ml,v 1.17 2007/05/02 19:30:59 mff Exp $ *)

(* Module: Code_tree_join
   Description:
     This module contains code building for the TreeJoin operator.
*)

open Error 

open Xquery_common_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_physical_type_ast
open Xquery_physical_type_ast_util
open Xquery_physical_algebra_ast

open Algebra_type
open Cs_util_coercion

open Code_util_xpath 

open Processing_context
open Compile_context
open Code_selection_context   

open Physical_item 
open Physical_value

(* TreeJoin Code *)

let build_default_tree_join_code code_ctxt axis anode_test =
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let eval_node_test_fun = eval_axis_node_test stat_ctxt axis anode_test in
  (fun alg_ctxt input_cursor ->
    let eval_axis_single_item item =
      let complete_node_cursor =
	match item_kind item with
	| NodeKind ->
            eval_node_test_fun (getNode item)
	| AtomicValueKind ->
	    raise (Query (Type_Error "Applying a step on an atomic value"))
      in
      Cursor.cursor_map (fun x -> (Item_Node x)) complete_node_cursor
    in
    Cursor.cursor_map_concat eval_axis_single_item input_cursor)

let build_default_streaming_tree_join_code code_ctxt axis node_test =
  if (Debug.default_debug()) then Debug.print_default_debug "Picking up streaming TreeJoin";
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let apply_xpath_step input_stream =
    match axis with
    | Descendant ->
	Streaming_xpath.xpath_axis_descendant stat_ctxt input_stream node_test
    | Child ->
	Streaming_xpath.xpath_axis_child stat_ctxt input_stream node_test
    | Descendant_or_self ->
	Streaming_xpath.xpath_axis_descendant_or_self stat_ctxt input_stream node_test
    | Attribute ->
	Streaming_xpath.xpath_axis_attribute stat_ctxt input_stream node_test
    | _ -> raise (Query (Code_Selection "[StreamTreeJoin] Can't deal with that kind of axes!"))
  in
  (fun alg_ctxt typed_xml_stream -> apply_xpath_step typed_xml_stream)


let is_sort_index_joinable code_ctxt axis anode_test =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  let right_axis =
    match axis with
    | Descendant
    | Descendant_or_self -> true
    | _ -> false
  in
  let (right_node_test,handler) =
    match anode_test with
    | APNameTest anon_symbol ->
	begin
	  match get_name_index_handler code_ctxt anon_symbol with
	  | None -> false,None
	  | Some index_handler -> true,Some index_handler
	end
    | _ -> false,None
  in
  if ((proc_ctxt.Processing_context.treejoin_phys = Processing_context.IndexSortJoin) && right_node_test && right_axis)
  then
    true,handler
  else
    false,None

let get_handler handler =
  match handler with
  | None -> raise (Query (Internal_Error "where is the btree handler??"))
  | Some handler -> handler

(* Covers all xml stream output currently available, although, in theory,
   more could be 'hidden' behind output of type physical_value.
   - Michael *)

let produces_xml_stream_output = false
(*
  match input_sig with
    | OneInput(PT_XML(PT_Sax)) -> true
    | _ -> false
*)

let build_treejoin_code code_ctxt algop (axis, anode_test) =
  let _ = access_nosub algop.pdep_sub_expression in
  let _ = access_onesub algop.psub_expression in
  let physop = Cs_util.get_physical_opname algop in 
  match physop with 
    | POTreeJoin_Sort -> 
      begin
	Format.printf ">>>\tPicked up INDEX SORT JOIN!!!\n\n";
	flush stdout;
	let (_,handler) = is_sort_index_joinable code_ctxt axis anode_test in
	let btree_index = get_handler handler in
		      (* 1. pre-compile the conditions *)
	let pred_desc = raise (Query (Prototype "NOT READY FOR INDEX JOIN YET")) in
	let built_cond =  raise (Query (Prototype "NOT READY FOR INDEX JOIN YET"))
					(*
					   Cs_util_join_helper.build_sort_cond
					   default_code_selection
					   code_ctxt
					   algop 
					*)
	in
		      (* 2. build the sort join code *)
	Code_sort_join.build_sort_join_with_btree_index
	  Code_util_join.StandardJoin
	  code_ctxt
	  btree_index
	  built_cond
	  pred_desc
      end
  | POTreeJoin_Stream ->
      begin  (*let _ = Printf.printf "Using Streaming\n" in*)
	let fn = build_default_streaming_tree_join_code code_ctxt axis anode_test in
	(coerce_nodep fn coerce_unary_sax_to_sax), code_ctxt  
      end
  | POTreeJoin_NestedLoop ->
      begin (* Select non-streaming tree join code. *)
			  (*let _ = Printf.printf "NOT Using Streaming\n" in*)
	let fn = build_default_tree_join_code code_ctxt axis anode_test in
	(coerce_nodep fn coerce_unary_item_cursor_to_item_cursor), code_ctxt
      end
  | _ -> raise(Query(Code_Selection("Invalid physical operator in build_treejoin_code")))

(* A streamed variant can be selected if
   1) streaming is enabled
   2) the indep. subexpression's output is streamed.
   - Michael *)
let select_physical_op code_ctxt indep_signature algop (axis, anode_test) =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  if (proc_ctxt.streaming)
  then
    begin
      let t = access_one_non_discarded_xml_type indep_signature in
      match t with	      
      | PT_Sax PT_Stream-> POTreeJoin_Stream
      | PT_Dom _ -> POTreeJoin_NestedLoop
      | _ -> raise (Query (Prototype "Expected non-discarded (OneInput (PT_XML _)) in select_physical_op of AOETreeJoin."))
    end
  else
    let (sort_index_joinable, _) = is_sort_index_joinable code_ctxt axis anode_test in
    if sort_index_joinable then
      begin
	Format.printf ">>>\tPicked up INDEX SORT JOIN!!!\n\n";
	flush stdout;
	  (* 1. pre-compile the conditions *)
	let _ = raise (Query (Prototype "NOT READY FOR INDEX JOIN YET")) in
	POTreeJoin_Sort
      end
    else POTreeJoin_NestedLoop
