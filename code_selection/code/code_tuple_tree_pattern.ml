(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_tuple_tree_pattern.ml,v 1.13 2007/05/02 19:30:59 mff Exp $ *)

(* Module: Code_tuple_tree_pattern
   Description:
     This module contains code building for the TTP operator.
*)

open Error
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_physical_algebra_ast
open Code_selection_context
open Code_nestedloop
open Cs_util_coercion

open Optimization_judge

(* *)
let input_is_ordered code_ctxt algop =
  false

(* Means: 
    (1) indices in place 
    (2) only desc/child axes
    (3) input is sorted by document order and dupl-free
*)
let qualifies_for_twig_join code_ctxt op input treepattern =
  let indices_match = 
    try
      ignore(Code_util_tj.check_available_indices code_ctxt treepattern);
      true
    with _ -> false
  in
  let axis_array = get_axis_array treepattern in
  let axes_match = Array.fold_left 
      (fun b a -> 
	match a with  
	| Xquery_common_ast.Child | Xquery_common_ast.Descendant ->
	    b
	| _ -> false)
      true axis_array
  in
  let order_match = true (* fixme *) in
  if Debug.default_debug() then 
    begin
      Debug.print_default_debug "Checking if TTP qualifies for twig join :";
      Debug.sprintf_default_debug " - indices ok: %b" indices_match;
      Debug.sprintf_default_debug " - axes ok   : %b" axes_match;
      Debug.sprintf_default_debug " - order ok  : %b" order_match
    end;
  indices_match && axes_match && order_match


(* handle for creating indices w/o user interaction *)
let create_index_for_nametest code_ctxt qname =
  ()

let build_tuple_tree_pattern_code code_ctxt algop (input, pattern) = 
  let all_slots = get_all_outputs_from_twig_pattern pattern in
  let rec add_slots_to_ctxt ctxt slots =
    match slots with
    | [] -> ctxt
    | hd::tl -> 
	let new_ctxt = add_tuple_reference ctxt hd in
	add_slots_to_ctxt new_ctxt tl
  in
  let physop = Cs_util.get_physical_opname algop in 
  let _ = access_nosub algop.pdep_sub_expression in
  let _ = access_onesub algop.psub_expression in
  match physop with
  | POTupleTreePattern_TwigJoin output_fields -> 
      begin
	let new_ctxt = add_slots_to_ctxt code_ctxt (*output_fields*) all_slots in
	let fn = Code_tj_twigstack.build_holistic_tuple_tree_pattern_code new_ctxt input pattern in
	(coerce_unitdep fn () coerce_unary_tuple_cursor_to_tuple_cursor), new_ctxt
      end
  | POTupleTreePattern_SCJoin output_fields ->
      let _ = access_onesub algop.psub_expression in
      let new_ctxt = add_slots_to_ctxt code_ctxt (*output_fields*) all_slots in
      let fn = Code_sc_join.build_staircase_join_code new_ctxt input pattern in
      let _ = access_nosub algop.pdep_sub_expression in
      (coerce_unitdep fn () coerce_unary_tuple_cursor_to_tuple_cursor), new_ctxt
  | POTupleTreePattern_IndexSortJoin  output_fields ->
      raise (Query (Prototype "Index-sort join not supported for Twigs yet."))
  | POTupleTreePattern_Streaming  output_fields ->
      raise (Query (Prototype "Streaming not supported for Twigs yet."))
  | POTupleTreePattern_NestedLoop  output_fields ->
      begin
	let new_ctxt = add_slots_to_ctxt code_ctxt (*output_fields*) all_slots in
	let fn = build_default_tuple_tree_pattern_code new_ctxt input pattern in
	(coerce_unitdep fn () coerce_unary_tuple_cursor_to_tuple_cursor), new_ctxt
      end
  | _ -> raise(Query(Code_Selection("Invalid physical operator in build_tuple_tree_join_code")))

let select_physical_op code_ctxt algop (input, pattern) = 
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  let output_fields = Xquery_algebra_ast_util.get_restored_outputs_from_twig_pattern pattern in
  match proc_ctxt.Processing_context.treejoin_phys with
  | Processing_context.TwigJoin 
    when qualifies_for_twig_join code_ctxt algop input pattern ->
      POTupleTreePattern_TwigJoin output_fields
  | Processing_context.SCJoin 
    when true ->
       POTupleTreePattern_SCJoin output_fields
  | Processing_context.IndexSortJoin ->
      (raise (Query (Prototype "Index-sort join not supported for Twigs yet."))
       (* POTupleTreePattern_IndexSortJoin output_fields *))
  | Processing_context.Streaming ->
      (raise (Query (Prototype "Streaming not supported for Twigs yet."))
       (* POTupleTreePattern_Streaming output_fields *))
  | Processing_context.NestedLoop 
  | _ ->
      POTupleTreePattern_NestedLoop output_fields


