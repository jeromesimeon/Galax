(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_join.ml,v 1.11 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_join
   Description:
     This module contains code building for operators that implement
     joins.
*)

open Error 

open Xquery_common_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_physical_type_ast

open Algebra_type
open Code_util_materialize
open Cs_util_coercion
open Cs_util

open Compile_context
open Code_selection_context   

open Physical_item 

type default_code_selection_type = (code_selection_context -> Algebra_type.algop_expr -> code_selection_context)

let build_default_tuple_join_code code_ctxt pred_desc =
  (* Annotation for the side we are materializing *)
  let annot = retrieve_annotation "build_default_tuple_join_code" code_ctxt in 
  let materialize_fun, restore_fn, needed_names = materialize_cursor_to_dom_value_array code_ctxt annot () in
  
  (fun conds eval alg_ctxt left_side_cursor right_side_cursor ->
       (* Materialize the right hand side of the product *)
    let materialized_right  = materialize_fun eval alg_ctxt right_side_cursor in
    let materialized_length = Array.length materialized_right in

	 (* Right hand cursor *)
    let right_hand_cursor () = 
      let offset = ref 0 in
      (fun () ->
	let bfound = ref false in
	while (!offset < materialized_length) && (not !bfound) do
	  restore_fn materialized_right.(!offset);
	  bfound := (Code_util_predicates.eval_predicate_desc eval alg_ctxt conds pred_desc);  
	  incr offset
	done;
	if !bfound then
	  Some empty_tuple
	else
	  None	 
	    )
    in
	 (* Now for each tuple in the left hand side, evaluate the concatenation of the 
	    two together *)
    let eval_concat tup1 =       
	 (* Should return a cursor that "concatenates" each tuple of the left
	    with one on the right *)
      Cursor.cursor_of_function 
	(right_hand_cursor ())
    in
    Cursor.cursor_map_concat eval_concat left_side_cursor)

(************************)
(* Left outer Join code *)
(************************)

let build_left_outer_tuple_join_code code_ctxt vn pred_desc = 
  let loj_null_fn      = build_create_dom_tuple_code code_ctxt vn in
    (* Annotation for the side we are materializing *)
  let annot = retrieve_annotation "build_left_outer_tuple_join_code" code_ctxt in 
  let materialize_fun, restore_fn, needed_names = materialize_cursor_to_dom_value_array code_ctxt annot () in
  let empty_right = 
    Array.make (Array.length needed_names) empty_dom_sequence in
  
  (fun conds eval alg_ctxt left_side_cursor right_side_cursor ->
       (* Materialize the right hand side of the product *)
    let materialized_right  = materialize_fun eval alg_ctxt right_side_cursor in
    let materialized_length = Array.length materialized_right in

	 (* Right hand cursor *)
    let right_hand_cursor () = 
      let offset = ref 0 in
      (fun () ->
	let bfound = ref false in
	let start_offset = !offset in
	while (!offset < materialized_length) && (not !bfound) do
	  restore_fn materialized_right.(!offset);
	  bfound := (Code_util_predicates.eval_predicate_desc eval alg_ctxt conds pred_desc);   
	  incr offset
	done;
	if !bfound then
	  begin
	    loj_null_fn non_empty_dom_sequence;
	    Some empty_tuple
	  end
	else if start_offset = 0 then (* we made it the whole way without a match*)
	  begin
		    (* in case !offset = 0, without this modification, we get an infinite loop - Chris *)
	    incr offset;
	    loj_null_fn empty_dom_sequence;
	    restore_fn empty_right;
	    Some empty_tuple
	  end
	else
	  None
	    )
    in
	 (* Now for each tuple in the left hand side, evaluate the concatenation of the 
	    two together *)
    let eval_concat tup1 =
      Cursor.cursor_of_function 
	(right_hand_cursor ())
    in
    Cursor.cursor_map_concat eval_concat left_side_cursor)

let build_join_code code_ctxt algop default_code_selection =
  let _     = access_manysub algop.pdep_sub_expression in
  let l, r  = access_twosub algop.psub_expression in 
  let code_ctxt = store_annotation code_ctxt r.compile_annotations in
  let (logical_join_kind, pred_desc) = 
    match algop.palgop_expr_name with
    | AOEJoin pred_desc -> (Code_util_join.StandardJoin, pred_desc)
    | AOELeftOuterJoin (null_name,pred_desc) -> (Code_util_join.OuterJoin null_name, pred_desc)
    | _ -> raise(Query(Internal_Error("Attempting to select join code for non-join algebraic operator")))
  in
  match Xquery_algebra_ast_util.pname_of_algop algop with
  | Xquery_physical_algebra_ast.POJoin_Hash
  | Xquery_physical_algebra_ast.POLeftOuterJoin_Hash _ ->
      begin
	(* 1. pre-compile the conditions *)
	let built_cond =
	  Code_util_join.build_hash_cond
	    default_code_selection 
	    code_ctxt
	    algop
	in
	(* 2. build the hash join code *)
	Code_hash_join.build_hash_join
	  logical_join_kind
	  code_ctxt
	  built_cond
	  pred_desc
      end
  | Xquery_physical_algebra_ast.POJoin_Sort
  | Xquery_physical_algebra_ast.POLeftOuterJoin_Sort _ ->
      begin
	let built_cond =
	  Code_util_join.build_sort_cond
	    default_code_selection
	    code_ctxt
	    algop
	in
	(* 2. build the sort join code *)
	Code_sort_join.build_sort_join
	  logical_join_kind
	  code_ctxt
	  built_cond
	  pred_desc
      end
  | Xquery_physical_algebra_ast.POJoin_NestedLoop
  | Xquery_physical_algebra_ast.POLeftOuterJoin_NestedLoop _ ->
      begin
	(* Store the annotation of what needs to be materialized *)
	let fn = 
	  match logical_join_kind with
	  | Code_util_join.StandardJoin ->
	      build_default_tuple_join_code code_ctxt pred_desc 
	  | (Code_util_join.OuterJoin null_name) -> 
	      build_left_outer_tuple_join_code code_ctxt null_name pred_desc
	in
	let dep   = access_manysub algop.pdep_sub_expression in
	(coerce_manydep fn dep coerce_binary_tuple_cursor_to_tuple_cursor), code_ctxt
      end
    | _ -> raise(Query(Internal_Error("Invalid physical operator for logical join operator")))
