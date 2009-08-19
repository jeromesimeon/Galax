(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_nestedloop.ml,v 1.15 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_nestedloop
   Description:
     This module contains code building for operators that implement
     nested loops.
*)

open Error
open Xquery_algebra_ast
open Xquery_algebra_ast_util

open Algebra_type
open Code_selection_context
open Execution_context

open Datatypes
open Dm
open Dm_util
open Dm_types
open Dm_atomic

open Physical_value
open Physical_name_index
open Physical_sequence
open Physical_value_util
open Physical_table
open Physical_item

open Code_util
open Code_util_xpath

open Cs_util
open Cs_util_coercion
open Code_util_materialize

(* ************************* *)
(* *** Utility functions *** *)
(* ************************* *)

(* Duplicate elimination over tuples :
 * Used for applying SBDO after nested-loop style TupleTreePattern
 * evaluation.
 * PreCondition: Input tuples must be sorted on the field that the
 *               duplicate elimination works on.
 *)

let tuple_dupl_elim input_cursor code_ctxt retrieve_code =
  let buffer = ref None in
  let filter_fun t =
    let next_node  = getNode (Physical_util.get_item (cursor_of_sequence (retrieve_code ()))) in
    let next_node_id = next_node#nodeid() in
    let ret = 
      match !buffer with
      | Some b 
	when Nodeid.nodeid_is b next_node_id -> false
      | _ ->  true
    in
    buffer := Some next_node_id;
    ret
  in
  Cursor.cursor_filter filter_fun input_cursor

let build_tuple_nodup_code code_ctxt tn =
  let retrieve_code = build_retrieve_dom_tuple_code code_ctxt tn in
  (fun  alg_ctxt (input_cursor : tuple_unit Cursor.cursor) ->
     tuple_dupl_elim input_cursor code_ctxt retrieve_code
  )

(* DocOrder codequeries/straightlinepath-02.xq
 * This function generates materialization and sorting-by-docorder
 * code for the nested-loop style evaluation of twig patterns.
 * -- philippe
 *)
let build_docorder_code code_ctxt tn =
  let annot = retrieve_annotation "build_docorder_code" code_ctxt in
  let materialize_fun, restore_fn, needed_names = materialize_cursor_to_dom_value_array code_ctxt annot () in
  let index = ref (-1) in
  let _ =
    for i = 0 to ((Array.length needed_names) -1) do
      if needed_names.(i) = tn then index := i
    done
  in
  let sort_fun tup1 tup2 =
    let node1 = getNode (Physical_util.get_item (cursor_of_sequence (tup1.(!index)))) in
    let node2 = getNode (Physical_util.get_item (cursor_of_sequence (tup2.(!index)))) in
    node_compare node1 node2
  in
  if !index < 0 then 
    raise (Query (Code_Selection ("Tuple Slot not found during code selection")))
  else
    (* REALLY REALLY ;-) Should be a common operation.. *)
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
    (fun () (eval:eval_fun)  (alg_ctxt: algebra_context) input_table ->     
      let materialized = materialize_fun eval alg_ctxt input_table in
      Array.stable_sort sort_fun materialized;
      Cursor.cursor_of_function (our_cursor materialized)
    )

(* debug messaging *)
let debug_print_tj i o axis nt =
  begin
  if Debug.default_debug() then
    Format.fprintf !Conf.algebra_optimization_rewrite_formatter 
      "@?**** Generating TTJ code: TupleTreeJoin[(%a,%a)][%a,%a](...)\n"
      Print_common.print_rqname i 
      Print_common.print_rqname o 
      Print_common.print_axis axis
      Print_xquery_algebra.print_anode_test nt;
  end

(* Some errors *)
let leaf_code_error = 
  (Query 
     (Code_Selection 
	"[TupleTreePattern] Attempt to generate code for leaf node."))
and node_test_error =
  (Query
     (Code_Selection 
	"[TupleTreePattern] No node test for non-root pattern node (None)."))
and out_field_error =
  (Query 
     (Code_Selection 
	"[TupleTreePattern] No output field for pattern node (None)."))

(* Effective boolean value code for use in Twig code
 * Note: factor out this code from cs_code_fn 
 * --philippe
 *)
let empty_string = new atomicString "" 
and zero_integer = new atomicInteger Decimal._integer_zero
and zero_decimal = new atomicDecimal Decimal._decimal_zero
and zero_float   = new atomicFloat 0.0
and zero_double  = new atomicDouble 0.0

let effective_boolean_value p1 =
  if (Cursor.cursor_is_empty p1)
  then 
    false
  else
    if (Cursor.cursor_is_singleton p1) 
    then 
      let item = Physical_util.get_item p1 in 
      if (Physical_item_util.isAtomicValue item)
      then
	let a = Physical_item.getAtomicValue item in
	match a#getAtomicValueKind() with
	| ATBoolean -> a#getAtomicBoolean()
	| ATString  -> not (a#atomic_value_eq empty_string)
	| ATInteger -> not (a#atomic_value_eq zero_integer)
	| ATDecimal -> not (a#atomic_value_eq zero_decimal)
	| ATFloat   -> not (a#atomic_value_eq zero_float)
	| ATDouble  -> not (a#atomic_value_eq zero_double)
	| _ -> true
      else 
	true
    else
      true

(*
 * TupleTreeJoin Code :
 * Builds the default code for one twig of a TupleTreePattern.
 * NOTE: this evaluation strategy corresponds with nested-loop.
 * -- philippe
 *)

let build_default_tuple_tree_join_code code_ctxt input_field output_field axis anode_test =
  let retrieve_code = build_retrieve_dom_tuple_code code_ctxt input_field in
  let restore_fn = build_create_dom_tuple_code code_ctxt output_field in
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let eval_node_test_fun = eval_axis_node_test stat_ctxt axis anode_test in
  (fun () eval alg_ctxt input_cursor ->
    let eval_axis_single_tuple tup =
      match Physical_util.get_optional_item (cursor_of_sequence (retrieve_code ())) with
      | Some item ->
	  begin
	    let complete_node_cursor =
	      match item_kind item with
	      | NodeKind ->
		  eval_node_test_fun (getNode item)
	      | AtomicValueKind ->
		  raise (Query (Type_Error "Applying a step on an atomic value"))
	    in
	    Cursor.cursor_map 
	      (fun x -> 
		begin 
		  restore_fn (materialized_of_list [(Item_Node x)]); 
		  empty_tuple 
		end) 
	      complete_node_cursor
	  end
      | None -> 
	  Cursor.cursor_empty ()
    in
    Cursor.cursor_map_concat eval_axis_single_tuple input_cursor)

(* *************************************************************
 *  TupleTreePattern Default Code Selection
 *
 *  The default code selection for TupleTreePatterns should
 *  correspond with the code selection of its composing TreeJoin
 *  steps (including those for the predicates).
 *  NOTE: duplicated code for effective boolean value for 
 *        convenience. Consider exporting that functionality 
 *        in cs_code_fn.mli
 *  -- Philippe
 * ************************************************************ *)

let rec build_default_twig_code 
    (code_ctxt:code_selection_context) 
    (input:Namespace_names.rqname) 
    (step_data: (Namespace_names.rqname * Xquery_common_ast.axis * anode_test) option) 
    (pattern:twig_pattern) 
    (index:int)
    = 
  let input' = 
    if index = 0 then input
    else
      match pattern.(index).out with
      | Some o -> o
      | None -> raise out_field_error
  in
  let (sigma, delta) = pattern.(index).requires_sbdo in

  (* step 1 : generate code for a single step -- unless we are in the root *)
  let step_code = 
    match step_data with
    | None -> (fun () eval alg_ctxt curs -> curs) (* root -> end of recursion *)
    | Some (output, axis, nt) ->
	let _ = debug_print_tj input output axis nt in 
	let docord = 
	  if sigma 
	  then build_docorder_code code_ctxt output 
	  else (fun () eval alg_ctxt curs -> curs) in
	let nodup  = 
	  if delta 
	  then build_tuple_nodup_code code_ctxt output 
	  else (fun alg_ctxt curs -> curs) in
	let bare_step =
	  build_default_tuple_tree_join_code code_ctxt input output axis nt 
	in
	fun dep eval alg_ctxt cursor ->
	  begin
	  if !Conf.aggressive_sbdo_remove then
	    bare_step () eval alg_ctxt cursor 
	  else
	    (nodup alg_ctxt
	       (docord () eval alg_ctxt
		  (bare_step () eval alg_ctxt cursor))) 
	  end
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
      let (output',axis',nt') = get_treejoin_attrs pattern (typ', index') in
      let nested_step_code = 
	build_default_twig_code code_ctxt input' (Some (output',axis',nt')) pattern index' 
      in
(*      let docord = build_docorder_code code_ctxt output' in
      let nodup  = build_tuple_nodup_code code_ctxt output' in *)
      fun () eval alg_ctxt cursor -> 
	(nested_step_code () eval alg_ctxt 
	   (step_pred_code () eval alg_ctxt cursor))

and get_treejoin_attrs pattern (twigtype,index) =
    let axis = twigtype in
    let nt = 
      match pattern.(index).node_test with
      | Some n -> n
      | None -> raise node_test_error
    in
    let output =
      match pattern.(index).out with
      | Some o -> o
      | None -> raise out_field_error
    in  
    (output, axis, nt)

(* Build the code for a list of chained predicates rooted at 'index' *)
and build_predicates code_ctxt input pattern index =
  let predicate_list = pattern.(index).pred_twigs in
  let build_single_predicate (typ, pred_index)  =
    begin
      let (output, axis, nt) = get_treejoin_attrs pattern (typ, pred_index) in
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
	      effective_boolean_value
		(Cursor.cursor_map_concat 
		   (fun x -> cursor_of_sequence (retrieve_code ())) new_curs)
	    end
      | None -> raise out_field_error
    end
  in
  let pred_funs = List.map build_single_predicate predicate_list in
  fun () eval alg_ctxt tuple ->
    List.for_all (fun p -> p () eval alg_ctxt tuple) pred_funs

  
(*
 * Default TupleTreePattern Code (=nested loop)
 *)
let build_default_tuple_tree_pattern_code 
    (code_ctxt:code_selection_context) 
    (input:Namespace_names.rqname) 
    (pattern:twig_pattern) 
    =
      build_default_twig_code code_ctxt input None pattern 0

let build_distinct_code code_ctxt algop field = 
  let _ = access_onesub algop.psub_expression in
  let _ = access_nosub algop.pdep_sub_expression in
  let fn = build_tuple_nodup_code code_ctxt field in
  (coerce_nodep fn coerce_unary_tuple_cursor_to_tuple_cursor), code_ctxt
