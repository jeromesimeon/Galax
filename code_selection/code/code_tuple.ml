(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_tuple.ml,v 1.17 2007/07/05 08:35:53 simeon Exp $ *)

(* Module: Code_tuple
   Description:
     This is the code for basic tuple operations.
*)

open Code_util_materialize
open Code_selection_context
open Execution_context

open Cs_util_coercion
open Cs_util
open Code_binding

open Physical_sequence
open Physical_value_util
open Physical_xml_value

open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_physical_algebra_ast
open Xquery_physical_type_ast

open Error


(* Tuple construction *)
let build_default_cecreatetuple_code_physical_type code_ctxt tuple_field_types physop =
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  (* Would not need these conversions if we had heterogenious coercion! Is
     AOECreateTuple ever applied to more than a single argument? - Michael *)
  let add_type_assertion odt =
    match odt with
    | None -> (fun x -> x)
    | Some dt ->
	(fun input_cursor -> Code_util_matching.dynamic_type_check stat_ctxt dt input_cursor)
  in
  let add_type_assertion_list odt =
    match odt with
    | None -> (fun x -> (item_list_of_physical_value (physical_value_of_xml_value x)))
    | Some dt ->
	(fun x ->
	  let input_cursor = item_cursor_of_xml_value x in
	  let output_cursor = Code_util_matching.dynamic_type_check stat_ctxt dt input_cursor in
	  Cursor.list_of_cursor "[build_default_cecreatetuple_code_physical_type]" output_cursor)
  in
  let build_bind_tuple_field_code code_ctxt (tn, xml_type) odt =
    match xml_type with
    | PT_Sax PT_Stream ->
	(fun xml_value -> build_bind_sax_value_to_tuple_field_code code_ctxt tn (sax_value_of_xml_value xml_value))
    | PT_Sax PT_Discarded ->
	(fun xml_value -> Streaming_ops.discard_typed_xml_stream (sax_value_of_xml_value xml_value))
    | PT_Dom PT_CursorSeq ->
	(fun xml_value -> build_bind_item_cursor_to_tuple_field_code code_ctxt tn ((add_type_assertion odt) (item_cursor_of_physical_value (physical_value_of_xml_value xml_value))))
    | PT_Dom PT_ListSeq ->
	(fun xml_value -> build_bind_item_list_to_tuple_field_code code_ctxt tn (add_type_assertion_list odt xml_value))
  in
  let ptb =
    match physop with
    | POCreateTuple tfbs -> tfbs
    | _ -> raise (Query (Code_Selection ("Invalid physical operator in build_create_tuple_code")))
  in
  let insert_code_list  = List.map2 (build_bind_tuple_field_code code_ctxt) ptb tuple_field_types in
  let insert_code_array  = Array.of_list insert_code_list in
  let n_names            = Array.length insert_code_array in
  let returned_value     = empty_tuple in
  (fun alg_ctxt arg_list ->
    if (Array.length arg_list) != n_names then
      raise (Query (Code_Selection ("Tuple Slot - Tuple Element mismatch")))
    else
      for i = 0 to n_names - 1 do
	insert_code_array.(i) arg_list.(i)
      done;
    returned_value
  )

let build_create_tuple_code code_ctxt algop names =
  (* Note:
     References for the new tuple must be allocated before
     building the tuple code.
  *)
  let tuple_field_names = List.map (fun (odt,name) -> name) (Array.to_list names) in
  let tuple_field_types = List.map (fun (odt,name) -> odt) (Array.to_list names) in
  let code_ctxt = List.fold_left add_tuple_reference code_ctxt tuple_field_names  in
  let physop = Cs_util.get_physical_opname algop in 
  let fn = build_default_cecreatetuple_code_physical_type code_ctxt tuple_field_types physop in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_many_xml_to_tuple), code_ctxt


(* Tuple field access *)
let build_default_ceaccesstuple_code code_ctxt rname =
  let retrieve_code = build_retrieve_tuple_code code_ctxt rname in
  (fun (alg_ctxt:algebra_context) () -> retrieve_code ())

let build_access_tuple_code code_ctxt algop crname1 =
  let fn = build_default_ceaccesstuple_code code_ctxt crname1 in
  let _ = access_nosub algop.pdep_sub_expression in
  (coerce_nodep fn coerce_unit_to_xml), code_ctxt

(* Tuple concatenation *)
(* Concatentation does not do anything currently (since tuples are compiled) *)
let build_default_tuple_concat_code code_ctxt =
  (fun alg_ctxt tup1 tup2 ->  empty_tuple )
    (*  concat_tuples tup1 tup2) *)

let build_tuple_concat_code code_ctxt algop =
  (* Concat tuple affects scoping since it is not symmetric
     let $x := [a:1]
     let $y := [a:2]
     1. (return $x ++ $y)#a => 1
     2. (return $y ++ $x)#a => 2 	   	   
   *)
  let fn = build_default_tuple_concat_code code_ctxt in
  let _ = access_nosub algop.pdep_sub_expression in
  (coerce_nodep fn coerce_binary_tuple_to_tuple), code_ctxt

(* Tuple product *)
let build_default_tuple_product_code code_ctxt =
  (* this needs to be of the dependent code *) 
  let annot = retrieve_annotation "build_default_tuple_product_code" code_ctxt in
  (* Need to retrieve_annotation for the left hand side *)
  let materialize_fun, restore_fn, needed_names = materialize_cursor_to_dom_value_array code_ctxt annot () in
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
  (fun () eval alg_ctxt left_side_cursor right_side_cursor ->
    (* Materialize the right hand side of the product *)
    let materialized_right = materialize_fun eval alg_ctxt right_side_cursor in
    (* Now for each tuple in the left hand side, evaluate the concatenation of the 
       two together *)
    let eval_concat tup1 =       
      (* Should return a cursor that "concatenates" each tuple of the left
	 with one on the right *)
      Cursor.cursor_of_function (our_cursor materialized_right)
    in
    Cursor.cursor_map_concat eval_concat left_side_cursor)

let build_tuple_product_code code_ctxt algop =
  let l, r = access_twosub algop.psub_expression in
  let _ = access_nosub algop.pdep_sub_expression in 
		(* We must store the annotation of the right hand side, the
		   reason is that this is the portion that is materialized...
		   This should probably be replaced. It seems we may need
		   to pass the op to the build code - not just the annotation *)
  let code_ctxt' = store_annotation code_ctxt r.compile_annotations in
  let fn = build_default_tuple_product_code code_ctxt' in
  (coerce_unitdep fn () coerce_binary_tuple_cursor_to_tuple_cursor), code_ctxt'

(* Tuple select *)
let build_default_tuple_select_code code_ctxt pd =
  (fun pred_exprs eval alg_ctxt input_cursor1 ->          
    let eval_pred tup =
      let eval_predicate pred_expr =
	 (Physical_util.get_boolean)
	    (item_cursor_of_physical_value 
	       (eval alg_ctxt pred_expr))
      in
      let rec eval_predicate_desc desc =
	match desc with
	  | SimpleConjunct (start_index, end_index) ->
	      let rec eval_conj cur_index end_index =
		if cur_index <= end_index then
		  ((eval_predicate pred_exprs.(cur_index))
		    &&  eval_conj (cur_index + 1) end_index)
		else
		  true
	      in
		eval_conj start_index end_index
	  | ComplexConjunct (l,r) ->
	      (eval_predicate_desc l)
		&& (eval_predicate_desc r)
	  | Disjunct (l,r) ->
	      (eval_predicate_desc l) 
	      || (eval_predicate_desc r)
      in
	eval_predicate_desc pd
    in
    Cursor.cursor_filter eval_pred input_cursor1)


let build_tuple_select_code code_ctxt algop pred =
  let dep = access_manysub algop.pdep_sub_expression in 
  let fn = build_default_tuple_select_code code_ctxt pred in
  (coerce_manydep fn dep coerce_unary_tuple_cursor_to_tuple_cursor), code_ctxt

(*******************************************)
(* We return the empty typle, because tuples
   have little (nothing now) to do with the
   return, they are in-memory assignments *)
(*******************************************)
let build_default_input_tuple_code code_ctxt =
  (fun alg_ctxt () -> empty_tuple )

let build_input_tuple_code code_ctxt algop =
  let _ = access_nosub algop.pdep_sub_expression in
  let fn = build_default_input_tuple_code code_ctxt in
  (coerce_nodep fn coerce_unit_to_tuple), code_ctxt


(* let build_materialize_table_code code_ctxt algop = *)
(*   let _ = access_nosub algop.pdep_sub_expression in  *)
(*   let fn = build_default_materialize_table_code code_ctxt in *)
(*   (coerce_unitdep fn () coerce_unary_tuple_cursor_to_tuple_cursor), code_ctxt *)

