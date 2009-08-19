(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cs_code_top.ml,v 1.19 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Cs_top_code
   Description:
     This module contains the actual caml code that is used for
     evaluation of the prolog and closure parts of the algebra.
*)

open Error
open Namespace_names

open Xquery_core_ast
open Xquery_common_ast
open Algebra_type

open Norm_context
open Typing_context
open Compile_context
open Code_selection_context
open Execution_context

open Physical_value
open Physical_item
open Physical_sequence
open Physical_value_util 
open Physical_table

open Cs_util


(* Global variable declaration *)

let build_default_var_decl_code code_ctxt odt vn =
  let add_fn = build_add_var_item_cursor_safe code_ctxt vn in
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  (fun  alg_ctxt input_cursor -> 
Debug.print_dxq_debug ("Stuffing value for "^(Namespace_names.prefixed_string_of_rqname vn));
    let ndt = Code_util_matching.dynamic_opttype_check stat_ctxt odt input_cursor in
      (*add_var_item_cursor_to_algebra_context alg_ctxt (vn,ndt)*)
      add_fn ndt;
      alg_ctxt
  )
(* The array will not yet be allocated. So we must wait until execution to 
   store the value in the context. However we can do the type checking stuff 
   now.
*)
let build_default_var_decl_external_code code_ctxt odt vn=
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let fn_value  = get_external_variable_fn_value code_ctxt vn in
  let add_fn = build_add_var_item_cursor_safe code_ctxt vn in

  (fun alg_ctxt () ->
    let value = !fn_value () in
    let n = Cursor.cursor_of_list value in 
    let ndt = Code_util_matching.dynamic_opttype_check stat_ctxt odt n in       
       (* construct the fn. *)
    add_fn ndt;
    alg_ctxt
  )


(* Key declarations *)

let build_default_key_decl_code code_ctxt kn =
  let add_fn =
    build_add_var_xml_value_safe code_ctxt Xquery_common_ast.fs_dot
(* BUG: The 'unsafe' versions do not seem to work in the prolog! - Jerome
    build_add_var_xml_value_unsafe_allowed code_ctxt Xquery_common_ast.fs_dot *)
  in
  (fun ea2 eval alg_ctxt n1 ->
    let process_e2 alg_ctxt n1 =
      let n2 =
	(* Extends the dynamic environment for '.' *)
	add_fn (Physical_value.DomValue (sequence_of_singleton n1));
	(* Evaluates the key expression *)
	dom_value_of_physical_value (eval alg_ctxt ea2)
      in
      let keyval =
	try
	  if (sequence_is_empty n2) then 
	    ""
	  else
	    (* Mary ERROR HANDLING : This is a hack! *)
	    let i = (sequence_get_singleton n2) in
	    match item_kind i with 
	    | NodeKind -> (getNode i)#string_value()
	    | AtomicValueKind -> (getAtomicValue i)#erase_atomic_value()
	with
	| _ ->
	    raise (Query (Code_Selection "key definition: key value not an item"))
      in
      add_key_to_algebra_context alg_ctxt (kn,keyval) [n1]
    in
    Cursor.cursor_fold_left process_e2 alg_ctxt n1)

let build_default_name_index_decl_code code_ctxt =
  (fun alg_ctxt () ->
    alg_ctxt
  )

let build_default_twig_index_decl_code code_ctxt =
 (fun alg_ctxt () ->
    alg_ctxt
  )

(* Function extend_var_context takes one (variable, value) pair,
   stores it in execution context, and returns modified context.
*)
let extend_var_context (code_ctxt, execution_ctxt) (vname, resolved_xml_stream) = 
  let store_var_value = build_default_var_decl_code code_ctxt (None) vname in
  let boxed_xml_stream = Streaming_ops.typed_of_resolved_xml_stream resolved_xml_stream in
  let item_list = Planio_physical_value.unbox_and_load_item_sequence boxed_xml_stream in 
  Debug.print_dxq_debug ("After unboxing var value\n");
      (* 2. Store the variable's value *)
  let execution_ctxt' = store_var_value execution_ctxt (Cursor.cursor_of_list item_list) in
  Debug.print_dxq_debug ("After stuffing var "^(Namespace_names.prefixed_string_of_rqname vname)^" in execution_context\n@?");
      (* 3. Give physical type of value *)
  let ctc' = Code_typing_context.add_variable_type 
      (code_type_context_from_code_selection_context code_ctxt) vname Xquery_physical_type_ast_util.dom_list_type in
  let code_ctxt' = replace_code_type_context_in_code_selection_context ctc' code_ctxt in
  (code_ctxt', execution_ctxt')

let extend_tuple_context code_ctxt (tn, resolved_xml_stream) = 
  let fn_store = build_create_tuple_code code_ctxt tn in
  let boxed_xml_stream = Streaming_ops.typed_of_resolved_xml_stream resolved_xml_stream in
  let item_list = Planio_physical_value.unbox_and_load_item_sequence boxed_xml_stream in 
  Debug.print_dxq_debug ("After unboxing tuple field\n");
      (* 2. Store the tuple field's value *)
  let _ = fn_store (Physical_xml_value.xml_value_of_item_list item_list) in
  Debug.print_dxq_debug ("After stuffing field "^
			 (Namespace_names.prefixed_string_of_rqname tn)^" in execution_context\n@?");
  (* For now, all inputs are materialized *)
  (tn, Xquery_physical_type_ast_util.dom_list_type)

