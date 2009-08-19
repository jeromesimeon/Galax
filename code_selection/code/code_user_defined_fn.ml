(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_user_defined_fn.ml,v 1.16 2007/08/01 17:06:17 mff Exp $ *)

(* Module: Code_user_defined_fn
   Description:
     This module contains code building operations for user-defined
     functions.
*)

open Compile_context
open Cs_util
open Cs_util_coercion
open Code_selection_context
open Code_util_matching
open Namespace_names
open Error
open Physical_xml_value
open Physical_value_util
open Xquery_algebra_ast
open Xquery_algebra_ast_util

(* User defined wrapper for compiled functions *)

let build_default_user_defined_function
    code_ctxt fn_body_ref fn vars optintypes optdtm =
(* print_string ("Code selection for "^(Namespace_names.prefixed_string_of_rqname fn)^"\n"); *)
  if (Array.length optintypes) != (Array.length vars)
  then
    raise
      (Query
	 (Wrong_Args
	    ("Number of types and variables does not match for user defined function "
	     ^ (prefixed_string_of_rqname fn))));
  let arity = (Array.length optintypes) in
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let fn_code_block = 
    Code_selection_context.build_function_code code_ctxt (fn,arity) vars in
  let insert_code_array = fn_code_block.parameter_insertion_code in
  let enter_context     = fn_code_block.entrance_code  in
  let exit_context      = fn_code_block.exit_code      in
  (fun () eval alg_ctxt the_args -> 
    if arity != (Array.length the_args)
    then
      raise (Query (Wrong_Args ("Incompatible number of elements in function"
				^ (prefixed_string_of_rqname fn))));

    enter_context ();
    (* Perform the actual insert *)
    for i = 0 to arity - 1 do
      insert_code_array.(i) 
	(materialize_xml_value
	   (xml_value_of_item_cursor (
	    (dynamic_opttype_check stat_ctxt optintypes.(i) the_args.(i)))))
    done;
    begin
      (* For recursive function calls, this de-reference must occur at run-time, 
	 because at code-selection time, the function's body will not yet be defined. 
      *)
      let fn_body =
	match !fn_body_ref with
	| None -> raise(Query(Code_Selection(
			      "Physical plan for "^(Namespace_names.prefixed_string_of_rqname fn)^" not defined.")))
	| Some fn_body -> fn_body
      in
      (* Type match return value and return type *)
      let return_expr  = eval alg_ctxt fn_body in
      let return_value = 
	materialize_physical_value
	  (physical_value_of_item_cursor
	     (dynamic_opttype_check stat_ctxt (Some optdtm)
             (item_cursor_of_physical_value return_expr)))
      in
      (* Exit the context *)
      exit_context ();
      return_value
    end),

  (* Entry code for tail-recursive function calls *)
  
  (fun pv_args -> 
    let the_args = Array.map Physical_value_util.item_cursor_of_physical_value pv_args in 
    if arity != (Array.length the_args)
    then
      raise (Query (Wrong_Args ("Incompatible number of elements in function"
				^ (prefixed_string_of_rqname fn))));
    for i = 0 to arity - 1 do
      insert_code_array.(i) 
	(materialize_xml_value
	   (xml_value_of_item_cursor (
	    (dynamic_opttype_check stat_ctxt optintypes.(i) the_args.(i)))))
    done;
      (* For recursive function calls, this de-reference must occur at run-time, 
	 because at code-selection time, the function's body will not yet be defined. 
      *)
    let fn_body =
      match !fn_body_ref with
    | None -> raise(Query(Code_Selection(
                    "Physical plan for "^(Namespace_names.prefixed_string_of_rqname fn)^" not defined.")))
    | Some fn_body -> fn_body
    in fn_body),

  (* Exit code for tail-recursive function calls *)
  (fun pv ->
      (* Type match return value and return type *)
    let return_value = 
      materialize_physical_value
	(physical_value_of_item_cursor
	   (dynamic_opttype_check stat_ctxt (Some optdtm)
              (item_cursor_of_physical_value pv)))
    in
    return_value)

let build_user_defined_fn_code code_ctxt algop ((fname,arity), optintypes, outtype) = 
	      (* Bind to the function *)	
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let dep = access_unitsub algop.pdep_sub_expression in 
  let (fn, entry, exit) = 
    try
      let func_defn = get_function_from_compile_context "build_user_defined_fn" comp_ctxt (fname,arity) in
      build_default_user_defined_function code_ctxt func_defn.palgop_func_physical_plan
	fname func_defn.palgop_func_formal_args optintypes outtype
    with
      (* We should only end up here if an external function does not have a binding *)
    | Query(Undefined _) -> 
	(fun () eval alg_ctxt the_args -> 
	  raise(Query(Code_Selection("Execution code for external function "^
				     (Namespace_names.curly_uri_string_of_rqname fname)^" undefined")))),
     
	(fun the_args -> 
	  raise(Query(Code_Selection("Execution code for external function "^
				     (Namespace_names.curly_uri_string_of_rqname fname)^" undefined")))),
     
	(fun pv -> 
	  raise(Query(Code_Selection("Execution code for external function "^
				     (Namespace_names.curly_uri_string_of_rqname fname)^" undefined"))))
     
  (* Here's where we should be accessing the _physical_plan_ for
     fname, not the logical plan, and passing it to
     build_default_user_defined_function. 
  *)
  in
  let coerce_unitdep input_code entry_code exit_code () coercion_fun =
    Algebra_type.SomeDep ((fun ef ->
      let f' = input_code () ef in
      coercion_fun f'), Some (entry_code, exit_code))
  in
  ((coerce_unitdep fn entry exit dep coerce_many_item_cursor_to_physical_value), code_ctxt)

