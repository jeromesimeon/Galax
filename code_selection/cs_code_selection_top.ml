(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cs_code_selection_top.ml,v 1.31 2007/10/25 00:08:41 mff Exp $ *)

(* Module: Cs_code_selection_top
   Description:
     This module is the entry-point to code-selection.
*)

open Error 

open Xquery_common_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util


open Algebra_type
open Cs_util_coercion

open Code_group_order
open Code_update
open Cs_code_top
open Cs_code_selection_expr

open Compile_context
open Code_selection_context

open Code_typing_context 
open Cs_code_typing_top

(**************)
(* Statements *)
(**************)

let default_code_selection_statement module_uri code_ctxt ae =
  (* Physical typing sub-phase *)
  let ctc'       = Cs_code_typing_top.code_typing_statement code_ctxt ae in
  (* For a top-level statement, we store the compilation annotations
     as both global and local annotations *)
  Debug.print_dxq_debug("In default_code_selection_statement; "^(Print_xquery_algebra.bprintf_logical_algstatement "" ae)^"\nAnnotation is: "^
			(match ae.compile_annotations with None -> "None" | _ -> "Some")^"\n");
  let code_ctxt = store_annotation code_ctxt ae.compile_annotations in
  let code_ctxt =
    store_global_annotation code_ctxt ae.compile_annotations in
  let code_ctxt = replace_code_type_context_in_code_selection_context ctc' code_ctxt in
  let code_ctxt = enter_statement_context code_ctxt in 
  let code_ctxt = default_code_selection module_uri code_ctxt ae in
  let _ = exit_statement_context code_ctxt in 
  code_ctxt


(***********************)
(* Prolog declarations *)
(***********************)

(* Code Selection for User Defined Functions:
   - Walk the function and allocate fresh variables
   (after allocating some for the params )
   - Allocate a new variable context for the function and associate it *)

(* 
  This is where we should be storing the physical plan for a function
*)
let default_code_selection_function comp_prog module_uri code_ctxt fn_decl =
  let (((name, arity) as fname), _, func_defn, _) = fn_decl.palgop_function_decl_desc in
  Debug.print_compile_debug ("\t\tCode selecting functions " ^ (Namespace_names.prefixed_string_of_rqname name));
  match !(func_defn.palgop_func_optimized_logical_plan) with
  | AOEFunctionImported ->
      begin
(*print_string ("Imported\n"); *)
      let (_, uri, _) = name in 
      try
	let imported_mod = 
	  Compiled_program_units.module_of_compiled_program comp_prog (Namespace_names.string_of_uri uri) in 
	let imported_code_ctxt = Compiled_program_units.code_selection_context_of_module imported_mod in 
	add_imported_function_context code_ctxt (name, arity) imported_code_ctxt;
	code_ctxt
      with
      |	exn ->
	raise(Query(Internal_Error(Error.bprintf_error ("Function "^(Namespace_names.prefixed_string_of_rqname name)^" not in compile context.;") exn)))
      end
  | AOEFunctionUser alg_expr ->
(* print_string ("User-defined \n"); *)
      let vc = enter_function_context code_ctxt (name,arity) func_defn.palgop_func_formal_args in
      (* Make a copy for code selection *)
      let phys_alg_expr = Xquery_algebra_ast_annotation_util.deep_copy_expr alg_expr  in

      (* Here we should reset the counter for functions *)
      let vc = store_global_annotation vc phys_alg_expr.compile_annotations in
      let vc = default_code_selection module_uri vc phys_alg_expr in (* Decorate it*)

      (*  func_defn.palgop_func_optimized_logical_plan := alg_expr; *)
      (* ==> update_physical_plan below does this, why is it repeated here?
         func_defn.palgop_func_physical_plan := Some phys_alg_expr; *)

      let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in 
      if Compile_context.mem_function_from_compile_context comp_ctxt fname 
      then Compile_context.update_physical_plan_in_compile_context comp_ctxt fname phys_alg_expr
      else raise(Query(Code_Selection("Function "^(Namespace_names.prefixed_string_of_rqname name)^" not in compile context.")));
      begin
	exit_function_context vc (name,arity);
	code_ctxt
      end

let single_op_default_code_selection_decl comp_prog module_uri code_ctxt algop =
  (* The algebraic operator contains the location that should be
     correlated with errors, so we catch any exceptions here and
     rewrap the exceptions with the location. *)
  try
  (* Modify the code *)
    let eval_code, csc =
      match algop.alg_decl_name with
      | AOEVarDecl (ocdt,vname) ->
          (* If we want to evaluate global variables lazily, we need
             to cache the plan associated with the global variable
             (algop) so that it can be executed on demand. Most
             likely, this should go in the code_ctxt. -Mary & Kristi
	  *)
	  let code_ctxt = add_variable_to_current_context code_ctxt vname in
	  let fn = build_default_var_decl_code code_ctxt ocdt vname in
	  let _ = access_nosub algop.alg_decl_dep in 
	  (coerce_nodep_prolog fn coerce_unary_item_cursor_to_algebra_context), code_ctxt
      | AOEVarDeclExternal (ocdt,vname) ->
	  let code_ctxt = declare_external_variable code_ctxt vname in 
	  (*	let code_ctxt =  add_variable_to_current_context code_ctxt vname in *)
	  let fn = build_default_var_decl_external_code code_ctxt ocdt vname in
	  let _ = access_nosub algop.alg_decl_dep in 
	  (coerce_nodep_prolog fn coerce_unit_to_algebra_context), code_ctxt

      | AOEVarDeclImported (ocdt,vname) ->
	  let (_, uri, _) = vname in 
	  let imported_mod = 
	    Compiled_program_units.module_of_compiled_program comp_prog (Namespace_names.string_of_uri uri) in 
          let imported_code_ctxt = Compiled_program_units.code_selection_context_of_module imported_mod in 
	  add_imported_variable_context code_ctxt vname  imported_code_ctxt; 
	  let fn =  (fun alg_ctxt () -> alg_ctxt) in
	  let _ = access_nosub algop.alg_decl_dep in 
	  (coerce_nodep_prolog fn coerce_unit_to_algebra_context), code_ctxt

      | AOEValueIndexDecl str ->
	  let code_ctxt = add_variable_to_current_context code_ctxt Xquery_common_ast.fs_dot in
	  let fn = build_default_key_decl_code code_ctxt str in
	  let dep = access_onesub algop.alg_decl_dep in 
	  (coerce_onedep_prolog fn dep coerce_unary_item_cursor_to_algebra_context), code_ctxt
      | AOENameIndexDecl ename ->
	  let code_ctxt = create_new_name_index code_ctxt ename in
	  let fn = build_default_name_index_decl_code code_ctxt in
	  let _ = access_nosub algop.alg_decl_dep in 
	  (coerce_nodep_prolog fn coerce_unit_to_algebra_context), code_ctxt
    in
    algop.alg_decl_eval := eval_code;
    csc
  with
  | exn -> raise (Error.error_with_file_location algop.alg_decl_loc exn)
    
let default_code_selection_decl comp_prog module_uri code_ctxt algop =
  let sub_exprs = algop.alg_decl_indep in
  let dep_sub_exprs = algop.alg_decl_dep in
  (* Selects the code for the subexpressions *)
  let code_ctxt = sub_expr_default_code_selection module_uri code_ctxt sub_exprs in
  let code_ctxt = single_op_default_code_selection_decl comp_prog module_uri code_ctxt algop in    
  (* Selects the code for the dependant subexpressions *)
  let code_ctxt = sub_expr_default_code_selection module_uri code_ctxt dep_sub_exprs in
  (* Selects the code for the current expression *)
  code_ctxt


(**********)
(* Prolog *)
(**********)

let default_code_selection_prolog comp_prog module_uri code_ctxt prolog =
  Debug.print_compile_debug "Starting code selection of prolog";
  let code_ctxt = enter_prolog_context code_ctxt in
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in 
  let prolog',comp_ctxt = Cs_annotate.annotate_prolog_with_bindings comp_ctxt prolog in

  (* I'm not happy having physical typing wedged here, but I think it
     has to follow the "annotation" phase, which takes a logical
     algebraic op and returns a dummy physical operator that is
     elaborated during physical typing and code selection. -Mary *)

  (* Physical typing sub-phase *)
  Debug.print_compile_debug "\tStarting code typing of prolog";
  let ctc' = Cs_code_typing_top.code_typing_prolog code_ctxt prolog' in
  Debug.print_compile_debug "\tFinished code typing of prolog";
  let code_ctxt = replace_code_type_context_in_code_selection_context ctc' code_ctxt in
  (* Functions currently should not modify the code selection context
     -> this is because each of them should be starting an independent
     variable context *)
  Debug.print_compile_debug "\tCode selecting variables";
  let code_ctxt =
    List.fold_left (default_code_selection_decl comp_prog module_uri) code_ctxt prolog'.palgop_prolog_vars
  in
  Debug.print_compile_debug "\tCode selecting indices";
  let code_ctxt =
    List.fold_left (default_code_selection_decl comp_prog module_uri) code_ctxt prolog'.palgop_prolog_indices
  in
  (* Functions may refer to variables *)
  Debug.print_compile_debug "\tCode selecting functions";
  let code_ctxt =
    List.fold_left (default_code_selection_function comp_prog module_uri) code_ctxt prolog'.palgop_prolog_functions
  in
  let _ = exit_prolog_context code_ctxt in
  Debug.print_compile_debug "Finished code selection of prolog";
  code_ctxt, prolog'


(***********)
(* Modules *)
(***********)

let default_code_selection_module comp_prog module_uri code_ctxt cm = 
  let code_ctxt,prolog =
    default_code_selection_prolog comp_prog module_uri code_ctxt cm.palgop_module_prolog
  in
  let comp_ctxt  = annotated_compile_context_from_code_selection_context code_ctxt in 
  (* Physical annotation sub-phase *)
  let statements =
    List.map (Cs_annotate.annotate_statement comp_ctxt) cm.palgop_module_statements
  in
  let (code_ctxt) =
    List.fold_left (default_code_selection_statement module_uri) code_ctxt statements
  in
  code_ctxt, (fmkalgop_xmodule prolog statements)


(*************)
(* Top-level *)
(*************)

let code_selection_statement (comp_prog, module_uri, code_ctxt) ae =
  let comp_ctxt  = annotated_compile_context_from_code_selection_context code_ctxt in 
  (* Physical annotation sub-phase *)
  let ae = Cs_annotate.annotate_statement comp_ctxt ae in
  let code_ctxt'  = default_code_selection_statement module_uri code_ctxt ae 
  in (code_ctxt', ae)

let code_selection_prolog (comp_prog, module_uri, code_ctxt) prolog =
  default_code_selection_prolog comp_prog module_uri code_ctxt prolog

let code_selection_module (comp_prog, module_uri, code_ctxt) cm =
  default_code_selection_module comp_prog module_uri code_ctxt cm

