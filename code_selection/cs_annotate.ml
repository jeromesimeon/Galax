(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cs_annotate.ml,v 1.19 2007/07/13 18:24:42 mff Exp $ *)

(* Module: Cs_annotate
   Description:
     This module implements algebraic annotation for the physical
     layer.
*)

(* NOTE:
   Annotation is really a separate 'sub-phase', that deals with
   annotations needed for physical operators, notably it decides on
   which physical types should be used for each operator.
*)

open Algebra_type

open Xquery_algebra_ast
open Xquery_algebra_ast_util

(***************************)
(* Physical typing context *)
(***************************)

type return_env =
    { contains_side_effect   : bool;
      contains_delta_update  : bool;
      contains_snapped_delta : bool }

let mk_renv cse csd cdu = 
  { contains_side_effect   = cse;
    contains_snapped_delta = csd;
    contains_delta_update  = cdu }

let make_renv_of_annotation ann =
  match ann with
  | {has_nested_snap=hns;has_side_effect=hse;has_delta_update=hdu} ->
      mk_renv hns hse hdu

let make_annotation renv path_annotation =
  { has_nested_snap  = renv.contains_snapped_delta;
    has_side_effect  = renv.contains_side_effect;
    has_delta_update = renv.contains_delta_update;
    path_annotation = path_annotation;
    materialize_tuple_stream = true }


(*************)
(* Dummy code*)
(*************)

let dummy_error alg_ctxt () =
  raise (Error.Query
	   (Error.Malformed_Algebra_Expr
	      "Missing code in the algebraic plan! [Expr]"))
let dummy_code = NoDep ((fun ef -> AOECUnit dummy_error), None)
let decl_dummy_code = PNoDep (PAOECUnit dummy_error)


(*****************************)
(* Annotated op constructors *)
(*****************************)

let combine_renv e1 e2 = 
  mk_renv 
    (e1.contains_side_effect   || e2.contains_side_effect)
    (e1.contains_snapped_delta || e2.contains_snapped_delta) 
    (e1.contains_delta_update  || e2.contains_delta_update) 

let no_side_effect_renv ()     = mk_renv false false false
let atomic_side_effect_renv () = mk_renv true false true
let non_trivial_snap_renv   () = mk_renv true true true
let ensure_delta env           = combine_renv (mk_renv false false true) env

let make_annotated_op op indep dep renv =
  let path_annotation = op.annotation in
  let annotation = make_annotation renv path_annotation in
  algop_mkop
    dummy_code
    op.palgop_expr_eval_sig
    op.palgop_expr_name
    indep
    dep
    annotation
    op.compile_annotations
    op.palgop_expr_origin
    op.palgop_expr_loc

let make_temp_annotated_expr opname indep dep loc =
  let path_annotation = ref None in
  let annotation = make_annotation (no_side_effect_renv()) path_annotation in
  algop_mkop
    dummy_code
    None
    opname
    indep
    dep
    annotation
    None
    None
    loc

(* Lookup the function body *)
let get_function_body_renv compile_ctxt ((name,arity) as fn_name) =
(* Debug.print_default_debug("In get_function_body_renv \n"); *)
  let snap =  if Compile_context.mem_function_from_compile_context compile_ctxt fn_name
  then
    begin
      let func_defn =
	Compile_context.get_function_from_compile_context "get_function_body_renv" compile_ctxt fn_name
      in
      match !(func_defn.palgop_func_optimized_logical_plan) with
      |	AOEFunctionImported -> (* Assume 'the worst' *) non_trivial_snap_renv ()
      |	AOEFunctionUser userbody -> 
	  make_renv_of_annotation userbody.annotation
    end
  else (* Assume 'the worst' *)
    non_trivial_snap_renv ()
  in
(* Debug.print_default_debug("Out get_function_body_renv \n"); *)
  snap

let rec has_non_trivial_snap compile_ctxt algop =
  match algop.palgop_expr_name with
  | AOESnap sm ->
      let indep,renv =
	has_non_trivial_snap_subexpr compile_ctxt algop.psub_expression
      in
      let dep, renv =
	has_non_trivial_snap_subexpr compile_ctxt algop.pdep_sub_expression
      in
      let renv        =
	if renv.contains_delta_update
	then non_trivial_snap_renv ()
	else no_side_effect_renv ()
      in
      (make_annotated_op algop indep dep renv), renv
  | AOECallUserDefined  ((cfname,arity), _,_,_, _) ->
      let indep,renv1 =
	has_non_trivial_snap_subexpr compile_ctxt algop.psub_expression
      in 
      let _ = access_unitsub algop.pdep_sub_expression in
      let body_renv = get_function_body_renv compile_ctxt (cfname,arity) in
      let renv      = combine_renv renv1 body_renv in 
      (make_annotated_op algop indep NoSub renv), renv
  | AOEDelete
  | AOEInsert _ 
  | AOERename _
  | AOEReplace _ ->
      let indep,renv1 = has_non_trivial_snap_subexpr compile_ctxt algop.psub_expression in 
      let dep,renv2   = has_non_trivial_snap_subexpr compile_ctxt algop.pdep_sub_expression in 
      let combined    = combine_renv renv1 renv2 in 
      let renv        = ensure_delta combined in 
      (make_annotated_op algop indep dep combined), renv
  | _ -> 
      let indep,renv1 = has_non_trivial_snap_subexpr compile_ctxt algop.psub_expression in 
      let dep,renv2   = has_non_trivial_snap_subexpr compile_ctxt algop.pdep_sub_expression in 
      let renv        = combine_renv renv1 renv2 in 
      (make_annotated_op algop indep dep renv), renv

and has_non_trivial_snap_subexpr compile_ctxt sexpr = 
  match sexpr with 
  | NoSub -> NoSub, (no_side_effect_renv ())
  | OneSub op -> 
      let op, renv = has_non_trivial_snap compile_ctxt op in
      (OneSub op), renv
  | TwoSub (op1,op2) ->
      let op1, renv1 = has_non_trivial_snap compile_ctxt op1 in 
      let op2, renv2 = has_non_trivial_snap compile_ctxt op2 in 
      (TwoSub(op1,op2)), (combine_renv renv1 renv2)
  | ManySub ops ->	    
      let renv_ref = ref (no_side_effect_renv ()) in 
      let process_op op = 
	let op,renv = has_non_trivial_snap compile_ctxt op in 
	begin
	  renv_ref := combine_renv !renv_ref renv;
	  op
	end
      in
      let ops = Array.map process_op ops in 
      (ManySub ops), !renv_ref


(********************)
(* Internal Helpers *)
(********************)
	   
let annotate_decl context dd = (* annotate subexprs and then go! *)
  let indep,renv1 = has_non_trivial_snap_subexpr context dd.alg_decl_indep in
  let dep, renv2  = has_non_trivial_snap_subexpr context dd.alg_decl_dep in
  let renv        = combine_renv renv1 renv2 in 
  let path_annotation = dd.alg_decl_annotation in
  let annotation  = make_annotation renv path_annotation in 
  algop_decl_mkop
    decl_dummy_code
    dd.alg_decl_name
    indep
    dep
    annotation
    dd.alg_decl_loc 

let annotate_function_body compile_context func_defn = 
  let func_body = 
    match !(func_defn.palgop_func_optimized_logical_plan) with
    | AOEFunctionImported -> AOEFunctionImported
    | AOEFunctionUser userbody -> 
	let op, _ = has_non_trivial_snap compile_context userbody in 
	(AOEFunctionUser op)
  in
  fmkalgop_function_body func_defn.palgop_func_formal_args func_body None func_defn.palgop_func_output_type
  
let annotate_function_decl compile_context 
  { palgop_function_decl_desc=(fname,signature,fn_body,upd);
    palgop_function_decl_loc=loc } =
  (* Annotate the body *)
  let fn_body = annotate_function_body compile_context fn_body in
  if (Compile_context.mem_function_from_compile_context compile_context fname) 
  then ()
  else Compile_context.add_function_to_compile_context compile_context fname fn_body;
  (* Update the compile context *)
  fmkalgop_function_decl (fname,signature,fn_body,upd) loc


(************)
(* External *)
(************)

let annotate_statement compile_ctxt ae = 
  let op, _ = has_non_trivial_snap compile_ctxt ae in op

let annotate_expr compile_ctxt ae = 
  let op, _ = has_non_trivial_snap compile_ctxt ae in op

let annotate_prolog_with_bindings compile_ctxt p = 
  (* function decls have the side-effect of adding themselves to the compile context *)
  let fds  = List.map (annotate_function_decl compile_ctxt) p.palgop_prolog_functions in
  let vars = List.map (annotate_decl compile_ctxt) p.palgop_prolog_vars in 
  let ind  = List.map (annotate_decl compile_ctxt) p.palgop_prolog_indices in 
  (fmkalgop_prolog fds vars ind), compile_ctxt

let annotate_prolog compile_ctxt p =
  let compile_ctxt = Compile_context.copy_without_functions compile_ctxt in
  annotate_prolog_with_bindings compile_ctxt p 

(* annotate takes an old style compile_context *)
let annotate_module_with_bindings comp_ctxt m = 
  let prolog,(comp_ctxt:alg_compile_context) =
    annotate_prolog_with_bindings comp_ctxt m.palgop_module_prolog
  in 
  let statements =
    List.map (annotate_statement comp_ctxt) m.palgop_module_statements
  in
  (fmkalgop_xmodule prolog statements), comp_ctxt

let annotate_module comp_ctxt m = 
  let prolog,(comp_ctxt:alg_compile_context) =
    annotate_prolog comp_ctxt m.palgop_module_prolog
  in 
  let statements =
    List.map (annotate_statement comp_ctxt) m.palgop_module_statements
  in
  (fmkalgop_xmodule prolog statements), comp_ctxt

let annotate_context context =  
  let comp_ctxt = Compile_context.copy_without_functions context in
  let annotate_functions ht key func_defn  =
    let op = 
      match !(func_defn.palgop_func_optimized_logical_plan) with
      |	AOEFunctionImported -> AOEFunctionImported 
      |	AOEFunctionUser userbody -> AOEFunctionUser(annotate_statement comp_ctxt userbody)
    in 
    (* Order of bindings is unimportant because we are just dealing
    with their ops here *)
    let func_defn' = fmkalgop_function_body 
	func_defn.palgop_func_formal_args op None func_defn.palgop_func_output_type in
    Namespace_util.RQNameIntHashtbl.add ht key func_defn'
  in
  Compile_context.map_function_bodies context annotate_functions 

