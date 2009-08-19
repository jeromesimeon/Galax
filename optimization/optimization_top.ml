(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_top.ml,v 1.13 2007/09/13 18:36:42 simeon Exp $ *)

(* Module: Optimization_top
   Description:
     This module contains top-level operation for the algebraic
     optimizer.
*)

(* NOTE: Annotation is really a separate 'sub-phase' but is bundled
   here since it is on the boundary of two phases. *)

open Processing_context
open Compile_context

open Xquery_algebra_ast
open Xquery_algebra_ast_util

open Optimization_rewrite

let optimize_statement comp_ctxt algop_expr =
  rewrite_expression comp_ctxt algop_expr

let function_rewrite comp_ctxt func_decl = 
    let (fnname, arity), signature, func_defn, upd = func_decl.palgop_function_decl_desc in
    match !(func_defn.palgop_func_optimized_logical_plan) with
    | AOEFunctionImported -> func_decl
    | AOEFunctionUser userbody ->
	begin
	  let new_expr = (rewrite_expression comp_ctxt) userbody in 
	  func_defn.palgop_func_optimized_logical_plan := AOEFunctionUser new_expr; 
	  { palgop_function_decl_desc = (fnname,arity), signature, func_defn, upd ;
	    palgop_function_decl_loc  = func_decl.palgop_function_decl_loc}
	end
(*
let var_rewrite comp_ctxt var_decl  = 

type ('a,'b,'c) aalgop_decl =
    { alg_decl_name          : algop_decl_name; 	 (* Variable/index kind *)
      alg_decl_eval          : 'c ref ;           	 (* The evaluation code *)
      mutable alg_decl_indep : ('a,'b) aalgop_sub_exprs; (* Indep. sub-expr *)
      mutable alg_decl_dep   : ('a,'b) aalgop_sub_exprs; (* Dep. sub-expr *)
      alg_decl_annotation    : 'b;                       (* Annotations *)
      alg_decl_loc           : Finfo.finfo }             (* File location *)
*)
let optimize_prolog comp_ctxt alg_prolog =
  Debug.print_compile_debug "Starting optimization of prolog";
  let palg_prolog_functions' = List.map (function_rewrite comp_ctxt) alg_prolog.palgop_prolog_functions in
  let palg_prolog_vars' = alg_prolog.palgop_prolog_vars in 
(*  let palg_prolog_vars' = List.fold_left (var_rewrite comp_ctxt) [] alg_prolog.palgop_prolog_vars in*)
  (* We do not optimize the expressions associated with indices *)
  let opt_alg_prolog = 
    { palgop_prolog_functions = palg_prolog_functions';
      palgop_prolog_vars = palg_prolog_vars';
      palgop_prolog_indices = alg_prolog.palgop_prolog_indices; }
  in
  Debug.print_compile_debug "Finished optimization of prolog\n";
  (comp_ctxt,opt_alg_prolog)

let optimize_module comp_ctxt alg_module =
  let (alg_prolog,alg_statements) = split_main_module_plan alg_module in
  let (_, opt_alg_prolog) = optimize_prolog comp_ctxt alg_prolog in
  let opt_alg_statements = List.map (rewrite_expression comp_ctxt) alg_statements in
  comp_ctxt, (fmkalgop_xmodule opt_alg_prolog opt_alg_statements)

let optimize_library_module comp_ctxt alg_module =
  optimize_module comp_ctxt alg_module

let optimize_main_module comp_ctxt alg_module =
  optimize_module comp_ctxt alg_module

