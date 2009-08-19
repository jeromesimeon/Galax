(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: compile_context_util.ml,v 1.5 2007/07/13 18:24:42 mff Exp $ *)

(* This function has been excised from Compile_context
   in order to resolve the cyclic dependency
   Logical_algebra_types -> Compile_context -> Logical_algebra_types.

    - Michael *)

open Compile_context
open Namespace_util
open Xquery_algebra_ast

let copy_strip_functions comp_ctxt = 
  let convert_binding ht key func_defn = 
    match !(func_defn.palgop_func_optimized_logical_plan) with
    | AOEFunctionImported -> ()
    | AOEFunctionUser op -> 
    (* What does stripping do here? *)
	let body = Xquery_algebra_ast_annotation_util.strip_annotation op in 
	let func_defn = Xquery_algebra_ast_util.fmkalgop_function_body 
	    func_defn.palgop_func_formal_args (AOEFunctionUser body) None func_defn.palgop_func_output_type in
	RQNameIntHashtbl.add ht key func_defn
  in
  map_function_bodies comp_ctxt convert_binding 

