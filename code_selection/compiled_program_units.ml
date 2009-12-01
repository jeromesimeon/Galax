(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* Module: Compiled_program_units
   Description:
     This module contains the types for all the compiled program units: 
     a program, library modules, interfaces, main module, and statements. 
*)

open Error

open Xquery_algebra_ast
open Ast_logical_algebra_types
open Algebra_type

open Processing_context
open Parse_context
open Norm_context
open Typing_context
open Compile_context
open Code_selection_context
open Execution_context



(*********************************)
(* Types used during compilation *)
(*********************************)

let main_module_uri = "#MAINMODULE#"

(***********************)
(* Logical compilation *)
(***********************)

(* Compiled statements *)

type logical_compiled_statement = logical_algop_expr

(* Compiled prolog *)

type logical_compiled_prolog =
    { logical_compiled_prolog_uri                     : string;
      mutable logical_compiled_prolog_plan  	      : logical_algop_prolog;
      mutable logical_compiled_prolog_compile_context : logical_compile_context }

(* Compiled module *)

type logical_compiled_library_module = logical_compiled_prolog
type logical_compiled_main_module    = logical_compiled_prolog * logical_compiled_statement list

type logical_compiled_program =
    { logical_compiled_program_proc_context          : Processing_context.processing_context;
      logical_compiled_program_stdlib_norm_context   : Norm_context.norm_context;
      logical_compiled_program_interfaces            : Norm_context.norm_interface_table;
      mutable logical_compiled_program_library_list  : string list;
      logical_compiled_program_library_module_table  : (string, logical_compiled_library_module) Hashtbl.t;
      mutable logical_compiled_program_main_module   : logical_compiled_main_module option; }

(* Copy a compiled program *)

let copy_logical_compiled_module comp_mod = 
  { logical_compiled_prolog_uri = comp_mod.logical_compiled_prolog_uri;
    logical_compiled_prolog_plan            = comp_mod.logical_compiled_prolog_plan;
    logical_compiled_prolog_compile_context = 
    Compile_context.copy_compile_context comp_mod.logical_compiled_prolog_compile_context }

let copy_logical_compiled_program comp_prog = 
  let copied_main_mod = 
    match comp_prog.logical_compiled_program_main_module with
    | None -> None
    | Some (main_mod, stmts) -> Some (copy_logical_compiled_module main_mod, stmts)
  in
  { logical_compiled_program_proc_context        = comp_prog.logical_compiled_program_proc_context;
    logical_compiled_program_stdlib_norm_context = comp_prog.logical_compiled_program_stdlib_norm_context;
    (* Copy the algebra context *)
    logical_compiled_program_interfaces          = comp_prog.logical_compiled_program_interfaces;
    logical_compiled_program_library_list        = comp_prog.logical_compiled_program_library_list;
    logical_compiled_program_library_module_table = comp_prog.logical_compiled_program_library_module_table;
    (* Copy the main_module *)
    logical_compiled_program_main_module     = copied_main_mod } 



(************************)
(* Physical compilation *)
(************************)

(* Compiled statements *)

type compiled_statement = algop_expr

(* Compiled prolog *)

type compiled_prolog =
    { compiled_prolog_uri                            : string;
      mutable compiled_prolog_plan  	             : algop_prolog;
      mutable compiled_prolog_code_selection_context : code_selection_context }

(* Compiled module *)

type compiled_library_module = compiled_prolog
type compiled_main_module = compiled_prolog * compiled_statement list

(* Compiled program *)

type compiled_program =
    { compiled_program_proc_context        : Processing_context.processing_context;
      compiled_program_stdlib_norm_context : Norm_context.norm_context;
      mutable compiled_program_algebra_context : algebra_context;
      (* Imported interfaces *)
      compiled_program_interfaces          : Norm_context.norm_interface_table;
      (* Modules listed in imported order *)
      mutable compiled_program_library_list : string list; 
      compiled_program_library_module_table : (string, compiled_library_module) Hashtbl.t;
      mutable compiled_program_main_module  : (compiled_main_module) option }

(* In a prepared program, all global variables' values are bound: *)
type prepared_program = compiled_program

(****************************)
(* Module context accessors *)
(****************************)

let module_of_compiled_program p uri = 
  try 
    Hashtbl.find p.compiled_program_library_module_table uri 
  with
  | Not_found -> raise (Query(Internal_Error("Module '"^uri^"' not in program")))

let logical_module_of_compiled_program p uri = 
  try 
    Hashtbl.find p.logical_compiled_program_library_module_table uri 
  with
  | Not_found -> raise (Query(Internal_Error("Module '"^uri^"' not in program")))

let main_module_of_compiled_program p = 
  match p.compiled_program_main_module with 
  | None -> raise (Query(Internal_Error("Main module undefined in program")))
  | Some m -> m

let logical_main_module_of_compiled_program p = 
  match p.logical_compiled_program_main_module with 
  | None -> raise (Query(Internal_Error("Main module undefined in program")))
  | Some m -> m

let statements_of_compiled_program p = 
  match p.compiled_program_main_module with 
  | None -> raise (Query(Internal_Error("Main module undefined in program")))
  | Some (_,stmts) -> stmts

let logical_statements_of_compiled_program p = 
  match p.logical_compiled_program_main_module with 
  | None -> raise (Query(Internal_Error("Main module undefined in program")))
  | Some (_,stmts) -> stmts

let main_module_defined_in_compiled_program p = 
  match p.compiled_program_main_module with 
  | None -> false
  | Some (m,st) -> not(st = [])

let standard_library_of_compiled_program p = 
  module_of_compiled_program p (Conf.glxns) 

let processing_context_of_compiled_program m =
  m.compiled_program_proc_context

let logical_processing_context_of_compiled_program m =
  m.logical_compiled_program_proc_context

let algebra_context_of_compiled_program p =
  p.compiled_program_algebra_context
let replace_algebra_context_of_compiled_program p ctxt =
  p.compiled_program_algebra_context <- ctxt

let nsenv_of_module m =
  nsenv_from_norm_context
    (norm_context_from_stat_context
       (static_context_from_code_selection_context
	  m.compiled_prolog_code_selection_context))

let nsenv_of_main_module p = 
  let (m,_) = main_module_of_compiled_program p in
  nsenv_of_module m

let norm_context_of_module m =
  (norm_context_from_stat_context
     (static_context_from_code_selection_context
	m.compiled_prolog_code_selection_context))

let norm_context_of_main_module p = 
  let (m,_) = main_module_of_compiled_program p in 
  norm_context_of_module m

let norm_context_of_standard_library p = 
  let m = module_of_compiled_program p Conf.glxns in 
  norm_context_of_module m

let norm_context_of_prolog p =
  (norm_context_from_stat_context
     (static_context_from_code_selection_context
	p.compiled_prolog_code_selection_context))
    
let static_context_of_module m =
  static_context_from_code_selection_context
    m.compiled_prolog_code_selection_context

let static_context_of_main_module p = 
  let (m,_) = main_module_of_compiled_program p in
  static_context_of_module m

let compile_context_of_module m = 
  compile_context_from_code_selection_context
    m.compiled_prolog_code_selection_context

let compile_context_of_main_module p = 
  let (m,_) = main_module_of_compiled_program p in
  compile_context_of_module m

let code_selection_context_of_module m = 
  m.compiled_prolog_code_selection_context

let code_selection_context_of_main_module p =
  let (m,_) = main_module_of_compiled_program p in
  code_selection_context_of_module m

let replace_code_selection_context_of_module m ctxt =
  m.compiled_prolog_code_selection_context <- ctxt

let update_code_selection_context_in_compiled_prolog m csc = 
  m.compiled_prolog_code_selection_context <- csc

let logical_replace_compile_context_of_main_module p ctxt =
  let (m,_) = logical_main_module_of_compiled_program p in
  m.logical_compiled_prolog_compile_context <- ctxt

let logical_update_compile_context_in_compiled_prolog m csc = 
  m.logical_compiled_prolog_compile_context <- csc

(* We merge the old plan with the new plan *)
let update_physical_plan_in_compiled_prolog m plan = 
  let plan' = Xquery_algebra_ast_util.merge_alg_prologs m.compiled_prolog_plan plan in 
  m.compiled_prolog_plan <- plan'

let logical_update_logical_plan_in_compiled_prolog m plan = 
  let plan' = Xquery_algebra_ast_util.merge_alg_prologs m.logical_compiled_prolog_plan plan in 
  m.logical_compiled_prolog_plan <- plan'

(*******************************)
(* The default compiled prolog *)
(*******************************)

let default_compiled_module comp_prog mod_uri = 
  let norm_ctxt = Norm_context.copy_norm_context comp_prog.compiled_program_stdlib_norm_context in 
  let mod_proc_ctxt = Norm_context.module_context_from_norm_context norm_ctxt in 
  mod_proc_ctxt.system <- false;
  { compiled_prolog_uri = mod_uri ;
    compiled_prolog_plan                   = Xquery_algebra_ast_util.empty_prolog_plan;
    compiled_prolog_code_selection_context = default_code_selection_context norm_ctxt;
  }

let logical_default_compiled_module comp_prog mod_uri = 
  let norm_ctxt = Norm_context.copy_norm_context comp_prog.logical_compiled_program_stdlib_norm_context in 
  let mod_proc_ctxt = Norm_context.module_context_from_norm_context norm_ctxt in 
  mod_proc_ctxt.system <- false;
  { logical_compiled_prolog_uri = mod_uri ;
    logical_compiled_prolog_plan           = Xquery_algebra_ast_util.empty_prolog_plan;
    logical_compiled_prolog_compile_context = default_compile_context norm_ctxt;
  }

let default_compiled_program proc_ctxt norm_ctxt = { 
  compiled_program_proc_context  = proc_ctxt;
  compiled_program_stdlib_norm_context  = norm_ctxt;
  compiled_program_algebra_context = default_algebra_context();
  compiled_program_interfaces      = Hashtbl.create 11;
    (* Modules listed in imported order *)
  compiled_program_library_list = [];
  compiled_program_library_module_table = Hashtbl.create 11;
  compiled_program_main_module       = None;
} 

let copy_compiled_module comp_mod = 
  { compiled_prolog_uri  	           = comp_mod.compiled_prolog_uri;
    compiled_prolog_plan  	           = comp_mod.compiled_prolog_plan;
    compiled_prolog_code_selection_context = 
    Code_selection_context.copy_code_selection_context comp_mod.compiled_prolog_code_selection_context;
  }

let copy_compiled_program comp_prog = 
  let copied_main_mod = 
    match comp_prog.compiled_program_main_module with
    | None -> None
    | Some (main_mod, stmts) -> Some (copy_compiled_module main_mod, stmts)
  in
  {
  compiled_program_proc_context  = comp_prog.compiled_program_proc_context;
  compiled_program_stdlib_norm_context  = comp_prog.compiled_program_stdlib_norm_context;
  (* Copy the algebra context *)
  compiled_program_algebra_context = Execution_context.copy_algebra_context comp_prog.compiled_program_algebra_context; 
  compiled_program_interfaces      = comp_prog.compiled_program_interfaces;
  compiled_program_library_list    = comp_prog.compiled_program_library_list;
  (* Copy the library module table *)
  compiled_program_library_module_table = Hashtbl.copy comp_prog.compiled_program_library_module_table;
  (* Copy the main_module *)
  compiled_program_main_module     = copied_main_mod;
  } 

