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

open Xquery_algebra_ast
open Logical_algebra_types
open Algebra_type


open Dm_atomic
open Physical_value

open Processing_context
open Namespace_context
open Parse_context
open Norm_context
open Typing_context
open Compile_context
open Code_selection_context
open Execution_context
open Monitoring_context


(***********************)
(* Logical compilation *)
(***********************)

val main_module_uri : string

(* Compiled statements *)

type logical_compiled_statement = logical_algop_expr

(*
(* Compiled prolog *)

type logical_compiled_prolog =
   { logical_compiled_prolog_uri                     : string;
     mutable logical_compiled_prolog_plan  	     : logical_algop_prolog;
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
val copy_logical_compiled_program : logical_compiled_program -> logical_compiled_program
*)


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

type compiled_program =
    { compiled_program_proc_context            : Processing_context.processing_context;
      compiled_program_stdlib_norm_context     : Norm_context.norm_context;
      mutable compiled_program_algebra_context : Execution_context.algebra_context;
      compiled_program_interfaces              : Norm_context.norm_interface_table;
      mutable compiled_program_library_list    : string list;
      compiled_program_library_module_table    : (string, compiled_library_module) Hashtbl.t;
      mutable compiled_program_main_module     : compiled_main_module option }

(* In a prepared program, all global variables' values are bound: *)
type prepared_program = compiled_program

(* Copy a compiled program *)
val copy_compiled_program : compiled_program -> compiled_program

(****************************)
(* Module context accessors *)
(****************************)

(*
val logical_main_module_of_compiled_program : logical_compiled_program -> logical_compiled_main_module
val logical_processing_context_of_compiled_program : logical_compiled_program -> processing_context
val logical_module_of_compiled_program      : logical_compiled_program -> string -> logical_compiled_library_module
val logical_statements_of_compiled_program  : logical_compiled_program -> logical_compiled_statement list
*)

val main_module_of_compiled_program : compiled_program -> compiled_main_module
val processing_context_of_compiled_program : compiled_program -> processing_context
val algebra_context_of_compiled_program    : compiled_program -> algebra_context
val replace_algebra_context_of_compiled_program : compiled_program -> algebra_context -> unit
val module_of_compiled_program      : compiled_program -> string -> compiled_library_module
val statements_of_compiled_program  : compiled_program -> compiled_statement list

val main_module_defined_in_compiled_program : compiled_program -> bool
val standard_library_of_compiled_program    : compiled_program -> compiled_library_module

val replace_algebra_context_of_compiled_program :  compiled_program -> Execution_context.algebra_context -> unit

val nsenv_of_module             : compiled_prolog -> Namespace_context.nsenv
val nsenv_of_main_module        : compiled_program -> Namespace_context.nsenv
val norm_context_of_module      : compiled_prolog -> Norm_context.norm_context
val norm_context_of_main_module : compiled_program -> Norm_context.norm_context
val norm_context_of_standard_library : compiled_program -> Norm_context.norm_context
val norm_context_of_prolog      : compiled_prolog -> Norm_context.norm_context
val static_context_of_module    : compiled_prolog -> Typing_context.static_context
val static_context_of_main_module  : compiled_program -> Typing_context.static_context
val compile_context_of_module      : compiled_prolog -> Logical_algebra_types.logical_compile_context
val compile_context_of_main_module : compiled_program -> Logical_algebra_types.logical_compile_context
val code_selection_context_of_module : compiled_prolog -> Code_selection_context.code_selection_context
val code_selection_context_of_main_module : compiled_program -> Code_selection_context.code_selection_context

val replace_code_selection_context_of_module : 
    compiled_prolog -> Code_selection_context.code_selection_context -> unit
val update_code_selection_context_in_compiled_prolog :
    compiled_prolog -> Code_selection_context.code_selection_context -> unit

(*
val logical_replace_compile_context_of_main_module : 
    logical_compiled_program -> logical_compile_context -> unit
val logical_update_compile_context_in_compiled_prolog :
    logical_compiled_prolog -> logical_compile_context -> unit
val logical_update_logical_plan_in_compiled_prolog :
    logical_compiled_prolog -> logical_algop_prolog -> unit
val logical_default_compiled_module  : logical_compiled_program -> logical_compiled_prolog
*)

val update_physical_plan_in_compiled_prolog :
    compiled_prolog ->
      (Algebra_type.alg_eval_code_dep, Algebra_type.physical_annotation,
       Algebra_type.alg_eval_code_dep_prolog)
	Xquery_algebra_ast.aalgop_prolog -> unit

val default_compiled_module  : compiled_program -> string -> compiled_prolog
val default_compiled_program : Processing_context.processing_context -> Norm_context.norm_context -> compiled_program

val copy_compiled_module  : compiled_prolog -> compiled_prolog
val copy_compiled_program : compiled_program -> compiled_program
