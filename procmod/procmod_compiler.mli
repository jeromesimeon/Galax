(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: procmod_compiler.mli,v 1.26 2007/10/16 01:25:35 mff Exp $ *)

(* Module: Procmod_compiler
   Description:
     This module contains top-level operations over the processing
     model. This notably supports various levels of compilations for
     queries.
*)

open Xquery_common_ast
open Xquery_ast
open Xquery_core_ast
open Xquery_algebra_ast
open Ast_logical_algebra_types
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

open Compiled_program_units

(*******************************************)
(* Type for intermediate compilation steps *)
(*******************************************)

type ginput =
  | IOStatement of Galax_io.input_spec
  | IOProlog of Galax_io.input_spec
  | IOLibraryModule of Galax_io.input_spec
  | IOMainModule of Galax_io.input_spec

type ast =
  | ASTStatement of statement
  | ASTProlog of prolog
  | ASTLibraryModule of library_module
  | ASTMainModule of main_module

type core_ast =
  | CoreASTStatement of acstatement
  | CoreASTProlog of acprolog
  | CoreASTLibraryModule of acxmodule
  | CoreASTMainModule of acxmodule


type logical_plan =
  | LogicalPlanStatement of logical_algop_expr
  | LogicalPlanProlog of logical_algop_prolog
  | LogicalPlanLibraryModule of logical_algop_xmodule
  | LogicalPlanMainModule of logical_algop_xmodule

type logical_plan_statement = logical_algop_expr
type logical_plan_main_module = logical_algop_xmodule

type physical_plan =
  | PhysicalPlanStatement of algop_expr
  | PhysicalPlanProlog of algop_prolog
  | PhysicalPlanLibraryModule of algop_xmodule
  | PhysicalPlanMainModule of algop_xmodule


(********************)
(* External context *)
(********************)

type external_context =
    { external_context_item : item option;
      external_timezone     : atomicDayTimeDuration option;
      external_variables    : (cvname * item list) list }

val create_external_context :
    processing_context ->
      (item option) ->
	(atomicDayTimeDuration option) ->
	  (string * item list) list ->
	    external_context

val default_external_context : unit -> external_context

(*************************)
(* Statement compilation *)
(*************************)

val logical_compile_statement :
    compiled_program -> Galax_io.input_spec -> (compiled_program * logical_compiled_statement)

val compile_statement :
    compiled_program -> Galax_io.input_spec -> (compiled_program * compiled_statement)
val compile_statement_from_logical_plan :
    compiled_program -> compiled_prolog  -> (logical_plan_statement) -> (compiled_program * compiled_statement)
val compile_statement_from_optimized_logical_plan :
    compiled_program -> (logical_plan_statement) -> (compiled_program * compiled_statement)

(**********************)
(* Prolog compilation *)
(**********************)

(* A compiled prolog is installed as the prolog of the program's main module *)
val compile_prolog :
    bool (* external context item *) -> compiled_program -> Galax_io.input_spec -> compiled_program

(**********************)
(* Module compilation *)
(**********************)

val logical_compile_main_module : 
    bool (* external context item *) -> compiled_program -> Galax_io.input_spec -> (compiled_program * (logical_compile_context * logical_plan_main_module))

val compile_library_module :
    compiled_program -> Galax_io.input_spec -> (Namespace_names.ncname * string * compiled_program)

val compile_main_module :
    bool (* external context item *) -> compiled_program -> Galax_io.input_spec -> (compiled_program * compiled_statement list)
val compile_main_module_from_logical_plan :
    compiled_program -> (logical_compile_context * logical_plan_main_module) -> (compiled_program * compiled_statement list)
val compile_main_module_from_optimized_logical_plan :
    compiled_program -> (logical_compile_context * logical_plan_main_module) -> (compiled_program * compiled_statement list)

(***************************************)
(* Standard library module compilation *)
(***************************************)

val compile_standard_library_module : processing_context -> compiled_program

(*****************************)
(* Prepare a compiled prolog *)
(*****************************)

val prepare_compiled_program : external_context option -> compiled_program -> prepared_program

(********************************)
(* Execute a compiled statement *)
(********************************)

val execute_compiled_statement : prepared_program -> compiled_statement -> physical_value

