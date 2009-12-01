(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: procmod_types.mli,v 1.9 2007/10/16 01:25:35 mff Exp $ *)

(* Module: Procmod_types
   Description:
     This module contains types used by the processing model.
*)

open Galax_io

open Monitoring_context
open Processing_context
open Parse_context
open Norm_context
open Typing_context
open Compile_context
open Code_selection_context
open Compiled_program_units
open Execution_context

open Xquery_ast
open Xquery_core_ast
open Xquery_algebra_ast
open Ast_logical_algebra_types
open Algebra_type

open Physical_value


(******************)
(* Phase handlers *)
(******************)

(* Parsing phase:
     Parses the original query or module.  Takes a statement a string
     and returns a statement as output. *)

type parsing_phase_handler =
    { parse_statement      : parse_context -> input_spec -> statement;
      parse_prolog         : parse_context -> input_spec -> parse_context * prolog;
      parse_library_module : parse_context -> input_spec -> parse_context * library_module;
      parse_main_module    : parse_context -> input_spec -> parse_context * main_module }

(* Pre-processing phase: 

   Pre-processes a module and returns transitive closure of imported
   modules, to be used during normalization of program units.
  
   The interface for each imported module is normalized. 

   Returns a table of normalized interfaces and a list of modules in
   imported order.
*)
type preprocessing_phase_handler =
    { preprocess_prolog         : processing_context -> prolog ->        
	(norm_interface_table * Xquery_ast.library_module list);
      preprocess_library_module : processing_context -> library_module -> 
	(norm_interface_table * Xquery_ast.library_module list);
      preprocess_main_module    : processing_context -> main_module -> 
	(norm_interface_table * Xquery_ast.library_module list);
    } 

(* Normalization phase: 
     Normalizes the original query or module.
     Takes a statement as input and returns an acstatement (with no
     annotations) as output. *)

type normalization_phase_handler =
    { normalize_statement      : norm_context -> statement      -> acstatement;
      normalize_prolog         : (norm_interface_table * norm_context) -> prolog         -> norm_context * acprolog;
      normalize_library_module : (norm_interface_table * norm_context) -> library_module -> norm_context * acxmodule;
      normalize_main_module    : (norm_interface_table * norm_context) -> main_module    -> norm_context * acxmodule }

(* Rewriting phase:
     Performs some rewriting of the query. Takes an acstatement as
     input and returns a rewritten acstatement as output. *)

type rewriting_phase_handler =
    { rewriting_statement      : static_context -> acstatement -> acstatement;
      rewriting_prolog         : static_context -> acprolog    -> static_context * acprolog;
      rewriting_library_module : static_context -> acxmodule   -> static_context * acxmodule;
      rewriting_main_module    : static_context -> acxmodule   -> static_context * acxmodule }

(* Factorization phase:
     Performs code factorization and query canonicalization to prepare
     for optimization. Takes an acstatement as input and returns a
     factorized acstatement as output. *)

type factorization_phase_handler =
    { factorize_statement      : static_context -> acstatement -> acstatement;
      factorize_prolog         : static_context -> acprolog    -> static_context * acprolog;
      factorize_library_module : static_context -> acxmodule   -> static_context * acxmodule;
      factorize_main_module    : static_context -> acxmodule   -> static_context * acxmodule }

(* Compilation phase:
     Builds a naive query plan. Takes an acstatement as input and
     returns an algop_statement as output. *)

type compile_phase_handler =
    { compile_statement      : logical_compile_context -> acstatement -> logical_algop_expr;
      compile_prolog         : logical_compile_context -> acprolog    -> logical_compile_context * logical_algop_prolog;
      compile_library_module : logical_compile_context -> acxmodule   -> logical_compile_context * logical_algop_xmodule;
      compile_main_module    : logical_compile_context -> acxmodule   -> logical_compile_context * logical_algop_xmodule }

(* Optimization phase:
     Performs algebraic query optimization. Takes an algop_expr as
     input and returns an optimized algop_expr as output.
*)

type optimization_phase_handler =
    { optimize_statement      : logical_compile_context -> logical_algop_expr    -> logical_algop_expr;
      optimize_prolog         : logical_compile_context -> logical_algop_prolog  -> logical_compile_context * logical_algop_prolog;
      optimize_library_module : logical_compile_context -> logical_algop_xmodule -> logical_compile_context * logical_algop_xmodule;
      optimize_main_module    : logical_compile_context -> logical_algop_xmodule -> logical_compile_context * logical_algop_xmodule }

(* Code selection phase:
   Select actual code for each algebraic operation in the query
   plan, thereby producing a "physical" plan.
*)

type selection_phase_handler =
    { selection_statement      : (compiled_program * string * code_selection_context) -> 
        logical_algop_expr -> code_selection_context * algop_expr;
      selection_prolog         : (compiled_program * string * code_selection_context) -> 
	logical_algop_prolog  -> code_selection_context * algop_prolog;
      selection_library_module : (compiled_program * string * code_selection_context) -> 
	logical_algop_xmodule -> code_selection_context * algop_xmodule;
      selection_main_module    : (compiled_program * string * code_selection_context) -> 
	logical_algop_xmodule -> code_selection_context * algop_xmodule 
    }

(* Evaluation phase:
   Evaluates the query. Takes an algop_expr(prolog,module,library) as
   input and returns an XML value as output. *)

type evaluation_phase_handler =
    { eval_statement 	  : algebra_context -> algop_expr    -> physical_value;
      eval_prolog         : algebra_context -> algop_prolog  -> algebra_context * physical_value list;
      eval_library_module : algebra_context -> algop_xmodule -> algebra_context * physical_value list;
      eval_main_module    : algebra_context -> algop_xmodule -> algebra_context * physical_value list }

(******************)
(* Global Handler *)
(******************)

(* The global handler contains some code for each processing phase *)

type phase_handler =
    { parsing_phase       : parsing_phase_handler;
      preprocessing_phase : preprocessing_phase_handler; 
      normalization_phase : normalization_phase_handler;
      rewriting_phase     : rewriting_phase_handler;
      factorization_phase : factorization_phase_handler;
      compile_phase       : compile_phase_handler;
      optimization_phase  : optimization_phase_handler;
      selection_phase     : selection_phase_handler;
      evaluation_phase    : evaluation_phase_handler }


