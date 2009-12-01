(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_selection_context.mli,v 1.32 2007/08/08 19:27:52 mff Exp $ *)

(* Module: Code_selection_context
     This module implements the context used during code selection.
*)

(*
   Notes on usage:

   The code selection context uses the typical caml method to handle
   scoping (copies and carrying around the context).  This module
   handles setting up contexts for modules and functions and compiling
   variable references to array lookups.
*)

open Xquery_common_ast
open Namespace_names

open Physical_value

open Processing_context
open Typing_context
open Ast_logical_algebra_types
open Algebra_type
open Variable_context_manager
open Tuple_context_manager

(******************************)
(* The code selection context *)
(******************************)

type code_selection_context

(* Default code selection context *)

val default_code_selection_context : Norm_context.norm_context -> code_selection_context

(* Compilation steps - Can only compile a function once (or it creates two records) *)
val enter_statement_context : code_selection_context -> code_selection_context
val exit_statement_context  : code_selection_context -> unit
val enter_function_context  : code_selection_context -> 
                               cfname_arity -> 
				 Namespace_names.rqname array -> code_selection_context
val exit_function_context   : code_selection_context -> cfname_arity -> unit

(* Closure takes free variables and tuple fields *)
val enter_closure_context   : code_selection_context -> 
                               Namespace_names.rqname list -> 
				Namespace_names.rqname list -> code_selection_context
val exit_closure_context    : code_selection_context -> unit

val enter_prolog_context    : code_selection_context -> code_selection_context
val exit_prolog_context     : code_selection_context -> unit

val enter_scope             : code_selection_context -> code_selection_context
val exit_scope              : bool -> code_selection_context -> code_selection_context -> code_selection_context
val restore_variables       : code_selection_context -> code_selection_context -> code_selection_context

(* Enter scope of remote execute, returning previous value *)
val get_in_remote_execute_operator : code_selection_context -> bool
val set_in_remote_execute_operator : code_selection_context -> bool -> unit

(* On creation *)
val store_annotation        : code_selection_context -> Xquery_algebra_ast.free_variable_desc option -> code_selection_context
val store_global_annotation : code_selection_context -> Xquery_algebra_ast.free_variable_desc option -> code_selection_context

(* Annotation is not optional for retrieve *)
val retrieve_annotation           : string -> code_selection_context -> Xquery_algebra_ast.free_variable_desc 
val retrieve_global_annotation    : code_selection_context -> Xquery_algebra_ast.free_variable_desc 

(************)
(* Variable *)
(************)

val add_variable_to_current_context : code_selection_context -> cvname -> code_selection_context
val add_variable_with_ref           : code_selection_context -> cvname -> variable_ref -> code_selection_context
val get_variable_reference          : code_selection_context -> cvname -> variable_ref

(*********)
(* Tuple *)
(*********)

val add_tuple_reference             : code_selection_context -> cvname -> code_selection_context
val get_tuple_reference             : code_selection_context -> cvname -> tuple_ref
val get_input_tuple_fields          : code_selection_context -> cvname list

(****************************************)
(* Accessors/Mutators of other contexts *)
(****************************************)

val cxschema_from_code_selection_context :
    code_selection_context -> Xquery_type_core_ast.cxschema

val compile_context_from_code_selection_context :
    code_selection_context -> Compile_context.logical_compile_context

val replace_compile_context_in_code_selection_context :
    alg_compile_context -> code_selection_context -> code_selection_context

val annotated_compile_context_from_code_selection_context :
    code_selection_context -> alg_compile_context

val static_context_from_code_selection_context :
    code_selection_context -> static_context

val code_type_context_from_code_selection_context :
    code_selection_context -> Code_typing_context.code_type_context

val replace_code_type_context_in_code_selection_context :
    Code_typing_context.code_type_context -> code_selection_context -> code_selection_context


(**********************)
(* External Variables *)
(**********************)

val add_external_variable_value     : code_selection_context -> cvname -> item list -> code_selection_context
val declare_external_variable       : code_selection_context -> cvname -> code_selection_context
val get_external_variable_fn_value  : code_selection_context -> cvname -> (unit -> item list) ref

(************************************)
(* Imported Variables and Functions *)
(************************************)

val add_imported_variable_context : code_selection_context -> cvname -> code_selection_context -> unit
val get_imported_variable_context : code_selection_context -> cvname -> code_selection_context 

val add_imported_function_context : code_selection_context -> cfname_arity -> code_selection_context -> unit
val get_imported_function_context : code_selection_context -> cfname_arity -> code_selection_context 

(*********************)
(*** Code builders ***)
(*********************)

val build_current_insert_code :
    code_selection_context -> cvname -> (xml_value -> unit)

val build_current_assign_code :
    code_selection_context -> cvname -> (unit -> xml_value) -> (unit -> xml_value)

val build_current_retrieve_code :
    code_selection_context -> cvname -> (unit -> xml_value)

val build_retrieve_tuple_code : 
    code_selection_context -> crname -> (unit -> xml_value)

val build_retrieve_dom_tuple_code : 
    code_selection_context -> crname -> (unit -> dom_value)

val build_create_tuple_code :
    code_selection_context -> crname -> (xml_value -> unit)

val build_create_tuple_code_unsafe  :
    code_selection_context -> crname -> (xml_value -> unit)

val build_create_dom_tuple_code  :
    code_selection_context -> crname -> (dom_value -> unit)

val build_create_dom_tuple_code_unsafe  :
    code_selection_context -> crname -> (dom_value -> unit)

(* Since entrance and exit code have the same signature, this will hopefully
   prevent confusion in its use.  *)
type cs_function_code =
    { parameter_insertion_code : (xml_value -> unit) array;
      entrance_code  : (unit -> unit);
      exit_code      : (unit -> unit) }

val build_function_code :
    code_selection_context -> cfname_arity -> cvname array -> cs_function_code

val copy_code_selection_context : code_selection_context -> code_selection_context

(* Type for the code_selection main functions, which has to be passed
   as parameter in some parts of the code selection, notably for
   predicate handling in joins *)

type code_selection_function =
    code_selection_context -> algop_expr -> code_selection_context


(*************************)
(* Operations on indices *)
(*************************)

val create_new_name_index :
    code_selection_context -> Namespace_symbols.relem_symbol -> code_selection_context
(* Note the int is for the number of BTrees for the index *)

(* Access to name indices *)
val get_name_index :
    code_selection_context -> Namespace_symbols.relem_symbol -> Physical_name_index.name_index option

val get_name_index_handler :
    code_selection_context -> Namespace_symbols.relem_symbol -> Physical_name_index.name_index_handler option

val get_all_name_indices :
    code_selection_context -> Physical_name_index.name_indices

val get_all_name_indices_handler :
    code_selection_context -> Physical_name_index.name_indices_handler


 
