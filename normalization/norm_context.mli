(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: norm_context.mli,v 1.30 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Norm_context
   Description:
     This module implements the part of the context which is required
     during the normalization phase.
*)

open Processing_context

open Xquery_common_ast
open Xquery_core_ast

(* Type for the normalization context *)

type norm_context 

(* Table of normalized module interfaces *)

and norm_interface_table = ((Namespace_names.prefix * string), (string list option * norm_context * acinterface)) Hashtbl.t

(* Default normalization context *)

type function_signature = 
    ((cfname * int) * (cfname * cfunction_signature * acfunction_body_kind * updating_modifier))

val default_norm_context : Processing_context.processing_context -> norm_context
val build_norm_context : 
    Processing_context.processing_context -> 
      Namespace_context.nsenv -> 
	Xquery_type_core_ast.cxschema -> 
	  function_signature list -> 
	    norm_context

(* Merge normalization context of an imported module in place *)
val merge_imported_norm_context : norm_context -> norm_context -> unit

(* Replace the processing context *)

val replace_namespace_env_in_norm_context :
    Namespace_context.nsenv -> norm_context -> norm_context

(* Copy the entire norm context *)

val copy_norm_context : norm_context -> norm_context

(* Create copy of norm context, preserving the processing context and
   function signatures, but using a new namespace and schema envs. *)

val copy_norm_context_with_sigs :
    norm_context -> Namespace_context.nsenv -> Xquery_type_core_ast.cxschema -> norm_context

val add_ns_bindings_to_norm_context :
    norm_context -> Namespace_context.binding_table -> norm_context * Namespace_context.nsenv

(* Add a list of function signatures to normalization context *)
val add_sigs_to_norm_context :
    norm_context ->
      function_signature list -> norm_context

val add_sig_to_norm_context_in_place :
  norm_context -> function_signature -> unit

val get_in_scope_nsenv : norm_context -> Namespace_context.nsenv

(* Extract components from normalization the context *)
  
val processing_context_from_norm_context : norm_context -> Processing_context.processing_context
val module_context_from_norm_context : norm_context -> Processing_context.module_processing_context
val nsenv_from_norm_context          : norm_context -> Namespace_context.nsenv
val cxschema_from_norm_context 	     : norm_context -> Xquery_type_core_ast.cxschema
val interface_table_from_norm_context : norm_context -> norm_interface_table

(* Lookup a function's signatures in normalization context *)

val one_sig_from_norm_context :
    norm_context -> (cfname * int) -> cfunction_signature * acfunction_body_kind * updating_modifier
val all_sigs_from_norm_context :
    norm_context -> (cfname * int) -> (cfname * cfunction_signature * acfunction_body_kind * updating_modifier) list

val register_var : norm_context -> cvname -> norm_context
val register_global_var : norm_context -> cvname -> Xquery_core_ast.acvar_body -> norm_context
val check_var    : norm_context -> Finfo.finfo -> cvname -> acvar_body option

val gen_new_cvar : norm_context -> Xquery_ast.expr_handle -> Finfo.finfo -> (cvname * acexpr)
val gen_new_cvar_typed : norm_context -> Xquery_core_ast_annotation.ast_annot -> Xquery_ast.expr_handle -> Finfo.finfo -> (cvname * acexpr)

(* DXQ : true if normalizing within a DXQ execute expression *)
val set_in_execute_expr : norm_context -> bool -> unit
val get_in_execute_expr : norm_context -> bool
val set_interface_table : norm_context -> norm_interface_table -> unit

(* Var graph *)

val set_top_var_decl : norm_context -> Namespace_names.rqname -> unit
val set_top_fun_decl : norm_context -> Namespace_names.rqname -> unit
val unset_top_decl : norm_context -> unit

val add_var_dependency : norm_context -> Namespace_names.rqname -> unit
val add_fun_dependency : norm_context -> Namespace_names.rqname -> unit

val check_cycles : norm_context -> unit

val dump_norm_context : norm_context -> unit
