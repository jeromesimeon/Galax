(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_tpnf_util.mli,v 1.6 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Factorize_tpnf_util
   Description:
     This module contains utilities for the TPNF.
*)

open Error

open Namespace_names
open Namespace_builtin

open Norm_util

open Xquery_common_ast
open Xquery_common_ast_util

open Xquery_core_ast
open Xquery_type_core_ast
open Xquery_core_ast_util
open Xquery_core_ast_annotation

open Ast_walker_rewrite_context
open Ast_walker_rewrite

open Processing_context
open Typing_context

val debug_apply: string -> acexpr -> acexpr -> unit

val get_opt_sbdo_arg_desc: acexpr -> acexpr_desc option
val get_one_arg_from_call: acexpr -> acexpr
val get_opt_sbdo_arg: acexpr ->
    (cfname * acexpr * 
       ((csequencetype * cxtype) option list * (csequencetype * cxtype)) 
       * updating_modifier) option

val wrap_in_sbdo: static_context rewrite_context -> acexpr -> acexpr
val wrap_in_fn_boolean: static_context rewrite_context -> acexpr -> acexpr

val var_name_equals: acexpr -> cvname -> bool
val is_free_var_of: cvname -> acexpr -> bool
val observes_doc_order: acexpr -> bool

val is_step: acexpr -> bool
val mk_fn_false: static_context rewrite_context -> 
  Xquery_ast.expr_handle -> 
    Finfo.finfo -> acexpr

val get_properties: 
    static_context rewrite_context ->
      acexpr ->
	(cvname, bool * bool * bool * bool) Hashtbl.t ->
	  bool * bool * bool * bool

val is_in_tpnf: acexpr -> bool
val is_in_tpnf': acexpr -> bool
val is_in_cxq_plus: static_context rewrite_context -> acexpr -> bool
