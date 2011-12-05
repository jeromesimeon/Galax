(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_constructors.mli,v 1.11 2007/08/17 18:28:54 simeon Exp $ *)

(* Module: Code_constructors
   Description:
     This module contains code building for constructors.
*)

open Code_selection_context
open Execution_context
open Namespace_symbols
open Physical_value

val build_scalar_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	Xquery_common_ast.literal -> 
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_seq_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_imperative_seq_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_empty_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_error_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_document_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_pi_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Namespace_names.ncname * string) ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_picomputed_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_comment_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	Datatypes.xs_untyped ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_commentcomputed_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_text_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	Datatypes.xs_untyped ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_charref_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	int ->
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_textcomputed_code :
    code_selection_context ->
      Algebra_type.algop_expr -> 
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_elem_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	(relem_symbol * Namespace_context.nsenv) ->
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_anyelem_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	Namespace_context.nsenv ->
	  Namespace_context.nsenv ->
	    (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_attr_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	rattr_symbol ->
	  Namespace_context.nsenv ->
	    (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val build_anyattr_code :
    code_selection_context -> 
      Algebra_type.algop_expr -> 
	Namespace_context.nsenv ->
	(Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

val select_physical_op : 
  Code_selection_context.code_selection_context -> 
    Xquery_algebra_ast.input_signature ->
      Algebra_type.algop_expr -> 
	Xquery_physical_algebra_ast.physop_expr_name

