(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_type_operators.ml,v 1.6 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_type_operators
   Description:
     This module contains code building for operators on types.
*)

open Code_util_matching
open Code_selection_context
open Cs_util_coercion
open Error
open Execution_context
open Norm_context
open Physical_value
open Physical_value_util
open Typing_context
open Xquery_algebra_ast
open Xquery_common_ast
open Xquery_algebra_ast_util

(* CETreat *)
   (* JS: no need to perform validation there
      because strong typing makes sure this will succeed *)
   (* JS: on the other hand, wouldn't validation annotate the data
      with appropriate type information ? *)
let build_default_cetreat_code code_ctxt cdt =
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  (fun alg_ctxt c ->
    dynamic_type_check stat_ctxt cdt c)

let build_treat_code code_ctxt algop cmodel1 =
  let _ = access_nosub algop.pdep_sub_expression in 
  let fn = build_default_cetreat_code code_ctxt cmodel1 in
  (coerce_nodep fn coerce_unary_item_cursor_to_item_cursor), code_ctxt

(* CEValidate *)

let build_default_cevalidate_strict_code code_ctxt =
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let norm_ctxt = norm_context_from_stat_context stat_ctxt in
  let cxschema = cxschema_from_norm_context norm_ctxt in
  (fun alg_ctxt input_stream ->
    (* 1. Input must be a SAX Stream *)
    let erased_input_stream = Streaming_ops.erase_xml_stream input_stream in
    
    (* 2. Perform validation *) 
    Schema_validation.validate cxschema erased_input_stream	
  )

let build_validate_code code_ctxt algop vmode =
  match vmode with
  | Strict ->
      let fn = build_default_cevalidate_strict_code code_ctxt in
      let _ = access_nosub algop.pdep_sub_expression in 
      (coerce_nodep fn coerce_unary_sax_to_sax), code_ctxt
  | Lax ->
      raise (Query (Prototype "Lax validation not supported yet"))

(* CECast 
   NB: Note that the namespace environment is a static parameter of the algebraic operator. *)
let build_default_cecast_code code_ctxt adt nsenv =
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let norm_ctxt = norm_context_from_stat_context stat_ctxt in
  let cxschema = cxschema_from_norm_context norm_ctxt in
  let (base_type,opt_flag) = Schema_norm_util.get_optional_atomic_type_asequencetype cxschema adt in
  let atomic_type = Schema_judge.atomic_type_of_typename cxschema base_type in
  (fun alg_ctxt e ->
    let e = Cursor.cursor_of_list e in
    if (Cursor.cursor_is_empty e && opt_flag)
    then []
    else
      let base_value =
	try
	  Physical_util.get_singleton_atomic e
	with
	| _ ->
	    raise (Query (Expr_Error ("Cast not applied to a single atomic value")))
      in
      let new_base_value = base_value#cast_to nsenv base_type atomic_type in
      [Item_Atomic new_base_value]
  )

let build_cast_code code_ctxt algop (nsenv,cmodel1) =
  let _ = access_nosub algop.pdep_sub_expression in 
  let fn = build_default_cecast_code code_ctxt cmodel1 nsenv in
  (coerce_nodep fn coerce_unary_item_list_to_item_list), code_ctxt

(* CECastable:
   NB: Note that the namespace environment is a static parameter of the algebraic operator.  *)
let build_default_cecastable_code code_ctxt adt nsenv =
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let norm_ctxt = norm_context_from_stat_context stat_ctxt in
  let cxschema = cxschema_from_norm_context norm_ctxt in
  let (base_type,opt_flag) = Schema_norm_util.get_optional_atomic_type_asequencetype cxschema adt in
  let atomic_type = Schema_judge.atomic_type_of_typename cxschema base_type in
  (fun alg_ctxt e ->
    let e = Cursor.cursor_of_list e in
    let b = 
      if (Cursor.cursor_is_empty e && opt_flag)
      then true
      else
	try
	  let base_value =
	    try
	      Physical_util.get_singleton_atomic e
	    with
	    | _ ->
		raise (Query (Expr_Error ("Cast not applied to a single atomic value")))
	  in
	  let _ = base_value#cast_to nsenv base_type atomic_type in true
	with
	| _ -> false
      in
      [Physical_item_util._boolean b])

let build_castable_code code_ctxt algop (nsenv,cmodel1) = 
  let _ = access_nosub algop.pdep_sub_expression in 
  let fn = build_default_cecastable_code code_ctxt cmodel1 nsenv in
  (coerce_nodep fn coerce_unary_item_list_to_item_list), code_ctxt
