(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_item_tuple.ml,v 1.16 2007/08/27 18:49:03 simeon Exp $ *)

(* Module: Code_item_tuple
   Description:
     This module contains code building for operators that implement
     item maps.
*)

open Xquery_algebra_ast
open Xquery_common_ast
open Xquery_algebra_ast_util
open Xquery_physical_algebra_ast
open Xquery_physical_type_ast

open Cs_util_coercion
open Cs_util

open Code_selection_context 
open Code_binding
open Code_util_matching 

open Physical_sequence
open Physical_value_util

open Error


(* The actual quantifier code skeleton used in both cesome, ceevery. *)
let build_default_quantifier_code bind_fun slice_fun quantifier_fun =
  (fun e2 eval alg_ctxt input ->
     let eval_quantifier slice =
       bind_fun slice;
       Physical_util.get_boolean (item_cursor_of_physical_value (eval alg_ctxt e2))
     in
     let b = quantifier_fun eval_quantifier (slice_fun input) in
       [Physical_item_util._boolean b])

(* CESome *)
let build_default_cesome_code bind_fun slice_fun =
  build_default_quantifier_code bind_fun slice_fun Cursor.cursor_exists

let build_default_cesome_sax_value_code code_ctxt v =
  build_default_cesome_code (build_bind_sax_value_to_variable_code code_ctxt v) slice_sax_value

let build_default_cesome_sax_discard_code code_ctxt v =
  build_default_cesome_code (fun slice -> ()) slice_discard_sax_value

let build_default_cesome_item_cursor_code code_ctxt v =
  build_default_cesome_code (build_bind_item_cursor_to_variable_code code_ctxt v) slice_item_cursor

let build_default_cesome_type_checked_item_cursor_code code_ctxt dt v =
  build_default_cesome_code (build_bind_type_checked_item_cursor_to_variable_code code_ctxt dt v) slice_item_cursor

let build_default_cesome_item_list_code code_ctxt v =
  build_default_cesome_code (build_bind_item_list_to_variable_code code_ctxt v) slice_item_list

let build_default_cesome_type_checked_item_list_code code_ctxt dt v =
  build_default_cesome_code (build_bind_type_checked_item_list_to_variable_code code_ctxt dt v) slice_item_cursor

let build_some_code code_ctxt algop (odt, vn) =
  let dep = access_onesub algop.pdep_sub_expression in
  let csc = add_variable_to_current_context code_ctxt vn in
  let physop = Cs_util.get_physical_opname algop in
    match physop with
      | POSome (_, PT_Dom PT_CursorSeq) ->
	  begin
	    match odt with
	      | Some dt ->
		  let fn  = build_default_cesome_type_checked_item_cursor_code csc dt vn in 
		    (coerce_onedep fn dep coerce_unary_item_cursor_to_item_list), csc
	      | _ ->
		  let fn  = build_default_cesome_item_cursor_code csc vn in 
		    (coerce_onedep fn dep coerce_unary_item_cursor_to_item_list), csc
	  end
      | POSome (_, PT_Dom PT_ListSeq) ->
	  begin
	    match odt with
	      | Some dt ->
		  let fn  = build_default_cesome_type_checked_item_list_code csc dt vn in 
		    (coerce_onedep fn dep coerce_unary_item_cursor_to_item_list), csc
	      | _ ->
		  let fn  = build_default_cesome_item_list_code csc vn in 
		    (coerce_onedep fn dep coerce_unary_item_list_to_item_list), csc
	  end
      | POSome (_, PT_Sax PT_Stream) ->
	  let fn  = build_default_cesome_sax_value_code csc vn in 
	    (coerce_onedep fn dep coerce_unary_sax_to_item_list), csc
      | POSome (_, PT_Sax PT_Discarded) ->
	  let fn  = build_default_cesome_sax_discard_code csc vn in 
	    (coerce_onedep fn dep coerce_unary_sax_to_item_list), csc
      | _ -> raise(Query(Code_Selection("Invalid physical operator in build_some_code")))



(* CEEvery *)
let build_default_ceevery_code bind_fun slice_fun =
  build_default_quantifier_code bind_fun slice_fun Cursor.cursor_for_all

let build_default_ceevery_sax_value_code code_ctxt v =
  build_default_ceevery_code (build_bind_sax_value_to_variable_code code_ctxt v) slice_sax_value

let build_default_ceevery_sax_discard_code code_ctxt v =
  build_default_ceevery_code (fun slice -> ()) slice_discard_sax_value

let build_default_ceevery_item_cursor_code code_ctxt v =
  build_default_ceevery_code (build_bind_item_cursor_to_variable_code code_ctxt v) slice_item_cursor

let build_default_ceevery_type_checked_item_cursor_code code_ctxt dt v =
  build_default_ceevery_code (build_bind_type_checked_item_cursor_to_variable_code code_ctxt dt v) slice_item_cursor

let build_default_ceevery_item_list_code code_ctxt v =
  build_default_ceevery_code (build_bind_item_list_to_variable_code code_ctxt v) slice_item_list

let build_default_ceevery_type_checked_item_list_code code_ctxt dt v =
  build_default_ceevery_code (build_bind_type_checked_item_list_to_variable_code code_ctxt dt v) slice_item_cursor

let build_every_code code_ctxt algop (odt, vn) =
  let dep = access_onesub algop.pdep_sub_expression in 
  let csc = add_variable_to_current_context code_ctxt vn in
  let physop = Cs_util.get_physical_opname algop in 
    match physop with
      | POEvery (_, PT_Dom PT_CursorSeq) ->
	  begin
	    match odt with
	      | Some dt ->
		  let fn  = build_default_ceevery_type_checked_item_cursor_code csc dt vn in 
		    (coerce_onedep fn dep coerce_unary_item_cursor_to_item_list), csc
	      | _ ->
		  let fn  = build_default_ceevery_item_cursor_code csc vn in 
		    (coerce_onedep fn dep coerce_unary_item_cursor_to_item_list), csc
	  end
      | POEvery (_, PT_Dom PT_ListSeq) ->
	  begin
	    match odt with
	      | Some dt ->
		  let fn  = build_default_ceevery_type_checked_item_list_code csc dt vn in 
		    (coerce_onedep fn dep coerce_unary_item_cursor_to_item_list), csc
	      | _ ->
		  let fn  = build_default_ceevery_item_list_code csc vn in 
		    (coerce_onedep fn dep coerce_unary_item_list_to_item_list), csc
	  end
      | POEvery (_, PT_Sax PT_Stream) ->
	  let fn  = build_default_ceevery_sax_value_code csc vn in 
	    (coerce_onedep fn dep coerce_unary_sax_to_item_list), csc
      | POEvery (_, PT_Sax PT_Discarded) ->
	  let fn  = build_default_ceevery_sax_discard_code csc vn in 
	    (coerce_onedep fn dep coerce_unary_sax_to_item_list), csc
      | _ -> raise(Query(Code_Selection("Invalid physical operator in build_every_code")))



(* Item-to-tuple map *)
let build_default_item_to_tuple_map_code bind_fun slice_fun =
  (fun return_exp eval alg_ctxt input ->
     let eval_for slice =
       bind_fun slice;
       tuple_cursor_of_physical_value (eval alg_ctxt return_exp)
     in
       Cursor.cursor_map_concat eval_for (slice_fun input))

let build_default_item_to_tuple_map_sax_value_code code_ctxt v =
  build_default_item_to_tuple_map_code (build_bind_sax_value_to_variable_code code_ctxt v) slice_sax_value

let build_default_item_to_tuple_map_sax_discard_code code_ctxt v =
  build_default_item_to_tuple_map_code (fun slice -> ()) slice_discard_sax_value

let build_default_item_to_tuple_map_item_cursor_code code_ctxt v =
  build_default_item_to_tuple_map_code (build_bind_item_cursor_to_variable_code code_ctxt v) slice_item_cursor

let build_default_item_to_tuple_map_item_list_code code_ctxt v =
  build_default_item_to_tuple_map_code (build_bind_item_list_to_variable_code code_ctxt v) slice_item_list

(* MapFromItem *)
let build_item_to_tuple_map_code code_ctxt algop vn = 
  let dep = access_onesub algop.pdep_sub_expression in 
  let csc = add_variable_to_current_context code_ctxt vn in
  let physop = Cs_util.get_physical_opname algop in 
    match physop with
      | POMapFromItem (_, PT_Dom PT_CursorSeq) ->
	  let fn  = build_default_item_to_tuple_map_item_cursor_code csc vn in 
	    (coerce_onedep fn dep coerce_unary_item_cursor_to_tuple_cursor), csc
      | POMapFromItem (_, PT_Dom PT_ListSeq) ->
	  let fn  = build_default_item_to_tuple_map_item_list_code csc vn in 
	    (coerce_onedep fn dep coerce_unary_item_list_to_tuple_cursor), csc
      | POMapFromItem (_, PT_Sax PT_Stream) ->
	  let fn  = build_default_item_to_tuple_map_sax_value_code csc vn in 
	    (coerce_onedep fn dep coerce_unary_sax_to_tuple_cursor), csc
      | POMapFromItem (_, PT_Sax PT_Discarded) ->
	  let fn  = build_default_item_to_tuple_map_sax_discard_code csc vn in 
	    (coerce_onedep fn dep coerce_unary_sax_to_tuple_cursor), csc
      | _ -> raise(Query(Code_Selection("Invalid physical operator in build_item_to_tuple_map_code")))

(* Tuple-to-item map *)
let build_default_tuple_to_item_map_code code_ctxt =
  (* Now returning an xml_value here. - Michael *)
  (fun return_exp eval alg_ctxt input_cursor ->
    let eval_for tup =
      xml_value_of_physical_value (eval alg_ctxt return_exp)
    in
    concat_xml_value_cursor (Cursor.cursor_map eval_for input_cursor))
    
(* MapToItem *)
let build_tuple_to_item_map_code code_ctxt algop  =
  let dep = access_onesub algop.pdep_sub_expression in 
  let fn = build_default_tuple_to_item_map_code code_ctxt in
    (coerce_onedep fn dep coerce_unary_tuple_cursor_to_xml), code_ctxt
