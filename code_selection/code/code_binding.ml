(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_binding.ml,v 1.12 2007/02/21 21:14:44 simeon Exp $ *)

(* Module: Code_binding
   Description:
     This module contains code building for operators that bind
     variable or tuple fields.
*)

open Processing_context
open Code_selection_context

open Tuple_context_manager

open Xquery_algebra_ast
open Xquery_physical_type_ast
open Xquery_physical_type_ast_util
open Xquery_algebra_ast_annotation_util

open Physical_xml_value
open Physical_value_util

open Error


(* Variables *)

(* Selects the _smallest_ physical xml type for the binding
   according to current command-line switches, variable use
   counts, independent input signature and type checking
   requirements.

   The physical type returned here may thus be different ('too
   small') from the actual type enforced by means of coercion
   functions. - Michael *)
let select_physical_variable_binding code_ctxt indep_signature (odt, vn) =
  let physical_xml_type =
    if (not !Conf.force_materialized_variables)
    then
      let raise_not_found () =
	raise (Query (Code_Selection ("Variable " ^ (Namespace_names.prefixed_string_of_rqname vn) ^ " not listed in Code_binding.select_physical_variable_binding.")))
      in
      let variable_use_counts = get_bound_use_counts (retrieve_annotation "select_physical_variable_binding" code_ctxt) in
      let must_type_check = match odt with | Some _ -> true | _ -> false in
      if (not must_type_check)
      then
	begin
	  try
	    match (List.assoc vn variable_use_counts) with
	    | (0, Never) -> (PT_Sax PT_Discarded)
	    | (1, Once) -> (PT_Sax PT_Stream)
	    | _ -> (PT_Dom PT_ListSeq)
	  with Not_found -> raise_not_found ()
	end
      else
	begin
	  try
	    match (List.assoc vn variable_use_counts) with
	    | (0, Never) -> (PT_Dom PT_CursorSeq)
	    | (1, Once) -> (PT_Dom PT_CursorSeq)
	    | _ -> (PT_Dom PT_ListSeq)
	  with Not_found -> raise_not_found ()
	end
    else (PT_Dom PT_ListSeq)
  in
  let indep_xml_type = access_one_non_discarded_xml_type indep_signature in
    (vn, least_upper_xml_type physical_xml_type indep_xml_type)

let build_bind_variable_code code_ctxt vn conv =
  let f = build_current_insert_code code_ctxt vn in
    (fun input ->
       let pv = conv input in
	 f pv)

let build_bind_item_cursor_to_variable_code code_ctxt vn =
  build_bind_variable_code code_ctxt vn xml_value_of_item_cursor

let build_bind_type_checked_item_cursor_to_variable_code code_ctxt dt vn =
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let type_check item_cursor = Code_util_matching.dynamic_type_check stat_ctxt dt item_cursor in
  let f item_cursor = xml_value_of_item_cursor (type_check item_cursor) in
    build_bind_variable_code code_ctxt vn f

let build_bind_item_list_to_variable_code code_ctxt vn =
  build_bind_variable_code code_ctxt vn xml_value_of_item_list

(* This is _not_ a misnomer. Although the function's argument
   is an item cursor, a _list_ is bound to the variable. *)
let build_bind_type_checked_item_list_to_variable_code code_ctxt dt vn =
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let type_check item_cursor = Code_util_matching.dynamic_type_check stat_ctxt dt item_cursor in
  let f item_cursor = materialize_xml_value (xml_value_of_item_cursor (type_check item_cursor)) in
    build_bind_variable_code code_ctxt vn f

let build_bind_sax_value_to_variable_code code_ctxt vn =
  build_bind_variable_code code_ctxt vn xml_value_of_sax_value


(* Tuples *)

(* Selects the _smallest_ physical xml type for the binding
   according to current command-line switches, tuple field use
   counts and independent input signature and type checking
   requirements.

   The physical type returned here may thus be different ('too
   small') from the actual type enforced by means of coercion
   functions. - Michael *)
let select_physical_tuple_field_binding code_ctxt indep_xml_type (odt, tn) =
  let must_type_check = match odt with | Some _ -> true | _ -> false in
  let physical_xml_type =
    if (!Conf.allow_streamed_tuple_fields && not must_type_check)
    then
      let raise_not_found () =
	raise (Query (Code_Selection ("Tuple field " ^ (Namespace_names.prefixed_string_of_rqname tn) ^ " not listed in Code_binding.select_physical_tuple_field_binding.")))
      in
      let global_annotation = retrieve_global_annotation code_ctxt in
      let (tuple_field_use_counts, _, _) = global_annotation.tuple_field_use_counts in
      try
	match (List.assoc tn tuple_field_use_counts) with
	| (0, Never) -> (PT_Sax PT_Discarded)
	| (1, Once) -> (PT_Sax PT_Stream)
	| _ -> (PT_Dom PT_ListSeq)
      with Not_found -> raise_not_found ()
    else (PT_Dom PT_ListSeq)  
  in
    (tn, least_upper_xml_type physical_xml_type indep_xml_type)

let select_physical_tuple_binding code_ctxt indep_signature names =
  let name_list = Array.to_list names in
  let indep_xml_types = access_many_non_discarded_xml_types indep_signature in
    List.map2 (fun it name -> select_physical_tuple_field_binding code_ctxt it name) indep_xml_types name_list

let build_bind_tuple_field_code code_ctxt tn conv =
  let tr = get_tuple_reference code_ctxt tn in
  let f = build_tuple_store_code tr in
    (fun input ->
       let xml_value = conv input in
	 f xml_value)
      
let build_bind_sax_value_to_tuple_field_code code_ctxt tn =
  let conv input = xml_value_of_physical_value (physical_value_of_sax_value input) in
    build_bind_tuple_field_code code_ctxt tn conv

let build_bind_item_cursor_to_tuple_field_code code_ctxt tn =
  let conv input = xml_value_of_physical_value (physical_value_of_item_cursor input) in
    build_bind_tuple_field_code code_ctxt tn conv

let build_bind_type_checked_item_cursor_to_tuple_field_code code_ctxt dt tn =
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let type_check item_cursor = Code_util_matching.dynamic_type_check stat_ctxt dt item_cursor in
  let conv input = xml_value_of_physical_value (physical_value_of_item_cursor (type_check input)) in
    build_bind_tuple_field_code code_ctxt tn conv

let build_bind_item_list_to_tuple_field_code code_ctxt tn =
  let conv input = xml_value_of_physical_value (physical_value_of_item_list input) in
    build_bind_tuple_field_code code_ctxt tn conv

(* This is _not_ a misnomer. Although the function's argument
   is an item cursor, a _list_ is bound to the tuple field. *)
let build_bind_type_checked_item_list_to_tuple_field_code code_ctxt dt tn =
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let type_check item_cursor = Code_util_matching.dynamic_type_check stat_ctxt dt item_cursor in
  let conv input = 
      (materialize_xml_value (xml_value_of_item_cursor (type_check input))) in
  build_bind_tuple_field_code code_ctxt tn conv

