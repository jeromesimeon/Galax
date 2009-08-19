(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util_matching.ml,v 1.14 2007/07/13 18:24:42 mff Exp $ *)

(* Module: Code_util_matching
   Description:
     This module contains some auxiliary evaluation code for type
     matching.
*)

open Error
open Datatypes
open Datatypes_util

open Xquery_ast
open Xquery_algebra_ast
open Xquery_common_ast

open Dm_types
open Physical_value
open Physical_item

open Norm_context
open Typing_context

open Xquery_algebra_ast

(******************)
(* Error messages *)
(******************)

let raise_type_matching_error stat_ctxt ic sequencetype =
  let s = Serialization.bserialize_datamodel (Norm_context.processing_context_from_norm_context(Typing_context.norm_context_from_stat_context stat_ctxt)) ic in
  raise (error_with_file_location sequencetype.pasequencetype_loc 
    (Query (Type_Error (Print_top.bprintf_asequencetype ("Type of value '\n"^s^"\n' does not match sequence type: ") sequencetype))))

(*****************************************)
(* Auxiliary functions for type matching *)
(*****************************************)

let item_matches_named_type cxschema dtk (singlearg : item) : bool = 
  (* Note:
       New type matching code with named typing!
     - Jerome 05/22/2004
   *)
  match dtk with
  | AITItem -> true
  | _ ->
      begin
	match item_kind singlearg with
	| AtomicValueKind ->
	    begin
	      match dtk with
	      | AITAtomic a ->
		  begin
		    Schema_judge.check_declared_type cxschema a;
		    let at1 = (getAtomicValue singlearg)#atomic_type() in
		    Schema_judge.derives_from cxschema at1 a
		  end
	      | AITNumeric ->
		  let at1 = (getAtomicValue singlearg)#getAtomicValueKind() in
		  (Datatypes_util.atomic_is_numeric at1)
	      | _ -> false
	    end
	| NodeKind ->
	    begin  (* New code to support kind tests 09/06/2005 - Jerome *)
	      match dtk with
	      | AITKindTest AAnyKind ->
		  true
	      | AITKindTest dtk ->
		  let n = getNode singlearg in
		  Dm_step.item_matches_kind_test (n#get_access_ops_dm) cxschema dtk n
	      | _ -> false
	    end
      end

(* dynamic_type_check checks dynamically that a value matches a sequence type.  *)

let dynamic_type_check stat_ctxt dt input_cursor =
  (* 1. Extract the item type as well as the bounds for the Sequence Type *)
  let (adtk,b1,b2) = Typing_util.factor_asequencetype dt in
  let norm_ctxt = norm_context_from_stat_context stat_ctxt in
  let cxschema = cxschema_from_norm_context norm_ctxt in
  (* 2. Auxiliary function to check the bound once we know the
     cardinality of the sequence *)
  let check_bound counter =
    let b = Occurrence.occurs counter
    in (Occurrence.le b b2) && (Occurrence.le b1 b)
  in
  (* 3. Deal with the empty sequence first *)
  if Cursor.cursor_is_empty input_cursor
  then
    begin
      if
	(match adtk with
	| AITEmpty -> true
	| _ -> false) || check_bound 0
      then
	input_cursor
      else
	raise_type_matching_error stat_ctxt input_cursor dt
    end
  else
  (* 4. Now there is at least one item in the sequence *)
    match adtk with
    | AITEmpty ->
	raise_type_matching_error stat_ctxt input_cursor dt
    | _ ->
	let counter = ref 0 in
	let item_match_fun x =
	  incr counter;
	  if (item_matches_named_type cxschema adtk x)
	  then x
	  else raise_type_matching_error stat_ctxt input_cursor dt
	in
	let matched_cursor = Cursor.cursor_map item_match_fun input_cursor in
	let check_bound_fun x =
	  begin
	    if (Cursor.cursor_is_empty input_cursor)
	    then
	      if check_bound !counter
	      then ()
	      else raise_type_matching_error stat_ctxt input_cursor dt
	    else
	      ()
	  end;
	  x
	in
	Cursor.cursor_map check_bound_fun matched_cursor

let dynamic_opttype_check stat_ctxt odt input_cursor =
  match odt with
  | None -> input_cursor
  | Some dt -> dynamic_type_check stat_ctxt dt input_cursor

let dynamic_type_check_item stat_ctxt dt input_item =
  let input_cursor = Cursor.cursor_of_singleton input_item in
  ignore(dynamic_type_check stat_ctxt dt input_cursor)

let dynamic_opttype_check_item stat_ctxt dt input_item =
  let input_cursor = Cursor.cursor_of_singleton input_item in
  ignore(dynamic_opttype_check stat_ctxt dt input_cursor)

let boolean_dynamic_type_check stat_ctxt dt input_item_list =
  let input_cursor = Cursor.cursor_of_list input_item_list in
  try
    Cursor.cursor_iter (fun x -> ()) (dynamic_type_check stat_ctxt dt input_cursor);
    true
  with
  | (Query (Type_Error _)) ->
      false
