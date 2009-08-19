(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_map.ml,v 1.9 2008/03/21 19:02:32 simeon Exp $ *)

(* Module: Code_map
   Description:
     This module contains code building for operators that implement
     maps.
*)

open Code_selection_context
open Cs_util_coercion
open Cs_util
open Physical_sequence
open Physical_table
open Physical_value_util
open Xquery_algebra_ast
open Xquery_algebra_ast_util

(* Tuple index map *)
let build_default_tuple_map_index_code code_ctxt var =
  let restore_fn = build_create_tuple_code code_ctxt var in
  (fun alg_ctxt input_cursor ->
    (* If input_cursor is empty, we want to make sure that
       the variable is invalid *)
    if Cursor.cursor_is_empty input_cursor then
      begin
	restore_fn empty_sequence;  
	input_cursor
      end
    else
      begin
	let index = ref 1 in
	let eval_index tup =
	  let dm_index = 
	    Physical_xml_value.xml_value_of_dom_value (
	    materialized_of_list ([Physical_item_util._integer (Decimal._integer_of_int !index)])) in
	  incr index;
	  (* Add in the tuple that gives the index ... *)
	  restore_fn dm_index;	
	  tup
	in
	Cursor.cursor_map eval_index input_cursor
      end
  )

let build_tuple_map_index_code code_ctxt algop vname =
  let _ = access_nosub algop.pdep_sub_expression in
  let code_ctxt' = add_tuple_reference code_ctxt vname in
  let fn = build_default_tuple_map_index_code code_ctxt' vname in
  (coerce_nodep fn coerce_unary_tuple_cursor_to_tuple_cursor), code_ctxt'

let build_default_nullmap_code code_ctxt tn = 
  let loj_null_fn      = build_create_dom_tuple_code code_ctxt tn in
  (fun alg_ctxt input_cursor ->
    if Cursor.cursor_is_empty input_cursor then
      begin
  	loj_null_fn empty_dom_sequence;
	table_of_singleton empty_tuple
      end
    else
      begin
	loj_null_fn non_empty_dom_sequence;
	input_cursor
      end)

let build_null_map_code code_ctxt algop v =
  let _ = access_nosub algop.pdep_sub_expression in 
  let code_ctxt = add_tuple_reference code_ctxt v in
  let fn = build_default_nullmap_code code_ctxt v in 
  (coerce_nodep fn coerce_unary_tuple_cursor_to_tuple_cursor), code_ctxt
	      
let build_default_tuple_map_code code_ctxt =
  (fun return_exp eval alg_ctxt input_cursor ->
    let eval_for tup =
      tuple_cursor_of_physical_value (eval alg_ctxt return_exp)
    in
    Cursor.cursor_map_concat eval_for input_cursor)

let build_tuple_map_code code_ctxt algop =
  let dep = access_onesub algop.pdep_sub_expression in 
  let fn = build_default_tuple_map_code code_ctxt in
  (coerce_onedep fn dep coerce_unary_tuple_cursor_to_tuple_cursor), code_ctxt

(***************)
(* Concat maps *)
(***************)
(* Tuple Concat:
   ConcatMap{ Expr_1 ++ input }( E_2 )   *)
(* tuple concat is really handled by the allocation, 
   not the function itself. The annotations decide what is returned
   so these functions really just iterate through the cursors
   with the correct cardinality.   
*)
let build_default_tuple_map_concat_code code_ctxt = 
    (fun left eval alg_ctxt input_cursor -> 
       let eval_fun tup = 
	 tuple_cursor_of_physical_value (eval alg_ctxt left) 
       in
	 Cursor.cursor_map_concat eval_fun input_cursor
    )

let build_tuple_map_concat_code code_ctxt algop =
  let dep = access_onesub algop.pdep_sub_expression in 
  let fn  = build_default_tuple_map_concat_code code_ctxt in
  (coerce_onedep fn dep coerce_unary_tuple_cursor_to_tuple_cursor),  code_ctxt

(******************************************************)
(* NOTE: There is a potential problem with this code. *)
(* It does *NOT* Properly destroy the contents of the
   dependent expression tuple. This is because these
   tuples have not been allocated at this point in code
   selection - although they are in scope. This is due
   to tuple scoping being the reverse of variable
   scoping. It should not present a problem as no operator
   accesses them - but, it could be a problem for 
   debugging. I would not suggest relying on this 
   behavior

   This should eventually be fixed                    *)
(******************************************************)
let build_default_outer_tuple_map_concat_code code_ctxt null_name = 
  let restore_fn = build_create_tuple_code code_ctxt null_name in
    (fun dep eval alg_ctxt input_cursor -> 
       let eval_fun tup = 
	 let ret = tuple_cursor_of_physical_value (eval alg_ctxt dep) in
	   if Cursor.cursor_is_empty ret then
	     begin
	       (* set to empty 
		  ((returned_fields dep) - (returned_fields input_cursor))  *)
	       restore_fn empty_sequence;
	       (* Physical_table.table_empty () *)
	       table_of_singleton empty_tuple
	     end
	   else
	     begin
	       restore_fn non_empty_sequence;
	       ret
	     end
       in
	 Cursor.cursor_map_concat eval_fun input_cursor
    )

let build_outer_tuple_map_concat_code code_ctxt algop vn =
  let code_ctxt' = add_tuple_reference code_ctxt vn in	  
  let dep = access_onesub algop.pdep_sub_expression in 
  let fn  = build_default_outer_tuple_map_concat_code code_ctxt' vn in
  (coerce_onedep fn dep coerce_unary_tuple_cursor_to_tuple_cursor), code_ctxt'

     
