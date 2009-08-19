(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_hash_join.ml,v 1.8 2007/08/23 21:01:32 simeon Exp $ *)

(* Module: Code_hash_join
   Description:
     This module contains code building for operators that implement
     hash joins.
*)

(*** 
  WARNING:

  This code may still be ** UNSAFE **

  It handles type promotion now

  Currently, it will only be enabled if the -enable-unsafe-join-hack
  flag is set

*)

    (**************************************************************************) 
    (*********************************)
    (**** THIS IS EXTREMELY DIRTY ****)
    (*********************************)

    (* The above join information splits the incoming hash predicate
       into possibly four pieces.  These pieces are described in
       detail in the Code_util_join. This code also does code
       selection on *copies* of the pieces. This is very hackish - it
       is done inside the more complicated operands now.

       A solution is to have a physical ast (which we are currently
       planning on doing in the future). This physical AST will
       represent the algebra. In this AST, HashJoin will be a physical
       operation that can be picked up. It will have a particular
       semantic of its operations that will be directly identified
       there.  This solution is cleaner but the details have not been
       worked out.

       The bottom line is: If you start playing with this code, it
       could get nasty - and it should eventually fade away. *)

    (**************************************************************************) 


open Error

open Datatypes
open Datatypes_util

open Xquery_core_ast
open Xquery_common_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_physical_type_ast
open Algebra_type

open Norm_context
open Typing_context
open Compile_context
open Code_selection_context
open Execution_context

open Dm
open Physical_sequence
open Physical_value_util 
open Physical_table

open Cs_util

open Algebra_type
open Cs_util_coercion

open Compile_context
open Code_selection_context   

open Optimization_util

open Code_util_materialize
open Code_util_pattern_matcher
open Code_util_predicates
open Code_util_ridlist


(* When we have fs:untyped-to-any:
   o All numeric types are promoted (for comparison) to ATDouble
   o All untypedAtomic are:
     o promoted to strings and hashed
     o if we can cast them to doubles -> we do so
       and ALSO hash this.
  Since we require that the types be the same for the match
  this will be ok to have both floating around. 
   
  The duplicate elimination will be done by the second.

   The goal is to capture the the semantic of fs:untyped-to-any on 
   a stream.
*)

(***********************)
(* Hashing information *)
(***********************)
(* If successfully completed:
   Assume most general single (hashable) predicate
   Some x in |left_in_op| 
     Some y in |right_in_op|
      op:equal(|left_equal_op |, 
               |right_equal_op|)

   Notice left_equal_op is associated with left_in_op so that 
   unless it is fs:untyped-to-any() (which we deal with specially)
   we must have that right_equal_op independent from right_in_op.

   After completion the invariants hold. 
   If *_in_op is None -> it was not there so hash on the value
                         returned in *_equal_op.

   If left_equal_op is None -> it was fs:untyped-to-any and 
                               the comparison function should be
                               polymorphic
*)
(* This tells us how to deal with incoming values *)

(*********************************************************************)
(* This section deals with the type promotion semantics necessary to *)
(* do proper hashing                                                 *)
(*********************************************************************)

(* Integer sort and remove duplicates *)
let sort_remove_duplicates_and_split d = 
  if d = [] then 
    begin
      []
    end
  else (* has at least one element, so List.hd call is safe *)
    begin
      (* Sort them in opposite order so we can use tail recursive
	 fold_left without reverse or [] *)
      let sorted_list = List.fast_sort (fun x y -> y - x) d in
      let head = List.hd sorted_list in 

	(* now remove duplicates *)
	(* fst because last_seen is already in the list *)
	fst (List.fold_left (fun (cur_list, last_seen) new_value ->
			       if new_value = last_seen then
				 (cur_list, last_seen)
			       else
				 (new_value :: cur_list, new_value)) ([head], (head)) sorted_list)
    end

(* This compiles on part of the branch
   for example:
   Some $x in L
   ... op:equal(e_l) it returns (copies to be used) of code-selected ops
       this is necessary so that we can create the correct assign function. *)

(*****************************)
(*****************************)
(* THE ACTUAL BUILD FRONTEND *)
(*****************************)
(*****************************)

let build_hash_join nm_option code_ctxt built_cond pred_desc =
  (* Setup the contexts *)
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let norm_ctxt = norm_context_from_stat_context stat_ctxt in
  let nsenv     = nsenv_from_norm_context norm_ctxt in
  let annot     = retrieve_annotation "build_hash_join" code_ctxt in 
    
  let array_materialize_fun, array_restore_fun, needed_names =
    materialize_cursor_to_dom_value_array code_ctxt annot () in

  (********************************************)
  (* This is to null (if we are an outer join *)
  (********************************************)
  let (is_outer_join,null_function,not_null_fn,empty_right) =
    Code_util_join.get_null_functions code_ctxt nm_option needed_names
  in
  let fn =
    (* These ops are code-selected in this code.
       The predicate must be remvoed from code selection *)    
    (fun () eval alg_ctxt left_cursor right_cursor ->
      (****************************************)
      (* Runtime-Init code for this predicate *)
      (****************************************)

      (* This could be shared among many joins *)
      Debug.print_join_debug ">>> In HASH Join, BEFORE materialization point";
      let materialized_array = array_materialize_fun eval alg_ctxt right_cursor in
      Debug.print_join_debug ">>> In HASH Join, AFTER materialization point";

      Debug.print_join_debug ">>> In HASH Join, Building hash";
      let built_predicates =
	Array.map (fun (outer, inner, _) ->
	  let hash_materialize, restore_fn, needed_names =
	    materialize_array_to_hash code_ctxt annot array_restore_fun (inner,nsenv)
	  in
	  let the_hash_table = hash_materialize eval alg_ctxt materialized_array in
	  the_hash_table, (outer, inner)) built_cond
      in
      Debug.print_join_debug ">>> In HASH Join, AFTER building hash";

      (************************)
      (* The probing function *)
      (************************)
      (* This will give us back a sequence of indexes into the materialized array .*)

      (* This can be parameterized by the outer_predicate_branch and the hash table *)
      let hash_probe current_hash current_outer_predicate_branch =
	(* Get back the associated atomic values *)
	let atomic_values = evaluate_predicate_branch 
	    current_outer_predicate_branch eval alg_ctxt nsenv in

	(* now probe each one *)
	List.concat
	  (List.map
	     (Dm_atomic.AtomicValueHash.find_all current_hash) 
	     atomic_values)
      in

      (* predicate description *)	 
      let hash_probe_to_rid_list (ch,(copb,cipb)) =
	let ret_list = 
	  (sort_remove_duplicates_and_split 
	     (hash_probe ch copb))
	in
	if List.length ret_list = 0 then
	  Empty_RidList
	else
	  Regular_RidList ret_list
      in
      let get_rid_list_for_tuple () = 
	Code_util_predicates.eval_predicate_desc_to_rid_list 
	  hash_probe_to_rid_list 
	  built_predicates 
	  pred_desc
      in
      let eval_fn tup = 	 
	let rid_list = get_rid_list_for_tuple () in
	(* Unwrap code *)
	let ret =
	  unwrap_rid_list_cursor materialized_array array_restore_fun rid_list
	in
	if (is_outer_join) then
	  begin 
	    if Cursor.cursor_is_empty ret then
	      begin
		null_function ();
		array_restore_fun empty_right;
		table_of_singleton empty_tuple
	      end
	    else
	      begin
		not_null_fn ();
		ret
	      end
	  end
	else
	  ret
      in
      Debug.print_join_debug ">>> In HASH Join, NOW creating output cursor";
      Cursor.cursor_map_concat eval_fn left_cursor
    )
  in
  (coerce_unitdep fn () coerce_binary_tuple_cursor_to_tuple_cursor), code_ctxt


