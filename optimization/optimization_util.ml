(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_util.ml,v 1.32 2007/02/12 21:15:33 simeon Exp $ *)

(* Module: Optimization_util
   Description:
     This module contains some utility functions used during algebraic
     optimization.
 *)

open Compile_annotate
open Compile_context

open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_algebra_ast_annotation_util

open Optimization_walker
open Logical_algebra_types
open Error


(**************************)
(* *** Tuple Renaming *** *)
(**************************)

(* We assume all tuple field names are unique *)
(* Notice that since tuple names can occur above the location where they are being renamed
   (as opposed to variables which are in scope logically below where they are)
   we need to use side effects on the name to handle the update.
   I don't like this but, it seems hard to avoid because we have two distinct scopes. *)
let replace_tuple_field_name cur_name replacement_name comp_ctxt algop =
  let replace_name changed name =
    if Namespace_names.rqname_equal name cur_name then
      begin
	changed := true;
	replacement_name
      end
    else
      name
  in
  let rename_group changed gd =
    let gbn           = List.map (replace_name changed) (get_group_names gd)   in
    let induced       = List.map (replace_name changed) (get_induced_group gd) in 
    let must_be_valid = List.map (replace_name changed) (get_valid_names gd)   in
    let agg_name      = replace_name changed (get_aggregate_name gd) in
    let agg_type      = get_aggregate_type gd in 
      mk_group_desc gbn induced must_be_valid (agg_type,agg_name)
  in
  let sub_name, changed = 
    match algop.palgop_expr_name with
      | AOEAccessTuple tn when (Namespace_names.rqname_equal tn cur_name) ->
	  (AOEAccessTuple replacement_name), true
      | AOEMapIndex mi when (Namespace_names.rqname_equal mi cur_name) ->
	  (AOEMapIndex replacement_name), true
      | AOEMapIndexStep vn when (Namespace_names.rqname_equal vn cur_name) ->
	  (AOEMapIndexStep replacement_name), true

      | AOECreateTuple ct ->
	  let changed = ref false in
	  let ct =
	    Array.map 
	      (fun (aodt, name) ->
		 let name = (replace_name changed) name in
		   aodt, name
	      ) ct in
	    (AOECreateTuple ct), !changed

      | AOEGroupBy gd_list ->
	  let changed = ref false in
	  let gd_list = List.map (rename_group changed) gd_list in
	    (AOEGroupBy gd_list), !changed
	    
      | AOETupleTreePattern (input_field, pattern) ->
	  let _ = replace_field_in_pattern pattern cur_name replacement_name in  
	  if (Namespace_names.rqname_equal input_field cur_name) then
	    (AOETupleTreePattern(replacement_name, pattern)), true
	  else
	    algop.palgop_expr_name, false 

(*
      | AOEPrune (tf, axis) ->
          let changed = ref false in
	  AOEPrune (replace_name changed tf, axis), !changed
      
      | AOEDistinct tf ->
	  let changed = ref false in
	  AOEDistinct (replace_name changed tf), !changed
*)
      | _ ->
	  algop.palgop_expr_name, false
	    
  in
    (if changed then
       begin
	 (*replace_aalgop_name algop sub_name *)
	 algop.palgop_expr_name <- sub_name;
	   algop
	end
    else
      algop), changed

(* ASSUMES ALL TUPLE NAMES ARE UNIQUE *)
let replace_tuple_name comp_ctxt cur_name replace_name root = 
  let subst_changed = ref false in
  let wrapped_apply = generic_wrapper (replace_tuple_field_name cur_name replace_name) in
  let rule_apply    = wrapped_apply subst_changed comp_ctxt root   in
  let ret_value     = rewrite_apply rule_apply None root in
  ret_value, !subst_changed

(**** Some generic stuff ****)
let wrap_sep_sequence comp_ctxt op =
  match op.palgop_expr_name with
  | AOEMapIndexStep vn ->
	vn, op
    | _ ->
	let eh           = op.palgop_expr_origin in 
	let fi           = op.palgop_expr_loc    in
	let vn           = get_new_group_name comp_ctxt in 
	let sep_seq_op = logical_aalgop_mkop (AOEMapIndexStep vn) (OneSub op) NoSub None eh fi in      		  
	  vn, sep_seq_op
	
(*    | AOEMapIndex vn -> (* Should these be handled sep. *) *)

let wrap_map_index_name v1 op =
  let eh           = op.palgop_expr_origin in 
  let fi           = op.palgop_expr_loc    in
  let map_index_op = logical_aalgop_mkop (AOEMapIndex v1) (OneSub op) NoSub None eh fi in 
  map_index_op

let wrap_map_index comp_ctxt op =
  match op.palgop_expr_name with
    | AOEMapIndex vn ->
	(* It is already mapped *)
	vn, op
    | _->
	let fresh_name = get_new_group_name comp_ctxt in 
	let ret_op     = wrap_map_index_name fresh_name op in
	  fresh_name, ret_op


let wrap_map_null_named name op = 
  let eh           = op.palgop_expr_origin in 
  let fi           = op.palgop_expr_loc    in
  let map_index_op =
    logical_aalgop_mkop (AOENullMap name) (OneSub op) NoSub None eh fi
  in
  map_index_op
  
let wrap_map_null comp_ctxt op =
  match op.palgop_expr_name with
    | AOENullMap vn ->
	(* It is already mapped *)
	vn, op
    | _->
	let fresh_name = get_new_group_name comp_ctxt in 
	let ret_op     = wrap_map_null_named fresh_name op in
	  fresh_name, ret_op

(* return all the field accesses and keep track of their cardinality 
   return: (field_name, count)
   We maintain that the list is in sorted order. 

   *** This does not count in scope because of independent names ***
*)
let count_tuple_field_access algop =
  (* Want this local to this function *)
  let combine_return l r = 
    let merged = List.merge compare l r in 
    let combine_fold (list,(fn,count)) (fn',count') = 
      if fn = fn'
      then (list, (fn,count + count'))
      else ((fn,count) :: list), (fn',count')
    in
    match merged with 
    | [] -> []
    | first :: rest ->
	let (list,last) = List.fold_left combine_fold ([],first) rest in
	List.rev (last :: list) 
  in
  (* Count the op *)
  let rec count_op algop = 
    match algop.palgop_expr_name with
    | AOEAccessTuple fn -> [(fn,1)]
    | _ -> 
	combine_return
	  (count_sexpr algop.psub_expression)
	  (count_sexpr algop.pdep_sub_expression)
  and count_sexpr sexpr = 
    match sexpr with
    | NoSub    -> []
    | OneSub o -> count_op o
    | TwoSub (o1,o2) ->
	combine_return (count_op o1) (count_op o2)
    | ManySub ops ->
	Array.fold_left 
	  (fun ret op -> combine_return ret (count_op op))
	  [] ops
  in
  count_op algop

(* Check if input is an algebreic update operation *)
let is_update op = 
  match op.palgop_expr_name with
  | AOEInsert loc -> true
  | AOEReplace flg -> true
  | AOEDelete  -> true
  | _ -> false
(* 
   Check whether this query plan contains any update 
   operation or not. First it checks the main op and 
   then recursively check its depedent and independent 
   sub-expression 
*)
let rec check_sub_expression se = 
    match se with
      |	NoSub -> false 
      | OneSub s0 -> contain_updates s0 
      | TwoSub (s0,s1) -> (contain_updates s0) || (contain_updates s1)
      | ManySub a ->
	begin
	  let len = (Array.length a) - 1 in 
	    let result = ref false in 
	    for i = 0 to len  do
		result := !result || (contain_updates (Array.get a i)) 
	    done;
            (* print_string ("Avi: ManySub returning " ^ string_of_bool(!result)); flush stdout; *)
	    !result
	end
and contain_updates op =
  if (is_update op) then
     true
  else
     begin
       let i1 = op.psub_expression in       
       let d1 = op.pdep_sub_expression in 
	(check_sub_expression i1) || (check_sub_expression d1) 
     end

(* A wrapper for snap rewrites *)

(* This wrapper will not execute the rule unless the algop is free of
   internal non-trivial snaps *)
let generic_snap_free_wrapper rule has_changed comp_ctxt root child_desc algop = 
  if Optimization_judge.has_non_trivial_snap comp_ctxt algop
  then algop
  else generic_wrapper rule has_changed comp_ctxt root child_desc algop


