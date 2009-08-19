(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_rules_cleanup.ml,v 1.5 2007/07/05 08:35:54 simeon Exp $ *)

(* Module: Optimization_rules_cleanup
   Description:
     This module contains optimization cleanup rules.
*)

open Error
open Namespace_builtin

open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_algebra_ast_annotation_util

open Processing_context
open Compile_context

open Optimization_util
open Optimization_walker
open Optimization_judge


(***************************)
(* Unnecessary Map Rewrite *)
(***************************)
(* Map{D_1}([]) 
 *  -----
 *  D_1 
 *)

let map_removal comp_ctxt algop = 
  if (is_a_map algop) then
    begin
      let i1 = access_onesub algop.psub_expression in       
	if (is_empty_tuple i1) then
	  begin
	    let d1 = access_onesub algop.pdep_sub_expression in 
	      (d1, true)
	  end
	else
	  (algop, false)
    end
  else
    (algop, false)


(* ** Inverse map rewrite 
   *
   * MapToItem{#x}
   *   (MapFromItem{[x:ID]}(Op))
   * ==
   * Op
   *
   ** *)

let inverse_map_rewrite ctxt root op =
  let fail = op, false in
  match op.palgop_expr_name with
  | AOEMapToItem ->
      begin
	let map_to_indep = access_onesub op.psub_expression in
	let map_to_dep   = access_onesub op.pdep_sub_expression in
	match map_to_dep.palgop_expr_name, map_to_indep.palgop_expr_name with
	| AOEAccessTuple a1, AOEMapFromItem vn1 ->
	    begin
	      let map_from_indep = access_onesub map_to_indep.psub_expression in
	      let map_from_dep   = access_onesub map_to_indep.pdep_sub_expression in
	      match map_from_dep.palgop_expr_name  with
	      | (AOECreateTuple a2) ->
		  begin
		    match a2 with
		    | [|(None,a2)|] ->
			if (Namespace_names.rqname_equal a1 a2)
			then
			  let input = access_manysub map_from_dep.psub_expression in
			  let create_indep = input.(0) in
			  begin
			    match create_indep.palgop_expr_name with
			    | AOEVar vn2 ->
				if (Namespace_names.rqname_equal vn1 vn2)
				then map_from_indep, true
				else
				  fail
			    | _ -> fail
			  end
			else fail
		    | _ -> fail
		  end
	      | _ -> fail
	    end
	| _ -> fail
      end
  | _ -> fail

(*****************)
(* Cleanup rules *)
(*****************)

let cleanup_rules =
  [ (generic_wrapper map_removal), "Map Removal";
    (removal_wrapper inverse_map_rewrite), "Inverse map rewrite" ]

