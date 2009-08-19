(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_predicates.ml,v 1.7 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Optimization_predicates
   Description:
     This module is used to process predicates during optimization.
*)

open Error
open Xquery_algebra_ast
open Xquery_algebra_ast_util

(* This module handles predicates
   All predicate ops have many dependent sub 
   expressions - i.e. the predicate expressions themselves


   operations needed:
   construct_singleton_*
   construct_many
   conjunctive_merge 
   is_simple_conjunct
*)

let construct_singleton_select cond to_apply eh fi =
  logical_aalgop_mkop (AOESelect (SimpleConjunct (0,0))) (OneSub to_apply) (ManySub [|cond|]) None eh fi

let is_simple_conjunct op =
  match op.palgop_expr_name with
    | AOESelect pd 
    | AOEJoin pd 
    | AOELeftOuterJoin (_,pd) 
	->
	begin
	  match pd with
	    | SimpleConjunct _ -> true
	    | _ -> false
	end
    | _ -> 
	raise (Query (Malformed_Algebra_Expr ("Attempting to determine if a predicate is simple, but is not a predicate")))

let extract_pred_desc op = 
  match op.palgop_expr_name with
    | AOESelect pd        
    | AOEJoin pd          
    | AOELeftOuterJoin (_,pd) -> pd 
    | _ ->
	raise (Query (Malformed_Algebra_Expr ("Extracting predicate description from non-predicate op")))

let rec renumber_conjunct offset pd =
  match pd with
    | SimpleConjunct(st,ed) ->
	SimpleConjunct( (st + offset), (ed+offset))
    | ComplexConjunct(l,r) ->
	ComplexConjunct( 
	  (renumber_conjunct offset l),
	  (renumber_conjunct offset r))
    | Disjunct (l,r) ->
	Disjunct (
	  (renumber_conjunct offset l),
	  (renumber_conjunct offset r))
			    
(* This merges the description, it assumes that pd1's args
   precede pd2s exactly 
   This should not be exported: instead export the function
   that does the entire merge (ops too)
*)
let conjunctive_merge_predicate_desc pd1 n_args_pd1 pd2 = 
  (* the order is pd1's first, pd2's second *)
  match (pd1,pd2) with
    | (SimpleConjunct (c1_start, c1_end)), (SimpleConjunct (c2_start,c2_end)) ->
	begin
	  if (c1_start <> 0) || 
	    (c2_start <> 0) then 
	      raise (Query (Malformed_Algebra_Expr ("Simple conjunct does not start at 1")))
	  ;	  
	  if (n_args_pd1 <> (c1_end+1)) then
	    raise (Query (Malformed_Algebra_Expr ("The number of args passed, and the end of the " ^
					  "simple conjunct do not agree")))
	  ;
	  SimpleConjunct (c1_start, (c1_end + c2_end + 1))
	end
    | _ -> (* we need to renumber all Simple conjuncts *)
	let rhs = renumber_conjunct n_args_pd1 pd2 in
	  ComplexConjunct( pd1, rhs )

(* merge pred1 -> pred2
   notice this really only makes sense
   if pred1 is a select operator 
   
   Return pred_2's indep
*)
let conjunctive_merge_select pred1 pred2 =
  let indep = pred2.psub_expression in
  let eh    = pred2.palgop_expr_origin in 
  let fi    = pred2.palgop_expr_loc    in
  let new_dep = ManySub (Array.append
			   (access_manysub pred1.pdep_sub_expression)  
			   (access_manysub pred2.pdep_sub_expression)) in
  let nargs_pred = Array.length (access_manysub pred1.pdep_sub_expression)  in
  let merged_pd  = conjunctive_merge_predicate_desc 
		    (extract_pred_desc pred1)
		     nargs_pred
		    (extract_pred_desc pred2) in
  let op_name = 
    match pred2.palgop_expr_name with
      | AOESelect pd ->
	  AOESelect merged_pd      
      | AOEJoin pd -> AOEJoin merged_pd
      | AOELeftOuterJoin (vn,pred_desc) -> AOELeftOuterJoin (vn, merged_pd)
      | _ -> raise (Query (Malformed_Algebra_Expr ("Merging non-select, join loj")))
  in
    logical_aalgop_mkop op_name indep new_dep None eh fi

(* This is a helper function for pds *)
(* Assume the op after is moved down one-index is removed *)
let rec remove_pred_desc_conjunct index pd =
  match pd with
    | SimpleConjunct(s,e) ->
	if (e < index) then
	  (* We do nothing, it comes before the deletion *)
	  (Some pd)
	else if (s <= index) &&
	  (e >= index) then (* it is in this simple conjunct *)
	    begin
	      (* Special case It is the only op *) 
	      if (s = e) then
		None
	      else
		Some (SimpleConjunct (s, (e-1)))
	    end
	else if (e > index) then (* we must translate it down one *)
	  Some (SimpleConjunct ((s-1), (e-1)))
	else 
	  raise (Query (Malformed_Algebra_Expr ("There is no way to get here...")))
    | ComplexConjunct (pd1,pd2) ->
	begin
	  let pd1 = remove_pred_desc_conjunct index pd1 in
	  let pd2 = remove_pred_desc_conjunct index pd2 in
	    match (pd1,pd2) with
	      | (None, None) -> None
	      | ((Some pd), None) | (None, (Some pd)) ->
		  (Some pd)
	      | (Some pd1, Some pd2) ->
		  Some (ComplexConjunct (pd1,pd2))
	end
    | Disjunct (pd1,pd2) -> 
	(* Does this make sense to remove? *)
	(* Because we are implicitly setting the removed
	   predicate to true -> should mean this is really none *)
	begin
	  let pd1 = remove_pred_desc_conjunct index pd1 in
	  let pd2 = remove_pred_desc_conjunct index pd2 in
	    match (pd1,pd2) with
	      | ((Some _), None) | (None, (Some _))
	      | (None, None) -> None
(*	      | ((Some pd), None) | (None, (Some pd)) ->
		  (Some pd) *)
	      | (Some pd1, Some pd2) ->
		  Some (Disjunct (pd1,pd2))
	end

let remove_sub_index a index =
  let len = Array.length a in
    if len = 1 then
      [||]
    else if index = 0 then
      Array.sub a 1 (len - 1)
    else if index = (len - 1) then
      Array.sub a 0 (len - 1)
    else
      Array.append 
	(Array.sub a 0 index)
	(Array.sub a (index+1) (len - index - 1))
	
    
	  
(* Remove the conjunct at index from the operand *)	      
(* Currently hardcoded to be select *)
let remove_conjunct op index =
  let _ =
    match op.palgop_expr_name with
	AOESelect _ -> ()
      | _ -> raise (Query (Malformed_Algebra_Expr ("Remove conjunct only supports AOESelect currently")))
  in

  let eh      = op.palgop_expr_origin in 
  let fi      = op.palgop_expr_loc    in
  let pd      = extract_pred_desc op in
  let pd'     = remove_pred_desc_conjunct index pd in
  let new_dep = remove_sub_index (access_manysub op.pdep_sub_expression) index in
    match pd' with
	None ->
	  None
      | Some v ->
	  Some (logical_aalgop_mkop (AOESelect v) op.psub_expression (ManySub new_dep) None eh fi)

