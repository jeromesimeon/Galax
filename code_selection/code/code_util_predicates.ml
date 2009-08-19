(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util_predicates.ml,v 1.8 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_util_predicates
   Description:
     This module implements some code utilities over predicates.
*)

open Error
open Datatypes
open Xquery_algebra_ast
open Optimization_predicates
open Xquery_algebra_ast_util (* Need for the is_* functions *)


(**********************)
(* Predicate branches *)
(**********************)

type op_expr_type =
  | RegularOp of Algebra_type.algop_expr
  | Fs_untyped_to_any
  | Invalid_Predicate_Branch

type predicate_branch =
    { some_in_expr : Algebra_type.algop_expr option;
      eq_expr      : op_expr_type;
      some_assign  : (Physical_value.xml_value -> unit) option}

let mk_predicate_branch in_expr_opt eq_expr assign_opt =
    { some_in_expr = in_expr_opt;
      eq_expr      = eq_expr;
      some_assign  = assign_opt }

let invalid_predicate_branch = mk_predicate_branch None Invalid_Predicate_Branch None

let is_fs_untyped_to_any_predicate_branch pb =
  pb.eq_expr = Fs_untyped_to_any


(***************************)
(* Runtime Evaluation Code *)
(***************************)

	 
let eval_predicate eval alg_ctxt pred_expr =
  (* Mary ERROR HANDLING : All the Physical value accessors require some Finfo.finfo
     value to report dynamic-typing errors.  It is troubling that I
     have to provide a generic algebra operator name here -- indicates
     something isn't right. *)
  Physical_util.get_boolean 
    (Physical_value_util.item_cursor_of_physical_value 
       (eval alg_ctxt pred_expr))
    

let eval_predicate_desc eval_fun alg_ctxt conds desc =
  let rec eval_predicate_desc_helper pd = 
    match pd with
    | SimpleConjunct (start_index, end_index) ->
	(********************************************************)
	(* Evaluate each element of the simple conjunct in turn *)
	(********************************************************)
	let rec eval_conj cur_index end_index =
	  if cur_index <= end_index then
	    ((eval_predicate eval_fun alg_ctxt conds.(cur_index))
	       &&  eval_conj (cur_index + 1) end_index)
	  else
	    true
	in
	eval_conj start_index end_index
    | ComplexConjunct (l,r) ->
	(eval_predicate_desc_helper l)
	  && (eval_predicate_desc_helper r)
    | Disjunct (l,r) ->
	(eval_predicate_desc_helper l) 
      || (eval_predicate_desc_helper r)
  in
  eval_predicate_desc_helper desc


(* Evaluate a predicate description given a context of evaluation
   functions which take unit and return lists of indexes.

   Two assumptions: These indexes are over the same materialized array 
   and they are in sorted ascending order on return. 

*)
let eval_predicate_desc_to_rid_list eval_cond cond_info desc =
  let rec eval_predicate_desc_helper pd =
    match pd with
    | SimpleConjunct (start_index, end_index) ->
	(********************************************************)
	(* Evaluate each element of the simple conjunct in turn *)
	(********************************************************)
	let rec eval_conj cur_rid_list cur_index end_index =
	  if cur_index <= end_index then
	    begin
	      let new_rid_list = eval_cond cond_info.(cur_index) in
	      let rid_list = Code_util_ridlist.intersect_rid_list cur_rid_list new_rid_list in
	      if (Code_util_ridlist.is_empty_rid_list rid_list) then
		Code_util_ridlist.Empty_RidList
	      else eval_conj rid_list (cur_index + 1) end_index
	    end
	  else cur_rid_list
	in
	eval_conj Code_util_ridlist.Full_RidList start_index end_index
    | ComplexConjunct (l,r) ->
	let l_rids = eval_predicate_desc_helper l in
	if (Code_util_ridlist.is_empty_rid_list l_rids) then
	  Code_util_ridlist.Empty_RidList
	else
	  Code_util_ridlist.intersect_rid_list l_rids (eval_predicate_desc_helper r)
    | Disjunct (l,r) ->
	let l_rids = eval_predicate_desc_helper l in
	if (Code_util_ridlist.is_empty_rid_list l_rids) then
	  Code_util_ridlist.Empty_RidList
	else
	  Code_util_ridlist.union_rid_list l_rids (eval_predicate_desc_helper r)
  in
  eval_predicate_desc_helper desc


(* Code to evaluate a "half" or a branch of a predicate *)

(* It should be noted that this relies on the fact that the current
   tuple is set.  That is it evaluates in the current context (which
   is set elsewhere). Whatever values are in the current tuple will be
   available (so long as they are the scoped correctly) inside these
   expressions *)

(* This returns the item seqeunce resulting from evaluation.  I
   suppose it could be a cursor... *)

let evaluate_predicate_branch pb eval alg_ctxt nsenv =
  match pb.some_in_expr with
  | Some some_in_expr ->
      begin
	let v_seq = 	  
	  Physical_value_util.item_list_of_physical_value
	    (eval alg_ctxt some_in_expr) in

	(* Now for each one in this value sequence, 
	   we must evaluate the op_code -> if present *)
	(* NOTE: This must only depend on the variable field (or the current tuple) *)
	      (* NOTE: We should be type checking here *)
	match pb.eq_expr, pb.some_assign with
	| Invalid_Predicate_Branch, _ -> 
	    raise (Query (Code_Selection ("Attempt to evaluate an invalid predicate branch")))
	      
	| Fs_untyped_to_any, _ -> (* fs:untyped-to-any semantic *)
	    List.concat 
	      (List.map 
		 (fun value ->
		   let av       = Physical_item.getAtomicValue value in
		   let (v1, v2) = Cs_util.handle_fs_untyped_to_any_semantic nsenv av in
		   v1 :: (match v2 with None -> [] | Some v -> v :: [])) v_seq)
	      
	| RegularOp eq, Some assign ->
	    (* THIS IS NOT CORRECT *)
	    (* NOTE: We need to consider all types that it could to which an
	       item could be promoted... *)
	    (* To each value in the return sequence, map
	       it to the variable and return the value of the expression *)
	    List.concat (* This *should* be artificial *)
	      (List.map 
		 (fun v ->
		   (* This is problematic, the some variable is not in scope yet
		      Since we are compiling in the join's context *)
		    (* Michael *)
		   assign (Physical_value.DomValue (Physical_sequence.sequence_of_singleton v));
		   let item_list = 
		     Physical_value_util.item_list_of_physical_value 
		       (eval alg_ctxt eq) in
		   List.map (fun x -> Physical_item.getAtomicValue x) item_list
		 ) v_seq)
	| RegularOp _, None ->
	    raise (Query (Code_Selection ("Assignment function and op predicate requried to be in predicate branch " ^
					  " when nested under a some")))
      end
  | None ->
      begin
	match pb.eq_expr with
	| RegularOp eq ->
	    List.map (fun x -> Physical_item.getAtomicValue x)
	      (Physical_value_util.item_list_of_physical_value 
		 (eval alg_ctxt eq))
	| Fs_untyped_to_any ->
	    (* This is akward - no type information but we have an only one condition that needs to be evaluated.. *)
	    raise (Query (Code_Selection ("Evaluate Predicate branch: Odd partial evaluation but, possible cs_code_predicate.ml - fix")))
	| Invalid_Predicate_Branch ->	      
	    raise (Query (Code_Selection ("Evaluate Predicate branch: No expression in predicate")))
      end	    

(* Revised join_type *)
type supported_predicates = 
  | Pred_Invalid
  | Pred_Equality
  | Pred_Greater_Than
  | Pred_Greater_Than_or_Equal
  | Pred_Less_Than
  | Pred_Less_Than_or_Equal
  | Pred_Arbitrary_Function of Algebra_type.algop_expr

(**************************)
(* Types of scans supported
   in the implementation.
   Note: Equality is a scan 
   because of duplicates *)
(**************************)

type supported_scans =
  | Value_Equality
  | Greater_Than
  | Greater_Than_or_Equal
  | Less_Than
  | Less_Than_or_Equal
  | Invalid_Scan (* Like none *)

let supported_predicate_supported_scans sp =
  match sp with
  | Pred_Invalid | Pred_Arbitrary_Function _ ->
      raise (Query (Code_Selection ("Attempting to build supported scan from invalid predicate")))
  | Pred_Equality -> Value_Equality
  | Pred_Greater_Than -> Greater_Than
  | Pred_Greater_Than_or_Equal -> Greater_Than_or_Equal
  | Pred_Less_Than -> Less_Than
  | Pred_Less_Than_or_Equal -> Less_Than_or_Equal


(* String conversion for debugging *)
let string_of_supported_scans op =
  match op with
    | Value_Equality -> "Value_Equality"
    | Greater_Than -> "Greater_Than"
    | Greater_Than_or_Equal -> "Greater_Than_or_Equal"
    | Less_Than -> "Less_Than"
    | Less_Than_or_Equal -> "Less_Than_or_Equal"
    | Invalid_Scan -> "Invalid Scan"

let switch_predicate pred = 
  match pred with
  | Pred_Invalid               -> Pred_Invalid
  | Pred_Equality              -> Pred_Equality
  | Pred_Greater_Than          -> Pred_Less_Than 
  | Pred_Greater_Than_or_Equal -> Pred_Less_Than_or_Equal
  | Pred_Less_Than             -> Pred_Greater_Than
  | Pred_Less_Than_or_Equal    -> Pred_Greater_Than_or_Equal
  (* Note: Arbitrary functions use bindings *)
  | Pred_Arbitrary_Function x  -> Pred_Arbitrary_Function x

let predicate_type op =
  match op with
  | x when (is_equal op) -> Pred_Equality
  | x when (is_gt op)    -> Pred_Greater_Than
  | x when (is_ge op)    -> Pred_Greater_Than_or_Equal
  | x when (is_lt op)    -> Pred_Less_Than
  | x when (is_le op)    -> Pred_Less_Than_or_Equal
  | _ -> (* Arbitrary *) Pred_Arbitrary_Function op

let needed_types op predicate_type =
  match predicate_type with
  | Pred_Arbitrary_Function _ ->  ATUntypedAtomic :: []
  | _ ->
      begin
	match get_function_name op with
	| Some fn_name ->
	    begin
	      (*******************************************************************)
	      (* What should happen is that we infer the type from the
		 op.  Most likely this means looking it up in
		 Code_fn.mli but, I don't have this implemented
		 without static typing.

		 This code should be annotated with type information
		 -> We should keep this annotation around for items
		 that are 1-1 with the core. *)
	      (*******************************************************************)
              (* let t = builtin_function_to_type fn_name in *)
	      let t = ATUntypedAtomic in
	      (* These are the types it could be promoted to *)
	      let first_cut_types = Datatypes_util.can_be_promoted_to t in
	      first_cut_types @ 
	      (match t with
		(* For any type but ATUntypedAtomic, we just return the type.
		   It has a type -> it operates *ONLY* over those types and
		   no promotion is needed *)
	      | ATString | ATBoolean | ATDecimal
	      | ATFloat  | ATDouble  | ATDuration
	      | ATDateTime | ATTime  | ATDate
	      | ATGYearMonth | ATGYear | ATGMonthDay
	      | ATGDay   | ATGMonth  | ATHexBinary
	      | ATBase64Binary | ATAnyURI | ATQName
	      | ATNOTATION | ATInteger
	      | ATYearMonthDuration | ATDayTimeDuration | ATAnyAtomic -> []
	      | ATUntypedAtomic ->
		  (* These are the possible types it could be
		     promoted to/be *)
		  ATDouble :: ATString :: [])
	    end
	| None -> (* This should not happen *)
	    raise (Query (Code_Selection ("Recongized operator, but not a function op.")))
      end

let predicate_invalid = Pred_Invalid

type predicate_functions =
    (predicate_branch * predicate_branch * supported_scans) array
    
