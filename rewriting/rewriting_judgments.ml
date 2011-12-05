(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: rewriting_judgments.ml,v 1.34 2007/10/16 01:25:35 mff Exp $ *)

(* Module: Rewriting_judgments
   Description:
     This module implements some auxiliary judgments used within
     rewriting rules.
*)

open Error

open Datatypes

open Namespace_names
open Namespace_builtin

open Xquery_common_ast
open Xquery_core_ast
open Xquery_core_ast_util


(**************************************************************************)
(*                    used_count function                                 *)
(* It determines the occurrance of variable $x in a given core expression *)
(**************************************************************************)

let zero = 0
let one  = 1
let many = 2

let rec used_count variablename ce =
  match ce.pcexpr_desc with
  | CEUnordered cexpr ->
      used_count variablename cexpr
  | CEOrdered cexpr ->
      used_count variablename cexpr
  | CEFLWOR (fl_clauses,where_clause,order_by_clause,return_clause) ->
	(* 1. process the fl clauses firsts *)
	begin
	  let first_clause,flwor_rest = get_first_fl_clause ce in
	  match first_clause with
	  | None ->
	      let where_count =
		match where_clause with
		| None -> zero
		| Some cexpr -> used_count variablename cexpr
	      in
	      let order_by_count =
		match order_by_clause with
		| None -> zero
		| Some (stablekind,order_spec_list, osig) ->
		    let apply_one_order_spec co (cexpr,_,_) =
		      co + (used_count variablename cexpr)
		    in
		    List.fold_left apply_one_order_spec zero order_spec_list
	      in
	      let return_count = used_count variablename return_clause in
	      where_count + order_by_count + return_count
	  | Some (CELET (odt,vname,cexpr1)) ->
	      if vname = variablename
	      then used_count variablename cexpr1
	      else (used_count variablename cexpr1) + (used_count variablename flwor_rest)
	  | Some (CEFOR (odt,vname,None,cexpr1)) ->
	      (used_count variablename cexpr1) +
		(if ((vname = variablename)
		   || ((used_count variablename flwor_rest) = zero))
		then zero
		else many)
	  | Some (CEFOR (odt,vname,Some vname',cexpr1)) ->
	      (used_count variablename cexpr1) +
		(if ((vname = variablename)
		   || (vname' = variablename)
		   || ((used_count variablename flwor_rest) = zero))
		then zero
		else
		  many)
	end
  | CESome (_, vname, cexpr1, cexpr2)
  | CEEvery (_, vname, cexpr1, cexpr2) ->
      (used_count variablename cexpr1) +
	(if ((vname = variablename)
	   || ((used_count variablename cexpr2) = zero))
	then zero
	else many)
  | CEIf (cexpr1, cexpr2, cexpr3) ->
      (used_count variablename cexpr1) 
        + (max
	     (used_count variablename cexpr2)
	     (used_count variablename cexpr3))
  | CEWhile (cexpr1, cexpr2) ->
      (used_count variablename cexpr1) 
        + (if ((used_count variablename cexpr2) = zero)
	then zero
	else many)
(* In XQuery:
   ----------------------------
   $x used in
   typeswitch Expr 
     case Type1 $v1 return Expr1
     case Type2 $v2 return Expr2
     ...
     default return Exprn
   We are matching against:
    CETypeswitch of (cexpr * (pattern * cexpr) list)
*)
    
  | CETypeswitch (cexpr, cases) ->
      (used_count variablename cexpr) + 
	(List.fold_left
	   (fun x y -> max x (used_count_in_case variablename y))
	   zero
	   cases)
  | CEVar vname -> 
      (* In XQuery: $x, $v, $fs:dot *)
      if vname = variablename 
      then one
      else zero
  | CEScalar _ ->
      zero
  | CEProtoValue _ ->
      zero
  | CEText _ ->
      zero
  | CECharRef _ ->
      zero
  | CETextComputed cexpr ->
      used_count variablename cexpr
  | CEPI _ -> 
      zero
  | CEPIComputed (cexpr1, cexpr2) -> 
      (used_count variablename cexpr1) + (used_count variablename cexpr2)
  | CEComment _ -> 
      zero
  | CECommentComputed cexpr 
  | CEDocument cexpr ->       	      (* In XQuery: document { Expr } *)
      used_count variablename cexpr
  | CECall (_, arguments, _, _,_)
  | CEOverloadedCall (_, arguments, _) ->
      List.fold_left (fun x y -> x + (used_count variablename y)) zero arguments
  | CESeq (cexpr1, cexpr2) 
  | CEImperativeSeq (cexpr1, cexpr2) ->
      (used_count variablename cexpr1) + (used_count variablename cexpr2)
  | CEEmpty ->
      zero
  | CEElem (relem_symbol, nsenv,cexprlist) ->
      List.fold_left
	(fun ct cexpr -> ct + (used_count variablename cexpr))
	zero
	cexprlist
  | CEAnyElem (cexpr1, nsenv1, nsenv2, cexpr2) ->  
      (used_count variablename cexpr1) + (used_count variablename cexpr2)
  | CEAttr (rattr_symbol, nsenv, cexprlist) ->
      List.fold_left
	(fun ct cexpr -> ct + (used_count variablename cexpr))
	zero
	cexprlist
  | CEAnyAttr (cexpr1, nsenv, cexpr2) ->
      (used_count variablename cexpr1) + (used_count variablename cexpr2)
  | CEError cexprlist ->
      List.fold_left (fun x y -> x + (used_count variablename y)) zero cexprlist
  | CETreat (cexpr, _) 
  | CEValidate (_,cexpr) 
  | CECast (cexpr, _, _) 
  | CECastable (cexpr, _, _) ->
      used_count variablename cexpr
  | CEForwardAxis (v, axis, cnode_test) 
  | CEReverseAxis (v, axis, cnode_test) ->
      if rqname_equal v variablename
      then one
      else zero
  | CEExecute (_, _, _, cexpr1,cexpr2) 
  | CELetServerImplement (_, _, cexpr1,cexpr2) ->
      (used_count variablename cexpr1) + (used_count variablename cexpr2)
  | CEForServerClose (_, _, cexpr1)
  | CEEvalClosure cexpr1 
  | CECopy cexpr1 
  | CEDelete cexpr1 ->
      (used_count variablename cexpr1)
  | CEInsert (cexpr1, cinsert_location) ->
      let count1 = used_count variablename cexpr1 in
      let count2 =
	match cinsert_location with
	| CUAsLastInto cexpr2
	| CUAsFirstInto cexpr2
	| CUInto cexpr2
	| CUAfter cexpr2
	| CUBefore cexpr2 ->
	    used_count variablename cexpr2
      in
      count1 + count2
  | CEReplace (_, cexpr1, cexpr2) 
  | CERename (_, cexpr1, cexpr2) ->
      (used_count variablename cexpr1) + (used_count variablename cexpr2)
  | CESnap (sm,cexpr) ->
      used_count variablename cexpr
  | CELetvar (_, vname, cexpr1, cexpr2) ->
      (used_count variablename cexpr1) +
	    (if ((vname = variablename)
	          || ((used_count variablename cexpr2) = zero))
	      then zero
	      else many)
  | CESet (vname, cexpr) -> 
	  (used_count variablename cexpr) +
		(if (vname = variablename)
		  then one
		  else zero)
        

(* Auxiliary function used in the used_count judgment for case clauses *)
	
and used_count_in_case variablename (_,ovn,case_cexpr) =
  match ovn with
  | None ->
      used_count variablename case_cexpr 
  | Some vname ->
      if (variablename = vname)
      then zero
      else used_count variablename case_cexpr


(******************************************************************)
(*                  is_safe_function judgement                    *)
(* Determines whether given built-in function can raise an error  *)
(******************************************************************)

let safe_functions_table =
  [ op_equal;
    op_nequal;
    op_is_same_node;
    op_node_before;
    op_node_after;
    op_le;
    op_ge;
    op_gt;
    op_lt;
    fn_empty;
    fn_count;
	(* Note:
             We consider the following as true, but this is not
	     right if the user is using it!!  It works for all uses of
	     index-of generated through normalization.
           - Gargi *)
    fn_index_of;
        (* Note:
             We remove data for now, since the semantics of data is
             still unclear and we do not know whether it might fail or
             not.
	   - Gargi and Jerome *)
    (*  (fn_uri, "data");  *)
  ]

let is_safe_function fname =
  (* Note: currently simple analysis only for built-in functions *)
  (List.exists (fun x -> x = fname) safe_functions_table)

(* Note:

   I had to change the previous pattern matching into an explicit
   lookup to deal with the namespace uri properly.

   The following functions where explicitly given as unsafe, but the
   default is unsafe.  Keep that information here for now.

     - Jerome

  (fn_uri, "int-divide")
  (fn_uri, "int-mod")
  (fn_uri, "decimal-divide")
  (fn_uri, "decimal-mod") ->
      false
*)

(*******************************************************************)
(*                    can_fail judgment                            *)
(* determines whether the given core expression can raise an error *)
(*******************************************************************)

(* MF: We do not distinguish between functions that can fail and
   "functions" that have side-effects -- "functions" with side-effects
   are lumped into the can-fail category. 

*)
let can_fail proc_ctxt ce = 
  let rec can_fail_aux ce1 =
    let typing_flag = proc_ctxt.Processing_context.typing in
    let datatype_check_can_fail odt = 
      match odt with
	None -> false
      | Some _ -> not(typing_flag)
              (* Type declaration cannot fail if the static checking is doing its job *)
    in
    match ce1.pcexpr_desc with
    | CEUnordered cexpr1 ->
	can_fail_aux cexpr1
    | CEOrdered cexpr1 ->
	can_fail_aux cexpr1
    | CEFLWOR (fl_clauses,where_clause,order_by_clause,return_clause) ->
	(* 1. process the fl clauses firsts *)
	begin
	  let first_clause,flwor_rest = get_first_fl_clause ce1 in
	  match first_clause with
	  | None ->
	      let where_can_fail =
		match where_clause with
		| None -> false
		| Some cexpr -> can_fail_aux cexpr
	      in
	      let order_by_can_fail =
		match order_by_clause with
		| None -> false
		| Some (stablekind,order_spec_list, osig) ->
		    let apply_one_order_spec co (cexpr,_,_) =
		      co || (can_fail_aux cexpr)
		    in
		    List.fold_left apply_one_order_spec false order_spec_list
	      in
	      let return_can_fail = can_fail_aux return_clause in
	      where_can_fail || order_by_can_fail || return_can_fail

	    | Some (CELET (odt,vname,cexpr1))
	    | Some (CEFOR (odt,vname,_,cexpr1)) ->
		can_fail_aux cexpr1 || can_fail_aux flwor_rest || (datatype_check_can_fail odt)
	end

    | CESome (odt, vname, cexpr1, cexpr2)
    | CEEvery (odt, vname, cexpr1, cexpr2) ->
	can_fail_aux cexpr1 || can_fail_aux cexpr2 || (datatype_check_can_fail odt)
    | CEIf (cexpr1, cexpr2, cexpr3) ->
	(can_fail_aux cexpr1 || can_fail_aux cexpr2 || can_fail_aux cexpr3)
    | CEWhile (cexpr1, cexpr2) ->
	(can_fail_aux cexpr1 || can_fail_aux cexpr2)
    | CETypeswitch (cexpr1, cases) ->
	 can_fail_aux cexpr1 || can_fail_cases cases
    | CEVar _ -> false (* It cannot fail if the static checking is doing its job *)
    | CEScalar _ -> false
    | CEProtoValue _ -> false
    | CEText _ -> false
    | CECharRef _ -> false
    | CETextComputed cexpr -> can_fail_aux cexpr
    | CEPI _ -> false
    | CEPIComputed (cexpr1, cexpr2) -> can_fail_aux cexpr1 || can_fail_aux cexpr2
    | CEComment _ -> false
    | CECommentComputed cexpr -> can_fail_aux cexpr
    | CEDocument cexpr -> can_fail_aux cexpr
    | CECall (fname, arguments, _, _,_) ->
      (* LOOK for differences between BUILTIN and USER-DEFINED ONES *)
	begin 
	  if (is_safe_function fname) then not(typing_flag)
	  else true
	end || 
	(List.fold_left 
	   (fun b cexpr -> b || can_fail_aux cexpr) 
	   false arguments)
     | CEOverloadedCall (fname, arguments, sigs) ->
	 (if (is_safe_function fname) then not(typing_flag)
	 else true)
       || 
	 (List.fold_left 
	    (fun b cexpr -> b || can_fail_aux cexpr) 
	    false arguments)
    | CESeq (cexpr1, cexpr2) ->
	(can_fail_aux cexpr1 || can_fail_aux cexpr2)
    | CEImperativeSeq (cexpr1, cexpr2) ->
	(can_fail_aux cexpr1 || can_fail_aux cexpr2)
    | CEEmpty -> false
    | CEElem (relem_symbol, nsenv,cexprlist) ->   (* element a { cexprlist } *)
	List.fold_left
	  (fun b cexpr -> b || can_fail_aux cexpr) false cexprlist
      (* According to the new semantics, element constructors never fail *)
    | CEAnyElem (cexpr1, nsenv1, nsenv2, cexpr2) ->
     (* It cannot fail if the static checking is doing its job *)
	typing_flag || (can_fail_aux cexpr1 || can_fail_aux cexpr2)
	(* It can fail dynamically if the expression computing
	   the name returns something else than a QName of a string.
	   For instance : element { 1 } { "Hello" } *)

    | CEAttr (rattr_symbol, nsenv, cexprlist) -> 
	List.fold_left (fun b cexpr -> b || can_fail_aux cexpr) false cexprlist
      (* According to the new semantics, attribute constructors never fail *)

    | CEAnyAttr (cexpr1, nsenv, cexpr2) ->
    (* It cannot fail if the static checking is doing its job *)
	typing_flag || (can_fail_aux cexpr1 || can_fail_aux cexpr2)
	  (* It can fail dynamically if the expression computing
	     the name returns something else than a QName of a string.
	     For instance : attribute { 1 } { "Hello" } *)
    | CEError _ -> true      
    | CETreat (cexpr, _) -> true
    | CEValidate cexpr -> true
    | CECast (cexpr, _, _) -> true
    | CECastable (cexpr, _, _) -> true
    | CEForwardAxis (_, _, _) -> false
    | CEReverseAxis (_, _, _) -> false
    | CEForServerClose (_, _, cexpr1) -> can_fail_aux cexpr1
    | CELetServerImplement (_, _, cexpr1, cexpr2) -> can_fail_aux cexpr1 || can_fail_aux cexpr2
    | CEEvalClosure _
    | CEExecute _ -> true
    | CECopy cexpr1 -> false
    | CEDelete cexpr1 -> true
    | CEInsert (cexpr1, cinsert_location) -> true
    | CEReplace (vof, cexpr1, cexpr2) -> true
    | CERename (_, cexpr1, cexpr2) -> true
    | CESnap (sm,cexpr) -> can_fail_aux cexpr
    | CELetvar (odt, _, cexpr1, cexpr2) ->
	can_fail_aux cexpr1 || can_fail_aux cexpr2 || datatype_check_can_fail odt
    | CESet (_, cexpr) -> can_fail_aux cexpr

  and can_fail_cases cases = 
    match cases with
    | [] -> false
    | (_, _, case_cexpr) :: remaining_cases ->
	if (can_fail_aux case_cexpr)
	then true
	else (can_fail_cases remaining_cases)
  in
  can_fail_aux ce

(******************************************************************)
(*                      is_ordered judgement                      *)
(* Determines whether the given core expression returns results   *)
(* in document order or not                                       *)
(******************************************************************)

let rec does_node_care_about_order ce =
  match ce.pcexpr_desc with
  | CEUnordered _ ->
      true
  | CEOrdered _ ->
      true
  | CESome _ ->
      false
  | CEEvery _ ->
      false
  | CEFLWOR _ ->
      false
  | CEIf _ ->
      false
  | CEWhile _ ->
      false
  | CETypeswitch (cexpr, cases) ->
      false
  | CEVar vname -> 
      false
  | CEScalar _ ->
      false
  | CEProtoValue _ ->
      false
  | CEText _ ->
      false
  | CECharRef _ ->
      false
  | CETextComputed _ -> 
      false
  | CEPI _ -> 
      false
  | CEPIComputed _ -> 
      false
  | CEComment _ -> 
      false
  | CECommentComputed _ -> 
      false
  | CEDocument _ ->
      false
  | CECall (fname, _, _, _, _) ->
      if ((fname = fs_docorder)
	||(fname = fs_distinct_docorder))
      then
	true
      else
	false
  | CEOverloadedCall (fname, _, _) ->
	false
  | CESeq _ ->
      false
  | CEImperativeSeq _ ->
      false
  | CEEmpty -> 
      false
  | CEElem _ ->
      false
  | CEAnyElem _ ->  
      false
  | CEAttr _ ->
      false
  | CEAnyAttr _ ->
      false
  | CEError _ ->
      false
  | CETreat _ ->
      false
  | CEValidate _ ->
      false
  | CECast _ ->
      false
  | CECastable _ ->
      false
  | CEForwardAxis _ ->
      false
  | CEReverseAxis _ ->
      false
  | CELetServerImplement _
  | CEForServerClose _
  | CEEvalClosure _
  | CEExecute _ ->
      false
  | CECopy _ ->
      false
  | CEDelete cexpr1 ->
      false
  | CEInsert (cexpr1, cinsert_location) ->
      false
  | CERename (_, cexpr1, cexpr2) ->
      false
  | CEReplace (vof, cexpr1, cexpr2) ->
      false
  | CESnap (sm,cexpr) ->
      false
  | CELetvar _ ->
      false
  | CESet _ ->
      false


(************)
(* Inlining *)
(************)

(* Nicola: this is needed for the path analysis, in order to detect an
   AOEParse operator for an fn:doc call *)
let should_inline_variable proc_ctxt ret_expr =
  if proc_ctxt.Processing_context.inline_variables then true
  else if proc_ctxt.Processing_context.infer_independence then
    match ret_expr.pcexpr_desc with
      | CECall (cfname, _, _, _, _) when (Namespace_names.rqname_equal cfname fn_doc) -> true
      | _ -> false
  else false


(*************)
(* Recursion *)
(*************)

(*
let is_recursive_function norm_ctxt check_name =
  let visited_functions = Namespace_util.RQNameIntHashtbl.create 1 in 
  let rec is_recursive_function_aux norm_ctxt check_name ce =
    match ce.pcexpr_desc with
    (* Expressions with no sub-expressions *)
    | CEForwardAxis _
    | CEReverseAxis _
    | CEVar _
    | CEScalar _
    | CEProtoValue _
    | CEText _
    | CEPI _
    | CEComment _
    | CEEmpty
    | CECharRef _ -> false
    (* Expressions with one sub-expressions *)
    | CESnap (_,cexpr)
    | CESet (_, cexpr)
    | CEExecute (_,_,_,cexpr)
    | CECopy cexpr
    | CEDelete cexpr
    | CETreat (cexpr, _)
    | CEValidate (_,cexpr)
    | CECast (cexpr, _, _)
    | CECastable (cexpr, _, _)
    | CETextComputed cexpr
    | CECommentComputed cexpr
    | CEDocument cexpr
    | CEUnordered cexpr
    | CEOrdered cexpr -> is_recursive_function_aux norm_ctxt check_name cexpr
    (* Expressions with two sub-expressions *)
    | CELetvar (_, _, cexpr1, cexpr2)
    | CEAnyElem (cexpr1, _,cexpr2)
    | CEAnyAttr (cexpr1, _, cexpr2)
    | CESeq (cexpr1, cexpr2)
    | CEImperativeSeq (cexpr1, cexpr2)
    | CEPIComputed (cexpr1, cexpr2)
    | CESome (_, _, cexpr1, cexpr2)
    | CEReplace (_, cexpr1, cexpr2)
    | CERename (_, cexpr1, cexpr2)
    | CEEvery (_, _, cexpr1, cexpr2) ->
	begin
	  (is_recursive_function_aux norm_ctxt check_name cexpr1)
	|| (is_recursive_function_aux norm_ctxt check_name cexpr2)
	end
    | CEInsert (cexpr1, cinsert_location) ->
	let count1 = is_recursive_function_aux norm_ctxt check_name cexpr1 in
	let count2 =
	  match cinsert_location with
	  | CUAsLastInto cexpr2
	  | CUAsFirstInto cexpr2
	  | CUInto cexpr2
	  | CUAfter cexpr2
	  | CUBefore cexpr2 ->
	      is_recursive_function_aux norm_ctxt check_name cexpr2
	in
	count1 || count2
    (* Expressions with three sub-expressions *)
    | CEIf (cexpr1, cexpr2, cexpr3) ->
	begin
	  (is_recursive_function_aux norm_ctxt check_name cexpr1)
	|| (is_recursive_function_aux norm_ctxt check_name cexpr2)
	|| (is_recursive_function_aux norm_ctxt check_name cexpr3)
	end
    (* Expressions with many sub-expressions *)
    | CEFLWOR (fl_clauses,where_clause,order_by_clause,return_clause) ->
	(* 1. process the fl clauses firsts *)
	begin
	  let first_clause,flwor_rest = get_first_fl_clause ce in
	  match first_clause with
	  | None ->
	      let where_count =
		match where_clause with
		| None -> false
		| Some cexpr -> is_recursive_function_aux norm_ctxt check_name cexpr
	      in
	      let order_by_count =
		match order_by_clause with
		| None -> false
		| Some (stablekind,order_spec_list, osig) ->
		    let apply_one_order_spec co (cexpr,_,_) =
		      co || (is_recursive_function_aux norm_ctxt check_name cexpr)
		    in
		    List.fold_left apply_one_order_spec false order_spec_list
	      in
	      let return_count = is_recursive_function_aux norm_ctxt check_name return_clause in
	      where_count || order_by_count || return_count
	    | Some (CELET (odt,vname,cexpr1)) ->
		begin
		  (is_recursive_function_aux norm_ctxt check_name cexpr1)
		|| (is_recursive_function_aux norm_ctxt check_name flwor_rest)
		end
	    | Some (CEFOR (odt,vname,None,cexpr1)) ->
		begin
		  (is_recursive_function_aux norm_ctxt check_name cexpr1)
		|| (is_recursive_function_aux norm_ctxt check_name flwor_rest)
		end
	    | Some (CEFOR (odt,vname,Some vname',cexpr1)) ->
		begin
		  (is_recursive_function_aux norm_ctxt check_name cexpr1)
		|| (is_recursive_function_aux norm_ctxt check_name flwor_rest)
		end
	end
    | CETypeswitch (cexpr, cases) ->
	(is_recursive_function_aux norm_ctxt check_name cexpr) ||
	(List.fold_left
	   (fun x y -> x || (is_recursive_function_aux_in_case check_name y))
	   false
	   cases)
    | CEError cexprlist
    | CEAttr (_, cexprlist)
    | CEElem (_, _,cexprlist) ->
	List.fold_left
	  (fun ct cexpr -> ct || (is_recursive_function_aux norm_ctxt check_name cexpr))
	  false
	  cexprlist

    (* Function calls *)
    | CECall (cfname, arguments, _, _, _)
    | CEOverloadedCall (cfname, arguments, _) ->
	let arity = List.length arguments in
	((Namespace_names.rqname_int_equal check_name (cfname,arity)) ||
	(check_function_body norm_ctxt (cfname,arity)) ||
	(List.fold_left
	   (fun ct cexpr -> ct || (is_recursive_function_aux norm_ctxt check_name cexpr))
	   false
	   arguments))

  and is_recursive_function_aux_in_case check_name (_,_,case_cexpr) =
    is_recursive_function_aux norm_ctxt check_name case_cexpr

  and check_function_body norm_ctxt fname = 
    if Namespace_util.RQNameIntHashtbl.mem visited_functions fname
    then false
    else
      begin
	Namespace_util.RQNameIntHashtbl.add visited_functions fname ();
	try
	  let func_defn = Norm_context.get_body_from_norm_context norm_ctxt fname in 
	  is_recursive_function_aux norm_ctxt check_name func_defn
	    (* We should only end up here for user-defined external functions *)
	with Not_found -> false
      end
  in
  check_function_body norm_ctxt check_name

*)

(****************)
(* Side effects *)
(****************)

(* let register_side_effect_free_function cfname = *)
(*   base_set := Function_Name_Set.add cfname !base_set *)

(* let global_is_side_effect_free_function cfname = *)
(*   Function_Name_Set.mem cfname !base_set *)

let has_side_effect_judge (ce: acexpr) =
  let has_side_effect_aux ce =
    match ce.pcexpr_desc with
    | CECall (fname, _, _, upd, _) ->
	begin
	  match upd with
	  | Updating -> true
	  | NonUpdating -> false
	end
    | CEDelete _	  
    | CEInsert _
    | CERename _
    | CEReplace _
    | CESet _
    | CEExecute _
    | CEEvalClosure _
    | CEError _ ->
	true
    | _ -> false
  in Ast_walker_fold.fold_over_cexpr has_side_effect_aux (fun x y -> x||y) false ce

let side_effect_free_judge ce = 
  not (has_side_effect_judge ce)

