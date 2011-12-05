(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ast_walker_fold.ml,v 1.8 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Ast_walker_fold
   Description:
     This module implements a generic tree walker, which recursively
     computes a value over the XQuery core AST.
*)

open Error

open Datatypes

open Xquery_core_ast
open Xquery_core_ast_util


(*********************)
(* Annotation walker *)
(*********************)

let rec fold_over_cexpr loc agg val1 ce =
  begin
    let val1 = agg val1 (loc ce)in
    match ce.pcexpr_desc with
    (* Zeroary expressions *)
    | CEForwardAxis _
    | CEReverseAxis _
    | CEVar _
    | CEScalar _
    | CEProtoValue _
    | CEText _
    | CECharRef _
    | CEComment _
    | CEEmpty
    | CEPI _ ->
	val1
    (* Unary expressions *)
    | CESet (_,cexpr)
    | CESnap (_,cexpr)
    | CECopy cexpr
    | CEDelete cexpr
    | CETreat (cexpr, _)
    | CECast (cexpr, _, _)
    | CECastable (cexpr, _, _)
    | CEValidate (_,cexpr)
    | CEUnordered cexpr
    | CEOrdered cexpr
    | CECommentComputed cexpr
    | CEDocument cexpr
    | CETextComputed cexpr
    | CEForServerClose (_, _, cexpr) 
    | CEEvalClosure (cexpr) ->
	fold_over_cexpr loc agg val1 cexpr
    (* Binary expressions *)
    | CEExecute (_, _, _, cexpr1, cexpr2)
    | CELetServerImplement (_, _, cexpr1, cexpr2)
    | CELetvar (_, _, cexpr1, cexpr2)
    | CEReplace (_, cexpr1, cexpr2)
    | CERename (_, cexpr1, cexpr2)
    | CESome (_, _, cexpr1, cexpr2)
    | CEEvery (_, _, cexpr1, cexpr2)
    | CEAnyElem (cexpr1, _, _, cexpr2)
    | CEAnyAttr (cexpr1, _, cexpr2)
    | CESeq (cexpr1, cexpr2) 
    | CEImperativeSeq (cexpr1, cexpr2)
    | CEPIComputed (cexpr1, cexpr2)
    | CEWhile (cexpr1, cexpr2) ->
	let val1 = fold_over_cexpr loc agg val1 cexpr1 in
	fold_over_cexpr loc agg val1 cexpr2
    | CEInsert (cexpr1, cinsert_location) ->
	begin
	  let cexpr2 =
	    match cinsert_location with
	    | CUAsLastInto cexpr2
	    | CUAsFirstInto cexpr2
	    | CUInto cexpr2
	    | CUAfter cexpr2
	    | CUBefore cexpr2 -> cexpr2
	  in
	  let val1 = fold_over_cexpr loc agg val1 cexpr1 in
	  fold_over_cexpr loc agg val1 cexpr2
	end
    (* Ternary expressions *)
    | CEIf (cexpr1, cexpr2, cexpr3) ->
	let val1 = fold_over_cexpr loc agg val1 cexpr1 in
	let val1 = fold_over_cexpr loc agg val1 cexpr2 in
	fold_over_cexpr loc agg val1 cexpr3
    (* Nary expressions *)
    | CEError cexprlist
    | CEElem (_, _, cexprlist)
    | CEAttr (_, _, cexprlist)
    | CECall (_, cexprlist, _, _, _)
    | CEOverloadedCall (_, cexprlist, _) ->
	List.fold_left (fold_over_cexpr loc agg) val1 cexprlist
    | CEFLWOR (fl_clauses,where_clause,order_by_clause,return_clause) ->
	begin
	  let apply_one val1 fl_clause =
	    begin
	      match fl_clause with
	      | CELET (_,_,cexpr) ->
		  fold_over_cexpr loc agg val1 cexpr
	      | CEFOR (_,_,_,cexpr) ->
		  fold_over_cexpr loc agg val1 cexpr
	    end
	  in
	  let val1 = List.fold_left apply_one val1 fl_clauses in
	  let val1 =
	    match where_clause with
	    | None -> val1
	    | Some cexpr ->
		fold_over_cexpr loc agg val1 cexpr
	  in
	  let val1 =
	    match order_by_clause with
	    | None -> val1
	    | Some (stablekind,order_spec_list,osig) ->
		let apply_one_order_spec val1 (cexpr,sortkind,emptysortkind) =
		  fold_over_cexpr loc agg val1 cexpr 
		in
		List.fold_left apply_one_order_spec val1 order_spec_list 
	  in
	  fold_over_cexpr loc agg val1 return_clause
	end
    | CETypeswitch (cexpr, cases) ->
	begin
	  let val1 = fold_over_cexpr loc agg val1 cexpr in
	  let apply_one_case val1 (pat,ovn,case_cexpr) =
	    fold_over_cexpr loc agg val1 case_cexpr
	  in
	  List.fold_left apply_one_case val1 cases 
	end
  end

(**********************)
(* Cxtype fold walker *)
(**********************)


open Xquery_type_core_ast

let rec fold_over_cxtype loc agg val1 cx =
  let val1 = agg val1 (loc cx) in
	match cx.pcxtype_desc with
		(* Base types *)
	  | CAtomicRef _
	  | CElementRef _
	  | CAttributeRef _
	  | CElementLocal (_, _, _)
	  | CAttributeLocal (_, _)
	  | CText
	  | CPI _
	  | CComment
	  | CEmpty
	  | CNone -> val1

	    (* Single-parameter types *)
	  | CDocument sub_cx
	  | CBound (sub_cx, _, _) -> fold_over_cxtype loc agg val1 sub_cx

	    (* Double-parameter types *)
	  | CSequence (sub_cx1, sub_cx2)
	  | CChoice (sub_cx1, sub_cx2)
	  | CInterleave (sub_cx1, sub_cx2) ->
		  let val1 = fold_over_cxtype loc agg val1 sub_cx1 in
			fold_over_cxtype loc agg val1 sub_cx2
		
