(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ast_walker_rewrite.ml,v 1.39 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Ast_walker_rewrite
   Description:

     This module implements a generic tree walker, which recursively
     applies rewrite rules until a fix-point is reached.

     This module is purely *functional* : it takes annotated cexprs as
     input and produces annotated cexprs as output.  It does not
     destructively modify any AST nodes.
*)

open Error

open Xquery_common_ast
open Xquery_core_ast
open Xquery_core_ast_util

open Datatypes

open Ast_walker_rewrite_context
open Namespace_names

(* Exception raised if a given rewriting rule does not apply *)

exception Not_applied

(*****************************************)
(* Generic application of the rewritings *)
(*****************************************)

(* Applies a single rewriting *)

let apply_single_rule (rewrite_ctxt : 'a rewrite_context) ((ce, changed) : acexpr * bool) one_rule =
  try
    let (ce',changed') = one_rule rewrite_ctxt ce in
    (ce', changed' || changed)
  with
    Not_applied ->
      (ce, changed)

let apply_single_prolog_rule ((rewrite_ctxt, cvardecl, changed) : 'a rewrite_context * acvar_decl * bool) one_rule =
  try
    let (rewrite_ctxt', cvardecl',changed') = one_rule rewrite_ctxt cvardecl in
    (rewrite_ctxt', cvardecl', changed' || changed)
  with
    Not_applied ->
      (rewrite_ctxt, cvardecl, changed)

(* Applies a list of rewritings *)

let apply_rule_generic (rewrite_ctxt : 'a rewrite_context) ce =
  List.fold_left (apply_single_rule rewrite_ctxt) (ce,false) (get_rewrite_rules rewrite_ctxt)

let apply_prolog_rule_generic (rewrite_ctxt : 'a rewrite_context) cvardecl =
  let (rewrite_ctxt', cvardecl', changed') = 
       List.fold_left apply_single_prolog_rule (rewrite_ctxt, cvardecl, false) (get_rewrite_prolog_rules rewrite_ctxt)
  in (rewrite_ctxt', cvardecl')

(***************************)
(* The generic tree-walker *)
(***************************)

let rec generic_cexpr (rewrite_ctxt : 'a rewrite_context) (ce : acexpr) =
  let ma = ce.pcexpr_annot in
  let eh = ce.pcexpr_origin in
  let loc = ce.pcexpr_loc in
  try
  let (optimized_ce, optimized_changed) =
  match ce.pcexpr_desc with
  | CEUnordered cexpr ->
      let (cexpr',changed) = generic_cexpr rewrite_ctxt cexpr in
      (fmkacexpr (CEUnordered cexpr') ma eh loc, changed)

  | CEOrdered cexpr ->
      let (cexpr',changed) = generic_cexpr rewrite_ctxt cexpr in
      (fmkacexpr (CEOrdered cexpr') ma eh loc, changed)

  | CEFLWOR (fl_clauses,where_clause,order_by_clause,return_clause) ->
      let (fl_clauses', fl_changed) =
	let apply_one fl_clause =
	  match fl_clause with
	  | CELET (cst,vname,cexpr) ->
	      let (cexpr',changed) = generic_cexpr rewrite_ctxt cexpr in
	      (CELET(cst,vname,cexpr'),changed)
	  | CEFOR (cst,vname1,vname2,cexpr) ->
	      let (cexpr',changed) = generic_cexpr rewrite_ctxt cexpr in
	      (CEFOR(cst,vname1,vname2,cexpr'),changed)
	in
	let fl_clauses1 = List.map apply_one fl_clauses in 
	let (fl_clauses2,changed2) = List.split fl_clauses1 in
	let changed_bool_value = List.mem true changed2 in 
	(fl_clauses2,changed_bool_value)
      in
      let (where_clause', where_changed) =
	match where_clause with
	| None -> (None,false)
	| Some cexpr ->
	    let (cexpr',changed) = generic_cexpr rewrite_ctxt cexpr in
	    (Some cexpr',changed)
      in
      let (order_by_clause', order_by_changed) =
	match order_by_clause with
	| None -> (None,false)
	| Some (stablekind,order_spec_list,osig) ->
	    let apply_one_order_spec (cexpr,sortkind,emptysortkind) =
	      let (new_cexpr, changed) = generic_cexpr rewrite_ctxt cexpr in
	      ((new_cexpr,sortkind,emptysortkind),changed)
	    in
	    let order_spec_list1 = List.map apply_one_order_spec order_spec_list in
	    let (order_spec_list2,changed2) = List.split order_spec_list1 in
	    let changed_bool_value = List.mem true changed2 in 
	    (Some (stablekind, order_spec_list2,osig), changed_bool_value)
      in
      let (return_clause',return_changed) = generic_cexpr rewrite_ctxt return_clause in
      (fmkacexpr (CEFLWOR(fl_clauses',where_clause',order_by_clause',return_clause')) ma eh loc, fl_changed || where_changed || order_by_changed || return_changed)

  | CEIf (cexpr1, cexpr2, cexpr3) ->
      let (cexpr1',changed1) = generic_cexpr rewrite_ctxt cexpr1 in
      let (cexpr2',changed2) = generic_cexpr rewrite_ctxt cexpr2 in
      let (cexpr3',changed3) = generic_cexpr rewrite_ctxt cexpr3 in
      (fmkacexpr (CEIf (cexpr1', cexpr2', cexpr3')) ma eh loc, changed1 || changed2 || changed3)

  | CEWhile (cexpr1, cexpr2) ->
      let (cexpr1',changed1) = generic_cexpr rewrite_ctxt cexpr1 in
      let (cexpr2',changed2) = generic_cexpr rewrite_ctxt cexpr2 in
      (fmkacexpr (CEWhile (cexpr1', cexpr2')) ma eh loc, changed1 || changed2)

  | CETypeswitch (cexpr, cases) ->
      let (cexpr',changed') = generic_cexpr rewrite_ctxt cexpr in
      let apply_one_case (pat,ovn,case_cexpr) =
	let (new_case_cexpr, changed) = generic_cexpr rewrite_ctxt case_cexpr
	in ((pat,ovn,new_case_cexpr), changed)
      in
      let cases1 = List.map apply_one_case cases in 
      let (cases2,changed2) = List.split cases1 in
      let changed_bool_value = List.mem true changed2 in 
      (fmkacexpr (CETypeswitch (cexpr', cases2)) ma eh loc, changed' || changed_bool_value)

  | CEVar vname ->
      (ce, false)

  | CEScalar _ -> 
      (ce,false)

  | CEProtoValue _ -> 
      (ce,false)

  | CEText _ -> 
      (ce,false)

  | CECharRef _ -> 
      (ce,false)

  | CETextComputed cexpr1 ->
      let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
      (fmkacexpr (CETextComputed cexpr1') ma eh loc, changed1)

  | CEPI _ ->
      (ce,false)

  | CEPIComputed (cexpr1, cexpr2) ->
      let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
      let (cexpr2', changed2) = generic_cexpr rewrite_ctxt cexpr2 in
      (fmkacexpr (CEPIComputed(cexpr1', cexpr2')) ma eh loc, changed1 || changed2)

  | CEComment _ ->
      (ce,false)

  | CECommentComputed cexpr1 ->
      let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
      (fmkacexpr (CECommentComputed cexpr1') ma eh loc, changed1)

  | CEDocument cexpr1 ->
      let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
      (fmkacexpr (CEDocument cexpr1') ma eh loc, false)

  | CECall (fname, arguments, sign, upd, selfrecur) ->
      let arguments' = List.map (generic_cexpr rewrite_ctxt) arguments in
      let arguments1 = List.map (fun (arg, changed) -> arg) arguments' in
      let changed_list1 = List.map (fun (arg, changed) -> changed) arguments' in
      (fmkacexpr (CECall (fname, arguments1, sign, upd, selfrecur)) ma eh loc, (List.mem true changed_list1))

  | CEOverloadedCall (fname, arguments,sigs) ->
      let arguments' = List.map (generic_cexpr rewrite_ctxt) arguments in
      let arguments1 = List.map (fun (arg, changed) -> arg) arguments' in
      let changed_list1 = List.map (fun (arg, changed) -> changed) arguments' in
      (fmkacexpr (CEOverloadedCall (fname, arguments1,sigs)) ma eh loc, (List.mem true changed_list1))

  | CELetServerImplement (nc1, nc2, ce1, ce2) ->
      let (ce1', changed1) = generic_cexpr rewrite_ctxt ce1 in
      let (ce2', changed2) = generic_cexpr rewrite_ctxt ce2 in
      (fmkacexpr (CELetServerImplement(nc1,nc2,ce1',ce2')) ma eh loc, changed1 || changed2)

  | CEExecute (async, ncname, uri, hostport, cexpr) ->
      let (hostport', changed1) = generic_cexpr rewrite_ctxt hostport in 
      let (cexpr', changed2) = generic_cexpr rewrite_ctxt cexpr in
      (fmkacexpr (CEExecute(async, ncname, uri, hostport', cexpr')) ma eh loc, changed1 || changed2)

  | CEForServerClose (nc1, uri, ce1) ->
      let (ce1', changed1) = generic_cexpr rewrite_ctxt ce1 in
      (fmkacexpr (CEForServerClose(nc1,uri,ce1')) ma eh loc, changed1)

  | CEEvalClosure (ce1) ->
      let (ce1', changed1) = generic_cexpr rewrite_ctxt ce1 in
      (fmkacexpr (CEEvalClosure(ce1')) ma eh loc, changed1)

  | CESeq (cexpr1, cexpr2) ->
      let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
      let (cexpr2', changed2) = generic_cexpr rewrite_ctxt cexpr2 in
      (fmkacexpr (CESeq(cexpr1', cexpr2')) ma eh loc, changed1 || changed2)

  | CEImperativeSeq (cexpr1, cexpr2) ->
      let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
      let (cexpr2', changed2) = generic_cexpr rewrite_ctxt cexpr2 in
      (fmkacexpr (CEImperativeSeq(cexpr1', cexpr2')) ma eh loc, changed1 || changed2)


  | CEEmpty -> 
      (ce,false)

  | CEElem (relem_symbol, nsenv, cexprlist) ->
      let (cexprlist', changed') = 
	List.fold_right (fun cexpr (cel, changed) -> 
	let (cexpr',changed') = generic_cexpr rewrite_ctxt cexpr in
	(cexpr' :: cel, changed || changed')) cexprlist ([], false) in
      (fmkacexpr (CEElem (relem_symbol, nsenv, cexprlist')) ma eh loc, changed')

  | CEAnyElem (cexpr1, nsenv1, nsenv2, cexpr2) ->  
      let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
      let (cexpr2', changed2) = generic_cexpr rewrite_ctxt cexpr2 in
      (fmkacexpr (CEAnyElem (cexpr1', nsenv1, nsenv2, cexpr2')) ma eh loc, changed1 || changed2)

  | CEAttr (rattr_symbol, cexprlist)->
      let (cexprlist', changed') = 
	List.fold_right (fun cexpr (cel, changed) -> 
	let (cexpr',changed') = generic_cexpr rewrite_ctxt cexpr in
	(cexpr' :: cel, changed || changed')) cexprlist ([], false) in
      (fmkacexpr (CEAttr (rattr_symbol, cexprlist')) ma eh loc, changed')

  | CEAnyAttr (cexpr1, nsenv, cexpr2) ->
      let (cexpr1',changed1) = generic_cexpr rewrite_ctxt cexpr1 in
      let (cexpr2', changed2) = generic_cexpr rewrite_ctxt cexpr2 in
      (fmkacexpr (CEAnyAttr (cexpr1', nsenv, cexpr2')) ma eh loc, changed1 || changed2)

  | CEError _ -> 
      (ce, false)

  | CETreat (cexpr, model) ->
      let (cexpr', changed') = generic_cexpr rewrite_ctxt cexpr in
      (fmkacexpr (CETreat (cexpr', model)) ma eh loc, changed')

  | CECast (cexpr, nsenv, model) ->
      let (cexpr', changed') = generic_cexpr rewrite_ctxt cexpr in
      (fmkacexpr (CECast (cexpr', nsenv, model)) ma eh loc, changed')

  | CECastable (cexpr, nsenv, model) ->
      let (cexpr', changed') = generic_cexpr rewrite_ctxt cexpr in
      (fmkacexpr (CECastable (cexpr', nsenv, model)) ma eh loc, changed')

  | CEValidate (vmode,cexpr) ->
      let (cexpr', changed') = generic_cexpr rewrite_ctxt cexpr in
      (fmkacexpr (CEValidate (vmode,cexpr')) ma eh loc, changed')

  | CEForwardAxis (v, axis, cnode_test) ->
      (ce,false)

  | CEReverseAxis (v, axis, cnode_test) ->
      (ce,false)

  | CESome (odt, vname, cexpr1, cexpr2) ->
      let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
      let (cexpr2', changed2) = generic_cexpr rewrite_ctxt cexpr2 in
      (fmkacexpr (CESome (odt, vname, cexpr1', cexpr2')) ma eh loc, changed1 || changed2)
  | CEEvery (odt, vname, cexpr1, cexpr2) ->
      let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
      let (cexpr2', changed2) = generic_cexpr rewrite_ctxt cexpr2 in
      (fmkacexpr (CEEvery (odt, vname, cexpr1', cexpr2')) ma eh loc, changed1 || changed2)
  | CECopy ce1 ->
      let (ce1', changed1) = generic_cexpr rewrite_ctxt ce1 in
      (fmkacexpr (CECopy ce1') ma eh loc, changed1)
  | CEDelete ce1 ->
      let (ce1', changed1) = generic_cexpr rewrite_ctxt ce1 in
      (fmkacexpr (CEDelete ce1') ma eh loc, changed1)
  | CEInsert (ce1, cinsert_location) ->
      let (ce1', changed1) = generic_cexpr rewrite_ctxt ce1 in
      let (cinsert_location', changed2) =
	rewrite_cinsert_location rewrite_ctxt cinsert_location
      in
      (fmkacexpr (CEInsert (ce1', cinsert_location')) ma eh loc, changed1 || changed2)
  | CERename (nsenv, ce1, ce2) ->
      let (ce1', changed1) = generic_cexpr rewrite_ctxt ce1 in
      let (ce2', changed2) = generic_cexpr rewrite_ctxt ce2 in
      (fmkacexpr (CERename (nsenv, ce1',ce2')) ma eh loc, changed1 || changed2)
  | CEReplace (vof, ce1, ce2) ->
      let (ce1', changed1) = generic_cexpr rewrite_ctxt ce1 in
      let (ce2', changed2) = generic_cexpr rewrite_ctxt ce2 in
      (fmkacexpr (CEReplace (vof,ce1',ce2')) ma eh loc, changed1 || changed2)
  | CESnap (sm, ce1) ->
      let (ce1',changed1) = generic_cexpr rewrite_ctxt ce1 in
	(fmkacexpr (CESnap (sm,ce1')) ma eh loc, changed1)
  | CELetvar (rodt,v,ce1,ce2) ->
	  let (ce1', changed1) = generic_cexpr rewrite_ctxt ce1 in
      let (ce2', changed2) = generic_cexpr rewrite_ctxt ce2 in
	    (fmkacexpr(CELetvar(rodt,v,ce1',ce2')) ma eh loc,changed2)
  | CESet (v,ce) ->
	  let (ce', changed) = generic_cexpr rewrite_ctxt ce in
	    (fmkacexpr(CESet(v,ce')) ma eh loc,changed)
  in
  (* apply local rewritings *)
  let (local_optimized_ce, local_changed) = apply_rule_generic rewrite_ctxt optimized_ce in
  if local_changed
  then 
    begin 
      if (Debug.default_debug()) then 
	begin
	  let msg =
	    "LOCAL :" ^
	    (Print_top.bprintf_acexpr "" local_optimized_ce)
	  in
	  Debug.print_default_debug msg
	end;
      (local_optimized_ce, true)
    end
  else 
    begin
      if (optimized_changed && (Debug.default_debug())) then
	begin
	  let msg =
	    "OPT :" ^ 
	    (Print_top.bprintf_acexpr "" optimized_ce)
	  in
	  Debug.print_default_debug msg
	end;
      (optimized_ce, optimized_changed)
    end
  with
  | exn -> raise(Error.error_with_file_location loc exn)

and rewrite_cinsert_location rewrite_ctxt cinsert_location =
  match cinsert_location with
  | CUAsLastInto ce1 ->
      let (ce1',changed1) = generic_cexpr rewrite_ctxt ce1 in
      (CUAsLastInto ce1',changed1)
  | CUAsFirstInto ce1 ->
      let (ce1',changed1) = generic_cexpr rewrite_ctxt ce1 in
      (CUAsFirstInto ce1',changed1)
  | CUInto ce1 ->
      let (ce1',changed1) = generic_cexpr rewrite_ctxt ce1 in
      (CUInto ce1',changed1)
  | CUAfter ce1 ->
      let (ce1',changed1) = generic_cexpr rewrite_ctxt ce1 in
      (CUAfter ce1',changed1)
  | CUBefore ce1 ->
      let (ce1',changed1) = generic_cexpr rewrite_ctxt ce1 in
      (CUBefore ce1',changed1)


(******************)
(* Children apply *)
(******************)

(* Note:
   Applies not to the current expression, but to all of the
   sub-expressions.
   - Jerome *)

let children_cexpr rewrite_ctxt ce =
  let ma = ce.pcexpr_annot in
  let eh = ce.pcexpr_origin in
  let loc = ce.pcexpr_loc in
  try
    let (optimized_ce, optimized_changed) =
      match ce.pcexpr_desc with
      | CEUnordered cexpr ->
	  let (cexpr',changed) = generic_cexpr rewrite_ctxt cexpr in
	  (fmkacexpr (CEUnordered cexpr') ma eh loc, changed)
      | CEOrdered cexpr ->
	  let (cexpr',changed) = generic_cexpr rewrite_ctxt cexpr in
	  (fmkacexpr (CEOrdered cexpr') ma eh loc, changed)
      | CEFLWOR (fl_clauses,where_clause,order_by_clause,return_clause) ->
	  let (fl_clauses', fl_changed) =
	    let apply_one fl_clause =
	      match fl_clause with
	      | CELET (cst,vname,cexpr) ->
		  let (cexpr',changed) = generic_cexpr rewrite_ctxt cexpr in
		  (CELET(cst,vname,cexpr'),changed)
	      | CEFOR (cst,vname1,vname2,cexpr) ->
		  let (cexpr',changed) = generic_cexpr rewrite_ctxt cexpr in
		  (CEFOR(cst,vname1,vname2,cexpr'),changed)
	    in
	    let fl_clauses1 = List.map apply_one fl_clauses in 
	    let (fl_clauses2,changed2) = List.split fl_clauses1 in
	    let changed_bool_value = List.mem true changed2 in 
	    (fl_clauses2,changed_bool_value)
	  in
	  let (where_clause', where_changed) =
	    match where_clause with
	    | None -> (None,false)
	    | Some cexpr ->
		let (cexpr',changed) = generic_cexpr rewrite_ctxt cexpr in
		(Some cexpr',changed)
	  in
	  let (order_by_clause', order_by_changed) =
	    match order_by_clause with
	    | None -> (None,false)
	    | Some (stablekind,order_spec_list,osig) ->
		let apply_one_order_spec (cexpr,sortkind,emptysortkind) =
		  let (new_cexpr, changed) = generic_cexpr rewrite_ctxt cexpr in
		  ((new_cexpr,sortkind,emptysortkind),changed)
		in
		let order_spec_list1 = List.map apply_one_order_spec order_spec_list in
		let (order_spec_list2,changed2) = List.split order_spec_list1 in
		let changed_bool_value = List.mem true changed2 in 
		(Some (stablekind, order_spec_list2,osig), changed_bool_value)
	  in
	  let (return_clause',return_changed) = generic_cexpr rewrite_ctxt return_clause in
	  (fmkacexpr (CEFLWOR(fl_clauses',where_clause',order_by_clause',return_clause')) ma eh loc, fl_changed || where_changed || order_by_changed || return_changed)

      | CEIf (cexpr1, cexpr2, cexpr3) ->
	  let (cexpr1',changed1) = generic_cexpr rewrite_ctxt cexpr1 in
	  let (cexpr2',changed2) = generic_cexpr rewrite_ctxt cexpr2 in
	  let (cexpr3',changed3) = generic_cexpr rewrite_ctxt cexpr3 in
	  (fmkacexpr (CEIf (cexpr1', cexpr2', cexpr3')) ma eh loc, changed1 || changed2 || changed3)

      | CEWhile (cexpr1, cexpr2) ->
	  let (cexpr1',changed1) = generic_cexpr rewrite_ctxt cexpr1 in
	  let (cexpr2',changed2) = generic_cexpr rewrite_ctxt cexpr2 in
	  (fmkacexpr (CEWhile (cexpr1', cexpr2')) ma eh loc, changed1 || changed2)

      | CETypeswitch (cexpr, cases) ->
	  let (cexpr',changed') = generic_cexpr rewrite_ctxt cexpr in
	  let apply_one_case (pat,ovn,case_cexpr) =
	    let (new_case_cexpr, changed) = generic_cexpr rewrite_ctxt case_cexpr
	    in ((pat,ovn,new_case_cexpr), changed)
	  in
	  let cases1 = List.map apply_one_case cases in
          let aggregate_case ((pat, ovn, new_case_cexpr), changed) = (pat, ovn, new_case_cexpr) in
	  let aggregate_bool ((pat, ovn, new_case_cexpr), changed) = changed in
	  let cases2 = List.map aggregate_case cases1 in
	  let changed2 = List.map aggregate_bool cases1 in
	  let changed_bool_value = List.mem true changed2 in 
	  (fmkacexpr (CETypeswitch (cexpr', cases2)) ma eh loc, changed' || changed_bool_value)

      | CEVar vname -> 
	  (ce, false)

      | CEScalar _ ->
	  (ce,false)

      | CEProtoValue _ ->
	  (ce,false)

      | CEText _ ->
	  (ce,false)

      | CECharRef _ ->
	  (ce,false)

      | CETextComputed cexpr1 ->
	  let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
	  (fmkacexpr (CETextComputed cexpr1') ma eh loc, changed1)

      | CEPI _ ->
	  (ce,false)

      | CEPIComputed (cexpr1, cexpr2) ->
	  let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
	  let (cexpr2', changed2) = generic_cexpr rewrite_ctxt cexpr2 in
	  (fmkacexpr (CEPIComputed(cexpr1', cexpr2')) ma eh loc, changed1 || changed2)

      | CEComment _ ->
	  (ce,false)

      | CECommentComputed cexpr1 ->
	  let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
	  (fmkacexpr (CECommentComputed cexpr1') ma eh loc, changed1)

      | CEDocument cexpr1 ->
	  let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
	  (fmkacexpr (CEDocument cexpr1') ma eh loc, changed1)

      | CECall (fname, arguments, sign, upd, selfrecur) ->
	  let arguments' = List.map (generic_cexpr rewrite_ctxt) arguments in
	  let arguments1 = List.map (fun (arg, changed) -> arg) arguments' in
	  let changed_list1 = List.map (fun (arg, changed) -> changed) arguments' in
	  (fmkacexpr (CECall (fname, arguments1,sign, upd, selfrecur)) ma eh loc, (List.mem true changed_list1))

      | CEOverloadedCall (fname, arguments, sigs) ->
	  let arguments' = List.map (generic_cexpr rewrite_ctxt) arguments in
	  let arguments1 = List.map (fun (arg, changed) -> arg) arguments' in
	  let changed_list1 = List.map (fun (arg, changed) -> changed) arguments' in
	  (fmkacexpr (CEOverloadedCall (fname, arguments1, sigs)) ma eh loc, (List.mem true changed_list1))

      | CELetServerImplement (nc1, nc2, ce1, ce2) ->
	  let (ce1', changed1) = generic_cexpr rewrite_ctxt ce1 in
	  let (ce2', changed2) = generic_cexpr rewrite_ctxt ce2 in
	  (fmkacexpr (CELetServerImplement(nc1,nc2,ce1',ce2')) ma eh loc, changed1 || changed2)

      | CEExecute (async,ncname, uri, hostport, cexpr) ->
	  let (hostport', changed1) = generic_cexpr rewrite_ctxt hostport in 
	  let (cexpr', changed2) = generic_cexpr rewrite_ctxt cexpr in
	  (fmkacexpr (CEExecute (async, ncname, uri, hostport', cexpr')) ma eh loc, changed1 || changed2)

      | CEForServerClose (nc1,uri,cexpr1) ->
	  let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
	  (fmkacexpr (CEForServerClose(nc1, uri,cexpr1')) ma eh loc, changed1)

      | CEEvalClosure cexpr1 ->
	  let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
	  (fmkacexpr (CEEvalClosure cexpr1') ma eh loc, changed1)

      | CESeq (cexpr1, cexpr2) ->
	  let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
	  let (cexpr2', changed2) = generic_cexpr rewrite_ctxt cexpr2 in
	  (fmkacexpr (CESeq(cexpr1', cexpr2')) ma eh loc, changed1 || changed2)

      | CEImperativeSeq (cexpr1, cexpr2) ->
	  let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
	  let (cexpr2', changed2) = generic_cexpr rewrite_ctxt cexpr2 in
	  (fmkacexpr (CEImperativeSeq(cexpr1', cexpr2')) ma eh loc, changed1 || changed2)

      | CEEmpty -> 
	  (ce,false)

      | CEElem (relem_symbol, nsenv, cexprlist) ->
	  let (cexprlist', changed') = 
	    List.fold_right (fun cexpr (cel, changed) ->
	      let (cexpr',changed') = generic_cexpr rewrite_ctxt cexpr in
	      (cexpr' :: cel, changed || changed')) cexprlist ([], false)
	  in
	  (fmkacexpr (CEElem (relem_symbol, nsenv, cexprlist')) ma eh loc, changed')

      | CEAnyElem (cexpr1, nsenv1, nsenv2, cexpr2) ->  
	  let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
	  let (cexpr2', changed2) = generic_cexpr rewrite_ctxt cexpr2 in
	  (fmkacexpr (CEAnyElem (cexpr1', nsenv1, nsenv2, cexpr2')) ma eh loc, changed1 || changed2)

      | CEAttr (rattr_symbol, cexprlist)->
	  let (cexprlist', changed') = 
	    List.fold_right (fun cexpr (cel, changed) ->
	      let (cexpr',changed') = generic_cexpr rewrite_ctxt cexpr in
	      (cexpr' :: cel, changed || changed')) cexprlist ([], false)
	  in
	  (fmkacexpr (CEAttr (rattr_symbol, cexprlist')) ma eh loc, changed')

      | CEAnyAttr (cexpr1, nsenv, cexpr2) ->
	  let (cexpr1',changed1) = generic_cexpr rewrite_ctxt cexpr1 in
	  let (cexpr2', changed2) = generic_cexpr rewrite_ctxt cexpr2 in
	  (fmkacexpr (CEAnyAttr (cexpr1', nsenv, cexpr2')) ma eh loc, changed1 || changed2)

      | CEError _ ->
	  (ce, false)

      | CETreat (cexpr, model) ->
	  let (cexpr', changed') = generic_cexpr rewrite_ctxt cexpr in
	  (fmkacexpr (CETreat (cexpr', model)) ma eh loc, changed')

      | CECast (cexpr, nsenv, model) ->
	  let (cexpr', changed') = generic_cexpr rewrite_ctxt cexpr in
	  (fmkacexpr (CECast (cexpr', nsenv, model)) ma eh loc, changed')

      | CECastable (cexpr, nsenv, model) ->
	  let (cexpr', changed') = generic_cexpr rewrite_ctxt cexpr in
	  (fmkacexpr (CECastable (cexpr', nsenv, model)) ma eh loc, changed')

      | CEValidate (vmode,cexpr) ->
	  let (cexpr', changed') = generic_cexpr  rewrite_ctxt cexpr in
	  (fmkacexpr (CEValidate (vmode,cexpr')) ma eh loc, changed')

      | CEForwardAxis (v, axis, cnode_test) ->
	  (ce,false)

      | CEReverseAxis (v, axis, cnode_test) ->
	  (ce,false)

      | CESome (odt, vname, cexpr1, cexpr2) ->
	  let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
	  let (cexpr2', changed2) = generic_cexpr rewrite_ctxt cexpr2 in
	  (fmkacexpr (CESome (odt, vname, cexpr1', cexpr2')) ma eh loc, changed1 || changed2)

      | CEEvery (odt, vname, cexpr1, cexpr2) ->
	  let (cexpr1', changed1) = generic_cexpr rewrite_ctxt cexpr1 in
	  let (cexpr2', changed2) = generic_cexpr rewrite_ctxt cexpr2 in
	  (fmkacexpr (CEEvery (odt, vname, cexpr1', cexpr2')) ma eh loc, changed1 || changed2)

      | CECopy ce1 ->
	  let (ce1', changed1) = generic_cexpr rewrite_ctxt ce1 in
	  (fmkacexpr (CECopy ce1') ma eh loc, changed1)
      | CEDelete ce1 ->
	  let (ce1', changed1) = generic_cexpr rewrite_ctxt ce1 in
	  (fmkacexpr (CEDelete ce1') ma eh loc, changed1)
      | CEInsert (ce1, cinsert_location) ->
	  let (ce1', changed1) = generic_cexpr rewrite_ctxt ce1 in
	  let (cinsert_location', changed2) =
	    rewrite_cinsert_location rewrite_ctxt cinsert_location
	  in
	  (fmkacexpr (CEInsert (ce1', cinsert_location')) ma eh loc, changed1 || changed2)
      | CERename (nsenv, ce1, ce2) ->
	  let (ce1', changed1) = generic_cexpr rewrite_ctxt ce1 in
	  let (ce2', changed2) = generic_cexpr rewrite_ctxt ce2 in
	  (fmkacexpr (CERename (nsenv, ce1',ce2')) ma eh loc, changed1 || changed2)
      | CEReplace (vof, ce1, ce2) ->
	  let (ce1', changed1) = generic_cexpr rewrite_ctxt ce1 in
	  let (ce2', changed2) = generic_cexpr rewrite_ctxt ce2 in
	  (fmkacexpr (CEReplace (vof,ce1',ce2')) ma eh loc, changed1 || changed2)
      | CESnap (sm,ce1) ->
	  let (ce',changed) = generic_cexpr rewrite_ctxt ce1 in
	    (fmkacexpr (CESnap (sm,ce')) ma eh loc, changed)
      | CELetvar (codt,v,ce1,ce2) ->
	  let (ce1', changed1) = generic_cexpr rewrite_ctxt ce1 in
	  let (ce2', changed2) = generic_cexpr rewrite_ctxt ce2 in
	  (fmkacexpr (CELetvar (codt,v,ce1',ce2')) ma eh loc, changed1 || changed2)
      | CESet (v,ce) ->
	      let (ce', changed) = generic_cexpr rewrite_ctxt ce in
	        (fmkacexpr(CESet(v,ce')) ma eh loc,changed)

    in
    (optimized_ce, optimized_changed)
  with
  | exn -> raise(Error.error_with_file_location loc exn)

(**************************************)
(* Fix-point simplify core expression *)
(**************************************)

let rec fix_point_cexpr rewrite_ctxt ce =
  let (ce1, changed) = generic_cexpr rewrite_ctxt ce in
  if changed
  then fix_point_cexpr rewrite_ctxt ce1
  else ce1


(***********************)
(* Expression rewriter *)
(***********************)

let rewrite_cexpr (toplevel_rewrite_ctxt, recurse_rewrite_ctxt) ce =
  let (ce', changed) = apply_rule_generic toplevel_rewrite_ctxt ce in
  fix_point_cexpr recurse_rewrite_ctxt ce'

(***********************)
(* Statements rewriter *)
(***********************)

let rewrite_cstatement rewrite_ctxt ce =
  rewrite_cexpr rewrite_ctxt ce


(*************************)
(* Query prolog rewriter *)
(*************************)

(* Applying a rewriting pass to function declarations *)

let cfunction_body_simplify rewrite_ctxt cfunction_body =
  match cfunction_body with
  | CEFunctionInterface 
  | CEFunctionImported
  | CEFunctionBltIn -> cfunction_body
  | CEFunctionUser ce ->
      CEFunctionUser (rewrite_cexpr rewrite_ctxt ce)

let function_simplify rewrite_ctxt cfunction_def =
  let new_desc =
    match cfunction_def.pcfunction_def_desc with
    | (rfname, vars, cfunction_signature, cfunction_body, upd) ->
	(rfname, vars, cfunction_signature, cfunction_body_simplify rewrite_ctxt cfunction_body, upd)
  in
  fmkcfunction_def new_desc cfunction_def.pcfunction_def_loc

let var_simplify (rewrite_ctxt, cvar_decl_list) cvar_decl =
  let new_desc =
    match cvar_decl.pcvar_decl_desc with
    | (vname, model, CEVarUser ce) ->
	(vname, model, CEVarUser (rewrite_cexpr rewrite_ctxt ce))
    | (vname, model, cvar_body) ->
	(vname, model, cvar_body)
  in
  let (toplevel_rewrite_ctxt, recurse_rewrite_ctxt) = rewrite_ctxt in 
  let cvar_decl' = fmkcvar_decl new_desc cvar_decl.pcvar_decl_loc in
  let (toplevel_rewrite_ctxt', cvar_decl'') = apply_prolog_rule_generic toplevel_rewrite_ctxt cvar_decl' in
  ((toplevel_rewrite_ctxt', recurse_rewrite_ctxt), cvar_decl_list @ [cvar_decl''])

let server_simplify (rewrite_ctxt, csrv_decl_list) cserver_decl =
  let (prefix, uri, ce) = cserver_decl.pcserver_decl_desc in 
  let new_desc = (prefix, uri, (rewrite_cexpr rewrite_ctxt ce)) in
  let cserver_decl' = fmkcserver_decl new_desc cserver_decl.pcserver_decl_loc in
  (rewrite_ctxt, csrv_decl_list @ [cserver_decl'])

let index_simplify rewrite_ctxt cindex_def =
  let new_desc =
    match cindex_def.pcindex_def_desc with
    | CValueIndex (name, ce1, ce2) ->
	CValueIndex(name,rewrite_cexpr rewrite_ctxt ce1,rewrite_cexpr rewrite_ctxt ce2)
    | CNameIndex name -> 
	CNameIndex name
  in
  fmkcindex_def new_desc cindex_def.pcindex_def_loc

(* Main optimizer call for the query prolog *)

let rewrite_cprolog rewrite_ctxt cprolog =
  let pcprolog_functions' =
    List.map (function_simplify rewrite_ctxt) cprolog.pcprolog_functions
  in
  let (rewrite_ctxt', pcprolog_vars') = List.fold_left var_simplify (rewrite_ctxt, []) cprolog.pcprolog_vars in
  let (rewrite_ctxt', pcprolog_servers') = List.fold_left server_simplify (rewrite_ctxt', []) cprolog.pcprolog_servers in
  let pcprolog_indices' = List.map (index_simplify rewrite_ctxt') cprolog.pcprolog_indices in
  { pcprolog_functions = pcprolog_functions';
    pcprolog_vars = pcprolog_vars';
    pcprolog_servers = pcprolog_servers';
    pcprolog_indices = pcprolog_indices'; }

let rewrite_cxmodule rewrite_ctxt cxmodule =
  { pcmodule_prolog = rewrite_cprolog rewrite_ctxt cxmodule.pcmodule_prolog;
    pcmodule_statements = List.map (rewrite_cstatement rewrite_ctxt) cxmodule.pcmodule_statements }

(***************************)
(* Specific rewriting rule *)
(***************************)

(**************************************************************************)
(*                    free_variables function                             *)
(* Returns the variables that are free in core expression                 *)
(**************************************************************************)

let rec free_var_aux bound_vars ce =
  let fi = ce.pcexpr_loc in

  (* The core expression contains the file location that
     should be correlated with errors, so we catch any exceptions here
     and rewrap the exceptions with the file location. *) 
  try
    match ce.pcexpr_desc with
    | CEUnordered cexpr ->
	(free_var_aux bound_vars cexpr)
    | CEOrdered cexpr ->
	(free_var_aux bound_vars cexpr)
    | CESome (_, vname, cexpr1, cexpr2)
    | CEEvery (_, vname, cexpr1, cexpr2) ->
	(free_var_aux bound_vars cexpr1) @ 
	(free_var_aux (vname::bound_vars) cexpr2)
    | CEFLWOR (fl_clauses,where_clause,order_by_clause,return_clause) ->
      (* 1. process the fl clauses firsts *)
	let first_clause,flwor_rest = get_first_fl_clause ce in
	begin
	  match first_clause with
	  | None ->
	      begin
		let where_vars =
		  match where_clause with
		  | None -> []
		  | Some cexpr ->
		      (free_var_aux bound_vars cexpr)
		in
		let order_by_vars =
		  match order_by_clause with
		  | None -> []
		  | Some (_,order_spec_list,osig) ->
		      free_vars_in_order_spec_list bound_vars order_spec_list
		in
		let return_vars =
		  (free_var_aux bound_vars return_clause)
		in
		where_vars @ order_by_vars @ return_vars
	      end
	  | Some (CELET (_,vname,cexpr1)) ->
	      (free_var_aux bound_vars cexpr1) @ (free_var_aux (vname::bound_vars) flwor_rest)
	  | Some (CEFOR (_,vname,None,cexpr1)) ->
	      (free_var_aux bound_vars cexpr1) @ (free_var_aux (vname::bound_vars) flwor_rest)
	  | Some (CEFOR (_,vname,Some vname',cexpr1)) ->
	      (free_var_aux bound_vars cexpr1) @ (free_var_aux (vname::vname'::bound_vars) flwor_rest)
	end
    | CEIf (cexpr1, cexpr2, cexpr3) ->
	(free_var_aux bound_vars cexpr1) @ (free_var_aux bound_vars cexpr2) @ (free_var_aux bound_vars cexpr3)

    | CEWhile (cexpr1, cexpr2) ->
	(free_var_aux bound_vars cexpr1) @ (free_var_aux bound_vars cexpr2)
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
	(free_var_aux bound_vars cexpr) @ (free_vars_in_cases bound_vars cases)

    | CEVar vname -> 
     (* In XQuery: $x, $v, $fs:dot *)
	if List.mem vname bound_vars 
	then []
	else [vname]

    | CEScalar _ -> []
	  
    | CEProtoValue _ -> []
	  
    | CEText _ -> []

    | CECharRef _ -> []

    | CETextComputed cexpr ->
	free_var_aux bound_vars cexpr
	  
    | CEPI _ -> []
	  
    | CEPIComputed (cexpr1, cexpr2) ->
	(free_var_aux bound_vars cexpr1) @ (free_var_aux bound_vars cexpr2)

    | CEComment _ -> []
	  
    | CECommentComputed cexpr ->
	free_var_aux bound_vars cexpr
	  
    | CEDocument cexpr ->       	      
	free_var_aux bound_vars cexpr

    | CECall (fname, arguments, _, _,_ ) ->
	List.concat (List.map (free_var_aux bound_vars) arguments)

    | CEOverloadedCall (fname, arguments, sigs) ->
	List.concat (List.map (free_var_aux bound_vars) arguments)

    | CESeq (cexpr1, cexpr2) ->
	(free_var_aux bound_vars cexpr1) @ (free_var_aux bound_vars cexpr2)

    | CEImperativeSeq (cexpr1, cexpr2) ->
	(free_var_aux bound_vars cexpr1) @ (free_var_aux bound_vars cexpr2)


    | CEEmpty -> []
	  
    | CEElem (relem_symbol, nsenv, cexprlist) ->
	List.concat (List.map (free_var_aux bound_vars) cexprlist)
	  
    | CEAttr (rattr_symbol, cexprlist) ->
	List.concat (List.map (free_var_aux bound_vars) cexprlist)

	  (* Binary expressions *)
    | CEAnyElem (cexpr1, _, _, cexpr2) 
    | CEAnyAttr (cexpr1, _, cexpr2) ->
	(free_var_aux bound_vars cexpr1) @ 
	(free_var_aux bound_vars cexpr2)	
					     
    | CEError (_) -> []

	  (* Unary expressions *)
    | CETreat (cexpr, _) 
    | CEValidate (_,cexpr) 
    | CECast (cexpr, _, _) 
    | CECastable (cexpr, _, _) ->
	(free_var_aux bound_vars cexpr)

    | CEForwardAxis (v, axis, cnode_test) ->
	if List.mem v bound_vars 
	then []
	else [v]

    | CEReverseAxis (v, axis, cnode_test) ->
(*	if List.mem fs_dot bound_vars *)
	if List.mem v bound_vars 
	then []
	else [v]
    | CELetServerImplement (_,_, ce1,ce2) 
    | CEExecute (_, _, _, ce1, ce2) -> 
	(free_var_aux bound_vars ce1)@
	(free_var_aux bound_vars ce2)
	  (* Unary Operators *)
    | CEForServerClose (_,_,cexpr1) 
    | CEEvalClosure (cexpr1) 
    | CECopy cexpr1 
    | CEDelete cexpr1 ->
	(free_var_aux bound_vars cexpr1)
    | CEInsert (cexpr1, cinsert_location) ->
	(free_var_aux bound_vars cexpr1)
	@ (free_vars_in_insert_location bound_vars cinsert_location)
    | CERename (nsenv, cexpr1, cexpr2) ->
	(free_var_aux bound_vars cexpr1) @ (free_var_aux bound_vars cexpr2)	
    | CEReplace (vof, cexpr1, cexpr2) ->
	(free_var_aux bound_vars cexpr1) @ (free_var_aux bound_vars cexpr2)	
    | CESnap (_,ce) ->
	free_var_aux bound_vars ce
    | CELetvar (_,v,ce1,ce2) ->
        (free_var_aux bound_vars ce1) @ (free_var_aux (v::bound_vars) ce2)
    | CESet (_,ce) ->
        free_var_aux bound_vars ce

  with
  | exn -> raise(Error.error_with_file_location fi exn)

(* Auxiliary function used in the free_var_aux judgment for case clauses *)
	
and free_vars_in_cases bound_vars cases =
  match cases with
  | [] -> []
  | (_,ovn,case_cexpr) :: other_cases ->
      (match ovn with
      |	None ->
	  (free_var_aux bound_vars case_cexpr) 
      |	Some vname ->
	  (free_var_aux (vname::bound_vars) case_cexpr))
      @ (free_vars_in_cases bound_vars other_cases) 

and free_vars_in_order_spec_list bound_vars ssl =
  match ssl with
  | [] ->  []
  | (cexpr,_,_) :: ssl' ->
      (free_var_aux bound_vars cexpr)
      @ (free_vars_in_order_spec_list bound_vars ssl')

and free_vars_in_insert_location bound_vars cinsert_location =
  match cinsert_location with
  | CUAsLastInto ce1
  | CUAsFirstInto ce1
  | CUInto ce1
  | CUAfter ce1
  | CUBefore ce1 ->
      (free_var_aux bound_vars ce1)

let free_variables ce =
  free_var_aux [] ce

(***************************************************)
(* Substitute Expr1 for Variable in Expr2          *)
(*     Expr2 [ Expr1 / Variable ]                  *)
(* as long as Expr2 does not capture any free      *)
(* variables in Expr1                              *)
(* returns (Expr2', Changed) where Changed is true *)
(* if Expr1 was substituted for Variable in Expr2  *)
(***************************************************)

(* Mary -- why isn't this function using the generic tree walker? *)
(*         ---> because the modifications made here are scattered 
                around the AST, so locally operating rewrites are
                not a good solution ...                           *)

let substitute_var ce1 vname fv ce2 =
  let rec substitute_var_aux ce1 ce2 = 
    let ann = ce2.pcexpr_annot in
    let eh = ce2.pcexpr_origin in
    let loc = ce2.pcexpr_loc in
    match ce2.pcexpr_desc with
    | CEUnordered cexpr ->
	let (cexpr',changed) = substitute_var_aux ce1 cexpr in
	(fmkacexpr (CEUnordered cexpr') ann eh loc, changed)
    | CEOrdered cexpr ->
	let (cexpr',changed) = substitute_var_aux ce1 cexpr in
	(fmkacexpr (CEOrdered cexpr') ann eh loc, changed)
    | CEFLWOR (fl_clauses,where_clause,order_by_clause,return_clause) ->
	(* 1. process the fl clauses firsts *)
	begin
	  let first_clause,flwor_rest = get_first_fl_clause ce2 in
	  match first_clause with
	  | None ->
	      let (where_clause',where_changed) =
		match where_clause with
		| None -> (None,false)
		| Some cexpr ->
		    let (cexpr',changed) = substitute_var_aux ce1 cexpr in
		    (Some cexpr',changed)
	      in
	      let (order_by_clause', order_by_changed) =
		match order_by_clause with
		| None -> (None,false)
		| Some (stablekind,order_spec_list,osig) ->
		    let apply_one_order_spec (cexpr,sortkind,emptysortkind) =
		      let (new_cexpr, changed) = substitute_var_aux ce1 cexpr in
		      ((new_cexpr,sortkind,emptysortkind),changed)
		    in
		    let order_spec_list1 = List.map apply_one_order_spec order_spec_list in
		    let (order_spec_list2,changed2) = List.split order_spec_list1 in
		    let changed_bool_value = List.mem true changed2 in 
		    (Some (stablekind, order_spec_list2,osig), changed_bool_value)
	      in
	      let (return_clause',return_changed) = substitute_var_aux ce1 return_clause in
	      (fmkacexpr (CEFLWOR([],where_clause',order_by_clause',return_clause')) ann eh loc, where_changed || order_by_changed || return_changed)
	  | Some (CELET (odt,vname',cexpr1)) ->
	      let (cexpr1', changed1) = (substitute_var_aux ce1 cexpr1) in
	      let fl_clause = CELET (odt, vname', cexpr1') in
	      if (List.mem vname' fv)
	      then
		(add_first_fl_clause fl_clause flwor_rest, changed1)
	      else
		let (flwor_rest', changed2) = substitute_var_aux ce1 flwor_rest in
		(add_first_fl_clause fl_clause flwor_rest', changed1 || changed2)
	  | Some (CEFOR (odt,vname',None,cexpr1)) ->
	      let (cexpr1', changed1) = (substitute_var_aux ce1 cexpr1) in
	      let fl_clause = CEFOR (odt, vname', None, cexpr1') in
	      if (List.mem vname' fv)
	      then
		(add_first_fl_clause fl_clause flwor_rest, changed1)
	      else
		let (flwor_rest', changed2) = substitute_var_aux ce1 flwor_rest in
		(add_first_fl_clause fl_clause flwor_rest', changed1 || changed2)
	  | Some (CEFOR (odt,vname',Some vname'',cexpr1)) ->
	      let (cexpr1', changed1) = (substitute_var_aux ce1 cexpr1) in
	      let fl_clause = CEFOR (odt, vname', Some vname'', cexpr1') in
	      if (List.mem vname' fv) || (List.mem vname'' fv)
	      then
		(add_first_fl_clause fl_clause flwor_rest, changed1)
	      else
		let (flwor_rest', changed2) = substitute_var_aux ce1 flwor_rest in
		(add_first_fl_clause fl_clause flwor_rest', changed1 || changed2)
	end

    | CEIf (cexpr1, cexpr2, cexpr3) ->
	let (cexpr1', changed1) = substitute_var_aux ce1 cexpr1 in
	let (cexpr2', changed2) = substitute_var_aux ce1 cexpr2 in
	let (cexpr3', changed3) = substitute_var_aux ce1 cexpr3 in
	(fmkacexpr (CEIf (cexpr1', cexpr2', cexpr3')) ann eh loc, changed1 || changed2 || changed3)
	  
    | CEWhile (cexpr1, cexpr2) ->
	let (cexpr1', changed1) = substitute_var_aux ce1 cexpr1 in
	let (cexpr2', changed2) = substitute_var_aux ce1 cexpr2 in
	(fmkacexpr (CEWhile (cexpr1', cexpr2')) ann eh loc, changed1 || changed2)
	  
    | CETypeswitch (cexpr, cases) ->
	let (cexpr', changed') = (substitute_var_aux ce1 cexpr) in 
	let (new_cases, changed_bool) = (substitute_in_cases_var ce1 cases) in
	(fmkacexpr (CETypeswitch (cexpr', new_cases)) ann eh loc, changed' || changed_bool) 

    | CEVar vname' -> 
	if (vname = vname') 
	then (ce1, true)
	else (ce2, false)

    | CEScalar _ ->
	(ce2, false)

    | CEProtoValue _ ->
	(ce2, false)

    | CEText _ -> 
	(ce2, false)

    | CECharRef _ -> 
	(ce2, false)

    | CETextComputed cexpr ->
	let (cexpr', changed') = substitute_var_aux ce1 cexpr in
	(fmkacexpr (CETextComputed cexpr') ann eh loc, changed')

    | CEPI _ -> 
	(ce2, false)

    | CEPIComputed (cexpr1, cexpr2) ->
	let (cexpr1', changed1) = substitute_var_aux ce1 cexpr1 in
	let (cexpr2', changed2) = substitute_var_aux ce1 cexpr2 in
	(fmkacexpr (CEPIComputed(cexpr1', cexpr2')) ann eh loc, changed1 || changed2)

    | CEComment _ -> 
	(ce2, false)

    | CECommentComputed cexpr -> 
	let (cexpr', changed') = substitute_var_aux ce1 cexpr in
	(fmkacexpr (CECommentComputed cexpr') ann eh loc, changed')

    | CEDocument cexpr ->
	let (cexpr', changed') = substitute_var_aux ce1 cexpr in
	(fmkacexpr (CEDocument cexpr') ann eh loc, changed')

    | CECall (fname, arguments, sign, upd, selfrecur) ->
	let arguments' = List.map (substitute_var_aux ce1) arguments in
	let arguments1 = List.map (fun (arg, changed) -> arg) arguments' in
	let changed1 = List.map (fun (arg, changed) -> changed) arguments' in
	(fmkacexpr (CECall (fname, arguments1, sign, upd, selfrecur)) ann eh loc, List.mem true changed1) 

    | CEOverloadedCall (fname, arguments, sigs) ->
	let arguments' = List.map (substitute_var_aux ce1) arguments in
	let arguments1 = List.map (fun (arg, changed) -> arg) arguments' in
	let changed1 = List.map (fun (arg, changed) -> changed) arguments' in
	(fmkacexpr (CEOverloadedCall (fname, arguments1, sigs)) ann eh loc, List.mem true changed1) 
    | CELetServerImplement (nc1, nc2, cexpr1, cexpr2) ->
	let (ce1', changed1) = substitute_var_aux ce1 cexpr1 in
	let (ce2', changed2) = substitute_var_aux ce1 cexpr2 in
	(fmkacexpr (CELetServerImplement(nc1,nc2,ce1',ce2')) ann eh loc, changed1 || changed2)
    | CEExecute (async, ncname, uri, hostport, cexpr) ->
	let (hostport',changed1) = substitute_var_aux ce1 hostport in
	let (cexpr',changed2) = substitute_var_aux ce1 cexpr in
	(fmkacexpr (CEExecute (async, ncname, uri, hostport', cexpr')) ann eh loc, changed1 || changed2) 

    | CEForServerClose (nc1, uri, cexpr1) ->
	let (ce1', changed1) = substitute_var_aux ce1 cexpr1 in
	(fmkacexpr (CEForServerClose(nc1,uri,ce1')) ann eh loc, changed1)

    | CEEvalClosure (cexpr1) ->
	let (ce1', changed1) = substitute_var_aux ce1 cexpr1 in
	(fmkacexpr (CEEvalClosure(ce1')) ann eh loc, changed1)

    | CESeq (cexpr1, cexpr2) ->
	let (cexpr1', changed1) = substitute_var_aux ce1 cexpr1 in
	let (cexpr2', changed2) = substitute_var_aux ce1 cexpr2 in
	(fmkacexpr (CESeq(cexpr1', cexpr2')) ann eh loc, changed1 || changed2)

    | CEImperativeSeq (cexpr1, cexpr2) ->
	let (cexpr1', changed1) = substitute_var_aux ce1 cexpr1 in
	let (cexpr2', changed2) = substitute_var_aux ce1 cexpr2 in
	(fmkacexpr (CEImperativeSeq(cexpr1', cexpr2')) ann eh loc, changed1 || changed2)

    | CEEmpty -> 
	(ce2, false)

    | CEElem (relem_symbol, nsenv, cexprlist) ->
	let (cexprlist', changed') = List.fold_right
	    (fun cexpr (cexprlist, changed) -> 
	      let (cexpr', changed') = substitute_var_aux ce1 cexpr in
	      (cexpr' :: cexprlist, changed || changed')) cexprlist ([], false) 
	in
	(fmkacexpr (CEElem (relem_symbol, nsenv, cexprlist')) ann eh loc, changed')

    | CEAnyElem (cexpr1, nsenv1, nsenv2, cexpr2) ->  
	let (cexpr1', changed1) = substitute_var_aux ce1 cexpr1 in
	let (cexpr2', changed2) = substitute_var_aux ce1 cexpr2 in
	(fmkacexpr (CEAnyElem (cexpr1', nsenv1, nsenv2, cexpr2')) ann eh loc, changed1 || changed2)

    | CEAttr (rattr_symbol, cexprlist)->
	let (cexprlist', changed') = List.fold_right
	    (fun cexpr (cexprlist, changed) -> 
	      let (cexpr', changed') = substitute_var_aux ce1 cexpr in
	      (cexpr' :: cexprlist, changed || changed')) cexprlist ([], false) 
	in
	(fmkacexpr (CEAttr (rattr_symbol, cexprlist')) ann eh loc, changed')

    | CEAnyAttr (cexpr1, nsenv, cexpr2) ->
	let (cexpr1', changed1) = substitute_var_aux ce1 cexpr1 in
	let (cexpr2', changed2) = substitute_var_aux ce1 cexpr2 in
	(fmkacexpr (CEAnyAttr (cexpr1', nsenv, cexpr2')) ann eh loc, changed1 || changed2)

    | CEError arguments ->
	let arguments' = List.map (substitute_var_aux ce1) arguments in
	let arguments1 = List.map (fun (arg, changed) -> arg) arguments' in
	let changed1 = List.map (fun (arg, changed) -> changed) arguments' in
	(fmkacexpr (CEError (arguments1)) ann eh loc, List.mem true changed1) 

    | CETreat (cexpr, model) ->
	let (cexpr', changed') = substitute_var_aux ce1 cexpr in
	(fmkacexpr (CETreat (cexpr', model)) ann eh loc, changed')

    | CECast (cexpr, nsenv, model) ->
	let (cexpr', changed') = substitute_var_aux ce1 cexpr in
	(fmkacexpr (CECast (cexpr', nsenv, model)) ann eh loc, changed')

    | CECastable (cexpr, nsenv, model) ->
	let (cexpr', changed') = substitute_var_aux ce1 cexpr in
	(fmkacexpr (CECastable (cexpr', nsenv, model)) ann eh loc, changed')

    | CEValidate (vmode,cexpr) ->
	let (cexpr', changed') = substitute_var_aux ce1 cexpr in
	(fmkacexpr (CEValidate (vmode,cexpr')) ann eh loc, changed')

    | CEForwardAxis (v, axis, cnode_test) -> 
	if (rqname_equal vname v)
	then
	  match ce1.pcexpr_desc with
	  | CEVar newname ->
	      (fmkacexpr (CEForwardAxis (newname, axis, cnode_test)) ann eh loc, true)
	  | _ -> 
	      (fmkacexpr (CEFLWOR ([CELET (None, vname, ce1)],None,None,ce2)) ann eh loc, true)
	else
	  (ce2,false)

    | CEReverseAxis (v, axis, cnode_test) -> 
	if (rqname_equal vname v)
	then
	  match ce1.pcexpr_desc with
	  | CEVar newname ->
	      (fmkacexpr (CEReverseAxis (newname, axis, cnode_test)) ann eh loc, true)
	  | _ -> 
	      (fmkacexpr (CEFLWOR ([CELET (None, vname, ce1)],None,None,ce2)) ann eh loc, true)
	else
	  (ce2,false)

    | CESome (odt, vname', cexpr1, cexpr2) ->
	let (cexpr1', changed1) = (substitute_var_aux ce1 cexpr1) in
	if (List.mem vname' fv)
	then 
          (fmkacexpr (CESome (odt, vname', cexpr1', cexpr2)) ann eh loc, changed1)
	else 
	  let (cexpr2', changed2) = (substitute_var_aux ce1 cexpr2) in 
          (fmkacexpr (CESome (odt, vname', cexpr1', cexpr2')) ann eh loc, changed1 || changed2)
    | CEEvery (odt, vname', cexpr1, cexpr2) ->
	let (cexpr1', changed1) = (substitute_var_aux ce1 cexpr1) in
	if (List.mem vname' fv)
	then 
          (fmkacexpr (CEEvery (odt, vname', cexpr1', cexpr2)) ann eh loc, changed1)
	else 
	  let (cexpr2', changed2) = (substitute_var_aux ce1 cexpr2) in 
          (fmkacexpr (CEEvery (odt, vname', cexpr1', cexpr2')) ann eh loc, changed1 || changed2)
    | CECopy cexpr1 ->
	let (cexpr1', changed1) = (substitute_var_aux ce1 cexpr1) in
	(fmkacexpr (CECopy cexpr1') ann eh loc, changed1)
    | CEDelete cexpr1 ->
	let (cexpr1', changed1) = (substitute_var_aux ce1 cexpr1) in
	(fmkacexpr (CEDelete cexpr1') ann eh loc, changed1)
    | CEInsert (cexpr1, cinsert_location) ->
	let (cexpr1', changed1) = (substitute_var_aux ce1 cexpr1) in 
	let (cinsert_location', changed2) =
	  (substitute_in_insert_location_var ce1 cinsert_location)
	in
	(fmkacexpr (CEInsert (cexpr1', cinsert_location')) ann eh loc, changed1 || changed2)
    | CERename (nsenv,cexpr1,cexpr2) ->
	let (cexpr1', changed1) = (substitute_var_aux ce1 cexpr1) in
	let (cexpr2', changed2) = (substitute_var_aux ce1 cexpr2) in
	(fmkacexpr (CERename (nsenv,cexpr1',cexpr2')) ann eh loc, changed1 || changed2)
    | CEReplace (vof, cexpr1, cexpr2) ->
	let (cexpr1', changed1) = (substitute_var_aux ce1 cexpr1) in
	let (cexpr2', changed2) = (substitute_var_aux ce1 cexpr2) in
	(fmkacexpr (CEReplace (vof,cexpr1',cexpr2')) ann eh loc, changed1 || changed2)
    | CESnap (sm,cexpr) ->
	let (cexpr, changed) = substitute_var_aux ce1 cexpr in
	(fmkacexpr (CESnap (sm,cexpr)) ann eh loc, changed)
    | CELetvar (codt,vn,cexpr1, cexpr2) ->
	    let (cexpr1', changed1) = (substitute_var_aux ce1 cexpr1) in
          if (List.mem vn fv) then
            (fmkacexpr (CELetvar (codt,vn,cexpr1',cexpr2)) ann eh loc, changed1)
          else
	        let (cexpr2', changed2) = (substitute_var_aux ce1 cexpr2) in
	          (fmkacexpr (CELetvar (codt,vn,cexpr1',cexpr2')) ann eh loc, changed1 || changed2)                
    | CESet (vn,cexpr) ->
	    let (cexpr', changed) = (substitute_var_aux ce1 cexpr) in
	      (fmkacexpr (CESet (vn,cexpr')) ann eh loc, changed)
            
  and substitute_in_cases_var ce1 cases =
    let one_case ce1 (pattern, ovn, case_cexpr) = 
      let (new_case_cexpr, changed) =
	match ovn with 
	| None -> substitute_var_aux ce1 case_cexpr
	| Some vn ->
	    if (List.mem vn fv) then (case_cexpr, false)
	    else substitute_var_aux ce1 case_cexpr
      in ((pattern, ovn, new_case_cexpr), changed)
    in
    let substituted_cases = List.map (one_case ce1) cases in 
    let aggregate_case ((pattern, ovn, new_case_cexpr), changed) = (pattern, ovn, new_case_cexpr) in
    let aggregate_bool ((pattern, ovn, new_case_cexpr), changed) = changed in 
    let new_cases = List.map aggregate_case substituted_cases in 
    let new_changed_value = List.map aggregate_bool substituted_cases in
    let changed_bool_value = List.mem true new_changed_value in
    (new_cases, changed_bool_value)
      
  and substitute_in_insert_location_var ce1 cinsert_location =
    match cinsert_location with
    | CUAsLastInto cexpr1 ->
	let (cexpr1', changed1) = (substitute_var_aux ce1 cexpr1) in 
	(CUAsLastInto cexpr1',changed1)
    | CUAsFirstInto cexpr1 ->
	let (cexpr1', changed1) = (substitute_var_aux ce1 cexpr1) in 
	(CUAsFirstInto cexpr1',changed1)
    | CUInto cexpr1 ->
	let (cexpr1', changed1) = (substitute_var_aux ce1 cexpr1) in 
	(CUInto cexpr1',changed1)
    | CUAfter cexpr1 ->
	let (cexpr1', changed1) = (substitute_var_aux ce1 cexpr1) in 
	(CUAfter cexpr1',changed1)
    | CUBefore cexpr1 ->
	let (cexpr1', changed1) = (substitute_var_aux ce1 cexpr1) in 
	(CUBefore cexpr1',changed1)
  in substitute_var_aux ce1 ce2


let force_substitute_var ce1 vname ce2 =
  let fv = [ vname ] in
  substitute_var ce1 vname fv ce2

let safe_substitute_var ce1 vname ce2 =
  let fv = vname :: (free_variables ce2) in 
  substitute_var ce1 vname fv ce2

