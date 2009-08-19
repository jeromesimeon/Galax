(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_dxq.ml,v 1.12 2007/10/16 01:25:35 mff Exp $ *)

(* Module: Optimization_dxq
   Description:
     This module implements the optimization rewrite rules for
     Distributed XQuery.
*)


open Error
open Namespace_builtin

open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_algebra_ast_annotation_util

open Compile_annotate
open Debug

open Processing_context
open Compile_context
open Logical_algebra_types

open Optimization_util
open Optimization_walker

(* An algebraic plan is "closed" if:

   o It does not call any LOCAL user-defined functions.
 
   HACK ALERT:  

   Calls to external user-defined functions, whether declared
   explicitly or imported in from a module, are mapped to CallBuiltIn
   operators.  So the only way to distinguish between genuinely
   builtin functions and external user-defined functions is by
   examining the function's namespace URI.  Blech.

 *)
let is_closed_algop algop = 
  let user_fns = calls_user_defined_functions algop in
  (List.iter (fun n -> print_default_debug ((Namespace_names.prefixed_string_of_rqname n)^"\n")) user_fns);
  List.length(user_fns) = 0

(* NOTES on invariants from Optimization_rewrite:

   It is the responsibility of each rule to recompute the annotations
   for the tree - that is, after application a rule should always
   leave a valid tree.

   Enforcement: 
   - A rule takes an algop -> algop * boolean (changed)
   rules must proved an annotation walker which is of the form:
   - 'info -> algop -> algop * boolean (this annotation finished)
*)

(*****************************************************
 * Lifting Execute rule over constructor

   AOEElem [qname, nsenv] (
    Execute(host-expr, port-expr, Expr),
   )
  ==
   Execute(host-expr, port-expr, 
    AOEElem [qname, nsenv] (Expr)
   )

  <foobar> { Expr } </foobar>

  element { Expr1 } { Expr2 }
 *****************************************************)

(*****************************************************
 * Lifting Execute rule

   CallBuiltIn [any-builtin-fn] (
    ..., Execute(host-expr, port-expr, Expr), ....
   )
  ==
   Execute(host-expr, port-expr, 
    CallBuiltIn [any-builtin-fn] (Expr)
   )
 *****************************************************)

let lift_execute_over_builtin_function_rewrite comp_ctxt op =
  let fail = op, false in
  let eh = op.palgop_expr_origin in
  let fi = op.palgop_expr_loc in
  match op.palgop_expr_name with
  | AOECallBuiltIn ((name,n),st1,st2, upd) ->
      begin
          (* 
	     General rule for pushing built-in function call through
	     Execute, assuming that only one argument to function call
	     is remote.

	     fn:foo (E_0, ... E_k-1, exec { host }{Expr_k}, ... E_n) 
	     =======
	     exec { host } (fn:foo (E_0, ... E_k-1, Expr_k, ... E_n)) =>
	  *)
	let match_execute tj_op = 
	  match tj_op.palgop_expr_name with
	  | AOEExecute (ncname, uri) -> true
	  | _ -> false
	in
	let rec find_execute args idx  = 
	  if idx = n then [ ]
	  else if (match_execute args.(idx)) then idx :: (find_execute args (idx+1)) else (find_execute args (idx+1))
	in
	let builtin_fn_args = (access_manysub op.psub_expression) in
	let execute_args = find_execute builtin_fn_args 0 in
	if (List.length execute_args) = 1 then (* Do the rewrite *)
	  begin
		(* Extract arguments from Execute *)
	    let pos_of_execute = List.hd execute_args in
	    let execute_expr =  ((access_manysub op.psub_expression).(pos_of_execute)) in
	    let (ncname, uri) = 
	      (match execute_expr.palgop_expr_name with 
	      | AOEExecute (ncname, uri) -> (ncname,uri)
	      |	_  -> raise (Query(Internal_Error("In Optimization_dxq: expected remote-execute")))) in
	    let execute_sub_exprs = access_manysub execute_expr.psub_expression in
	    let hostport = execute_sub_exprs.(0) in
	    let expr = execute_sub_exprs.(1) in
		(* Create new CallBuiltIn *)
	    let new_args = Array.mapi (fun idx arg -> if (idx = pos_of_execute) then expr else arg) builtin_fn_args in
	    let pushed_expr = logical_aalgop_mkop (AOECallBuiltIn((name,n), st1, st2, upd)) (ManySub new_args) NoSub None eh fi in
		(* Create new Execute *)
	    let new_expr = logical_aalgop_mkop (AOEExecute (ncname, uri)) (TwoSub (hostport, pushed_expr)) NoSub None eh fi in
	    new_expr, true
	  end
        else 
	  fail
      end
  | _ -> fail
	
(**************************************************************************
 * Pushing MapFromItem through "dependent" Execute Rule
 * - Metric: reduce messages 
 *
 * MapFromItem {Execute("host", port, Expr)} (IndepExprs)
 * ==
 * Execute("host", port, MapFromItem {Expr} (IndepExprs))
 * When ...conditions?...
***************************************************************************)

let push_map_from_item_rewrite comp_ctxt op =
  let fail = op, false in
  let eh = op.palgop_expr_origin in
  let fi = op.palgop_expr_loc in
  match op.palgop_expr_name with
  | AOEMapFromItem name ->
      begin
	let mapfrom_indep =  access_onesub op.psub_expression in
	let mapfrom_dep = access_onesub op.pdep_sub_expression in
	match mapfrom_dep.palgop_expr_name with
	| AOEExecute (ncname, uri) ->
	    begin
	      (* Check that MapFromItem's independent sub expression is closed *)
	      if (is_closed_algop mapfrom_indep) then
		let execute_sub_exprs = access_manysub mapfrom_dep.psub_expression in
		let hostport = execute_sub_exprs.(0) in
		let expr = execute_sub_exprs.(1) in
		let pushed_expr = logical_aalgop_mkop (AOEMapFromItem name) (OneSub mapfrom_indep) (OneSub expr) None eh fi in
		let new_expr = logical_aalgop_mkop (AOEExecute (ncname, uri)) (TwoSub (hostport, pushed_expr)) NoSub None eh fi in
		new_expr, true
	      else fail
	    end
	| _ -> fail
      end
  | _ -> fail
	

(**************************************************************************
 * Pushing MapFromItem through "independent" Execute Rule
 * - Metric: reduce data (assuming selections in dependent sub-expression)
 *
 * MapFromItem {DepExpr} (Execute("host", port, Expr))
 * ==
 * Execute("host", port, MapFromItem {DepExpr} (Expr))
 * When ...conditions?...
***************************************************************************)

let push_map_from_item_indep_rewrite comp_ctxt op =
  let fail = op, false in
  let eh = op.palgop_expr_origin in
  let fi = op.palgop_expr_loc in
  match op.palgop_expr_name with
  | AOEMapFromItem name ->
      begin
	let mapfrom_indep =  access_onesub op.psub_expression in
	let mapfrom_dep = access_onesub op.pdep_sub_expression in
	match mapfrom_indep.palgop_expr_name with
	| AOEExecute (ncname, uri) ->
	    begin
	      (* Check that MapFromItem's dependent sub expression is closed *)
	      if (is_closed_algop mapfrom_dep) then
		let execute_sub_exprs = access_manysub mapfrom_indep.psub_expression in
		let hostport = execute_sub_exprs.(0) in
		let expr = execute_sub_exprs.(1) in
		let pushed_expr = logical_aalgop_mkop (AOEMapFromItem name) (OneSub expr) (OneSub mapfrom_dep) None eh fi in
		let new_expr = logical_aalgop_mkop (AOEExecute (ncname,uri)) (TwoSub (hostport, pushed_expr)) NoSub None eh fi in
		new_expr, true
	      else (print_default_debug "MapFromItem Dep is NOT closed\n"; fail)
	    end
	| _ -> fail
      end
  | _ -> fail
	

(**************************************************************************
 * Pushing MapToItem through "dependent" Execute Rule
 * - Metric: reduce messages 
 *
 * MapToItem {Execute("host", port, Expr)} (IndepExprs)
 * ==
 * Execute("host", port, MapToItem {Expr} (IndepExprs))
 * When ...conditions?...

   MapToItem {Execute("host", port, Expr)} (IndepExprs) 
==
   Execute("host", port, MapToItem{Expr}(IndepExprs))
                  
***************************************************************************)

let push_map_to_item_rewrite comp_ctxt op =
  let fail = op, false in
  let eh = op.palgop_expr_origin in
  let fi = op.palgop_expr_loc in
  match op.palgop_expr_name with
  | AOEMapToItem ->
      begin
	let mapto_indep =  access_onesub op.psub_expression in
	let mapto_dep = access_onesub op.pdep_sub_expression in
	match mapto_dep.palgop_expr_name with
	| AOEExecute (ncname, uri) ->
	    begin
   	      (* Check that MapToItem's independent sub expression is closed *)
	      if (is_closed_algop mapto_indep) then
		let execute_sub_exprs = access_manysub mapto_dep.psub_expression in
		let hostport = execute_sub_exprs.(0) in
		let expr = execute_sub_exprs.(1) in
		let pushed_expr = logical_aalgop_mkop AOEMapToItem (OneSub mapto_indep) (OneSub expr) None eh fi in
		let new_expr = logical_aalgop_mkop (AOEExecute(ncname,uri)) (TwoSub (hostport, pushed_expr)) NoSub None eh fi in
		new_expr, true
	      else fail
	    end
	| _ -> fail
      end
  | _ -> fail
	

(**************************************************************************
 * Pushing MapToItem through "independent" Execute Rule
 * - Metric: reduce data (assuming selections in dependent sub-expression)
 *
 * MapToItem {DepExpr} (Execute(host, port, Expr))
 * ==
 * Execute(host, port, MapToItem {DepExpr} (Expr))
 * When ...conditions?...
***************************************************************************)

let push_map_to_item_indep_rewrite comp_ctxt op =
  let fail = op, false in
  let eh = op.palgop_expr_origin in
  let fi = op.palgop_expr_loc in
  match op.palgop_expr_name with
  | AOEMapToItem ->
      begin
	let mapto_indep =  access_onesub op.psub_expression in
	let mapto_dep = access_onesub op.pdep_sub_expression in
	match mapto_indep.palgop_expr_name with
	| AOEExecute (ncname,uri)->
	    begin
	      (* Check that MapToItem's dependent sub expression is closed *)
	      if (is_closed_algop mapto_dep) then
		let execute_sub_exprs = access_manysub mapto_indep.psub_expression in
		let hostport = execute_sub_exprs.(0) in
		let expr = execute_sub_exprs.(1) in
		let pushed_expr = logical_aalgop_mkop AOEMapToItem (OneSub expr) (OneSub mapto_dep) None eh fi in
		let new_expr = logical_aalgop_mkop (AOEExecute(ncname,uri)) (TwoSub (hostport, pushed_expr)) NoSub None eh fi in
		new_expr, true
	      else fail
	    end
	| _ -> fail
      end
  | _ -> fail
	

(**************************************************************************
 * Push Join through two Executes at same location
 * - Metric: reduce data 
 *
 * Join {DepExpr} 
   (Execute {host1, port1, Expr1},
    Execute {host2, port2, Expr2})
   host1 ~~ host2 and port1 ~~ port2
 * ==
 * Execute {host, port, 
     Join {DepExpr} (Expr1, Expr2)

 * When hosts and ports are same expression
 **************************************************************************)
let lift_execute_over_join_rewrite comp_ctxt op =
  let fail = op, false in
  let eh = op.palgop_expr_origin in
  let fi = op.palgop_expr_loc in
  match op.palgop_expr_name with
  | AOEJoin join_pred ->
      begin
	let join_ca = op.compile_annotations in 
	let join_dep = access_manysub op.pdep_sub_expression in
	let (join_indep1, join_indep2) =  access_twosub op.psub_expression in
	match (join_indep1.palgop_expr_name, join_indep2.palgop_expr_name) with
	| (AOEExecute (ncname1,uri1), AOEExecute(ncname2,uri2)) when (ncname1 = ncname2 && uri1 = uri2) ->
	    begin
	      (* Check that Join's dependent sub expressions are closed *)
	      if (List.for_all is_closed_algop (Array.to_list join_dep)) then
		let exec1_indep = access_manysub join_indep1.psub_expression in
		let hostport1 = exec1_indep.(0) in
		let expr1 = exec1_indep.(1) in

		let exec2_indep = access_manysub join_indep2.psub_expression in
		(* hostport2 NOT USED? - Jerome *)
		(* let hostport2 = exec2_indep.(0) in *)
		let expr2 = exec2_indep.(1) in

		    (* This lifting should be factorized into a generic function *)
		    (* MISSING: CHECK THAT HOSTS AND PORTS MATCH!!!! DUH! *)
		let pushed_join = logical_aalgop_mkop (AOEJoin join_pred) (TwoSub (expr1, expr2)) (ManySub join_dep) join_ca eh fi in
	            (* What should the compile annotation be?! *)
		let fused_execute = logical_aalgop_mkop (AOEExecute(ncname1, uri1)) (TwoSub (hostport1, pushed_join)) NoSub None eh fi in
		fused_execute, true
	      else fail
	    end
	| _ -> fail
      end
  | _ -> fail

(************************************)
(* Phase 6 (a2) Lift Execute - Doug *)
(************************************)
let lift_execute_rules = [
  (lift_execute_over_builtin_function_rewrite), "Lifting the Execute operator over built-in function call"; 
  (push_map_from_item_rewrite), "Push MapFromItem through dependent Execute";
  (push_map_from_item_indep_rewrite), "Push MapFromItem through independent Execute";
  (push_map_to_item_rewrite), "Push MapToItem through dependent Execute";
  (push_map_to_item_indep_rewrite), "Push MapToItem through independent Execute";
  (lift_execute_over_join_rewrite), "Lift Executes over Join";
]

