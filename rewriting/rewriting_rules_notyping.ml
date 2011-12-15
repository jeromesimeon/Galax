(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: rewriting_rules_notyping.ml,v 1.57 2008/03/12 22:30:58 simeon Exp $ *)

(* Module: Rewriting_rules_notyping
   Description:
     This module contains the rewriting rules that are independant of
     typing information.
*)

open Error

open Namespace_names
open Namespace_builtin

open Datatypes

open Dm_atomic
open Dm

open Norm_util

open Xquery_common_ast
open Xquery_common_ast_util

open Xquery_core_ast
open Xquery_core_ast_util
open Xquery_core_ast_annotation

open Ast_walker_rewrite_context
open Ast_walker_rewrite

open Processing_context

open Rewriting_judgments

open Schema_builtin
open Schema_util
open Print_top

let atomicFalse = BooleanLiteral false
let atomicTrue = BooleanLiteral true

type clause_rewriter =
  Typing_context.static_context Ast_walker_rewrite_context.rewrite_context ->
  Xquery_core_ast.acfl_expr list ->
  Xquery_core_ast.acfl_expr ->
  Xquery_core_ast.acexpr ->
  Xquery_core_ast.acfl_expr list * Xquery_core_ast.acexpr * bool

(* 
  Generic rewrite rule for function calls.
*)
let call_rewrite (fname, arity, rewrite_fn) rewrite_ctxt ce =
  match ce.pcexpr_desc with
  | CECall (fname', arguments, sign, _, _) ->
      if (fname' = fname) then
	if (arity = List.length arguments) then rewrite_fn rewrite_ctxt ce arguments
	else raise (Norm_util.incorrect_arg_count fname (List.length arguments) arity)
      else raise Not_applied
  | _ -> raise Not_applied

(*********************)
(* IF SIMPLIFICATION *)
(*********************)

(*
   if cexpr1 then cexpr2 else cexpr3      if cexpr1 then cexpr2 else cexpr3  
   cexpr1 := true		          cexpr1 := false		      
   ---------------------------------      ---------------------------------  
   cexpr2			          cexpr3                             
*)
let if_rewrite (rewrite_ctxt) (ce : acexpr) =
  match ce.pcexpr_desc with
  | CEIf (cexpr1, cexpr2, cexpr3) ->
      begin
        match cexpr1.pcexpr_desc with
	| CEScalar (BooleanLiteral b)  ->
	    ((if b then cexpr2 else cexpr3), true)
	|  _ ->
	    raise Not_applied
      end
  | _ ->
      raise Not_applied

(****************)
(* SNAP REMOVAL *)
(****************)

(*
   snap { cexpr }
   cexpr pure
   --------------
   cexpr
*)
let snap_removal (rewrite_ctxt) (ce : acexpr) =
  match ce.pcexpr_desc with
  | CESnap (_,cexpr) ->
      begin
	if side_effect_free_judge cexpr
	then (cexpr,true)
        else raise Not_applied
      end
  | _ ->
      raise Not_applied


(***************************)
(* REMOVE Copy FROM Insert *)
(***************************)

(*
   do insert { copy ce } into t
   ce is an elem. constr.
   -----------------------------
   do insert { copy ce } into t
*)
let insert_rewrite (rewrite_ctxt) (ce : acexpr) =
  match ce.pcexpr_desc with
    | CEInsert (s, t) ->
        begin
          match s.pcexpr_desc with
            | CECopy cp ->
                begin
                  match cp.pcexpr_desc with 
                    | CEElem _ -> (fmkcexpr(CEInsert (cp,t)) ce.pcexpr_origin ce.pcexpr_loc, true)
                    | _ -> raise Not_applied
                end
            | _ ->  raise Not_applied
        end
    | _ -> raise Not_applied


(*********************)
(* FUNCTION INLINING *)
(*********************)

(* Function inlining is implemented by applying the standard  	*)
(* rule of replacing formal arguments by let-bound variables. 	*)
(*                                                            	*)
(* declare function foo ($x1 as T1,$x2 as T2) =  { BodyExpr };  *)
(*                                                            	*)
(*   foo(Expr1, Expr2);                                    	*)
(*   foo not recursive                                        	*)
(*   ==>                                                      	*)
(*   let $x1 as T1 := Expr1, $x2 as T2 := Expr2 return BodyExpr *)

(*
let function_inlining_rewrite rewrite_ctxt ce =
  let stat_ctxt = get_context rewrite_ctxt in
  let proc_ctxt = Typing_context.processing_context_from_stat_context stat_ctxt in
  let norm_context = Typing_context.norm_context_from_stat_context stat_ctxt in
  let return_failure () = raise Not_applied in
  if proc_ctxt.inline_functions then
    match ce.pcexpr_desc with
    | CECall (cfname,arguments,types,output_type,_) ->
	let arity = List.length arguments in
	if not(Rewriting_judgments.is_recursive_function norm_context (cfname,arity))
	then
	  begin (* Replace it with the body *)
	    (* Algop functions *)
	    let eh = ce.pcexpr_origin in
	    (* Get the function *)
	    try
	      let func_defn = Compile_context.get_body_from_norm_context norm_ctxt cfname in 
	      (* We make a deep copy of the function body, which may be
		 transformed at the call site by subsequent optimization *)
	      let typed_vars = List.combine 
		  (Array.to_list types) 
		  (Array.to_list func_defn.palgop_func_formal_args) in 
	      let argexpr_var_list = List.combine 
		  (Array.to_list (access_manysub fn_indep)) typed_vars in
	      let new_op =
		List.fold_left 
		  (fun depexpr (argexpr, type_var) ->
		    logical_aalgop_mkop (AOELet type_var) 
		      (OneSub argexpr) (OneSub depexpr) None eh fi)
		  newbody
		  argexpr_var_list 
	      in
	      let new_op =
		if (is_item_star output_type)
		then new_op
		else
  		  let newvarname = get_new_var_name () in
		  let newvar = logical_aalgop_mkop (AOEVar newvarname) NoSub NoSub None eh fi in
		  let newtypevar =
		    (Some output_type,newvarname)
		  in
		  logical_aalgop_mkop (AOELet newtypevar)
		    (OneSub new_op) (OneSub newvar) None eh fi
	      in
	      new_op, true
	    with (* External user-defined functions cannot be inlined *)
	    | Query(Undefined _) -> return_failure()
	  end
	else return_failure()
    | _ -> return_failure()
  else return_failure()
*)

(*********************)
(* LET REWRITINGS    *)
(*********************)

(*

  Rule1: let $v := Expr1 return Expr2
         Expr1 has no side effects
         used_count $v in Expr2 = 0
         ----------------------------
         Expr2
 
  Rule2: let $v := Expr1 return Expr2
         used_count $v in Expr2 => 1
         Expr2 does not shadow/capture variables free in Expr1
         -----------------------------------------------------
         Expr2 [ Expr1 / $v ]
*)

(* Note:
     The first rule uses the non-deterministic semantics allowed by
     XQuery in the sense that the optimization occurs even though the
     expression might fail.
   - Jerome *)

(***************************************************)
(* Substitute Expr1 for Variable in Expr2          *)
(*     Expr2 [ Expr1 / Variable ]                  *)
(* returns (Expr2', Changed) where Changed is true *)
(* if Expr1 was substituted for Variable in Expr2  *)
(***************************************************)

(* Note:
     Variable substitution is in Ast_walker_rewrite, so it can be used
     during normalization. It is notably used now for normalization of
     the "order by" clause.
 - Jerome
 *)
    (* Aggressive optimization, which ignores whether the
       right-hand-side of the let clause may fail.  This is allowed by
       the XQuery semantics.  If a variable is never used, then the
       type assertion may also be ignored.  If it is used one or more
       times, the assertion cannot be ignored.  *)

(* Function to apply let-clause simplification *)
let print_inlining vname =
  if Debug.join_debug()
  then
    begin
      let vns = Namespace_names.prefixed_string_of_rqname vname in
      Debug.print_join_debug ("---->>> INLINING VARIABLE $" ^ vns ^" NOW")
    end

let print_removing vname =
  if Debug.join_debug()
  then
    begin
      let vns = Namespace_names.prefixed_string_of_rqname vname in
      Debug.print_join_debug ("---->>> REMOVING VARIABLE $" ^ vns ^" NOW")
    end

let let_clause_rewrite rewrite_ctxt (cfl_clause_list) (cfl_clause) (ce) =
  let stat_ctxt = get_context rewrite_ctxt in 
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  match (cfl_clause_list, cfl_clause, ce.pcexpr_desc) with
    (* If this is the only let-clause in a FLWOR expression and the
       FLWOR includes a WHERE or ORDER-BY clause, we conservatively
       leave the let-clause in place. *)
  | ([], CELET _, CEFLWOR ([], Some _, _, _)) 
  | ([], CELET _, CEFLWOR ([], _, Some _, _)) ->
      begin 
	([cfl_clause], ce, false)
      end
  | (_, CELET (None, vname, cexpr1), CEFLWOR (other_clauses, opt_where, opt_orderby, ret_expr)) ->
      begin
	match (used_count vname ce) with
	| 0 ->
	    begin
	      print_removing vname;
	      if (side_effect_free_judge cexpr1) then 
 		match (cfl_clause_list, other_clauses, opt_where, opt_orderby) with
 		| ([], [],  None, None) -> ([], ret_expr, true)
 		| _ -> (cfl_clause_list, ce, true)
	      else (cfl_clause_list@[cfl_clause], ce, false) 
	    end
	| 1 ->
	    begin
	      if (should_inline_variable proc_ctxt ret_expr) 
	      then
		begin
		  print_inlining vname;
		  if not(rqname_equal fs_dot vname) && (side_effect_free_judge cexpr1) 
		  then
		    let (ce', changed) = safe_substitute_var cexpr1 vname ce in
 		    match (cfl_clause_list, ce'.pcexpr_desc) with
 		    | ([], CEFLWOR([], None, None, ret_expr')) -> ([], ret_expr', true)
 		    | _ -> (cfl_clause_list, ce', true)
		  else
		    (cfl_clause_list@[cfl_clause], ce, false)
		end
	      else
		(cfl_clause_list@[cfl_clause], ce, false)
	    end
	| _ -> 
	    (cfl_clause_list@[cfl_clause], ce, false)
      end
  | _ ->
      (cfl_clause_list@[cfl_clause], ce, false)

(**********************)
(* FOR SIMPLIFICATION *)
(**********************)

(*
  for $x at $i in Expr0
  where $i = Expr1
  return Expr2

  used_count $i in Expr1 = 0
  used_count $i in Expr2 = 0 
  used_count $x in Expr1 = 0
  ==
  let $x := fn:subsequence(Expr0,Expr1,1)
  return Expr2
*)

let subtype_check rewrite_ctxt actual expected = 
  let stat_ctxt = get_context rewrite_ctxt in
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  let schema = Norm_context.cxschema_from_norm_context norm_ctxt in
  Subtyping_top.is_subtype_of schema actual expected

let double_cast rewrite_ctxt (t1,t2) =
  let double = Schema_builtin.cxtype_double in
  (subtype_check rewrite_ctxt t1 double) && (subtype_check rewrite_ctxt t1 double)

let double_uncast rewrite_ctxt (ce1,ce2) =
  match (ce1.pcexpr_desc,ce2.pcexpr_desc) with
  | (CECast (ce1',_, (_, ctype1')),CECast (ce2',_, (_, ctype2'))) ->
      begin
	if (double_cast rewrite_ctxt (ctype1',ctype2')) then (ce1',ce2')
	else (ce1,ce2)
      end
  | _ -> (ce1,ce2)

let decompose_equal_on_variable rewrite_ctxt vname ce =
  match ce.pcexpr_desc with
  | CECall (fname,[ce1;ce2],_,_,_) when fname = op_double_equal ->
      let ce1',ce2' = double_uncast rewrite_ctxt (ce1,ce2) in
      begin
	match ce1'.pcexpr_desc with
	| CEVar vname' when vname' = vname ->
	    (* Printf.printf "Found positional condition against [%s]\n" (Print_xquery_core.bprint_cexpr "" ce2); flush stdout; *)
	    (ce2,true)
	| _ ->
	    begin
	      match ce2'.pcexpr_desc with
	      | CEVar vname' when vname' = vname ->
		  (* Printf.printf "Found positional condition against [%s]\n" (Print_xquery_core.bprint_cexpr "" ce2); flush stdout; *)
		  (ce1,true)
	      | _ ->
		  ce,false
	    end
      end
  | CECall (fname,[ce1;ce2],_,_,_) when fname = op_integer_equal ->
      begin
	match ce1.pcexpr_desc with
	| CEVar vname' when vname' = vname ->
	    (* Printf.printf "Found positional condition against [%s]\n" (Print_xquery_core.bprint_cexpr "" ce2); flush stdout; *)
	    (ce2,true)
	| _ ->
	    begin
	      match ce2.pcexpr_desc with
	      | CEVar vname' when vname' = vname ->
		  (* Printf.printf "Found positional condition against [%s]\n" (Print_xquery_core.bprint_cexpr "" ce2); flush stdout; *)
		  (ce1,true)
	      | _ ->
		  ce,false
	    end
      end
  | _ -> ce,false

let is_positional_condition rewrite_ctxt in_vname at_vname where_expr ret_expr =
  (* Printf.printf "Trying for positional condition on [%s]\n" (Print_xquery_core.bprint_cexpr "" where_expr); flush stdout; *)
  if not((used_count in_vname where_expr) = 0) then false
  else if not((used_count at_vname where_expr) = 0) then false
  else if not((used_count at_vname ret_expr) = 0) then false
  else
    begin
      match where_expr.pcexpr_desc with
      | CECall (fname, arguments, sign, _, _) ->
	  if (fname = op_double_equal) || (fname = op_integer_equal) then
	    begin
	      match arguments with
	      | [arg1;arg2] ->
		  let (_,success) = decompose_equal_on_variable rewrite_ctxt at_vname where_expr
		  in success
	      | _ -> false
	    end
	  else false
      | _ -> false
    end

(* Function to apply for-clause simplification *)
let for_clause_rewrite rewrite_ctxt
    (cfl_clause_list : acfl_expr list) (cfl_clause : acfl_expr) (ce : acexpr) =
  match cfl_clause with
  | CEFOR (odt, vname, Some vname', cexpr1) ->
      begin
	match ce.pcexpr_desc with
	| CEFLWOR([], Some where_expr, None, ret_expr) when (is_positional_condition rewrite_ctxt vname vname' where_expr ret_expr) ->
	    let ah = ce.pcexpr_annot in
	    let eh = ce.pcexpr_origin in
	    let loc = ce.pcexpr_loc in
	    let stat_ctxt = get_context rewrite_ctxt in
	    let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
	    begin
	      match cfl_clause with
	      | CEFOR (odt, vname, Some vname', cexpr1) ->
		  (* Printf.printf "Found: for $x in $i where $i=$j return Expr\n"; flush stdout; *)
		  let (cexpr2,success) =
		    decompose_equal_on_variable rewrite_ctxt vname' where_expr
		  in
		  let cexpr_one = fmkacexpr (CEScalar (IntegerLiteral (Big_int.big_int_of_int  1))) ah eh loc
		  in
		  let (input_types, output_type), opt_fun_kind, upd = 
		    Norm_context.one_sig_from_norm_context norm_ctxt (fn_subsequence, 3)
		  in
		  let input_types = List.map (fun _ -> None) input_types in
		  let ce1 =
		    fmkacexpr (CECall (fn_subsequence, [cexpr1;cexpr2;cexpr_one], (input_types,output_type), upd, false)) ah eh loc
		  in
		  let ce = fmkacexpr (CEFLWOR([], None, None, ret_expr)) ah eh loc in
		  (cfl_clause_list@[CELET(odt, vname, ce1)], ce, false)
	      | _ ->
		  (cfl_clause_list@[cfl_clause], ce, false)
	    end
	| _ ->
	    begin
	      match used_count vname' ce with
	      | 0 ->
		  (cfl_clause_list@[ CEFOR(odt, vname, None, cexpr1) ], ce, true) (* Rule 1 *)
	      | _ ->
		  (cfl_clause_list@[cfl_clause], ce, false)
	    end
      end
  | _ ->
      (cfl_clause_list@[cfl_clause], ce, false)

(* Each FLWOR rewrite rules is applied to one clause at a time *)
let rec flclause_rewrite for_clause_rewriter let_clause_rewriter
    (rewrite_ctxt) 
    (cfl_clause_list : acfl_expr list) (cfl_clause : acfl_expr) rest_flwr_body =
  match (cfl_clause) with 
  | CEFOR(odt, vname, optvname, ce1) -> for_clause_rewriter rewrite_ctxt cfl_clause_list cfl_clause rest_flwr_body
  | CELET(odt, vname, ce1) -> let_clause_rewriter rewrite_ctxt cfl_clause_list cfl_clause rest_flwr_body

(* flwr_rewrite_aux recursively rewrites each LET/FOR clause in the
   FLWOR expression.  The function takes:
   (1) the list of clauses that have already been rewritten
   (2) the current clause to rewrite
   (3) the rest of the flowr expression
*)

and flwr_rewrite_aux for_clause_rewriter let_clause_rewriter
                     rewrite_ctxt (cfl_clause_list : acfl_expr list) ce =
  let ah = ce.pcexpr_annot in
  let eh = ce.pcexpr_origin in
  let loc = ce.pcexpr_loc in
  match ce.pcexpr_desc with
  | CEFLWOR([], opt_where_expr, opt_orderby_clause, ret_expr) -> 
      begin
	match cfl_clause_list with
	| [] -> ([], ret_expr, false)
	| _ ->
	    let new_flwr_expr = fmkacexpr (CEFLWOR(cfl_clause_list, opt_where_expr, opt_orderby_clause, ret_expr)) ah eh loc
	    in ([], new_flwr_expr, false)
      end
  | CEFLWOR(cfl_list, opt_where_expr, opt_orderby_clause, ret_expr) ->
      let next_cfl_clause = (List.hd cfl_list) in 
      let next_cfl_clause_list = List.tl cfl_list in 
      let rest_flwr_expr = fmkacexpr (CEFLWOR(next_cfl_clause_list, opt_where_expr, opt_orderby_clause, ret_expr)) ah eh loc in 
      let (cfl_clause_list', rest_flwr_expr', changed') =
	flclause_rewrite for_clause_rewriter let_clause_rewriter rewrite_ctxt cfl_clause_list next_cfl_clause rest_flwr_expr
      in
      let (cfl_clause_list'', rest_flwr_expr'', changed'') =
	flwr_rewrite_aux for_clause_rewriter let_clause_rewriter rewrite_ctxt cfl_clause_list' rest_flwr_expr'
      in (cfl_clause_list'', rest_flwr_expr'', changed' || changed'')
  | _ ->
      (* We end up here if the entire FLWOR expression can be reduced
      to the RETURN clause *)
      (cfl_clause_list, ce, false)

and flwr_rewrite for_clause_rewriter let_clause_rewriter rewrite_ctxt (ce : acexpr) =
  match ce.pcexpr_desc with
  | CEFLWOR _ ->
      let (_, ce', changed) = flwr_rewrite_aux for_clause_rewriter let_clause_rewriter rewrite_ctxt [] ce in
      if (changed) then (ce', changed)
      else raise Not_applied  (* Should we do the fix-point on clauses here ? *)
  | _ ->
      raise Not_applied

(*******************************)
(* Not operator simplification *)
(*******************************)

(*
   fn:not(Expr)           fn:not(Expr)	   
   Expr = true	          Expr = false	   
   -------------          -------------   
   false	          true            
*)
   
let not_value_rewrite rewrite_ctxt ce arguments = 
  let ah = ce.pcexpr_annot in
  let eh = ce.pcexpr_origin in
  let loc = ce.pcexpr_loc in
  let cexpr = List.hd arguments in
  match cexpr.pcexpr_desc with
  | CEScalar (BooleanLiteral b) -> 
      (fmkacexpr (CEScalar (BooleanLiteral (not (b)))) ah eh loc, true)
  | _ -> raise Not_applied

(*
   fs:item-sequence-to-node-sequence() is injected in the normalized
   expression just for the purposes of static typing.  It is always
   removed after static typing.  The function's semantics implemented
   in Streaming_constructors.element_constructor.
*)
let remove_function_call rewrite_ctxt ce arguments = 
  (List.hd arguments, true)

(*
 * When expressions of the form path//axis::nodetest occur without a tailing 
 * predicate, we can rewrite those as path/descendant::nodetest (with exception
 * of the attribute axis).
 *
 *)
 
(** XPath annotations removed -- adjust following functions to deal with that
  * NOTE: once TPNF is in place, moving this to factorization (after TPNF)
  *       may graetly simplify the implementation
  *)

(* helper function -- detects desc-or-self::node() *)
(*
let get_desc_step ds =
  let match_step step =
    match step.pcexpr_desc with
    | CEForwardAxis(v,Descendant_or_self, CPNodeKindTest(CAnyKind, ckind_type)) -> true
    | _ -> false
  in
  match ds with
  | CELET(_, v, e) ->
      begin
        match e.pcexpr_desc with
        | CECall (fname, [argument], sign, _, _) ->
	    begin
              if (get_xpath_annot argument.pcexpr_annot = Some Xp_flwor) then
	        match argument.pcexpr_desc with
                | CEFLWOR (cfl_list, _, _, step) -> 
		    if (match_step step) then Some argument
		    else None
                | _ -> None
	      else None
	    end
	| CEFLWOR (_, _, _, step) -> 
	    if (match_step step) then Some e
	    else None
        | _ -> None
      end
  | _ -> None *)


(*
   fn:data function simplification 

   fn:data(ScalarExpr)
   ------------------------
   ScalarExpr

*)
let fn_data_rewrite rewrite_ctxt ce arguments = 
  let cexpr = List.hd arguments in 
  match cexpr.pcexpr_desc with 
  | CEScalar _ -> (cexpr, true)
  | _ -> raise Not_applied



(*************************************)
(* Global sets of optimization rules *)
(*************************************)

(* Generic rewriting rules that are independent of other optimization & typing options *)
let generic_rule_set =
  [ snap_removal;
    insert_rewrite;
    if_rewrite;
    (* function_inlining_rewrite; *)
    flwr_rewrite for_clause_rewrite let_clause_rewrite;
    call_rewrite (fn_not, 1, not_value_rewrite);
    call_rewrite (fn_data, 1, fn_data_rewrite);
    call_rewrite (fs_item_sequence_to_node_sequence, 1, remove_function_call) ]

let generic_toplevel_rule_set = generic_rule_set

(***********************************************************************)

(* More optimization rules: *)

(* Function to simplify and boolean operator *)
(* Ask Mary/Jerome : t & _ | _ & t = _ ??? *)


(***************************************)
(* SQL-X specific simplification rules *)
(***************************************)

(* Note by Jerome / September 2002 *)

(* Descendant or self rewriting *)

(* Example:

   Input schema:
   
      define element users { element user_tuple* }
      define element user_tuple {
	element userid,
	element name,
	element rating?
      }

   Query:

      $users//user_tuple

        where $users is of type element users

   Should be rewritten as:

      $users/user_tuple/user_tuple

   $users/descendant-or-self::*/child::user_tuple

   $users,self::*     	    	     --> element users
   $users,child::* 	    	     --> element user_tuple
   $users,child::*,child::* 	     --> element userid
		       	 		 element name
		       	 		 element rating
   $users,child::*,child::*,child::* --> xsd:string

   $users/
     (child::user_tuple
     | (child::user_tuple/child::userid)
     | (child::user_tuple/child::name)
     | (child::user_tuple/child::rating)
     | (child::user_tuple/child::userid/text())
     | (child::user_tuple/child::name/text())
     | (child::user_tuple/child::rating/text()))/child::user_tuple

   two cases:
     element
     attribute
*)

