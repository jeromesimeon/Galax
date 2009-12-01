(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_walker.ml,v 1.4 2007/03/09 19:26:33 mff Exp $ *)

(* Module: Optimization_walker
   Description:
     This module implements the generic rule-based rewriter used for
     optimization.
*)

open Error
open Namespace_builtin

open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_algebra_ast_annotation_util

open Compile_context
open Compile_annotate
open Processing_context

(******************************)
(* *** Rewrite Rule Types *** *)
(******************************)

type optimization_rewrite_rule = 
    (Compile_context.logical_compile_context ->
      Ast_logical_algebra_types.logical_algop_expr ->
	Ast_logical_algebra_types.logical_algop_expr * bool) 

type optimization_removal_rewrite_rule =
    (Compile_context.logical_compile_context ->
      Ast_logical_algebra_types.logical_algop_expr ->
	Ast_logical_algebra_types.logical_algop_expr ->
	  Ast_logical_algebra_types.logical_algop_expr * bool)

(*************************)
(* *** Generic Stuff *** *)
(*************************)

type sub_expr_kind = 
  | Dependent 
  | Independent

type ('a, 'b) child_desc = ('a, 'b) aalgop_expr * sub_expr_kind * int

let get_cardinality se =
  match se with
  | NoSub     -> 0
  | OneSub _  -> 1
  | TwoSub _  -> 2
  | ManySub a -> Array.length a

let check_cardinality se index =
  if ((get_cardinality se) <= index) then
    begin
      let card = get_cardinality se in
      raise (Query (Malformed_Algebra_Expr("Attempt to access out of bounds index in subexpr: " ^ (string_of_int index) ^ " : " ^ (string_of_int card))))
    end

let get_sub_expr op dep_indep =
  match dep_indep with
  | Dependent   -> op.pdep_sub_expression 
  | Independent -> op.psub_expression

let assign_sub_expr op dep_indep se = 
  match dep_indep with
  | Dependent   -> op.pdep_sub_expression <- se
  | Independent -> op.psub_expression <-se

let replace_subexpr se index new_op =
  check_cardinality se index;
  match se with
  | NoSub -> (* Check cardinality should have caught this *)
      raise (Query (Malformed_Algebra_Expr("check_cardinality should have caught this. Attempt to assign subexpr to a NoSub")))
  | OneSub s -> OneSub new_op
  | TwoSub (s0, s1) ->
      if (index = 0) then
	TwoSub (new_op, s1)
      else
	TwoSub (s0, new_op)
  | ManySub a -> 
      a.(index) <- new_op;
      ManySub a

let get_op_from_sub_expr op dep_indep index = 
  let se = get_sub_expr op dep_indep in
  check_cardinality se index;
  match se with
  | NoSub ->
      raise (Query (Malformed_Algebra_Expr("check_cardinality should have caught this."
					   ^ " Attempt to retrieve subexpr to a NoSub")))
  | OneSub s -> s
  | TwoSub (s0,s1) ->
      if (index = 0) then
	s0
      else
	s1
  | ManySub a -> a.(index)

let replace_op parent dep_indep index new_op =
  let se = get_sub_expr parent dep_indep in
  assign_sub_expr parent dep_indep (replace_subexpr se index new_op)

(* Exposed: Update the parent of this op by using the given child
   description *)

let update_parent cd op =
  match cd with
  | None -> (* we are the root *) ()
  | Some (parent,dep_indep,index) ->
      replace_op parent dep_indep index op

let make_child_desc parent dep_indep index = 
  parent,dep_indep, index

(***************************************)
(* *** Optimization Rewrite Walker *** *)
(***************************************)

(* We need to make a child description, so we are able to reset it during the
   walk *)
let rec sub_expr_walk rewrite_rule parent dep_indep se =
  let f = rewrite_apply rewrite_rule in
  let child_fn = (fun x -> Some (make_child_desc parent dep_indep x))in
    match se with
	NoSub -> NoSub
      | OneSub s0 ->
	  let c0 = child_fn 0 in
	    OneSub (f c0 s0)
      | TwoSub (s0,s1) ->
	  let c0 = child_fn 0 in
	  let c1 = child_fn 1 in
	    TwoSub ((f c0 s0), (f c1 s1))
      | ManySub a ->
	  (* To keep evaluation order consistent *)
	  let len = (Array.length a) - 1 in 
	    for i = 0 to len  do
	      let reversed = len - i in
		a.(reversed) <- f (child_fn reversed) a.(reversed)
	    done;
	    ManySub a
	      (* rewrite_apply is a front end it takes a rewrite_rule
		 and an algop.  It walks the query tree and modifies
		 it as need be. The rewrite_rule should be wrapped in
		 a function that takes care of all annotations so that
		 on return of each application the tree is in the
		 correct state.

	      *)

(* helper functions for the walking to correctly
   set the flags for child descriptions *)
and walk_dep f op = 
  sub_expr_walk f op Dependent (op.pdep_sub_expression)
and walk_indep f op = 
  sub_expr_walk f op Independent (op.psub_expression)

and rewrite_apply rewrite_rule child_desc algop = 
  let fi = algop.palgop_expr_loc in 

  (* The algebraic operator contains the file location that should be
     correlated with errors, so we catch any exceptions here and
     rewrap the exceptions with the file location. *)
  try
  (* Apply to the ops first *)
    algop.psub_expression <- walk_indep rewrite_rule algop;   
    algop.pdep_sub_expression <- walk_dep rewrite_rule algop;

  (* Recalculate the annotation *)
    rewrite_rule child_desc algop
  with
  | exn -> raise (error_with_file_location fi exn)

(* Really descendent or self *)
(* walker *)
let rec descendent_walker satisfy next cur =
  if satisfy cur then
    begin
      let n = next cur in
	if satisfy n then
	  begin
	    descendent_walker satisfy next n
	  end
	else
	  cur, true
    end
  else
    cur, false
(* Semantic: Descendent or self satisfy and return the last one that does
   On return a triple:
   cur   : The last one that satisfies
   parent: The parent of this one (None if it is the first)
   success/failure : true if there was at least one
*)
   
let rec descendent_walker satisfy next parent cur =
  if satisfy cur then
    begin
      let n = next cur in
	if satisfy n then
	  begin
	    (* cur is now the parent *)
	    descendent_walker satisfy next (Some cur) n
	  end
	else
	  (* Here cur is the last one *)
	  cur, parent, true
    end
  else
    cur, None, false

(****************************)
(* Fold over algebraic plan *)
(****************************)
let rec fold_over_algop loc agg v se =

  match se.pdep_sub_expression with
    NoSub -> agg v (loc se)
  | OneSub s0 ->
      let v1 = agg v (loc se) in
      fold_over_algop loc agg v1 s0
  | TwoSub (s0,s1) ->
      let v1 = agg v (loc se) in
      let v2 = fold_over_algop loc agg v1 s0 in
      fold_over_algop loc agg v2 s1
  | ManySub a ->
	  (* To keep evaluation order consistent *)
      let apply_once v s = 
	fold_over_algop loc agg v s
      in
      Array.fold_left 
	(fun v s -> apply_once v s) v a 

(*********************************************)
(* *** Tree Walker Rewrite Rule Wrappers *** *)
(*********************************************)

let generic_wrapper rule has_changed comp_ctxt root child_desc algop = 
  let new_algop, changed = rule comp_ctxt algop in
  if changed then (* need to update annotations *)
    begin
      has_changed := true;
      update_parent child_desc new_algop;
	(* If algop was the root, then we reannotate the new operator *)
      if (algop == root) then reannotate_algebraic_expression new_algop
      else reannotate_algebraic_expression root;
      new_algop
    end
  else
    algop (* Then return the old one *)

let removal_wrapper rule has_changed comp_ctxt root child_desc algop = 
  let new_algop, changed = rule comp_ctxt root algop in
  if changed then (* need to update annotations *)
    begin
      has_changed := true;
      update_parent child_desc new_algop;
	(* If algop was the root, then we reannotate the new operator *)
      if (algop == root) then reannotate_algebraic_expression new_algop
      else reannotate_algebraic_expression root; 
      new_algop
    end
  else
    algop (* Then return the old one *)

(*
let removal_snap_free_wrapper rule has_changed comp_ctxt root child_desc algop = 
  if Optimization_judge.has_non_trivial_snap comp_ctxt algop
  then algop
  else removal_wrapper rule has_changed comp_ctxt root child_desc algop

*)
