(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_rewrite.ml,v 1.126 2007/07/05 08:35:54 simeon Exp $ *)

(* Module: Optimization_rewrite
   Description:
     This module contains algebraic optimizations.
*)

open Error
open Namespace_builtin

open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_algebra_ast_annotation_util

open Processing_context
open Compile_context
open Compile_annotate

open Optimization_util
open Optimization_walker

open Ast_path_struct

(***********************************************************************) 
(* Optimzer - This module's main task is to apply the several          *)
(* optimization rules in separate phases. PLEASE, stop adding rewrite  *)
(* rules here. Any new rules should be added into a separate module.   *)
(*                                                                     *)
(* It is the responsibility of each rule to recompute the annotations  *)
(* for the tree - that is after application a rule should always leave *)
(* a valid tree.                                                       *)
(*                                                                     *)
(* Enforcement:                                                        *)
(* - A rule takes an algop -> algop * boolean (changed)                *)
(*   rules must proved an annotation walker which is of the form:      *)
(* - 'info -> algop -> algop * boolean (this annotation finished)      *)
(***********************************************************************)


(* PLEASE NOTE: If you add algebraic operators that are pushed down or  *)
(* moved in the tree There may be interactions with other operations    *)
(* that are moved in a tree.                                            *)
(*                                                                      *)
(* We must be careful that our rules have a means of termination.  If   *)
(* two rules commute, we must pick one otherwise a cycle will form.     *)
(* Clearly, this could get worse if entire blocks of rules commute.     *)
(* This property could be guarenteed by appropriate choice of cost      *)
(*  model.  (i.e. a decreasing benefit with "time" in mind.)  - chris   *)



type input_tuple_state =
    { mutable consumed : bool;
      op               : Ast_logical_algebra_types.logical_algop_expr;
      mutable valid    : bool;}

let build_invalid root = 
  { consumed = false;
    op = root;
    valid = false;}

let get_current_tuple its =
  if (not its.valid) then
    raise (Query (Optimization ("Getting invalid tuple during rewrite")))
  ;
  its.consumed <- true;
  its.op

let is_valid_input_tuple its = 
  its.valid

let invalidate_input_tuple_state its =
  its.valid <- false

let build_new_input_tuple op =
  { consumed = false;
    op = op;
    valid = true; }

(* 
 [Apply|sort_1|sort_2] new_op
--
 [Apply|new_op|sort_1|sort_2]
*)
let group_cons_dep new_op dep = 
  (* assert length dep >= 2 *)
  let len = Array.length dep in
  match len with
    | 0 -> (* assert false *)
	raise (Query (Optimization ("Group dep without an apply clause")))
    | 1 -> (* assume just the apply *)
	Array.append dep [|new_op|]
    | _ ->
	let new_array = Array.create (len +1 ) new_op in
	  new_array.(0) <- dep.(0);
	  (* unnecessary because of Array.create *)
	  new_array.(1) <- new_op; 
	  Array.blit dep 1 new_array 2 (len - 2);
	    new_array
     

let is_input_tuple_consumed its = its.consumed

let rec input_tuple_rewrite_sexpr cit se =
  match se with
    | NoSub -> NoSub
    | OneSub s ->
	OneSub (input_tuple_rewrite cit s)
    | TwoSub (s0,s1)->
	TwoSub ((input_tuple_rewrite cit s0),
		(input_tuple_rewrite cit s1))
    | ManySub a ->
	let len =  (Array.length a) - 1 in
	  for i = 0 to len do
	    a.(len - i) <- (input_tuple_rewrite cit a.(len - i))
	  done;
	  ManySub a
	    (* Rewrite the current_input_tuple *)

and input_tuple_rewrite current_input_tuple cur_op =
  cur_op.psub_expression <-
  input_tuple_rewrite_sexpr current_input_tuple cur_op.psub_expression; (* side effecting *)

  if (is_a_tuple_input_map cur_op) then
    begin      
      let new_input_tuple = access_onesub cur_op.psub_expression in
	if (is_input_tuple new_input_tuple) &&
 	   (is_valid_input_tuple current_input_tuple) 
	then
	  begin
	    let input_tuple = get_current_tuple current_input_tuple in
	      cur_op.psub_expression <- (OneSub input_tuple);
	      cur_op
	  end
	else	
	  (* this map would have created a new input tuple *)
	  cur_op
    end
  else 
    begin
      if (is_group cur_op) 
      then cur_op (*invalidate_input_tuple_state current_input_tuple; *)
      else 
	begin
	  cur_op.pdep_sub_expression <- (input_tuple_rewrite_sexpr current_input_tuple cur_op.pdep_sub_expression) ;
	  cur_op
	end
    end

(* ace the scoped occurance of the input tuple
   in the current op *)
let input_tuple_replace replace_op cur_op =
  let its = build_new_input_tuple replace_op in
  let dep = input_tuple_rewrite its cur_op in    
    dep, (is_input_tuple_consumed its)

(* Walk in scope and replace the operand *)
let rec input_tuple_replace replace_op cd cur_op =
  (* check if we are still in scope *)
  if is_input_tuple cur_op then
    begin
      update_parent cd replace_op;
      true
    end
  else
    begin
      let cur_cd = make_child_desc cur_op Independent in
	(* We never need to walk down dependent expressions *)
	match cur_op.psub_expression with
	  | NoSub -> false
	  | OneSub s ->
	      let cd = Some (cur_cd 0) in
		input_tuple_replace replace_op cd s
	  | TwoSub (s0,s1) ->
	      (input_tuple_replace replace_op (Some (cur_cd 0)) s0)
	      || (input_tuple_replace replace_op (Some (cur_cd 1)) s1)
	  | ManySub sa ->
	      let r_array = 
		Array.mapi (fun index v ->
			      input_tuple_replace replace_op (Some (cur_cd index)) v)
		  sa
	      in
		Array.fold_left (||) false r_array
    end

let debug_print_op op = 
  Format.fprintf !Conf.algebra_optimization_rewrite_formatter 
    "@?**** Expr found  ****@.@\n:@\n%a@."
    (fun ff x -> Print_xquery_algebra.fprintf_logical_algstatement ff "" x) op
    

(* 
   Match:
    LeftOuterJoin(SepSequence(_)
         LeftOuterJoin(SepSequence(_)...
            Join(InputTuple, R) | LeftOuterJoin(SepSequence?(InputTuple), R)
         )
     ,R)
   | Match:
   LeftOuterJoin... (LeftOuterJoin(InputTuple
   Return Join,true, cd

   where cd can be used with update parent to update the parent of the join we are replacing 
   or None if it can not be replaced
*)
(* THIS CODE SHOULD BE FACTORED *)    
let leftmost_join_input_tuple op cd =
  let return_failure = op, false, cd in
    (* Above pattern *)
  let pattern_instance pat =
    if is_outer_join pat then    
      begin	
	let l, _ = access_twosub pat.psub_expression in
	  is_a_sep_sequence l 
      end
    else
      begin
	false
      end
  in
    (* Walk passed *)
  let walk_left x = 
    let l, r = access_twosub x.psub_expression in
      (access_onesub l.psub_expression)
  in
    
  (* Strip out the LeftOuterJoins *)
  (* If it is valid then it leaves us with
     the left_most left outer join. If the child of this is a sep_sequence/Regular join we are done *)
  let left_most_join, parent, valid = descendent_walker pattern_instance walk_left None op in
    
  (* If it is valid, then we are at the LOJ, so move one more. *)       
  let join, cd = 
    if valid then
      begin
	(* There is LOJ(SepSequence,) we need access to the sep sequence *)
	let parent,_ = access_twosub left_most_join.psub_expression in 
	let cd = Some (make_child_desc parent Independent 0) in
	  (walk_left left_most_join), cd
      end
    else
      op, cd
  in
    (* Should now be at the regular join *)
    if (is_regular_join join) then
      begin
	let l, _ = access_twosub join.psub_expression in 
	  if (is_input_tuple l) then
	    join, true, cd
	  else return_failure      
      end
    else (* See if we really are at an Input Tuple, i.e. there are no left outer joins, in this case
	    we return left_most_join, a cd for the parent of this join and true *)
      if (is_input_tuple join) then
      begin
	let cd = 
	  match parent with
	      None -> None
	    | Some parent -> Some (make_child_desc parent Independent 0) (* We are the left most join *)
	in
	  left_most_join, true, cd
      end
    else return_failure

    
(***********************)
(* Push down selection *)
(***********************)
(*

  Select{D_1}(Op{D2..Dk}(I_1,..,I_j))
  ----
  Cond: 
      Exists z s.t. Accessed(D_1) subset I_z 
  OP..(I_1,., Select{D_1}(I_z)..)
  
  It is possible that a tuple field occurs in 
  multiple places - this is now only due to 
  a push down map. 

  So the rule is actually that *each* index is
  covered.
  

  -- Change to more complicated predicates ---
  1. push the whole thing down the indep
  2. push individual parts down the indep
  3. push whole thing down the dep
  4. push individual parts down the dep

  (* Currently we only support pushing down simple conjuncts *)
*)
(* below relies on returning the original on failure! *)
let pushdown_individual_select simple_conjunct (comp_ctxt:Compile_context.logical_compile_context) op_to_try =
  let eh       = simple_conjunct.palgop_expr_origin in 
  let fi       = simple_conjunct.palgop_expr_loc    in
  let accessed_fields = algop_get_accessed_fields simple_conjunct in
  let returned_fields = algop_get_returned_fields op_to_try in

  if (is_a_map_concat op_to_try) then
    if (Gmisc.is_subset accessed_fields returned_fields) && 
      (returned_fields <> []) (* This ensures we are not pushing into items *) then
      begin
	(* Wrap the op with the select, i.e. return 
	   Select[(SimpleConjunct)]{simple_conjucnt}(op_to_try)
	   and true
	 *)
	(* Wrap this up *)
	let op =
	  Optimization_predicates.construct_singleton_select simple_conjunct op_to_try eh fi
	in
	true, op	  
      end
    else false, op_to_try
  else false, op_to_try

let pushdown_individual indep_dep (comp_ctxt:Compile_context.logical_compile_context) select_op =
  let return_not_applied = select_op, false in
    if (is_select select_op) &&
      (Optimization_predicates.is_simple_conjunct select_op) then
	begin
	  (*****************************)
	  (* try to push down each one *)
	  (*****************************)
	  let conds   = access_manysub select_op.pdep_sub_expression in 
	  let to_push = access_onesub  select_op.psub_expression in 
	  let ops     = 
	    match indep_dep with
		Independent -> to_push.psub_expression
	      | Dependent   -> to_push.pdep_sub_expression
	  in

	  let _ = Optimization_predicates.extract_pred_desc select_op in
	  let n_conds   = Array.length conds in
	  (* Try each index in turn, stopping after the first one to be
	     pushed down *)
	  let rec try_index cur_index =
	    if (cur_index >= n_conds) then
	      return_not_applied
	    else
	      begin
		let success, wrapped = 
		  match ops with
		      NoSub -> false, NoSub
		    | OneSub s -> 
			begin
			  let suc0, wrap0 = pushdown_individual_select conds.(cur_index) comp_ctxt s in
			    suc0, (OneSub wrap0)
			end
		    | TwoSub (s0, s1) ->
			begin
			  let suc0, wrap0 = pushdown_individual_select conds.(cur_index) comp_ctxt s0 in
			  let suc1, wrap1 = pushdown_individual_select conds.(cur_index) comp_ctxt s1 in
			    (suc0 || suc1), (TwoSub (wrap0, wrap1))
			end
		    | ManySub sa ->
			begin
			  let suc_wrap_array = Array.map (pushdown_individual_select conds.(cur_index) comp_ctxt) sa in
			  let suc     = Array.fold_left (fun t (x,y)->t||x) false suc_wrap_array in
			  let wrapped = Array.map snd suc_wrap_array in
			    suc, (ManySub wrapped)
			end
		in
		  if success then
		    begin
		      (* Notice we are only operating on single conjuncts *)
		      let ret_value = Optimization_predicates.remove_conjunct select_op cur_index in
			(* No matter what we store the wrapped in the to_push's indep *)
			to_push.psub_expression <- wrapped;
			match ret_value with
			  | None ->  (* The predicate has been totally pushed down *)
		              (* We return the indep expression then *)
			      to_push, true
			  | Some v ->
			      (* The select still remains,  *)
			      v, true
		    end
		  else
		    try_index (cur_index + 1)
	      end
	  in try_index 0
		(* If all conds are push-ed down then the return is the indep expression
		   if not... then nothing *)
	end
    else
      return_not_applied


(*************************************************************)
(* This rewrite combines selection_operators that are nested *)
(* Select{cond1}(Selct{cond2}(indep))                        *)
(*     -----------                                           *)
(* Select{cond1 AND cond2}(indep)                            *)
(*************************************************************)

(*************************************************************)
(* This rewrite combines selection_operators that are nested *)
(* Select{cond1}(Selct{cond2}(indep))                        *)
(* cond1 is pure & cond1#cond2                               *)
(* ----------------------------------                        *)
(* Select{cond2 AND cond1}(indep)                            *)
(*************************************************************)

let combine_multiple_selections comp_ctxt algop =
  let return_not_applied = algop, false in
  let sel1 = algop in
  if (is_select sel1) then
    begin
      let sel2 = access_onesub sel1.psub_expression in 
      if (is_select sel2) && 
	(Optimization_judge.side_effect_free comp_ctxt sel1) &&
	(Optimization_judge.side_effect_free comp_ctxt sel2)
      then
	begin
	  (Optimization_predicates.conjunctive_merge_select sel1 sel2), true
	end
      else return_not_applied
    end
  else return_not_applied

(* Need an operation to deal with predicates of Select correctly *)
let pushdown_selection comp_ctxt algop =
  (* Parameterized by indep,dep, want to push down on indeps
     first since they are evalutated earlier in query execution *)
  let pushdown_helper indep_dep =
    if (is_select algop) then
      begin
	let the_select    = algop in 
	let the_cond      = access_manysub algop.pdep_sub_expression in 

	(* This holds if we are able to do this pushdown *)
	let the_op        = access_onesub algop.psub_expression in
	  if   ((is_group the_op) && (indep_dep = Dependent))
	    || (is_a_map_index the_op)
	    || ((is_an_outer_mapconcat the_op) && (indep_dep = Dependent)) then
	      algop, false
	  else
	    begin
	      let select_fields = 
		Array.fold_left (fun fields op ->
				   fields @ (algop_get_accessed_fields op)) 
		  [] (access_manysub algop.pdep_sub_expression)
	      in

	      (* Check if is ok w.r.t side-effects *)
	      let expr,side_effect_ok          = 
		match indep_dep with
		  | Independent -> 
		      let side_effect_ok = 		
			not (Optimization_judge.subexpr_has_side_effect comp_ctxt (ManySub the_cond))  
			  && (not (Optimization_judge.has_dependent_side_effect comp_ctxt the_op))
		      in
			the_op.psub_expression, side_effect_ok
		  | Dependent   -> 
		      (* We know that no side-effect operation has
			 dependent expressions, so we are ok in saying
			 true *)
		      the_op.pdep_sub_expression, true
	      in
	      (* Condition for when we are pushing down on the dependent *)
	      (* If the current sub op, which we index, of the
		 operation meets the condition that its returned
		 fields is a subset of the fields the select accesses,
		 then we can push the select down this branch. This
		 helper function makes a list of which indexes can be pushed down on. *)
	      let select_field_helper (cur_index, cur_list) s = 
		let next = cur_index + 1 in
		let returned_fields = algop_get_returned_fields s in
		  if (Gmisc.is_subset select_fields returned_fields) &&
		    (returned_fields <> []) (* Make sure we don't pushd down constants out of tuple fields *)
		    then
		      (next, (cur_index, s) :: cur_list)
		  else 
		    (next, cur_list)
	      in

	      (* list_of_subexprs contains the list of sub
		 expressions that have all the required fields to be
		 pushded down on and their index within the current op *)
	      let _, list_of_subexprs  = 
		match expr with
		  | NoSub -> -1,[]
		  | OneSub s0 ->
		      select_field_helper (0,[]) s0 
		  | TwoSub (s1,s2) ->
		      select_field_helper 
			(select_field_helper (0, []) s1) s2
		  | ManySub sa ->
		      Array.fold_left select_field_helper (0, []) sa 
	      in

		if side_effect_ok then 
		  begin
		    (* If we are ok w.r.t side-effects proceed *)
		    match list_of_subexprs with
		      | [] -> (* There are no ops that contain all the fields the select operates on, so fail *)
			  algop, false
		      | [(index_in_op, op_to_be_pushed)] ->
			  (* Select(indep{..}(..,s,..)) ->
			     indep{..}(..,Select{}(s),...) *)		
			  (* This is the unique op that contains all the information for the select *)
			  (* make a child description so we can update the_op at the correct location *)
			  let op_cd     = Some (make_child_desc the_op indep_dep index_in_op) in
			    (* Make the description for updating the select operation, namely in its independent expression *)
			  let select_cd = Some (make_child_desc the_select Independent 0) in
			    (* now make the op we are pushing the child (independent subexpr) of the select *)
			    update_parent select_cd op_to_be_pushed;
			    (* and make the old op point to the select *)
			    update_parent op_cd the_select;
			    (* Now return the_op, because it is in the place of the select *)
			    the_op, true
		      | _ -> 
			  (* If you want to replicate the select, we
			     can, simply copy and iterate the code
			     above, but I'm removing it for clarity - Chris *)
			  algop,false
		  end	   (* side_effect pos_cond branch end *)
		else algop,false		  
	    end (* map conditions op check, negative__cond *)
      end
    else
      algop,false
  in
  let ret, changed = pushdown_helper Independent in
    if changed then
      ret,changed
    else
      pushdown_helper Dependent


(* Todo:
   o Add in a way to recompute annotations with early termination
   o Write up formal rule
*)
(* Given a tuple rewrite it into a product *)

(*
  Start with:

  MapConcat
    {D_0}
    (Map{D_1}(I_1))
    |-  Indep_0  -|
  ----------------------
  Map{Product(D_1, D_0)}
     (I_1)

  Order: Notice that for each i in I_1, D_1(i) is computed in its
  order. Then the D_0(i) are appended.  So the final sequence order is
  D_1(i), D_0(i) and is reflected in the product

 --- Product Takes i
  in I_1 -> materializes D_2 and then returns a cursor appending each
  param of D_1 with D_2

*)
(* Assumes indep_expr are valid *)
let map_product_rewrite comp_ctxt algop =   
  (* Cond *)
  if (is_a_map_concat algop) then
    begin
      let dep_0   = access_onesub algop.pdep_sub_expression in
      let indep_0 = access_onesub algop.psub_expression     in		
      if (is_a_dep_map indep_0) &&
        (Optimization_judge.map_tuple_independent dep_0 indep_0) &&
	(not (Optimization_judge.has_side_effect comp_ctxt dep_0)) &&
	(* independence weak check *)
	(not (Optimization_judge.has_non_trivial_snap comp_ctxt indep_0)) 
      then 
	begin (* This can be turned into a product *)
	  (* Action *)
	  let indep_1  = access_onesub indep_0.psub_expression in
	  let dep_1    = access_onesub indep_0.pdep_sub_expression in
	  let eh       = algop.palgop_expr_origin in 
	  let fi       = algop.palgop_expr_loc    in
	  let left     = dep_1 in
	  let right    = dep_0 in
	  (* This could be an item tuple map.. *)
	  let outer_map_name = indep_0.palgop_expr_name in 
	  let new_product = 
	    match algop.palgop_expr_name with
	    | AOEMapConcat -> 
		logical_aalgop_mkop AOEProduct (TwoSub (left, right)) NoSub None eh fi
	    | _ -> 
		raise (Query (Optimization ("unknown type of map concatentation")))
	  in
	  (* Now compute the wrapper map.. notice that the name is the same *)
	  let mod_expr = 
	    logical_aalgop_mkop
	      outer_map_name (OneSub indep_1) (OneSub new_product) None eh fi 
	  in
	  (* Annotations recomputed elsewhere *)
	  mod_expr, true 
	end	    
      else
	algop, false
    end
  else
    algop, false

(* 
  MapConcat{Dep_0}(Indep_0)
  ---- independent ----
   Product(Indep_0, Dep_0)

   Cond: accessed(dep_0) intersect (Indep_0) = empty_set
*)

let debug_build_product_result r =
  if Debug.join_debug()
  then
    if r then
      Debug.print_join_debug "Succeeded!!"
    else
      Debug.print_join_debug "Failed!!"

let debug_build_product () =
  if Debug.join_debug()
  then
    Debug.print_join_debug "PRODUCT: trying."
(*

  P1 : [R1] -> Seq[R1;R2]
  P2 : [R1] ->p Seq[R3]
  and stuff about R1 not changing (P1: [R1:a] -> Seq[R1:a;R2])
  ------------------------------------------------------------
  MapConcat P2 P1
  ==
  Product P1 P2 : [R1] -> Seq[R1;R2;R3]

  P1 : [R1] -> [R1;R2]
  P2 : [R1;R2] -> [R1;R2;R3]
  ----------------------------------------
  MapConcat P2 P1 : [R1] -> Seq[R1;R2;R3]

  
  P1 : [R1] -> [R1;R2]
  P2 : [R1] -> [R1;R3]
  ----------------------------------------
  Product P1 P2 : [R1] -> Seq[R1;R2;R3]

  Type preservation:

  Mapconcat inference uses subtyping. Note that P2 does not depend on
  R2!!!

                            P2 : [R1] ->p Seq[R3]   [R1;R2] <= [R1]
                            ---------------------------------------
  P1 : [R1] -> Seq[R1;R2]   P2 : [R1;R2] ->p Seq[R3]
  -----------------------------------------------------------------
  MapConcat P2 P1 : [R1] -> Seq[R1;R2;R3]

  Product inference is simpler.

  P1 : [R1] -> Seq[R1;R2]  P2 : [R1] ->p Seq[R3]
  ----------------------------------------------
  Product P1 P2 : [R1] -> Seq[R1;R2;R3]


*)


let natural_product_rewrite comp_ctxt algop =
  (* Is that a map concat? *)
  if (is_a_map_concat algop) then
    begin
      debug_build_product ();
      let dep_0   = access_onesub algop.pdep_sub_expression in
      let indep_0 = access_onesub algop.psub_expression     in
      (* Is that a safe map concat? *)
      let tuple_indep = Optimization_judge.natural_tuple_independent dep_0 indep_0 in
      let side_effect_free = Optimization_judge.side_effect_free comp_ctxt dep_0 in
      debug_build_product_result tuple_indep;
      if tuple_indep && side_effect_free
      then
	(* This can be turned into a product *)
	begin
	  (* Action *)
	  let eh       = algop.palgop_expr_origin in 
	  let fi       = algop.palgop_expr_loc    in
	  let left     = indep_0 in
	  let right    = dep_0 in
	  let new_product =
	    match algop.palgop_expr_name with
	    | AOEMapConcat -> 
		logical_aalgop_mkop AOEProduct (TwoSub (left, right)) NoSub None eh fi
	    | _ -> 
		raise (Query (Malformed_Algebra_Expr ("unknown type of map concatentation")))
	  in		
	  (* Annotations recomputed elsewhere *)
	  new_product, true 
	end	    
      else
	algop, false
    end
  else
    algop, false  


(* 
   Product(L,R)
   Product is singleton_tuple
   --------------------------
   L ++ R 
*)
let singleton_product_to_concat comp_ctxt algop =
  let return_failure = algop, false in
  if is_product algop &&
    (Optimization_judge.is_singleton_tuple comp_ctxt algop)
    && (not(Optimization_judge.contains_update algop))
  then 
    begin
      let eh       = algop.palgop_expr_origin in 
      let fi       = algop.palgop_expr_loc    in
      (logical_aalgop_mkop
	 AOEConcatTuples algop.psub_expression NoSub None eh fi), true
    end 
  else 
    return_failure


(*********************)
(*** Join Rewrites ***)
(*********************)

(*
  Join Rewrite
  
  Select(Product(..), dep_Expr) = Join(...) on dep_expr*)
(* First try to push down on a branch *)
let join_rewrite comp_ctxt algop =
  let op, changed = pushdown_selection comp_ctxt algop in
    if changed then op, true
    else begin
      if (is_select algop)
(*	(not (Optimization_judge.subexpr_has_side_effect comp_ctxt algop.pdep_sub_expression)) *)
      then
	begin
	  let indep_0 = access_onesub algop.psub_expression in
	  if (is_product indep_0) then
	    begin
	      let l,r     = access_twosub indep_0.psub_expression in
	      let eh      = algop.palgop_expr_origin in
	      let fi      = algop.palgop_expr_loc    in
	      let pd      = Optimization_predicates.extract_pred_desc algop in
	      let mod_exp = logical_aalgop_mkop (AOEJoin pd)  (TwoSub (l,r)) algop.pdep_sub_expression None eh fi in
	      mod_exp, true
	    end
	  else if (is_any_join indep_0)
	  then
	    (* Combine expression *)
	    (Optimization_predicates.conjunctive_merge_select algop indep_0), true
	  else
	    algop, false
	end
      else
	algop, false
    end

  (* Map Through

     Semantic of Group with missing group.
     .... Should be:  x <- empty_seq
          This requires:
          - special value for aggregate
          or, bundling of the join with the group
     
     LeftOuterJoin Semantic should now be
     Left <- values
     Right <- special NULL value

     Group Null ==> apply is not called.
     This is ok with nondeterministic semantic
     and outputs correct result with respect
     to element construction.     

     So what should output of Group with
     null mixed values be? Should 
     this condition be possible?
     How does NULL Interact with everything?

     
     
     MapConcat { Group By Names apply {x: } 
     (Join(InputTuple, e_2)) } 
     (e_1)
     ====
     Group By (z :: Names) apply {x: ... }
     LeftOuterJoin(e_1, e_2) on.. 

     -----

     Group By (z :: Names) apply {x : ... }
     LeftOuterJoin(MapIndex(z)(e_1),e_2)

     --- is currently only correct if Names = {} --- 
  *)

(* New Rule *)
(*
  MapConcat { (Group group_desc) Join(InputTuple, e_2)}(e_1)
----
  Group group_desc' 
    LeftOuterJoin(MapIndex(z) e_1, e_2)
****
  Add to each group_desc a grouping by z, the new element.
  
  group_desc' = List.map 
    gd'.group_names = z :: gd.group_names
     
*)
let add_name_to_group_desc fresh_name induced gd_list =
  List.map (fun gd ->
	      let new_group = get_group_names gd in
	      mk_group_desc
		(fresh_name :: new_group) 
		(induced @ (get_induced_group gd))
		(get_valid_names gd)
		(get_aggregate_name_and_type gd) 
	   )
    gd_list
(*
(* Not updated *)
let append_name_to_group_desc fresh_name gd_list =
  List.map (fun gd ->
	      let new_group = get_group_names gd in
	      mk_group_desc (new_group @ fresh_name :: []) (get_valid_names gd) (get_aggregate_name gd) (get_distinct_value gd) )
    gd_list
*)    

let add_valid_name_to_group_desc valid_name gd_list =
  List.map (fun gd ->
	      let new_valid = valid_name :: (get_valid_names gd) in
	      mk_group_desc 
		(get_group_names gd) 
		(get_induced_group gd)
		new_valid
		(get_aggregate_name_and_type gd)
	   )
    gd_list

(* Map through, without a group *)
let map_through_with_join_rewrite comp_ctxt root algop =
  if (is_any_map_concat algop) then 
    begin
      let head_join = access_onesub algop.pdep_sub_expression in 

      (* we need to set the op after *)
      (*      let cd = make_child_desc  Independent 0 in  *)
	
      (* We are changing this join so we need a reference to its
	 parent *)
      let join, valid, cd = leftmost_join_input_tuple head_join None in 
	if valid then
	  (* We know the left is an SepSequence(InputTuple) *)
	  begin
	    let on_cond    = join.pdep_sub_expression in
	    let pd         = Optimization_predicates.extract_pred_desc join in 

	    let eh         = join.palgop_expr_origin in 
	    let fi         = join.palgop_expr_loc    in

	    (* If the join is outer or is an outermap, there already
	       is a name. Notice also we are giving outer join names
	       a priority over outer map names. This is important as per 
	       the note below (we can drop the outer map name).
	    *)
	    let fresh_name = 
	      if (is_outer_join join) then
		get_outer_join_name join
	      else if is_an_outer_mapconcat algop then
		get_outer_map_name algop
	      else
		get_new_group_name comp_ctxt 
	    in 
	      (* Notice if it is a left outer join in a left
		 outer map:

		 OMapConcat[(map_name)]{LeftOuterJoin[(y)](...) ..}(SepSequence[(map_sep_name)] (e1)
		 ----
		 1. v appears *ONLY* in a group by clause, and
		    even more specifically only in the must be valid portion.
		 2. v can never be invalid if its input is non-empty

		 => it is valid to rename #map_name -> #map_sep_name (seq will be non-empty (i.e. valid) iff e1 is
		     non-empty (as a sequence) and then drop it.

		 -----
		 Notice that Left Outer Joins have SepSequences on the input tuple. (So do outer map concats)
		 Since the scoping of these is query or function - we can rename them as well.
		 
		 Consider the basic case:
		 OMapConcat[(map_name)]{
		   LeftOuterJoin[(loj_name)](
		        SepSequence[(loj_sep_name)] (InputTuple), R)
		  } (SepSequence[(map_sep_name)](expr))
                    |------------ e1 -----------------|

		 is equivalent to 
		 LeftOuterJoin[(loj_name)] (SepSequence[(loj_sep_name)] expr, R)
		 
		 with the renaming
                 #map_name -> #loj_sep_name
                 #map_sep_name -> #loj_sep_name
		 
		 * The later renaming comes because by the above, we would get
		 * SepSequence[(loj_sep_name)](SepSequence[(map_sep_name)](e1)) 
		 * on the outer branch, which can be collapsed
                                            
	      *)		 		 
	      if (is_outer_join join) && (is_an_outer_mapconcat algop) then
		begin
		  let loj_sep_name  = get_sep_sequence_name (fst (access_twosub join.psub_expression)) in
		  let map_name      = get_outer_map_name algop in
		  let map_sep_name  = get_sep_sequence_name (access_onesub algop.psub_expression) in		    

		  (* NOTE THIS COULD CHANGE THINGS SEE BELOW *)
		  let new_root, changed = replace_tuple_name comp_ctxt map_name     loj_sep_name root in		
		  let new_root, changed = replace_tuple_name comp_ctxt map_sep_name loj_sep_name new_root in
		    
		    if not (new_root = root) then
		      raise (Query (Optimization ("Tuple renaming changed root")))
		    ;		    
		end
	      ;
	      (* IT IS POSSIBLE THE ABOVE BLOCK CHANGED ALGOP, specifically e_1 *)
	      (* So, we can not have computed this any earlier *)
	      let e_1        = access_onesub algop.psub_expression in
	      let _,e_2      = access_twosub join.psub_expression in 		
	  
	    (* The left hand side generates the groups *)
	    (* So, it's contents will be valid outside of this scope *)

	    let _ = algop_get_returned_fields e_1 in
	    let loj_op      = 
	      logical_aalgop_mkop (AOELeftOuterJoin (fresh_name,pd)) (TwoSub (e_1,e_2)) on_cond None eh fi in
	      	      
	      (* Now update for return and return the modified group *)
	      match cd with 
		| None -> (* this new join is the head *) loj_op, true
		| Some _ ->
		    update_parent  cd loj_op; 	      
		    head_join, true (* We've dispensed with the map *)
	      
	  end	
	else algop, false
    end
 else algop, false




(* Helpers *)

(* Is the tuple a trivial copy of the given variable name.  
   None    -> No.  
   Some tn -> yes and tn is the tuple name 
*)
let trivial_tuple_create algop crname =       
  let return_failure = None in 
    match algop.palgop_expr_name with
      | AOECreateTuple a ->
	  if (Array.length a) = 0 || (Array.length a) > 1 then
	    return_failure
	  else 
	    begin
	      (* Now check if it is a AOEVar *)
	      let var = (access_manysub algop.psub_expression).(0) in 
		match var.palgop_expr_name with
		  | AOEVar name ->
		      let cname,odt = a.(0) in 
		      if Namespace_names.rqname_equal crname name 
		      then Some cname
		      else return_failure
		  | _ -> return_failure
	    end
      | _ -> return_failure

(* 
  None   ->  Not a tuple access. 
  Some v -> the tuple access field 
*)
let tuple_access_name op = 
  match op.palgop_expr_name with
    | AOEAccessTuple n -> Some n
    | _ -> None
	  

(* Unnecessary MapConcat Rewrite *)
(* MapConcat{MapConcat{D_1}(InputTuple)}
     (I_1)
   ====
   MapConcat{D_1}(Indep_1)

   ---- more general ----
   Map{MapConcat{D_1}(InputTuple)}(Indep_1)
   ---> MapConcat{D_1}(Indep_1)

   --- one more twist ---
   Map{MapConcat{D_1}(MapIndex(z_1) InputTuple)}(Indep_1)
   --> MapConcat(D_1)(MapIndex(z_1) Indep_1)

   --- there is a more general condition ---
 *)

let double_concat_removal comp_ctxt algop =
  let return_failure = algop, false in
    (*  if (is_a_map_concat algop) then *)
    if (is_a_dep_map algop) then
      begin
	let inside = access_onesub algop.pdep_sub_expression in
	let indep_1 = access_onesub algop.psub_expression in 
	  if (is_a_map_concat inside) then
	    begin
	      let concat_input = access_onesub inside.psub_expression in
		match concat_input.palgop_expr_name with
		  | AOEInputTuple ->
		      begin
			  inside.psub_expression <- (OneSub indep_1);
			  inside, true
		      end
		  | AOEMapIndex v ->
		      begin
			let input_tuple = access_onesub concat_input.psub_expression in 
			  if (is_input_tuple input_tuple) then
			    begin
			      (* wrap the map around the input to the original map *)
			      concat_input.psub_expression <- (OneSub indep_1);
			      inside.psub_expression <- (OneSub concat_input);
				inside, true
			    end
			  else
			    return_failure
		      end
		  | _ ->		      
		      return_failure
	    end
	  else  return_failure
      end
    else return_failure
	

(****************************)
(* double map index removal *)
(*
  NOTE: we require that all map index names be unique.
  MapIndex variables can only occur in group
*)      
let double_index_removal comp_ctxt root algop =
  let return_not_applied = algop, false in
  match algop.palgop_expr_name with
    | AOEMapIndex vn ->
	begin
	  let mi = access_onesub algop.psub_expression in
	  match mi.palgop_expr_name with
	    | AOEMapIndex vn2 ->
		begin		  
		  let new_root,changed = replace_tuple_name comp_ctxt vn vn2 root in
		    if not (new_root = root) then
		      raise (Query (Optimization ("Tuple renaming changed root")))
		    ;		    
		    mi, true (* Eliminate the map by returning the second *)
		end	    
	    | _ -> return_not_applied
	end
    | _ -> return_not_applied


(********************
  OMapConcat[(v1)]{ 
     NullMap[(v2)](Dep_1) 
    } (indep)
----
\v2 -> v1
  OMapConcat[(v1)]{dep}(indep)

 Nicola: This rule is probably not fired now that we changed the push-map-concat
         not to change MapConcat into an OMapConcat anymore.         

******************)
let nested_null_index comp_ctxt root algop =
  let return_not_applied = algop, false in
    if (is_an_outer_mapconcat algop) then
      begin
	let name    = get_outer_map_name algop in
	let null_op = access_onesub algop.pdep_sub_expression in 
	  if (is_null_map null_op) then
	    begin
	      let null_name = get_null_map_name null_op  in
	      let dep = access_onesub null_op.psub_expression in
		algop.pdep_sub_expression <- (OneSub dep);
		(* Change the null_name -> outer map's name *)
		let new_root,changed = replace_tuple_name comp_ctxt null_name name root in
                  (* fix by Philippe: replaced structural equaity (=) *)
                  (* in test below by physical equality (==)          *)
		  if not (new_root == root) then
		    raise (Query (Optimization ("Tuple renaming changed root")))
		  ;		    
		  algop, true
	    end
	  else return_not_applied
      end
    else return_not_applied
  


(********************
  P1: T->Seq[R1]
  P2: [R1]->Seq[R2]
  R2 = a1;..;an
  ----------------------------
  MapConcat(OMap[a1..an][null] ID P2) P1
  ==
  OMapConcat[a1..an][null] P2 P1 : T -> Seq[a1;..;an;R1;null:Bool] 
******************)
let map_concat_over_omap_into_omapconcat comp_ctxt root algop =
  let return_not_applied = algop, false in
    if (is_a_map_concat algop) then
      begin
	    let null_op = access_onesub algop.pdep_sub_expression in 
	      if (is_null_map null_op) then
	        let null_name = get_null_map_name null_op  in
            let new_outer_map = 
		      logical_aalgop_mkop 
			    (AOEOuterMapConcat null_name) 
			    (OneSub (access_onesub algop.psub_expression)) 
			    (OneSub (access_onesub null_op.psub_expression)) 
			    None algop.palgop_expr_origin algop.palgop_expr_loc
            in
              new_outer_map, true
	      else return_not_applied
      end
    else return_not_applied
      
      
      
  (* let to value rewrite.
     This is replicated from cleaning, the
     reason being that there are CELET bindings (not cleaned)
     and CELet bindings which are cleaned by the rule.

     So here, we reapply to Let bindings
  *)
let get_indep_1 op = access_onesub op.psub_expression 
let get_dep_1   op = access_onesub op.pdep_sub_expression 

(**************************************************)
(* This Section of code deals with
   rewriting map concats *)
(* This function determines if there
   are InputTuples (of this operand) in scope
   such that they can all be reached without 
   traversing an AOEGroupBy (or other op we can't map across)
   ----
   Three Conditions:
   type open_scoped_tuple_return_cond = 
        | Unbound_input_tuples
        | No_unbound
        | Blocked
*)
(****************************************************)

type open_scoped_tuple_return_cond = 
  | Unbound_input_tuples
  | No_unbound
  | Blocked

let has_open_scoped_tuples start_op = 
  let calculate_return r_array =
    let any_blocked, any_unbound = 
      Array.fold_left 
	(fun (blocked, unbound) item -> 
	   let ret_blocked = blocked || (Blocked = item)              in
	   let ret_unbound = unbound || (Unbound_input_tuples = item) in
	     (ret_blocked, ret_unbound)
	) 
	(false,false) r_array in
      (* If any are blocked -> the return is too *)
      if any_blocked then
	Blocked
      else (* If they are all unbound or not at all,
	      and there is at least one unbound, then 
	      return is unbound *)
	if any_unbound then 
	  Unbound_input_tuples
	else (* They are no unbound *)
	  No_unbound
  in
    (* The actual walking code *)
  let rec op_helper op =     
    if (is_input_tuple op) then
      Unbound_input_tuples
    else if (is_access_tuple op) then
      (* If it is accessed in scope, we do not push down *)
      Blocked
    else if (is_group op) then
      begin
	Blocked 

(*	(* If there is no input tuple underneath, then we are ok *)	 
	if (walk_independent_expression op) = No_unbound then
	  No_unbound
	else
	  Blocked *)
      end
    else walk_independent_expression op
  and walk_independent_expression op =
    match op.psub_expression with
      | NoSub -> No_unbound
      | OneSub s -> op_helper s
      | TwoSub (s0,s1) ->
	  calculate_return [|(op_helper s0); (op_helper s1)|]
      | ManySub sa ->
	  calculate_return (Array.map op_helper sa)
  in
  let ret = op_helper start_op in
    
    match ret with
      | Unbound_input_tuples -> true
      | _ -> false

(* This assumes it is ok to do this operation 
   (i.e. the above has been called and returned true)*)
let rec replace_input_tuple_in_scope replace op =
  let input_replace x =
    if is_input_tuple x then
      begin
	replace
      end
    else 
      begin
	replace_input_tuple_in_scope replace x;
	x
      end	
  in
    if (is_access_tuple op) then
      ()
    else
      begin
	let new_input_sub_expression =
	  match op.psub_expression with
	    | NoSub -> NoSub
	    | OneSub s -> OneSub (input_replace s)
	    | TwoSub (s0,s1) ->
		TwoSub ((input_replace s0), 
			(input_replace s1))
	    | ManySub sa ->
		ManySub (Array.map input_replace sa)
	in	
	  op.psub_expression <- new_input_sub_expression 
      end

(***************************************)
(* MapConcat{D_1}(I_1) somewhere in D_1 
   there are valid input tuples and a 
   MapConcat with input InputTuple
   *****************************************
   *** THIS WILL REPLICATE THE OPERATION ***
   *****************************************
*)
(* If this replicates, it should      *)
(* generate new tuple names           *)
let push_through_map op =  
  let return_failure = op, false in
  let mc = op in
    if (is_a_map_concat mc) then
      begin
	let it_replace = access_onesub mc.psub_expression in 
	let dep        = access_onesub mc.pdep_sub_expression in
	  if (has_open_scoped_tuples dep) then
	    begin	 	      
	      if (is_input_tuple it_replace) then
		return_failure
	      else 	    
		begin
		  let replace_function_walker = 
		    replace_input_tuple_in_scope it_replace 
		  in
		    replace_function_walker dep;
		    dep, true
		end
	    end
	  else return_failure
      end
    else return_failure
	
	  
(*
  Tuple field replacement should be used
  for grouping. 

  This rewrite is only valid if each element has cardinality 1 
  
*)

(*************************************)
(* This only works if tn is unique.  *)
(* We completely ignore scoping here *)
(*    Ensure Tuple names are unique  *)
(*************************************)

let tuple_field_replacement vn replace_vn comp_ctxt algop =
  match algop.palgop_expr_name with
    | AOEAccessTuple v ->
	if v = vn then
	  begin
	    let eh      = algop.palgop_expr_origin in
	    let fi      = algop.palgop_expr_loc    in

	    let new_op  = logical_aalgop_mkop (AOEAccessTuple replace_vn) NoSub NoSub None eh fi in
	      new_op, true
	  end
	else
	  algop, false
    | AOECreateTuple a ->
	(* modifiy the create in place *)
	let is_modified = ref false in
	for i = 0 to (Array.length a) - 1 do
	  let a_type, a_vn = a.(i) in
	  if a_vn = vn then
	    begin
	      a.(i) <- (a_type, replace_vn);
	      is_modified := true
	    end
	  ;
	done;
	  algop, !is_modified
    | _ -> algop, false
	    

let subst_wrapper_tuple comp_ctxt vn replace cur_op = 
  let subst_changed = ref false in
  let wrapped_apply = generic_wrapper (tuple_field_replacement vn replace) in
(*  let rule_apply    = wrapped_apply comp_ctxt subst_changed cur_op in*)
  let rule_apply    = wrapped_apply subst_changed comp_ctxt cur_op in
  let ret_value     =
    rewrite_apply rule_apply None cur_op in
    ret_value, !subst_changed

(****************************************)
(*      |---- indep_1 ---------------|  *)
(* [x : TIMap{AccessTuple#y}(indep_2)]  *)
(*           |--- dep_2 ---|            *)
(*                                      *)
(*   indep_2 \ #y --> #x                *)
(*                                      *)
(* Eliminates wrapping/unwrapping       *)
(*   of Single items (happens in lets)  *)
(****************************************)
(* THIS RULE IS WRONG IT IS JUST FOR TESTING *)   
let tuple_field_replacement_rule comp_ctxt root algop =
  let no_replace_return = algop, false in
  match algop.palgop_expr_name with
    | AOECreateTuple a when (Array.length a) = 1 ->
	let x_type, x_name = a.(0) in
	let indep   = access_manysub algop.psub_expression in
	  if ((Array.length indep) != 1) then
	    raise (Query (Malformed_Algebra_Expr ("AOECreate with 1 tuple name does not have 1 indep sub (has "
					    ^ (string_of_int (Array.length a))^ ")")))
	  ;
	let indep_1 = indep.(0) in
	if (is_a_map_to_item indep_1) then
	  begin
	    let dep_2   = access_onesub indep_1.pdep_sub_expression in 
	    let indep_2 = access_onesub indep_1.psub_expression     in
	    match dep_2.palgop_expr_name with
	      | AOEAccessTuple y_name ->
		  (* trigger the replacement *)
		  let op, _ = subst_wrapper_tuple comp_ctxt y_name x_name indep_2 in
		    op, true (* we changed *)
	      | _ -> no_replace_return
	  end
	else no_replace_return
    | _ -> no_replace_return


(* Group rewrites *)
(************************************)
(* GroupBy Introduction Rewrite      *)
(************************************)
(*
  The general rule is:
  
  P1 : Seq U -> T1
  P2 : R -> U
  P3 : T -> Seq [R]
  [R] = [a1:U1;..an:Un]
  ---------------------------------
  [ x: P1 o (Map P2 P3) ]
  == 
  GroupBy [x] [] [null] P1 P2 (OMap[a1 .. an][null]  ID P3)  

  But since our groupby doesn't contain the aggreg. function (P1 here), 
  what the implementation does is:
   
  P2 : R -> U
  P3 : T -> Seq [R]
  [R] = [a1:U1;..an:Un]
  ---------------------------------
  [ x: Map P2 P3 ]
  == 
  GroupBy [x] [] [null] P2 (OMap[a1 .. an][null]  ID P3)    

*)

  
(* If indep is the input tuple, we do not create a group because this
   is necessary a singleton *)

let groupby_introduction_rule comp_ctxt algop = 
  let no_replace_return = algop, false in
  let eh = algop.palgop_expr_origin in
  let fi = algop.palgop_expr_loc in
  match algop.palgop_expr_name with
    (* currently only dealing with single groups *)
  | AOECreateTuple a  when (Array.length a) = 1 ->
      let x = a.(0) in
      let map_to_item = (access_manysub algop.psub_expression).(0) in
      if is_a_map_to_item map_to_item 
          (* &&	(not (Optimization_judge.has_non_trivial_snap comp_ctxt algop)) *)
      then
	begin
	  let indep   = access_onesub map_to_item.psub_expression in
	  (* is_input_tuple indep *)
	  if Optimization_judge.is_singleton_tuple comp_ctxt  indep
	  then no_replace_return
	  else begin
	    let dep     = access_onesub map_to_item.pdep_sub_expression in
	    let fresh_name, map_index_op = wrap_map_null comp_ctxt indep  in
	    let group_desc = mk_group_desc [] [] [fresh_name] x in
	    let group_name = AOEGroupBy (group_desc :: [])      in
	    let new_op  = logical_aalgop_mkop group_name (OneSub map_index_op) (ManySub [|dep|]) None eh fi in
	    new_op, true
	  end
	end
      else no_replace_return
  | _ -> no_replace_return

(***********************************************)
(* MapConcat Group Creation Rewrite            *)
(* MapConcat{[x : e1)]}(Group X)               *)
(*      ----   ----                            *)
(*                                             *)
(* Group [ X x, e1] :: X                       *)
(* The important thing is that the description *)
(* is the same except for x and e1             *)
(*                                             *)
(***********************************************)

let map_concat_group_creation_rule map_concat = 
  let no_replace_return = map_concat, false in
  let eh      = map_concat.palgop_expr_origin in
  let fi      = map_concat.palgop_expr_loc    in
    
    if (is_a_map_concat map_concat) then
      begin
	let tuple_create = access_onesub map_concat.pdep_sub_expression in
	let group_op     = access_onesub map_concat.psub_expression     in
	  match (tuple_create.palgop_expr_name, group_op.palgop_expr_name) with	
	      (* currently only dealing with single groups *)
	    | ((AOECreateTuple a), (AOEGroupBy ((gd_head :: rest) as gd)))
		when (Array.length a) = 1
	      ->
		let x = a.(0) in 
		let e1 = (access_manysub tuple_create.psub_expression).(0)in 	  
		
		let group_desc = mk_group_desc (get_group_names gd_head) (get_induced_group gd_head) (get_valid_names gd_head) x in 		  
		let group_name = AOEGroupBy (group_desc :: gd)      in
		
		  (* Group [ [t], t, x, e1,false]        *)
		  (*    (SepSequence[(t)] (indep))        *)
		let deps = access_manysub group_op.pdep_sub_expression in
		let g1 = ManySub (Array.append [|e1|] deps) in
		let new_op  = logical_aalgop_mkop group_name (group_op.psub_expression) g1 None eh fi in      
		  new_op, true
	    | _ -> no_replace_return
      end
    else
      no_replace_return

(********************************************)
(*   Maps and Group                         *)
(*   --------------                         *)
(* Map{Group (Indep_1)                      *)
(*     by Name_1                            *)
(*      apply {Dep_1} }(Indep_2)            *)
(*                                          *)
(* Cond: Map must return the fields         *)
(*         fields of Indep_2                *)
(*            =========                     *)
(* If there is no input tuple outstanding   *)
(*   inscope .....                          *)
(* Group ( MapOuter{Indep_1}                *)
(*   (MapIndex(i)(Indep_2)))                *)
(*	by i :: Name_1                      *)
(*	apply {Dep_1}                       *)
(********************************************)
(* If there is a tuple outstanding in scope *)
(* replace_scoped_input_tuple with          *)
(*    MapIndex(i)(Indep_2)                  *)
(*    -----------------------------------   *)
(* This flattens the nested scope           *)
(********************************************)
(* THIS IS NOT CORRECT.
   DOES NOT CORRECTLY HANDLE MISSING VALUES 
*)

let push_mapconcat_through_group_rule comp_ctxt algop =
  let no_replace_return = algop, false in
  let eh      = algop.palgop_expr_origin in
  let fi      = algop.palgop_expr_loc    in

    if (is_any_map_concat algop) then
      begin	
	    let the_map     = algop in  
	    let the_group   = access_onesub algop.pdep_sub_expression in

	      match the_group.palgop_expr_name with
	        | AOEGroupBy gd_list ->
		        begin
                  begin
                    if Debug.materialization_debug() then
                      match !(algop.annotation) with
                        | None -> print_endline "map_through_group_rule: no annotation found in GroupBy op"
                        | Some annot ->
                            match annot.path_analysis with
                              | None -> print_endline "map_through_group_rule: no analysis found in GroupBy op"
                              | Some anlz ->
                                  print_endline "map_through_group_rule: found path annotation"; Alg_path_analysis.print_intermediate_analysis Format.std_formatter anlz
                  end;

                  let dep_1_array  = access_manysub the_group.pdep_sub_expression in (* array of P2s *)   
		          let indep_1  = access_onesub the_group.psub_expression in (* P3 *)
		          let indep_2  = access_onesub the_map.psub_expression in	(* P4 *)
	                if not (Optimization_judge.commute_logical_with_array comp_ctxt indep_1 dep_1_array) then
                      no_replace_return
                    else              
		              let g_eh     = the_group.palgop_expr_origin in
		              let g_fi     = the_group.palgop_expr_loc in
                        
		              let fresh_name, inner_op = wrap_sep_sequence comp_ctxt indep_2  in
		                
		              (* Wrap Map Index is not the correct one
		                 the indep_2 is because this is the 
		                 real LHS *)
		              let map_induced = algop_get_returned_fields indep_2 in
		              let op_name = 
		                match algop.palgop_expr_name with
		                  | AOEMapConcat
		                  | AOEOuterMapConcat _ -> algop.palgop_expr_name
		                  | _ -> raise (Query (Malformed_Algebra_Expr ("This should ONLY be an outermap or a mapconcat")))
		              in
		              let omap = logical_aalgop_mkop op_name (OneSub inner_op) (OneSub indep_1) None eh fi in
		                (* Make the sequence description, and the code
		                   to access the sort *)		 
		                (* Group by these vars *)
		              let gd_list     = add_valid_name_to_group_desc fresh_name gd_list in
		              let gd_list     = add_name_to_group_desc fresh_name map_induced gd_list       in 
		              let group_name  = AOEGroupBy gd_list in
		                
		              let the_group       = logical_aalgop_mkop group_name (OneSub omap) the_group.pdep_sub_expression
					    None g_eh g_fi in
		                the_group, true
		        end
	        | _ -> no_replace_return
      end
    else no_replace_return


(* Group Apply Flatten
   The point is to denest the apply statement 
   ----- Push down to different grouping levels
   
   Group X=[[ ...., MapToItem{D_1}(I_1) ]] (Input)

   Notice I_1 must have a dependence on the input tuple.
   Cond: I_1 != InputTuple
         |X| = 1 - can only push down on the last one for now

   Group [..., MapToItem{D_1}(InputTuple) ] (I_1 \InputTuple -> Input)

THIS RULE IS WRONG. COUNTER EXAMPLE.
TURNED OFF FOR NOW. - Jerome

  GroupBy[x][out]
    {MapToItem{IN#x}(MapConcat{()}(IN))}
    ([x : 2])
=>
  [ out : () ]


  GroupBy[x][out]
    {MapToItem{IN#x}(IN)}
    (MapConcat{()}([x : 2]))
=>
  ()  

*)

let group_apply_flatten_rule comp_ctxt algop = 
  let return_not_applied = algop, false in
  let eh     = algop.palgop_expr_origin in
  let fi     = algop.palgop_expr_loc in

    match algop.palgop_expr_name with
      | AOEGroupBy gd_list when (List.length gd_list) = 1 ->	  
	  begin
	    let the_group = algop in
	    let input     = access_onesub the_group.psub_expression in
	    let dep_1     = (access_manysub the_group.pdep_sub_expression).(0) in 
	      if is_a_map_to_item dep_1 then (* It should be... *)
		begin
		  let i_1 = access_onesub dep_1.psub_expression in 
		    (* Check to make sure we don't execute forever *)
		    if not (is_input_tuple i_1) then 
	  	      begin		  
	  		let input_tuple = logical_aalgop_mkop AOEInputTuple NoSub NoSub None eh fi in
	  		  (* Assign the input tuple to the TIMap *)
	  		  dep_1.psub_expression <- OneSub input_tuple;
			  (* Now fill in the dependece *)
			  (* Replace the input tuple in i_1 with the input to the group *)
			  let filled = input_tuple_replace input None i_1 in
			    if filled then
			      begin
				the_group.psub_expression <- (OneSub i_1);
				the_group, true				  
			      end
			    else 
			      (* this could happen if the apply expression is a constant *)
			      (* Though it would be nicer to be an exception... *)
			      return_not_applied
	  	      end
		    else return_not_applied
		end
	      else return_not_applied
	  end
      | _ -> return_not_applied

(*****************)
(* Group combine *)
(*****************)
(* We must group by the 
   most fine grained (i.e. the last)
   group from the outer on each one

THIS RULE IS WRONG.
TURNED OFF FOR NOW. - Jerome

*)
let prepend_grouping gd cur = 
  let gb_names = Gmisc.remove_duplicates ((get_group_names gd) @ (get_group_names cur)) in 
  let induced  = get_induced_group cur  in
  let mbv      = Gmisc.remove_duplicates ((get_valid_names gd) @ (get_valid_names cur)) in 
  let agg_name = get_aggregate_name_and_type cur in
    mk_group_desc gb_names induced mbv agg_name 

let group_desc_combine gd_out gd_in =
(*  let dump_gd g =
    print_endline
      (List.fold_left (^) "Valid: " 
	 (List.map (fun x -> " "  ^ (Namespace_names.prefixed_string_of_rqname x))
	    (get_valid_names g)))
  in
    print_endline "**** COMBINE ***";
    print_string "OUTER: "; List.iter dump_gd gd_out;
    print_string "INNER: "; List.iter dump_gd gd_in; *)
				    
  let the_last_gd = List.nth gd_out ((List.length gd_out) - 1) in
  let gd_in       = List.map (prepend_grouping the_last_gd) gd_in in
    gd_out @ gd_in 

let group_combine comp_ctxt g1 =
  let eh     = g1.palgop_expr_origin in
  let fi     = g1.palgop_expr_loc in
  let return_not_applied = g1, false in
    match g1.palgop_expr_name with
      | AOEGroupBy g1_list ->
	  begin
	    let g2 = access_onesub g1.psub_expression in 
	      match g2.palgop_expr_name with
	  	| AOEGroupBy g2_list ->
	  	    let g1_dep   = access_manysub g1.pdep_sub_expression in
	  	    let g2_dep   = access_manysub g2.pdep_sub_expression in
	  	    let g2_indep = access_onesub  g2.psub_expression     in
	  	    let new_dep  = ManySub (Array.append g1_dep g2_dep)  in
		    let new_gd   = group_desc_combine g1_list g2_list    in
	  		    let new_op   =
	  		      logical_aalgop_mkop (AOEGroupBy new_gd) (OneSub g2_indep) new_dep None eh fi 
	  		    in
	  		      new_op, true
	  	| _ -> return_not_applied
	  end
	        | _ -> return_not_applied

(* Put in Map(Product(InputTuple,..))(E_1) ==> Product(E_1,...)
          Map(Join(InputTuple,..)(E_1) ==> Join(E_1,..) *)

(* Map{Join(InputTuple, indep_2)}(indep_1) 

   ===

   Join(indep_1, indep_2)

   Same holds for Product. Order matters. Must be on the left.

Not implemented:
   If it is an outer map then we have the following rule:
   OMap[(v)]{Join(InputTuple, indep_2)(indep_1) --
     LeftOuterJoin[(v)](indep_1, indep_2)

   OMap[(v)]{LeftOuterJoin[(v1)](InputTuple, indep_2)}(indep_1)
   
   LeftOuterJoin[(v)](indep_1, indep_2)

*)
let map_singleton_join_or_product_rule comp_ctxt algop =
  let return_failure = algop, false in
  if (is_a_dep_map algop) && 
     (not (is_an_outer_mapconcat algop)) then
    begin
      let the_map = algop in
      let dep_0   = access_onesub the_map.pdep_sub_expression in 
      let indep_0 = access_onesub the_map.psub_expression     in
	if (is_any_join dep_0) ||
	   (is_product dep_0)then
	     begin
	       let join_or_product = dep_0 in	       
	       let l,r = access_twosub join_or_product.psub_expression in 
		 if (is_input_tuple l) 
		   && (not (Optimization_judge.has_non_trivial_snap comp_ctxt r))
		   && (not (Optimization_judge.has_non_trivial_snap comp_ctxt indep_0))
		 then
		   begin
		     join_or_product.psub_expression <- TwoSub (indep_0,r);
		     join_or_product, true
		   end
		 else return_failure
	     end
	else return_failure	       		
    end 
  else return_failure


(*
   P2: T -> Seq[R2]
   P3:[R2]->[a1:T1..an:Tn]
   P1:[R2;a1:T1;..an:Tn] -> Seq Item
   -----------------------------------------------------------------------------
   OMapConcat[a1..an][null] (Join P1 ID P3) P2
   ===
   LOuterJoin[a1..an][null] P1 P2 P3 : T -> Seq[a1:T1;..;an:Tn;R2;null:Bool]

   Note: there are some issues here with concatenating tuples, coercion etc. Need
   to be figured out at some point.

   Comment from previous developer: Same holds for Product. Order matters. Must be on the left.
                                    Wonder if it's true.
 *)
let left_outer_join_introduction_rule comp_ctxt root algop =
  let return_failure = algop, false in
  let eh     = algop.palgop_expr_origin in
  let fi     = algop.palgop_expr_loc in
    
    if is_an_outer_mapconcat algop then
      begin
	    let null_name = get_outer_map_name algop in 
	    let the_map   = algop in
	    let dep_0     = access_onesub the_map.pdep_sub_expression in 
	    let indep_0   = access_onesub the_map.psub_expression     in
	      match dep_0.palgop_expr_name with

	        | AOENullMap vn ->
                (* Nicola: this part should become a separate rule --> turn OMapConcat (OMap ) ... into OMapConcat ...*)
		        begin
		          let join = access_onesub dep_0.psub_expression in
		            match join.palgop_expr_name with		      
		              | AOEJoin pred ->
			              begin
			                let l,r    = access_twosub join.psub_expression in 
			                  
			                  if (is_input_tuple l) 
                                (* 			      	&& (Optimization_judge.has_trivial_snap comp_ctxt r) *)
                                (* 				        && (Optimization_judge.has_trivial_snap comp_ctxt indep_0) *)
			                  then
				                begin
				                  (* We need to merge the tuple field names, that 
				                     is rename occurences of vn -> null_name *)
				                  let new_root,changed = replace_tuple_name comp_ctxt vn null_name root in
				                    if not (new_root = root) then
				                      raise (Query (Optimization ("Tuple renaming changed root")));
				                    let new_op = logical_aalgop_mkop (AOELeftOuterJoin (null_name,pred)) (TwoSub(indep_0, r)) 
				                      join.pdep_sub_expression None eh fi in
				                      new_op, true
			                    end
			                  else
				                return_failure
			              end
		              | _ -> return_failure   
		        end
	        | AOEJoin pred ->
                (* that's the real rule *)
		        begin
		          let l,r     = access_twosub dep_0.psub_expression in 
                  let conds   = access_manysub dep_0.pdep_sub_expression in (* join conditions *)
		            if (is_input_tuple l) 
                      && Optimization_judge.commute_logical_with_array comp_ctxt r conds
		            then
		              begin
		                let new_op = logical_aalgop_mkop (AOELeftOuterJoin (null_name,pred)) (TwoSub(indep_0, r)) 
			              dep_0.pdep_sub_expression None eh fi in
		                  new_op, true
		              end
		            else
		              return_failure
		        end
	              (* |  We can't rewrite through products.. *)
	              (* | AOELeftOuterJoin (v,pred) ->  *)
		          (* The rewrite is the following *)
		          (* OMap[(v1)]{LeftOuterJoin[(v2)](InputTuple, indep_2)}(indep_1)
		             rename v1 -> v2 *)
	        | _ -> return_failure
      end
    else return_failure
        
(*

This is a bit nasty. It happens in nesting.
These indexes are everywhere and their 
properties are strange. We can only rely
on successor of them... DOCUMENT THIS


  MapIndex
   Join ( InputTuple, 
          R ) on X
---- 
  Join (InputTuple,
          MapIndex R) on X
*)
(* THIS DOES NOT APPEAR TO BE USED ANYMORE - Chris *)
let singleton_join_index_move comp_ctxt algop =
  let return_failure = algop, false in
  if is_a_map_index algop then
    begin
      let join = access_onesub algop.psub_expression in 
	if is_regular_join join then
	  begin
	    let l, r = access_twosub join.psub_expression in 
	      if is_input_tuple l then
		begin
		  (* 1. switch the index to the R *)
		  algop.psub_expression <- (OneSub r);
		    
		  (* 2. switch the join to use the map_indexed R as its new R *)
		  join.psub_expression <- (TwoSub (l, algop));
		  
		  (* 3. return the new *)
		  join, true
		end
	      else
		return_failure
	  end
	else return_failure
    end 
  else return_failure



(* PushDown Product Rule *)
(*
  MapConcat{D_1}
     (Product(I_1, I_2))
  
  ----
  COND:  accessed D_1 subset returned(I_2) (w/o loss)

  Product(I_1,
          MapConcat{D_1}(I_2))

  Notice this rule does not hold in general.
  Consider :
  MapConcat{D_1}
    (Select {Input[1]}
            (I_1))

  an application of D_1 could return a sequence S but, only the first
  element of the seqeunce would be returned should we try to write:
  
  Select {Input[1]}
    MapConcat{D_1}(I_1)
  
  This one was chosen since it helps ease detecting joins.
*)

let pushdown_map_in_product_rule comp_ctxt algop =
  let return_failure = algop, false in
  let mc = algop in
    if is_a_map_concat mc then
      begin
	let dep     = access_onesub mc.pdep_sub_expression in 
	let product = access_onesub mc.psub_expression in 
	  if is_product product then
	    begin
	      let l,r = access_twosub product.psub_expression in 
		if Optimization_judge.natural_tuple_independent dep l then
		  begin
		    (* This means we should push down on r *)		    
		    mc.psub_expression <- (OneSub r);
		    product.psub_expression <- TwoSub (l, mc);
		    product, true
		  end		  
		else if Optimization_judge.natural_tuple_independent dep r then
		  begin
		    (* This means we should push down on l *)		    
		    mc.psub_expression <- (OneSub l);
		    product.psub_expression <- TwoSub (mc, r);
		    product, true
		  end		  
		else return_failure (* Can't push down on either *)
	    end
	  else return_failure
      end
    else return_failure


(* 
   MapIndex[(v)]( 
     Product(InputTuple, R) ) 
  ===> Product(InputTuple, MapIndex[(R)]
   
*)
let pushdown_mapindex_through_product op = 
  let return_failure = op, false in
  if is_a_map_index op then
    begin
      let product = access_onesub op.psub_expression in
	if is_product product then
	  begin
	    let l,r = access_twosub product.psub_expression in 
	      if is_input_tuple l then
		begin
		  op.psub_expression <- OneSub r;
		  product.psub_expression <- TwoSub(l, op);
		  product, true
		end
	      else return_failure
	  end
	else return_failure
    end
  else return_failure


(* *** Map pushdown rule *** *)

(* You want to push down the maps that depend only on one branch.  For      *)
(* example MapConcat{[y: #x]}MapFromItem{[x: }(doc1) Should be a            *)
(* branch. This needs to be pushed down.  There are functions for which     *)
(* this is a bad idea - for example an arbitrarily function that is         *)
(* expensive to compute -> you would not want to compute this for           *)
(* each. However, if it is cheap to compute (i.e. an XPath step) or         *)
(* requires only one pass over the document, then we should push it         *)
(* down.                                                                    *)

(*  MapConcat { dep } (indep(indep_1, .. indep_k))                          *)
(*                                                                          *)
(*  COND 1a: If exist unique i s.t. accessed (MapConcat{ }) subset indep_i  *)
(*       1b: returned fields != empty_set (can't push through to items)     *)
(*  COND [cheap]: If is_cheap dep (* right now true *)                      *)
(*                                                                          *)
(* indep(MapConcat{dep}(indep_1), ... )                                     *)

(* This problem seems to only affect Selections, since they have similiar   *)
(* pushdown rules. If we add more operators that have pushdown              *)
(* characteristics, we need to be aware of this.                            *)

(*   PLEASE DO NOT DELETE THESE NOTES UNLESS YOU                            *)
(*   REPLICATE THE NOTES ELSEWHERE - chris                                  *)

(********* CHANGE *******)   
(* CHANGE: (cond 2) We check: the pushdown is done only if  *)
(* the maps are not independent                             *)
let map_concat_pushdown_cond1 dep indep =
  let dep_accessed   = algop_get_accessed_fields dep in
  let dep_returned   = algop_get_returned_fields dep in
  let indep_accessed = algop_get_accessed_fields indep in 
  let indep_returned = algop_get_returned_fields indep in
    Gmisc.is_subset dep_accessed indep_returned && (* cond 1a *)
    (dep_returned <> [])  &&                  (* cond 1b *)
    (indep_returned <> []) &&                 (* cond 2a *) 
    (indep_accessed <> []) && (dep_accessed <> []) &&
    (not (Gmisc.is_subset indep_accessed dep_returned)) (* cond 2b *)

      

(* I want this local to a branch. *)
type exists_unique_field = 
		  Invalid
		| UniqueIndex of int
		| None_found
      

let rec map_concat_pushdown comp_ctxt algop = 
  let return_failure = algop, false in
    if (is_a_map_concat algop) && 
      (* CHOICE FOR TERMINATION - Chris *)
      (not (is_select (access_onesub algop.psub_expression))) &&
      (not (is_project (access_onesub algop.psub_expression)))
    then
      begin
	let dep = access_onesub algop.pdep_sub_expression in
	let main_indep = access_onesub algop.psub_expression in
	  match main_indep.psub_expression with
	    | NoSub -> return_failure
	    | OneSub s -> 
		(* BUGFIX: if main_indep again is a MapConcat, you end up switching around
                           MapConcats eternally -- Philippe *)
		if map_concat_pushdown_cond1 dep s && not(is_a_map_concat main_indep) then
		  begin
		    algop.psub_expression <- (OneSub s);
		    (* This has advanced it, now call the rule recurisvely *)
		    let main_child,v = map_concat_pushdown comp_ctxt algop in
		      main_indep.psub_expression <- OneSub main_child;
		      main_indep, true
		  end
		else return_failure

	    | TwoSub (s0,s1) -> 
		let ok1 = map_concat_pushdown_cond1 dep s0 in
		let ok2 = map_concat_pushdown_cond1 dep s1 in
		  if (* XOR *) (not (ok1 && ok2)) && (ok1 || ok2) then
		    begin
		      if ok1 then
			begin
			  algop.psub_expression <- (OneSub s0);
			  main_indep.psub_expression <- (TwoSub (algop,s1));
			  main_indep, true
			end
		      else if ok2 then
			begin
			  algop.psub_expression <- (OneSub s1);
			  main_indep.psub_expression <- (TwoSub (s0,algop));
			  main_indep, true
			end
		      else raise (Query (Malformed_Algebra_Expr ("XOR calculated incorrectly")))
		    end
		  else return_failure

	    | ManySub sa ->
		begin
		  let exists_unique (cur_index, unique_index) cur =
		    let ok_cur       = map_concat_pushdown_cond1 dep cur in
		    let index = 
		      match (unique_index,ok_cur) with
			| (Invalid, _) -> Invalid
			| (UniqueIndex _, true) -> (* not unique *) Invalid
			| (UniqueIndex _, false) -> unique_index (* Still unique*)
			| (None_found, false) (* no change *) -> unique_index
			| (None_found, true) -> UniqueIndex cur_index
		    in
		      (cur_index +1, index)
		  in
		  let _, is_unique =  Array.fold_left exists_unique (0, None_found) sa in
		    match is_unique with
		      | None_found | Invalid
			  -> return_failure
		      | UniqueIndex ui -> 
			  begin
			    algop.psub_expression <- (OneSub sa.(ui));
			    (access_manysub main_indep.psub_expression).(ui) <- algop;
			    main_indep, true
			  end		    
		end      
      end
    else return_failure
 

(* Singleton Map inline 
   Map{dep}(indep)
   indep is (AOEConcat? ((InputTuple|AOECreate|AOEConcat),
                         (InputTuple|AOECreate|AOEConcat)) (and valid)
   (Now we can associate to every field x an independent expression to inline)
   
   forall field in returned(indep)
      field is accessed once    
   Map is MapToItem
   ------------------------
   dep[Input#x -> ]
*)
type singleton_map_inline_return =
  | SMI_no_match
  | SMI_does_match of (Xquery_common_ast.crname * Ast_logical_algebra_types.logical_algop_expr) list

let combine_singleton_map_inline_return l r = 
  match (l,r) with
    | (SMI_no_match,_) | (_,SMI_no_match) -> SMI_no_match
    | (SMI_does_match l, SMI_does_match r) ->
	SMI_does_match (l @ r)

let rec check_indep_pattern algop =
  match algop.palgop_expr_name with
  | AOEInputTuple    -> 
      SMI_does_match []
  | AOECreateTuple names ->
      if (List.for_all (fun (typ,_) -> typ=None) (Array.to_list names))
      then
	let subs = access_manysub algop.psub_expression in 
	let create_smi_return index (typ,name) = (name, subs.(index)) in
	SMI_does_match (Array.to_list (Array.mapi create_smi_return names))
      else
	SMI_no_match
(*
  | AOEConcatTuples  ->
      let l,r = access_twosub algop.psub_expression in 
      combine_singleton_map_inline_return
	(check_indep_pattern l)
	(check_indep_pattern r)
*)
  | _ ->
      SMI_no_match

(* Because field names are unique, we do not have to worry about
   scoping and can just replace on name alone. 
*)
let inline_map_helper names algop = 
  let rec do_op op = 
    match op.palgop_expr_name with
      | AOEAccessTuple name ->
	  if List.mem_assoc name names
	  then List.assoc name names
	  else op
      (* Why isn't create tuple in here? *)
      | _ ->
	  op.psub_expression     <- inline_subexpr op.psub_expression;
	  op.pdep_sub_expression <- inline_subexpr op.pdep_sub_expression;
	  op
  and inline_subexpr sexpr =
    match sexpr with
    | NoSub          -> NoSub
    | OneSub o       -> OneSub (do_op o)
    | TwoSub (o1,o2) -> TwoSub ((do_op o1), (do_op o2))
    | ManySub ops    -> ManySub (Array.map do_op ops)
  in
  do_op algop

(* The actual rewrite rule *)
let singleton_map_inline comp_ctxt root algop = 
  if is_a_map_to_item algop then
    begin
      let indep = access_onesub algop.psub_expression in 
      let dep   = access_onesub algop.pdep_sub_expression in 
      (* let debug_copy = Xquery_algebra_ast_util.deep_copy_expr algop in  *)
      (* Cannot inline any expression that has side effects *)
      if (not(Optimization_judge.contains_update indep)) then 
	match check_indep_pattern indep with
	| SMI_no_match         -> algop, false
	| SMI_does_match names ->
	    begin
	    (* check that only once *)
	      let counts = count_tuple_field_access dep in
	      if (List.for_all (fun (fn,count) -> count = 1) counts) && ((List.length counts) > 0)
	      then
		begin 
		  let return = inline_map_helper names dep in 
		  return, true
		end
	      else algop, false
	    end
      else
	algop, false
    end
  else algop, false


(* this one's a hack and is incorrect ! -Ph *)   
let treepattern_join_support ctxt sel =
  let seh     = sel.palgop_expr_origin in
  let sfi     = sel.palgop_expr_loc in
    
  match sel.palgop_expr_name with
  | AOESelect p ->
      begin
	let prj = access_onesub sel.psub_expression in
	let selops = access_manysub sel.pdep_sub_expression in
	match prj.palgop_expr_name with
	| AOEProject f ->
	    begin
	      let ttp = access_onesub prj.psub_expression in
	      match ttp.palgop_expr_name with
	      | AOETupleTreePattern _ ->
		  begin
		    let eh = ttp.palgop_expr_origin in
		    let fi = ttp.palgop_expr_loc in

let input = logical_aalgop_mkop (AOECreateTuple [||])
			(ManySub [||]) NoSub None eh fi in
		    let mapc  = logical_aalgop_mkop AOEMapConcat 
			(OneSub input) (OneSub ttp) None eh fi in
		    let select' = logical_aalgop_mkop (AOESelect p) 
			(OneSub mapc) (ManySub selops) None seh sfi in
		    let project' = logical_aalgop_mkop (AOEProject f) 
			(OneSub select') NoSub None seh sfi in
		    project', true
		  end
	      | _ -> sel, false
	    end
	| _ -> sel, false
      end
  | _ -> sel, false

let push_outermap_through_project ctxt op =
  let eh     = op.palgop_expr_origin in
  let fi     = op.palgop_expr_loc in
  match op.palgop_expr_name with
  | AOEOuterMapConcat g ->
      begin
	let dep = access_onesub op.pdep_sub_expression in
	let indep = access_onesub op.psub_expression in
	match dep.palgop_expr_name with
	| AOEProject f ->
	    begin
	      let fields = Array.of_list (algop_get_returned_fields indep) in
	      let indep' = access_onesub dep.psub_expression in
	      let omapc' = logical_aalgop_mkop (AOEOuterMapConcat g)
			(OneSub indep) (OneSub indep') None eh fi in
(* fixme, should merge fields here! *)
	      let fa = Array.append (Array.append f fields) [|g|] in
	      let project' = logical_aalgop_mkop (AOEProject fa)
			(OneSub omapc') NoSub None eh fi in
	      project', true
	    end
	| _ -> op, false
      end
  | _ -> op, false

let remove_project_dont_care_about_correctness ctxt op =
  match op.palgop_expr_name with
  | AOEProject f -> access_onesub op.psub_expression, true
  | _ -> op, false

(**************************************)
(* Top level application of the rules *)
(**************************************)

let top_level_apply comp_ctxt has_changed root (rule, name)  =
  let _ = if Debug.default_debug() then Debug.sprintf_default_debug " *** Rewrite attempt: %s *** \n%!" name  in
  let this_rule_applied = ref false in
  let rule_apply      = rule (* has_changed *) this_rule_applied comp_ctxt root in
  let result          = rewrite_apply rule_apply None root in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
    has_changed := !has_changed || !this_rule_applied;  
    if (!has_changed && proc_ctxt.infer_independence) then
      Alg_path_analysis.path_analysis_of_logical_algop_expr root; (* redo the path analysis *)
    if (!Conf.print_algebra_optimization_rewrite) &&
      (!this_rule_applied) then
	begin
	  Format.fprintf !Conf.algebra_optimization_rewrite_formatter 
	    "@?**** Rule Applied: %s ****@.@\nBefore:@\n%a@.After:@\n%a@."
	    name
	    (fun ff x -> Print_xquery_algebra.fprintf_logical_algstatement ff "" x) root 
	    (fun ff x -> Print_xquery_algebra.fprintf_logical_algstatement ff "" x) result
	end
    ;
    result


(* List of rewrite rules *)
(****************************)
(* Phase 1: Clean up Rules  *)
(****************************)
let cleanup_rules = Optimization_rules_cleanup.cleanup_rules

(***************************************************)
(* Phase 2: Syntactic Tuple Tree Join Introduction *)
(***************************************************)

let treepattern_rules =  Optimization_rules_treepattern.tree_pattern_rewrites
(*let treepattern_rules =  Optimization_rewrite_ttp_old.tree_pattern_rewrites *)


(**************************)
(* Phase 3: Pushdown Maps *)
(**************************)
let pushdown_maps = [
  (* Map removal *)    
  (generic_wrapper double_concat_removal), "Remove Double Concat";

  (removal_wrapper nested_null_index), "Remove Nested Null Indexes (Inside Outer Map)";
  (removal_wrapper map_concat_over_omap_into_omapconcat) , "Turn MapConcat over OMap into OMapConcat"; 
  (removal_wrapper double_index_removal), "Double Index Removal";
  (* (removal_wrapper double_null_index_removal), "Double Null Index Removal"; *)
  (generic_snap_free_wrapper map_concat_pushdown), "Pushdown a Map Concat";
  (* This rule seems redundant... - Chris*)
  (* (removal_wrapper map_through_with_join_rewrite), "Push through a group to a Join"; *)
  (generic_wrapper pushdown_selection), "Pushdown Selection";
  (generic_snap_free_wrapper (pushdown_individual Independent)), "Split Predicate and Pushdown Selection";
  (generic_wrapper combine_multiple_selections), "Combine Selections"
]

(**********************************************************)
(* Phase 4: Query Unnesting. Introduce Grouping operators *)
(**********************************************************)
let group_introduction = [
  (generic_wrapper groupby_introduction_rule), "Introduce GroupBy";  

  (************************)
  (* Group rewrites :     *)  
  (************************)  
  (generic_wrapper push_mapconcat_through_group_rule), "Push a map-concat through a group (General)";    
(* (generic_snap_free_wrapper group_combine), "Combine Groups"; *)
(* (generic_snap_free_wrapper group_apply_flatten_rule), "Apply Flatten"; *)
] @ pushdown_maps (* Additionally, keep cleaning up maps *)

(************************************************)
(* Phase 5: Joins. Introduce products and Joins *)
(************************************************)
let join_creation_rules = [ 
  (generic_wrapper natural_product_rewrite), "Natural Product Rewrite";

  (******************************************************************************)
  (* NOTE: THERE IS A RACE CONDITION HERE WITH PUSHDOWNS HAPPEN WITH INDEPENDENT MAPS
     IT IS NOT A PROBLEM BECAUSE THEY WILL BE TURNED INTO A PRODUCT ON THE NEXT 
     RUN. THE COND TO PREVENT THIS SHOULD BE ADDED *)
  (******************************************************************************)
  (generic_wrapper map_product_rewrite), "Branched Product Rewrite";

  (* Join Introduction *)
  (generic_wrapper map_singleton_join_or_product_rule), "Map Over Singleton to Join or Product";
  (removal_wrapper left_outer_join_introduction_rule), "Introduce Left Outer Join";
  (* If this rule is necessary, perhaps we need to introduce another
  round of pushodwns - Chris *)
  (generic_snap_free_wrapper pushdown_map_in_product_rule), "Pushdown a Map into a Product";

  (*** NOTE: join_rewrite, first attempts to push down select conditions more ***)
  (generic_wrapper join_rewrite), "Join Rewrite";

  (* Inlining  *)
  (generic_wrapper singleton_product_to_concat), "Singleton Product to Tuple Concatenation";

(* these are for join robustness or better, for reducing join brittleness *)
(* (generic_wrapper treepattern_join_support), "TTP Join Support"; *)
  (generic_wrapper push_outermap_through_project), "Push OMapConcat through Project";
  (*(generic_wrapper remove_project_dont_care_about_correctness), " Make Projects Disappear";*)
]

(***********************************)
(* Phase 6 (a) Lift Execute rules  *)
(***********************************)
let dxq_rules = 
  List.map (fun (rule, description) -> (generic_snap_free_wrapper rule, description)) Optimization_dxq.lift_execute_rules

(************************************)
(* Phase 6 (b) reintroduce treejoin *)
(************************************)
let treejoin_rules = Optimization_rules_treejoin.treejoin_rewrites
let rule_sbdo = [
  (generic_wrapper Optimization_rules_sbdo.sbdo_rewrite),  "TP x - SBDO Automaton";
  (generic_wrapper Optimization_rules_sbdo.sbdo_rewrite_two),  "TP x - Remove redundant SBDO"
]

let add_rule cond rule =
  if cond 
  then [ rule ]
  else []

(*************************************************)
(* Apply the rewrite_rules in a given phase.     *)
(* Each phase is applied to a fixed point before *)
(* the next phase.                               *)
(*************************************************)
let rec apply_rewrite_phase comp_ctxt algop rewrite_rules = 
  let has_changed = ref false in
  let algop = 
    List.fold_left (top_level_apply comp_ctxt has_changed)
      algop rewrite_rules in

    if !has_changed 
    then apply_rewrite_phase comp_ctxt algop rewrite_rules      
    else algop

let should_add_treepattern proc_ctxt = 
  match proc_ctxt.treejoin_log with
    | Default       -> false
    | TreeJoin      -> true
    | Twig          -> true

let should_revert_to_treejoin proc_ctxt =
  match proc_ctxt.treejoin_log with
    | Default       -> false
    | TreeJoin      -> true 
    | Twig          -> false

	  
(*********************************************
  External : Apply all phases of the rewrite 

  Below, there is an ordered sequence of optimization phases.  Each
  phase applies a set of rules until a fixed-point is reached, then
  the next phase is applied.  We do not repeat phases. 

**********************************************)

let rewrite_expression comp_ctxt start_op = 
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  let rewrite_phases = 
    (add_rule true cleanup_rules)
    @ (add_rule (should_add_treepattern proc_ctxt) treepattern_rules)
    @ (add_rule true rule_sbdo) (* sbdo automaton optim *)
    @ (add_rule true pushdown_maps)
    @ (add_rule true group_introduction)
    @ (add_rule true join_creation_rules) 
    (* ALL OF THESE ARE Phase 6 *)
    @ (add_rule (proc_ctxt.dxq_optimization) dxq_rules) 
    @ (add_rule (should_revert_to_treejoin proc_ctxt) treejoin_rules)
    (* Phase 6 End *)
  in
  (*****************************)
  (* Apply the phases in order *)
  (*****************************)
  List.fold_left (apply_rewrite_phase comp_ctxt) start_op rewrite_phases

