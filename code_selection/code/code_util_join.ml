(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util_join.ml,v 1.13 2007/09/27 19:19:28 simeon Exp $ *)

(* Module: Code_util_join
   Description:
     Some common utilities used for the join algorithms.
*)

open Error

open Xquery_common_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_algebra_ast_annotation_util
open Xquery_physical_algebra_ast

open Algebra_type
open Optimization_util (* Need for the is_* functions *)
open Code_selection_context

open Code_util_pattern_matcher
open Code_util_predicates
open Cs_util

(* This compiles predicates as used by join algorithms.
   The pattern it deals with in the general case is the 
   double nested existential i.e:
     Some x in e_1 satisfies
      (fn:boolean)? Some x in e_2 satisfies 
      (fn:boolean)? op:(l,r)

   This is documented in other places as well but here is 
   the short version.

*)

(********************************************************)
(* Types required contains a list of atomic_types       *)
(* to which an input operand could possibly be promoted *)
(********************************************************)

type join_pred_desc =
    { types_required : Datatypes.atomic_type list;
      join_predicate : supported_predicates }

let mk_join_pred_desc tr jp =
  { types_required = tr;
    join_predicate = jp }

let switch_predicate pred_desc = 
  let switched_pred = switch_predicate pred_desc.join_predicate in
  mk_join_pred_desc pred_desc.types_required switched_pred

(********************)
(* join information *)
(********************)
type join_information  =
    { outer_some         : (algop_expr * crname) option;
      outer_equal_op     : op_expr_type;
      inner_some         : (algop_expr * crname) option;
      inner_equal_op     : op_expr_type;

      (* Join type is described by the above type *)
      (* It is intent to capture the op and the type semantic *)
      join_type          : join_pred_desc; }
      (* Do we need to do promotion on the halves of the equality *)

let mk_join_information os os_eq_op is is_eq_op jt =
  { outer_some     = os;
    outer_equal_op = os_eq_op;
    inner_some     = is;
    inner_equal_op = is_eq_op;
    join_type      = jt }

let invalid_pred_desc  = mk_join_pred_desc [] predicate_invalid 
let empty_join_info    = mk_join_information None Invalid_Predicate_Branch None Invalid_Predicate_Branch invalid_pred_desc
let cannot_join_return = false, empty_join_info

let get_join_predicate ji = ji.join_type.join_predicate
let get_types_required ji = ji.join_type.types_required


(**********************************************************)
(* Extraction functions:
   They take in an operand and the join information (st)
   and return the updated state *)
(**********************************************************)

(* Outer Some extract *)
let extract_outer_in_term op st =
  match op.palgop_expr_name with 
    | AOESome (_,vn) ->
	mk_join_information 
	  (Some (access_onesub op.psub_expression, vn)) st.outer_equal_op
	  st.inner_some st.inner_equal_op 
	  st.join_type

    | _ ->
	raise (Query (Code_Selection ("(outer hash) Attempting to extract the 'in' portion of something that is not an AOESome")))
  
(* Inner some extract *)
let extract_inner_in_term op st =
  match op.palgop_expr_name with
    | AOESome (_,vn) ->	
	mk_join_information 
	  st.outer_some st.outer_equal_op
	  (Some ((access_onesub op.psub_expression), vn)) st.inner_equal_op
	  st.join_type
    | _ ->
	raise (Query (Code_Selection ("(inner hash) Attempting to extract the 'in' portion of something that is not an AOESome")))

(**************************************)
(* Determine the join type from above *)
(**************************************)

let extract_join_type op =
  let predicate_type = predicate_type op in
  let needed_types = needed_types op predicate_type in
  mk_join_pred_desc needed_types predicate_type

(*****************************)
(* Extract a binary function *)
(*****************************)

let extract_binary_op op st =
  let inputs = access_manysub op.psub_expression in
  if (Array.length inputs) = 2
  then
    begin
      let jt     = extract_join_type op in
      let outer  = RegularOp (inputs.(0)) in
      let inner  = RegularOp (inputs.(1)) in
      mk_join_information
	st.outer_some outer
	st.inner_some inner
	jt
    end
  else
    raise (Query (Code_Selection ("Non Binary Op in extract_binary_op")))


(********************************************)
(* Double nested pattern in join predicates *)
(********************************************)

(* Those are the pattern matching rules for predicates...

Here is the normalized form for predicates:

      [[ $x = $y ]]
       ==
      some $fs:v7 in fn:data($x)
      satisfies
        fn:boolean(
          some $fs:v8 in fn:data($y)
          satisfies
            fn:boolean(
              let $fs:v5 := fs:untyped-to-any($fs:v7, $fs:v8),
                  $fs:v6 := fs:untyped-to-any($fs:v8, $fs:v7)
              return op:equal($fs:v5, $fs:v6)
            )
        )

[[ where ($x = $y) ]
 ==
where fn:boolean( [[ $x = $y ]] )

Inside a where clause, there should be an extra fn:boolean call around
such a predicate.

rule2 deals with the most general case...

*)

let is_quant x = (is_some x)

let double_nested_patterns index allowed_op_test =
  let dep_traverse = mk_dep_child_param index in
  let rule1 =
    [ (dep_traverse, (extract_term allowed_op_test extract_binary_op)) ]
  in
  let rule2 =
    [ (dep_traverse, (no_extraction is_boolean) ); 
      (indep_zero, (extract_term allowed_op_test extract_binary_op)) ]
  in
  let rule3 =
    [ (dep_traverse, (extract_term is_quant extract_outer_in_term)); 
      (dep_zero, (extract_term is_quant extract_inner_in_term)); 
      (dep_zero, (extract_term allowed_op_test extract_binary_op)) ]
  in
  let rule3a =
    [ (dep_traverse, (extract_term is_quant extract_outer_in_term)); 
      (dep_zero, (extract_term allowed_op_test extract_binary_op)) ]
  in
  let rule4 =
    [ (dep_traverse, (extract_term is_quant extract_outer_in_term)); 
      (dep_zero, (no_extraction is_boolean) ); 
      (indep_zero, (extract_term is_quant extract_inner_in_term)); 
      (dep_zero, (no_extraction is_boolean));
      (indep_zero, (extract_term allowed_op_test extract_binary_op)) ]
  in
  let rule5 =
    [ (dep_traverse, (no_extraction is_boolean) ); 
      (indep_zero, (extract_term is_quant extract_outer_in_term)); 
      (dep_zero, (no_extraction is_boolean) ); 
      (indep_zero, (extract_term is_quant extract_inner_in_term)); 
      (dep_zero, (no_extraction is_boolean));
      (indep_zero, (extract_term allowed_op_test extract_binary_op)) ]
  in
  let rule6 = (* here we assume a binary comes first *)
    [ (dep_traverse, (no_extraction is_boolean));
      (indep_zero,  (extract_term is_quant extract_outer_in_term))]
      (* same as rule 1 after here *)
    @ (List.tl rule1)
  in
  rule1 :: rule2 :: rule3 :: rule3a :: rule4 :: rule5 :: rule6 :: []

(***********************************)
(* Op verification and normal form *)
(***********************************)
(* Given a join operand with branches
   Join (L,R) and a given join_information (filled out above)
   make sure it is the following properties (or fail if is not possible)


   Ensure the following invariants:
    1. accessed (ji.outer_in_op)    subset returned (L)
    2. accessed (ji.outer_equal_op) subset returned (L) and 
         inner_variable (not in) free(outer_equal_op) 
   
       or outer_equal_op = None (* fs:untyped-to-any hack *)

   - symmetric conditions for hi.inner_* -      
   
   *** make sure predicates are "facing right way" 
   for example greater than. ***
   $x < $y -> and left and right switch.. then you need switch the operator


   NOTE: Even though we are taking in a join_information, it is
   of a restricted form. It does not have FS_untyped_to_any (all ops
   on both sides of the op are regular ops).

   This function enforces that the join_information structure is
   valid.

   It may be worth having a valid bit set at the end (or a polymorphic type)
   to force correct compilation order.
*)

let debug_build_join1 ok =
  if Debug.join_debug()
  then
    if ok
    then Debug.print_join_debug " ---> [Condition1 Succeeded]."
    else Debug.print_join_debug " ---> [Condition1 Failed]."

let debug_switching ok =
  if Debug.join_debug()
  then
    if ok
    then Debug.print_join_debug " ---> [Switching predicate order!]."
    else Debug.print_join_debug " ---> [NOT switching predicate order!]."

let debug_build_join2 ok =
  if Debug.join_debug()
  then
    if ok
    then Debug.print_join_debug " ---> [Condition2 Succeeded]."
    else Debug.print_join_debug " ---> [Condition2 Failed]."

let build_join_helper join_information join_op = 
  let l, r = access_twosub join_op.psub_expression in
  let all_returned_fields =
    (algop_get_returned_fields l) @ (algop_get_returned_fields r)
  in

  (***********************)
  (* Verify condition 1. *)
  (***********************)

  (* This is matching the two operands of the join predicate with each
     branch of the join, based on field accessed and returned. *)

  let tuple_dependent predop joinbranch =
    let accessed_fields = algop_get_accessed_fields predop in
    (* Note, we project out fields that may be created inside the
    predicate itself since they do not participate in how the
    predicate branches are matched with the join branches. - Jerome
    and Mary. May 2006 *)
    let relevant_accessed_fields =
      Gmisc.intersect_list all_returned_fields accessed_fields
    in
    let returned_fields = algop_get_returned_fields joinbranch in
    begin
      Gmisc.is_subset
	relevant_accessed_fields
	returned_fields
    end
  in
  (* Check if these are switched. This is irrelevant unless
     we use the untyped-to-any hack below *)
  let b_ok, (left, right) =
    match (join_information.outer_some, join_information.inner_some) with
    | (Some (out_op, out_v), Some (in_op, in_v)) ->
	(* NEED TO ASSERT THAT out_op and in_op are indep *)
	(* not List.mem out_v algop_get_free_variables    *)
	if tuple_dependent out_op l &&
 	  tuple_dependent in_op  r then
	  true, (Some (out_op,out_v), Some (in_op,in_v))
	else if tuple_dependent out_op r &&
	  tuple_dependent in_op  l then
	  true, (Some (in_op,in_v), Some (out_op,out_v))	      
	else false, (None, None)
	    (* Determine if this is from the left or the right *)
    | (Some (an_op,vn), None) | (None, Some (an_op, vn)) ->
	if tuple_dependent an_op l then
	  true, (Some (an_op, vn), None)
	else if tuple_dependent an_op r then
	  true, (None, Some (an_op,vn))
	else
	  false, (None, None)
    | (None, None) ->
	true, (None, None)
  in

  debug_build_join1 b_ok;
  (* Ensure that 
     accessed (op) subset returned (join_branch)
     indep_var not in free_variables(op)
     
     That is it does not depend on the "other" branch
     
  *)
  let dependence_check op join_branch (indep_var:(algop_expr * Xquery_common_ast.crname) option) =
    match indep_var with
    | None -> 	  
	Gmisc.is_subset 
	  (algop_get_accessed_fields op)
	  (algop_get_returned_fields join_branch)
    | Some (an_op, iv) ->
	(* Check that op only accesses fields form the correct join branch 
	   and the op does *NOT* access the indep_var that is passed in *) 
	(Gmisc.is_subset 
	   (algop_get_accessed_fields op)
	   (algop_get_returned_fields join_branch)) && 
	(not (List.mem iv (algop_get_free_variables op)))
  in
  (* Notice, we could have updated left and right, 
     so we need to perform this check in terms of the 
     change operand *)
    
  (* Here we check if it is untyped to any (and then hack the semantic)
     though this should not *really* be necessary *)
  (* Additionally, we check if the operand is switched (doc'd below) *)

  let b_ok2, b_predicate_switched, (out_op_eq, in_op_eq) = 
    let fail_to_match =	false, false, (Invalid_Predicate_Branch, Invalid_Predicate_Branch) in
    (* This is a hack... we need to know if the op should have been switched. Same
       example as below. However consider if there are no Some.. for example:
       
       Some $y' in $y satisfies
       op:equal(fs:untyped-to-any( Input#x, $y'),
       fs:untyped-to-any( $y', Input#x))
       
       Now consider to figure out which branch, we need to extract
       the first component of the untyped-to-any, call it expr.
       
       If it is on the "left" branch the following must hold: (it does not imply it
       comes from the left, - it could be a constant)
       We need stronger provinence.
       
       o accessed_fields (expr) intersect returned_fields (R) = empty_set
       o vn_right not in free_variables  (expr) (where vn_right is the *optional* name
       of the right hand side sequence variable)
       call this check determine_sides_check_eq_op
     *)
    let determine_sides_check_eq_op (* (l,left) (r,right) *) op =
      let can_be_left =
	(Gmisc.intersect_list
	   (algop_get_accessed_fields op)
	   (algop_get_returned_fields r) = []) &&
	match right with
	| None -> true
	| Some (v, vn) ->
	    not (List.mem vn (algop_get_free_variables op)) 
      in
      let can_be_right =
	(Gmisc.intersect_list
	   (algop_get_accessed_fields op)
	   (algop_get_returned_fields l) = []) &&
	match left with
	| None -> true
	| Some (v, vn) ->
	    not (List.mem vn (algop_get_free_variables op))
      in
      can_be_left, can_be_right
    in
    match (join_information.outer_equal_op, join_information.inner_equal_op) with
      (* NOTICE AT THIS POINT EVERYTHING IS A REGULAR OP! *)
    | (RegularOp op1, RegularOp op2) when
	(is_fs_untyped_to_any op1) &&
	(is_fs_untyped_to_any op2) ->
	  let op1_expr = (access_manysub op1.psub_expression).(0) in
	  let op2_expr = (access_manysub op2.psub_expression).(0) in
	  let op1_can_be_left, op1_can_be_right =
	    determine_sides_check_eq_op (* (l,left) (r,right) *) op1_expr in
	  let op2_can_be_left, op2_can_be_right =
	    determine_sides_check_eq_op (* (l,left) (r,right) *) op2_expr in
	  if op1_can_be_left && op2_can_be_right then
	    true, false, (Fs_untyped_to_any, Fs_untyped_to_any)
	  else if op1_can_be_right && op2_can_be_left then
	    true, true, (Fs_untyped_to_any, Fs_untyped_to_any)
	  else fail_to_match
	      (* Find out if the conditions are satisfied *)
    | (RegularOp op1, RegularOp op2) when
	(is_fs_untyped_to_any op1) ->
	  let op1_expr = (access_manysub op1.psub_expression).(0) in
	  let op1_can_be_left, op1_can_be_right =
	    determine_sides_check_eq_op (* (l,left) (r,right) *) op1_expr in
	  let op2_can_be_left, op2_can_be_right =
	    (dependence_check op2 l right),(dependence_check op2 r left)
	  in
	  if op1_can_be_left && op2_can_be_right then
	    true, false, (Fs_untyped_to_any, RegularOp op2) (* current order is ok *)
	  else if op1_can_be_right && op2_can_be_left then
	    true, true,(RegularOp op2, Fs_untyped_to_any) (* new order is not *)
	  else fail_to_match
    | (RegularOp op1, RegularOp op2) when
	(is_fs_untyped_to_any op2) ->
	  let op2_expr = (access_manysub op2.psub_expression).(0) in
	  let op2_can_be_left, op2_can_be_right =
	    determine_sides_check_eq_op (* (l,left) (r,right) *) op2_expr in
	  let op1_can_be_left, op1_can_be_right =
	    (dependence_check op1 l right),(dependence_check op1 r left)
	  in
	  if op1_can_be_left && op2_can_be_right then
	    true, false, (RegularOp op1, Fs_untyped_to_any) (* current order is ok *)
	  else if op1_can_be_right && op2_can_be_left then
	    true, true,(Fs_untyped_to_any, RegularOp op1) (* new order is not *)
	  else fail_to_match
    | (RegularOp out_op, RegularOp in_op) ->
	(* do we have the order correctly done? *)
	if (dependence_check out_op l right) &&
	  (dependence_check in_op r left) then
	  (* Order is correct *)
	  true, false, (RegularOp out_op, RegularOp in_op)
	else if (* Do we have it reversed *)
	  (dependence_check in_op l right) &&
	  (dependence_check out_op r left) then
	  (* this means the predicate should be switched to reflect *)
	  true, true, (RegularOp in_op, RegularOp out_op)
	else fail_to_match
	    (* This should never happen *)
    | _ ->
	fail_to_match
  in
  debug_build_join2 b_ok2;

  (* It is possible we swtiched the left and
     right hand sides of the op 
     Example:
     for $x in (1,2,3)
     for $y in (1,2,3)
     where $y < $x
     return ($x, $y)

     ==========================================================
     --- Notice that x is from the outer branch and so will ---
     --- be grouped there. This is effectively rewritten to ---

     for $x in (1,2,3)
     for $y in (1,2,3)
     where $x > $y
     return ($x, $y)
   *)
  let corrected_join_type = 
    debug_switching b_predicate_switched;
    if b_predicate_switched
    then
      switch_predicate join_information.join_type
    else
      join_information.join_type
  in
  (b_ok && b_ok2),
  mk_join_information
    left out_op_eq
    right in_op_eq
    corrected_join_type

let debug_case1 () =
  if Debug.join_debug()
  then
    Debug.print_join_debug "None, Fs_untyped_to_any"

let debug_case2 () =
  if Debug.join_debug()
  then
    Debug.print_join_debug "Some outside, Fs_untyped_to_any"

let debug_case3 () =
  if Debug.join_debug()
  then
    Debug.print_join_debug "None, (RegularOp inside)"

let debug_case4 () =
  if Debug.join_debug()
  then
    Debug.print_join_debug "(Some outside), (RegularOp inside)"

let debug_case41 () =
  if Debug.join_debug()
  then
    Debug.print_join_debug "Code selection for SOME part."

let debug_case42 vn =
  if Debug.join_debug()
  then
    let vns = Namespace_names.prefixed_string_of_rqname vn in
    Debug.print_join_debug ("Adding variable "^vns^" to code context.")

let debug_case43 () =
  if Debug.join_debug()
  then
    Debug.print_join_debug "Code selection for (IN)EQUAL part."

let debug_case5 () =
  if Debug.join_debug()
  then
    Debug.print_join_debug "_, Invalid_Predicate_Branch"


(* Private function to compile predicate branches given certain inputs
   this will be called by exposed functions to compile given branches *)
let compile_predicate_branch code_select_fn code_ctxt some_in_op_opt equal_op =
  let copy_and_code_select code_ctxt op = 
    let copy = copy_algop op in
    let code_ctxt = code_select_fn code_ctxt copy in
    copy, code_ctxt
  in
  match (some_in_op_opt, equal_op) with	    
    (* Inside is an fs:untyped-to-any which must be handled specially *)
    (* Here we do not need an assign function *)
  | None, Fs_untyped_to_any -> (* does this make sense? *)
      debug_case1();
      let pb = mk_predicate_branch None Fs_untyped_to_any None in
      pb, code_ctxt
  | (Some outside, Fs_untyped_to_any) ->
      debug_case2();
      let outside_op, name = outside in
      let outside_op,code_ctxt = copy_and_code_select code_ctxt outside_op in
      let pb = mk_predicate_branch (Some outside_op) Fs_untyped_to_any None in
      pb, code_ctxt
	(* One nested some has been removed, here we can just evaluate the expression, 
	   again no assignment function is needed *)
  | None, (RegularOp inside) ->
      debug_case3();
      let inside,code_ctxt = copy_and_code_select code_ctxt inside in
      let pb = mk_predicate_branch None (RegularOp inside) None in
      pb, code_ctxt
	(* Here we have the full pattern - and we *need* an assignment operation *)
	(* Also: we need to cmopile the inside portion in this code_selection context
	   this is because it may reference the variable we are assigning and they
	   should point to the same place *)
  | (Some outside), (RegularOp inside) ->
      debug_case4();
      let outside_op, vn = outside in
      debug_case41();
      let outside_op,code_ctxt = copy_and_code_select code_ctxt outside_op in
      let code_ctxt            = add_variable_to_current_context code_ctxt vn in
      debug_case42 vn;
      let assign_fn            = build_add_var_xml_value_safe code_ctxt vn in
      debug_case43();
      let inside,code_ctxt     = copy_and_code_select code_ctxt inside in
      let pb =
	mk_predicate_branch (Some outside_op) (RegularOp inside) (Some assign_fn)
      in
      pb, code_ctxt 

  | _, Invalid_Predicate_Branch -> (* Should we throw the error here? *)
      debug_case5();
      let pb = mk_predicate_branch None Invalid_Predicate_Branch None in
      pb, code_ctxt

let string_of_pred p =
  match p with
  | RegularOp op ->
      Print_xquery_algebra.bprintf_logical_algstatement "" op
  | Fs_untyped_to_any ->
      "Fs_untyped_to_any"
  | Invalid_Predicate_Branch ->
      "Invalid_Predicate_Branch"

let debug_outer join_information =
  let outer_some = join_information.outer_some in
  let outer_equal_op = join_information.outer_equal_op in
  let os =
    match outer_some with
    | None -> ""
    | Some (outer_some,_) ->
	Print_xquery_algebra.bprintf_logical_algstatement "" outer_some
  in
  let oe = string_of_pred outer_equal_op in
  if Debug.join_debug()
  then
    begin
      Debug.print_join_debug "Making outer predicate branch.";
      Debug.print_join_debug ("Outer some:\n" ^ os);
      Debug.print_join_debug ("Outer equal:\n" ^ oe);
    end

let debug_inner () =
  if Debug.join_debug()
  then
    Debug.print_join_debug "Making inner predicate branch."

let debug_done () =
  if Debug.join_debug()
  then
    Debug.print_join_debug "DONE."

let construct_outer_predicate_branch code_select_fn code_ctxt join_information =
  begin
    debug_outer join_information;
    let result =
      compile_predicate_branch code_select_fn code_ctxt join_information.outer_some join_information.outer_equal_op
    in
    debug_done();
    result
  end

let construct_inner_predicate_branch code_select_fn code_ctxt join_information =
  begin
    debug_inner ();
    let result =
      compile_predicate_branch code_select_fn code_ctxt join_information.inner_some join_information.inner_equal_op
    in
    debug_done();
    result
  end


(**********************************)
(* Types and helper functions for *)
(*   sort joins                   *)
(**********************************)

let is_sortjoinable_op op =
  (is_gt op) || (is_ge op) ||
  (is_lt op) || (is_le op) || 
  (is_equal op)

let is_hashjoinable_op op =
  (is_equal op)

(***********************)
(* Pattern description *)
(***********************)
(* Some $x in (e_1)
   Some $y in (e_2)
   {op:..}
 *)

let sort_join_double_nested_patterns index = 
  double_nested_patterns index is_sortjoinable_op


(************************)
(* Sort join conditions *)
(************************)	    

(* We know the return must be in order and Some 1 in, Some 2 in and
   (e_l,e_r) *)

let debug_print_sort_matched () =
  if Debug.join_debug()
  then
    begin
      Debug.print_join_debug "Predicate was matched for SORT."
    end

let debug_print_sort_notmatched () =
  if Debug.join_debug()
  then
    begin
      Debug.print_join_debug "Predicate was NOT matched for SORT."
    end

let debug_print_hash_matched () =
  if Debug.join_debug()
  then
    begin
      Debug.print_join_debug "Predicate was matched for HASH."
    end

let debug_print_hash_notmatched () =
  if Debug.join_debug()
  then
    begin
      Debug.print_join_debug "Predicate was NOT matched for HASH."
    end

(*
let patch_join_op join_op =
  *)

(* Try to build the parameters for a sort
   join assuming it is the double nested variety *)
let build_sort_join_double_nested join_op index = 
  (* Figure out which is a subset of which expression *)
  let matched, return = walk_patterns empty_join_info
      (sort_join_double_nested_patterns index) join_op in
  if matched then
    begin
      debug_print_sort_matched ();
      build_join_helper return join_op
    end
  else
    begin
      debug_print_sort_notmatched ();
      cannot_join_return
    end


(* Conditions for a sort to work:

   1. Must be in one of the listed 3 patterns. 
   (If a portion of the pattern is not present it is None)
   Only E_l and E_r are mandatory

   
   a.
   Some $x in E_1 satisfies 
      boolean (
       Some $y in E_2 satisfies 
       boolean ( op:equal(E_l,E_r) ))

   Let L, R be the branches of the join.
   It must be the case that
    accessed(E_1) subset returned (L)
    accessed(E_2) subset returned (R)
   - or - 
    accessed(E_1) subset returned (R)
    accessed(E_2) subset returned (L)
  - this also implies the branches are disjoint.
    and independent with respect to the input.
   
   b. *** We do not check this because of
          XPath type semantics violates it
          now. *** 

   (eg: fs:untyped-to-any($x,$y)) - the below can not be satisfied
 
   E_l and E_r must be disjoin and independent as above
    accessed(E_l) subset returned(L)
    accessed(E_r) subset returned(R) or vice versa

   c. We require (assume now) that the op:equal is of the form
      op:equal($z,$z') that is two variables bound outside.
   
      *** C. could be important for our rewrites ***

   if accessed (E_1) subset returned (E_2) then
    I'm going to say E_1 is included in E_2.
******

   Other assumptions: only deal with doubly
   nested now, need to put in the others

---
   The implementation of these conditions
   are all over the place and not centrally doc'd

   WRITE IT UP

---- 
   Note: Sort joins can have different numbers
   of dependent expressions. 
*)

(* Can build a sort from this particularly join operation
   right now it just tries to build it and returns the success 
   code. This could definetly be improved.

   Also, the check is not complete since we are hacking around
   static typing. 
 *)

(**********************************)
(* Types and helper functions for *)
(*   hash joins                   *)
(**********************************)

(* Try to build the parameters for a hash join assuming it is the
   double nested variety *)

let build_hash_join_double_nested join_op index = 
  (* Figure out which is a subset of which expression *)
  let matched, return =
    walk_patterns
      empty_join_info
      (double_nested_patterns index is_hashjoinable_op)
      join_op
  in
  if matched
  then
    begin
      debug_print_hash_matched ();
      build_join_helper return join_op
    end
  else
    begin
      debug_print_hash_notmatched ();
      cannot_join_return
    end

(* Conditions for a hash to work:

   1. Must be in one of the listed 3 patterns. 
   (If a portion of the pattern is not present it is None)
   Only E_l and E_r are mandatory

   
   a.
   Some $x in E_1 satisfies 
   boolean (
   Some $y in E_2 satisfies 
   boolean ( op:equal(E_l,E_r) ))

   Let L, R be the branches of the join.
   It must be the case that
   accessed(E_1) subset returned (L)
   accessed(E_2) subset returned (R)
   - or - 
   accessed(E_1) subset returned (R)
   accessed(E_2) subset returned (L)
   - this also implies the branches are disjoint.
   and independent with respect to the input.
   
   b. *** We do not check this because of
   XPath type semantics violates it
   now. *** 

   (eg: fs:untyped-to-any($x,$y)) - the below can not be satisfied
   
   E_l and E_r must be disjoin and independent as above
   accessed(E_l) subset returned(L)
   accessed(E_r) subset returned(R) or vice versa

   c. We require (assume now) that the op:equal is of the form
   op:equal($z,$z') that is two variables bound outside.
   
   *** C. could be important for our rewrites ***

   if accessed (E_1) subset returned (E_2) then
   I'm going to say E_1 is included in E_2.
   ******

   Other assumptions: only deal with doubly
   nested now, need to put in the others

   ---
   The implementation of these conditions
   are all over the place and not centrally doc'd

   WRITE IT UP

   ---- 
   Note: Hash joins can have different numbers
   of dependent expressions. 
*)

(* Can build a hash from this particularly join operation
   right now it just tries to build it and returns the success 
   code. This could definitely be improved.

   Also, the check is not complete since we are hacking around
   static typing. 
*)


(******************)
(* Join detection *)
(******************)

type join_kind =
  | NestedLoopJoin
  | HashJoin
  | SortJoin

let debug_print_trying_hash_join () =
  if Debug.join_debug()
  then
    begin
      Debug.print_join_debug "================================";
      Debug.print_join_debug "Trying to pick-up a hash join..."
    end

let debug_print_trying_sort_join () =
  if Debug.join_debug()
  then
    begin
      Debug.print_join_debug "================================";
      Debug.print_join_debug "Trying to pick-up a sort join..."
    end

let debug_print_trying_join () =
  if Debug.join_debug()
  then
    begin
      Debug.print_join_debug "================================";
      Debug.print_join_debug "Choosing which join to run";
      Debug.print_join_debug "================================"
    end

let debug_print_result b =
  if Debug.join_debug()
  then
    begin
      if b
      then
	Debug.print_join_debug "YAY! :-)"
      else
	Debug.print_join_debug "NAY! :-(";
      Debug.print_join_debug "================================"
    end

let can_build_hash op = 
  if !Conf.nested_loop_join then
    false
  else
    begin
      debug_print_trying_hash_join ();
      let n_ops      = Array.length (access_manysub op.pdep_sub_expression) in
      let matched    = ref true in
      for i = 0 to (n_ops-1) do
	matched := !matched && fst (build_hash_join_double_nested op i)
      done;
      let r = !matched in
      debug_print_result r;
      r
    end

let can_build_sort op =
  if !Conf.nested_loop_join
  then false
  else
    begin
      debug_print_trying_sort_join ();
      let n_ops      = Array.length (access_manysub op.pdep_sub_expression) in
      let matched    = ref true in
      for i = 0 to (n_ops-1) do
	matched := !matched && fst (build_sort_join_double_nested op i)
      done;
      let r = !matched in
      debug_print_result r;
      r
    end

type outer_kind =
  | StandardJoin
  | OuterJoin of crname

let get_join_kind algop =
  debug_print_trying_join ();
  if can_build_hash algop
  then HashJoin
  else if can_build_sort algop
  then SortJoin
  else NestedLoopJoin

let select_physical_op outer_kind algop_expr =
  match get_join_kind algop_expr with
  | HashJoin -> 
      begin
	match outer_kind with 
	| StandardJoin -> POJoin_Hash
	| OuterJoin v  ->  POLeftOuterJoin_Hash v
      end
  | SortJoin -> 
      begin
	match outer_kind with 
	| StandardJoin -> POJoin_Sort 
	| OuterJoin v  -> POLeftOuterJoin_Sort v
      end
  | NestedLoopJoin ->  
      begin
	match outer_kind with 
	| StandardJoin -> POJoin_NestedLoop 
	| OuterJoin v  -> POLeftOuterJoin_NestedLoop v
      end
(****************************)
(* "Pre-compile" predicates *)
(****************************)

type predicate_functions =
    (predicate_branch * predicate_branch * supported_scans) array
    
(******************************************************************)
(*********************************)
(**** THIS IS EXTREMELY DIRTY ****)
(*********************************)
(* The above join information splits the incoming predicate into
   possibly four pieces.  These piecse are described in detail in the
   join_helper. This code also does code selection on *copies* of the
   pieces. This is very hackish - it is done inside the more
   complicated operands now.

   A solution is to have a physical ast (which we are currently
   planning on doing in the future). This physical ast will represent
   the algebra. In this ast, SortJoin will be a physical operation
   that can be picked up. It will have a particular semantic of its
   operations that will be directly identified there.  This solution
   is cleaner but the details have not been worked out.

   The bottom line is: If you start playing with this code, it could
   get nasty - and it should eventually fade away. *)
(******************************************************************)

let build_common_cond code_select_fn code_ctxt join_op build_double build_op_type =
  let array_len = Array.length (access_manysub join_op.pdep_sub_expression) in 
  let build_predicate_branches code_ctxt join_op index =
    let _,join_information = build_double join_op index in
    let outer_predicate_branch, code_ctxt =
      construct_outer_predicate_branch code_select_fn code_ctxt join_information
    in
    let inner_predicate_branch, code_ctxt =
      construct_inner_predicate_branch code_select_fn code_ctxt join_information
    in
    let op_type = build_op_type join_information in
    (outer_predicate_branch, inner_predicate_branch, op_type), code_ctxt
  in

  (* Build the conditions so that the information for each of the
     hash evaluation predicates is there *)
  let built_cond =
    let init_built_cond =
      Array.make
	array_len
	(invalid_predicate_branch, invalid_predicate_branch, Invalid_Scan)
    in
    begin
      for i = 0 to array_len - 1 do
	let bc = fst (build_predicate_branches code_ctxt join_op i) in
	init_built_cond.(i) <- bc
      done;
      init_built_cond
    end
  in
  built_cond

let build_sort_cond code_select_fn code_ctxt join_op =
  let build_double = build_sort_join_double_nested in
  let build_op_type join_information =
    supported_predicate_supported_scans (get_join_predicate join_information)
  in
  build_common_cond code_select_fn code_ctxt join_op build_double build_op_type

let build_hash_cond code_select_fn code_ctxt join_op =
  let build_double = build_hash_join_double_nested in
  let build_op_type join_information = Invalid_Scan in
  build_common_cond code_select_fn code_ctxt join_op build_double build_op_type

(**********************)
(* Outer-join support *)
(**********************)

type null_functions =
    (bool * (unit -> unit) * (unit -> unit) * Physical_value.dom_value array)

let get_null_functions code_ctxt nm_option needed_names =
  let empty_right = Array.make (Array.length needed_names) empty_dom_sequence in 
  match nm_option with
  | StandardJoin ->
      (false,
       (fun () ->
	 raise (Query (Code_Selection ("LeftOuterJoin (hash) without a null name")))),
       (fun () -> ()),
       empty_right)
  | OuterJoin v ->
      let fn = build_create_tuple_code code_ctxt v in
      (true,
       (fun () -> fn empty_sequence),
       (fun () -> fn non_empty_sequence),
       empty_right)


