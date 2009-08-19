(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_rules_treepattern.ml,v 1.8 2007/02/12 21:15:33 simeon Exp $ *)

(* Module: Optimization_rules_treepattern
   Description:
     This module implements the TreePattern optimization rules, as
     described in.

       Put a Tree Pattern in Your Algebra. Philippe Michiels, George
       Mihaila, and Jérôme Siméon

       ICDE 2007
       http://www.adrem.ua.ac.be/bibrem/pubs/michiels07treepattern.pdf
*)

open Error
open Namespace_builtin

open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_algebra_ast_annotation_util

open Processing_context
open Compile_context

open Optimization_util
open Optimization_walker
open Optimization_judge

(* ** Tree pattern related rewrites
 *  This module implements the rewrites prsented in
 *   Put a Tree Pattern in Your Tuple Algebra; 
 *   Philippe Michiels, George Mihaila, Jérôme Siméon
 *   ICDE 2007;
 *
 * Note: the insertion of project operators in some rewrites is necessary
 *       for the correctness within Galax.
 * **)

(*********************************)
(* ** Some Utility Functions  ** *)
(*********************************)
let tp_create_single_step_treepattern input output axis nt indep eh fi =
  let twig_node = {
    node_test  = Some nt;
    out        = Some output;
    restore_out = true;
    pred_twigs = [];
    child_twig = None;
    requires_sbdo = (true,true);
  } in
  let rt = {
    node_test = None;
    out = None;
    restore_out = false;
    pred_twigs = [];
    child_twig = Some (axis,1);
    requires_sbdo = (false,false);
  }
  in
  let pattern = [|rt; twig_node|] in
    logical_aalgop_mkop 
      (AOETupleTreePattern(input,pattern)) 
      (OneSub indep) 
      NoSub None eh fi

let rec get_nested_treepattern op = 
  match op.palgop_expr_name with
  | AOETupleTreePattern (i2, p2) -> None, Some op
  | AOEProject fields -> 
      begin
	let ttp_op = get_nested_treepattern (access_onesub op.psub_expression) in
	match ttp_op with
	| None, Some ttp_op -> Some fields, Some ttp_op
	| _ -> None, None
      end
  | _ -> None, None

let merge_project_fields f1 f2 =
  let contains field list =
    List.fold_left 
      (fun b x -> Namespace_names.rqname_equal x field || b) 
      false list
  in
  (List.filter (fun x -> not(contains x f2)) f1) @ f2

let rec get_predicate_indices pd =
  match pd with
  | SimpleConjunct (i1, i2) ->
      if i1 <> i2 then [i1; i2] else [i1]
  | ComplexConjunct (p1, p2) ->
      get_predicate_indices p1 @ get_predicate_indices p2
  | _ -> raise (Query (Internal_Error "Only conjunctive predicates allowed"))

let strip_fn_boolean op =
  match op.palgop_expr_name with
  | AOECallBuiltIn ((fname,arity), _, _, _) 
    when Namespace_names.rqname_equal fname fn_boolean 
    -> 
      let args = access_manysub op.psub_expression in
      if (Array.length args) = 1 then
	Some args.(0)
      else
	None
  | _ -> None

let non_twig_error () =
  raise (Query (Internal_Error ("Expected a twig for merging")))

let get_twig_at_pos predicates pos = 
  let op = predicates.(pos) in
  match strip_fn_boolean op  with
  | Some map ->
      begin
	match map.palgop_expr_name with
	| AOEMapToItem ->
	    begin
	      let pattern = 
		let p = access_onesub map.psub_expression in
		match p.palgop_expr_name with
		| AOEProject _ ->
		    access_onesub p.psub_expression
		| _ -> p
	      in
	      match pattern.palgop_expr_name  with
	      | AOETupleTreePattern (i, p) -> 
		  begin
		    let indep = access_onesub pattern.psub_expression in
		    match indep.palgop_expr_name with
		    | AOEInputTuple -> (i, p)
		    |_ -> non_twig_error ()
		  end
	      | _ -> non_twig_error ()
	    end
	| _ -> non_twig_error ()
      end 
  | None -> non_twig_error ()

(************************************)
(* ** Tree Pattern Rewrite Rules ** *)
(************************************)

(* ** TP 1a - Syntactic TupleTreePattern Rewrite (i)
   *
   * MapToItem { Input ->
   *  TreeJoin[axis::nt](Input#glx:dot)
   * }(Op)
   * ==
   * MapToItem {Input -> Input#new_dot}
   *  (TupleTreePattern[glx:dot][axis::nt{new_dot}](Op))
   *
   ** *)
let treejoin_rewrite comp_ctxt op =
  let fail = op, false in
  let eh = op.palgop_expr_origin in
  let fi = op.palgop_expr_loc in
  match op.palgop_expr_name with
  | AOEMapToItem ->
      begin
	let mapto_dep = access_onesub op.pdep_sub_expression in
	let mapto_indep = access_onesub op.psub_expression in
	match mapto_dep.palgop_expr_name with
	| AOETreeJoin (axis, nt) ->
	    begin
	      let tj_indep = access_onesub mapto_dep.psub_expression in
	      match tj_indep.palgop_expr_name with
	      | AOEAccessTuple input ->
		  begin
		    let output = get_new_dot_name comp_ctxt in
		    let treepattern = tp_create_single_step_treepattern 
			input output axis nt mapto_indep eh fi in
		    let field_access_op = logical_aalgop_mkop (AOEAccessTuple output) 
			NoSub NoSub None eh fi in
		    let new_op =  logical_aalgop_mkop AOEMapToItem
			(OneSub treepattern) (OneSub field_access_op) None eh fi
		    in
		    new_op, true
		  end
	      | _ -> fail
	    end
	| _ -> fail
      end
  | _ -> fail
	
(* ** TP 1b - Standalone Treejoin Rewrite
   * TreeJoin[child::b](Input#glx:dot)
   * ==
   * MapToItem{Input#out}
   *   (TupleTreePattern[glx:dot][axis::nt{out}](Input))
   ** *)

let standalone_treejoin_rewrite comp_ctxt op =
  let fail = op, false in
  let eh = op.palgop_expr_origin in
  let fi = op.palgop_expr_loc in
  match op.palgop_expr_name with
  | AOETreeJoin(axis, nt) ->
      begin
	let ta = access_onesub op.psub_expression in
	match ta.palgop_expr_name with
	  (* matching VAR here, assuming input queries are type correct and *)
          (*  treejoins are intially applied on sigleton sequences          *)
	(*| AOEVar input -- fixme: what if this happens?*)
	| AOEAccessTuple input ->
	    begin
	      let input_op = logical_aalgop_mkop AOEInputTuple  NoSub NoSub None eh fi in
	      let output = get_new_dot_name comp_ctxt in
	      let treepattern = tp_create_single_step_treepattern 
		  input output axis nt input_op eh fi in
	      let access = logical_aalgop_mkop (AOEAccessTuple output) 
		  NoSub NoSub None eh fi in
	      let map = logical_aalgop_mkop AOEMapToItem (OneSub treepattern) 
		  (OneSub access) None eh fi in
	      map, true
	    end
	| _ -> fail
      end
  | _ -> fail


(* ** TP 2a - Item to Tuple Rewrite 
   *
   * MapFromItem{$comp -> [glx:dot_1 : $comp]}
   *   (MapToItem{Input#glx:dot_2}
   *     (TupleTreePattern[glx:dot_3][axis::nt{glx:dot_2}])(Op))
   * ==
   * Project[glx:dot_1](
   * TupleTreePattern[glx:dot_3][axis::nt{glx:dot_1}](Op))
   ** *)
let item_to_tuple_rewrite ctxt root op =
  let fail = op, false in
  let eh = op.palgop_expr_origin in
  let fi = op.palgop_expr_loc in    
  match op.palgop_expr_name with
  | AOEMapFromItem _ ->
      begin
	let map_from_indep = access_onesub op.psub_expression in
	let map_from_dep   = access_onesub op.pdep_sub_expression in
	match map_from_dep.palgop_expr_name, map_from_indep.palgop_expr_name with
	| AOECreateTuple a, AOEMapToItem  ->
	    begin
	      let (asequencetype,dot1) = a.(0) in
	      let map_to_indep = access_onesub map_from_indep.psub_expression in
	      let map_to_dep   = access_onesub map_from_indep.pdep_sub_expression in
	      match map_to_dep.palgop_expr_name, map_to_indep.palgop_expr_name  with
	      | AOEAccessTuple dot2, AOETupleTreePattern(i, p) ->
		  (* FIXME: check if output field of tree pattern is dot2 *)
		  let _ = replace_tuple_name ctxt dot2 dot1 root in
		  let project = logical_aalgop_mkop (AOEProject [|dot1|]) 
		      (OneSub map_to_indep) NoSub None eh fi 
		  in
		  project, true
	      | _ -> fail
	    end
	| _ -> fail
      end
  | _ -> fail


(* ** TP 3a - Twig Inlining Rewrite 
   *
   * (TupleTreePattern[dot_1][Input/nt1{dot_2}]
   *   (Project[f](
   *    (TupleTreePattern[dot_3][Input/nt2{dot_1}]
   *       (Indep))))
   * ==
   * Project[f,dot_2](
   *   (TupleTreePattern[dot_3][Input/n2{dot_1}/n1{dot_2}](Indep)))
   ** *)
let twig_inlining_rewrite ctxt tp1 =
  let eh = tp1.palgop_expr_origin in
  let fi = tp1.palgop_expr_loc in    
  let fail = tp1, false in
  match tp1.palgop_expr_name with
  | AOETupleTreePattern (i1, p1) ->
      begin
	let nested = access_onesub tp1.psub_expression in
	match get_nested_treepattern nested with
	| x, Some tp2 ->
	    begin
	      match tp2.palgop_expr_name with
	      | AOETupleTreePattern (i2, p2) ->
		  begin
		    let indep = access_onesub tp2.psub_expression in
		    let ap = find_attach_point p2 i1 in 
		    if ap > 0 then
		      begin
			match p2.(ap).child_twig with
			| None ->
			    begin
			      let p = append_twigs p2 p1 i1 in
			      let new_ttp =
				logical_aalgop_mkop 
				  (AOETupleTreePattern(i2,p)) 
				  (OneSub indep) NoSub None eh fi
			      in
			      match x with 
			      | Some fields ->
				  let outputs  = get_all_outputs_from_twig_pattern p in
				  let fields' = merge_project_fields 
				      (Array.to_list fields) outputs 
				  in
				  logical_aalgop_mkop 
				    (AOEProject(Array.of_list fields')) 
				    (OneSub new_ttp) NoSub None eh fi, true
			      | None -> new_ttp, true
			    end
			| Some _ ->
			    (* not inlining, a child twig already *)
                            (* exists at attach point in p2       *)
			    tp1, false
		      end
		    else tp1, false
		  end
	      | _ -> tp1, false
		    
	    end
	| _ -> fail
      end
  | _ -> fail


(* ** TP 3b - Branching Twig Rewrite 
   *
   *  (Select {
   *    fn:boolean(
   *      (MapToItem { Input -> (Input#dot_1)}
   *        (TupleTreePattern[dot_3][p2{dot_2}](Input)))
   *    and ... }
   *     (Project[f](
   *     (TupleTreePattern[dot_2][p3{dot_3}]
   *       (Indep))))
   *  ==
   *  Project[f](
   *  TupleTreePattern[dot_2][p3{dot_3}[./p2{dot_2}] ...](Indep))
   ** *)
let branching_twig_rewrite ctxt op =
  let eh = op.palgop_expr_origin in
  let fi = op.palgop_expr_loc in
  match op.palgop_expr_name with
  | AOESelect predicate_desc ->
      begin
	try
	  let preds = access_manysub op.pdep_sub_expression in
	  let pred_indices = get_predicate_indices predicate_desc in
	  let pred_list = List.map (get_twig_at_pos preds) pred_indices in
	  let prj = access_onesub op.psub_expression in
	  match prj.palgop_expr_name with
	  | AOEProject f ->
	      begin
		let tp = access_onesub prj.psub_expression in 
		match tp.palgop_expr_name with
		| AOETupleTreePattern (i, p) ->
		    let indep = access_onesub tp.psub_expression in
		    let p' = merge_twigs p pred_list in
		    let new_tp = logical_aalgop_mkop 
			(AOETupleTreePattern(i,p')) (OneSub indep) NoSub None eh fi in
		    logical_aalgop_mkop 
		      (AOEProject(f)) (OneSub new_tp) NoSub None eh fi, true
		| _ -> 
		    op, false
	      end
(*	  | AOETupleTreePattern (i, p) ->
	      begin
		let indep = access_onesub prj.psub_expression in
		let p' = merge_twigs p tp_list in
		let new_tp = logical_aalgop_mkop (AOETupleTreePattern(i,p')) 
		    (OneSub indep) NoSub None eh fi in
		new_tp, true
	      end
*)
	  | _ -> op, false
	with _ -> 
	  (* Exceptions are raised when the predicate is *)
          (* not a conjunction of TreePattern ops        *)
	  (*
	  let _ = Printf.printf 
	    "Select does not contain  a conjunction of TreePattern ops\n%!" 
	  in
	  *)
	  op, false
      end
  | _ -> op, false


(**************************************)
(* ** Extra rules (Galax specific) ** *)
(**************************************)


(* ** TP extra 1 - Merge projects rewrite 
   * Project[f_i1, ..., f_in](
   *   Project[f_j1, ..., f_jn](Op))
   * ==
   * Project[f_i1, ..., f_in, f_j1, ..., f_jn](Op)
   *
   * (remove duplicate fields)
   ** *)
let merge_projects_rewrite ctxt op =
  let eh = op.palgop_expr_origin in
  let fi = op.palgop_expr_loc in    
  let fail = op, false in
  match op.palgop_expr_name with
  | AOEProject fl1 ->
      begin
	let indep = access_onesub op.psub_expression in
	match indep.palgop_expr_name with
	| AOEProject fl2 ->
	    begin
	      let indep' = access_onesub indep.psub_expression in
	      let fl = merge_project_fields (Array.to_list fl1) (Array.to_list fl2) in
	      logical_aalgop_mkop 
		(AOEProject(Array.of_list fl)) (OneSub indep') NoSub None eh fi, true
	    end
	| _ -> fail
      end
  | _ -> fail


(* ** TP extra 2 - Push MapConcat through Project
   * 
   * MapConcat{Project[f](op1)}(op2)
   * ==
   * Project[f + fields returned by op2](MapConcat{op1}(op2))
   * 
   * MapConcat{op2}(Project[f](op1))
   * ==
   * Project[f + fields returned by op2](MapConcat{op2}(op1))
   *)
let push_mapc_through_project ctxt op =
  let eh = op.palgop_expr_origin in
  let fi = op.palgop_expr_loc in    
  let fail = op, false in
  match op.palgop_expr_name with
  | AOEMapConcat ->
      begin
	let dep = access_onesub op.pdep_sub_expression in
	let op2 = access_onesub op.psub_expression in
	match dep.palgop_expr_name with
	| AOEProject fields ->
	    begin
	      let op1 = access_onesub dep.psub_expression in
	      let op2_fields = algop_get_returned_fields op2 in
	      let all_fields = merge_project_fields 
		  (Array.to_list fields) op2_fields in
	      let new_mc = 
		logical_aalgop_mkop
		  AOEMapConcat (OneSub op2) (OneSub op1) None eh fi
	      in
	      logical_aalgop_mkop 
		(AOEProject (Array.of_list all_fields)) (OneSub new_mc) NoSub None eh fi, true
	    end
(* not necessary, since TPNF *)
(*	| _ -> 
	    begin
	      match op2.palgop_expr_name with
	      | AOEProject fields ->
		  begin
		    let op1 = access_onesub op2.psub_expression in
		    let op2_fields = algop_get_returned_fields dep in
		    let all_fields = merge_project_fields 
			(Array.to_list fields) op2_fields in
		    let new_mc = 
		      logical_aalgop_mkop
			AOEMapConcat (OneSub op1) (OneSub dep) None eh fi
		    in
		    logical_aalgop_mkop 
		      (AOEProject (Array.of_list all_fields)) (OneSub new_mc) NoSub None eh fi, true
		  end
	      | _ -> fail
	    end
*)
	| _ -> fail
      end
  | _ -> fail


(* ** TP extra 3 - Remove MapConcat over TupleTreePattern
  *
  * MapConcat{TupleTreePattern[i,p](Input)}(indep)}
  * ==
  * {TupleTreePattern[i,p](indep)
  *
  * OR
  *
  * MapConcat
  *  {TupleTreePattern[i,p](INPUT)}
  *  (TupleTreePattern[i',p'](sub))
  * == TupleTreePattern[i,p](TupleTreePattern[i',p'](sub))
  *
  * OR
  *
  * MapConcat{[glx:x : INPUT#glx:dot_new_0]}
  *  (TupleTreePattern[(glx:dot_1),(./child::a{glx:dot_new_0*})](sub))
  * ==
  * (TupleTreePattern[(glx:dot_1),(./child::a{glx:dot_new_0*})](sub))
  * + rename x into dot_new_0
  *)
let mapconcat_rewrite ctxt root op =
  let eh = op.palgop_expr_origin in
  let fi = op.palgop_expr_loc in    
  match op.palgop_expr_name with
  | AOEMapConcat ->
      begin
	let dep = access_onesub op.pdep_sub_expression in
	let indep = access_onesub op.psub_expression in
	match dep.palgop_expr_name with
	| AOETupleTreePattern (input, pattern) ->
	    let ttp_indep = access_onesub dep.psub_expression in
	    if is_input_tuple indep then 
	      logical_aalgop_mkop (AOETupleTreePattern(input,pattern)) 
		(OneSub ttp_indep) NoSub None eh fi, true
	    else if is_input_tuple ttp_indep then
	      match indep.palgop_expr_name with
	      | AOETupleTreePattern (input', pattern') ->
		  let _ = dep.psub_expression <- OneSub indep in
		  dep, true
	      | _ -> op, false
	    else 
	      (* Not applying rewrite (indep != INPUT) *)
	      op, false 
	| AOECreateTuple flds when Array.length flds = 1 ->
	    begin
	      let _,f = flds.(0) in
	      let ta = (access_manysub dep.psub_expression).(0) in
	      match ta.palgop_expr_name with
	      | AOEAccessTuple a ->
		  begin
		    match indep.palgop_expr_name with
		    | AOETupleTreePattern (input', pattern') ->
			let _ = replace_tuple_name ctxt f a root in
			indep, true
		    | _ -> op, false
		  end
	      | _ -> op, false
	    end
	|_ -> op, false
      end
  | _ -> op, false

(* *** TP extra 4  - Remove unnecessary sbdo-wrapping 
 * This is just clean-up, really
 *
 * fs:distinct-docorder(
 *   MapToItem { Input->(Input#glx:dot_new_1)}
 *     (TupleTreePattern[glx:dot_1][p](op)))
 * == 
 *  MapToItem { Input->(Input#glx:dot_new_1)}
 *     (TupleTreePattern[glx:dot_1][p](op))
 *)

let rec is_tp_op_with_output op out =
  match op.palgop_expr_name with
  | AOETupleTreePattern (input,p) ->
      let outputs = 
	Xquery_algebra_ast_util.get_restored_outputs_from_twig_pattern p 
      in
      (List.length outputs = 1)  && (Namespace_names.rqname_equal (List.hd outputs) out) 
  | AOEProject _ ->
      let sub = access_onesub op.psub_expression in
      is_tp_op_with_output sub out
  | AOESelect  _ -> 
      let sub = access_onesub op.psub_expression in
      is_tp_op_with_output sub out
  | _ -> false


let remove_ddo_rewrite cmp_ctxt op =
  match op.palgop_expr_name with
  | AOECallBuiltIn ((fn,arity), _, _, _) 
    when Namespace_names.rqname_equal fn fs_distinct_docorder ->
      begin
	let args = access_manysub op.psub_expression in
	let arg  = args.(0) in
	match arg.palgop_expr_name with
	| AOEMapToItem ->
	    begin
	      let dep = access_onesub arg.pdep_sub_expression in
	      let indep = access_onesub arg.psub_expression in
	      match dep.palgop_expr_name with
	      | AOEAccessTuple f ->
		  begin
		    if is_tp_op_with_output indep f 
		    then arg, true
		    else op, false
		  end
	      | _ -> op, false
	    end
	| _ -> op, false
      end
  | _ -> op, false

(*************************)
(* *** Rewrite Rules *** *)
(*************************)

let tree_pattern_rewrites = [
  (generic_wrapper treejoin_rewrite), "TreeJoin rewrite";
  (generic_wrapper standalone_treejoin_rewrite), "Standalone TreeJoin rewrite";
  (removal_wrapper item_to_tuple_rewrite), "Item to tuple rewrite";
  
  (generic_wrapper twig_inlining_rewrite), "Twig inlining rewrite";
  (generic_wrapper branching_twig_rewrite), "Branching twig rewrite";

  (generic_wrapper merge_projects_rewrite), "Merge projects rewrite"; 
  (generic_wrapper push_mapc_through_project), "Push MapConcat through project";
  (removal_wrapper mapconcat_rewrite), "MapConcat rewrite";

(*  (generic_wrapper remove_ddo_rewrite), "Remove ddo operations on tree patterns";  *)
]
