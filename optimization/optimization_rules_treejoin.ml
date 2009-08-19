(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_rules_treejoin.ml,v 1.5 2007/02/12 21:15:33 simeon Exp $ *)

(* Module: Optimization_rules_treejoin
   Description:
     This module implements the treejoin optimization rules, as
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
open Compile_annotate

open Optimization_util
open Optimization_walker

(*************************************************************************)
(* Introducing Streamable TreeJoins                                      *)
(* To obtain optimal streaming plans, we need plain chained TreeJoins,   *)
(* without Maps in between.                                              *) 
(*************************************************************************)

(******************************************)
(* Some module-specific utility functions *)
(******************************************)

(* Check wether op is a parse-operator, wrapped in a map *)
let is_parse_op_to_field f op =
  match op.palgop_expr_name with
  | AOEMapFromItem a ->
      begin
	let indep = access_onesub op.psub_expression in
	let dep = access_onesub op.pdep_sub_expression in
	match dep.palgop_expr_name with
	| AOECreateTuple flds
	  when Array.length flds = 1 ->
	    begin
	      let (_,create_field) = flds.(0) in
	      if create_field = f then
		match indep.palgop_expr_name with
		| AOEParse str  -> Some indep
		| _ -> None
	      else None
	    end
	| _ -> None
      end
  | _ -> None

(* wrap an operator in an fn:boolean call *)
let wrap_in_boolean op eh fi =
  let st1 = {pasequencetype_desc = (AITKindTest(AAnyKind),None); 
             pasequencetype_loc  = fi} 
  in
  let st2 = {pasequencetype_desc = (AITAtomic(Namespace_symbols.booleansym),None); 
             pasequencetype_loc = fi}
  in
  logical_aalgop_mkop 
    (AOECallBuiltIn ((fn_boolean, 1),[|Some st1|],st2,Xquery_common_ast.NonUpdating ))
    (ManySub [| op |]) 
    (NoSub) None eh fi 

(* wrap an operator in a select with given predicate(s)  *)
let wrap_in_select comp_ctxt fld input_op select_ops eh fi =
  let i = get_new_var_name comp_ctxt in
  let input_j = logical_aalgop_mkop (AOEVar i) NoSub NoSub None eh fi in
  let tc = logical_aalgop_mkop (AOECreateTuple [|(None,fld)|]) (ManySub [|input_j|]) NoSub None eh fi 
  in
  let map = logical_aalgop_mkop (AOEMapFromItem i) (OneSub input_op) (OneSub tc) None eh fi  in
  let create_pd select_ops pos = SimpleConjunct (0, Array.length select_ops -1) in
  let select_op =
    let pd = create_pd select_ops 0 in
    logical_aalgop_mkop (AOESelect pd) (OneSub map) (ManySub select_ops) None eh fi
  in
  let access = logical_aalgop_mkop (AOEAccessTuple fld) NoSub NoSub None eh fi in
  logical_aalgop_mkop AOEMapToItem (OneSub select_op) (OneSub access) None eh fi 

let get_tuplefield_name op =
  match op.palgop_expr_name with 
  | AOECreateTuple fa
    when Array.length fa = 1 ->
      let (_,f) = fa.(0) in
      Some f 
  | AOEAccessTuple f ->
      Some f
  | _ -> None 

(***********************************************)
(* rewriting TTP predicates into selects:      *)
(*  1) map input_op to tuples                  *)
(*  2) wrap select_ops into fn:boolean calls   *)
(*     using and-connectives                   *)
(*  3) wrap up in select and map back to items *)
(***********************************************)
  
(***************)
(* Some errors *)
(***************)
let nt_error = (Query (Malformed_Algebra_Expr  ("Tp_to_tj: Missing node test."))) 
let fld_error = (Query (Malformed_Algebra_Expr  ("Tp_to_tj: Missing field."))) 
let tp_error = (Query (Malformed_Algebra_Expr  ("Tp_to_tj: Expecting a TTP."))) 
let fld_error' = (Query (Malformed_Algebra_Expr  ("Tp_to_tj: Expecting a single output field."))) 

(* Take a TreePattern and convert it to a nesting of bulk treejoins *)
let rec treepattern_to_treejoins comp_ctxt tp_op input_op start =
  let p = 
    match tp_op.palgop_expr_name with
    | AOETupleTreePattern (i, p) -> p
    | _ -> raise tp_error
  in
  let eh = tp_op.palgop_expr_origin in
  let fi = tp_op.palgop_expr_loc in

  let rec tp2tj_helper index op =

    let twignode = p.(index) in
    match twignode.child_twig with
    | Some (axis, i) ->
	begin
	let nt =
	  match p.(i).node_test with
	  | Some nt -> nt
	  | None -> raise  nt_error
	in
	let new_op = logical_aalgop_mkop (AOETreeJoin(axis,nt)) (OneSub op) NoSub None eh fi in
	match p.(i).pred_twigs with
	| [] -> tp2tj_helper i new_op
	| _  ->
	    begin
	    let fld = 
	      match p.(i).out with
	      | Some f -> f
	      | None -> raise fld_error
	    in
	    let new_op' = wrap_in_select comp_ctxt fld new_op (get_predicate_treejoins comp_ctxt tp_op i) eh fi in
	    tp2tj_helper i new_op'
	    end
	end
    | None -> op
  in
  tp2tj_helper start input_op

(* recursively map the predicates into nested bulk treejoin operators *)
and get_predicate_treejoins comp_ctxt tp_op index =
  let eh = tp_op.palgop_expr_origin in
  let fi = tp_op.palgop_expr_loc in
  
  let create_tj tp f (ax, index') = 
    let node_test = 
      match tp.(index').node_test with
      | Some nt -> nt
      | _ -> raise nt_error
    in
    let ta = logical_aalgop_mkop (AOEAccessTuple f) NoSub NoSub None eh fi in
    let fs = logical_aalgop_mkop (AOETreeJoin(ax,node_test)) (OneSub ta) NoSub None eh fi in
    let tj = treepattern_to_treejoins comp_ctxt tp_op fs index' in
    wrap_in_boolean tj eh fi
  in
  match tp_op.palgop_expr_name with 
  | AOETupleTreePattern (i, tp) ->
      begin
	match tp.(index).pred_twigs with
	| [] -> [||]
	| lst -> 
	    begin
	      match tp.(index).out with
	      | Some field -> Array.map (create_tj tp field) (Array.of_list lst)
	      | None -> raise fld_error
	    end
      end
  | _ -> [| |]
    
(* Map an entire TTP operator to nested TreeJoin operators using the functions above *)
let tj_treepattern_to_treejoin comp_ctxt op =
  match op.palgop_expr_name with
  | AOEMapToItem ->
      begin
	let map_dep = access_onesub op.pdep_sub_expression in
	let map_indep = access_onesub op.psub_expression in
	if is_access_tuple map_dep then
	  match map_indep.palgop_expr_name with
	  | AOETupleTreePattern (i, p)->
	      begin
		if is_streamable_treepattern p then
		  let indep = access_onesub map_indep.psub_expression in
		  (* Fixme: parse-op requirement is not necessary *)
		  match is_parse_op_to_field i indep with
		  | Some parse_op -> treepattern_to_treejoins comp_ctxt map_indep parse_op 0, true
		  | _ -> op, false
		else
		  let _ = 
		    if Debug.default_debug() then Debug.print_default_debug " -> Not introducing treejoin: not a streamable pattern\n" 
		  in op, false
	      end
	  | _ -> op, false
	else
	  op, false
      end
  | _ -> op, false


(* Helper function for ttp_to_treejoin_generic *)
let build_map_from_item comp_ctxt fld input eh fi =
  let i = get_new_var_name comp_ctxt in
  let input_j = logical_aalgop_mkop (AOEVar i) NoSub NoSub None eh fi in
  let tc = logical_aalgop_mkop (AOECreateTuple [|(None,fld)|]) (ManySub [|input_j|]) NoSub None eh fi 
  in
  logical_aalgop_mkop (AOEMapFromItem i) (OneSub input) (OneSub tc) None eh fi

(*
 * TupleTreePattern [dot1](s1/.../sk{dot2}* ) (indep)
 * ==
 * MapConcat {
 *   MapFromItem{glx:comp1 -> [glx:dot2 : $glx:comp1]}(
 *     TreeJoin[sk](...(TreeJoin[s1](INPUT#dot1))))
 * } (indep)
 *)
let ttp_to_treejoin_generic comp_ctxt op =
  let eh = op.palgop_expr_origin in
  let fi = op.palgop_expr_loc in
  let fail = op, false in
  match op.palgop_expr_name with
  | AOETupleTreePattern (i, p) ->
      begin
	let outfs = get_restored_outputs_from_twig_pattern p in
        match outfs with
	| [ f ] ->
	    let indep = access_onesub op.psub_expression in
	    let ta  = logical_aalgop_mkop (AOEAccessTuple i) NoSub NoSub None eh fi in
	    let tjs = treepattern_to_treejoins comp_ctxt op ta 0 in
            let mfi = build_map_from_item comp_ctxt f tjs eh fi in
	    logical_aalgop_mkop 
	      AOEMapConcat (OneSub indep) (OneSub mfi) None eh fi, true
	| _ -> raise fld_error'
      end
  | _ -> fail

let pull_up_ttp_as_treejoin comp_ctxt op =
  let eh = op.palgop_expr_origin in
  let fi = op.palgop_expr_loc in
  let fail = op, false in
  match op.palgop_expr_name with
  | AOEMapToItem ->
      begin
	let dep = access_onesub op.pdep_sub_expression in
	let indep = access_onesub op.psub_expression in
	match dep.palgop_expr_name with
	| AOEAccessTuple f ->
	    begin
	      match indep.palgop_expr_name with 
	      | AOETupleTreePattern (i, p) ->
		  begin
		    let it = access_onesub  indep.psub_expression in
		    match it.palgop_expr_name with
		    | AOEInputTuple ->
			begin
			  let input_op = logical_aalgop_mkop 
			      (AOEAccessTuple i) NoSub NoSub None eh fi  in
			  treepattern_to_treejoins comp_ctxt indep input_op 0, true
			end
		    | _ -> fail 
		  end
	      | _ -> fail
	    end
	| _ -> fail
      end
  | _ -> fail

(* more conservative and robust version of the above *)
let pull_up_conservative comp_ctxt op =
  let eh = op.palgop_expr_origin in
  let fi = op.palgop_expr_loc in
  let fail = op, false in
  match op.palgop_expr_name with
  | AOEMapToItem ->
      begin
	let dep = access_onesub op.pdep_sub_expression in
	let indep = access_onesub op.psub_expression in
	match dep.palgop_expr_name with
	| AOEAccessTuple f ->
	    begin
	      match indep.palgop_expr_name with 
	      | AOETupleTreePattern (i, p) ->
		  begin
		    let ta = logical_aalgop_mkop (AOEAccessTuple i) NoSub NoSub None eh fi  in
		    op.psub_expression <- indep.psub_expression;
		    op.pdep_sub_expression <- (OneSub ta);
		    treepattern_to_treejoins comp_ctxt indep op 0, true
		  end
	      | _ -> fail
	    end
	| _ -> fail
      end
  | _ -> fail


let push_down_ttp_as_treejoin comp_ctxt op =
  let fail = op, false in
  match op.palgop_expr_name with
  | AOETupleTreePattern (i,p) ->
      begin
	let outfield = List.hd (get_restored_outputs_from_twig_pattern p) in
	let map = access_onesub op.psub_expression in
	match map.palgop_expr_name with
	| AOEMapFromItem i ->
	    begin
	      let tc = access_onesub map.pdep_sub_expression in
	      match tc.palgop_expr_name with
	      | AOECreateTuple f 
		when Array.length f = 1 ->
		  begin
		    let (t,_) = f.(0) in
		    let input_op = access_onesub map.psub_expression in
		    let tj_ops = treepattern_to_treejoins comp_ctxt op input_op 0 in
		    let _ = map.psub_expression <- (OneSub tj_ops) in
		    let _ = f.(0) <- (t,outfield) in
 		    map, true		    
		  end
	      | _ -> fail
	    end
	| _ -> fail 
      end
  | _ -> fail

let map_to_from_rewrite comp_ctxt op =
  let fail = op, false in
  let match_tuple_fields op1 op2 =
    match get_tuplefield_name op1, get_tuplefield_name op2 with
    | Some f, Some f' -> Namespace_names.rqname_equal f f' 
    | _ -> false
  in
  match op.palgop_expr_name with
  | AOEMapToItem ->
      begin
	let dep = access_onesub op.pdep_sub_expression in
	let indep = access_onesub op.psub_expression in
	match indep.palgop_expr_name with
	| AOEMapFromItem _ ->
	    begin
	      let dep' = access_onesub indep.pdep_sub_expression in
	      let indep' = access_onesub indep.psub_expression in
	      if match_tuple_fields dep dep' then
		indep', true
	      else fail
	    end
	| AOEProject flds ->
	    begin
	      let indep' = access_onesub indep.psub_expression in
	      match indep'.palgop_expr_name with
	      | AOEMapFromItem _ ->
		  let dep'' = access_onesub indep'.pdep_sub_expression in
		  let indep'' = access_onesub indep'.psub_expression in
		  if match_tuple_fields dep dep'' then
		    indep'', true
		  else fail
	      | _ -> fail
	    end
	| _ -> fail
      end
  | _ -> fail

(************************)
(* The rewrite rule set *)
(************************)
let treejoin_rewrites =
 [
  (generic_snap_free_wrapper pull_up_ttp_as_treejoin), 
     "Streaming Rewrite, pull up TreeJoin";

  (generic_snap_free_wrapper push_down_ttp_as_treejoin), 
     "Streaming Rewrite, push down TreeJoin";

  (generic_snap_free_wrapper ttp_to_treejoin_generic), 
     "Streaming Rewrite, TTP generic rule";

  (generic_snap_free_wrapper map_to_from_rewrite), 
     "Streaming Rewrite, MapTo over MapFrom elimination";

  (generic_snap_free_wrapper pull_up_conservative), 
     "Streaming Rewrite, Pull up TJ conservative" 
]
