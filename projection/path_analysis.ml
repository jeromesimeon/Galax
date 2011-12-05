(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: path_analysis.ml,v 1.44 2007/08/17 18:28:54 simeon Exp $ *)

(* Module: Path_analysis
   Description:
     This module implements the path analysis algorithm used by
     projection.
*)

open Format

open Error
open Namespace_builtin

open Datatypes
open Dm

open Xquery_core_ast

open Path_struct
open Path_structutil
open Optim_vars
open Analysis_context
open Function_analysis


(* -------------------------------------------------------------------------
   Accesses the document id from the document function
-------------------------------------------------------------------------- *)

(* The pattern matching must assume an XQuery core ast which has undergone *)
(* rewritings; a function call will therefore have its arguments wrapped   *)
(* inside fs:convert-simple-operand(fn:data(.)).                 - Michael *)
let get_docid cexpr =
  let fun_arg1 opt_cexpr cfname =
    match opt_cexpr with
      | None -> None
      | Some cexpr ->
	  begin
	    match cexpr.pcexpr_desc with
	      | CECall(cfn, cexpr_l, types, _,_) ->
		  if (Namespace_names.rqname_equal cfn cfname)
		  then Some (List.hd cexpr_l)
		  else None
	      | CEVar cvn ->
		  None
	      | _ ->
		  None
	  end
  in
  let fn_doc_arg = fun_arg1 (Some cexpr) fn_doc in
  let fs_cso_arg = fun_arg1 fn_doc_arg fs_convert_simple_operand in
  let fn_data_arg = fun_arg1 fs_cso_arg fn_data in
    match fn_data_arg with
      | None -> None
      | Some arg ->
	    begin
	      match arg.pcexpr_desc with
		| CEScalar av ->
		    begin
		      try
			let uri_string = Xquery_common_ast.string_of_literal av in
			  (*let uri = AnyURI._uri_of_string uri_string in*)
			  (*let absolute_uri = AnyURI._uri_resolve base_uri uri in*)
			  (*let absolute_uri_string = AnyURI._string_of_uri absolute_uri in*)
			  (*Glx_http.glx_decode_url uri_string*)
			  Some uri_string
		      with
			| Query (Datamodel _) -> None
		    end
		| _ -> None
	    end



(* ------------------------------------------------------------------
   Serialize function: 
       Add subtrees to return
       Combine retrun and using
------------------------------------------------------------------- *)

let serialize (ps,ups) =
  let ps_s = imposes_subtree ps
  in Gmisc.remove_duplicates (ps_s @ ups)


(* ------------------------------------------------------------------
   Path Analysis function: 
      Without optimization
------------------------------------------------------------------- *)


let rec cexpr_path_analysis analysis_context cexpr =
  match cexpr.pcexpr_desc with
      (* I did not check this. This should do almost like optimization
         except that cexpr2 always returns a boolean - Jerome *)
  | CESome (dto, var, cexpr1, cexpr2)
  | CEEvery (dto, var, cexpr1, cexpr2) ->
      let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr1 in
      let analysis_context' = add_var_path analysis_context var path_seq1 in
      let (path_seq2, used_path_seq2) = cexpr_path_analysis analysis_context' cexpr2 in
      (path_seq2,path_seq1 @ used_path_seq1 @ used_path_seq2)

	(* Note this is without optimization. Optimization tries to
   	   remove path_seq1 from the second part of the returned
   	   pair. - Jerome and Amelie *)

  | CEFLWOR (cfl_exprs, cexpr_option, cexpr_order, cexpr3)->
  begin
  	let (analysis_context', paths_list) = List.fold_left cfl_expr_path_analysis (analysis_context,[]) cfl_exprs in
  	let (path_seq1,used_path_seq1) = 
  		match cexpr_option with
  		| Some cexpr_option' ->
  			 cexpr_path_analysis analysis_context' cexpr_option'
  		| None ->
  			([],[])
  			in
  	let (path_seq2,used_path_seq2) = 
  		match cexpr_order with
  		| Some (_,order_list, osig) ->
  			let (order_list',_,_) = Gmisc.triple_split order_list in
  			let path_lists = List.map (cexpr_path_analysis analysis_context') order_list' in
  			let (returns, useds) = List.split path_lists in
  			(List.concat returns, List.concat useds)
  		|	None ->
  			([],[])
  			in
  		let (path_seq3,used_path_seq3) = cexpr_path_analysis analysis_context' cexpr3 in
  		(path_seq3, paths_list @ path_seq1@ used_path_seq1 @ path_seq2@ used_path_seq2 @ used_path_seq3)
  	end
(*  	end
 
      raise (Query (Internal_Error "Projection: path analysis must be extended for the new FLWOR blocks"))
      *)

  | CEVar var ->
  		begin
	try
	  let path_sequence = get_var_path analysis_context var in
	  (path_sequence, [])
	with
	| _ ->
	    ([(InputVariable var,([],NoSubtree))],[])
      end

  | CESeq (cexpr1, cexpr2) ->
      let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr1
      and (path_seq2, used_path_seq2) = cexpr_path_analysis analysis_context cexpr2 in
      (path_seq1 @ path_seq2, used_path_seq1 @ used_path_seq2)

  | CEIf (cexpr1, cexpr2, cexpr3) ->
      let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr1
      and (path_seq2, used_path_seq2) = cexpr_path_analysis analysis_context cexpr2
      and (path_seq3, used_path_seq3) = cexpr_path_analysis analysis_context cexpr3 in
      (path_seq2 @ path_seq3, path_seq1 @ used_path_seq1 @ used_path_seq2 @ used_path_seq3)

  | CECast (cexpr1, nsenv, model) ->
      let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr1 in
      ([], path_seq1 @ used_path_seq1)
 
  | CECastable (cexpr1, nsenv, model) ->
      let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr1 in
      ([], path_seq1 @ used_path_seq1)
 
  | CEValidate (vmode,cexpr1) ->
      let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr1 in
      ([], path_seq1 @ used_path_seq1)

  | CEForwardAxis (v,axis, cnode_test) ->
      let path_seq = get_var_path analysis_context Xquery_common_ast.fs_dot in
      let axis_fun (docid,(path,subtree)) = (docid, (path @ [(axis, cnode_test)], subtree)) in
      (List.map axis_fun path_seq, [])

  | CEReverseAxis (v,axis, cnode_test) ->
      let path_seq = get_var_path analysis_context Xquery_common_ast.fs_dot in
      let axis_fun (docid,(path,subtree)) = (docid, (path @ [(axis, cnode_test)], subtree)) in
      (List.map axis_fun path_seq, [])

  | CEUnordered cexpr1 ->
      cexpr_path_analysis analysis_context cexpr1 
  | CEOrdered cexpr1 ->
      cexpr_path_analysis analysis_context cexpr1 

  | CEText _ -> ([],[])
  | CETextComputed cexpr1 ->
      let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr1 in
      let path_seq1' = imposes_subtree path_seq1 in  (*do we need the subtree here? should not pose problem. Amelie*)
      ([], path_seq1' @ used_path_seq1)

  | CEScalar _ -> ([],[])

  | CEEmpty -> ([],[])

  | CEComment _ -> ([],[])
  | CECommentComputed cexpr1 ->
      let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr1 in
      let path_seq1' = imposes_subtree path_seq1 in  (*do we need the subtree here? should not pose problem. Amelie*)
      ([], path_seq1' @ used_path_seq1)


  | CEPI _ -> ([],[])

  | CEPIComputed (cexpr1, cexpr2) ->
      let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr1
      and (path_seq2, used_path_seq2) = cexpr_path_analysis analysis_context cexpr2 in
      (path_seq1 @ path_seq2, used_path_seq1 @ used_path_seq2)

  | CEDocument cexpr1 ->
  		 (*let _ = fprintf Format.std_formatter "Document\n" in *)

      let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr1 in
      let path_seq1' = imposes_subtree path_seq1 in
      ([], path_seq1' @ used_path_seq1)

  | CEElem (symbol, nsenv, cexprlist) ->
      let (path_seq1', used_path_seq1) = 
	List.fold_right 
	  (fun cexpr (path_seqs, used_paths) -> 
	    let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr in
	    let path_seq1' = imposes_subtree path_seq1 in
	    (path_seq1' @ path_seqs, used_path_seq1 @ used_paths)) 
	  cexprlist ([], []) 
      in
      ([], path_seq1' @ used_path_seq1)

  | CEAttr (symbol, nsenv, cexprlist) ->
      let (path_seq1', used_path_seq1) = 
	List.fold_right 
	  (fun cexpr (path_seqs, used_paths) -> 
	    let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr in
	    let path_seq1' = imposes_subtree path_seq1 in (*do we need the subtree here? should not pose problem. Amelie*)
	    (path_seq1' @ path_seqs, used_path_seq1 @ used_paths)) 
	  cexprlist ([], []) 
      in
      ([], path_seq1' @ used_path_seq1)

  | CEAnyElem (cexpr1, nsenv1, nsenv2, cexpr2) ->
      let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr1 
      and (path_seq2, used_path_seq2) = cexpr_path_analysis analysis_context cexpr2 in  
      let path_seq1' = imposes_subtree path_seq1  (*do we need the subtree here? should not pose problem. Amelie*)
      and path_seq2' = imposes_subtree path_seq2 in
      ([], path_seq1' @ path_seq2' @ used_path_seq1 @ used_path_seq2)
      
  | CEAnyAttr (cexpr1, nsenv, cexpr2) ->
      let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr1 
      and (path_seq2, used_path_seq2) = cexpr_path_analysis analysis_context cexpr2 in  
      let path_seq1' = imposes_subtree path_seq1  (*do we need the subtree here? should not pose problem. Amelie*)
      and path_seq2' = imposes_subtree path_seq2 in  (*do we need the subtree here? should not pose problem. Amelie*)
      ([], path_seq1' @ path_seq2' @ used_path_seq1 @ used_path_seq2)

  | CETypeswitch (cexpr1, tsclause) ->
      let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr1 in
      let (path_seq2, used_path_seq2) = typeswitchclauses_path_analysis analysis_context tsclause path_seq1 in
      (path_seq2, path_seq1 @ used_path_seq1 @ used_path_seq2)
	
  | CEError cexprlist ->
      (* Mary: I'm not sure about this: *)
      let paths_list = List.map (cexpr_path_analysis analysis_context) cexprlist in
      let (return_paths,used_paths) = List.split paths_list in
      let return_paths' = List.concat return_paths in
      let used_paths' = List.concat used_paths in
      ([], return_paths' @ used_paths')
      (* Old code *)
      (* let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr in *)
      (* ([], path_seq1 @ used_path_seq1) *)

  | CETreat (cexpr, model) ->
      cexpr_path_analysis analysis_context cexpr

  | CECall (rfname, el, sign, _,_) ->
      begin
	if (Namespace_names.rqname_equal fn_doc rfname)
	then
	  let odocid = get_docid (*el*) cexpr in
	  match odocid with
	  | Some docid ->
	      ([(InputDocument docid,([],NoSubtree))],[])
	  | None ->
	      let paths_list = List.map (cexpr_path_analysis analysis_context) el in
	      let (return_paths,used_paths) = List.split paths_list in
	      let return_paths' = List.concat return_paths in
	      let used_paths' = List.concat used_paths in
	      let return_paths_subtree = imposes_subtree return_paths' in
	      (return_paths_subtree, used_paths')
	else
	 let paths_list = List.map (cexpr_path_analysis analysis_context) el in
         let (return_paths,used_paths) = List.split paths_list in
         let return_paths' = List.concat return_paths in
         let used_paths' = List.concat used_paths in
	 match get_fun_analysis_type rfname with 
	 | UsedOnly ->
	 (*let _ = fprintf Format.std_formatter "UsedOnly\n" in*)
	     ([],used_paths')
	 | UsedReturnSimple ->
	     (*  let _ = fprintf Format.std_formatter "ReturnSimple\n" in*)
	       ([],return_paths' @ used_paths')
	 | UsedReturnSubtree ->
	    (* let _ = fprintf Format.std_formatter "ReturnSubtree\n" in*)
	   let return_paths_subtree = imposes_subtree return_paths' in
	     ([], return_paths_subtree @ used_paths')
	 | ReturnsPaths ->
	     (*let _ = fprintf Format.std_formatter "ReturnsPaths\n" in*)
	    (return_paths', used_paths')
	 | ReturnsDefault ->
	      (*let _ = fprintf Format.std_formatter "Default\n" in*)
	    let return_paths_subtree = imposes_subtree return_paths' in
	     (return_paths_subtree, used_paths')
      end
  | CEOverloadedCall (rfname, el, sigs) ->
      begin
	if (Namespace_names.rqname_equal fn_doc rfname)
	then
	  let odocid = get_docid (*el*) cexpr in
	  match odocid with
	  | Some docid ->
	      ([(InputDocument docid,([],NoSubtree))],[])
	  | None ->
	      let paths_list = List.map (cexpr_path_analysis analysis_context) el in
	      let (return_paths,used_paths) = List.split paths_list in
	      let return_paths' = List.concat return_paths in
	      let used_paths' = List.concat used_paths in
	      let return_paths_subtree = imposes_subtree return_paths' in
	      (return_paths_subtree, used_paths')
	else
	 let paths_list = List.map (cexpr_path_analysis analysis_context) el in
         let (return_paths,used_paths) = List.split paths_list in
         let return_paths' = List.concat return_paths in
         let used_paths' = List.concat used_paths in
	 match get_fun_analysis_type rfname with 
	 | UsedOnly ->
	(* let _ = fprintf Format.std_formatter "UsedOnly\n" in*)
	     ([],used_paths')
	 | UsedReturnSimple ->
	    (* let _ = fprintf Format.std_formatter "ReturnSimple\n" in*)
	       ([],return_paths' @ used_paths')
	 | UsedReturnSubtree ->
	     (*let _ = fprintf Format.std_formatter "ReturnSubtree\n" in*)
	    let return_paths_subtree = imposes_subtree return_paths' in
	     ([], return_paths_subtree @ used_paths')
	 | ReturnsPaths ->
	     (*let _ = fprintf Format.std_formatter "ReturnsPaths\n" in*)
	    (return_paths', used_paths')
	 | ReturnsDefault ->
	   (*  let _ = fprintf Format.std_formatter "Default\n" in*)
	    let return_paths_subtree = imposes_subtree return_paths' in
	     (return_paths_subtree, used_paths')
      end
  | _ ->
      raise (Query (Prototype "Projection path analysis not supported for updates"))

and typeswitchclauses_path_analysis analysis_context tsclause input_path_seq = 
  match tsclause with
  | [] ->
      ([],[])
  | (_, ovar, cexpr1) :: tsclause_rest ->
      begin
	match ovar with
	|  Some var -> 
	    let analysis_context' = add_var_path analysis_context var input_path_seq in
	    let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context' cexpr1 in
	    let (path_seq2, used_path_seq2) = typeswitchclauses_path_analysis analysis_context tsclause_rest input_path_seq in
	    (path_seq1 @ path_seq2, used_path_seq1 @ used_path_seq2)
	| None ->
	   (* let _ = fprintf Format.std_formatter "No Matches" in *)
	    let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr1 in
	    let (path_seq2, used_path_seq2) = typeswitchclauses_path_analysis analysis_context tsclause_rest input_path_seq in
	    (path_seq1 @ path_seq2, used_path_seq1 @ used_path_seq2)
      end
      
and cfl_expr_path_analysis (analysis_context,used_path_seq) cfl_exprs=
	match cfl_exprs with 
	| CELET (dto,var,cexpr) -> 
		 let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr in
		 let analysis_context' = add_var_path analysis_context var path_seq1 in
		 (analysis_context',path_seq1@used_path_seq1@used_path_seq)
  | CEFOR (dto,var,ovar',cexpr) ->
  	let (path_seq1, used_path_seq1) = cexpr_path_analysis analysis_context cexpr in
		let analysis_context' = add_var_path analysis_context var path_seq1 in
		let analysis_context'' =
		match ovar' with
			| None -> analysis_context'
			| Some var' ->
			    add_var_path analysis_context' var' []
      in (analysis_context'',path_seq1@used_path_seq1@used_path_seq)
      
	


(* ---------------------------------------------------------------------------
   Path Analysis with optimization
---------------------------------------------------------------------------- *)


let rec cexpr_path_analysis_optim analysis_context cexpr =
  match cexpr.pcexpr_desc with
  | CESome (dto, var, cexpr1, cexpr2)
  | CEEvery (dto, var, cexpr1, cexpr2) ->
      let (path_seq1, used_path_seq1, optim_var1 ) = cexpr_path_analysis_optim analysis_context cexpr1 in
      let analysis_context' = add_var_path analysis_context var path_seq1 in
      let (path_seq2, used_path_seq2, optim_var2) = cexpr_path_analysis_optim analysis_context' cexpr2 in
      let optim_var3 = remove var optim_var2 in
      (path_seq2,path_seq1 @ used_path_seq1 @ used_path_seq2, (union optim_var1 optim_var3))

  | CEFLWOR (cfl_exprs, cexpr_option, cexpr_order, cexpr3)->
	  begin
	  	let (var_list, analysis_context', paths_list, optim_vars) = List.fold_left cfl_expr_path_analysis_optim ([],analysis_context,[], empty_optimvars) cfl_exprs in
	  	let (path_seq1,used_path_seq1,_) = 
	  		match cexpr_option with
	  		| Some cexpr_option' ->
	  			 cexpr_path_analysis_optim analysis_context' cexpr_option'
	  		| None ->
	  			([],[],empty_optimvars)
	  			in
	  	let (path_seq2,used_path_seq2,_) = 
	  		match cexpr_order with
	  		| Some (_,order_list, osig) ->
	  			let (order_list',_,_) = Gmisc.triple_split order_list in
	  			let path_lists = List.map (cexpr_path_analysis_optim analysis_context') order_list' in
	  			let (returns, useds,_) = Gmisc.triple_split path_lists in
	  			(List.concat returns, List.concat useds,empty_optimvars)
	  		|	None ->
	  			([],[],empty_optimvars)
	  		in
	  		let (path_seq3,used_path_seq3, optim_vars3) = cexpr_path_analysis_optim analysis_context' cexpr3 in
	  		let optim_vars' = remove_list var_list optim_vars in
	  		(path_seq3, paths_list @ path_seq1@ used_path_seq1 @ path_seq2@ used_path_seq2 @ used_path_seq3, (union optim_vars' optim_vars3))
  	end

  | CEVar var ->
      begin
	try
	  let path_sequence = get_var_path analysis_context var in
	  (path_sequence, [], single_optimvars var)
	with
	| _ ->
	    ([(InputVariable var,([],NoSubtree))],[],empty_optimvars)
      end

  | CESeq (cexpr1, cexpr2) ->
      let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context cexpr1
      and (path_seq2, used_path_seq2, optim_var2) = cexpr_path_analysis_optim analysis_context cexpr2 in
      (path_seq1 @ path_seq2, used_path_seq1 @ used_path_seq2, (intersection optim_var1 optim_var2))

  | CEIf (cexpr1, cexpr2, cexpr3) ->
      let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context cexpr1
      and (path_seq2, used_path_seq2, optim_var2) = cexpr_path_analysis_optim analysis_context cexpr2
      and (path_seq3, used_path_seq3, optim_var3) = cexpr_path_analysis_optim analysis_context cexpr3 in
      (path_seq2 @ path_seq3, path_seq1 @ used_path_seq1 @ used_path_seq2 @ used_path_seq3, intersection optim_var2 optim_var3)

  | CECast (cexpr1, nsenv, model) ->
      let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context cexpr1 in
      ([], path_seq1 @ used_path_seq1, optim_var1)
 
  | CECastable (cexpr1, nsenv, model) ->
      let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context cexpr1 in
      ([], path_seq1 @ used_path_seq1, optim_var1)
 
  | CEValidate (vmode,cexpr1) ->
      let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context cexpr1 in
      ([], path_seq1 @ used_path_seq1, optim_var1)

  | CEForwardAxis (v,axis, cnode_test) ->
      let path_seq = get_var_path analysis_context Xquery_common_ast.fs_dot in
      let axis_fun (docid,(path,subtree)) = (docid, (path @ [(axis, cnode_test)], subtree)) in
      (List.map axis_fun path_seq, [], single_optimvars Xquery_common_ast.fs_dot)

  | CEReverseAxis (v,axis, cnode_test) ->
      let path_seq = get_var_path analysis_context Xquery_common_ast.fs_dot in
      let axis_fun (docid,(path,subtree)) = (docid, (path @ [(axis, cnode_test)], subtree)) in
      (List.map axis_fun path_seq, [], single_optimvars Xquery_common_ast.fs_dot)

  | CEUnordered cexpr1 ->
      cexpr_path_analysis_optim analysis_context cexpr1 
  | CEOrdered cexpr1 ->
      cexpr_path_analysis_optim analysis_context cexpr1 

  | CEText _ -> ([],[],empty_optimvars)
	
  | CEScalar _ -> ([],[],empty_optimvars)

  | CEEmpty -> ([],[], all_optimvars)

  | CEComment _ -> ([],[],empty_optimvars)

  | CEPI _ -> ([],[],empty_optimvars)

  | CEPIComputed (cexpr1, cexpr2) ->
      let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context cexpr1
      and (path_seq2, used_path_seq2, optim_var2) = cexpr_path_analysis_optim analysis_context cexpr2 in
      (path_seq1 @ path_seq2, used_path_seq1 @ used_path_seq2, (intersection optim_var1 optim_var2))

  | CEDocument cexpr1 ->
      let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context cexpr1 in
      let path_seq1' = imposes_subtree path_seq1 in
      ([], path_seq1' @ used_path_seq1, empty_optimvars)

  | CEElem (symbol, nsenv, cexprlist) ->
      let (path_seq1', used_path_seq1) = 
	List.fold_right (fun cexpr1 (path_seqs, used_paths) -> 
	  let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context cexpr1 in
	  let path_seq1' = imposes_subtree path_seq1 in
	  (path_seq1' @ path_seqs, used_path_seq1 @used_paths)) cexprlist ([], [])
      in
      ([], path_seq1' @ used_path_seq1, empty_optimvars)

  | CEAttr (symbol, nsenv, cexprlist) ->
      let (path_seq1', used_path_seq1) = 
	List.fold_right (fun cexpr1 (path_seqs, used_paths) -> 
	  let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context cexpr1 in
	  let path_seq1' = imposes_subtree path_seq1 in (*do we need the subtree here? should not pose problem. Amelie*)
	  (path_seq1' @ path_seqs, used_path_seq1 @used_paths)) cexprlist ([], [])
      in
      ([], path_seq1' @ used_path_seq1, empty_optimvars)

  | CETextComputed cexpr1 ->
      let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context cexpr1 in
      let path_seq1' = imposes_subtree path_seq1 in  (*do we need the subtree here? should not pose problem. Amelie*)
      ([], path_seq1' @ used_path_seq1, empty_optimvars)
  | CECommentComputed cexpr1 ->
      let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context cexpr1 in
      let path_seq1' = imposes_subtree path_seq1 in  (*do we need the subtree here? should not pose problem. Amelie*)
      ([], path_seq1' @ used_path_seq1, empty_optimvars)

  | CEAnyElem (cexpr1, nsenv1, nsenv2, cexpr2) ->
      let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context cexpr1 
      and (path_seq2, used_path_seq2, optim_var2) = cexpr_path_analysis_optim analysis_context cexpr2 in  
      let path_seq1' = imposes_subtree path_seq1  (*do we need the subtree here? should not pose problem. Amelie*)
      and path_seq2' = imposes_subtree path_seq2 in
      ([], path_seq1' @ path_seq2' @ used_path_seq1 @ used_path_seq2, empty_optimvars)
  
  | CEAnyAttr (cexpr1, nsenv, cexpr2) ->
      let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context cexpr1 
      and (path_seq2, used_path_seq2, optim_var2) = cexpr_path_analysis_optim analysis_context cexpr2 in  
      let path_seq1' = imposes_subtree path_seq1  (*do we need the subtree here? should not pose problem. Amelie*)
      and path_seq2' = imposes_subtree path_seq2 in  (*do we need the subtree here? should not pose problem. Amelie*)
      ([], path_seq1' @ path_seq2' @ used_path_seq1 @ used_path_seq2, empty_optimvars) 

  | CETypeswitch (cexpr1, tsclause) ->
      let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context cexpr1 in
      let (path_seq2, used_path_seq2, optim_var2) = ts_path_analysis_optim analysis_context tsclause path_seq1 in
      (path_seq2, path_seq1 @ used_path_seq1 @ used_path_seq2, optim_var2) 
	
  | CEError cexprlist ->
      (* Mary: I'm not sure about this *)
      let paths_list = List.map (cexpr_path_analysis_optim analysis_context) cexprlist in
      let (return_paths,used_paths,optim_vars) = Gmisc.triple_split paths_list in
      let return_paths' = List.concat return_paths in
      let used_paths' = List.concat used_paths in
      ([], return_paths'@used_paths',empty_optimvars)
      (* Old code *)
      (* let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context cexpr in
	 ([], path_seq1 @ used_path_seq1,optim_var1) *)

  | CETreat (cexpr, model) ->
      cexpr_path_analysis_optim analysis_context cexpr

  | CECall (rfname, el, sign, _,_) ->
      begin
	if (Namespace_names.rqname_equal fn_doc rfname) 
	then
	  let odocid = get_docid (*el*) cexpr in
	  match odocid with
	  | Some docid ->
	      ([(InputDocument docid,([],NoSubtree))],[],empty_optimvars)
	  | None ->
	      let paths_list = List.map (cexpr_path_analysis_optim analysis_context) el in
	      let (return_paths,used_paths,optim_vars) = Gmisc.triple_split paths_list in
	      let return_paths' = List.concat return_paths in
	      let used_paths' = List.concat used_paths in
	      (* let optim_vars' = List.concat optim_vars in // use union here*)
	      let return_paths_subtree = imposes_subtree return_paths' in
	      (return_paths_subtree, used_paths',empty_optimvars)
	else
	  let paths_list = List.map (cexpr_path_analysis_optim analysis_context) el in
	  let (return_paths,used_paths,optim_vars) = Gmisc.triple_split paths_list in
	  let return_paths' = List.concat return_paths in
	  let used_paths' = List.concat used_paths in
	  (* let optim_vars' = List.concat optim_vars in // use union here*)
	  match get_fun_analysis_type rfname with 
	 | UsedOnly ->
			 ([],used_paths',empty_optimvars)
	 | UsedReturnSimple ->
	 		([],return_paths' @ used_paths',empty_optimvars)
	 | UsedReturnSubtree ->
	  	let return_paths_subtree = imposes_subtree return_paths' in
	     ([], return_paths_subtree @ used_paths',empty_optimvars)
	 | ReturnsPaths ->
	  	(return_paths', used_paths',empty_optimvars)
	 | ReturnsDefault ->
	  	let return_paths_subtree = imposes_subtree return_paths' in
	     (return_paths_subtree, used_paths',empty_optimvars)
     
      end
  | CEOverloadedCall (rfname, el, sigs) ->
      begin
	if (Namespace_names.rqname_equal fn_doc rfname) 
	then
	  let odocid = get_docid (*el*) cexpr in
	  match odocid with
	  | Some docid ->
	      ([(InputDocument docid,([],NoSubtree))],[],empty_optimvars)
	  | None ->
	      let paths_list = List.map (cexpr_path_analysis_optim analysis_context) el in
	      let (return_paths,used_paths,optim_vars) = Gmisc.triple_split paths_list in
	      let return_paths' = List.concat return_paths in
	      let used_paths' = List.concat used_paths in
	      (* let optim_vars' = List.concat optim_vars in // use union here*)
	      let return_paths_subtree = imposes_subtree return_paths' in
	      (return_paths_subtree, used_paths',empty_optimvars)
	else
	  let paths_list = List.map (cexpr_path_analysis_optim analysis_context) el in
	  let (return_paths,used_paths,optim_vars) = Gmisc.triple_split paths_list in
	  let return_paths' = List.concat return_paths in
	  let used_paths' = List.concat used_paths in
	  (* let optim_vars' = List.concat optim_vars in // use union here*)
	  match get_fun_analysis_type rfname with 
	 | UsedOnly ->
	     ([],used_paths',empty_optimvars)
	 | UsedReturnSimple ->
	      ([],return_paths' @ used_paths',empty_optimvars)
	 | UsedReturnSubtree ->
	     let return_paths_subtree = imposes_subtree return_paths' in
	     ([], return_paths_subtree @ used_paths',empty_optimvars)
	 | ReturnsPaths ->
	     (return_paths', used_paths',empty_optimvars)
	 | ReturnsDefault ->
	     let return_paths_subtree = imposes_subtree return_paths' in
	     (return_paths_subtree, used_paths',empty_optimvars)
     
      end
  | _ ->
      raise (Query (Prototype "Projection path analysis not supported for updates"))

and ts_path_analysis_optim analysis_context tsclause input_path_seq = 
  match tsclause with
  | [] ->
      ([],[],empty_optimvars)
  | (_, ovar, cexpr1) :: tsclause_rest ->
      begin
	match ovar with
	|  Some var -> 
	    let analysis_context' = add_var_path analysis_context var input_path_seq in
	    let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context' cexpr1 in
	    let (path_seq2, used_path_seq2, optim_var2) = ts_path_analysis_optim analysis_context tsclause_rest input_path_seq in
	    (path_seq1 @ path_seq2, used_path_seq1 @ used_path_seq2, (intersection optim_var1 optim_var2))
	| None ->
	    let (path_seq1, used_path_seq1, optim_var1) = cexpr_path_analysis_optim analysis_context cexpr1 in
	    let (path_seq2, used_path_seq2, optim_var2) = ts_path_analysis_optim analysis_context tsclause_rest input_path_seq in
	    (path_seq1 @ path_seq2, used_path_seq1 @ used_path_seq2, (intersection optim_var1 optim_var2))
      end 
      
 and cfl_expr_path_analysis_optim (var_list,analysis_context,used_path_seq,optim_vars) cfl_exprs=
 	match cfl_exprs with 
 	| CELET (dto,var,cexpr) -> 
 		 let (path_seq1, used_path_seq1,optim_vars1) = cexpr_path_analysis_optim analysis_context cexpr in
 		 let analysis_context' = add_var_path analysis_context var path_seq1 in
 		 let newvar_list = var::var_list in
 		 (newvar_list, analysis_context',path_seq1@used_path_seq1@used_path_seq,(union optim_vars optim_vars1))
   | CEFOR (dto,var,ovar',cexpr) ->
   	let (path_seq1, used_path_seq1,optim_vars1) = cexpr_path_analysis_optim analysis_context cexpr in
 		let analysis_context' = add_var_path analysis_context var path_seq1 in
 		let newvar_list = var::var_list in
 		let analysis_context'' =
 		match ovar' with
 			| None -> analysis_context'
 			| Some var' ->
 			    add_var_path analysis_context' var' []
      in (newvar_list, analysis_context'',path_seq1@used_path_seq1@used_path_seq,(intersection optim_vars optim_vars1))

(* ---------------------------------------------------------------------------
   Analysis for query prolog
---------------------------------------------------------------------------- *)

let query_functions_analysis analysis_context cfunction_def =
  (* Note: We will eventually have to do some analysis of the
       user-defined functions. - Jerome and Amelie *)
  (analysis_context,[])

let query_var_analysis analysis_context vardecl =
  match vardecl.pcvar_decl_desc with
  | (vname, cdt, CEVarUser cexpr) ->
      let (path_seq, used_path_seq) = cexpr_path_analysis analysis_context cexpr in
      let analysis_context' = add_var_path analysis_context vname path_seq in
      (*let _ = print_path_sequence Format.std_formatter path_seq in
      let _ = print_path_sequence Format.std_formatter used_path_seq in*)
 	(analysis_context', used_path_seq)
  | (vname, cdt, CEVarInterface) 
  | (vname, cdt, CEVarImported) 
  | (vname, cdt, CEVarExternal) ->
      raise (Query (Prototype "Do not know yet what to do with external or imported variables during projection!"))

let rec query_vars_analysis analysis_context vardecls =
  match vardecls with
  | [] -> 
  	(*let _ = fprintf Format.std_formatter "\nNo Vars\n" in*)
  (analysis_context,[])
  | vardecl :: vardecls' ->
  	(*	let _ = fprintf Format.std_formatter "\nYES\n" in*)
  		let (analysis_context',used_path_seq') = query_var_analysis analysis_context vardecl in
      let (analysis_context'',used_path_seq'') = query_vars_analysis analysis_context' vardecls' in
   (analysis_context'', used_path_seq' @ used_path_seq'')

let query_prolog_analysis analysis_context cprolog =
	 let (analysis_context',used_path_seq') = query_functions_analysis analysis_context cprolog.Xquery_core_ast.pcprolog_functions in
   let (analysis_context'',used_path_seq'') = query_vars_analysis analysis_context' cprolog.Xquery_core_ast.pcprolog_vars in
 (analysis_context'', used_path_seq' @ used_path_seq'')
  

let query_functions_analysis_optim analysis_context cfunction_def =
  (* Note: We will eventually have to do some analysis of the
       user-defined functions. - Jerome and Amelie *)
  (analysis_context,[])

let query_var_analysis_optim analysis_context vardecl =
  match vardecl.pcvar_decl_desc with
  | (vname, dt, CEVarUser cexpr) ->
  let (path_seq, used_path_seq,_) = cexpr_path_analysis_optim analysis_context cexpr in
  let analysis_context' = add_var_path analysis_context vname path_seq in
  (analysis_context', used_path_seq)
  | (vname, dt, CEVarInterface) 
  | (vname, dt, CEVarImported) 
  | (vname, dt, CEVarExternal) ->
      raise (Query (Prototype "Do not know yet what to do with external or imported variables during projection!"))


let rec query_vars_analysis_optim analysis_context vardecls =
  match vardecls with
  | [] -> (analysis_context,[])
  | vardecl :: vardecls' ->
      let (analysis_context',used_path_seq') = query_vars_analysis_optim analysis_context vardecls' in
      let (analysis_context'',used_path_seq'') = query_var_analysis_optim analysis_context' vardecl in
      (analysis_context'', used_path_seq' @ used_path_seq'')

let query_prolog_analysis_optim analysis_context cprolog =
  let (analysis_context',used_path_seq') = query_functions_analysis analysis_context cprolog.pcprolog_functions in
  let (analysis_context'',used_path_seq'') = query_vars_analysis analysis_context' cprolog.pcprolog_vars in
  (analysis_context'', used_path_seq' @ used_path_seq'')


(* ---------------------------------------------------------------------------
   Analysis for single statements
---------------------------------------------------------------------------- *)

let path_analysis_of_cexpr analysis_context cexpr =
  serialize(cexpr_path_analysis analysis_context  cexpr)
  
let path_analysis_of_cexpr_optim  analysis_context cexpr =
  let (returns, used,_) = cexpr_path_analysis_optim analysis_context  cexpr in
	  serialize(returns,used)


(*let path_analysis_of_cupdate cupdate =
  raise (Query (Prototype "Analysis: Updates not supported yet")) *)

let path_analysis_of_cstatement analysis_ctxt cstatement =
 (* match cstatement.pcstatement_desc with
  | CSExpression ce ->*)
      path_analysis_of_cexpr analysis_ctxt cstatement
 (* | CSUpdate cu ->
      path_analysis_of_cupdate analysis_ctxt cu *)
      
 let path_analysis_of_cstatement_optim analysis_ctxt cstatement =
 (*  match cstatement.pcstatement_desc with
   | CSExpression ce ->*)
       path_analysis_of_cexpr_optim analysis_ctxt cstatement
 (*  | CSUpdate cu ->
      path_analysis_of_cupdate analysis_ctxt cu*)

let cstatement_path_analysis analysis_ctxt cstatement =
 (* match cstatement.pcstatement_desc with
  | CSExpression ce ->*)
      cexpr_path_analysis analysis_ctxt cstatement
(*  | CSUpdate  cu ->
      raise (Query (Prototype "Analysis: Updates not supported yet")) *)



let cstatement_path_analysis_optim analysis_ctxt cstatement =
(*  match cstatement.pcstatement_desc with
  | CSExpression ce ->*)
      cexpr_path_analysis_optim analysis_ctxt cstatement
      
  (*| CSUpdate cu ->
      path_analysis_of_cupdate analysis_ctxt cu *)
(*let path_analysis_of_cstatement_optim cstatement =
  serialize(cstatement_path_analysis_optim (build_analysis_context()) cstatement)
*)
(* ---------------------------------------------------------------------------
   Analysis for complete queries
---------------------------------------------------------------------------- *)

let path_analysis_of_cprolog cprolog =
  (* Dealing with the prolog *)
  let (analysis_context,used_path_seq) =
    query_prolog_analysis (build_analysis_context ()) cprolog
  in

  (* Final serialization *)
  (serialize ([], used_path_seq),analysis_context)

let path_analysis_of_cxmodule cqmodule =
  (* Dealing with the prolog *)
  let (analysis_context,used_path_seq) =
    query_prolog_analysis (build_analysis_context ()) cqmodule.pcmodule_prolog
  in

  (* Analysis for each expression in the module *)
  let statements = cqmodule.pcmodule_statements in

  (* Merging the paths *)
  let paths_list = List.map (cstatement_path_analysis analysis_context) statements in
  let (return_paths,used_paths) = List.split paths_list in
  let return_paths' = List.concat return_paths in
  let used_paths' = List.concat used_paths in 

  (* Final serialization *)
   let paths' = serialize (return_paths', used_path_seq @ used_paths') in 
   (paths',analysis_context) 
   

let path_analysis_of_cxmodule_optim cqmodule =
  (* Dealing with the prolog *)
  let (analysis_context,used_path_seq) =
    query_prolog_analysis_optim (build_analysis_context ()) cqmodule.pcmodule_prolog
  in

  (* Analysis for each expression in the module *)
  let statements = cqmodule.pcmodule_statements in

  (* Merging the paths *)
  let paths_list = List.map (cstatement_path_analysis_optim analysis_context) statements in

  let (return_paths,used_paths,_) = Gmisc.triple_split paths_list in
  let return_paths' = List.concat return_paths in
  let used_paths' = List.concat used_paths in

  (* Final serialization *)
  let paths' = serialize (return_paths', used_path_seq @ used_paths') in
  (paths',analysis_context)


(* ---------------------------------------------------------------------------
   Printing of analysis results
---------------------------------------------------------------------------- *)


let print_intermediate_analysis ff (ps1,ps2) =
  begin
    fprintf ff "-------------------------------------\nRETURN\n";
    print_path_sequence ff ps1;
    fprintf ff "USING\n";
    print_path_sequence ff ps2;
    fprintf ff "-------------------------------------\n";
  end

let print_full_analysis ff ps =
  begin
    (*fprintf ff "-------------------------------------\nPATHS\n";*)
    print_path_sequence ff ps;
   (* fprintf ff "-------------------------------------\n";*)
  end
