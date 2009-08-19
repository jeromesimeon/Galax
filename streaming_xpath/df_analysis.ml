(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: df_analysis.ml,v 1.17 2007/08/17 18:28:54 simeon Exp $ *)

(* Module: Df_analysis
   Description:
     This module provides

   1) a concrete instantiation of the generic data flow graph defined in Df_graph
   2) algorithms for deriving such an instance from a given XQuery core module.

   Since the data flow analysis is a whole-program analysis by nature, the module
   does not provide functions operating on individual parts of a query.

   - Michael *)


open Xquery_common_ast
open Xquery_core_ast
open Xquery_core_ast_util

open Error

open Df_struct

open Df_analysis_context


(* Just storing a reference to the original XQuery core ast node does not
   provide enough contextual information to uniquely determine the function
   of a dfnode. For some acexpr kinds, additional hints must be stored in
   addition to its origin. *)

type ac_hint =

  (* flwor *)
  | ACHFor of cvname
  | ACHLet of cvname
  | ACHOrderby
  | ACHWhere
      
  (* typeswitch *)
  | ACHTsclause of cvname

  (* module level *)
  | ACHVar of cvname

type ac_handle = acexpr option * ac_hint option


(**********************************)
(* XQuery core data flow analysis *)
(**********************************)

let rec df_analysis_of_cexpr_internal dfa_ctxt cexpr =
  let default_ac_handle = (Some cexpr, None) in
    match cexpr.pcexpr_desc with
	
      (***********************)
      (* sink core ast nodes *)
      (***********************)
	
      | CEText _
      | CEComment _
      | CEPI _
	  
      | CEEmpty
      | CEScalar _ ->
	  let n = mkdfnode_dfpass DFImmediate default_ac_handle in
	  let _ = affiliate_unary n in
	    mkdfgraph_singleton n
	      
	      
      (******************************)
      (* non-forking core ast nodes *)
      (******************************)
	      
      (* no input *)
	      
      | CEVar var ->
	  get_var_dfgraph dfa_ctxt var
	    
	    
      (* single input *)
	    
      | CEUnordered cexpr1
      | CEOrdered cexpr1
	  
      | CEDocument cexpr1
      | CETextComputed cexpr1
      | CECommentComputed cexpr1
	  
      | CEValidate (_, cexpr1)
	  
      | CETreat (cexpr1, _)
      | CECast (cexpr1, _, _)
      | CECastable (cexpr1, _, _) ->
	  let n_unord = mkdfnode_dfpass DFUnordered default_ac_handle in
	  let n_merge = mkdfnode_dfpass DFMerge default_ac_handle in
	  let _ = affiliate_binary n_unord n_merge in
	    
	  let g_cexpr1 = df_analysis_of_cexpr_internal dfa_ctxt cexpr1 in
	  let g_unord = terminate_unary n_unord g_cexpr1 in
	    terminate_unary n_merge g_unord
	      
      | CEForwardAxis _
      | CEReverseAxis _ ->
	  let n_unord = mkdfnode_dfpass DFUnordered default_ac_handle in
	  let n_merge = mkdfnode_dfpass DFMerge default_ac_handle in
	  let _ = affiliate_binary n_unord n_merge in
	    
	  let g_fs_dot = get_var_dfgraph dfa_ctxt fs_dot in
	  let g_unord = terminate_unary n_unord g_fs_dot in
	    terminate_unary n_merge g_unord
	      
	      
      (* double input *)
	      
      | CEAnyElem (cexpr1, _, _, cexpr2)
      | CEAnyAttr (cexpr1, _, cexpr2)
      | CEPIComputed (cexpr1, cexpr2)
	  
      | CESeq (cexpr1, cexpr2) ->
	  let n_ord1 = mkdfnode_dfpass (DFOrdered 1) default_ac_handle in
	  let n_ord2 = mkdfnode_dfpass (DFOrdered 2) default_ac_handle in
	  let n_merge = mkdfnode_dfpass DFMerge default_ac_handle in
	  let _ = affiliate_many [n_ord1; n_ord2; n_merge] in
	    
	  let g_cexpr1 = df_analysis_of_cexpr_internal dfa_ctxt cexpr1 in
	  let g_cexpr2 = df_analysis_of_cexpr_internal dfa_ctxt cexpr2 in
	  let g_ord1 = terminate_unary n_ord1 g_cexpr1 in
	  let g_ord2 = terminate_unary n_ord2 g_cexpr2 in
	    terminate_binary n_merge g_ord1 g_ord2
	      
	      
      (* multiple input *)
	      
      | CEError cexprs
	  
      | CEOverloadedCall (_, cexprs, _)
      | CECall (_, cexprs, _, _, _)
	  
      | CEAttr (_, cexprs)
      | CEElem (_, _, cexprs) ->
	  let i = ref 0 in
	    
	  let mk_int x = i := !i + 1; !i in
	  let mk_n_ord i = mkdfnode_dfpass (DFOrdered i) default_ac_handle in
	    
	  let ints = List.map mk_int cexprs in
	  let n_ords = List.map mk_n_ord ints in
	  let n_merge = mkdfnode_dfpass DFMerge default_ac_handle in
	  let _ = affiliate_many (n_merge :: n_ords) in
	    
	  let g_cexprs = List.map (df_analysis_of_cexpr_internal dfa_ctxt) cexprs in
	  let g_ords = List.map2 terminate_unary n_ords g_cexprs in
	    terminate_many n_merge g_ords
	      
      | CEIf (cexpr1, cexpr2, cexpr3) ->
	  let n_con = mkdfnode_dfsink DFControl default_ac_handle in
	  let n_ord1 = mkdfnode_dfpass (DFOrdered 1) default_ac_handle in
	  let n_ord2 = mkdfnode_dfpass (DFOrdered 2) default_ac_handle in
	  let n_merge = mkdfnode_dfpass DFMerge default_ac_handle in
	  let _ = affiliate_many [n_con; n_ord1; n_ord2; n_merge] in
	    
	  let g_cexpr1 = df_analysis_of_cexpr_internal dfa_ctxt cexpr1 in
	  let g_cexpr2 = df_analysis_of_cexpr_internal dfa_ctxt cexpr2 in
	  let g_cexpr3 = df_analysis_of_cexpr_internal dfa_ctxt cexpr3 in
	  let g_con = terminate_unary n_con g_cexpr1 in
	  let g_ord1 = terminate_unary n_ord1 g_cexpr2 in
	  let g_ord2 = terminate_unary n_ord2 g_cexpr3 in
	  let g_merge = terminate_binary n_merge g_ord1 g_ord2 in
	    
	    (* Make sure to not lose graph components due to independence between
	       if and then / else clauses. *)
	    merge_dfgraphs g_merge g_con
	      
	      
      (**************************)
      (* forking core ast nodes *)
      (**************************)
	      
      | CESome (_, cvname, cexpr1, cexpr2)
      | CEEvery (_, cvname, cexpr1, cexpr2) ->
	  let n_fork = mkdfnode_dffork default_ac_handle in
	  let n_merge = mkdfnode_dfpass DFMerge default_ac_handle in
	  let _ = affiliate_binary n_fork n_merge in
	    
	  let g_fork = df_analysis_of_cexpr_internal dfa_ctxt cexpr1 in
	  let g_fork' = terminate_unary n_fork g_fork in
	    
	  let dfa_ctxt' = add_var_dfgraph dfa_ctxt cvname g_fork' in
	    
	  let g_merge = df_analysis_of_cexpr_internal dfa_ctxt' cexpr2 in
	  let g_merge' = terminate_unary n_merge g_merge in
	    
	    (* Make sure to not lose graph components due to independence between
	       some / every and satisfies clauses. *)
	    merge_dfgraphs g_merge' g_fork'
	      
      | CETypeswitch (cexpr1, tsclauses) ->
	  let n_fork = mkdfnode_dffork default_ac_handle in
	  let n_con = mkdfnode_dfsink DFControl default_ac_handle in
	  let n_merge = mkdfnode_dfpass DFMerge default_ac_handle in
	    
	  let _ = mkdfgraph_singleton n_con in
	  let g_fork = df_analysis_of_cexpr_internal dfa_ctxt cexpr1 in
	  let g_fork' = terminate_unary n_fork g_fork in
	  let g_con' = terminate_unary n_con g_fork' in
	    
	  let (_, opt_cvnames, cexprs) = Gmisc.triple_split tsclauses in
	    
	  (* Returns the newly created nodes for use in affiliation. *)
	  let add_opt_cv =
	    (fun opt_cvname ->
	       match opt_cvname with
		 | None -> (None, None, dfa_ctxt)
		 | Some cvname ->
		     let ac_handle = (Some cexpr, Some (ACHTsclause cvname)) in
		     let n_merge = mkdfnode_dfpass DFMerge ac_handle in
		     let n_fork' = mkdfnode_dffork ac_handle in
		     let g_fork'' = terminate_unary n_fork' g_fork' in
		       (Some n_fork', Some n_merge, add_var_dfgraph dfa_ctxt cvname g_fork''))
	  in

	  let rec flatten_options lst =
	    match lst with
	      | [] -> []
	      | None :: rest -> flatten_options rest
	      | Some x :: rest -> x :: flatten_options rest
	  in

	  let terminate_unary_optional opt_n g =
	    match opt_n with
	      | None -> g
	      | Some n -> terminate_unary n g
	  in
	    
	  let n_fork_ctxts = List.map add_opt_cv opt_cvnames in
	  let (n_forks, n_merges, dfa_ctxts) = Gmisc.triple_split n_fork_ctxts in
	  let n_forks' = flatten_options n_forks in
	  let n_merges' = flatten_options n_merges in
	  let _ = affiliate_many ([n_fork; n_con; n_merge] @ n_forks' @ n_merges') in
	    
	  let g_forks = List.map2 df_analysis_of_cexpr_internal dfa_ctxts cexprs in
	  let g_forks' = List.map2 terminate_unary_optional n_merges g_forks in
	  let g_forks'' = terminate_many n_merge g_forks' in
	    
	    (* Make sure to not lose graph components due to independence between
	       typeswitch and case clauses. *)
	    merge_dfgraphs g_forks'' g_con'
	      
      | CEFLWOR (cfl_exprs, cexpr_where, cexpr_orderby, cexpr1) ->
	  let (g_flwor, n_flwors, dfa_ctxt') = df_analysis_of_cexpr_flwor dfa_ctxt cfl_exprs cexpr1 cexpr in
	  let (g_where, n_wheres) = where_df_analysis dfa_ctxt' cexpr_where cexpr in
	  let (g_ord, n_ords) = orderby_df_analysis dfa_ctxt' cexpr_orderby cexpr in
	  let _ = affiliate_many (n_flwors @ n_wheres @ n_ords) in
	    
	  (* Make sure to not lose graph components due to independence between
	     flwor clauses. *)
	  let g_flwor' = merge_dfgraphs g_flwor g_where in
	    merge_dfgraphs g_flwor' g_ord
	      
      | _ -> raise (Query (Streaming_XPath "Cannot handle that kind of core expression."))
	  
(* Returns the nodes newly created by the innermost for-loop for use in affiliation. *)		  
and df_analysis_of_cexpr_flwor dfa_ctxt fl_clauses ret_cexpr cexpr =
  match fl_clauses with
    | [] ->
	let g_merge = df_analysis_of_cexpr_internal dfa_ctxt ret_cexpr in
	  (g_merge, [], dfa_ctxt)
	    
    | fl_cexpr :: rest ->
	begin
	  match fl_cexpr with
	    | CELET (_, cvname, cexpr1) ->
		begin
		  let ac_handle = (Some cexpr, Some (ACHLet cvname)) in
		  let n_fork = mkdfnode_dffork ac_handle in
		  let n_merge = mkdfnode_dfpass DFMerge ac_handle in
		  let _ = affiliate_binary n_fork n_merge in
		    
		  let g_fork = df_analysis_of_cexpr_internal dfa_ctxt cexpr1 in
		  let g_fork' = terminate_unary n_fork g_fork in
		    
		  let dfa_ctxt' = add_var_dfgraph dfa_ctxt cvname g_fork' in
		    
		  let (g_rest, n_flwors, dfa_ctxt'') = df_analysis_of_cexpr_flwor dfa_ctxt' rest ret_cexpr cexpr in
		  let g_rest' = terminate_unary n_merge g_rest in
		    (merge_dfgraphs g_rest' g_fork', n_flwors, dfa_ctxt'')
		end
		  
	    | CEFOR (_, cvname, opt_pos_cvname, cexpr1) ->
		begin
		  let ac_handle = (Some cexpr, Some (ACHFor cvname)) in
		  let n_fork = mkdfnode_dffork ac_handle in
		  let n_merge = mkdfnode_dfpass DFMerge ac_handle in
		  let _ = affiliate_binary n_fork n_merge in
		    
		  let g_fork = df_analysis_of_cexpr_internal dfa_ctxt cexpr1 in
		  let g_fork' = terminate_unary n_fork g_fork in
		    
		  let dfa_ctxt' = add_var_dfgraph dfa_ctxt cvname g_fork' in

		  (* Take into account the optional positional variable. *)
		  let dfa_ctxt'' =
		    match opt_pos_cvname with
		      | None -> dfa_ctxt'
		      | Some pos_cvname ->
			  let n_imm = mkdfnode_dfpass DFImmediate ac_handle in
			  let g_imm = mkdfgraph_singleton n_imm in
			  let _ = affiliate_many [n_fork; n_merge; n_imm] in
			    add_var_dfgraph dfa_ctxt' pos_cvname g_imm
		  in
		    
		  let (g_rest, n_flwors, dfa_ctxt''') = df_analysis_of_cexpr_flwor dfa_ctxt'' rest ret_cexpr cexpr in
		  let g_rest' = terminate_unary n_merge g_rest in
		  let n_flwors' =
		    match rest with
		      | [] -> [n_fork; n_merge]
		      | _ -> n_flwors
		  in
		    (merge_dfgraphs g_rest' g_fork', n_flwors', dfa_ctxt''')
		end
	end

and where_df_analysis dfa_ctxt cexpr_where cexpr =
  match cexpr_where with
    | None -> (mkdfgraph_empty (), [])
    | Some cexpr1 ->
	let ac_handle = (Some cexpr, Some ACHWhere) in
	let n_con = mkdfnode_dfsink DFControl ac_handle in
	let g_con = df_analysis_of_cexpr_internal dfa_ctxt cexpr1 in
	  (terminate_unary n_con g_con, [n_con])

and orderby_df_analysis dfa_ctxt corder_by cexpr =
  match corder_by with
    | None -> (mkdfgraph_empty (), [])
    | Some (_, order_specs, osig) ->
	let ac_handle = (Some cexpr, Some ACHOrderby) in
	let mk_n_con = fun cexpr1 -> mkdfnode_dfsink DFControl ac_handle in
	let (cexprs,_ ,_ ) = Gmisc.triple_split order_specs in
	let n_cons = List.map mk_n_con cexprs in
	let g_cons = List.map (df_analysis_of_cexpr_internal dfa_ctxt) cexprs in
	let g_cons' = List.map2 terminate_unary n_cons g_cons in
	  (List.fold_left merge_dfgraphs (mkdfgraph_empty ()) g_cons', n_cons)

let df_analysis_of_cexpr cexpr =
  let dfa_ctxt = build_df_analysis_context () in

  (* The serialize node is associated the toplevel node in the ast. *)
  let ac_handle = (Some cexpr, None) in
  let n_ser = mkdfnode_dfsink DFSerialize ac_handle in
  let _ = affiliate_unary n_ser in

  let g = df_analysis_of_cexpr_internal dfa_ctxt cexpr in
    terminate_unary n_ser g

let df_analysis_of_statements statements dfa_ctxt =
  let ac_handle = (None, None) in
  let cexprs = statements in

  let i = ref 0 in
    
  let mk_int x = i := !i + 1; !i in
  let mk_n_ord i = mkdfnode_dfpass (DFOrdered i) ac_handle in
    
  let ints = List.map mk_int cexprs in
  let n_ords = List.map mk_n_ord ints in
  let n_merge = mkdfnode_dfpass DFMerge ac_handle in
  let _ = affiliate_many (n_merge :: n_ords) in
    
  let g_stats = List.map (df_analysis_of_cexpr_internal dfa_ctxt) statements in
  let g_ords = List.map2 terminate_unary n_ords g_stats in
    terminate_many n_merge g_ords

(* Structurally very similar to CELET construct; consider refactoring! *)
let rec df_analysis_of_var_decls acvar_decls statements dfa_ctxt =
  match acvar_decls with
    | [] -> (df_analysis_of_statements statements dfa_ctxt, dfa_ctxt)
    | var_decl :: rest ->
	let (cvname, _, opt_cexpr) = var_decl.pcvar_decl_desc in
	  begin
	    match opt_cexpr with
	      | CEVarExternal
	      |	CEVarImported
	      |	CEVarInterface -> (mkdfgraph_empty (), dfa_ctxt)
	      | CEVarUser cexpr ->
		  let ac_handle = (None, Some (ACHVar cvname)) in
		  let n_fork = mkdfnode_dffork ac_handle in
		  let n_merge = mkdfnode_dfpass DFMerge ac_handle in
		  let _ = affiliate_binary n_fork n_merge in
		    
		  let g_fork = df_analysis_of_cexpr_internal dfa_ctxt cexpr in
		  let g_fork' = terminate_unary n_fork g_fork in
		    
		  let dfa_ctxt' = add_var_dfgraph dfa_ctxt cvname g_fork' in
		    
		  let (g_rest, dfa_ctxt'') = df_analysis_of_var_decls rest statements dfa_ctxt' in
		  let g_rest' = terminate_unary n_merge g_rest in
		    (merge_dfgraphs g_rest' g_fork', dfa_ctxt'')
	  end
		    
let df_analysis_of_prolog_statements prolog statements dfa_ctxt =
  df_analysis_of_var_decls prolog.pcprolog_vars statements dfa_ctxt


(***********)
(* exposed *)
(***********)

let df_analysis_of_xmodule cxmodule =
  let prolog = cxmodule.pcmodule_prolog in
  let statements = cxmodule.pcmodule_statements in

  let dfa_ctxt = build_df_analysis_context () in
  let (dfgraph, _) = df_analysis_of_prolog_statements prolog statements dfa_ctxt in

  let ac_handle = (None, None) in
  let n_ser = mkdfnode_dfsink DFSerialize ac_handle in
  let _ = affiliate_unary n_ser in
    terminate_unary n_ser dfgraph


(******************************************)
(* XQuery core data flow graph dot output *)
(******************************************)

let string_of_atomic_value av =
  try ("\\\"" ^ av#getAtomicString ()  ^ "\\\"") with
      (Query (Datamodel _)) ->
	try Decimal._string_of_integer (av#getAtomicInteger()) with
	    (Query (Datamodel _)) -> (string_of_float (av#getAtomicDouble ()))

let string_of_cnode_test cnode_test =
  match cnode_test with
  | CPNameTest rqname -> Namespace_names.prefixed_string_of_rqname rqname
  | CPNodeKindTest (cnode_kind, cnode_type) -> Print_top.bprintf_cnode_kind "" cnode_kind

let string_of_axis axis = Print_common.string_of_axis axis

let print_dot_nodelabel ff dfnode_kind dfnode_id ac_handle  =
  let (opt_cexpr, opt_ac_hint) = ac_handle in
  let dot_nodename = string_of_dfnode_id dfnode_id in

  let small_filled_box_str = "box, color=white, style=filled, fillcolor=grey, fontsize=10, height=0.25" in
  let small_point_str = "point, fixedsize=true, height=0.1, width=0.1" in
  let plaintext_str = "plaintext" in
  let small_plaintext_str = "plaintext, fontsize=10, height=0.25" in
  let diamond_str = "diamond" in
  let double_diamond_str ="diamond, peripheries=2" in
  let ellipse_str = "ellipse" in
  let small_box_str = "box, fontsize=10, height=0.25" in
  let trapezium_str = "trapezium" in
  let invtrapezium_str = "invtrapezium" in

  let dot_nodelabel, dot_nodeattribs = 
    match dfnode_kind with
      | DFSink DFSerialize -> "ser", double_diamond_str

      | DFSink DFControl ->
	  begin
	    match opt_cexpr with
	      | Some cexpr ->
		  begin
		    match cexpr.pcexpr_desc with
		      | CEIf _ -> "if", diamond_str
		      | CEFLWOR _ ->
			  begin
			    match opt_ac_hint with
			      | Some ACHWhere -> "where", diamond_str
			      | Some ACHOrderby -> "sort", diamond_str
			      |	_  -> raise (Dferror "Unexpected ac_hint on DFControl.")
			  end
		      | CETypeswitch _ -> "ts", diamond_str
		      | _ -> raise (Dferror "Encountered DFControl in unexpected context.")
		  end
	      | None -> raise (Dferror "Encountered DFControl in unexpected context.")
	  end

      | DFPass DFMerge ->
	  begin
	    match opt_cexpr with
	      | Some cexpr ->
		  begin
		    match cexpr.pcexpr_desc with
		      | CECall (cfname, _, _, _, _) -> (Namespace_names.prefixed_string_of_rqname cfname), small_filled_box_str
		      | CEForwardAxis _
		      | CEReverseAxis _ -> "", small_point_str
		      | CEDocument _ -> "doc {...}", plaintext_str
		      | CETextComputed _ -> "\\\"...\\\"", plaintext_str
		      | CECommentComputed _ -> "<!-- ... -->", plaintext_str
		      | CEPIComputed _ -> "<?...?>", plaintext_str
		      | CEAttr (caname, _) -> "@" ^ (Namespace_names.prefixed_string_of_rqname caname), plaintext_str
		      | CEElem (cename, _, _) -> "<" ^ (Namespace_names.prefixed_string_of_rqname cename) ^ ">", plaintext_str
		      | CEAnyElem _ -> "<.../>", plaintext_str
		      | CEAnyAttr _ -> "@...", plaintext_str
		      | CEIf _ -> "cond", small_box_str
		      | CETypeswitch _ ->
			  begin
			    match opt_ac_hint with
			      | Some (ACHTsclause cvname) -> "unbind" ^ " $" ^ (Namespace_names.prefixed_string_of_rqname cvname), invtrapezium_str
			      | _ -> "switch", small_box_str
			  end
		      | _ ->
			  begin
			    match opt_ac_hint with
			      | Some (ACHFor cvname) -> "iter" ^ " $" ^ (Namespace_names.prefixed_string_of_rqname cvname), invtrapezium_str
			      | Some (ACHLet cvname) -> "unbind" ^ " $" ^ (Namespace_names.prefixed_string_of_rqname cvname), invtrapezium_str
			      | _ -> "merge", ellipse_str
			  end
		  end
	      | None ->
		  begin
		    match opt_ac_hint with
		      | Some (ACHVar cvname) -> "unbind" ^ " $" ^ (Namespace_names.prefixed_string_of_rqname cvname), invtrapezium_str
		      | _ -> "stmts", plaintext_str(*raise (Dferror "Encountered unexpected ac_hint on DFMerge.")*)
		  end
	  end

      | DFFork ->
	  begin
	    match opt_cexpr with
	      | Some cexpr ->
		  begin
		    match opt_ac_hint with
		      | Some (ACHFor cvname) -> "slice" ^ " $" ^ (Namespace_names.prefixed_string_of_rqname cvname), trapezium_str
		      | Some (ACHLet cvname) -> "bind" ^ " $" ^ (Namespace_names.prefixed_string_of_rqname cvname), trapezium_str
		      | Some (ACHTsclause cvname) -> "bind" ^ " $" ^ (Namespace_names.prefixed_string_of_rqname cvname), trapezium_str
		      | _ -> "fork", ellipse_str
		  end
	      | None -> 
		  begin
		    match opt_ac_hint with
		      | Some (ACHVar cvname) -> "bind" ^ " $" ^ (Namespace_names.prefixed_string_of_rqname cvname), trapezium_str
		      | _ -> raise (Dferror "Encountered unexpected ac_hint on DFControl.")
		  end
	  end

      | DFPass (DFOrdered i) ->
	  begin
	    match opt_cexpr with
	      | Some cexpr ->
		  begin
		    match cexpr.pcexpr_desc with
		      | CEText _
		      | CEComment _
		      | CEPI _
		      | CEPIComputed _
		      | CEAttr _
		      | CEElem _
		      | CEAnyElem _
		      | CEAnyAttr _ -> (string_of_int i), small_plaintext_str
		      | CECall _ ->  "arg" ^ (string_of_int i), small_plaintext_str
		      | CEIf _ ->
			  if i = 1 then "then", small_box_str
			  else "else", small_box_str
		      | _ -> "ord" ^ (string_of_int i), ellipse_str
		  end
	      | None -> (string_of_int i), small_plaintext_str
	  end

      | DFPass DFUnordered ->
	  begin
	    match opt_cexpr with
	      | Some cexpr ->
		  begin
		    match cexpr.pcexpr_desc with
		      | CEForwardAxis (v,axis, cnode_test)
		      | CEReverseAxis (v,axis, cnode_test) -> "(" ^ (string_of_axis axis) ^ "::" ^ (string_of_cnode_test cnode_test) ^ "," ^ (Namespace_names.prefixed_string_of_rqname v) ^ ")", plaintext_str
		      | CEDocument _
		      | CETextComputed _
		      | CECommentComputed _ -> "0", small_plaintext_str
		      | _ -> "unord", ellipse_str
		  end
	      | None -> raise (Dferror "Encountered DFUnordered in unexpected context.")
	  end

      | DFPass DFImmediate ->
	  begin
	    match opt_cexpr with
	      | Some cexpr ->
		  begin
		    match cexpr.pcexpr_desc with
		      | CEEmpty -> "()", plaintext_str
		      | CEScalar av -> string_of_literal av, plaintext_str
		      | CEText ut -> "\\\"" ^ ut ^ "\\\"", plaintext_str
		      | CEComment str -> "<!-- " ^ str ^ " -->", plaintext_str
		      | CEPI (str1, str2) -> "<?" ^ str1 ^ ", " ^ str2 ^ "?>", plaintext_str
		      | _ ->
			  begin
			    match opt_ac_hint with
			      | Some ACHFor _ -> "pos", plaintext_str
			      | _ -> raise (Dferror "Encountered DFImmediate in unexpexted context.")
			  end
		  end
	      | None -> raise (Dferror "Encountered DFImmediate in unexpexted context.")
	  end
  in
    Format.fprintf ff "%s" (dot_nodename ^ " [label=\"" ^ dot_nodelabel ^ "\", shape=" ^ dot_nodeattribs ^ "];\n")


(***********)
(* Exposed *)
(***********)

let print_dot_dfgraph ff dfgraph =
  print_dot_dfgraph_custom ff print_dot_nodelabel dfgraph
