(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: compile_expr.ml,v 1.98 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Compile_expr
   Description:
     This module compiles an XQuery core expression into the ALGEBRA.
*)

open Error

open Namespace_builtin

open Xquery_common_ast

open Xquery_core_ast
open Xquery_core_ast_util

open Xquery_algebra_ast
open Xquery_algebra_ast_util

open Logical_algebra_types

open Compile_context
open Compile_util

open Processing_context


let decode_proto_value cexpr_list =
  match cexpr_list with
  | [cexpr1;cexpr2] ->
      begin
	match cexpr2.pcexpr_desc with
	| CEProtoValue atomic_value -> (cexpr1,atomic_value)
	| CEScalar (IntegerLiteral _ ) -> (cexpr1,Datatypes.ATInteger)
	| CEScalar (DecimalLiteral _ ) -> (cexpr1,Datatypes.ATDecimal)
	| CEScalar (DoubleLiteral _ ) -> (cexpr1,Datatypes.ATDouble)
	| CEScalar (StringLiteral _ ) -> (cexpr1,Datatypes.ATString)
	| CEScalar (BooleanLiteral _ ) -> (cexpr1,Datatypes.ATBoolean)
	| CEScalar (URILiteral _ ) -> (cexpr1,Datatypes.ATAnyURI)
	| _ ->
	    raise (Query (Error ("Could not find proto value during compilation "^(Print_top.bprintf_acexpr "" cexpr2))))
      end
  | _ ->
      raise (Query (Error "Could not find proto value during compilation"))

let decode_noproto_value cexpr_list =
  match cexpr_list with
  | [cexpr1] -> cexpr1
  | _ ->
      raise (Query (Error "Could not find proto value during compilation"))

let get_fresh_var_name comp_ctxt =
  Compile_context.get_new_variable_name comp_ctxt "comp"

type 'a compile_select = 
  | Cs_Disjunction     of 'a compile_select * 'a compile_select
  | Cs_ComplexConjunct of 'a compile_select * 'a compile_select
  | Cs_SimpleConjunct  of logical_algop_expr list

let is_fn_true op =
  match op.pcexpr_desc with
    | CECall (x, [], _, _, _) when Namespace_names.rqname_equal x fn_true ->
	true
    | _ -> false

let is_fn_false op =  
  match op.pcexpr_desc with
    | CECall (x, [], _, _, _) when Namespace_names.rqname_equal x fn_false ->
	true
    | _ -> false

let is_ce_empty op =  
  match op.pcexpr_desc with
    | CEEmpty -> true
    | _ -> false

let is_fn_boolean_cexpr op =
  match op.pcexpr_desc with
    | CECall (x, _, _, _, _ ) when Namespace_names.rqname_equal x fn_boolean ->
	true
    | _ -> false


(*********************************************************)
(* Helper functions for tree join recognition. - Michael *)
(*********************************************************)

(* Indicates wether the semantics emposed by the expression
   is covered by the semantics of a tree join at its output. *)
let is_tree_join_input_redundant cexpr =
  match cexpr.pcexpr_desc with
    | CECall (cfname, _, _, _, _) when cfname = fs_node_sequence -> true
    | _ -> false

(* Indicates wether the semantics imposed by the expression
   is covered by the semantics of a tree join at its input. *)
let is_tree_join_output_redundant cexpr =
  match cexpr.pcexpr_desc with
    | CECall (cfname, _, _, _, _) ->
	if cfname = fs_distinct_docorder
	  || cfname = fs_distinct_docorder_or_atomic_sequence
	  || cfname = fs_distinct
	  || cfname = fs_distinct_or_atomic_sequence
	  || cfname = fs_docorder
	  || cfname = fs_docorder_or_atomic_sequence
	  || cfname = fs_node_sequence
	  || cfname = fs_node_sequence_or_atomic_sequence
	then true
	else false
    | _ -> false
	
let is_fl_clauses_well_behaved fl_clauses =
  match fl_clauses with
    | fl_clause :: [] ->
	begin
	  match fl_clause with
	    | CEFOR (None, _, None, _) -> true
	    | _ -> false
	end
    | _ -> false

let is_where_clause_well_behaved where_clause =
  match where_clause with
    | None -> true
    | Some _ -> false

let is_order_by_clause_well_behaved order_by_clause =
  match order_by_clause with
    | None -> true
    | Some _ -> false

(* Actual pattern matching for identifying tree joins. *)
let axis_nt_for_tree_join fl_clauses where_clause order_by_clause return_clause =
  let raise_tree_join_match_error () = raise (Query (Compilation "Tree join match error.")) in

    (*****************************************************)
    (* The pattern is: for $var in expr return axis::nt. *)
    (*****************************************************)
    match return_clause.pcexpr_desc with
      | CEForwardAxis (v,axis, cnode_test)
      | CEReverseAxis (v,axis, cnode_test) ->

	  (* The following makes sure the other flwor clauses are structured as expected. *)
	  if is_fl_clauses_well_behaved fl_clauses
	    && is_where_clause_well_behaved where_clause
	    && is_order_by_clause_well_behaved order_by_clause
	  then
	    Some (v, axis, cnode_test)
	  else raise_tree_join_match_error ()
		    
      | _ -> None
	  
(* Helps assessing wether a sbdo-related function call can be removed.
   That is the case whenever its argument qualifies for being compiled
   into a tree join. *)
let qualifies_for_tree_join cexpr =
  match cexpr.pcexpr_desc with
    | CEFLWOR (fl_clauses, where_clause, order_by_clause, return_clause) ->
	begin
	  match axis_nt_for_tree_join fl_clauses where_clause order_by_clause return_clause with
	  | Some _ -> true
	  | None -> false
	end
    | _ -> false

let access_arg1 cexpr =
  match cexpr.pcexpr_desc with
    | CECall (_, arg1 :: [], _, _, _) -> arg1
    | _ -> raise (Query (Compilation "Was expecting exactly one function argument."))

let access_for_clause fl_clauses =
  match fl_clauses with
    | CEFOR (None, _, None, cexpr) :: [] -> cexpr
    | _ -> raise (Query (Compilation "Was expecting exactly one for-clause."))

(************************************************************)
(* Helper functions for parse stream recognition. - Michael *)
(************************************************************)

let access_static_document_uri mod_proc_ctxt cexpr =
  let raise_fn_doc_match_error () =
    raise (Query (Compilation "fn:doc match error.")) in
  let access_arg1 cexpr cfname =
    match cexpr.pcexpr_desc with
    | CECall(cfn, cexprs, _, _, _) when (Namespace_names.rqname_equal cfname cfn) ->
	List.hd cexprs
    | CECall(cfn, cexprs, _, _, _)  -> raise_fn_doc_match_error ()
    | _ -> raise_fn_doc_match_error ()
  in
  let fn_doc_arg1 = access_arg1 cexpr fn_doc in
    (* When evaluating normalized exprs, it may be that the fn_data,
       fs_convert_simple_operand and fs_promote_to_anystring are 
       not there. The following deals with this *)
  let data_arg = 
    match fn_doc_arg1.pcexpr_desc with
    | CEScalar _ -> fn_doc_arg1
    | _ ->
        let fs_promote_arg1 = access_arg1 fn_doc_arg1 fs_promote_to_anystring in
        match  fs_promote_arg1.pcexpr_desc with
        | CEScalar _ -> fs_promote_arg1
        | _ -> 
            let fs_cso_arg1 = access_arg1 fs_promote_arg1 fs_convert_simple_operand in 
            match fs_cso_arg1.pcexpr_desc with
            | CEScalar _ -> fs_cso_arg1
            | _ ->  access_arg1 fs_cso_arg1 fn_data
  in
  match data_arg.pcexpr_desc with
  | CEScalar av ->
      begin
	try
	  let uri_string = string_of_literal av in
	  let base_uri = Processing_context.get_base_uri mod_proc_ctxt in
	  match base_uri with
	  | None -> uri_string
	  | Some base_uri ->
	      let uri = AnyURI._kinda_uri_of_string uri_string in
	      let absolute_uri = AnyURI._uri_resolve base_uri uri in
	      let absolute_uri_string = AnyURI._string_of_uri absolute_uri in
	      absolute_uri_string
	with
	| Query (Datamodel _) -> raise_fn_doc_match_error ()
      end
  | _ -> raise_fn_doc_match_error ()
          
let is_static_fn_doc_cexpr mod_proc_ctxt cexpr =
  try
    ignore(access_static_document_uri mod_proc_ctxt cexpr);
    true
  with
  | (Query (Compilation "fn:doc match error.")) -> false


(***************)
(* Expressions *)
(***************)

(*******************************************
  Compile_cexpr:
  Goal: Compile core XQuery expressions into the algebraic representation
      
  Template for each portion of the match statement:
  - Compile parameter expressions into the Algebra
  - Construct the operation name (keeping non-algebraic information for rewrites)
  - Build caml code capabable of implementing the functionality
       - Coerce this into algop_eval_code_dep 
  - Construct the return structure (algop_mkop ... )
*)

let rec compile_cexpr compile_ctxt cexpr =

  (* The Core expression contains the file location that
     should be correlated with errors, so we catch any exceptions here
     and rewrap the exceptions with the file location. *) 

  let cexpr_desc = cexpr.pcexpr_desc in
  let eh         = cexpr.pcexpr_origin in
  let fi         = cexpr.pcexpr_loc in
    try
      match cexpr_desc with
      | CEUnordered cexpr1 ->
	  (* Currently we ignore unordered expressions *)
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in ao1
      | CEOrdered cexpr1 ->
	  (* Currently we ignore ordered expressions *)
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in ao1
      | CEFLWOR (fl_clauses,where_clause,order_by_clause,return_clause) ->
	  compile_flwor compile_ctxt fl_clauses 
	    (where_clause,order_by_clause,return_clause) eh fi
      | CEIf(cexpr1,cexpr2,cexpr3) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let ao2 = compile_cexpr compile_ctxt cexpr2 in
	  let ao3 = compile_cexpr compile_ctxt cexpr3 in
	  let indep = OneSub ao1 in
	  let dep = TwoSub (ao2,ao3) in
	  let op_name = AOEIf in
	  logical_aalgop_mkop op_name indep dep None eh fi 
      | CEWhile(cexpr1,cexpr2) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let ao2 = compile_cexpr compile_ctxt cexpr2 in
	  let indep = NoSub in
	  let dep = TwoSub (ao1,ao2) in
	  let op_name = AOEWhile in
	  logical_aalgop_mkop op_name indep dep None eh fi 
      | CETypeswitch(cexpr0,branch_list) ->
	  let ao0 = compile_cexpr compile_ctxt cexpr0 in
	  let pattern, dep_exprs = 
	    List.split (List.map (compile_typeswitch_branch compile_ctxt) branch_list) in
	  let indep = OneSub ao0 in
	  let dep = ManySub (Array.of_list dep_exprs) in
	  let op_name = AOETypeswitch (Array.of_list pattern) in
	  logical_aalgop_mkop op_name indep dep None eh fi 
      | CEVar vname1 ->
	  begin
	    match get_variable_field_from_compile_context compile_ctxt vname1 with
	      (* If the variable is not bound to a field access, then this
		 is a standard XQuery variable or an imported variable. 
	       *)
	    | None ->
		let indep = NoSub in
		let dep = NoSub in
		let op_name = AOEVar vname1 in
		logical_aalgop_mkop op_name indep dep None eh fi 
		  (* Otherwise, we just need to access the proper field from the
		     input tuple. *)
	    | Some crname ->
		let indep = NoSub in
		let dep = NoSub in
		let op_name = AOEAccessTuple(crname) in
		logical_aalgop_mkop op_name indep dep None eh fi 
	  end
      | CECall (cfname,cexpr_list,(optintypes,outtype), upd, selfrecur) ->
	  let mod_proc_ctxt = 
	    Norm_context.module_context_from_norm_context (norm_context_from_compile_context compile_ctxt)  in
	  if (is_static_fn_doc_cexpr mod_proc_ctxt cexpr)
	  then
	    (* In the case of a fn:doc call with a static URL, compile
	       to dedicated Parse operator. *)
	    let indep = NoSub in
	    let dep = NoSub in
	    let document_uri = access_static_document_uri mod_proc_ctxt cexpr in
	    let op_name = AOEParse(document_uri) in
	    logical_aalgop_mkop op_name indep dep None eh fi
	  else if (Namespace_names.rqname_equal cfname Namespace_builtin.fs_promote_to_numeric)
	  then
	    (* In the case of protofunctions (fs:convert-simple-operand...),
	       compile to dedicated algebraic operators. *)
	    let (cexpr1,atomic_type) = decode_proto_value cexpr_list in
	    let ao1 = compile_cexpr compile_ctxt cexpr1 in
	    let indep = OneSub ao1 in
	    let dep = NoSub in
	    let op_name = AOEPromoteNumeric atomic_type in
	    logical_aalgop_mkop op_name indep dep None eh fi
	  else if (Namespace_names.rqname_equal cfname Namespace_builtin.fs_promote_to_anystring)
	  then
	    (* In the case of protofunctions (fs:convert-simple-operand...),
	       compile to dedicated algebraic operators. *)
	    let cexpr1 = decode_noproto_value cexpr_list in
	    let ao1 = compile_cexpr compile_ctxt cexpr1 in
	    let indep = OneSub ao1 in
	    let dep = NoSub in
	    let op_name = AOEPromoteAnyString in
	    logical_aalgop_mkop op_name indep dep None eh fi
	  else if (Namespace_names.rqname_equal cfname Namespace_builtin.fs_unsafe_promote_to_numeric)
	  then
	    let (cexpr1,atomic_type) = decode_proto_value cexpr_list in
	    let ao1 = compile_cexpr compile_ctxt cexpr1 in
	    let indep = OneSub ao1 in
	    let dep = NoSub in
	    let op_name = AOEUnsafePromoteNumeric atomic_type in
	    logical_aalgop_mkop op_name indep dep None eh fi
	  else if (Namespace_names.rqname_equal cfname Namespace_builtin.fs_convert_simple_operand)
	  then
	    let (cexpr1,atomic_type) = decode_proto_value cexpr_list in
	    let ao1 = compile_cexpr compile_ctxt cexpr1 in
	    let indep = OneSub ao1 in
	    let dep = NoSub in
	    let op_name = AOEConvertSimple atomic_type in
	    logical_aalgop_mkop op_name indep dep None eh fi
	  else
	    (* Otherwise, compile the function call as usual. *)
	    begin
	      let ao_args = List.map (compile_cexpr compile_ctxt) cexpr_list in
	      let arity = List.length cexpr_list in 
	      let indep = ManySub (Array.of_list ao_args) in
	      let dep = NoSub in
	      let op_name = 
		if is_builtin (cfname,arity) then
		  AOECallBuiltIn (
		  (cfname,arity), 
		  (Array.of_list (List.map (compile_opt_ctype compile_ctxt) optintypes)), 
		  compile_ctype compile_ctxt outtype,
		  upd) 
		else
		  AOECallUserDefined (
		  (cfname,arity), 
		  (Array.of_list (List.map (compile_opt_ctype compile_ctxt) optintypes)), 
		  compile_ctype compile_ctxt outtype,
		  upd, selfrecur)
	      in
	      logical_aalgop_mkop op_name indep dep None eh fi
	    end
      | CEOverloadedCall (cfname,cexpr_list, osig) ->
	  let ao_args = List.map (compile_cexpr compile_ctxt) cexpr_list in
	  let arity = List.length cexpr_list in 
	  let indep = ManySub (Array.of_list ao_args) in
	  let dep = NoSub in
	  let asig = compile_overloaded_table_sigs compile_ctxt osig in
	  let op_name = AOECallOverloaded ((cfname,arity),asig) in
	  logical_aalgop_mkop op_name indep dep None eh fi 
      | CEScalar dmv ->
	  let indep = NoSub in
	  let dep = NoSub in
	  let op_name = AOEScalar dmv in
	  logical_aalgop_mkop op_name indep dep None eh fi 
      | CEProtoValue pv ->
	  raise (Query (Internal_Error "Prototypical value occuring outside of proper fs:functions!"))
      | CESeq (cexpr1,cexpr2) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let ao2 = compile_cexpr compile_ctxt cexpr2 in
	  let indep = TwoSub (ao1, ao2) in
	  let dep = NoSub in
	  let op_name = AOESeq in
	  logical_aalgop_mkop op_name indep dep None eh fi    
      | CEEmpty ->
	  let indep = NoSub in
	  let dep = NoSub in
	  let op_name = AOEEmpty in
	  logical_aalgop_mkop op_name indep dep None eh fi    
      | CEDocument cexpr1 ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let indep = OneSub ao1 in
	  let dep = NoSub in
	  let op_name = AOEDocument in
	  logical_aalgop_mkop op_name indep dep None eh fi  
      | CEPI (ncname,str) ->
	  let indep = NoSub in
	  let dep = NoSub in
	  let op_name = AOEPI (ncname,str) in
	  logical_aalgop_mkop op_name indep dep None eh fi 
      | CEPIComputed (cexpr1, cexpr2) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let ao2 = compile_cexpr compile_ctxt cexpr2 in
	  let indep = TwoSub (ao1, ao2) in
	  let dep = NoSub in
	  let op_name = AOEPIComputed in
	  logical_aalgop_mkop op_name indep dep None eh fi    
      | CEComment str ->
	  let indep = NoSub in
	  let dep = NoSub in
	  let op_name = AOEComment str in
	  logical_aalgop_mkop op_name indep dep None eh fi 
      | CECommentComputed cexpr1 ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let indep = OneSub ao1 in
	  let dep = NoSub in
	  let op_name = AOECommentComputed in
	  logical_aalgop_mkop op_name indep dep None eh fi 
      | CEText text ->
	  let indep = NoSub in
	  let dep = NoSub in
	  let op_name = AOEText text in
	  logical_aalgop_mkop op_name indep dep None eh fi 
      | CECharRef i ->
	  let indep = NoSub in
	  let dep = NoSub in
	  let op_name = AOECharRef i in
	  logical_aalgop_mkop op_name indep dep None eh fi 
      | CETextComputed cexpr1 ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let indep = OneSub(ao1) in
	  let dep = NoSub in
	  let op_name = AOETextComputed in
	  logical_aalgop_mkop op_name indep dep None eh fi 
      | CEElem (cename,nsenv,cexpr_list) ->
	  let ao_list = List.map (compile_cexpr compile_ctxt) cexpr_list in
	  let indep = ManySub (Array.of_list ao_list) in
	  let dep = NoSub in
	  let relem_sym = Namespace_symbols.relem_symbol cename in
	  let op_name = AOEElem (relem_sym,nsenv) in
	  logical_aalgop_mkop op_name indep dep None eh fi    
      | CEAnyElem (cexpr1,nsenv1, nsenv2, cexpr2) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let ao2 = compile_cexpr compile_ctxt cexpr2 in
	  let indep = TwoSub(ao1,ao2) in
	  let dep = NoSub in
	  let op_name = AOEAnyElem (nsenv1, nsenv2) in
	  logical_aalgop_mkop op_name indep dep None eh fi    
      | CEAttr (caname,cexpr_list) ->
	  let ao_list = List.map (compile_cexpr compile_ctxt) cexpr_list in
	  let indep = ManySub (Array.of_list ao_list) in
	  let dep = NoSub in
	  let rattr_sym = Namespace_symbols.rattr_symbol caname in
	  let op_name = AOEAttr rattr_sym in
	  logical_aalgop_mkop op_name indep dep None eh fi    
      | CEAnyAttr (cexpr1,nsenv,cexpr2) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let ao2 = compile_cexpr compile_ctxt cexpr2 in
	  let indep = TwoSub(ao1,ao2) in
	  let dep = NoSub in
	  let op_name = AOEAnyAttr nsenv in
	  logical_aalgop_mkop op_name indep dep None eh fi    
      | CEError cexpr_list ->
	  let ao_list = List.map (compile_cexpr compile_ctxt) cexpr_list in
	  let indep = ManySub (Array.of_list ao_list) in
	  let dep = NoSub in
	  let op_name = AOEError in
	  logical_aalgop_mkop op_name indep dep None eh fi    
      | CETreat (cexpr1,cdt1) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let indep = OneSub ao1 in
	  let dep = NoSub in
	  let adt1 = compile_ctype compile_ctxt cdt1 in
	  let op_name = AOETreat adt1 in
	  logical_aalgop_mkop op_name indep dep None eh fi    
      | CEValidate (vmode,cexpr1) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let indep = OneSub ao1 in
	  let dep = NoSub in
	  let op_name = AOEValidate vmode in
	  logical_aalgop_mkop op_name indep dep None eh fi    
      | CECast (cexpr1,nsenv,cdt1) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let indep = OneSub ao1 in
	  let dep = NoSub in
	  let adt1 = compile_ctype compile_ctxt cdt1 in
	  let op_name = AOECast (nsenv, adt1) in
	  logical_aalgop_mkop op_name indep dep None eh fi    
      | CECastable (cexpr1,nsenv,cdt1) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let indep = OneSub ao1 in
	  let dep = NoSub in
	  let adt1 = compile_ctype compile_ctxt cdt1 in
	  let op_name = AOECastable (nsenv, adt1) in
	  logical_aalgop_mkop op_name indep dep None eh fi    
      | CEForwardAxis (v,axis,cnode_test) 
      | CEReverseAxis (v,axis,cnode_test) ->
          let dot_access =
	    begin
	      match get_variable_field_from_compile_context compile_ctxt v with
	      | None ->
		  let indep = NoSub in
		  let dep = NoSub in
		  let op_name = AOEVar v in
		  logical_aalgop_mkop op_name indep dep None eh fi 
	      | Some crname ->
		  let indep = NoSub in
		  let dep = NoSub in
		  let op_name = AOEAccessTuple(crname) in
		  logical_aalgop_mkop op_name indep dep None eh fi
	    end
	  in
          let indep = OneSub dot_access in
	  let dep = NoSub in
	  let anode_test = compile_cnode_test cnode_test in
	  let op_name = AOETreeJoin(axis,anode_test) in
	  logical_aalgop_mkop op_name indep dep None eh fi    	
      | CESome (ocdt,vname1,cexpr1,cexpr2) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let ao2 = compile_cexpr compile_ctxt cexpr2 in
	  let indep = OneSub ao1 in
	  let dep = OneSub ao2 in
	  let oadt = compile_opt_ctype compile_ctxt ocdt in
	  let op_name = AOESome (oadt, vname1) in
	  logical_aalgop_mkop op_name indep dep None eh fi 
      | CEEvery (ocdt,vname1,cexpr1,cexpr2) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let ao2 = compile_cexpr compile_ctxt cexpr2 in
	  let indep = OneSub ao1 in
	  let dep = OneSub ao2 in
	  let oadt = compile_opt_ctype compile_ctxt ocdt in
	  let op_name = AOEEvery (oadt, vname1) in
	  logical_aalgop_mkop op_name indep dep None eh fi 
      | CELetServerImplement (nc1, uri, hostport, cexpr) ->
	  let hostportao = compile_cexpr compile_ctxt hostport in
	  let ao1 = compile_cexpr compile_ctxt cexpr in
	  let indep = OneSub hostportao in
	  let dep = OneSub ao1 in
	  let op_name = AOEServerImplements (nc1, uri) in 
	  logical_aalgop_mkop op_name indep dep None eh fi
      | CEForServerClose (nc1, uri, cexpr) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr in
	  let indep = OneSub ao1 in
	  let dep = NoSub in
	  let op_name = AOEForServerClose (nc1, uri) in 
	  logical_aalgop_mkop op_name indep dep None eh fi
      | CEEvalClosure (cexpr) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr in
	  let indep =  OneSub ao1 in
	  let dep = NoSub in
	  let op_name = AOEEvalClosure in 
	  logical_aalgop_mkop op_name indep dep None eh fi
      | CEExecute (async, nc, uri, hostport, cexpr) ->
	  let hostportao = compile_cexpr compile_ctxt hostport in
	  let ao1 = compile_cexpr compile_ctxt cexpr in
	  let indep = TwoSub (hostportao, ao1) in
	  let dep = NoSub in
	  let op_name = if (async) then AOEASyncExecute (nc, uri) else AOEExecute (nc, uri) in
	  logical_aalgop_mkop op_name indep dep None eh fi    
      | CECopy cexpr1 ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let indep = OneSub ao1 in
	  let dep = NoSub in
	  let op_name = AOECopy in
	  logical_aalgop_mkop op_name indep dep None eh fi
      | CEDelete cexpr1 ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let indep = OneSub ao1 in
	  let dep = NoSub in
	  let op_name = AOEDelete in
	  logical_aalgop_mkop op_name indep dep None eh fi
      | CEInsert (cexpr1,cinsert_location) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let (ao2,insert_flag) =
	    compile_cinsert_location compile_ctxt cinsert_location
	  in
	  let indep = TwoSub (ao1,ao2) in
	  let dep = NoSub in
	  let op_name = AOEInsert insert_flag in
	  logical_aalgop_mkop op_name indep dep None eh fi
      | CERename (nsenv,cexpr1,cexpr2) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let ao2 = compile_cexpr compile_ctxt cexpr2 in
	  let indep = TwoSub(ao1,ao2) in
	  let dep = NoSub in
	  let op_name = AOERename nsenv in
	  logical_aalgop_mkop op_name indep dep None eh fi
      | CEReplace (value_of_flag,cexpr1,cexpr2) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let ao2 = compile_cexpr compile_ctxt cexpr2 in
	  let indep = TwoSub(ao1,ao2) in
	  let dep = NoSub in
	  let op_name = AOEReplace value_of_flag in
	  logical_aalgop_mkop op_name indep dep None eh fi
      | CESnap (sm,acexpr) ->	  
	  let ao      = compile_cexpr compile_ctxt acexpr in
	  let indep   = NoSub in
	  let dep     = OneSub ao in
	  let op_name = AOESnap sm in
	  logical_aalgop_mkop op_name indep dep None eh fi
      | CELetvar (codt,vn,cexpr1,cexpr2) ->
	  let aodt         = compile_opt_ctype compile_ctxt codt in
	  let ao1          = compile_cexpr compile_ctxt cexpr1 in 
	  let compile_ctxt' = hide_variable_field_from_compile_context compile_ctxt vn in
	  let ao2     =  compile_cexpr compile_ctxt' cexpr2 in 
	  logical_aalgop_mkop (AOELetvar(aodt,vn)) (OneSub ao1) (OneSub ao2) None eh fi	
      | CESet (vn,cexpr1) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in 
	  logical_aalgop_mkop (AOESet vn) (OneSub ao1) NoSub None eh fi	
      | CEImperativeSeq (cexpr1,cexpr2) ->
	  let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  let ao2 = compile_cexpr compile_ctxt cexpr2 in
	  let indep = TwoSub (ao1, ao2) in
	  let dep = NoSub in
	  let op_name = AOEImperativeSeq in
	  logical_aalgop_mkop op_name indep dep None eh fi    
    with
    | exn -> raise (Error.error_with_file_location fi exn)

(*****************************************)
(* Compilation of typeswitch expressions *)
(*****************************************)

and compile_cpattern compile_ctxt cpattern =
  let apattern_desc =
    match cpattern.pcpattern_desc with
      | CCase csequencetype -> ACase (compile_ctype compile_ctxt csequencetype)
      | CDefault -> ADefault
  in
    fmkapattern apattern_desc cpattern.pcpattern_loc

and compile_typeswitch_branch compile_ctxt (cpattern,ovname,cexpr1) =
  let compile_ctxt' =
    match ovname with
      | None -> compile_ctxt
      | Some vname -> hide_variable_field_from_compile_context compile_ctxt vname
  in
  let apattern = compile_cpattern compile_ctxt cpattern in
    (apattern,ovname),(compile_cexpr compile_ctxt' cexpr1)


(************************************)
(* Compilation of flwor expressions *)
(************************************)

and compile_return_clause compile_ctxt op3 return_clause eh fi =
  (* [[ return Expr1 ]](Op3)
     ==
     Map_(i -> [[ Expr1 ]])(Op3) *)

  let op1 = compile_cexpr compile_ctxt return_clause in
(*   let table_op =  *)
(*     logical_aalgop_mkop AOEMaterializeTable (OneSub op3) NoSub None eh fi *)
(*   in  *)
  let map =
    logical_aalgop_mkop AOEMapToItem (OneSub op3) (OneSub op1) None eh fi
  in
    map

and compile_order_by_clause compile_ctxt op3
  (order_by_clause,return_clause) eh fi =
  match order_by_clause with
    | None ->
	compile_return_clause compile_ctxt op3 return_clause eh fi
    | Some (stablekind,corder_spec_list,osig) ->
	begin
	  let rec split_fun corder_spec_list =
	    match corder_spec_list with
	      | [] -> ([],[])
	      | (cexpr,sortkind,emptysortkind) :: corder_spec_list' ->
		  let (cexpr_list,rest_list) = split_fun corder_spec_list' in
		    (cexpr :: cexpr_list, (sortkind,emptysortkind) :: rest_list)
	  in
	  let splited = split_fun corder_spec_list in
	  let asig = compile_overloaded_table_sigs compile_ctxt osig in
	  let op_name = AOEOrderBy (stablekind,snd splited, asig) in
	  let op1_list = List.map (compile_cexpr compile_ctxt) (fst splited) in
	  let op1_array = Array.of_list op1_list in
	  let new_op3 = logical_aalgop_mkop op_name (OneSub op3) (ManySub op1_array) None eh fi in
	    compile_return_clause compile_ctxt new_op3 return_clause eh fi
	end

and compile_where_clause compile_ctxt op3
  (where_clause,order_by_clause,return_clause) eh fi =
  match where_clause with
    | None ->
	compile_order_by_clause compile_ctxt op3
	  (order_by_clause,return_clause) eh fi
    | Some where_expr ->
	let select =
	  compile_select compile_ctxt where_expr op3 eh fi
	in
	  compile_order_by_clause compile_ctxt select
	    (order_by_clause,return_clause) eh fi

and compile_fl_clauses compile_ctxt op3
  fl_clauses (where_clause,order_by_clause,return_clause) eh fi =
  match fl_clauses with
    | [] ->
	(* Calls compilation for the rest of the FLWOR block *)
	begin
	  compile_where_clause compile_ctxt op3
	    (where_clause,order_by_clause,return_clause) eh fi
	end
    | (CEFOR (odt,vname,None,cexpr1)) :: fl_clauses' ->
	(* [[ for $v in Expr1 return Expr2 ]](Op3)
           ==
           [ [[ Expr2 ]](Map_(i -> Map_(j -> [ v : j ] o i)([[ Expr1 ]]))(Op3))
           / $v --> #v ] 
	   
	   Changed to:

           [ [[Expr2]] 
           (MapConcat( i -> MapItemTuple{[v : j]}([[Expr1]]) o i)(Op3)
           / $v --> #v ]
	*)
	(* 1. Compile Expr1 *)      
	begin
	  let op1 = compile_cexpr compile_ctxt cexpr1 in	  
	    (* 2. Build the new Op3 *)
	  let fresh_name = get_fresh_var_name compile_ctxt in
	  let input_j = logical_aalgop_mkop (AOEVar fresh_name) NoSub NoSub None eh fi in

	  let compile_ctxt'  = add_variable_field_to_compile_context compile_ctxt vname   in 
	  let tname          = get_tuple_field_name compile_ctxt' vname  in
	  let aodt           = compile_opt_ctype compile_ctxt odt in
	  let tuple_construct =
	    logical_aalgop_mkop (AOECreateTuple [|(aodt,tname)|]) (ManySub [|input_j|]) NoSub None eh fi
	  in	     
	  let map_inner = 
	    logical_aalgop_mkop (AOEMapFromItem fresh_name) (OneSub op1) (OneSub tuple_construct) None eh fi
	  in
	  let map_outer =
	    logical_aalgop_mkop AOEMapConcat (OneSub op3) (OneSub map_inner) None eh fi
	  in
      	    
	    (* 3. Apply compilation to the rest of the FLWOR block, passing the new Op3 *)
	    compile_fl_clauses compile_ctxt' map_outer
	      fl_clauses' (where_clause,order_by_clause,return_clause) eh fi
	end

    | (CEFOR (odt,vname,Some vname',cexpr1)) :: fl_clauses' ->
	(* [[ for $v at $i in Expr1 return Expr2 ]](Op3)
	   ==
	   [ [[ Expr2 ]](Map_(i -> MapIndex(i)(Map_(j -> [ v : j ] o i)([[ Expr1 ]])))(Op3))
	   / $v --> #v ] 

	   ChangedTo:
	   
	   [ [[ Expr2 ]](MapConcat( t -> MapIndex(i)(MapItemTuple{[v: j]} ([[ Expr1 ]]))) (Op3))
	   / $v --> #v ] 
	*)
	(* 1. Compile Expr1 *)
	begin
	  let op1 = compile_cexpr compile_ctxt cexpr1 in
	    (* 2. Build the new Op3 *)
	  let fresh_name =  get_fresh_var_name compile_ctxt in
	  let input_j = logical_aalgop_mkop (AOEVar fresh_name) NoSub NoSub None eh fi in

	  (***************************)
	  (* Compile the tuple names *)
	  (***************************)
	  let compile_ctxt'  = add_variable_field_to_compile_context compile_ctxt vname   in 
	  let compile_ctxt'' = add_variable_field_to_compile_context compile_ctxt' vname' in

	  let tname         = get_tuple_field_name compile_ctxt'' vname  in
	  let tname'        = get_tuple_field_name compile_ctxt'' vname' in
	  let aodt           = compile_opt_ctype compile_ctxt odt in

	  let tuple_construct =
	    logical_aalgop_mkop (AOECreateTuple [|(aodt,tname)|]) (ManySub [|input_j|]) NoSub None eh fi
	  in

	  let input_i = compile_inputtuple eh fi in
	  let tuple_append =
	    logical_aalgop_mkop AOEConcatTuples (TwoSub (tuple_construct,input_i)) NoSub None eh fi
	  in
	  let map_inner =
	    logical_aalgop_mkop (AOEMapFromItem fresh_name) (OneSub op1) (OneSub tuple_append) None eh fi
	  in
	  let map_index =
	    logical_aalgop_mkop (AOEMapIndex(tname')) (OneSub map_inner) NoSub None eh fi
	  in

	  let map_outer = 
	    logical_aalgop_mkop AOEMapConcat (OneSub op3) (OneSub map_index) None eh fi
	  in

	    (* 3. Apply compilation to the rest of the FLWOR block, passing the new Op3 *)
	    
	    compile_fl_clauses compile_ctxt'' map_outer
	      fl_clauses' (where_clause,order_by_clause,return_clause) eh fi
	end

    | (CELET (odt,vname,cexpr1)) :: fl_clauses' ->
	(* [[ let $v in Expr1 return Expr2 ]](Op3)
	   ==
	   [ [[ Expr2 ]](Map_(i -> [ v : [[ Expr1 ]] ] o i)(Op3)) / $v --> #v ] 
	   Changed To:
	   [ [[ Expr2 ]](MapConcat(t -> [ v : [[ Expr1 ]] ]))(Op3) / $v --> #v


	*)
	(* 1. Compile Expr1 *)
	begin
	  let op1 = compile_cexpr compile_ctxt cexpr1 in
	  let compile_ctxt' = add_variable_field_to_compile_context compile_ctxt vname in 
	  let tname         = get_tuple_field_name compile_ctxt' vname in

	  let aodt           = compile_opt_ctype compile_ctxt odt in
	    (* 2. Build the new Op3 *)
	  let tuple_construct =
	    logical_aalgop_mkop (AOECreateTuple [|(aodt,tname)|]) (ManySub [|op1|]) NoSub None eh fi 
	  in	  
	  let map = logical_aalgop_mkop AOEMapConcat (OneSub op3) (OneSub tuple_construct) None eh fi in
	    
	  compile_fl_clauses compile_ctxt' map
	    fl_clauses' (where_clause,order_by_clause,return_clause) eh fi
	end

and compile_flwor compile_ctxt fl_clauses
    (where_clause,order_by_clause,return_clause) eh fi =
  (* A special case is handled first, i.e. only let bindings and no other conditions *)
  (* Order By can be used, but is meaningless in all let bindings *)
  begin
    (* The initial input is an empty table *)
    let init_op3 =
      if has_input_set compile_ctxt
      then
	compile_inputtuple eh fi
      else 
	logical_aalgop_mkop (AOECreateTuple [||]) (ManySub [||]) NoSub None eh fi
    in
    compile_fl_clauses compile_ctxt init_op3
      fl_clauses (where_clause,order_by_clause,return_clause) eh fi
  end

and compile_select compile_ctxt select_clause op3 eh fi =
  let rec compile_select_helper cur_expr replace_flag =
    match cur_expr.pcexpr_desc with
    | CEIf(cexpr1, cexpr2, cexpr3) ->
	(******************************************)
	(* Restricted form                        *)
	(* Or  -> if (e1) then true else (e2)     *)
	(* And -> if (e1) then (e2) else false|() *)
	(******************************************)
	let ae1 = compile_select_helper cexpr1 replace_flag in
	if (is_fn_true cexpr2)
	then
	  Cs_Disjunction( ae1, (compile_select_helper cexpr3 replace_flag))
	else if (is_fn_false cexpr3 || is_ce_empty cexpr3)
	then
	  begin
	    let rhs = compile_select_helper cexpr2 replace_flag in
	    match (ae1, rhs) with
	    | (Cs_SimpleConjunct( c1 ), Cs_SimpleConjunct (c2)) -> 
		Cs_SimpleConjunct ( c1 @ c2 ) 
	    | _ -> 
		Cs_ComplexConjunct( ae1, rhs )
	  end
	else 
	  begin
	    (* An introduced if statement... can not compile it *)
	    let ae1 = compile_cexpr compile_ctxt cur_expr in
	    Cs_SimpleConjunct ( ae1 :: [] )
 	  end
    | CECall(_,[cexpr],_,_,_) when (is_fn_boolean_cexpr cur_expr) ->
	(* We strip these out as we walk, but they should be replaced below *)
	compile_select_helper cexpr true
    | _ ->
	(* Here we need to ensure that we compute the effective
	   boolean value of these *)
	let norm_ctxt = norm_context_from_compile_context compile_ctxt in
	let cur_expr = 
	  if ((is_fn_boolean_cexpr cur_expr) || not(replace_flag))  (* no need to do it twice *)
	  then cur_expr
	  else 
	    begin
	      let eh         = cur_expr.pcexpr_origin in
	      let fi         = cur_expr.pcexpr_loc in
	      Norm_util.normalize_effective_boolean_value norm_ctxt cur_expr eh fi
	    end
	in
	let ae1 = compile_cexpr compile_ctxt cur_expr in
	Cs_SimpleConjunct ( ae1 :: [] )
  in
  let rec compile_conjuncts conjunct index = 
    match conjunct with
    | Cs_SimpleConjunct ( op_list ) ->
	let length = List.length op_list in 
	let o_list = Array.of_list op_list in
	SimpleConjunct( index, (index + length -1)), o_list, (index + length)
    | Cs_ComplexConjunct(c1, c2) ->
	let op1, op_l1, index = compile_conjuncts c1 index in
	let op2, op_l2, index = compile_conjuncts c1 index in	     
	let ops = Array.append op_l1 op_l2 in
	ComplexConjunct(op1,op2), ops, index
    | Cs_Disjunction (d1,d2) ->
	let c1, op1, cur_index = compile_conjuncts d1 index in
	let c2, op2, cur_index = compile_conjuncts d2 cur_index in
	let ops = Array.append op1 op2 in
	Disjunct( c1,c2 ), ops, cur_index
  in
  let internal_conjunct_desc = compile_select_helper select_clause false in
  let conjunct_desc,ops,_    = compile_conjuncts internal_conjunct_desc 0 in
  logical_aalgop_mkop (AOESelect conjunct_desc) (OneSub op3) (ManySub ops) None eh fi


(*************************************)
(* Compilation of update expressions *)
(*************************************)

and compile_cinsert_location compile_ctxt cinsert_location =
  match cinsert_location with
    | CUAsLastInto cexpr1 ->
	let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  (ao1,AOUAsLastInto)
    | CUAsFirstInto cexpr1 ->
	let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  (ao1,AOUAsFirstInto)
    | CUInto cexpr1 ->
	let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  (ao1,AOUInto)
    | CUAfter cexpr1 ->
	let ao1 = compile_cexpr compile_ctxt cexpr1 in 
	  (ao1,AOUAfter)
    | CUBefore cexpr1 ->
	let ao1 = compile_cexpr compile_ctxt cexpr1 in
	  (ao1,AOUBefore)

