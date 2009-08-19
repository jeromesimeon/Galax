(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: norm_expr.ml,v 1.91 2007/11/16 21:16:52 mff Exp $ *)

(* Module: Norm_expr
   Description:
     This module implements normalization for XQuery expressions.
*)

open Format

open Error

open Namespace_names
open Namespace_builtin
open Namespace_symbols
open Namespace_resolve

open Datatypes
open Datatypes_util

open Xquery_common_ast
open Xquery_common_ast_util

open Xquery_ast
open Xquery_ast_util

open Xquery_type_ast
open Xquery_type_ast_util

open Xquery_core_ast
open Xquery_core_ast_annotation
open Xquery_core_ast_util

open Norm_context
open Norm_util


(***************)
(* Expressions *)
(***************)

(***********************************************************************************)
(* Goal: transversing the Full XQuery expression AST                               *)
(* normalize_expr:                                                                 *)
(* - match the Full expression                                                     *)
(* - project its parts                                                             *)
(* - apply normalize_expr on each parts -> obtaining parts'                        *)
(* - construct intermediate mapping structure, defined in maputil.mli, from parts' *)
(* - apply the mapping function                                                    *)
(***********************************************************************************)


(************************************)
(* aux functions for normalize_expr *)
(************************************)

let is_constructor_function cxschema rfname =
  let fname_sym = Namespace_symbols.rtype_symbol rfname in
    try
      Schema_judge.derives_from
        cxschema
        fname_sym
        Namespace_symbols_builtin.xs_anyAtomicType
    with
      | _ -> false


let get_free_vars cx = 
  let rec get_free_rec cx free bound  =
    match cx.pcexpr_desc with
    | CEScalar _ -> free
    | CEUnordered x
    | CEOrdered x -> get_free_rec x free bound
    | CEFLWOR (fls,_,_,rx) ->
        let free',bound' = 
          List.fold_left
            (fun (fv,bv) fl -> match fl with
              | CELET (_,v,x) -> 
                  let b' = v::bv in (get_free_rec x fv b', b')
              | CEFOR (_,v,i,x)-> 
                  let b' = v::bv in
                  let b'' = 
                    (match i with
                      | None -> b'
                      | Some p -> p::b' ) in
                    (get_free_rec x fv b'',b'') ) 
            (free,bound) fls 
        in
          get_free_rec rx free' bound'

    | CELetServerImplement (_,_,x',x'') ->
        let free'  = get_free_rec x' free bound in
        get_free_rec x'' free' bound
    | CEExecute (_,_,_,_,x') 
    | CEForServerClose (_,_,x') 
    | CEEvalClosure (x') ->
        get_free_rec x' free bound
    | CEIf (x',x'',x''') -> 
        let free'  = get_free_rec x' free bound in
        let free''  = get_free_rec x'' free' bound in        
          get_free_rec x''' free'' bound
    | CEWhile (x',x'') -> 
        let free'  = get_free_rec x' free bound in
        get_free_rec x'' free' bound
    | CETypeswitch (tx, clist) -> 
        let free' = get_free_rec tx free bound in
        let free'',_ = 
          List.fold_left
            (fun (fv,bv) cl -> match cl with 
              | (_,None,x) -> (get_free_rec x fv bv, bv)
              | (_,Some v,x) -> let bv' = v::bv in (get_free_rec x fv bv', bv'))
            (free',bound) clist
        in free''
    | CEVar v -> 
        if List.mem v bound then free 
        else if List.mem v free then free
        else (v::free)
    | CECall (_,es,_,_,_) 
    | CEOverloadedCall(_,es,_)  
    | CEElem (_,_,es) 
    | CEAttr(_,es) 
    |   CEError es  ->  List.fold_left (fun fv x -> get_free_rec x fv bound) free es                   
    | CESeq (x',x'') 
    | CEImperativeSeq (x',x'') ->       
        let free'  = get_free_rec x' free bound in
          get_free_rec x'' free' bound
    | CEEmpty -> free
    | CEPIComputed (x',x'') ->       
        let free'  = get_free_rec x' free bound in
          get_free_rec x'' free' bound 
    | CEPI _  | CEComment _   | CEText _ | CECharRef _ -> free
    | CEDocument x 
    | CECommentComputed x
    | CETextComputed x -> get_free_rec x free bound
    | CEAnyElem (x',_,_,x'')
    | CEAnyAttr (x',_,x'')->       
        let free'  = get_free_rec x' free bound in
          get_free_rec x'' free' bound 
    | CETreat (x,_) | CEValidate(_,x) | CECast(x,_,_) | CECastable(x,_,_) -> get_free_rec x free bound 
    | CEForwardAxis _ | CEReverseAxis _ -> free

    | CESome (_,v,x',x'') | CEEvery (_,v,x',x'') -> 
        let bv' = (v::bound) in 
        let free' = get_free_rec x' free bv' in
          get_free_rec x'' free' bv'
    | CECopy x 
    | CEDelete x -> get_free_rec x free bound 
    | CEInsert (x,loc) -> 
        let free' = get_free_rec x free bound in
          get_free_rec (get_expr_from_insert_location loc) free' bound
    | CELetvar (_,v,x',x'') ->
        let bv' = (v::bound) in 
        let free'  = get_free_rec x' free bv' in
          get_free_rec x'' free' bv'         
    | CERename (_, x',x'') 
    | CEReplace(_,x',x'') -> 
        let free'  = get_free_rec x' free bound in
          get_free_rec x'' free' bound 
    | CESnap(_, x) -> get_free_rec x free bound
    | CEProtoValue _ -> free
    | CESet(_,x) ->  get_free_rec x free bound 

  in
    get_free_rec cx [] []
      
(* Note:
   Several 'special' functions are mapped to core expressions or
   special variables.
   - Jerome *)

(*******************************)
(* Beginning of normalize_expr *)     
(*******************************)
let normalize_expr norm_context e = 

  let cfunction_defs = ref [] in
  let rec normalize_function_application norm_context rfname celist e fi =
    let mod_proc_ctxt = module_context_from_norm_context norm_context in
    (* Gets the prefix, uri and local name of the function *)
    let (prefix,uri,lname) = rfname in
    let arity = List.length celist in
    let cxschema = cxschema_from_norm_context norm_context in 
    if (is_constructor_function cxschema rfname)
    then
      normalize_atomic_constructor norm_context rfname celist (Some e) fi
        (* Variadic or special functions in the fn: namespace  *)
    else if (uri = fn_uri)
    then
      begin
        (* fn:error() 
           Mary: I think that the variants of fn:error() should all be normalized here to
           fn:error(QName, string?, item* ).
           All the variant argument logic is in fn_error.ml right now, which I think is a hack.
        *)
        if (rqname_equal rfname fn_error) then 
          if (arity <= 3) then 
	    fmkcexpr (CEError celist) (Some e) fi
	  else
	    raise (incorrect_arg_count rfname (List.length celist) 0)
              (* fn:position() *)
        else if (rqname_equal rfname fn_position && arity = 0) then
	  cexpr_fs_position (Some e) fi

        (* fn:last() *)
        else if (rqname_equal rfname fn_last && arity = 0) then
	  cexpr_fs_last (Some e) fi

        (* fn:concat() *)
        else if (rqname_equal rfname fn_concat) then
          (* Note: fn:concat is variadic; convert arguments to sequence of strings then call fn:string-join *)
	  if (arity >= 2) then 
	    let fn_concat ce1 ce2 = normalize_function_app norm_context fn_concat [ce1; ce2] (Some e) fi in
	    List.fold_left fn_concat (fmkcexpr (CEScalar (StringLiteral "")) (Some e) fi) (celist) 
	  else
	    raise (Query(Parameter_Mismatch "fn:concat requires two or more arguments"))

        (* fn:resolve-uri() *)
        else if (rqname_equal rfname fn_resolve_uri && arity = 1) then
	  let base_uri  = Processing_context.get_base_uri mod_proc_ctxt in
	  match base_uri with
	  | None -> raise (Query (Parameter_Mismatch "[err:FONS0005] No base-uri property in the static context"))
	  | Some base_uri ->
	      let base_uri_item = fmkcexpr (CEScalar (StringLiteral (AnyURI._string_of_uri base_uri))) (Some e) fi in
	      build_core_call norm_context fn_resolve_uri (celist@[base_uri_item]) (Some e) fi

        (* fn:not() *)
        else if (rqname_equal rfname fn_not && arity = 1) then
	  begin
	    match celist with
	    | [ce] ->
	        let ce' = normalize_effective_boolean_value norm_context ce (Some e) fi in
	        build_core_call norm_context fn_not [ce'] (Some e) fi
	    | _ ->
	        raise (incorrect_arg_count rfname (List.length celist) 1)
	  end
        else 
	      (* The overloaded semantics of these functions cannot be resolved until the 
	         implicit_timezone is known at runtime in the execution context.
	      *)
	  let rfname = 
	    if (rqname_equal rfname fn_adjust_time_to_timezone && arity = 1) then 
	      fn_adjust_time_to_timezone_unary 
	    else if (rqname_equal rfname fn_adjust_date_to_timezone && arity = 1) then
	      fn_adjust_date_to_timezone_unary 
	    else if (rqname_equal rfname fn_adjust_dateTime_to_timezone && arity = 1) then
	      fn_adjust_dateTime_to_timezone_unary 
	    else rfname 
	  in 
	  let celist' = 
	    if (rqname_equal rfname fn_string && arity = 0) then
 		      (* fn:string() is normalized to fn:string($dot) *)
	      (cexpr_fs_dot (Some e) fi) :: []
	    else if (rfname = fn_string && arity =1 ) then
	      (List.hd celist) :: []
	    else if (rqname_equal rfname fn_name && arity = 0) then
		      (* fn:name() is normalized to fn:name($dot) *)
	      cexpr_fs_dot (Some e) fi :: []
	    else if (rqname_equal rfname fn_base_uri && arity = 0) then
		      (* fn:base-uri() is normalized to fn:base-uri($dot) *)
	      cexpr_fs_dot (Some e) fi :: []
	    else if (rqname_equal rfname fn_normalize_space && arity = 0) then
		      (* fn:base-uri() is normalized to fn:normalize-space($dot) *)
	      cexpr_fs_dot (Some e) fi :: []
	    else if (rqname_equal rfname fn_lang && arity = 1) then
		      (* fn:lang($textlang) is normalized to fn:name($textlang,$dot) *)
	      celist @ [(cexpr_fs_dot (Some e) fi)]
	    else if (rqname_equal rfname fn_normalize_unicode && arity = 1) then
		      (* fn:normalize-unicode($s) is normalized to fn:normalized-unicode($s,"NFC") *)
	      (List.hd celist) :: (fmkcexpr (CEScalar (StringLiteral "NFC")) (Some e) fi) :: []
	    else if (rqname_equal rfname fn_local_name && arity = 0) then
		      (* fn:local-name() is normalized to fn:local-name($dot) *)
	      (cexpr_fs_dot (Some e) fi ) :: []

	    else if (rqname_equal rfname fn_number && arity = 0) then
	          (* fn:number() is normalized to fn:number($dot) *)
	      (cexpr_fs_dot (Some e) fi ) :: []

  	    else if ((rqname_equal rfname fn_round_half_to_even)  && arity = 1) then
		      (* fn:sum(Expr) is normalized to fn:sum(Expr,0) *)
	      (List.hd celist) :: (fmkcexpr (CEScalar (IntegerLiteral Decimal._integer_zero)) (Some e) fi):: []
	    else if (rqname_equal rfname fn_namespace_uri && arity = 0) then
		      (* fn:namespace-uri() is normalized to fn:namespace-uri($dot) *)
	      (cexpr_fs_dot (Some e) fi) :: []
	    else if (rqname_equal rfname fn_substring && arity = 2) then
	      celist @ (fmkcexpr (CEScalar (DoubleLiteral infinity)) (Some e) fi):: []
	    else if ((rqname_equal rfname fn_matches or rqname_equal rfname fn_tokenize) && arity = 2) then
	      celist @ (fmkcexpr (CEScalar (StringLiteral "")) (Some e) fi) :: []
	    else if (rqname_equal rfname fn_replace && arity = 3) then
	      celist @ (fmkcexpr (CEScalar (StringLiteral "")) (Some e) fi) :: []
	    else if (((rqname_equal rfname fn_compare
		     || rqname_equal rfname fn_contains 
		     || rqname_equal rfname fn_starts_with) 
		    || rqname_equal rfname fn_ends_with
		    || rqname_equal rfname fn_substring_before
		    || rqname_equal rfname fn_substring_after
		    || rqname_equal rfname fn_index_of
		    || rqname_equal rfname fn_deep_equal) && arity = 2) ||
		      (rqname_equal rfname fn_distinct_values && arity = 1)
	    then
	      celist @ (fmkcexpr (CEScalar (StringLiteral (Processing_context.get_default_collation mod_proc_ctxt))) (Some e) fi) :: []
	    else if ((rqname_equal rfname fn_id || rqname_equal rfname fn_id) && arity = 1) then
	      celist @ (cexpr_fs_dot (Some e) fi :: [])
	    else
	      celist
	  in normalize_function_app norm_context rfname celist' (Some e) fi
      end	
    else
      normalize_function_app norm_context rfname celist (Some e) fi

(*
   [attribute QName { Expr }]Expr
   ==
   attribute QName { fs:item-sequence-to-untypedAtomic(([Expr]_Expr)) }
*)
  and normalize_eattfixed norm_ctxt caname celist eh fi =
    let normalize_eattcontent ce =
      normalize_atomize norm_ctxt ce eh fi
    in
    let celist' = List.map normalize_eattcontent celist in
    build_core_attribute_constructor norm_ctxt caname celist' eh fi

(* 
   [attribute { Expr1 } { Expr2 }]Expr
   ==
   attribute { fn:data(([Expr1]Expr)) } { fs:item-sequence-to-untypedAtomic(([Expr2]Expr)) }
*)
  and normalize_eattcomputed norm_ctxt caname celist eh fi =
    let name = normalize_atomize norm_ctxt caname eh fi in
    let ce' = map_to_sequence eh fi celist in
    let ce'' = normalize_atomize norm_ctxt ce' eh fi in
    (* There is no way to change the namespace environment for an computed attribute constructor - Jerome *)
    let nsenv = nsenv_from_norm_context norm_ctxt in 
    let fs_untyped_cexpr = item_seq_to_untyped norm_ctxt ce'' eh fi in
    fmkcexpr (CEAnyAttr (name, nsenv, fs_untyped_cexpr)) eh fi

(*
   [element QName { Expr }]Expr
   ==
   element QName { fs:item-sequence-to-node-sequence(([Expr]Expr)) }
*)
  and normalize_eelemfixed norm_ctxt cename cattlist childlist eh fi =
  (* 1. The namespace environment must be passed to the element constructor *)
    let nsenv = get_in_scope_nsenv norm_ctxt in
    (* 2. The processing context contains the boundary whitespace behavior *)
    let mod_proc_ctxt = module_context_from_norm_context norm_ctxt in
    (* 3. If boundary whitespace behavior is set to 'strip', we first
       convert boundary whitespace into empty text nodes. *)
    let childlist' =
      if (mod_proc_ctxt.Processing_context.boundary_space_kind = Xquery_common_ast.Strip)
      then remove_boundary_whitespace_from_children childlist
      else childlist
    in
    (* 4. We then remove empty text nodes at beginning and end of
       element content.  Any empty text nodes within the sequence are
       significant. *)
    let cchildlist = normalize_expr_list norm_ctxt  childlist' in
    let cchildlist' = remove_empty_textnodes cchildlist in
    (* 5. NB: fs:item-sequence-to-node-sequence() is injected in the
       normalized expression just for the purposes of typing.  It is
       always removed after typing.  The function's semantics
       implemented in Streaming_constructors.element_constructor. *)
    let content = item_seq_to_node_seq norm_ctxt (map_to_sequence eh fi (cattlist @ cchildlist')) eh fi in
    (* 6. Finally, the core element constructor is built *)
    build_core_element_constructor norm_ctxt cename nsenv [content] eh fi

(* 
   [element { Expr1 } { Expr2 }]Expr
   ==
   element { fn:data(([Expr1]_Expr)) }{ fs:item-sequence-to-node-sequence(([Expr2]_Expr))}
*)           
  and normalize_eelemcomputed norm_ctxt nsenv1 cename cchildlist eh fi =
    let name = normalize_atomize norm_ctxt cename eh fi in
    (* Currently, there is no way to change the namespace environment
       for a computed element constructor - Jerome *)
    let nsenv2 = get_in_scope_nsenv norm_ctxt in
    (* NB: fs:item-sequence-to-node-sequence() is injected in the
       normalized expression just for the purposes of typing.  It is
       always removed after typing.  The function's semantics
       implemented in Streaming_constructors.element_constructor.  *)
    (* See dynamic semantics in Code_constructors *)
    let content = item_seq_to_node_seq norm_ctxt (map_to_sequence eh fi cchildlist) eh fi in
    fmkcexpr (CEAnyElem (name, nsenv1, nsenv2, content)) eh fi 

(* 

   Here's what the spec says:

   3.8.3 Order By and Return Clauses

   The process of evaluating and comparing the orderspecs is based on
   the following rules:

   * Atomization is applied to the result of the expression in each
   orderspec. If the result of atomization is neither a single atomic
   value nor an empty sequence, a type error is raised.

   * If the value of an orderspec has the type xs:untypedAtomic, it is
   cast to the type xs:string.

   * Each orderspec must return values of the same type for all tuples
   in the tuple stream, and this type must be a (possibly optional)
   atomic type for which the gt operator is defined--otherwise, a
   dynamic error is raised.

   Here's what we implement:

   * Atomization is applied to the result of the expression in each
   orderspec. If the result of atomization is neither a single atomic
   value nor an empty sequence, a type error is raised.

   * Currently, do not cast xs:untypedAtomic to xs:string

*)

  and normalize_pattern norm_context p = 
    match p.ppattern_desc with
    | Case dt ->
        fmkcpattern (CCase (normalize_sequencetype norm_context dt)) p.ppattern_loc
    | Default ->
        fmkcpattern CDefault p.ppattern_loc

  and normalize_pat_expr norm_context pat_expr = 
    let (p, ovn, expr) = pat_expr in
    let (norm_context,covn) =
      match ovn with
      | None -> (norm_context,None)
      | Some vname ->
	  let (norm_context,cvname) =
	    (resolve_variable_qname_register norm_context vname)
	  in
	  (norm_context,Some cvname)
    in
    let p' = normalize_pattern norm_context p in 
    let ce = normalize_expr_aux norm_context expr in
    (norm_context, p', covn, ce)

  and normalize_pat_exprs norm_context pat_exprs =
    match pat_exprs with
    | [] -> (norm_context,[])
    | x :: pat_exprs' ->
        let (norm_context,p,covn,ce) = normalize_pat_expr norm_context x in
        let (norm_context,r) = normalize_pat_exprs norm_context pat_exprs' in
        (norm_context, (p,covn,ce) :: r)

(******************)
(* normalize_expr *)
(******************)

  and normalize_expr_aux norm_context e =
    let desc = e.pexpr_desc
    and fi = e.pexpr_loc 
    and eh = Some e in 
    let nsenv = nsenv_from_norm_context norm_context in 

let string_of_cvar cv = 
  match cv with 
  | Some CEVarExternal -> "External"
  | Some CEVarInterface -> "Interface"
  | Some CEVarImported -> "Imported"
  | Some CEVarUser _ -> "User"
  | None -> "None"
in

    (* The surface-syntax expression contains the file location that
       should be correlated with errors, so we catch any exceptions here
       and rewrap the exceptions with the file location. *) 
    try
      let cexpr_result =
	begin
          (* Expressions presented in order in the XQuery Formal Semantics *)
	  match desc with
	  | EScalar bv ->
	      fmkcexpr (CEScalar bv) eh fi
	  | EVar vname ->
	      begin
		let (cvname, ace_opt) = resolve_variable_qname_check norm_context fi vname in
		(* Dependencies *)
		begin
		  match ace_opt with
		  | None -> ()
		  | Some _ -> add_var_dependency norm_context cvname
		end;
		let (nc1, _, _) = cvname in 
		let cevar = mkcvar cvname eh fi in
		match nc1, ace_opt with
                (* Variable defined in scope of let-server *)              
		| NSServerPrefix ncv, Some CEVarInterface-> 
                    (* Lookup meaning of prefix ncv *)
		    let (nc1v, uri) = Norm_util.check_server_implementation norm_context fi ncv in
                    let nc1var = fmkcexpr (CEVar nc1v) eh fi in 
		    (* If not nested within an execute expression, then wrap the variable reference *)
		    if (get_in_execute_expr norm_context) then cevar
		    else fmkcexpr (CEExecute (false, ncv, uri, nc1var, cevar)) (Some e) fi
		| _, Some CEVarInterface -> 
		    raise (Query(DXQ_Error("Cannot refer to interface variable "^
					   (prefixed_string_of_rqname cvname)^" in expression.")))
		| NSPrefix _, Some CEVarImported
		| (NSDefaultElementPrefix|NSPrefix _), (None|Some CEVarUser _|Some CEVarExternal) -> cevar
		| p, cv  -> raise (Query(Internal_Error("Malformed reference to variable "^
			(prefixed_string_of_rqname cvname)^"/"^(Namespace_names.string_of_prefix p)^
							"/"^(string_of_cvar cv))))

	      end
	  | EList elist ->
	      let elist' = normalize_expr_list norm_context  elist in
	      map_to_sequence eh fi elist' 
	  | EApp (fn, elist) ->
	      let frfname = resolve_function_qname nsenv fn in
	      (* Dependencies *)
	      add_fun_dependency norm_context frfname;
	      let celist = normalize_expr_list norm_context  elist in
	      normalize_function_application norm_context frfname celist e fi
	  | EAttrFixed (aname, elist) ->
	      let caname = resolve_attribute_qname nsenv aname in
	      let celist = normalize_enclosed_expr_list norm_context elist in
	      normalize_eattfixed norm_context caname celist eh fi
	  | EAttrComputed (elist1, elist2) ->
	      let celist1 = normalize_expr_list norm_context  elist1 in
	      let caname = map_to_sequence eh fi celist1 in
	      let celist2 = normalize_expr_list norm_context  elist2 in
	      normalize_eattcomputed norm_context caname celist2 eh fi
	  | ETreat (e1, dt) ->
	      let ce = normalize_expr_aux norm_context  e1 in
	      let cdt = normalize_sequencetype norm_context dt in
	              (* Treat inherits its annotation from its argument expression *)
	      let annot = copy_annot ce.pcexpr_annot in
	      let treat_cexpr = fmkcexpr (CETreat (ce,cdt)) eh fi in
	      begin
		set_annotation_for_cexpr treat_cexpr annot;
		treat_cexpr
	      end
	  | EValidate (vmode,e1) ->
	      let ce = normalize_expr_aux norm_context  e1 in
	      let cvmode =
		match vmode with
		| None -> Strict
		| Some cvmode -> cvmode
	      in
	              (* Validate inherits its annotation from its argument expression *)
	      let annot = copy_annot ce.pcexpr_annot in
	      let validate_cexpr = fmkcexpr (CEValidate (cvmode,ce)) eh fi in
	      begin
		set_annotation_for_cexpr validate_cexpr annot;
		validate_cexpr
	      end
	  | ECast (e, dt) ->
                (* Argument to cast can be a node sequence, which is mapped
                   to a sequence of atomic values *)
	      let ce = normalize_expr_aux norm_context  e in
	      let cdt = normalize_sequencetype norm_context dt in 
	      normalize_to_cast norm_context ce cdt eh fi
	  | ECastable (e, dt) ->
                (* Argument to castable can be a node sequence, which is
                   mapped to a sequence of atomic values *)
	      let ce = normalize_expr_aux norm_context  e in
	      let cdt = normalize_sequencetype norm_context dt in 
	      normalize_to_castable norm_context ce cdt eh fi
	  | EBinaryOp (e1, bop, e2) ->
	      let ce1 = normalize_expr_aux norm_context  e1 in
	      let ce2 = normalize_expr_aux norm_context  e2 in
	      normalize_binary_operator norm_context bop ce1 ce2 eh fi
	  | EElemFixed (ename, attlist, childlist) ->
	      let (new_nss, other_atts) = get_ns_attributes attlist in
	      let norm_context',nsenv' = add_ns_bindings_to_norm_context norm_context new_nss in
	      let cattlist = normalize_expr_list norm_context' other_atts in
	      let cename = resolve_element_qname nsenv' ename in
	      if (!Conf.embed_xqueryx && (Namespace_names.rqname_equal Namespace_builtin.xqx_xquery cename))
	      then
		let newe =
		  Parse_xqueryx.normalize_xqx_xquery_expr other_atts childlist fi
		in
		normalize_expr_aux norm_context newe
	      else
		normalize_eelemfixed norm_context' cename cattlist childlist eh fi
	  | EElemComputed (elist, childlist) ->
	      let celist = normalize_expr_list norm_context elist in
	      let cename = map_to_sequence eh fi celist in
	      let cchildlist = normalize_expr_list norm_context  childlist in
	      normalize_eelemcomputed norm_context nsenv cename cchildlist eh fi
	  | EPragma (name, content, childlist) ->
	      let _ = resolve_pragma_qname nsenv name in
	      let cchildlist = normalize_expr_list norm_context  childlist in
	      map_to_sequence eh fi cchildlist
	  | EFLWOR (fl_clauses, where_clause, order_by_clause, return_clause) ->
	            (* Note:
	               As of 07/19/2004, FLWOR block are kept 'as is' in the
	               normalized core.
	               - Jerome
	            *)
	      let (norm_context,cfl_expr_list) = normalize_fl_clauses norm_context e fl_clauses in
	      let cwhere_clause = normalize_where_clause norm_context  where_clause fi in
	      let corder_by_clause = normalize_order_by_clause norm_context order_by_clause fi in
	      let creturn_clause = normalize_expr_aux norm_context return_clause in
	      fmkcexpr (CEFLWOR (cfl_expr_list,cwhere_clause,corder_by_clause,creturn_clause)) eh fi
	  | EIf (cond, e1, e2) ->
	      let cond' = normalize_expr_aux norm_context  cond in
	      let e1' = normalize_expr_aux norm_context  e1 in
	      let e2' = normalize_expr_aux norm_context  e2 in
	      normalize_to_if norm_context cond' e1' e2' eh fi
	  | EEnclosed e ->
	      normalize_expr_aux norm_context  e
	  | EPath p -> 
	      normalize_path_expr norm_context e p
	  | EPI (target,pi_content) ->
	      let _ = Streaming_ops.check_valid_processing_instruction target pi_content in 
	      fmkcexpr (CEPI (target,pi_content)) eh fi
	  | EPIComputed (elist1,elist2) ->
	      let celist1 = normalize_expr_list norm_context  elist1 in
	      let ce1 = map_to_sequence eh fi celist1 in
	      let target = normalize_atomize norm_context ce1 eh fi in
	      let celist2 = normalize_expr_list norm_context  elist2 in
	      let ce2 = map_to_sequence eh fi celist2 in
	      let pi_content = item_seq_to_untyped norm_context ce2 eh fi in
	      fmkcexpr (CEPIComputed (target,pi_content)) eh fi
	  | EText t ->
	      fmkcexpr (CEText t) eh fi
	  | ECharRef cp ->
	      fmkcexpr (CECharRef cp) eh fi
	  | ETextComputed elist ->
	      let elist' = normalize_expr_list norm_context  elist in
	      let e' = map_to_sequence eh fi elist' in
	      let e'' = item_seq_to_untyped_optional norm_context e' eh fi in
	      fmkcexpr (CETextComputed e'') eh fi
	  | EDocument elist ->
                (*
	           [document { Expr }]Expr
	           ==
	           document { fs:item-sequence-to-node-sequence(([Expr]_Expr)) }
	        *)
	      let elist' = normalize_expr_list norm_context  elist in
	      let e' = map_to_sequence eh fi elist' in
	      let e'' = item_seq_to_node_seq norm_context e' eh fi in
	      fmkcexpr (CEDocument e'') eh fi
	  | ESelf ->
	      cexpr_fs_dot eh fi
	  | EParent ->
	      let ce = 
		CEReverseAxis(fs_dot,
			      Parent,
			      CPNodeKindTest(normalize_kind_test norm_context AnyKind))
	      in fmkcexpr ce eh fi
	  | ESome (bdl, e2) ->
	      let rec normalize_bindings norm_context bdl =
		match bdl with
		| [] -> normalize_expr_aux norm_context e2
		| (odt,vname,e1) :: obdl ->
		    let e1' = normalize_expr_aux norm_context e1 in
		    let (norm_context,cvname) = resolve_variable_qname_register norm_context vname in
		    let result_expr = normalize_bindings norm_context obdl in
		    let rodt = (normalize_optional_sequencetype norm_context odt) in
		    normalize_to_some norm_context rodt cvname e1' result_expr eh fi
	      in
	      normalize_bindings norm_context bdl
	  | EEvery (bdl, e2) ->
	      let rec normalize_bindings norm_context bdl =
		match bdl with
		| [] -> normalize_expr_aux norm_context e2
		| (odt,vname,e1) :: obdl ->
		    let e1' = normalize_expr_aux norm_context  e1 in 
		    let (norm_context,cvname) = resolve_variable_qname_register norm_context vname in
		    let result_expr = normalize_bindings norm_context obdl in
		    let rodt = (normalize_optional_sequencetype norm_context odt) in
		    normalize_to_every norm_context rodt cvname e1' result_expr eh fi
	      in
	      normalize_bindings norm_context bdl
	  | ETypeswitch (e1, pat_expr_list) ->
	      let e1' = normalize_expr_aux norm_context  e1 in
	      let (_,pat_cexpr_list) = normalize_pat_exprs norm_context pat_expr_list in
	      fmkcexpr (CETypeswitch (e1', pat_cexpr_list)) eh fi
	  | EUnaryOp (uop, e1) ->
	      let ce1 = normalize_expr_aux norm_context  e1 in 
	      normalize_unary_operator norm_context uop ce1 eh fi
	  | EUnordered elist -> 
	      let elist' = normalize_expr_list norm_context  elist in
	      let e' = map_to_sequence eh fi elist' in
	      fmkcexpr (CEUnordered e') eh fi
	  | EOrdered elist -> 
	      let elist' = normalize_expr_list norm_context  elist in
	      let e' = map_to_sequence eh fi elist' in
	      fmkcexpr (CEOrdered e') eh fi
	  | EComment c ->
	      let _ = Streaming_ops.check_valid_comment c in 
	      fmkcexpr (CEComment c) eh fi
	  | ECommentComputed elist ->
	      let elist' = normalize_expr_list norm_context  elist in
	      let e' = map_to_sequence eh fi elist' in
	      let e'' = item_seq_to_untyped norm_context e' eh fi in
	      fmkcexpr (CECommentComputed e'') eh fi
	  | ERoot ->
	      let frfname = fn_root in
	      let celist = [cexpr_fs_dot eh fi] in
	      normalize_function_application norm_context frfname celist e fi
	  | EInstanceOf (e,dt) ->
	      let e' = normalize_expr_aux norm_context  e in
	      let pat_cexpr_list =
		[ (fmkcpattern (CCase (normalize_sequencetype norm_context dt)) fi, None, build_core_true norm_context eh fi );
		  (fmkcpattern (CDefault) fi, None, build_core_false norm_context eh fi) ]
	      in
	      fmkcexpr (CETypeswitch (e', pat_cexpr_list)) eh fi
	  | ERange (e1,e2) -> (* Call op:to - Jerome *)
	      let frfname = op_to in
	      let elist = [e1;e2] in
	      let celist = normalize_expr_list norm_context  elist in
	      normalize_function_application norm_context frfname celist e fi

  	  (* let server nc1 implement nc2 at e1 return { e2 } 
             ==
             let fs:nc1 := e1 return 
               let server nc1 implement uri at fs:nc1 return e2 

	     let server nc1 implement nc2 at e1 return { e2 } 
             ==
             let server nc1 implement uri at e1 return e2/[nc1->uri] 
          *)
	  | ELetServerImplement (nc1, nc2, e1, e2) ->
	      let ce1 = normalize_expr_aux norm_context e1 in
              let (norm_context, uri, cinterface, ce1) = 
		extend_server_environment false norm_context nc1 nc2 (e,ce1) in

	      let norm_context, nc1v = resolve_variable_qname_register norm_context (fs_prefix, nc1) in
	      let nc1var = fmkcexpr (CEVar nc1v) eh fi in 
	      let ce2 = normalize_expr_aux norm_context e2 in

              (* Temporary hack until dynamic server environment is implemented *)
              (* let $fs_nc1 := ce1 return let server nc1 implement URI at $fs_nc1 return ce2 *)
	      let srv = fmkcexpr (CELetServerImplement (nc1, uri, nc1var, ce2)) eh fi in
	      fmkcexpr (CEFLWOR ([CELET (None, nc1v, ce1)],None,None,srv)) eh fi

	  | EExecute (async,nc1,e2) -> 
	      let (nc1v, uri) = Norm_util.check_server_implementation norm_context fi nc1 in
              let nc1var = fmkcexpr (CEVar nc1v) eh fi in 
	      let in_exec = get_in_execute_expr norm_context in 
	      set_in_execute_expr norm_context true; 
	      let ce2 = normalize_expr_aux norm_context e2 in
	      set_in_execute_expr norm_context in_exec;
	      fmkcexpr (CEExecute (async, nc1, uri, nc1var, ce2)) (Some e) fi

 	  (* for server NCName implement NCName box E *)
	  | EForServerClose (nc1, nc2, e1) ->
	      let dummy_expr = fmkcexpr (CEScalar(StringLiteral "dummy_uri")) (Some e) fi in
              let (norm_context, uri, cinterface, _) = 
		extend_server_environment false norm_context nc1 nc2 (e,dummy_expr) in

	      let norm_context, nc1v = resolve_variable_qname_register norm_context (fs_prefix, nc1) in

	      (* The closure/box constructor expression is
                 semantically (almost) a composition of the
                 let-server-implement and remote-execute expressions:
                 a new server prefix is introduced into the
                 environment and in nested remote-execute expressions
                 should be treated like all other expressions that are
                 shipped.  *)

	      let in_exec = get_in_execute_expr norm_context in 
	      set_in_execute_expr norm_context true; 
	      let ce1 = normalize_expr_aux norm_context e1 in
	      set_in_execute_expr norm_context in_exec;

              (* Temporary hack until dynamic server environment is implemented *)
              (* let $fs_nc1 := ce1 return let server nc1 implement URI at $fs_nc1 return ce2 *)
	      let srv = fmkcexpr (CEForServerClose (nc1, uri, ce1)) (Some e) fi in
	      fmkcexpr (CEFLWOR ([CELET (None, nc1v, dummy_expr)],None,None,srv)) eh fi

 	  (* eval closure E *)
	  | EEvalClosure (e1) ->
	      let ce1 = normalize_expr_aux norm_context e1 in
	      fmkcexpr (CEEvalClosure (ce1)) (Some e) fi

          (* Normalization of updates *)
	  | ECopy e ->
	      let ce = normalize_expr_aux norm_context  e in
	      (fmkcexpr (CECopy ce) eh fi)
	  | ESnap (sm,e) ->
	      let ce = normalize_expr_aux norm_context  e in
	      (fmkcexpr (CESnap (sm, ce)) eh fi)
	  | EDelete (NoSnap,e) ->
	      let ce = normalize_expr_aux norm_context  e in
	      let cdelete = (fmkcexpr (CEDelete ce) eh fi) in
              if Conf.is_xqueryp() then 
		fmkcexpr (CESnap (Snap_Nondeterministic, cdelete)) eh fi
              else 
		cdelete
	  | EDelete (Snap,e) ->
	      let ce = normalize_expr_aux norm_context  e in
	      let cdelete = (fmkcexpr (CEDelete ce) eh fi) in
	      fmkcexpr (CESnap (Snap_Nondeterministic, cdelete)) eh fi
	  | EInsert (NoSnap,e,il) ->
	      let ce = normalize_expr_aux norm_context  e in
	              (* 
		         [[ insert { ce } into { il } ]]
			 -----------------------------
		         insert { copy { [[ ce ]] } } into { [[ il ]] } 
	              *)
	      let ce = fmkcexpr (CECopy ce) eh fi in 
	      let cil = normalize_insert_location norm_context il in
	      let cinsert = (fmkcexpr (CEInsert (ce, cil)) eh fi) in
              if Conf.is_xqueryp() then 
		        fmkcexpr (CESnap (Snap_Nondeterministic, cinsert)) eh fi
              else
		        cinsert
	  | EInsert (Snap,e,il) ->
	      (* There doesn't seem to be a need for an explicit copy
	         here - Chris *)	    
	      let ce = normalize_expr_aux norm_context  e in
	      let ce = fmkcexpr (CECopy ce) eh fi in 
	      let cil = normalize_insert_location norm_context il in
	      let cinsert = (fmkcexpr (CEInsert (ce, cil)) eh fi) in
	        fmkcexpr (CESnap (Snap_Nondeterministic, cinsert)) eh fi
	  | ERename (NoSnap,e1,e2) ->
	      let ce1 = normalize_expr_aux norm_context  e1 in
	      let ce2 = normalize_expr_aux norm_context  e2 in
              let crename = fmkcexpr (CERename (nsenv,ce1,ce2)) eh fi in
              if Conf.is_xqueryp() then
		fmkcexpr (CESnap (Snap_Nondeterministic, crename)) eh fi
              else
		crename
	  | ERename (Snap,e1,e2) ->
	      let ce1 = normalize_expr_aux norm_context  e1 in
	      let ce2 = normalize_expr_aux norm_context  e2 in
	      let crename = (fmkcexpr (CERename (nsenv,ce1,ce2)) eh fi) in
	      fmkcexpr (CESnap (Snap_Nondeterministic, crename)) eh fi
	  | EReplace (NoSnap,vo,e1,e2) ->        
	      let ce1 = normalize_expr_aux norm_context  e1 in
              begin
		match vo with
		| Normal_Replace ->
	                      (* 
		                 [[ replace { ce1 } with { ce2 } ]]
				 -----------------------------
		                 replace { [[ ce1 ]] }  with { copy { [[ ce2 ]] } } 
	                      *)
	            let ce2 = normalize_expr_aux norm_context  e2 in
	            let copy_of_2 = fmkcexpr (CECopy ce2) eh fi in 
	            let creplace = (fmkcexpr (CEReplace (vo,ce1,copy_of_2)) eh fi) in
                    if Conf.is_xqueryp() then
                      fmkcexpr (CESnap (Snap_Nondeterministic, creplace)) eh fi
                    else
	              creplace                
		| Value_Of_Replace ->
	                      (* 
		                 [[ replace { ce1 } with { ce2 } ]]
				 -----------------------------
		                 replace { [[ ce1 ]] }  with { text { [[ ce2 ]] } } 
	                      *)
                    let ce2 = normalize_expr_aux norm_context  
			(mkexpr (ETextComputed [mkexpr (EEnclosed (mkexpr (EList [e2])))])) in
	            let creplace = (fmkcexpr (CEReplace (vo,ce1,ce2)) eh fi) in
                    if Conf.is_xqueryp() then
	              fmkcexpr (CESnap (Snap_Nondeterministic, creplace)) eh fi
                    else
                      creplace
              end
	  | EReplace (Snap,vo,e1,e2) ->
	      let ce1 = normalize_expr_aux norm_context  e1 in
              begin
		match vo with
		| Normal_Replace ->
	            let ce2 = normalize_expr_aux norm_context  e2 in
	            let copy_of_2 = fmkcexpr (CECopy ce2) eh fi in 
	            let creplace = (fmkcexpr (CEReplace (vo,ce1,copy_of_2)) eh fi) in
	            fmkcexpr (CESnap (Snap_Nondeterministic, creplace)) eh fi
		| Value_Of_Replace ->
                    let ce2 = normalize_expr_aux norm_context  
			(mkexpr (ETextComputed [mkexpr (EEnclosed (mkexpr (EList [e2])))])) in
	            let creplace = (fmkcexpr (CEReplace (vo,ce1,ce2)) eh fi) in
	            fmkcexpr (CESnap (Snap_Nondeterministic, creplace)) eh fi
              end
	  | ERevalidate (snap,vmode,e) ->
	      raise (Query (Prototype "[revalidate] expression not supported yet"))
          | ETransform (cl_list, modifexpr, retexpr) ->          
                (*
                   transform copy $v1 := exp1, $v2 := exp2,...
                   modify expr_modif 
                   return expr_ret
                   -->
                   let $v1 := copy {exp1}, $v2 := copy {exp2}, ...
                   return
                   ( snap {expr_modif}, expr_ret )

                *)
              let norm_context = 
		List.fold_left (fun nc cp -> 
                  let v,_ = cp.pcopyvar_desc in
                  let nc',_ = resolve_variable_qname_register nc v
                  in nc') 
                  norm_context cl_list
              in
              let m1 = normalize_expr_aux norm_context  modifexpr in
              let r1 = normalize_expr_aux norm_context  retexpr in 
              let modifs =  fmkcexpr (CESnap (Snap_Nondeterministic, m1)) (Some modifexpr) modifexpr.pexpr_loc in
              let newReturn =  fmkcexpr (CESeq (modifs, r1)) (Some retexpr) modifexpr.pexpr_loc in
              fmkcexpr (CEFLWOR(letexpr_list_of_copyexpr_list cl_list norm_context  eh fi, None, None, newReturn)) eh fi           
          | EWhile (test, body) ->
	      let test' = normalize_expr_aux norm_context test in
	      let body' = normalize_expr_aux norm_context body in
	      normalize_to_while norm_context test' body' eh fi
          | ELetvar (odt,v,x,r) -> 
              let cbind = normalize_expr_aux norm_context x in
	      let (norm_context,cvar) = resolve_variable_qname_register norm_context v in
              let cret = normalize_expr_aux norm_context r in
              let rodt = normalize_optional_sequencetype norm_context odt in
              fmkcexpr (CELetvar (rodt,cvar,cbind,cret)) eh fi
          | ESet(v,x) ->  
	      let (cvar, _) = resolve_variable_qname_check norm_context fi v in
              let cx = normalize_expr_aux norm_context x in
              fmkcexpr (CESet (cvar,cx)) eh fi
          | EBlock(decls, elist) ->
              normalize_block norm_context eh fi decls elist 
	end
      in
      begin
	cexpr_result.pcexpr_origin <- Some e;
	cexpr_result
      end
    with
    | exn -> raise (error_with_file_location fi exn)


(***************************************)
(* helper functions for normalize_expr *)
(***************************************)

  and letexpr_list_of_copyexpr_list cl_list norm_context  eh fi =
    let nsenv = nsenv_from_norm_context norm_context in 
    List.map 
      (fun cp -> match cp.pcopyvar_desc with
      | var,expr -> 
          let cvname = resolve_variable_qname nsenv var in          
          let ce = (normalize_expr_aux norm_context  expr) in
          (CELET(None, cvname, fmkcexpr (CECopy ce) eh fi))) 
      cl_list
      
  and normalize_fl_clause norm_context flwor e  =
    match e.pfl_desc with
    | ELet (odt, vname, e') ->
        let ce = (normalize_expr_aux norm_context e') in
        let norm_context,cvname = resolve_variable_qname_register norm_context vname in
        let rodt = normalize_optional_sequencetype norm_context odt in
        (norm_context,CELET (rodt, cvname, ce))
    | EFor (odt, vname, ovn, e') ->
        let ce = (normalize_expr_aux norm_context e') in
        let norm_context,cvname = resolve_variable_qname_register norm_context vname in
        let (norm_context,covn) =
	  match ovn with
	  | None -> (norm_context,None)
	  | Some vname2 ->
	      let norm_context,cvname2 = resolve_variable_qname_register  norm_context vname2 in
	      if (rqname_equal cvname cvname2)
	      then
	        raise (Query (Parameter_Mismatch "Variable in for and at clauses have the same name"));
	      (norm_context,Some cvname2)
        in
        let rodt = normalize_optional_sequencetype norm_context odt in
        (norm_context,CEFOR (rodt, cvname, covn, ce))

  and normalize_fl_clauses norm_context flwor es = 
    match es with
    | [] -> (norm_context,[])
    | e :: es' ->
        let (norm_context,ce') = normalize_fl_clause norm_context flwor e in
        let (norm_context,ces') = (normalize_fl_clauses norm_context flwor es') in
        (norm_context,ce'::ces')

  and normalize_where_clause norm_context eo fi =
    match eo with
    | None -> None
    | Some e ->
        let ce = normalize_expr_aux norm_context  e in
        let ce' = normalize_effective_boolean_value norm_context ce (Some e) fi in
        Some ce'

  and normalize_order_by_clause norm_context order_by_clause fi =
    match order_by_clause with
    | None -> None
    | Some (stablekind,order_spec_list) ->
        let apply_one_order_spec (e,sortkind,emptysortkind) =
	  let ce = normalize_expr_aux norm_context  e in
	  let ckey = normalize_atomize norm_context ce (Some e) fi in
	  match emptysortkind with
	  | None ->
	      let mod_proc_ctxt = module_context_from_norm_context norm_context in
	      let emptysortkind = mod_proc_ctxt.Processing_context.default_order_kind in
	      (ckey,sortkind,emptysortkind)
	  | Some emptysortkind ->
	      (ckey,sortkind,emptysortkind)
        in
        let corder_spec_list = List.map apply_one_order_spec order_spec_list in
          (* Keeping track of the signature of op:gt, used in the semantics of order-by *)
        let osig = Norm_overloaded.table_for_op_gt norm_context in
        Some (stablekind, corder_spec_list, osig)

  and normalize_expr_list norm_context  es = 
    List.map (normalize_expr_aux norm_context ) es

  and normalize_enclosed_expr_list norm_context es =
    let normalize_enclosed_expr norm_context e =
      match e.pexpr_desc with 
      | EEnclosed e' ->
	  let ce' = normalize_expr_aux norm_context e' in
	  let eh' = ce'.pcexpr_origin in
	  let fi' = ce'.pcexpr_loc in
	  item_seq_to_untyped norm_context ce' eh' fi'
      | _ ->
	  normalize_expr_aux norm_context e
    in
    List.map (normalize_enclosed_expr norm_context) es

(*************************)
(* Path Expressions      *)
(*************************)

  and normalize_path_expr norm_context e p  =
    let nsenv = nsenv_from_norm_context norm_context in 
    let fi   = e.pexpr_loc in
    let eh = Some e in 
    let seqvar  = cexpr_fs_sequence eh fi in
    match p with
        (* 
           [StepExpr1 "/" StepExpr2]_Expr
           ==
           fs:distinct-docorder-or-atomic-sequence(
           let $fs:sequence node()* := [StepExpr1]_Expr
           let $fs:last := fn:count($fs:sequence)
           for $fs:dot at $fs:position in $fs:sequence
           [StepExpr2]_Expr

           Mary: I'm using this alternative because type-assertions on
           let-bound variables are currently broken:

           fs:distinct-docorder-or-atomic-sequence(
           let $fs:sequence := fs:node-sequence([StepExpr1]_Expr)
           let $fs:last := fn:count($fs:sequence)
           for $fs:dot at $fs:position in $fs:sequence
           [StepExpr2]_Expr
           )
        *)
    | PSlash (e1, e2) ->
        let ce1 = normalize_expr_aux norm_context e1 in
        let ce2 = normalize_expr_aux norm_context e2 in
        let count_expr = build_core_call norm_context fn_count [seqvar] eh fi in 
        let ce1' = build_core_call norm_context fs_node_sequence [ce1] eh fi in 
        let letc1 = CELET(None, fs_sequence, ce1') in
        let letc2 = CELET(None, fs_last, count_expr) in
        let forc1 = CEFOR(None, fs_dot, Some fs_position, seqvar) in 
        let cfl_expr_list = [letc1; letc2; forc1] in 
        let flwr_expr = fmkcexpr (CEFLWOR (cfl_expr_list,None,None,ce2)) eh fi in
        let sbdo_call_cexpr =
	  build_core_call norm_context fs_distinct_docorder_or_atomic_sequence [flwr_expr] eh fi
        in sbdo_call_cexpr

      (*
         [ StepExpr1 // StepExpr2 ]Expr
         ==
         [(StepExpr1 / descendant-or-self::node())/ StepExpr2]Expr
      *)
    | PSlashSlash (e1, e2) ->
        let axis = fmkexpr (EPath(PAxis(Descendant_or_self,PNodeKindTest AnyKind))) fi in
        let e' = fmkexpr (EPath(PSlash(e1, axis))) fi in
        normalize_path_expr norm_context e (PSlash(e', e2))
    | PAxis (a, nt) ->
        let ce = 
	  begin
	    match nt with 
	    | PNodeKindTest nk -> 
	        let cnk = normalize_kind_test norm_context nk in
	        if (forward_axis a) 
	        then CEForwardAxis(fs_dot, a, CPNodeKindTest cnk) 
	        else CEReverseAxis(fs_dot, a, CPNodeKindTest cnk)
	    | PNameTest qname ->
	                (* J&M : resolve_attr_symbol & resolve_elem_symbol have side effects: 
			   For the empty namespace, they update the global namespace table. 
			   Revisit this code when we clean up namespace resolution *)
	        let rqname =
		  if (a = Attribute) then 
		    resolve_attribute_qname nsenv qname
		  else 
		    resolve_element_qname nsenv qname
	        in
	        if (forward_axis a) 
	        then CEForwardAxis(fs_dot, a, CPNameTest rqname) 
	        else CEReverseAxis(fs_dot, a, CPNameTest rqname)
	  end 
        in
        let path_cexpr = fmkcexpr ce eh fi in
	path_cexpr

    | PStepQualifiers (e, sqs) ->
        let ce = normalize_expr_aux norm_context e in
        List.fold_left (normalize_step_qualifier ce norm_context) ce sqs

(*

   Step qualifiers can be applied to three types of step expressions:
   forward axes, reverse axes, and primary expressions.

*)

  and normalize_step_qualifier ce norm_context qual_expr sq = 
    match sq.pstep_qualifier_desc with 
    | PredicateQualifier e -> 
	let fi = e.pexpr_loc in 
	let eh = (Some e) in
	let dotvar = cexpr_fs_dot eh fi in
	let seqvar = cexpr_fs_sequence eh fi in
	let posvar = cexpr_fs_position eh fi in 
	let lastvar = cexpr_fs_last eh fi in 
	let cond = normalize_expr_aux norm_context e in
          (* 
             Norm_util.get_predicate_kind determines whether predicate is
	     a numeric literal (1 or another other literal), last(), or
	     some other predicate.
          *)
	let predkind = get_predicate_kind cond in
	let cond' = normalize_predicate_truth_value norm_context cond posvar eh fi in 
	let count_expr = build_core_call norm_context fn_count [seqvar] eh fi in
	let one_int_expr =
	  fmkcexpr (CEScalar (IntegerLiteral Decimal._integer_one)) eh fi
	in
	let step_expr =
	  begin
            (* Determine whether StepExpr is a forward or reverse axis or
               a primary expression *)
	    match ce.pcexpr_desc with
	    | CEForwardAxis _ -> 
		begin
		  match predkind with
		                (*  ForwardAxis ::= child
		                   | descendant
		                   | attribute
		                   | self
		                   | descendant-or-self 
				   | following-sibling
		                   | following
				   | namespace

		                   [ForwardStep Predicates "[" 1 "]"]Expr
		                   ==
		                   let $fs:sequence :=
		                   fs:distinct-doc-order( [ForwardStep Predicates]Expr )
				   return fn:first($fs:sequence)
		                *)
		  | First -> 
		      let first_expr = build_core_call norm_context fs_first [seqvar] eh fi in 
		      let docorder_expr = build_core_call norm_context fs_distinct_docorder [qual_expr] eh fi in
		      let letc = (CELET(None, fs_sequence, docorder_expr)) in
		      let flwr_expr = fmkcexpr (CEFLWOR ([letc],None,None, first_expr)) eh fi in
		      flwr_expr

		  | Last ->
		                  (*
		                     [ForwardStep Predicates "[" fn:last() "]"]Expr
		                     ==
		                     let $fs:sequence := fs:distinct-doc-order( [ForwardStep Predicates]Expr ) 
		                     return fs:last($fs:sequence)
		                  *)
		      let first_expr = build_core_call norm_context fs_last_fn [seqvar] eh fi in 
		      let docorder_expr = build_core_call norm_context fs_distinct_docorder [qual_expr] eh fi in
		      let letc = (CELET(None, fs_sequence, docorder_expr)) in
		      let flwr_expr = fmkcexpr (CEFLWOR ([letc],None,None, first_expr)) eh fi in
		      flwr_expr

		  | Numeric ->
		                  (*
		                     [ForwardStep Predicates "[" Numeric "]"]Expr
		                     ==
		                     let $fs:sequence := fs:distinct-doc-order( [ForwardStep Predicates]Expr ) 
		                     return fn:subsequence($fs:sequence,Numeric,1)
		                  *)
 		      let subseq_expr = build_core_call norm_context fn_subsequence [seqvar; cond; one_int_expr] eh fi in 
		      let docorder_expr = build_core_call norm_context fs_distinct_docorder [qual_expr] eh fi in
		      let letc = (CELET(None, fs_sequence, docorder_expr)) in
		      let flwr_expr = fmkcexpr (CEFLWOR ([letc],None,None, subseq_expr)) eh fi in
		      flwr_expr

		  | Other ->
		                  (*
		                     NB: We do not apply fs:apply-ordering-mode after fs:distinct-doc-order
		                     
		                     [ForwardStep PredicateList "[" Expr "]"]Expr
		                     ==
		                     let $fs:sequence := fs:distinct-docorder([ForwardStep PredicateList]_Expr)
		                     let $fs:last := fn:count($fs:sequence)
		                     for $fs:dot at $fs:position in $fs:sequence
		                     where ([Expr]_Predicates)
		                     return $fs:dot
		                  *)
		      begin
			let docorder_expr = build_core_call norm_context fs_distinct_docorder [qual_expr] eh fi in 
			let letc1 = CELET(None, fs_sequence, docorder_expr) in
			let letc2 = CELET(None, fs_last, count_expr) in
			let forc1 = CEFOR(None, fs_dot, Some fs_position, seqvar) in 
			let cfl_expr_list = [letc1; letc2; forc1] in 
			let flwr_expr = fmkcexpr (CEFLWOR (cfl_expr_list,Some cond',None,dotvar)) eh fi in
			flwr_expr
		      end
		end
	    | CEReverseAxis _ -> 
		begin
		  match predkind with
		  | First ->
                          (* ReverseAxis    ::= parent | ancestor | preceding-sibling | preceding | ancestor-or-self
		             [ReverseStep Predicates "[" 1 "]"]Expr
		             ==
		             let $fs:sequence := fs:distinct-doc-order( [ReverseStep Predicates]Expr ) 
		             return fn:last($fs:sequence,fn:count($fs:sequence))
		          *)
		      let last_expr = build_core_call norm_context fs_last_fn [seqvar] eh fi in 
		      let docorder_expr = build_core_call norm_context fs_distinct_docorder [qual_expr] eh fi in 
		      let letc1 = CELET(None, fs_sequence, docorder_expr) in 
		      let flwr_expr = fmkcexpr (CEFLWOR ([ letc1 ], None, None,last_expr)) eh fi in
		      flwr_expr

		  | Last -> 
                          (*
		             [ReverseStep Predicates "[" fn:last() "]"]Expr
		             ==
		             let $fs:sequence := fs:distinct-doc-order( [ReverseStep Predicates]Expr )
		             return fn:first($fs:sequence)
		          *)
                      let first_expr = build_core_call norm_context fs_first [seqvar] eh fi in 
                      let docorder_expr = build_core_call norm_context fs_distinct_docorder [qual_expr] eh fi in 
		      let letc = (CELET(None, fs_sequence, docorder_expr)) in
		      let flwr_expr = fmkcexpr (CEFLWOR ([letc],None,None, first_expr)) eh fi in
                      flwr_expr
			
	          | Numeric -> 
	                      (*
		                 [ReverseStep Predicates "[" Numeric "]"]Expr
		                 ==
		                 let $fs:sequence := fs:distinct-doc-order( [ReverseStep Predicates]Expr ) 
		                 let $fs:last := fn:count($fs:sequence) 
		                 let $fs:position := $fs:last - Numeric + 1 
		                 return fn:subsequence($fs:sequence,$fs:position,1)
		                 
		              *)
 		      let subseq_expr = build_core_call norm_context fn_subsequence [seqvar; posvar; one_int_expr] eh fi in 
		      let arith_expr = build_core_call norm_context op_integer_subtract [lastvar; cond] eh fi in
		      let arith_expr1 = build_core_call norm_context op_integer_add [arith_expr; one_int_expr] eh fi in
		      let docorder_expr = build_core_call norm_context fs_distinct_docorder [qual_expr] eh fi in 
		      let letc1 = CELET(None, fs_sequence, docorder_expr) in 
		      let letc2 = CELET(None, fs_last, count_expr) in 
		      let letc3 = CELET(None, fs_position, arith_expr1) in 
                      let flwr_expr = fmkcexpr (CEFLWOR ([letc1;letc2;letc3],None,None, subseq_expr)) eh fi in
		      flwr_expr

	          | Other ->
                          (*
		             [ReverseStep Predicates "[" Expr "]"]Expr
		             ==
		             let $fs:sequence := fs:distinct-doc-order( [ReverseStep Predicates]Expr )
		             let $fs:last := fn:count($fs:sequence) 
		             for $fs:dot at $fs:new in $fs:sequence 
		             let $fs:position := $fs:last - $fs:new + 1
		             where [Expr]Predicates 
		             return $fs:dot
		          *)
   		      begin
			let (v, var) = gen_new_cvar norm_context eh fi in 
			let arith_expr = build_core_call norm_context op_integer_subtract [lastvar; var] eh fi in
			let arith_expr1 = build_core_call norm_context op_integer_add [arith_expr; one_int_expr] eh fi in
			
			let docorder_expr = build_core_call norm_context fs_distinct_docorder [qual_expr] eh fi in 
			let letc1 = CELET(None, fs_sequence, docorder_expr) in 
			let letc2 = CELET(None, fs_last, count_expr) in 
			let forc1 = CEFOR(None, fs_dot, Some v, seqvar) in 
			let letc3 = CELET(None, fs_position, arith_expr1) in 
			let flwr_expr = fmkcexpr (CEFLWOR ([letc1;letc2;forc1;letc3],Some cond',None, dotvar)) eh fi in
			flwr_expr
		      end
		end
	    | _ ->
		begin
		  match predkind with
                        (* Philippe - fixme:
                           We never thought of this before, I think,
                           but expressions like the ones herebelow
                           return a result with at most one
                           node. Subsequent step expressions can be
                           processed with a new automaton ...  So,
                           even though we do not support these
                           expressions in general, we could handle
                           them here.
                        *)
                        (*  
                           [PrimaryExpr Predicates "[" 1 "]"]Expr
                           ==
                           fn:first([PrimaryExpr Predicates]Expr)
	                *)
	          | First -> 
		      build_core_call norm_context fs_first [qual_expr] eh fi
	          | Last ->
                          (*
		             [PrimaryExpr Predicates "[" fn:last() "]"]Expr
		             ==
		             let $fs:sequence := [PrimaryExpr Predicates]Expr return
		             fs:last($fs:sequence)
		          *)
		      let first_expr = build_core_call norm_context fs_last_fn [seqvar] eh fi in 
		      let letc = CELET(None, fs_sequence, qual_expr) in 
		      fmkcexpr (CEFLWOR ([letc], None, None, first_expr)) eh fi
	          | Numeric ->
                          (*
		             [PrimaryExpr Predicates "[" Numeric "]"]Expr
		             ==
		             fn:subsequence([PrimaryExpr Predicates]Expr,Numeric,1)
		          *)
 		      build_core_call norm_context fn_subsequence [qual_expr; cond; one_int_expr] eh fi 
	          | Other ->
                          (*  
		             [PrimaryExpr Predicates "[" Expr "]"]Expr   
		             == 
		             let $fs:sequence := [PrimaryExpr Predicates]Expr
		             let $fs:last := fn:count($fs:sequence)
		             for $fs:dot at $fs:position in $fs:sequence 
  		             where [Expr]Predicates 
		             return $fs:dot
		          *)
		      begin
			let letc1 = CELET(None, fs_sequence, qual_expr) in
			let letc2 = CELET(None, fs_last, count_expr) in
    			let forc = CEFOR(None, fs_dot, Some fs_position, seqvar) in
			fmkcexpr (CEFLWOR ([letc1; letc2; forc], Some cond', None, dotvar)) eh fi
		      end
		end
	  end
	in step_expr

  and normalize_insert_location norm_context il =
    match il with
    | EAsLastInto e ->
        let ce = normalize_expr_aux norm_context e in
        (CUAsLastInto ce)
    | EAsFirstInto e ->
        let ce = normalize_expr_aux norm_context e in
        (CUAsFirstInto ce)
    | EInto e ->
        let ce = normalize_expr_aux norm_context e in
        (CUInto ce)
    | EAfter e ->
        let ce = normalize_expr_aux norm_context e in
        (CUAfter ce)
    | EBefore e ->
        let ce = normalize_expr_aux norm_context e in
        (CUBefore ce)

(** Normalize XQueryP blocks into letvars and sequences *)
  and normalize_block norm_context eh fi decls elist =
    match decls with 
    | [] -> 
        let elist' = normalize_expr_list norm_context  elist in
        map_to_imperative_sequence eh fi elist'                     
    | d::rest -> 
        begin
          match d.bl_decl_desc with 
          | (odt,v,x) ->
	      let norm_context,cvar = resolve_variable_qname_register norm_context v in
              let cbind = normalize_expr_aux norm_context x in
              let rodt = normalize_optional_sequencetype norm_context odt in
              let cret = normalize_block norm_context eh fi rest elist in
              fmkcexpr (CELetvar (rodt,cvar,cbind,cret)) eh d.bl_decl_loc
        end

  in (!cfunction_defs, normalize_expr_aux norm_context e)
(*******************************)
(* End of normalize_expr *)     
(*******************************)



