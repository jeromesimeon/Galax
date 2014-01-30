(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: norm_ident_expr.ml,v 1.42 2007/11/16 21:16:52 mff Exp $ *)

(* Module: Norm_ident_expr
   Description:
     This module implements an identity normalization for XQuery
     expressions which are already in the core.
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

(* Note:
     Several 'special' functions are mapped to core expressions or
     special variables.
   - Jerome *)

(*******************************)
(* Begin of normalize_ident_expr *)     
(*******************************)
let normalize_ident_expr norm_context e =

  let rec normalize_ident_function_application norm_context rfname celist e fi =
  (* Gets the prefix, uri and local name of the function *)
    let (prefix,uri,lname) = rfname in
    let cempty = fmkcexpr CEEmpty None fi in
    let arity = List.length celist in

  (* Variadic or special functions in the fn: namespace  *)
    if (uri = fn_uri) then
      begin
      (* fn:error() *)
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
        (* Note: fn:concat is variadic; convert to binary applications of fn:concat here. *)
	  let fn_concat ce1 ce2 = normalize_ident_function_app norm_context fn_concat [ce1; ce2] (Some e) fi in
	  List.fold_left fn_concat (fmkcexpr (CEScalar (StringLiteral "")) (Some e) fi) (celist) 

      (* fn:resolve-uri() *)
	else if (rqname_equal rfname fn_resolve_uri && arity = 1) then
	  let mod_proc_ctxt = module_context_from_norm_context norm_context in
	  let base_uri  = Processing_context.get_base_uri mod_proc_ctxt in
	  match base_uri with
	  | None -> raise (Query (Parameter_Mismatch "[err:FONS0005]"))
	  | Some base_uri ->
	      let base_uri_item = fmkcexpr (CEScalar (StringLiteral (AnyURI._string_of_uri base_uri))) (Some e) fi in
	      build_core_call norm_context fn_resolve_uri (base_uri_item :: celist) (Some e) fi

	else
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
		  (* fn:lang($textlang) is normalized to fn:name($dot,$textlang) *)
	      (cexpr_fs_dot (Some e) fi) :: celist
	    else if (rqname_equal rfname fn_normalize_unicode && arity = 1) then
		  (* fn:normalize-unicode($s) is normalized to fn:normalized-unicode($s,"NFC") *)
	      (List.hd celist) :: (fmkcexpr (CEScalar (StringLiteral "NFC")) (Some e) fi) :: []
	    else if (rqname_equal rfname fn_local_name && arity = 0) then
		  (* fn:local-name() is normalized to fn:local-name($dot) *)
	      (cexpr_fs_dot (Some e) fi ) :: []

	    else if (rqname_equal rfname fn_number && arity = 0) then
	          (* fn:number() is normalized to fn:number($dot) *)
	      (cexpr_fs_dot (Some e) fi ) :: []

  	    else if ((rqname_equal rfname fn_sum || rqname_equal rfname fn_sum_double || 
            rqname_equal rfname fn_sum_float  || rqname_equal rfname fn_sum_decimal || 
            rqname_equal rfname fn_sum_integer)  && arity = 1) then
		  (* fn:sum(Expr) is normalized to fn:sum(Expr,0.0E0) *)
	      (List.hd celist) :: (fmkcexpr (CEScalar(IntegerLiteral Decimal._integer_zero)) (Some e) fi):: []
            else if (rqname_equal rfname fn_namespace_uri && arity = 0) then
		  (* fn:namespace-uri() is normalized to fn:namespace-uri($dot) *)
	      (cexpr_fs_dot (Some e) fi) :: []
	    else if (rqname_equal rfname fn_substring && arity = 2) then
	    (* Pass 1000000 as the length of the substring because I'm not sure how to get at the data right now. Change later *)
	      celist @ (fmkcexpr (CEScalar(DoubleLiteral (double_of_untyped("1000000.0")))) (Some e) fi):: []
	    else if ((rqname_equal rfname fn_matches || rqname_equal rfname fn_tokenize) && arity = 2) then
	      celist @ (fmkcexpr (CEScalar(StringLiteral "")) (Some e) fi) :: []
	    else if (rqname_equal rfname fn_replace && arity = 3) then
	      celist @ (fmkcexpr (CEScalar(StringLiteral "")) (Some e) fi) :: []
	    else if ((rqname_equal rfname fn_adjust_time_to_timezone 
		    || rqname_equal rfname fn_adjust_date_to_timezone
		    || rqname_equal rfname fn_adjust_dateTime_to_timezone)
		       && arity = 1) then
	      celist @ [cempty]
	    else
	      celist
	  in normalize_ident_function_app norm_context rfname celist' (Some e) fi
      end
 (* Constructor function in the xs:, or xsd: namespace *)
    else if ((uri = xs_uri) || (uri = xsd_uri))
    then
      begin
	if (List.length celist) != 1 then
	  raise (incorrect_arg_count rfname (List.length celist) 1)
	else
	  let atomicty = (fmkcsequencetype (CITAtomic rfname, None) fi, Schema_builtin.cxtype_atomic) in
	  build_core_cast norm_context (List.nth celist 0) atomicty (Some e) fi
      end
    else
      normalize_ident_function_app norm_context rfname celist (Some e) fi

  and normalize_ident_eattfixed norm_ctxt caname nsenv celist eh fi = 
    build_core_attribute_constructor norm_ctxt caname nsenv celist eh fi

  and normalize_ident_eelemfixed norm_context cename nsenv cattlist cchildlist eh fi =
  (* 1. The namespace environment must be passed to the element constructor *)
    let nsenv = nsenv_from_norm_context norm_context in
  (* 2. Finally, the core element constructor is built *)
    build_core_element_constructor norm_context cename nsenv (cattlist @ cchildlist) eh fi

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

  and normalize_ident_pattern norm_context p = 
    match p.ppattern_desc with
    | Case dt ->
	fmkcpattern (CCase (normalize_sequencetype norm_context dt)) p.ppattern_loc
    | Default ->
	fmkcpattern CDefault p.ppattern_loc

  and normalize_ident_pat_expr norm_context pat_expr = 
    let nsenv = nsenv_from_norm_context norm_context in 
    let (p, ovn, expr) = pat_expr in
    let covn =
      match ovn with
      | None -> None
      | Some vname ->
	  Some (resolve_variable_qname nsenv vname)
    in
    let p' = normalize_ident_pattern norm_context p in 
    let ce = normalize_ident_expr_aux norm_context expr in
    (p', covn, ce)

(************************)
(* normalize_ident_expr_aux *)
(************************)

  and normalize_ident_expr_aux norm_context e =

    let desc = e.pexpr_desc
    and fi = e.pexpr_loc in
  (* The surface-syntax expression contains the file location that
     should be correlated with errors, so we catch any exceptions here
     and rewrap the exceptions with the file location. *) 
    try
      let nsenv = nsenv_from_norm_context norm_context in 
      let cxschema = cxschema_from_norm_context norm_context in 
      let cexpr_result =
	begin
	  match desc with
	  | EApp (fn, elist) ->
	      let frfname = resolve_function_qname nsenv fn in
	      let celist = normalize_ident_expr_list norm_context elist in
	      normalize_ident_function_application norm_context frfname celist e fi
	  | EAttrFixed (aname, elist) ->
	      let caname = resolve_attribute_qname nsenv aname in
	      let celist = normalize_ident_expr_list norm_context elist in
	      (* There is no way to change the namespace environment for
		 an computed attribute constructor - Jerome *)
	      normalize_ident_eattfixed norm_context caname nsenv celist (Some e) fi
	  | EAttrComputed (elist1, elist2) ->
	      let celist1 = normalize_ident_expr_list norm_context elist1 in
	      let ce1 = map_to_sequence (Some e) fi celist1 in
	      let celist2 = normalize_ident_expr_list norm_context elist2 in
	      let ce2 = map_to_sequence (Some e) fi celist2 in
	  (* There is no way to change the namespace environment for
	     an computed attribute constructor - Jerome *)
              let nsenv' = nsenv in
              fmkcexpr (CEAnyAttr (ce1, nsenv', ce2)) (Some e) fi
	  | ETreat (e, dt) ->
	      let ce = normalize_ident_expr_aux norm_context e in
	      let cdt = normalize_sequencetype norm_context dt in
	  (* Treat inherits its annotation from its argument expression *)
	      let annot = copy_annot ce.pcexpr_annot in
	      let treat_cexpr = fmkcexpr (CETreat (ce,cdt)) (Some e) fi in
	      begin
		set_annotation_for_cexpr treat_cexpr annot;
		treat_cexpr
	      end
	  | EValidate (vmode,e) ->
	      let ce = normalize_ident_expr_aux norm_context e in
	      let cvmode =
		match vmode with
		| None -> Strict
		| Some cvmode -> cvmode
	      in
	  (* Validate inherits its annotation from its argument expression *)
	      let annot = copy_annot ce.pcexpr_annot in
	      let validate_cexpr = fmkcexpr (CEValidate (cvmode,ce)) (Some e) fi in
	      begin
		set_annotation_for_cexpr validate_cexpr annot;
		validate_cexpr
	      end
	  | ECast (e, dt) ->
          (* Argument to cast can be a node sequence, which is mapped
             to a sequence of atomic values *)
	      let ce = normalize_ident_expr_aux norm_context e in
	      let cdt = normalize_sequencetype norm_context dt in 
	      build_core_cast norm_context ce cdt (Some e) fi
	  | ECastable (e, dt) ->
          (* Argument to castable can be a node sequence, which is
             mapped to a sequence of atomic values *)
	      let ce = normalize_ident_expr_aux norm_context e in
	      let cdt = normalize_sequencetype norm_context dt in 
	      build_core_castable norm_context ce cdt (Some e) fi
	  | EBinaryOp (e1, bop, e2) ->
	      raise (Query (Malformed_Core_Expr "Expression is not normalized (found 'binary operation')"))
	  | EElemFixed (ename, attlist, childlist) ->
	      let (new_nss, other_atts) = get_ns_attributes attlist in
	      let nsenv' = Namespace_context.add_all_ns nsenv new_nss in
	      let norm_context' = copy_norm_context_with_sigs norm_context nsenv' cxschema in
	      let cattlist =  normalize_ident_expr_list norm_context' other_atts in
	      let cchildlist = normalize_ident_expr_list norm_context' childlist in
	      let cename = resolve_element_qname nsenv' ename in
	      normalize_ident_eelemfixed norm_context' cename nsenv' cattlist cchildlist (Some e) fi
	  | EElemComputed (elist, childlist) ->
	      let celist = normalize_ident_expr_list norm_context elist in
	      let cchildlist = normalize_ident_expr_list norm_context childlist in
	  (* Currently, there is no way to change the namespace environment
	     for a computed element constructor - Jerome *)
              let nsenv1 = nsenv in
              let nsenv2 = nsenv in
	  (* Element name is cast to a QName during query evaluation
	     -- should probably happen during normalization *)
	      fmkcexpr (CEAnyElem ((map_to_sequence (Some e) fi celist), nsenv1, nsenv2, (map_to_sequence (Some e) fi cchildlist))) (Some e) fi 
	  | EPragma _ ->
	      raise (Query (Malformed_Core_Expr "Expression is not normalized (found pragma)"))
	  | EFLWOR (fl_clauses, where_clause, order_by_clause, return_clause) ->
	  (* Note:
	     As of 07/19/2004, FLWOR block are kept 'as is' in the
	     normalized core.
	     - Jerome
	  *)
	      let cfl_expr_list = normalize_ident_fl_clauses norm_context fl_clauses in
	      let cwhere_clause = normalize_ident_expr_opt norm_context where_clause in
	      let corder_by_clause = normalize_ident_order_by_clause norm_context order_by_clause fi in
	      let creturn_clause = normalize_ident_expr_aux norm_context return_clause in
	      fmkcexpr (CEFLWOR (cfl_expr_list,cwhere_clause,corder_by_clause,creturn_clause)) (Some e) fi
	  | EIf (cond, e1, e2) ->
	      let cond' = normalize_ident_expr_aux norm_context cond in
	      let e1' = normalize_ident_expr_aux norm_context e1 in
	      let e2' = normalize_ident_expr_aux norm_context e2 in
	      build_core_if norm_context cond' e1' e2' (Some e) fi
	  | EList elist ->
	      let elist' = normalize_ident_expr_list norm_context elist in
	      map_to_sequence (Some e) fi elist' 
	  | EEnclosed e ->
	      normalize_ident_expr_aux norm_context e
	  | EPath p -> 
	      normalize_ident_path_expr norm_context e p
	  | EScalar bv ->
	      fmkcexpr (CEScalar bv) (Some e) fi
	  | EPI (target,pi_content) ->
	      fmkcexpr (CEPI (target,pi_content)) (Some e) fi
	  | EPIComputed (elist1,elist2) ->
	      let celist1 = normalize_ident_expr_list norm_context elist1 in
	      let ce1 = map_to_sequence (Some e) fi celist1 in
	      let celist2 = normalize_ident_expr_list norm_context elist2 in
	      let ce2 = map_to_sequence (Some e) fi celist2 in
	      fmkcexpr (CEPIComputed (ce1,ce2)) (Some e) fi
	  | EText t ->
	      fmkcexpr (CEText t) (Some e) fi
	  | ECharRef cp ->
	      let t = Galax_camomile.utf8_string_of_code_point cp in
	      fmkcexpr (CEText t) (Some e) fi
	  | ETextComputed elist ->
	      let elist' = normalize_ident_expr_list norm_context elist in
	      let e' = map_to_sequence (Some e) fi elist' in
	      fmkcexpr (CETextComputed e') (Some e) fi
	  | EDocument elist ->
	      let elist' = normalize_ident_expr_list norm_context elist in
	      let e' = map_to_sequence (Some e) fi elist' in
	      fmkcexpr (CEDocument e') (Some e) fi
	  | ESelf ->
	      raise (Query (Malformed_Core_Expr "Expression is not normalized (found '.')"))
	  | EParent ->
	      raise (Query (Malformed_Core_Expr "Expression is not normalized (found '..')"))
	  | ESome (bdl, e2) ->
	      let e2' = normalize_ident_expr_aux norm_context e2 in
	      let rec normalize_ident_bindings bdl =
		match bdl with
		| [] -> e2'
		| (odt,vname,e1) :: obdl ->
		    let cvname = resolve_variable_qname nsenv vname in
		    let result_expr = normalize_ident_bindings obdl in
		    let e1' = normalize_ident_expr_aux norm_context e1 in 
		    let rodt = (normalize_optional_sequencetype norm_context odt) in
		    build_core_some norm_context rodt cvname e1' result_expr (Some e) fi
	      in
	      normalize_ident_bindings bdl
(* Version for some with a single binding - Jerome
   let e1' = normalize_ident_expr_aux norm_context e1 in 
   let e2' = normalize_ident_expr_aux norm_context e2 in 
   let rodt = (normalize_optional_sequencetype norm_context odt) in
   normalize_ident_to_some rodt v e1' e2' (Some e) fi *)
	  | EEvery (bdl, e2) ->
	      let e2' = normalize_ident_expr_aux norm_context e2 in 
	      let rec normalize_ident_bindings bdl =
		match bdl with
		| [] -> e2'
		| (odt,vname,e1) :: obdl ->
		    let cvname = resolve_variable_qname nsenv vname in
		    let result_expr = normalize_ident_bindings obdl in
		    let e1' = normalize_ident_expr_aux norm_context e1 in 
		    let rodt = (normalize_optional_sequencetype norm_context odt) in
		    build_core_every norm_context rodt cvname e1' result_expr (Some e) fi
	      in
	      normalize_ident_bindings bdl
	  | ETypeswitch (e1, pat_expr_list) ->
	      let e1' = normalize_ident_expr_aux norm_context e1 in
	      let pat_cexpr_list = List.map (normalize_ident_pat_expr norm_context) pat_expr_list in
	      fmkcexpr (CETypeswitch (e1', pat_cexpr_list)) (Some e) fi
	  | EUnaryOp (uop, e1) ->
	      raise (Query (Malformed_Core_Expr "Expression is not normalized (found unary operation)"))
	  | EUnordered elist -> 
	      let elist' = normalize_ident_expr_list norm_context elist in
	      let e' = map_to_sequence (Some e) fi elist' in
	      fmkcexpr (CEUnordered e') (Some e) fi
	  | EOrdered elist -> 
	      let elist' = normalize_ident_expr_list norm_context elist in
	      let e' = map_to_sequence (Some e) fi elist' in
	      fmkcexpr (CEOrdered e') (Some e) fi
	  | EVar vname ->
	      let cvname = resolve_variable_qname nsenv vname in
	      mkcvar cvname (Some e) fi
	  | EComment c ->
	      fmkcexpr (CEComment c) (Some e) fi
	  | ECommentComputed elist ->
	      let elist' = normalize_ident_expr_list norm_context elist in
	      let e' = map_to_sequence (Some e) fi elist' in
	      fmkcexpr (CECommentComputed e') (Some e) fi
	  | ERoot ->
	      raise (Query (Malformed_Core_Expr "Expression is not normalized (found root '/')"))
	  | EInstanceOf (e,dt) ->
	      raise (Query (Malformed_Core_Expr "Expression is not normalized (found 'instance of')"))
	  | ERange (e1,e2) ->
	      raise (Query (Malformed_Core_Expr "Expression is not normalized (found 'to')"))

	(* Normalization of DXQ *)
	  | ELetServerImplement (nc1,nc2,e1,e2) ->
	      let ce1 = normalize_ident_expr_aux norm_context e1 in
	      let ce2 = normalize_ident_expr_aux norm_context e2 in
	      (fmkcexpr (CELetServerImplement (nc1,nc2,ce1,ce2)) None fi)
	  | EExecute (async,ncname,e1) ->
	      raise (Query(Prototype("Execute not implemented")));
(*	      let ce2 = normalize_ident_expr_aux norm_context e2 in
	      (fmkcexpr (CEExecute (async, ncname,ce2)) None fi)*)
	  | EForServerClose (nc1,nc2,e1) ->
	      raise (Query(Prototype("for-server-box not implemented")));
(*	      let ce1 = normalize_ident_expr_aux norm_context e1 in
	      (fmkcexpr (CEForServerClose (nc1,ce1)) None fi) *)
	  | EEvalClosure (e1) ->
	      raise (Query(Prototype("eval-box not implemented")));
(*	      let ce1 = normalize_ident_expr_aux norm_context e1 in
	      (fmkcexpr (CEEvalClosure (ce1)) None fi) *)
	(* Normalization of updates *)
	  | ECopy e ->
	      let ce = normalize_ident_expr_aux norm_context e in
	      (fmkcexpr (CEDelete ce) None fi)
	  | ESnap (sm,expr) ->
	      let ce = normalize_ident_expr_aux norm_context expr in
	      (fmkcexpr (CESnap (sm,ce)) None fi)
	  | EDelete (NoSnap,e) ->
	      let ce = normalize_ident_expr_aux norm_context e in
	      let cdelete = (fmkcexpr (CEDelete ce) None fi) in
	      cdelete
	  | EDelete (Snap,_) ->
	      raise (Query (Malformed_Core_Expr "Expression is not normalized (found 'snap' update)"))
	  | EInsert (NoSnap,e,il) ->
	      let ce = normalize_ident_expr_aux norm_context e in
	      let cil = normalize_ident_insert_location norm_context il in
	      let cinsert = (fmkcexpr (CEInsert (ce, cil)) None fi) in
	      cinsert
	  | EInsert (Snap,e,il) ->
	      raise (Query (Malformed_Core_Expr "Expression is not normalized (found 'snap' update)"))
	  | ERename (NoSnap,e1,e2) ->
	      let ce1 = normalize_ident_expr_aux norm_context e1 in
	      let ce2 = normalize_ident_expr_aux norm_context e2 in
	      let crename = (fmkcexpr (CERename (nsenv,ce1,ce2)) None fi) in
	      crename
	  | ERename (Snap,e1,e2) ->
	      raise (Query (Malformed_Core_Expr "Expression is not normalized (found 'snap' update)"))
	  | EReplace (NoSnap,vo,e1,e2) ->
	      let ce1 = normalize_ident_expr_aux norm_context e1 in
	      let ce2 = normalize_ident_expr_aux norm_context e2 in
	      let creplace = (fmkcexpr (CEReplace (vo,ce1,ce2)) None fi) in
	      creplace
	  | EReplace (Snap,vo,e1,e2) ->
	      raise (Query (Malformed_Core_Expr "Expression is not normalized (found 'snap' update)"))

	  | ERevalidate (snap,vmode,e) ->
	      raise (Query (Prototype "[revalidate] expression not supported yet"))
		
	  | ETransform _ -> 
	      raise (Query (Malformed_Core_Expr "Expression is not normalized (found 'transform')"))

	  | EWhile (cond, e1) ->
	      let cond' = normalize_ident_expr_aux norm_context cond in
	      let e1' = normalize_ident_expr_aux norm_context e1 in
	      build_core_while norm_context cond' e1' (Some e) fi

	  | ELetvar (odt,v,b,r) ->
              let nsenv = nsenv_from_norm_context norm_context in 
	      let cvar = resolve_variable_qname nsenv v in
              let rodt = normalize_optional_sequencetype norm_context odt in
              let cbind = normalize_ident_expr_aux norm_context b in
              let cret = normalize_ident_expr_aux norm_context r in
              fmkcexpr (CELetvar (rodt,cvar,cbind,cret)) None fi

	  | ESet (v,x) ->
              let nsenv = nsenv_from_norm_context norm_context in 
	      let cvar = resolve_variable_qname nsenv v in
              let cx = normalize_ident_expr_aux norm_context x in
              fmkcexpr (CESet (cvar,cx)) None fi
	  | EBlock _ ->
	      raise (Query (Malformed_Core_Expr "Expression is not normalized (found XQueryP block)"))
	end
      in
      begin
	cexpr_result.pcexpr_origin <- Some e;
	cexpr_result
      end
    with
    | exn -> raise (error_with_file_location fi exn)

(***************************************)
(* helper functions for normalize_ident_expr_aux *)
(***************************************)

  and normalize_ident_fl_clause norm_context e  =
    let nsenv = nsenv_from_norm_context norm_context in 
    match e.pfl_desc with
    | ELet (odt, vname, e') ->
	let cvname = resolve_variable_qname nsenv vname in
	let ce = (normalize_ident_expr_aux norm_context e') in
	let rodt = normalize_optional_sequencetype norm_context odt in
	(CELET (rodt, cvname, ce))
    | EFor (odt, vname, ovn, e') ->
	let cvname = resolve_variable_qname nsenv vname in
	let covn =
	  match ovn with
	  | None -> None
	  | Some vname ->
	      Some (resolve_variable_qname nsenv vname)
	in
	let ce = (normalize_ident_expr_aux norm_context e') in
	let rodt = normalize_optional_sequencetype norm_context odt in
	(CEFOR (rodt, cvname, covn, ce))

  and normalize_ident_fl_clauses norm_context es = 
    let f e  cexprs = 
      let ce' =  normalize_ident_fl_clause norm_context e
      in ce' :: cexprs 
    in  List.fold_right f es [] 

  and normalize_ident_order_by_clause norm_context order_by_clause fi =
    match order_by_clause with
    | None -> None
    | Some (stablekind,order_spec_list) ->
	let apply_one_order_spec (e,sortkind,emptysortkind,col) =
	  (* Checks for proper collation *)
	  let mod_proc_ctxt = module_context_from_norm_context norm_context in
	  begin
	    match col with
	    | None -> ()
	    | Some col -> Processing_context.check_collation mod_proc_ctxt col
	  end;
	  let ce = normalize_ident_expr_aux norm_context e in
	  match emptysortkind with
	  | None ->
	      let emptysortkind = mod_proc_ctxt.Processing_context.default_order_kind in
	      (ce,sortkind,emptysortkind)
	  | Some emptysortkind ->
	      (ce,sortkind,emptysortkind)
	in
	let corder_spec_list = List.map apply_one_order_spec order_spec_list in
      (* Keeping track of the signature of op:gt, used in the semantics of order-by *)
	let osig = Norm_overloaded.table_for_op_gt norm_context in
	Some (stablekind, corder_spec_list, osig)

  and normalize_ident_expr_list norm_context es = 
    List.map (normalize_ident_expr_aux norm_context) es

  and normalize_ident_expr_opt norm_context eo =
    match eo with
    | None -> None
    | Some e -> Some (normalize_ident_expr_aux norm_context e)

(*************************)
(* Path Expressions      *)
(*************************)

  and normalize_ident_path_expr norm_context e p  =
    let nsenv = nsenv_from_norm_context norm_context in 
    let fi   = e.pexpr_loc in
    match p with
    | PSlash (e1, e2) ->
	raise (Query (Malformed_Core_Expr "Expression is not normalized (found '/')"))
    | PSlashSlash (e1, e2) ->
	raise (Query (Malformed_Core_Expr "Expression is not normalized (found '//')"))
    | PAxis (a, nt) ->
	let ce = 
	  begin
	    match nt with 
	    | PNodeKindTest nk -> 
		let cnk = normalize_kind_test norm_context nk in
		if (forward_axis a) then CEForwardAxis(fs_dot, a, CPNodeKindTest cnk) else CEReverseAxis(fs_dot, a, CPNodeKindTest cnk) 
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
		if (forward_axis a) then CEForwardAxis(fs_dot, a, CPNameTest rqname) else CEReverseAxis(fs_dot, a, CPNameTest rqname)
	  end 
	in
	let path_cexpr = fmkcexpr ce (Some e) fi in
	path_cexpr

    | PStepQualifiers (b, e, sqs) ->
	raise (Query (Malformed_Core_Expr "Expression is not normalized (found '[]')"))

  and normalize_ident_insert_location norm_context il =
    match il with
    | EAsLastInto e ->
	let ce = normalize_ident_expr_aux norm_context e in
	(CUAsLastInto ce)
    | EAsFirstInto e ->
	let ce = normalize_ident_expr_aux norm_context e in
	(CUAsFirstInto ce)
    | EInto e ->
	let ce = normalize_ident_expr_aux norm_context e in
	(CUInto ce)
    | EAfter e ->
	let ce = normalize_ident_expr_aux norm_context e in
	(CUAfter ce)
    | EBefore e ->
	let ce = normalize_ident_expr_aux norm_context e in
	(CUBefore ce)

  in ([], normalize_ident_expr_aux norm_context e)
(*******************************)
(* End of normalize_ident_expr *)     
(*******************************)
