(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_globals.ml,v 1.37 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Factorize_globals
   Description:
     This module takes those expressions which are global variables
     (for expressions) and makes them let bindings. This lets the
     compiler know that the dependence is lest restrictive.
*)

open Xquery_common_ast
open Xquery_core_ast
open Xquery_core_ast_util
open Xquery_core_ast_annotation
open Xquery_type_core_ast

(* This module rewrites core expressions 
   so that global variables are 
   expressed as top level CElet bindings. 

   Consider:
   let $x := expr1 return expr2

   if free_variables(expr1) subset global_variables then
   we can rewrite $x as a top level global.

   Given an entire statement the free variables(statement) are assumed global. 

   NOTE: If the query is not properly formed, this will result in strange looking
   queries. Specifically, if a query uses a variable that is not bound, it could
   be rewritten as a toplevel let binding. 

   NOTE:
   for $y in ..
   let $z := expr($y) is not global.

   COND: We assume the variables have already been annotated with free variables 
   
*)

(* Note on annotations:
   o The annotations are not correctly kept by this module.
   Change this so that free variables are correctly calculated.
   o We need a better way to deal with preserving annotations.
*)
exception FactorError of string

let get_free_variables x = 
  match get_free_var_annot x.pcexpr_annot with
    | Some fv -> fv
    | None ->
	let s = Print_top.bprintf_acexpr "" x in
	raise (FactorError ("Unannotated core expression: " ^ s))

let is_subset vl1 vl2 =
  List.for_all (fun x -> List.mem x vl2) vl1
(***********************************)  
(* The return environment.         *)
(* It contains a list of bindings  *)
(***********************************)

type global_return_env         = 
    (* Order in this list is important.
       if i > j => b_i does not depend on b_j 
    *)
    { bindings   : (cvname * ((csequencetype * cxtype) option * acexpr)) list ; }

let mk_global_return  bindings = { bindings = bindings }
				   				   
let combine_global_envs b1 b2  = { 
  bindings = Gmisc.remove_duplicates (b1.bindings @ b2.bindings) }

let empty_renv = mk_global_return [] 

(*******************************)
(* The passed down environment *)
(*******************************)
type global_passed_env = {
  stat_ctxt  : Typing_context.static_context;
  (* Order enforces scoping as usual *)
  rebindings : (cvname * cvname) list;
  generator  : Namespace_generate.name_gen ref;
  globals    : cvname list} (* This is really a set *)

let mk_global_passed_env sc rebindings globals =
  let proc_ctxt = Typing_context.processing_context_from_stat_context sc in
  let ng =
    Processing_context.get_name_generator
      proc_ctxt Namespace_builtin.fs_prefix Namespace_builtin.fs_uri ""
  in
  { stat_ctxt  = sc;
    rebindings = rebindings;
    generator  = ng;
    globals    = globals; }

let get_global_variables genv = genv.globals

let get_new_factored_variable genv vn =
  let ng = !(genv.generator) in
  let (_, _, ncname) = vn in
  Namespace_generate.generate_name_with_prefix ng ncname

(* Called to add a binding for a variable which is not global *)
let add_global_binding genv vn ((odt,ce) as bind) =
  (* Make sure there are no scoping problems *)
  let vn' = get_new_factored_variable genv vn in    
  let new_genv = 
    mk_global_passed_env 
      genv.stat_ctxt
      ((vn, vn') :: genv.rebindings)
      (vn :: genv.globals)
  in
  let new_renv = mk_global_return [(vn', bind)] in
    new_genv, new_renv
     
let remove_name_binding vn l = 
  List.filter
    (fun x -> not (Namespace_names.rqname_equal x vn)) 
    l

(* Called for each variable that is not global *)
let add_non_global_binding genv vn =
  mk_global_passed_env
    genv.stat_ctxt
    ((vn, vn) :: genv.rebindings)
    (remove_name_binding vn genv.globals)

let add_opt_non_global_binding genv ovn =
  match ovn with
    | None -> genv
    | Some vn -> add_non_global_binding genv vn



let get_rebound_variable genv vn =
  if (List.mem_assoc vn genv.rebindings)
  then List.assoc vn genv.rebindings      
  else vn      

(**************************************************************)
(************************ IMPORTANT ***************************)
(* In this section, the order of bindings matter *)
(* Those bindings at the end, can depend on previous bindings *)
(* So we need to reverse the binding list (just for tail *)
(* recursion, we could use a fold_right *)
(**************************************************************)
(**************************************************************)
let dump_let_bindings (ce, renv) = 
  let at   = ce.pcexpr_annot in 
  let eh   = ce.pcexpr_origin in
  let fi   = ce.pcexpr_loc    in
  let construct_CELET_binding (vn, (odt,fce))  = CELET (odt, vn, fce) in
    (* Now include all top level bindings as CELets *)
  let let_bindings = List.map construct_CELET_binding renv.bindings in
    make_let_flwor let_bindings ce at eh fi

(************************************************)
(* Our condition is that the binding should move 
   ce: core expression
   1. free variables in ce are a 
      subset of the current globals 
      (it depends only on globals)
   2. ce is not a scalar or a variable value
   3. ce not dependent on the context item fs:dot
   4. ce is side-effect free
*)
(*************************************************)
let is_forbidden_expr ce =
  match ce.pcexpr_desc with       
    | CEComment _     | CEText _ 
    | CEScalar _      | CEPI _  
    | CEEmpty         | CEVar _
    | CEForwardAxis _ | CEReverseAxis _ -> true
    | _ -> false

let binding_should_move genv ce1 = 
  let fv = get_free_variables ce1 in 
    ((not (is_forbidden_expr ce1)) &&
     (not (List.mem Xquery_common_ast.fs_dot fv))) &&
    (is_subset fv
       (get_global_variables genv)) 
    && (not (Rewriting_judgments.has_side_effect_judge ce1))

(***********************)
(* Intermediate walker *)
(***********************)
let rec factor_globals_walker (genv:global_passed_env) cexpr = 
  let eh = cexpr.pcexpr_origin     in
  let fi = cexpr.pcexpr_loc        in

(* This should be used when we don't have 
   a new operand, and then it should be able to calc the annotaitons
   efficiently.... *)
  let build_unmodified_return desc = 
    (* SHOULD ASSERT THAT DESC = cexpr.desc *)
    (* let at = cexpr.pcexpr_annot      in  *)
    fmkcexpr desc eh fi 
  in
    
  (* This is should be called when we can efficiently
     recalculate the annotations *)    
  let build_changed_return desc =
    fmkcexpr desc eh fi 
  in
    match cexpr.pcexpr_desc with
      | CEUnordered ce -> 
	  let fce, renv = factor_globals_walker genv ce in
	    (build_unmodified_return (CEUnordered fce)), renv

      | CEOrdered ce   -> 
	  let fce, renv = factor_globals_walker genv ce in
	    (build_unmodified_return (CEOrdered fce)), renv
	      
      | CEFLWOR (cfls, wc, ob, ret) ->
	  let (fl,wc,ob,ret),renv = factor_global_flwor genv cfls wc ob ret in
	    (* We must handle the case when all the bindings are gone *)
	    if (fl = []) then
	      begin		
		(* NOTE: Order By can be ignored because only let
		   bindings are there. This means there is only a singleton
		   sequence to be ordered *)
		match wc with
		  (* Just the return clause should remain *)
		| None -> ret, renv
		| (Some wc) -> 
		    (* This is nasty *)
		    (* Normalize back to CEIf *)		       
		    let eh = cexpr.pcexpr_origin     in
		    let fi = cexpr.pcexpr_loc        in
		    let empty = fmkcexpr CEEmpty eh fi in 
		    (build_changed_return (CEIf (wc, ret,empty))), renv
	      end
	    else	       
	      (build_changed_return (CEFLWOR (fl,wc,ob,ret))), renv

      | CEIf (cond, ce1, ce2) ->
	  let fcond,renv = factor_globals_walker genv cond in
	  let fce1       = dump_let_bindings 
			     (factor_globals_walker genv ce1) in
	  let fce2       = dump_let_bindings 
			     (factor_globals_walker genv ce2) in	
	    (build_unmodified_return (CEIf(fcond, fce1, fce2))), renv

      | CETypeswitch (on_cond, patterns) ->
	  let fcond,renv = factor_globals_walker genv on_cond in 
	  let typeswitch_patterns (cp, ovn, ce) =
	    let genv = add_opt_non_global_binding genv ovn in 
	    let fce  = dump_let_bindings 
			 (factor_globals_walker genv ce) in 
	      (cp, ovn, fce)
	  in
	  let pats = List.map typeswitch_patterns patterns in
	    (build_unmodified_return (CETypeswitch (fcond, pats))), renv
	      
      | CECall(cfname, cexprs, ti, upd, selfrecur) ->	  
	  let fconds, renv = factor_globals_walker_map genv cexprs in
	    (build_unmodified_return (CECall(cfname, fconds, ti, upd, selfrecur))), renv

      | CEOverloadedCall(name, cexprs, sigtable) ->
	  let fconds, renv = factor_globals_walker_map genv cexprs in
	    (build_unmodified_return (CEOverloadedCall(name, fconds, sigtable))), renv


      | CESeq(ce1,ce2) ->
	  let fce1,renv1  = factor_globals_walker genv ce1 in 
	  let fce2,renv2  = factor_globals_walker genv ce2 in
	  let renv        = combine_global_envs renv1 renv2 in 
	    (build_unmodified_return (CESeq (fce1, fce2))), renv

      | CEImperativeSeq(ce1,ce2) ->
	  let fce1,renv1  = factor_globals_walker genv ce1 in 
	  let fce2,renv2  = factor_globals_walker genv ce2 in
	  let renv        = combine_global_envs renv1 renv2 in 
	    (build_unmodified_return (CEImperativeSeq (fce1, fce2))), renv

      | CEDocument ce ->
	  let fce,renv    = factor_globals_walker genv ce in
	    (build_unmodified_return (CEDocument fce)), renv

      | CEPIComputed (ce1, ce2) ->
	  let fce1,renv1  = factor_globals_walker genv ce1 in 
	  let fce2,renv2  = factor_globals_walker genv ce2 in
	  let renv        = combine_global_envs renv1 renv2 in 
	    (build_unmodified_return (CEPIComputed (fce1, fce2))), renv
	      
      | CECommentComputed ce ->
	  let fce,renv    = factor_globals_walker genv ce in
	    (build_unmodified_return (CECommentComputed fce)), renv

      | CETextComputed ce ->
	  let fce,renv    = factor_globals_walker genv ce in
	    (build_unmodified_return (CETextComputed fce)), renv
	      
      | CEElem (name,ns, cexprs) ->
	  let fces,renv = factor_globals_walker_map genv cexprs in
	    (build_unmodified_return (CEElem (name,ns, fces))),renv

      | CEAnyElem (ce1, ns, ce2) -> 
	  let fce1, renv1 = factor_globals_walker genv ce1 in
	  let fce2, renv2 = factor_globals_walker genv ce2 in 
	  let renv        = combine_global_envs renv1 renv2 in 
	    (build_unmodified_return (CEAnyElem (fce1, ns, fce2))), renv

      | CEAttr (name, cexprs) ->
	  let fces,renv = factor_globals_walker_map genv cexprs in
	    (build_unmodified_return (CEAttr (name,fces))),renv

      | CEAnyAttr (ce1, nsenv, ce2) ->
	  let fce1, renv1 = factor_globals_walker genv ce1 in
	  let fce2, renv2 = factor_globals_walker genv ce2 in 
	  let renv        = combine_global_envs renv1 renv2 in
	    (build_unmodified_return (CEAnyAttr (fce1, nsenv, fce2))), renv

      | CEError cexpr_list -> 
	  let fcexpr_list, renv = factor_globals_walker_map genv cexpr_list in
	    (build_unmodified_return (CEError fcexpr_list)), renv

      | CETreat (ce, dt) ->
	  let fce, renv  = factor_globals_walker genv ce in
	    (build_unmodified_return (CETreat (fce,dt))), renv

      | CEValidate (vm, ce) ->
	  let fce, renv  = factor_globals_walker genv ce in
	    (build_unmodified_return (CEValidate (vm,fce))), renv

      | CECast (ce, nsenv, dt) -> 
	  let fce, renv  = factor_globals_walker genv ce in
	    (build_unmodified_return (CECast (fce, nsenv, dt))), renv

      | CECastable (ce, nsenv, dt) ->
	  let fce, renv  = factor_globals_walker genv ce in
	    (build_unmodified_return (CECastable (fce, nsenv, dt))), renv


      | CESome (odt, vn, ce1, ce2) -> 
	  let fce1, renv1 = factor_globals_walker genv ce1 in
	  let genv        = add_non_global_binding genv vn in
	  let fce2, renv2 = factor_globals_walker genv ce2 in
	  let renv        = combine_global_envs renv1 renv2 in 
	    (build_unmodified_return (CESome (odt,vn, fce1, fce2))), renv

      | CEEvery (odt, vn, ce1, ce2) ->
	  let fce1, renv1 = factor_globals_walker genv ce1 in
	  let genv        = add_non_global_binding genv vn in
	  let fce2, renv2 = factor_globals_walker genv ce2 in
	  let renv        = combine_global_envs renv1 renv2 in 
	    (build_unmodified_return (CEEvery (odt,vn, fce1, fce2))), renv

      (* NOTE: Are the sequence expressions independent or dependent?
	 They are being treated here as independent. PLEASE MAKE SURE THIS
	 IS CORRECT.
	 If they should be dependent just switch the result to a fold walker
      *)
      | CECopy ce ->
	  let fce, renv    = factor_globals_walker genv ce in
	    (build_unmodified_return (CECopy fce)), renv
      | CEDelete ce ->
	  let fce, renv    = factor_globals_walker genv ce in
	    (build_unmodified_return (CEDelete fce)), renv
      | CEInsert (ce, aci) ->
	  let fce, renv1   = factor_globals_walker genv ce in
	  let faci, renv2  = factor_globals_walker_insert genv aci in 
	  let renv         = combine_global_envs renv1 renv2 in 
	  (build_unmodified_return (CEInsert (fce, faci))), renv
      | CEReplace (vof, ce1, ce2) ->
	  let fce1, renv1  = factor_globals_walker genv ce1 in
	  let fce2, renv2  = factor_globals_walker genv ce2 in
	  let renv         = combine_global_envs renv1 renv2 in 
	  (build_unmodified_return (CEReplace (vof, fce1, fce2))), renv 
      | CERename (nsenv, ce1, ce2) ->
	  let fce1, renv1  = factor_globals_walker genv ce1 in
	  let fce2, renv2  = factor_globals_walker genv ce2 in
	  let renv         = combine_global_envs renv1 renv2 in 
	  (build_unmodified_return (CERename (nsenv, fce1, fce2))), renv 
      | CESnap (sm,cexpr) -> 
	  let fce, renv = factor_globals_walker genv cexpr in
	    (build_unmodified_return (CESnap (sm,fce))), renv

      | CELetvar (odt, vn, ce1, ce2) -> 
	  let fce1, renv1 = factor_globals_walker genv ce1 in
	  let genv        = add_non_global_binding genv vn in
	  let fce2, renv2 = factor_globals_walker genv ce2 in
	  let renv        = combine_global_envs renv1 renv2 in 
	    (build_unmodified_return (CELetvar (odt,vn, fce1, fce2))), renv          
      | CESet (v,cexpr) -> 
	  let fce, renv = factor_globals_walker genv cexpr in
	    (build_unmodified_return (CESet (v,fce))), renv

	    (* Mary: I'm assuming that CEExecute behaves like CESnap *)
      | CEExecute (async, ncname, uri, hostport,cexpr) -> 
	  let fhostport, renv1 = factor_globals_walker genv hostport in
	  let fce, renv3   = factor_globals_walker genv cexpr in
	  let renv'        = combine_global_envs renv1 renv3 in 
	    (build_unmodified_return (CEExecute (async, ncname, uri, fhostport, fce))), renv'

      (* Should clean variables *)
      | CEVar vn -> 
	  (build_changed_return (CEVar (get_rebound_variable genv vn))), empty_renv
      | CEForwardAxis _ | CEReverseAxis _ (* fs:dot should never be renamed.. *)    
	    (* Bases *)
      | CEComment _     | CEText _ | CECharRef _
      | CEScalar _      | CEProtoValue _ | CEPI _
      | CEEmpty -> cexpr, empty_renv

(* It is a map, so I am assuming the individual expressions are 
   independent from one another *)
and factor_globals_walker_map genv cexprs =
  let fconds, renvs = List.split (List.map (factor_globals_walker genv) cexprs) in
  let renv =     
    match renvs with
      | [] -> 
	  (* Should this be an error? *)
	  empty_renv
      | x :: rest -> (* Should just check non-empty with an if CHANGE THIS *)
	  List.fold_left combine_global_envs x rest
  in
    fconds, renv

and factor_global_flwor genv cfls wc ob ret =
  match cfls with
      [] -> 
	let (wc,ob,ret),renv = factor_global_flwor_where genv wc ob ret in
	  ([], wc, ob,ret), renv

    | CELET(odt, vn, ce1) :: rest ->	
	let fce1, renv1 = factor_globals_walker genv ce1 in
	  if binding_should_move genv ce1 then
	    begin
	      let genv, bind = add_global_binding genv vn (odt, fce1) in
	      let op, renv2  = factor_global_flwor genv rest wc ob ret in
	      let renv       = combine_global_envs renv1
				  (combine_global_envs bind renv2) in
		op, renv
	    end
	  else 
	    begin
	      let genv        = add_non_global_binding genv vn in
	      let this_op = CELET (odt, vn, fce1) in		
	      let (fls, wc, ob, ret),renv2  = factor_global_flwor genv rest wc ob ret in		
	      let renv        = combine_global_envs renv1 renv2 in
		(this_op :: fls, wc, ob, ret), renv
	    end

    | CEFOR(odt, vn, pos, ce1) :: rest ->
	let genv        = add_non_global_binding genv vn in
	let genv        = add_opt_non_global_binding genv pos in
	let fce1, renv1 = factor_globals_walker genv ce1 in
	let this_op     = CEFOR(odt,vn,pos, fce1) in
	let (fls, wc, ob, ret),renv2  = factor_global_flwor genv rest wc ob ret in		
	let renv        = combine_global_envs renv1 renv2 in 
	  (this_op :: fls, wc, ob,ret), renv

and factor_global_flwor_where genv wc ob ret =
  match wc with
    | None   -> 
	let (ob,ret), renv = factor_global_flwor_order_by genv ob ret in
	  (None, ob, ret), renv

    | Some w ->
	let w,renv1         = factor_globals_walker genv w in 
	let (ob,ret), renv2 = factor_global_flwor_order_by genv ob ret in
	let renv            = combine_global_envs renv1 renv2 in 
	  ((Some w), ob,ret), renv

and factor_global_flwor_order_by genv ob ret =
  match ob with
    | None -> 
	let fret, renv = factor_globals_walker genv ret in 
	(None, fret), renv

    | Some (sk, ospecs, osig) ->
	let fospecs, renvs = List.split (List.map (factor_acorderby genv) ospecs) in
	let fret, renv     = factor_globals_walker genv ret in 
	let renv           = List.fold_left combine_global_envs renv renvs in
	  ((Some (sk, fospecs, osig)),fret), renv

and factor_acorderby genv (ce,sk,esk) =
  let fce, renv = factor_globals_walker genv ce in 
    (fce, sk,esk), renv

and factor_globals_walker_insert fenv il = 
  match il with
    | CUAsLastInto ce -> 
	let fce, renv = factor_globals_walker fenv ce in 
	  (CUAsLastInto fce), renv

    | CUAsFirstInto ce -> 
	let fce, renv = factor_globals_walker fenv ce in 
	  (CUAsFirstInto fce), renv

    | CUInto ce -> 
	let fce, renv = factor_globals_walker fenv ce in 
	  (CUInto fce), renv

    | CUAfter ce -> 
	let fce, renv = factor_globals_walker fenv ce in 
	  (CUAfter fce), renv

    | CUBefore ce ->
	let fce, renv = factor_globals_walker fenv ce in 
	  (CUBefore fce), renv

let factor_global_expression stat_ctxt ce =
  ignore ( Factorize_free_var.compute_free_vars ce );
  let globals   = get_free_variables ce in 
  let genv      = mk_global_passed_env stat_ctxt [] globals in
    dump_let_bindings 
      (factor_globals_walker genv ce) 
    
