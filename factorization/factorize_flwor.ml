(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_flwor.ml,v 1.35 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Factorize_flwor
   Description:
     This module factorizes out code from FLWOR expression. This is to
     help the optimizer pick up logical optimizations.
*)

(* Example:

    Currently the only component is pulling up FLWOR blocks
    nested in return statements.

   Ex:
   for $x in ...
   return <a> { for $y in ...
                where $x = $y } </a>

   -----
   for $x in ...
   let $FLWOR_norm := for $y in ...
                      where $x = $y
   return <a> { $FLWOR_norm } </a>
   

   Added Element construction rewrite and function calls.

*)

open Conf
open Error

open Xquery_common_ast
open Xquery_core_ast
open Xquery_core_ast_util



(******************)
(* Misc utilities *)
(******************)

(* Jerome: Probably those should be moved *)

type env = { 
  static_context            : Typing_context.static_context;
  mutable flwor_bindings    : (crname * acexpr) list option 
} 

let mk_env sc pl = 
  { static_context = sc; flwor_bindings= pl }

(* These are independent bindings, order does not matter 
   but we preserve it *)
let normed_count = ref 0 
let get_return_name () = 
  let nc = !normed_count in 
    incr normed_count;
    (Namespace_builtin.glx_prefix, Namespace_builtin.glx_uri, ("FLWOR_norm" ^ (string_of_int nc)))  

let get_bindings_unsafe env = 
  match env.flwor_bindings with
    | None -> 
	raise (Error.Query (Error.Factorization ("Factorization is attempting to extract bindings, but there is no place holder!")))
    | Some binding ->
	binding

let update_parent env vn binding =
  match env.flwor_bindings with
    | None -> 
	raise (Error.Query (Error.Factorization ("Factorization is attempting to update parent of FLWOR, but parent is None")))
    | Some flwor_bindings ->
	env.flwor_bindings  <- Some (flwor_bindings @ ((vn, binding) :: []))

let make_initial_env stat_ctxt = mk_env stat_ctxt None
let make_place_holder env      = mk_env env.static_context (Some [])
let make_null_place_holder env = mk_env env.static_context None

let make_CELET_binding (vn, binding) = CELET (None, vn, binding) 
let add_new_CELET_bindings fl_clauses env =
  let bindings = get_bindings_unsafe env in
  let new_lets = List.map make_CELET_binding bindings in    
  (* Order does matter here *)
  fl_clauses @ new_lets

(****************************)
(* Rewrite to a normal form *)
(****************************)

let rec put_in_normal_form (env: env) (ce: acexpr) =
  let ma = ce.pcexpr_annot in
  let eh = ce.pcexpr_origin in
  let loc = ce.pcexpr_loc in
  match ce.pcexpr_desc with
  | CEFLWOR (fl_clauses, where_clause, order_by_clause, return_clause)  ->
      (* 1. Apply update normal-form first. *)
      (* Note: this never effects the where and orderby clauses. *)
      (* let (fl_clauses,return_clause) =
	 update_in_normal_form fl_clauses return_clause
	 in *)
      (* 2. Then go on with general normal-form. - Jerome *)
      begin
	match env.flwor_bindings with
	| None ->
	    let fl_clauses   = List.concat (List.map (cfl_put_in_normal_form env) fl_clauses) in
	    let where_clause =
	      match where_clause with
		None -> None
	      | Some wc ->
		  Some (put_in_normal_form env wc)
	    in
	    let (order_by_clause: acorder_by option) =
	      match order_by_clause with
		None -> None
	      | Some (stable, co_specs, osig) ->
		  let (co_specs: acorder_spec list) =
		    List.map (fun (ce, sk, esk) ->
		      (put_in_normal_form env ce), sk,esk)  co_specs
		  in
		  Some (stable, co_specs, osig)
	    in
	    (* We are the parent of our return clause *)
	    (* Make a place holder for us, so we can get our new let clauses *)
	    let env              = make_place_holder env in
	    let return_clause    = put_in_normal_form env return_clause in
	    (* Add the new let bindings *)
	    let fl_clauses = add_new_CELET_bindings fl_clauses env in
	    let expr_name  = CEFLWOR(fl_clauses, where_clause, order_by_clause, return_clause) in
	    fmkacexpr expr_name ma eh loc
	| Some parent ->
	    begin
	      (* if we are side-effect free, Replace ce with a
		 stub variable else just return ce (don't move it)
	       *)
	      if Rewriting_judgments.side_effect_free_judge ce
	      then
		begin
		  let stub_name       = get_return_name () in
		  let replacement_var = fmkacexpr (CEVar stub_name) ma eh loc in
		  (* Update our parent expression with the body of our expression *)
		  update_parent
		    env stub_name (put_in_normal_form (make_null_place_holder env) ce);
		  replacement_var
		end
	      else		      			
		ce
	    end
      end	    
	
	(*************************************************)
	(* We stop factorization in Ifs and typeswitches *)
	(* This stopping is accomplished by replacing    *)
	(* parent_flwor with None                        *)
	(*************************************************)
  | CEIf (cexpr1, cexpr2, cexpr3) ->
      let cexpr1 = put_in_normal_form env cexpr1 in
      
      (* Do not factor inside branches, this is accomplished 
	 by not allowing parent expressions *)
      
      let cexpr2 = put_in_normal_form (make_null_place_holder env) cexpr2 in
      let cexpr3 = put_in_normal_form (make_null_place_holder env) cexpr3 in
      fmkacexpr (CEIf (cexpr1, cexpr2, cexpr3)) ma eh loc
	
  | CEWhile (cexpr1, cexpr2) ->
      let cexpr1 = put_in_normal_form env cexpr1 in
      (* Do not factor inside branches, this is accomplished 
	 by not allowing parent expressions *)
      let cexpr2 = put_in_normal_form (make_null_place_holder env) cexpr2 in
      fmkacexpr (CEWhile (cexpr1, cexpr2)) ma eh loc
	
  | CETypeswitch (cexpr, cases) ->
      let cexpr' = put_in_normal_form env cexpr in
      let apply_one_case (pat,ovn,case_cexpr) =
	let new_case_cexpr = put_in_normal_form (make_null_place_holder env) case_cexpr
	in (pat,ovn,new_case_cexpr)
      in
      let cases = List.map apply_one_case cases in
      fmkacexpr (CETypeswitch (cexpr', cases)) ma eh loc
	
	(****************************************************)
	(* These operators contain variable bindings        *)
	(* To be conservative we do not allow crossing      *)
	(* expressions to cross these for flwor extraction. *)
	(****************************************************)
	
  | CESome (odt, vname, cexpr1, cexpr2) ->
      let cexpr1' = put_in_normal_form (make_null_place_holder env) cexpr1 in
      let cexpr2' = put_in_normal_form (make_null_place_holder env) cexpr2 in
      fmkacexpr (CESome (odt, vname, cexpr1', cexpr2')) ma eh loc
	
  | CEEvery (odt, vname, cexpr1, cexpr2) ->
      let cexpr1' = put_in_normal_form (make_null_place_holder env) cexpr1 in
      let cexpr2' = put_in_normal_form (make_null_place_holder env) cexpr2 in
      fmkacexpr (CEEvery (odt, vname, cexpr1', cexpr2')) ma eh loc
	
	(**************************************************)
	(* The rest of this code is just walking the tree *)
	(**************************************************)
	
  | CEUnordered cexpr ->
      let cexpr' = put_in_normal_form env cexpr in
      fmkacexpr (CEUnordered cexpr') ma eh loc
	
  | CEOrdered cexpr ->
      let cexpr' = put_in_normal_form env cexpr in
      fmkacexpr (CEOrdered cexpr') ma eh loc
	
  | CEVar vname -> 
      ce
  | CEScalar _ ->
      ce
  | CEProtoValue _ ->
      ce
  | CEText _ ->
      ce
  | CECharRef _ ->
      ce
  | CETextComputed cexpr1 ->
      let cexpr1' = put_in_normal_form env cexpr1 in
      fmkacexpr (CETextComputed cexpr1') ma eh loc
  | CEPI _ ->
      ce
  | CEPIComputed (cexpr1,cexpr2) ->
      let cexpr1' = put_in_normal_form env cexpr1 in
      let cexpr2' = put_in_normal_form env cexpr2 in
      fmkacexpr (CEPIComputed (cexpr1', cexpr2')) ma eh loc
	
  | CEComment _ ->
      ce
	
  | CECommentComputed cexpr1 ->
      let cexpr1' = put_in_normal_form env cexpr1 in
      fmkacexpr (CECommentComputed cexpr1') ma eh loc
	
  | CEDocument cexpr1 ->
      let cexpr1' = put_in_normal_form env cexpr1 in
      fmkacexpr (CEDocument cexpr1') ma eh loc
	
  | CECall (fname, arguments, sign, upd, selfrecur) ->
      let arguments' = List.map (put_in_normal_form env) arguments in
      fmkacexpr (CECall (fname, arguments',sign, upd, selfrecur)) ma eh loc
	
  | CEOverloadedCall (fname, arguments, sigs) ->
      let arguments' = List.map (put_in_normal_form env) arguments in
      fmkacexpr (CEOverloadedCall (fname, arguments', sigs)) ma eh loc
	
  | CELetServerImplement (nc1, nc2, cexpr1, cexpr2) ->
      let cexpr1' = put_in_normal_form env cexpr1 in
      let cexpr2' = put_in_normal_form env cexpr2 in
      fmkacexpr (CELetServerImplement(nc1,nc2,cexpr1',cexpr2')) ma eh loc

  | CEExecute (async, ncname, uri, cexpr1, cexpr2) ->
      let cexpr1' = put_in_normal_form env cexpr1 in
      let cexpr2' = put_in_normal_form env cexpr2 in
      fmkacexpr (CEExecute(async, ncname, uri, cexpr1', cexpr2')) ma eh loc

  | CEForServerClose (nc1, uri, cexpr1) ->
      let cexpr1' = put_in_normal_form env cexpr1 in
      fmkacexpr (CEForServerClose(nc1,uri, cexpr1')) ma eh loc

  | CEEvalClosure (cexpr1) ->
      let cexpr1' = put_in_normal_form env cexpr1 in
      fmkacexpr (CEEvalClosure(cexpr1')) ma eh loc

  | CESeq (cexpr1, cexpr2) ->
      let cexpr1' = put_in_normal_form env cexpr1 in
      let cexpr2' = put_in_normal_form env cexpr2 in
      fmkacexpr (CESeq(cexpr1', cexpr2')) ma eh loc
	
  | CEImperativeSeq (cexpr1, cexpr2) ->
      let cexpr1' = put_in_normal_form env cexpr1 in
      let cexpr2' = put_in_normal_form env cexpr2 in
      fmkacexpr (CEImperativeSeq(cexpr1', cexpr2')) ma eh loc
	
  | CEEmpty -> ce
	
  | CEElem (relem_symbol, nsenv, cexprlist) ->
      let cexprlist' = List.map (put_in_normal_form env) cexprlist in
      fmkacexpr (CEElem (relem_symbol, nsenv, cexprlist')) ma eh loc
	
  | CEAnyElem (cexpr1, nsenv1, nsenv2, cexpr2) ->  
      let cexpr1' = put_in_normal_form env cexpr1 in
      let cexpr2' = put_in_normal_form env cexpr2 in
      fmkacexpr (CEAnyElem (cexpr1', nsenv1, nsenv2, cexpr2')) ma eh loc
	
  | CEAttr (rattr_symbol, nsenv, cexprlist)->
      let cexprlist' = List.map (put_in_normal_form env) cexprlist in	  
      fmkacexpr (CEAttr (rattr_symbol, nsenv, cexprlist')) ma eh loc
	
  | CEAnyAttr (cexpr1, nsenv, cexpr2) ->
      let cexpr1' = put_in_normal_form env cexpr1 in
      let cexpr2' = put_in_normal_form env cexpr2 in
      fmkacexpr (CEAnyAttr (cexpr1', nsenv, cexpr2')) ma eh loc
	
  | CEError cexpr_list ->
      (* Note: I don't see any reason why expressions inside the
	 error function shouldn't be factorized as anything else -
	 Jerome (* ce *) *)
      let cexpr_list' = List.map (put_in_normal_form env) cexpr_list in
      fmkacexpr (CEError cexpr_list') ma eh loc
	
  | CETreat (cexpr, model) ->
      let cexpr' = put_in_normal_form env cexpr in
      fmkacexpr (CETreat (cexpr', model)) ma eh loc
	
  | CECast (cexpr, nsenv, model) ->
      let cexpr' = put_in_normal_form env cexpr in
      fmkacexpr (CECast (cexpr', nsenv, model)) ma eh loc
	
  | CECastable (cexpr, nsenv, model) ->
      let cexpr' = put_in_normal_form env cexpr in
      fmkacexpr (CECastable (cexpr', nsenv, model)) ma eh loc
	
  | CEValidate (vmode,cexpr) ->
      let cexpr' = put_in_normal_form  env cexpr in
      fmkacexpr (CEValidate (vmode,cexpr')) ma eh loc
	
  | CEForwardAxis (v,axis, cnode_test)
  | CEReverseAxis (v,axis, cnode_test) -> ce
	
  | CECopy ce1 ->
      let ce1' = put_in_normal_form env ce1 in
      fmkacexpr (CECopy ce1') ma eh loc
  | CEDelete ce1 ->
      let ce1' = put_in_normal_form env ce1 in
      fmkacexpr (CEDelete ce1') ma eh loc
  | CEInsert (cexpr, cinsert_location) ->
      let cexpr' = put_in_normal_form env cexpr in
      let cinsert_location' =
	match cinsert_location with
	| CUAsLastInto ce1 ->
	    let ce1' = put_in_normal_form env ce1 in
	    (CUAsLastInto ce1')
	| CUAsFirstInto ce1 ->
	    let ce1' = put_in_normal_form env ce1 in
	    (CUAsFirstInto ce1')
	| CUInto ce1 ->
	    let ce1' = put_in_normal_form env ce1 in
	    CUInto ce1'
	| CUAfter ce1 ->
	    let ce1' = put_in_normal_form env ce1 in
	    (CUAfter ce1')
	| CUBefore ce1 ->
	    let ce1' = put_in_normal_form env ce1 in
	    (CUBefore ce1')
      in
      fmkacexpr (CEInsert (cexpr', cinsert_location')) ma eh loc
  | CERename (nsenv, ce1, ce2) ->
      let ce1' = put_in_normal_form env ce1 in
      let ce2' = put_in_normal_form env ce2 in
      fmkacexpr (CERename (nsenv, ce1',ce2')) ma eh loc
  | CEReplace (vof, ce1, ce2) ->
      let ce1' = put_in_normal_form env ce1 in
      let ce2' = put_in_normal_form env ce2 in
      fmkacexpr (CEReplace (vof,ce1',ce2')) ma eh loc
  | CESnap (sm,ce) ->
      let ce = put_in_normal_form env ce in
      fmkacexpr (CESnap (sm,ce)) ma eh loc
  | CELetvar (odt, vname, cexpr1, cexpr2) ->
      let cexpr1' = put_in_normal_form (make_null_place_holder env) cexpr1 in
      let cexpr2' = put_in_normal_form (make_null_place_holder env) cexpr2 in
      fmkacexpr (CELetvar (odt, vname, cexpr1', cexpr2')) ma eh loc
  | CESet (vname, ce) ->
      let ce = put_in_normal_form env ce in
      fmkacexpr (CESet (vname,ce)) ma eh loc

and cfl_put_in_normal_form env cfl =
  match cfl with
  | CELET(odt, vn, expr) ->
      begin
	match expr.pcexpr_desc with
	| CEFLWOR _ -> (* Already nested, rewrite to parent *)
	    CELET(odt, vn, (put_in_normal_form env expr)) :: []
	| _ -> (* we are this parents *)
	    begin
	      let env       = make_place_holder env in
	      let norm_expr = put_in_normal_form env expr in
	      let bindings  = get_bindings_unsafe env in
	      let new_lets  = List.map make_CELET_binding bindings in
	      (* these lets should occur before the returned let, it could depend
		 on them *)
	      new_lets @ CELET(odt, vn, norm_expr) :: []
	    end
      end
  | CEFOR(odt, vn, vn2, expr) ->
      let expr = put_in_normal_form env expr in
      CEFOR(odt, vn, vn2, expr) :: []

let factorize_flwor static_context expr =
  put_in_normal_form (make_initial_env static_context) expr

