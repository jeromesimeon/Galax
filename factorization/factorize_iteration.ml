(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_iteration.ml,v 1.36 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Factorize_iteration
   Description:
     This module attempts to put expressions in a normal form. For a
     fragment of the language it moves expressions close as close to
     the iteration they depend on as possible.
*)


open Xquery_core_ast


let unique_binding_id = ref 0 
let get_new_factored_name () = 
  let local_name = "itfact_" ^ (string_of_int !unique_binding_id) in 
    incr unique_binding_id;
    (Namespace_builtin.glx_prefix, Namespace_builtin.glx_uri, local_name)

(* Return environment, holds bindings of names to expressions *)
type return_env = (Xquery_common_ast.cvname * Xquery_core_ast.acexpr ) list

  
let empty_renv () = []
let make_renv l = l 
let combine_renvironments i1 i2 = i1 @ i2

let get_bindings renv = renv

let add_binding renv vn ce = 
  let renv = (vn,ce) :: renv in 
  let desc = CEVar vn in 
    (Xquery_core_ast_util.fmkacexpr desc ce.pcexpr_annot ce.pcexpr_origin ce.pcexpr_loc),
    renv


let add_fresh_binding (renv:return_env) ce = 
  add_binding renv (get_new_factored_name ()) ce

let assert_empty_renv renv e_string = 
  match renv with
    | [] -> ()
    | _  -> raise (Error.Query (Error.Factorization ("Assert Empty Renv failed: " ^ e_string)))

(**********************)
(* INTIAL ENVIRONMENT *)
(**********************)
(* type env. Here we record the variables that are in scope and are
iteration dependant. *)
type current_scope = 
  | TopLevel
  | InConditional
  | Iteration of Xquery_common_ast.cvname list

type env = {
  current_scope : current_scope;
  stat_ctxt     : Typing_context.static_context;
}

let mk_env scope sc = { current_scope=scope; stat_ctxt=sc}
let initial_env sc  = mk_env TopLevel sc

let enter_conditional env = mk_env InConditional env.stat_ctxt

let add_binding env vn =
  let scope =
    match env.current_scope with
      | TopLevel      -> TopLevel
      | InConditional -> InConditional
      | Iteration il  -> Iteration (vn :: il)
  in
    mk_env scope env.stat_ctxt

let add_opt_binding env ovn =
  match ovn with
    | None -> env
    | Some vn -> add_binding env vn 

let non_trivial_intersect (l1: 'a list) (l2: 'a list) =
  List.exists (fun v -> List.mem v l1) l2
  

let new_iterate_variable env vn = mk_env (Iteration [vn]) env.stat_ctxt

(* Judgments *)
let side_effect_free_judge env ce = 
  not (Rewriting_judgments.has_side_effect_judge ce)


(* Main judgement for iterate factorization:

   If the expression has side-effects, it will not be moved.

   If an expression is toplevel  (i.e. there is no
   iteration) or in a conditional, then it does not need to be pulled up.

   If an expression is not toplevel or in a conditional and depends on
   a variable that is in the most recent iteration scope, then it
   should not be pulled up.

   If an expression is not toplevel or in a conditional and only
   depends on those variables in the most recent iteration scope, then
   it should be pulled up.

*)
let does_not_depend_judge env ce = 
  match env.current_scope with      
    | TopLevel
    | InConditional  -> false
    | Iteration bindings ->
	match Xquery_core_ast_annotation.get_free_var_annot ce.pcexpr_annot with
	  | None -> 
	      let s = Print_top.bprintf_acexpr "" ce in
		raise (Error.Query (Error.Factorization ("Unannotated core expression: " ^ s)))
	  | Some fv -> 	      
	      not (non_trivial_intersect fv bindings)


(* Holds if the core_expression depends on the variable *)
let depends_on_judge vn ce =
  match Xquery_core_ast_annotation.get_free_var_annot ce.pcexpr_annot with
    | None -> 
	let s = Print_top.bprintf_acexpr "" ce in
	  raise (Error.Query (Error.Factorization ("Unannotated core expression: " ^ s)))
    | Some fv -> List.mem vn fv


(* Pullup judge *)
(* 
   (i)   does_not_depend fenv ce 
   (ii)  is not CEVar
   (iii) is not a constant
   (iv)  does not contain side-effects      
 *)
let pullup_judge fenv ce =
  let rule_2_or_3 = 
    match ce.pcexpr_desc with
      | CEVar _    | CEEmpty
      | CEScalar _ | CEComment _ 
      | CEText  _  | CEPI _  -> false
      | _ -> true
  in
  rule_2_or_3 && 
    (does_not_depend_judge fenv ce) &&
    (side_effect_free_judge fenv ce)


(************************)
(* Binding Construction *)
(************************)

let make_let_flwor let_bindings return_expression  = 
  if let_bindings = [] then return_expression
  else begin
    let at = return_expression.pcexpr_annot in
    let eh = return_expression.pcexpr_origin in
    let fi = return_expression.pcexpr_loc    in
    let ce = Xquery_core_ast_util.fmkacexpr (CEFLWOR(let_bindings, None, None, return_expression)) at eh fi in
      (***********************************)
      (* REANNOTATE ONLY FREE VARIABLES! *)
      (***********************************)
      ignore (Factorize_free_var.compute_free_vars ce);
      ce
  end

(******************************************)
(* Here, we handle restoring the bindings *)
(******************************************)
let make_let_binding (vn,ce) = CELET(None, vn, ce)


(* If no variable name is given, then ALL bindings stay here. *)
(* returns (Bindings that stay here * bindings that continue up *)  
(* Partition the bindings into those that are going to stay here,
   and those that are going to move up. *)
(* A binding stays here, if it depends on vn *)
let partition_bindings renv vn =
    match vn with 
      | None -> (get_bindings renv), []
      | Some vn ->
	  List.partition (fun (bname, bexpr) -> depends_on_judge vn bexpr) 
	    (get_bindings renv)

(* Handles partitioning the variabels and creating the return environment *)
let handle_return_helper renv vn = 
  let bindings_for_here, bindings_to_move = partition_bindings renv vn in 
  let renv = make_renv bindings_to_move in 
  let lets = List.map make_let_binding bindings_for_here in 
    lets, renv

let handle_return_exprs_flwor renv vn cur_fls = 
  let let_bindings, renv = handle_return_helper renv (Some vn) in 
    (cur_fls :: let_bindings), renv

let handle_return_exprs renv vn return_expr = 
  let let_bindings, renv = handle_return_helper renv vn in 
  let expr = make_let_flwor let_bindings return_expr in
    expr, renv
      
(******************)
(* Main Recursion *)
(******************)

let rec factor_expression fenv ce = 
  let {
    pcexpr_desc   = ce_desc; 
    pcexpr_annot  = at; 
    pcexpr_origin = eh; 
    pcexpr_loc    = fi;} = ce
  in    

    
    if pullup_judge fenv ce then 
      add_fresh_binding (empty_renv ()) ce 
    else
      begin    
	let ce_desc, renv =
	  match ce_desc with
	    (*****************************************)
	    (* Core Expressions that cause iteration *)
	    (*****************************************)
	  | CEFLWOR(cfls, where, orderby, ret) -> 
	      let (cfls, where,orderby,ret),renv = factor_flwor fenv cfls where orderby ret in
	      CEFLWOR (cfls, where, orderby, ret), renv
		(*************************************)
		(* Iteration implicitly happens for existential and universal quantification *)
		(*************************************)
	  | CESome  (odt, vn, in_expr, return_expr) ->
	      let in_expr, indep_renv   = factor_expression fenv in_expr in
	      let env                   = new_iterate_variable fenv vn in 
	      (*******************************)
	      (* Handle dependent expression *)	    
	      (*******************************)
	      let return_expr, dep_renv = factor_expression env return_expr in 
	      (* Add in all the necessary bindings *)
	      let return_expr, dep_renv = handle_return_exprs dep_renv (Some vn) return_expr in 
	      let fcexpr  = CESome (odt, vn, in_expr, return_expr) in 
	      let renv = combine_renvironments indep_renv dep_renv in 
	      fcexpr, renv
	  | CEEvery  (odt, vn, in_expr, return_expr) ->
	      let in_expr, indep_renv   = factor_expression fenv in_expr in
	      let fenv                   = new_iterate_variable fenv vn in 
	      (*******************************)
	      (* Handle dependent expression *)	    
	      (*******************************)
	      let return_expr, dep_renv = factor_expression fenv return_expr in 
	      (* Add in all the necessary bindings *)
	      let return_expr, dep_renv = handle_return_exprs dep_renv (Some vn) return_expr in 
	      let fcexpr  = CEEvery (odt, vn, in_expr, return_expr) in 
	      let renv = combine_renvironments indep_renv dep_renv in 
	      fcexpr, renv
	  (****************)
	  (* CONDITIONALS *)
	  (****************)
	  (**********************************************************)
	  (* Type switch is more complicated, we do not pull out of *)
	  (*  branches                                              *)
	  (**********************************************************)
	  | CETypeswitch(on_cond, pat_list) ->
	      let fcond,renv = factor_expression fenv on_cond in
	      let handle_branch (pat, name, ce) =
		let fenv = enter_conditional fenv in 
		(* Factor the expression *)
		let fce, renv = factor_expression fenv ce in
		let fce, renv = handle_return_exprs renv name ce in 
		let ()        = assert_empty_renv renv "CETypeswitch" in 
		(* Tossing renv *)
		pat, name, fce
	      in
	      let patterns = List.map handle_branch pat_list in	      
	      (CETypeswitch (fcond, patterns)), renv
	    (****************************************************)
	    (* Here we stop pulling up bindings on the branches *)
	    (****************************************************)
	  | CEIf(cond, ce1, ce2) ->
	      (* Only the cond is factored *)
	      let fcond, renv = factor_expression fenv cond in 
	      let fce1, renv_ce1 = factor_expression fenv ce1  in
	      let fce1, renv_ce1 = handle_return_exprs renv_ce1 None fce1 in
	      let ()             = assert_empty_renv renv_ce1 "CEIF [Cond1]" in 
	      let fce2, renv_ce2 = factor_expression fenv ce2  in 
	      let fce2, renv_ce2 = handle_return_exprs renv_ce2 None fce2 in
	      let ()             = assert_empty_renv renv_ce2 "CEIF [Cond2]" in 
	      CEIf(fcond, fce1, fce2), renv
	    (******************************************************************************)
	    (* In this section, we are just walking the tree. Our factorization algorithm *)
	    (* does not do anything to the children of these expressions                  *) 
	    (******************************************************************************)
	    (****************************************)
	    (* Beyond here we do not bind variables *)
	    (****************************************)
	  | CEUnordered ce -> 
	      let factored_expression, return_env = factor_expression fenv ce in
	      (CEUnordered (factored_expression)), return_env
	  | CEOrdered ce -> 
	      let factored_expression, return_env = factor_expression fenv ce in
	      (CEOrdered (factored_expression)), return_env
	  | CEWhile (ce1, ce2) ->
	      let fce1, renv1 = factor_expression fenv ce1 in
	      let fce2, renv2 = factor_expression fenv ce2 in
	      let renv = combine_renvironments renv1 renv2 in
	      CEWhile(fce1, fce2), renv
	  | CESeq (ce1, ce2) ->
	      let fce1, renv1 = factor_expression fenv ce1 in
	      let fce2, renv2 = factor_expression fenv ce2 in
	      let renv = combine_renvironments renv1 renv2 in
	      CESeq(fce1, fce2), renv
	  | CEImperativeSeq (ce1, ce2) ->
	      let fce1, renv1 = factor_expression fenv ce1 in
	      let fce2, renv2 = factor_expression fenv ce2 in
	      let renv = combine_renvironments renv1 renv2 in
	      CEImperativeSeq(fce1, fce2), renv
	  | CEPIComputed (ce1, ce2) ->
	      let fce1, renv1 = factor_expression fenv ce1 in
	      let fce2, renv2 = factor_expression fenv ce2 in
	      let renv = combine_renvironments renv1 renv2 in
	      (CEPIComputed(fce1, fce2)), renv
	  | CECommentComputed ( ce ) ->
	      let fce, renv = factor_expression fenv ce in
	      (CECommentComputed fce), renv
	  | CETextComputed ce ->
	      let tce, renv = factor_expression fenv ce in
	      (CETextComputed tce), renv
	  | CEElem(cename, nsenv, cexpr_l) ->
	      let fces, renv = factor_expression_list fenv cexpr_l in
	      (CEElem (cename, nsenv, fces)), renv
	  | CEAnyElem(ce1, nsenv1, nsenv2, ce2) ->
	      let fce1, renv1 = factor_expression fenv ce1 in
	      let fce2, renv2 = factor_expression fenv ce2 in
	      let renv = combine_renvironments renv1 renv2 in
	      (CEAnyElem (fce1, nsenv1, nsenv2, fce2)), renv
	  | CEAttr (can, nsenv, cexpr_l) -> 
	      let fces, renv = factor_expression_list fenv cexpr_l in
	      CEAttr (can, nsenv, fces), renv
	  | CEAnyAttr(ce1, nsenv, ce2) -> 	  
	      let fce1, renv1 = factor_expression fenv ce1 in
	      let fce2, renv2 = factor_expression fenv ce2 in
	      let renv = combine_renvironments renv1 renv2 in
	      (CEAnyAttr (fce1, nsenv, fce2)), renv
	  | CEError cexpr_list -> 
	      let fcexpr_list, renv = factor_expression_list fenv cexpr_list in	  
	      (CEError fcexpr_list), renv
	  | CETreat (ce, cst) -> 
	      let fce, renv = factor_expression fenv ce in	  
	      (CETreat (fce, cst)), renv
	  | CEValidate(vm, ce) ->
	      let fce, renv = factor_expression fenv ce in	  
	      (CEValidate (vm, fce)), renv
	  | CECast(ce, nsenv, cst) -> 
	      let fce, renv = factor_expression fenv ce in 
	      (CECast (fce, nsenv, cst)), renv
	  | CECastable(ce, nsenv, cst) ->
	      let fce, renv = factor_expression fenv ce in 
	      (CECastable (fce, nsenv, cst)), renv
	  | CELetServerImplement(nc1, nc2, ce1, ce2) ->
	      let fce1, renv1 = factor_expression fenv ce1 in
	      let fce2, renv2 = factor_expression fenv ce2 in
	      let renv = combine_renvironments renv1 renv2 in
	      (CELetServerImplement(nc1, nc2, fce1, fce2)), renv
	  | CEExecute(async, ncname, uri, ce1, ce2) ->
	      let fce1, renv1 = factor_expression fenv ce1 in
	      let fce2, renv2 = factor_expression fenv ce2 in
	      let renv = combine_renvironments renv1 renv2 in
	      (CEExecute(async, ncname, uri, fce1, fce2)), renv
	  | CEForServerClose(nc1, uri, ce1) ->
	      let fce1, renv1 = factor_expression fenv ce1 in
	      (CEForServerClose(nc1, uri, fce1)), renv1
	  | CEEvalClosure(ce1) ->
	      let fce1, renv1 = factor_expression fenv ce1 in
	      (CEEvalClosure(fce1)), renv1
	  | CEDocument ce -> 
	      let fce, renv = factor_expression fenv ce in 
	      (CEDocument fce), renv
	  | CECall(cfname, cexpr_l, types, upd, selfrecur) -> 
	      let fces, renv = factor_expression_list fenv cexpr_l in
	      (CECall(cfname, fces, types, upd, selfrecur)), renv
	  | CEOverloadedCall (cfname, cexpr_l, sigs) ->
	      let fces, renv = factor_expression_list fenv cexpr_l in
	      (CEOverloadedCall(cfname, fces, sigs)), renv
	  | CECopy ce1  ->
	      let fce1, renv = factor_expression fenv ce1 in
	      (CECopy fce1), renv
	    (******************************)
	    (* These are the update cases *)
	    (******************************)
	  | CEDelete ce1  ->
	      let fce1, renv = factor_expression fenv ce1 in
	      (CEDelete fce1), renv
	  | CEInsert (cei, il) ->
	      let finsert, renv1 = factor_insert_location fenv il in
	      let fces, renv2    = factor_expression fenv cei     in
	      let renv = combine_renvironments renv1 renv2        in
	      (CEInsert (fces, finsert)), renv
	  | CERename (nsenv, ce1, ce2) ->
	      let fce1, renv1    = factor_expression fenv ce1 in
	      let fce2, renv2    = factor_expression fenv ce2 in
	      let renv           = combine_renvironments renv1 renv2 in
	      (CERename (nsenv, fce1, fce2)), renv
	  | CEReplace (vof, ce1, ce2) ->
	      let fce1, renv1    = factor_expression fenv ce1 in
	      let fce2, renv2    = factor_expression fenv ce2 in
	      let renv           = combine_renvironments renv1 renv2 in
	      (CEReplace (vof, fce1, fce2)), renv
	  | CESnap (sm,cexpr) ->
	      let fce, renv = factor_expression fenv cexpr in
	      (CESnap (sm,fce)), renv
	  | CELetvar  (odt, vn, ce1, ce2) ->
	      let ce1', indep_renv   = factor_expression fenv ce1 in
	      let env                   = new_iterate_variable fenv vn in 
	      let ce2', dep_renv = factor_expression env ce2 in 
	      let ce2'', dep_renv = handle_return_exprs dep_renv (Some vn) ce2' in 
	      let fcexpr  = CELetvar (odt, vn, ce1', ce2'') in 		  
	      let renv = combine_renvironments indep_renv dep_renv in 
	      fcexpr, renv
          | CESet (v, cexpr) ->
	      let fce, renv = factor_expression fenv cexpr in
	      (CESet (v,fce)), renv
	    (*********************************************************)
	    (* Base case *********************************************)
	    (* These expressions can have their children factorized  *)
	    (*********************************************************)
	  | ((CEVar _) as v)
	  | ((CEScalar _) as v)
	  | ((CEProtoValue _) as v)
	  | (CEEmpty as v) | ((CEComment _) as v)
	  | ((CEPI _) as v)
	  | ((CEText _) as v)
	  | ((CECharRef _) as v)
	  | ((CEForwardAxis _) as v) 
	  | ((CEReverseAxis _) as v) ->
	      v, (empty_renv ())
	in
	(Xquery_core_ast_util.fmkacexpr ce_desc at eh fi),
	renv
      end

and factor_expression_list fenv cel = 
  let exprs, renvs = List.split (List.map (factor_expression fenv) cel) in
  let renv = List.fold_left combine_renvironments (empty_renv()) renvs in 
    exprs, renv
      
and factor_flwor fenv cfls where orderby ret = 
  (* Depencency is backward from the factorization steps *)
  (* Return is just an expression *)
  (* Order By *)
  let factor_orderby fenv ob ret =
    let factor_acorderby fenv (ce,sk,esk) =
      let fce, renv = factor_expression fenv ce in 
	(fce, sk,esk), renv
    in

      match ob with 
	| None -> 
	    let fret, renv = factor_expression fenv ret in
	      (None, fret), renv

	| Some (sk, ospecs, osig) ->
	    let fospecs, renvs = List.split (List.map (factor_acorderby fenv) ospecs) in
	    let fret, renv0    = factor_expression fenv ret in 
	    let renv = List.fold_left combine_renvironments renv0 renvs in
	    let ret_ospec = Some (sk, fospecs, osig) in	      
	      (ret_ospec, fret), renv

  in
    
  (* Where *)
  let factor_where fenv where orderby ret =
    match where with
      | None -> 
	  let (fob, fret), renv = factor_orderby fenv orderby ret in
	    (None, fob, fret), renv

      | Some wc ->	    
	  (* We *are* pulling out of where clauses *)
	  let fwc, ret_env      = factor_expression fenv  wc in
	  let (fob, fret), renv = factor_orderby fenv orderby ret in
	  let renv = combine_renvironments ret_env renv in
	    ((Some fwc), fob, fret), renv
  in      

  let prepend_fls stmts (fls, wc, ob, ret) = ((stmts@fls), wc, ob, ret) in
    (* Main match *)
    match cfls with
      | [] -> 
	  let  (fw, fob, fret), renv = factor_where fenv where orderby ret in
	    ([], fw, fob, fret), renv

      | CELET(odt, vn, ce1) :: rest ->
	  let fbound, indep_renv = factor_expression fenv ce1 in 
	  let fenv = add_binding fenv vn in
	  let cfl  = CELET(odt,vn, fbound) in

	  (* There are also returns coming up here *)
	  let flwor, dep_renv = factor_flwor fenv rest where orderby ret  in
	  let stmts, dep_renv = handle_return_exprs_flwor dep_renv vn cfl in 
	  let renv            = combine_renvironments indep_renv dep_renv in
	    (prepend_fls stmts flwor), renv

      | CEFOR(odt, vn, pos, ce1) :: rest ->
	  let fbound,indep_renv = factor_expression fenv ce1 in 
	  let cfl               = CEFOR(odt, vn, pos, fbound) in 
	    
	  (* Add in variable bindings *)
	  let fenv              = new_iterate_variable fenv vn in 
	  let fenv              = add_opt_binding fenv pos in 

	  (* There are also returns coming up here *)	  
	  let flwor,dep_renv  = factor_flwor fenv rest where orderby ret  in
	  let stmts,dep_renv  = handle_return_exprs_flwor dep_renv vn cfl in	    
	  let stmts,dep_renv  = match pos with
	    | None -> stmts, dep_renv
	    | Some pos -> handle_return_exprs_flwor dep_renv pos cfl 
	  in 
	  let renv            = combine_renvironments indep_renv dep_renv in
	    (prepend_fls stmts flwor), renv

(* Insert location *)

(************************************)
(* Update Factorization Section     *)
(* Specifically Insert location now *)
(************************************)
and factor_insert_location fenv il = 
  match il with
    | CUAsLastInto ce -> 
	let fce, renv = factor_expression fenv ce in 
	  (CUAsLastInto fce), renv

    | CUAsFirstInto ce -> 
	let fce, renv = factor_expression fenv ce in 
	  (CUAsFirstInto fce), renv

    | CUInto ce -> 
	let fce, renv = factor_expression fenv ce in 
	  (CUInto fce), renv

    | CUAfter ce -> 
	let fce, renv = factor_expression fenv ce in 
	  (CUAfter fce), renv

    | CUBefore ce ->
	let fce, renv = factor_expression fenv ce in 
	  (CUBefore fce), renv



(*************)
(* FRONT-END *)
(*************)
let factorize_expression stat_ctxt cexpr =
  (* annotate it *)
  ignore ( Factorize_free_var.compute_free_vars cexpr );
  let ce, renv = factor_expression (initial_env stat_ctxt) cexpr in
  let ce, renv = handle_return_exprs renv None ce in 
  let ()       = assert_empty_renv renv "TopLevel" in 
    ce
