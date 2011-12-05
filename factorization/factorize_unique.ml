(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_unique.ml,v 1.30 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Factorize_unique
   Description:
   This module assigns all variables a unique name in the core expression.
   
   o It assumes that names that are not bound are globals, or defined in another
     scope. These variable names remain untouched.

     o factorize_unique_with_context allows you to pass in initial bindings, 
       from global variables/parameters if renaming is done there.
       
   o fs:dot is a special variable and is never rebound.         

   The purpose is to simplify code later on. Currently this is not used.
*)


open Error
open Xquery_core_ast


(******************************)
(* Environment Helper section *)
(******************************)
type unique_environment = { cur_id: int ref; rebind_list: (Xquery_common_ast.cvname * Xquery_common_ast.cvname) list }

let mk_uenv idref l = { cur_id = idref; rebind_list = l }
(* Identity map for fs_dot since it should never be rebound *)
let empty_uenv () =  
  mk_uenv 
    (ref 0) 
    []

let mk_default_uenv binding = 
  mk_uenv (ref 0) binding

(* MUST ENSURE IT IS NOT fs:dot *)
let get_unique_name env ((prefix,uri,name) as vn) =  
  if Namespace_names.rqname_equal Xquery_common_ast.fs_dot vn then
    vn
  else
    begin
      let name = name ^ "_u" ^ (string_of_int !(env.cur_id)) in
	incr env.cur_id;
	(prefix,uri,name)
    end

(* Adding bindings *)
let add_binding env pair = 
  mk_uenv env.cur_id (pair :: env.rebind_list)

let add_binding_name env vn =
  let new_name = get_unique_name env vn in
  let env      = add_binding env (vn, new_name) in
    new_name, env
      
let add_opt_binding_name env ovn =
  match ovn with 
    | None -> None, env
    | Some vn -> 
	let vn, env = add_binding_name env vn in
	  (Some vn), env

(***********************)
(* Retrieving Bindings *)
(***********************)
let get_rebound_name env vn =
  if Namespace_names.rqname_equal Xquery_common_ast.fs_dot vn then
    vn
  else
    begin
      if List.mem_assoc vn env.rebind_list then
	List.assoc vn env.rebind_list
      else vn
    end
    
(*****************)
(* Main function *)
(*****************)
(* Mary : Why isn't this function written using the Ast_walker_rewrite module ? *)
let rec unique_rename env cexpr = 
  let {
    pcexpr_desc = ce_desc; 
    pcexpr_annot = annot; 
    pcexpr_origin = eh; 
    pcexpr_loc = loc;} = cexpr
  in 
  let ce_desc = 
    match ce_desc with
    | CEOrdered ce   -> CEOrdered (unique_rename env ce)
    | CEUnordered ce -> CEUnordered (unique_rename env ce)
    | CEFLWOR(cfls, wc, ob, ret) -> 
	flwr_unique_rename env cfls wc ob ret
    | CEIf(cond, ce1, ce2) ->
	let cond = unique_rename env cond in 
	let ce1  = unique_rename env ce1 in 
	let ce2  = unique_rename env ce2 in 
	CEIf(cond,ce1,ce2)
    | CEWhile(cond, ce1) ->
	let cond = unique_rename env cond in 
	let ce1  = unique_rename env ce1 in 
	CEWhile(cond,ce1)
    | CETypeswitch (on, patt) -> 
	let on       = unique_rename env on in
	let rename_branch (cpat, ovn, ce) = 
	  let ovn, env = add_opt_binding_name env ovn in 
	  (cpat, ovn, (unique_rename env ce))
	in
	let patt = List.map rename_branch patt in
	CETypeswitch (on, patt)
    | CEVar vn -> CEVar (get_rebound_name env vn)	  
    | CEOverloadedCall (cfname, cexpr_list, sig_table) ->
	let cexpr_list = List.map (unique_rename env) cexpr_list in
	CEOverloadedCall (cfname, cexpr_list, sig_table)
    | CECall (cfname, cexpr_list, in_out_types, upd, selfrecur) ->
	let cexpr_list = List.map (unique_rename env) cexpr_list in 
	CECall (cfname, cexpr_list, in_out_types, upd, selfrecur)
    | CEScalar av -> CEScalar av
    | CEProtoValue av -> CEProtoValue av
    | CESeq (ce1,ce2) -> 
	let ce1 = unique_rename env ce1 in
	let ce2 = unique_rename env ce2 in 
	CESeq (ce1,ce2)
    | CEImperativeSeq (ce1,ce2) -> 
	let ce1 = unique_rename env ce1 in
	let ce2 = unique_rename env ce2 in 
	CEImperativeSeq (ce1,ce2)
    | CEEmpty -> CEEmpty
    | CEDocument ce -> CEDocument (unique_rename env ce)
    | CEPI (name, pi) -> CEPI (name,pi)
    | CEPIComputed (ce1, ce2) -> 
	let ce1 = unique_rename env ce1 in
	let ce2 = unique_rename env ce2 in 
	CEPIComputed (ce1, ce2)
    | CEComment c -> CEComment c
    | CECommentComputed ce -> 
	CECommentComputed (unique_rename env ce)
    | CEText t -> CEText t
    | CECharRef t -> CECharRef t
    | CETextComputed ce -> CETextComputed (unique_rename env ce)
    | CEElem (name,nsenv,cexpr_list) -> 
	CEElem (name,nsenv, (List.map (unique_rename env) cexpr_list))
    | CEAnyElem(ce1,nsenv1, nsenv2, ce2) -> 
	CEAnyElem ((unique_rename env ce1), nsenv1, nsenv2, (unique_rename env ce2))
    | CEAttr (ca, nsenv, cexpr_list) ->
	CEAttr (ca, nsenv, (List.map (unique_rename env) cexpr_list))
    | CEAnyAttr(ce1, nsenv, ce2) ->
	CEAnyAttr ((unique_rename env ce1), nsenv, (unique_rename env ce2))
    | CEError cexpr_list -> CEError (List.map (unique_rename env) cexpr_list)
    | CETreat (ce, dt) -> CETreat ((unique_rename env ce), dt)
    | CEValidate (vm, ce) -> CEValidate (vm, (unique_rename env ce))
    | CECast (ce, nsenv, dt) -> CECast ((unique_rename env ce), nsenv, dt)
    | CECastable (ce, nsenv, dt) -> CECastable ((unique_rename env ce), nsenv, dt)
    | CEForwardAxis (v,a,nt) -> CEForwardAxis (v,a,nt)
    | CEReverseAxis (v,a,nt) -> CEReverseAxis (v,a,nt)
    | CESome  (odt, vn, ce1, ce2) -> 
	let ce1     = unique_rename env ce1   in 
	let vn, env = add_binding_name env vn in
	let ce2     = unique_rename env ce2   in 
	CESome (odt, vn, ce1, ce2)
    | CEEvery (odt, vn, ce1, ce2) -> 
	let ce1     = unique_rename env ce1   in 
	let vn, env = add_binding_name env vn in
	let ce2     = unique_rename env ce2   in 
	CEEvery (odt, vn, ce1, ce2)
	  (* Updates *)
    | CECopy ce1 -> 
	CECopy (unique_rename env ce1)
    | CEDelete ce1 -> 
	CEDelete (unique_rename env ce1)
    | CEInsert (cei, il) ->
	CEInsert ((unique_rename env cei),
		  (unique_rename_insertloc env il))
    | CERename(nsenv, ce1, ce2) ->
	CERename(nsenv,
		 (unique_rename env ce1),
		 (unique_rename env ce2))
    | CEReplace(vof,ce1, ce2) ->
	CEReplace(vof,
		  (unique_rename env ce1),
		  (unique_rename env ce2))
    | CESnap (sm,cexpr) ->
	CESnap (sm, (unique_rename env cexpr))
    | CELetvar (odt, vn, ce1, ce2) -> 
	let ce1     = unique_rename env ce1   in 
	let vn, env = add_binding_name env vn in
	let ce2     = unique_rename env ce2   in 
	CELetvar (odt, vn, ce1, ce2)
    | CESet (v, ce) ->
        CESet (v, unique_rename env ce)
	  (* NOTE: The following are extension to the XQuery Formal Semantics Core XQuery - Jerome *)
    | CELetServerImplement (nc1, nc2, ce1, ce2) -> 
	let ce1     = unique_rename env ce1   in 
	let ce2     = unique_rename env ce2   in 
	CELetServerImplement (nc1, nc2, ce1, ce2)
    | CEExecute (async, ncname, uri, ce1, ce2) -> 
	let ce1     = unique_rename env ce1   in 
	let ce2     = unique_rename env ce2   in 
	CEExecute (async, ncname, uri, ce1, ce2)
    | CEForServerClose (nc1, uri, ce1) -> 
	let ce1     = unique_rename env ce1   in 
	CEForServerClose (nc1, uri, ce1)
    | CEEvalClosure (ce1) -> 
	let ce1     = unique_rename env ce1   in 
	CEEvalClosure (ce1)
  in
  Xquery_core_ast_util.fmkacexpr ce_desc annot eh loc
      
and unique_rename_insertloc env il =
  match il with
    | CUAsLastInto ce  -> CUAsLastInto (unique_rename env ce)
    | CUAsFirstInto ce -> CUAsFirstInto (unique_rename env ce)
    | CUInto ce        -> CUInto (unique_rename env ce)
    | CUAfter ce       -> CUAfter (unique_rename env ce)
    | CUBefore ce      -> CUBefore (unique_rename env ce)

and flwr_unique_rename env cfls wc ob ret =
  (* NOTE: THIS REVERSES THE ORDER FOLD LEFT AND CONS *)
  let rebind_fold (cfls, env) cfl =
    match cfl with
      | CELET(odt,vn,ce) ->
	  let vn, env = add_binding_name env vn in
	  let ce = unique_rename env ce in
	    (CELET(odt, vn, ce) :: cfls, env)

      | CEFOR(odt,vn,ovn,ce) ->
	  let vn, env  = add_binding_name env vn in
	  let ovn, env = add_opt_binding_name env ovn in
	  let ce = unique_rename env ce in
	    (CEFOR(odt, vn, ovn, ce) :: cfls, env)
  in
  let cfls_rev, env = List.fold_left rebind_fold ([], env) cfls in
  let cfls = List.rev cfls_rev in
  let wc = 
    match wc with 
      | None -> None
      | Some w -> Some (unique_rename env w)
  in
  let ob = unique_rename_orderby env ob in
  let ret = unique_rename env ret in
    CEFLWOR(cfls, wc, ob, ret)

and unique_rename_orderby env ob = 
  match ob with 
    | None -> ob
    | Some (sk, aos, osig) ->
	let aos = List.map (fun (ce, sk,esk) -> ((unique_rename env ce), sk, esk)) aos in
	  Some (sk, aos, osig)

(***********************************)
(* These are the exposed functions *)
(***********************************)

type binding_list = (Xquery_common_ast.cvname * Xquery_common_ast.cvname) list

let factorize_unique_with_context bindings ce =
  let env = mk_default_uenv bindings in
    unique_rename env ce

let factorize_unique ce =
  factorize_unique_with_context [] ce
