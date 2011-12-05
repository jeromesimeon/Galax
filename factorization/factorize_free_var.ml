(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_free_var.ml,v 1.29 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Factorize_free_var
   Description:
     Given a core expression, recursively fill out the free variable
     annotation.  Additionally return the list of free variables for
     the expression.
*)

open Xquery_common_ast
open Xquery_core_ast
open Xquery_core_ast_util
open Xquery_core_ast_annotation

let rec remove_bindings vn vlist = 
  List.filter (fun x ->  not (Namespace_names.rqname_equal vn x)) vlist

let rec compute_free_vars cexpr =
  let {
    pcexpr_desc = ce_desc; 
    pcexpr_annot = annot; 
    pcexpr_origin = eh; 
    pcexpr_loc = loc;} = cexpr
  in 
  let fv_list =
    match ce_desc with
    | CEOrdered ce
    | CEUnordered ce -> 
	compute_free_vars ce
    | CEFLWOR(cfls, wc, ob, ret) -> 
	compute_flwr_free cfls wc ob ret
    | CEIf(cond, ce1, ce2) ->
	(compute_free_vars cond) @ 
	(compute_free_vars ce1)  @ 
	(compute_free_vars ce2)
    | CEWhile(cond, ce1) ->
	(compute_free_vars cond) @ 
	(compute_free_vars ce1)
    | CETypeswitch (on, patt) -> 
	let fv_on = compute_free_vars on in 
	let handle_branch (_, ovn, cexpr) = 
	  match ovn with 
	    None    -> compute_free_vars cexpr 
	  | Some vn -> remove_bindings vn (compute_free_vars cexpr)
	in
	fv_on @ (List.concat (List.map handle_branch patt))
    | CEVar vn -> 
	vn :: []
    | CEOverloadedCall (_, cexpr_list, _)
    | CECall (_, cexpr_list, _, _, _) ->
	List.concat (List.map compute_free_vars cexpr_list)
    | CEScalar _ -> []
    | CEProtoValue _ -> []
    | CESeq (ce1,ce2) -> 
	(compute_free_vars ce1) @ (compute_free_vars ce2)
    | CEImperativeSeq (ce1,ce2) -> 
	(compute_free_vars ce1) @ (compute_free_vars ce2)
    | CEEmpty -> [] 
    | CEDocument ce -> compute_free_vars ce
    | CEPI _ -> []
    | CEPIComputed (ce1, ce2) -> (compute_free_vars ce1) @ (compute_free_vars ce2)
    | CEComment _ -> []
    | CECommentComputed ce -> compute_free_vars ce
    | CEText _ -> []
    | CECharRef _ -> []
    | CETextComputed ce -> compute_free_vars ce
    | CEElem (_,_,cexpr_list) -> 
	List.concat (List.map compute_free_vars cexpr_list)
    | CEAnyElem(ce1,_, _, ce2) -> 
	(compute_free_vars ce1) @ (compute_free_vars ce2)
    | CEAttr (_, _, cexpr_list) ->
	List.concat (List.map compute_free_vars cexpr_list)
    | CEAnyAttr(ce1, nsenv, ce2) ->
	(compute_free_vars ce1) @ (compute_free_vars ce2)
    | CEError cexpr_list -> List.concat (List.map compute_free_vars cexpr_list)
    | CETreat (ce, _) -> compute_free_vars ce
    | CEValidate (_, ce) -> compute_free_vars ce
    | CECast (ce, _, _ ) -> compute_free_vars ce
    | CECastable (ce, _, _ ) -> compute_free_vars ce
    | CEForwardAxis _ -> [Xquery_common_ast.fs_dot]
    | CEReverseAxis _ -> [Xquery_common_ast.fs_dot]
    | CESome  (_, vn, ce1, ce2) 
    | CEEvery (_, vn, ce1, ce2) -> 
	remove_bindings vn ((compute_free_vars ce1) @ (compute_free_vars ce2))
	  (* Updates *)
    | CECopy ce1 -> 
	compute_free_vars ce1
    | CEDelete ce1 -> 
	compute_free_vars ce1
    | CEInsert (cei, il) ->
	(compute_free_vars cei) @ (compute_free_vars_insert_location il)
    | CERename(_, ce1, ce2) ->
	(compute_free_vars ce1) @ (compute_free_vars ce2) 
    | CEReplace(_,ce1, ce2) ->
	(compute_free_vars ce1) @ (compute_free_vars ce2) 
    | CESnap (sm,cexpr) ->
	compute_free_vars cexpr
    | CELetvar (_, vn, ce1, ce2) ->
	remove_bindings vn ((compute_free_vars ce1) @ (compute_free_vars ce2))
    | CESet (_,ce) ->
        compute_free_vars ce
	  (* NOTE: The following are extension to the XQuery Formal Semantics Core XQuery - Jerome *)
    | CELetServerImplement(_,_, ce1,ce2) 
    | CEExecute (_, _, _, ce1, ce2) -> (compute_free_vars ce1)@(compute_free_vars ce2)
    | CEForServerClose(_,_,ce1) 
    | CEEvalClosure (ce1) -> (compute_free_vars ce1)
  in
  (* Remove duplicates *)
  let fv_list = Gmisc.remove_duplicates fv_list in 
  set_free_var_annot annot fv_list;
  fv_list

(************************************************)
(* Compute Free Variabels for FLWOR Expressions *)      
(************************************************)
and compute_flwr_free cfls wc ob ret =
  match cfls with
      [] -> compute_free_where wc ob ret
    | CELET(_, vn, bound) :: rest ->
	(compute_free_vars bound)
	@ (remove_bindings vn (compute_flwr_free rest wc ob ret))
    | CEFOR(_, vn, pos, bound) :: rest -> 
	let vn_free = remove_bindings vn (compute_flwr_free rest wc ob ret) in
	let binding = 
	  match pos with 
	      None -> vn_free
	    | Some v -> remove_bindings v vn_free
	in
	  (compute_free_vars bound)
	  @ binding

and compute_free_where wc ob ret =
  match wc with 
    | None -> compute_free_orderby ob ret
    | Some wc ->
	(compute_free_vars wc) @
	(compute_free_orderby ob ret)

and compute_free_orderby ob ret =
  match ob with 
    | None -> compute_free_vars ret
    | Some (_, ospec, osig) -> 
	List.concat 
	  (List.map (fun (ce, _, _) ->
		       compute_free_vars ce) ospec) @
	compute_free_vars ret

(***********************************************)
(* Compute Free Variables for insert locations *)
(***********************************************)
and compute_free_vars_insert_location il =
  match il with 
    | CUAsLastInto ce  
    | CUAsFirstInto ce 
    | CUInto ce
    | CUAfter ce
    | CUBefore ce -> compute_free_vars ce

let annotate_free_vars stat_ctxt acexpr =
  ignore(compute_free_vars acexpr);
  stat_ctxt

