(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_tpnf_util.ml,v 1.15 2007/05/21 20:22:39 mff Exp $ *)

(* Module: Factorize_tpnf_util
   Description:
     This module contains utilities for the TPNF.
*)

open Error

open Namespace_names
open Namespace_builtin

open Datatypes

open Dm_atomic
open Dm

open Norm_util

open Xquery_common_ast
open Xquery_common_ast_util

open Xquery_core_ast
open Xquery_core_ast_util
open Xquery_core_ast_annotation

open Ast_walker_rewrite_context
open Ast_walker_rewrite

open Processing_context
open Rewriting_judgments

open Schema_builtin
open Schema_util
open Print_top

(* *** Helper functions *** *)
let debug_apply s ce ce' =
  let print_annot ff a =
    begin
    let (ta, da, fv, sc)  = Xquery_core_ast_annotation.annot_components a in 
    Format.fprintf ff "[sc:%s]%!" (Xquery_core_ast_annotation.print_scrambling_annot sc);
    Format.fprintf ff "[type: ";
    (match ta with
    | None -> Format.fprintf ff "None]"
    | Some m -> Format.fprintf ff "%a]%!" Print_type_core.print_cxtype m);
    end
  in
  if !Conf.print_algebra_optimization_rewrite then
    begin
      Format.fprintf !Conf.algebra_optimization_rewrite_formatter 
	"@?**** TPNF RULE APPLIED: %s ****%!@.@\nBefore:@\n%a%!@.After:@\n%a%!@.\n" s
	(fun ff x -> Print_xquery_core.print_cexpr ff x print_annot) ce
	(fun ff x -> Print_xquery_core.print_cexpr ff x print_annot) ce'
    end

let get_opt_sbdo_arg ce =
  match ce.pcexpr_desc with
  | CECall(fname, [arg], sign, x, selfrecur) 
    when rqname_equal fname fs_distinct_docorder ||
         rqname_equal fname fs_distinct_docorder_or_atomic_sequence ->
      Some (fname, arg, sign, x)
  | _ -> None

let get_opt_sbdo_arg_desc ce =
  match get_opt_sbdo_arg ce with
  | Some (_,a,_,_) -> Some a.pcexpr_desc
  | None -> None

let get_one_arg_from_call ce =
  match ce.pcexpr_desc with
  | CECall(fname, [arg], sign, x, selfrecur) -> arg
  | _ -> raise (Query (Internal_Error ("Argument count mismatch in get_one_arg_from_call")))

let wrap_in_sbdo ctxt ce =
  let norm_ctxt = 
    Typing_context.norm_context_from_stat_context (get_context ctxt)  in
  let (it, ot), opt_fun_kind, upd = Norm_context.one_sig_from_norm_context norm_ctxt 
      (fs_distinct_docorder, 1) in
  let it' = List.map (fun t -> Some t) it in
    fmkacexpr (CECall(fs_distinct_docorder, [ce], (it',ot), upd, false))
    (empty_ast_annot()) ce.pcexpr_origin ce.pcexpr_loc

let wrap_in_fn_boolean ctxt ce =
  let norm_ctxt = 
    Typing_context.norm_context_from_stat_context (get_context ctxt)  in
  let (it, ot), opt_fun_kind, upd = Norm_context.one_sig_from_norm_context norm_ctxt (fn_boolean, 1) in
  let it' = List.map (fun t -> Some t) it in
  let fn_bool = 
    fmkacexpr (CECall(fn_boolean, [ce], (it',ot), upd, false))
      (empty_ast_annot()) ce.pcexpr_origin ce.pcexpr_loc
  in
  let _ = set_scrambling_annot fn_bool.pcexpr_annot List in
  fn_bool

let var_name_equals ce name =
  match ce.pcexpr_desc with
  | CEVar name' -> name = name'
  | _ -> false

let is_free_var_of vname cexpr =
  let free = Ast_walker_rewrite.free_variables cexpr in
  List.mem vname free

let has_max_one_robust ctxt ce =
  try
    has_max_one ce
  with Query _ ->
    false
(*
    let ctxt' = Ast_walker_rewrite_context.get_context ctxt in
    let t = Typing_expr.type_cexpr ctxt' ce in
    let _ = set_type_annot ce.pcexpr_annot t in
    has_max_one ce
*)

(*** OBSERVE DOC ORDER ***********************************************)
(* An expression does not observe docorder if it is in the following *)
(* fragment :                                                        *)
(*    EXPR  :=  DOC | VAR | FLWOR | STEP                             *)
(*    STEP  := AXIS::NT                                              *)
(*    FLOWR := ("for" VAR "in" EXPR | "let" VAR ":=" EXPR)+          *)
(*             ("where" EXPR)?                                       *)
(*              "return" EXPR                                        *)
(*    DOC   := doc-call | root-call                                  *)
(*                                                                   *)
(*  maybe we can extend the fragment -- Ph                           *)
(*********************************************************************)

let rec observes_doc_order ce =
  match ce.pcexpr_desc with
  | CEFLWOR ([CEFOR(t_ce1,n_ce1,None,ce1)], _, None, ce2) ->
      observes_doc_order ce2
  | CECall (fname,[arg],sign,u, selfrecur) ->
      if rqname_equal fname fn_doc then false
      else if rqname_equal fname fn_root then false
      else if rqname_equal fname fs_distinct_docorder then false
      else if rqname_equal fname fs_distinct_docorder_or_atomic_sequence then false
      else true
  | CEForwardAxis (v, axis, nt) 
  | CEReverseAxis (v, axis, nt) -> false
  | CEVar v -> false
  | _ -> true

let is_step op =
  match op.pcexpr_desc with
  | CECall (fname, [arg], sign,u, selfrecur) 
    when rqname_equal fname fn_boolean  ->
      begin
	match arg.pcexpr_desc with
	| CEForwardAxis _ | CEReverseAxis _ -> true
	| _ -> false
      end
  | _ -> false

let mk_fn_false rewrite_ctxt eh fi =
  let stat_ctxt = get_context rewrite_ctxt in 
  (*let proc_ctxt = Typing_context.processing_context_from_stat_context stat_ctxt in*)
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  Norm_util.build_core_false norm_ctxt eh fi

(* Helper function for getting the selection (or 
 * root) variable for a subexpression
 *)
let rec get_select_var ce =
  match ce.pcexpr_desc with
  | CEIf (e1,e2,e3) ->
      begin
	match e3.pcexpr_desc with
	| CEEmpty -> get_select_var e2
	| _ -> None
      end
  | CEFLWOR ([CEFOR(t_x,n_x,None,e1)], None, None, e3) ->
      begin
	let s1 = get_select_var e1 in
	let s3 = get_select_var e3 in
	match s1, s3 with
	| Some s1', Some s3' ->
	    if rqname_equal s3' n_x 
	    then s1
	    else s3
	| _ -> None
      end
  | CEFLWOR ([CELET(t_x,n_x,e1)], None, None, e3) ->
      begin
	let s1 = get_select_var e1 in
	let s3 = get_select_var e3 in
	match s1, s3 with
	| Some s1', Some s3' ->
	    if rqname_equal s3' n_x 
	    then s1
	    else s3
	| _ -> None
      end
  | CECall (fname, [arg], sign,u, selfrecur) 
    when rqname_equal fname fs_distinct_docorder ||
         rqname_equal fname fs_distinct_docorder_or_atomic_sequence || 
	 rqname_equal fname fn_boolean ->
      begin
	get_select_var arg
      end
  | CEForwardAxis (vname, axis, nt) 
    when axis = Child || axis = Descendant_or_self || axis = Descendant ->
      Some vname
  | CEVar vname -> 
      Some vname
  | _ -> None



(* ** Check wether expression is in the CXQ+ fragment ** *)
let rec is_in_cxq_plus ctxt ce =
  match ce.pcexpr_desc with
  | CEFLWOR ([CEFOR(t_x,n_x,None,e1)], None, None, e2)
  | CEFLWOR ([CELET(t_x,n_x,e1)], None, None, e2) ->
      is_in_cxq_plus ctxt e1 && is_in_cxq_plus ctxt e2
  | CEIf (e1,e2,e3) ->
      begin
	match e3.pcexpr_desc with 
	| CEEmpty ->  is_in_cxq_plus ctxt e1 && is_in_cxq_plus ctxt e2
	| _ -> false
      end
  | CECall (fname, [arg], sign,u, selfrecur) when
      rqname_equal fname fs_distinct_docorder ||
      rqname_equal fname fs_distinct_docorder_or_atomic_sequence ||
      rqname_equal fname fn_boolean 
      -> is_in_cxq_plus ctxt arg
  | CECall (fname, [arg], sign,u, selfrecur) when
      rqname_equal fname fn_doc || 
      rqname_equal fname fn_root -> false  (* inlining doc-calls = bad idea!;-) *)
  | CEForwardAxis (vname, axis, nt) 
  | CEReverseAxis (vname, axis, nt) -> true
      (* Warning: need to double-check that allowing all axes here does not *)
      (*          break any of the rewrites                                 *)
  | CEVar vname -> true
(*  | CEDocument _ | CEElem _ | CEAnyElem _ | CEAttr _ | CEAnyAttr _ -> true *)
  | _ -> (* has_max_one_robust ctxt ce *) false

(* Helper function for detemining wether the
 * no2d, gen, ord and nodup properties (in that order)
 * hold for a given expression.
 *)
let rec get_properties ctxt ce symtab =
  let sink = false, false, false, false in

(*  let ff = !Conf.core_expr_formatter in *)
(*  let _ = Format.fprintf ff "==============Getting props for: \n" in *)
(*  let _ = Print_xquery_core.print_cexpr ff ce (fun a -> fun x -> ()) in *)
(*  let _ = Format.fprintf ff "\n==============\n" in *)

  match ce.pcexpr_desc with
  | CEIf (e1, e2, e3) ->
      begin
	match e3.pcexpr_desc with
	| CEEmpty -> 
	    let no2d, gen, ord, nodup = get_properties ctxt e2 symtab in
(*	    let _ = Format.fprintf ff "\t[%b;%b;%b;%b;]\n" no2d gen ord nodup in *)
	    no2d, gen, ord, nodup
	| _ -> sink
      end
  | CEFLWOR ([CEFOR(t_x,n_x,None,e1)], None, None, e3) ->
      begin
	let no2d1, gen1, ord1, nodup1 = get_properties ctxt e1 symtab in
	let symtab' = Hashtbl.copy symtab in
	let _ = Hashtbl.add symtab' n_x (true,true,true,true) in
	let no2d3, gen3, ord3, nodup3 = get_properties ctxt e3 symtab' in

	(* make sure to remain in 'sink' if one if the subexprs returns 'sink' *)
        if not (no2d1 || gen1 || ord1 || nodup1) 
           || not (no2d3 || gen3 || ord3 || nodup3)
	then
	  sink
	else

	  match get_select_var e3 with
	  | Some sel_var ->
	      let no2d = no2d3 && (( (*(rqname_equal sel_var n_x) &&*) no2d1) 
				 || not (rqname_equal sel_var n_x)) in
	      let gen = gen3 && (( (*(rqname_equal sel_var n_x) &&*) gen1)
			       || not (rqname_equal sel_var n_x)) in
	      let ord = ((rqname_equal sel_var n_x) 
			   && ((ord1 && no2d3) || (ord1 && gen1 && nodup1 && ord3)))
                          || (not (rqname_equal sel_var n_x)  
                         && (no2d3 || no2d1 && nodup1 && ord3)) in
	      let nodup = ((rqname_equal sel_var n_x) && nodup1 && gen1 && nodup3)
  	                || (not (rqname_equal sel_var n_x)  && no2d1 && nodup1 && nodup3)
	      in 
	      (* let _ = Format.fprintf ff "\t[%b;%b;%b;%b;]\n" no2d gen ord nodup in *)
	      no2d, gen, ord, nodup
	  | _ -> 
	      (* let _ = Format.fprintf ff "No select var, return \t[SINK]\n" in *)
	      sink
      end
  | CEFLWOR ([CELET(t_x,n_x,e1)], None, None, e3) ->
      begin
	let symtab' = Hashtbl.copy symtab in
	let props = get_properties ctxt e1 symtab in
	let _ = Hashtbl.add symtab' n_x props in
	get_properties ctxt e3 symtab'
      end
  | CECall (fname, [arg], sign,u, selfrecur) ->
    if rqname_equal fname fs_distinct_docorder ||
         rqname_equal fname fs_distinct_docorder_or_atomic_sequence 
    then
      begin
	let no2d, gen, ord, nodup = get_properties ctxt arg symtab in
	(* let _ = Format.fprintf ff "\t[%b;%b;%b;%b;]\n" no2d gen true true in *)
	no2d, gen, true, true
      end
    else if rqname_equal fname fn_boolean 
    then
      let no2d, gen, ord, nodup = get_properties ctxt arg symtab in
      (* let _ = Format.fprintf ff "\t[%b;%b;%b;%b;]\n" no2d gen ord nodup in *)
      no2d, gen, ord, nodup
    else if  rqname_equal fname fn_root ||  rqname_equal fname fn_doc then
      (* EXTENSION *)
      begin
	true, true, true, true
      end
    else
       if has_max_one_robust ctxt ce then
	 (* let _ = Printf.printf "\t[MAXONE]\n" in *)
	 true, true, true, true
       else
	 (* let _ = Printf.printf "\t[SINK]\n" in *)
	 sink
  | CEForwardAxis (vname, axis, nt) 
    when axis = Child || axis = Descendant_or_self || axis = Descendant ->
      begin
	match axis with
	| Child ->
	    (* let _ = Format.fprintf ff "\t[%b;%b;%b;%b;]\n" false true true true in *)
	    false, true, true, true
	| Descendant_or_self | Descendant ->
	    (* let _ = Format.fprintf ff "\t[%b;%b;%b;%b;]\n" false false true true in *)
	    false, false, true, true
	| _ -> sink
      end
  | CEVar vname ->
      begin
	try
	  let no2d, gen, ord, nodup = Hashtbl.find symtab vname in
	  (* let _ = Format.fprintf ff "\t[%b;%b;%b;%b;]\n" no2d gen ord nodup in *)
	  no2d, gen, ord, nodup
	with Not_found ->
	  (* externally declared? *)
	  if has_max_one_robust ctxt ce then true, true, true, true
	  else sink
      end
(*  | CEDocument _ | CEElem _ | CEAnyElem _ | CEAttr _ | CEAnyAttr _ ->
      (* EXTENSION *)
      begin
	true, true, true, true
      end *)
  | _ -> 
      (* Check type here for maxone? *)
      if has_max_one_robust ctxt ce && is_in_cxq_plus ctxt ce then
	(* let _ = Printf.printf "\t[MAXONE]\n" in *)
	true, true, true, true
      else
	(* let _ = Printf.printf "\t[SINK]\n" in *)
	sink



let is_max_one_expr c =
  match c.pcexpr_desc with
  | CEDocument _ | CEElem _ | CEAnyElem _ | CEAttr _ | CEAnyAttr _ -> true 
  | CECall (fname, [arg], sign,u, selfrecur) -> 
      (rqname_equal fname fn_doc || rqname_equal fname fn_root)
  | _ -> false

(* ** Check wether expression is in TPNF ** *)
let is_in_tpnf ce =

  let rec is_fp c =
    begin
      match c.pcexpr_desc with
      | CEIf (c1, c2, c3) ->
	  begin
	    match c3.pcexpr_desc with 
	    | CEEmpty -> 
		begin
		  match c1.pcexpr_desc with
		  | CECall (fname, [arg],_,_,_) 
		    when rqname_equal fname fn_boolean ->
		      is_tp arg && is_fp c2
		  | _ -> 
		      false
		end
	    | _ -> 
		false
	  end
      | _ -> 
          is_otp c
    end
  and is_tp c =
    begin
      match c.pcexpr_desc with
      | CEFLWOR([CEFOR(t_x,n_x,None,c1)], None, None, c2) ->
	  if rqname_equal n_x fs_dot then
	    begin
	      match c1.pcexpr_desc with
	      | CEVar _ -> is_rc c2
	      | _ -> is_max_one_expr c1 && is_rc c2
	    end
	  else
	    false
      | CEVar _ -> 
	  true
      | _ -> is_atp c || is_max_one_expr c
    end
  and is_otp c =
    begin
      match c.pcexpr_desc with
      | CEFLWOR([CEFOR(t_x,n_x,None,c1)], None, None, c2) -> 
	  if rqname_equal n_x fs_dot then
	    begin
	      match c1.pcexpr_desc with
	      | CEVar _ -> is_orc c2
	      | _ -> is_max_one_expr c1 && is_orc c2
	    end
	  else
	    false
      | CEVar _ -> true
      | _ -> 
	  is_aotp c || is_max_one_expr c
    end
  and is_atp c =
    begin
      match c.pcexpr_desc with
      | CEForwardAxis (v,a,nt) | CEReverseAxis(v,a,nt) -> true
      | CEFLWOR([CEFOR(t_x,n_x,None,c1)], None, None, c2) -> 
	  if rqname_equal n_x fs_dot then
	    begin
	      match c1.pcexpr_desc with
	      | CEForwardAxis (v,a,nt) | CEReverseAxis(v,a,nt) -> is_rc c2
	      | _ -> false
	    end
	  else
	    false
      | _ -> 
         false
    end
  and is_aotp c =
    begin
      match c.pcexpr_desc with
      | CEForwardAxis (v,a,nt) | CEReverseAxis(v,a,nt) -> true
      | CEFLWOR([CEFOR(t_x,n_x,None,c1)], None, None, c2) -> 
	  if rqname_equal n_x fs_dot then
	    begin
	      match c1.pcexpr_desc with
	      | CEForwardAxis (v,a,nt) | CEReverseAxis(v,a,nt) -> is_orc c2
	      | _ -> false
	    end
	  else
	    false
      | _ -> 
         false
    end
  and is_rc c =
    begin
      match c.pcexpr_desc with
      | CEIf (c1, c2, c3) ->
	  begin
	    match c3.pcexpr_desc with 
	    | CEEmpty -> 
		begin
		  match c1.pcexpr_desc with
		  | CECall (fname, [arg],_,_,_) 
		    when rqname_equal fname fn_boolean ->
		      is_atp arg && is_rc c2
		  | _ -> 
		      false
		end
	    | _ -> 
		false
	  end
      | _ -> 
          is_atp c
    end
  and is_orc c =
    begin
      match c.pcexpr_desc with
      | CEIf (c1, c2, c3) ->
	  begin
	    match c3.pcexpr_desc with 
	    | CEEmpty -> 
		begin
		  match c1.pcexpr_desc with
		  | CECall (fname, [arg],_,_,_) 
		    when rqname_equal fname fn_boolean ->
		      begin
			match c2.pcexpr_desc with
			| CEVar v when rqname_equal v fs_dot -> is_atp arg
			| _ -> is_atp arg && (is_max_one_expr c2 || is_orc c2)
		      end
		  | _ -> 
		      false
		end
	    | _ -> 
		false
	  end
      | _ -> 
          is_aotp c
    end
  in
  is_fp ce

(* ** Check wether expression is in TPNF ** *)
let is_in_tpnf' ce =

  let rec is_fp c =
    begin
      match c.pcexpr_desc with
      | CEIf (c1, c2, c3) ->
	  begin
	    match c3.pcexpr_desc with 
	    | CEEmpty -> 
		begin
		  match c1.pcexpr_desc with
		  | CECall (fname, [arg],_,_,_) -> 
		      rqname_equal fname fn_boolean && is_tp arg && is_fp c2
		  | _ -> false
		end
	    | _ -> false
	  end
      | _ -> is_otp c
    end
  and is_tp c =
    begin
      match c.pcexpr_desc with
      | CEFLWOR([CEFOR(t_x,n_x,None,c1)], None, None, c2) ->
	  rqname_equal n_x fs_dot && is_tp c1 && is_rc c2
      | CEVar _ -> true
      | _ -> is_atp c || is_max_one_expr c
    end
  and is_otp c =
    begin
      match c.pcexpr_desc with
      | CEFLWOR([CEFOR(t_x,n_x,None,c1)], None, None, c2) -> 
	  rqname_equal n_x fs_dot && is_tp c1 && is_orc c2
      | CEVar _ -> true
      | _ -> is_aotp c || is_max_one_expr c
    end
  and is_atp c =
    begin
      match c.pcexpr_desc with
      | CEForwardAxis (v,a,nt) | CEReverseAxis(v,a,nt) -> true
      | CEFLWOR([CEFOR(t_x,n_x,None,c1)], None, None, c2) -> 
	  rqname_equal n_x fs_dot && is_atp c1 && is_rc c2
      | _ -> false
    end
  and is_aotp c =
    begin
      match c.pcexpr_desc with
      | CEForwardAxis (v,a,nt) | CEReverseAxis(v,a,nt) ->  true
      | CEFLWOR([CEFOR(t_x,n_x,None,c1)], None, None, c2) -> 
	  rqname_equal n_x fs_dot && is_atp c1 && is_orc c2
      | _ -> false
    end
  and is_rc c =
    begin
      match c.pcexpr_desc with
      | CEForwardAxis (v,a,nt) | CEReverseAxis(v,a,nt) ->  true
      | CEIf (c1, c2, c3) ->
	  begin
	    match c3.pcexpr_desc, c1.pcexpr_desc  with 
	    | CEEmpty, CECall (fname, [arg],_,_,_) -> 
		rqname_equal fname fn_boolean && is_atp arg && is_rc c2
	    | _ -> false
	  end
      | _ -> 
          is_atp c
    end
  and is_orc c =
    begin
      match c.pcexpr_desc with
      | CEIf (c1, c2, c3) ->
	  begin
	    match c3.pcexpr_desc, c1.pcexpr_desc  with 
	    | CEEmpty, CECall (fname, [arg],_,_,_) when rqname_equal fname fn_boolean ->
		begin
		match c2.pcexpr_desc with
		| CEVar v when rqname_equal v fs_dot -> is_atp arg
		| _ -> (is_atp arg) && (is_orc c2 || is_max_one_expr c2)
		end
	    | _ -> 
		false
	  end
      | _ -> 
          is_aotp c
    end
  in
  is_fp ce



 
