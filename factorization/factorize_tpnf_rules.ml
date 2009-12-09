(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_tpnf_rules.ml,v 1.18 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Factorize_tpnf_rules
   Description:
     This module contains the rewriting rules used for the TPNF.
*)

open Error

open Namespace_names
open Namespace_builtin
open Norm_util

open Xquery_common_ast
open Xquery_common_ast_util

open Xquery_core_ast
open Xquery_core_ast_util
open Xquery_core_ast_annotation

open Processing_context
open Rewriting_judgments

open Schema_builtin
open Schema_util
open Print_top

open Factorize_tpnf_util

(* ** Utility functions -- move to factorize_tpnf_util ** *)
let has_bag_sem ce = 
  match get_scrambling_annot ce.pcexpr_annot with
  | Bag -> true
  | _   -> false

let has_bag_annot ce = has_bag_sem ce

let has_set_sem ce = 
  match get_scrambling_annot ce.pcexpr_annot with
  | Set | Bag -> true
  | _   -> false

let has_set_annot ce =
  match get_scrambling_annot ce.pcexpr_annot with
  | Set -> true
  | _   -> false

let annot_lt ce1 ce2 =
  match get_scrambling_annot ce1.pcexpr_annot,
        get_scrambling_annot ce2.pcexpr_annot
  with
  | List, Set 
  | List, Bag 
  | Set, Bag  -> true
  | _ -> false


(* ** debugging stuff ** *)
let print_c_list c =
  let ff = !Conf.core_expr_formatter in 
  let print_annot ff a =
    let (ta, da, fv, sc, st)  = Xquery_core_ast_annotation.annot_components a in 
    Format.fprintf ff "[sc:%s]%!" (Xquery_core_ast_annotation.print_scrambling_annot sc)
  in
  begin
    Format.fprintf ff "********** LIST ***********\n";
    let _ = List.map (fun (a,b,c,d) -> 
                  begin
                    Print_xquery_core.print_cexpr ff a print_annot;
                    Format.fprintf ff "\n---------\n";
		  end ) c in
    Format.fprintf ff "\n********** END ***********\n";
  end

(* *** utility functions *** *)
let rec conjunction_to_list c =
  try
    begin
       match c.pcexpr_desc with
      | CEIf(c1,c2,c3) ->
	  begin
	    match c3.pcexpr_desc with
	    | CEEmpty ->
		(c1, c.pcexpr_annot, c.pcexpr_origin, c.pcexpr_loc):: conjunction_to_list c2
	    | _ -> raise (Query (Internal_Error ("Not a pure conjunction.")))
	  end
      | _ -> 
	  [(c, c.pcexpr_annot, c.pcexpr_origin, c.pcexpr_loc)]
    end
  with _ -> []
	  
let rec list_to_conjunction l last_needs_bool =
  match l with
  | [tl] -> 
      let r, annot, ori, loc = tl in 
      if last_needs_bool then
	r
      else
	let r' = 
	  match r.pcexpr_desc with
	  | CECall (fn, [arg], s,u, selfrecur) when rqname_equal fn fn_boolean -> arg
	  | _ -> r
	in r'
  | hd :: tl ->
      begin 
	let ce1, annot, ori, loc = hd in
	let empty = fmkacexpr CEEmpty annot ori loc in
	let r = fmkacexpr (CEIf (ce1, list_to_conjunction tl last_needs_bool, empty))
	    annot ori loc in
	let _ = set_scrambling_annot ce1.pcexpr_annot Bag in r
      end
  | _ -> raise (Query (Internal_Error ("This function should not be called with an empty list.")))

(* *** *** PHASE ONE : Inserting DDO's and converting where clauses *** *** *)
let convert_where_clause rewrite_ctxt cexpr =
  match cexpr.pcexpr_desc with
  | CEFLWOR ([CEFOR(t_x,n_x,None,e1)], Some e2, None, e3) ->
      begin
	let ce_empty = fmkacexpr CEEmpty e2.pcexpr_annot e2.pcexpr_origin e2.pcexpr_loc in
	let if_expr = fmkacexpr (CEIf (e2, e3, ce_empty)) 
	    (empty_ast_annot()) e2.pcexpr_origin  e2.pcexpr_loc in
	let r = fmkacexpr (CEFLWOR ([CEFOR(t_x,n_x,None,e1)], None, None, if_expr))
	    cexpr.pcexpr_annot cexpr.pcexpr_origin  cexpr.pcexpr_loc in	
	let _ = debug_apply "convert where clause" cexpr r in 
	r, true
      end
  | CEFLWOR ([CELET(t_x,n_x,e1)], Some e2, None, e3) ->
      begin
	let ce_empty = fmkacexpr CEEmpty e2.pcexpr_annot e2.pcexpr_origin e2.pcexpr_loc in
	let if_expr = fmkacexpr (CEIf (e2, e3, ce_empty)) 
	    e2.pcexpr_annot e2.pcexpr_origin  e2.pcexpr_loc in
	let r = fmkacexpr (CEFLWOR ([CELET(t_x,n_x,e1)], None, None, if_expr))
	    cexpr.pcexpr_annot cexpr.pcexpr_origin  cexpr.pcexpr_loc in	
	let _ = debug_apply "convert where clause" cexpr r in 
	r, true
      end
  | _ -> cexpr, false


(* *** Decompose FLWOR blocks *** *)
let decompose_flwor ctxt ce =
  let rec decompose_flwor_list lst wh ob ret =
    match lst with
    | [last] ->
	begin
	  fmkacexpr (CEFLWOR ([last], wh, ob, ret))
	    ce.pcexpr_annot ce.pcexpr_origin  ce.pcexpr_loc	
	end
    | hd::tl -> 
	begin
	  let ret' = decompose_flwor_list tl wh ob ret in
	  fmkacexpr (CEFLWOR ([hd], None, None, ret'))
	    ce.pcexpr_annot ce.pcexpr_origin  ce.pcexpr_loc
	end
    | _ -> raise (Query (Internal_Error (
			 "Empty for/let list found during TPNF FLWOR decomposition")))
  in
  match ce.pcexpr_desc with
  | CEFLWOR (flwor_list, wh, ob, ret) 
    when (List.length flwor_list) > 1 ->
      decompose_flwor_list flwor_list wh ob ret, true
  | _ -> ce, false

(* *** Insert SBDO (ord/nodup) *** *)
(* Insert SBDO: if for a given subexpression, the ord and nodup       *)
(* properties can be derived, then insert an sbdo operation           *)
(* only apply sbdo's on non-scrambled CELet/CEFor                     *)
let is_flwor ce =
  match ce.pcexpr_desc with
  | CEFLWOR _ -> true
  | _ -> false

let is_if ce =
  match ce.pcexpr_desc with
  | CEIf _ -> true
  | _ -> false


let insert_sbdo_xpath rewrite_ctxt ce =
  (* testing scrambling to avoid recursive addition of SBDO's *)
  if not ( has_set_sem ce ) && (is_flwor ce || is_if ce) then
    let symtab = Hashtbl.create 10 in 
    let no2d, gen, ord, nodup = get_properties rewrite_ctxt ce symtab in
    if ord && nodup then
      let r = wrap_in_sbdo rewrite_ctxt ce in
      let _ = set_scrambling_annot ce.pcexpr_annot Set in
      let _ = debug_apply "insert distinct-docorder (ord/nodup holds)" ce r in 
      r, true
    else ce, false
  else ce, false

(* *** *** PHASE TWO : Inserting and propagating annotations *** *** *)

let insert_scrambling_ddo rewrite_ctxt cexpr = 
  match cexpr.pcexpr_desc with
  | CECall (fname, [arg], sign,u, selfrecur)
    when rqname_equal fname fs_distinct_docorder || 
         rqname_equal fname fs_distinct_docorder_or_atomic_sequence ->
      begin
	match get_scrambling_annot arg.pcexpr_annot with
	| List ->
	    let _ = set_scrambling_annot arg.pcexpr_annot Set in
	    let _ = debug_apply "insert scrambling (ddo)" cexpr cexpr in 
	    cexpr, true
	| _ ->
	    (* already scrambled *)
	    cexpr, false
      end
  | _ -> cexpr, false


let insert_scrambling_if rewrite_ctxt cexpr = 
  match cexpr.pcexpr_desc with
  | CEIf (ce1, ce2, cempty) ->
      begin
	match get_scrambling_annot ce1.pcexpr_annot with
	| List | Set ->
	    let _ = set_scrambling_annot ce1.pcexpr_annot Bag in
	    let _ = debug_apply "insert scrambling (if)" cexpr cexpr in 
	    cexpr, true
	| Bag ->
	    (* already scrambled *)
	    cexpr, false
      end
  | _ -> cexpr, false  



let propagate_scrambling_bool rewrite_ctxt ce = 
  let changed = ref false in
  let _ = 
    match ce.pcexpr_desc with
    | CECall (fname, [arg], sign,u, selfrecur)
      when rqname_equal fname fn_boolean ->
	begin
	  match get_scrambling_annot arg.pcexpr_annot with
	  | Bag -> ()
	  | _ when annot_lt arg ce ->
	      (set_scrambling_annot arg.pcexpr_annot (get_scrambling_annot ce.pcexpr_annot);
	       changed := true)
	  | _ -> ()
	end
    | _ -> ()
  in
  if !changed 
  then 
    let _ = debug_apply "propagate scrambling (fn_boolean)" ce ce in 
    ce, true 
  else ce, false


let propagate_scrambling_ddo rewrite_ctxt ce = 
  let changed = ref false in
  let _ = 
    match ce.pcexpr_desc with
    | CECall (fname, [arg], sign,u, selfrecur)
      when rqname_equal fname fs_distinct_docorder || 
      rqname_equal fname fs_distinct_docorder_or_atomic_sequence ->
	begin
	  if annot_lt arg ce then
	    (set_scrambling_annot arg.pcexpr_annot (get_scrambling_annot ce.pcexpr_annot);
	     changed := true)
	  else ()
	end
    | _ -> ()
  in
  if !changed 
  then 
    let _ = debug_apply "propagate scrambling (ddo)" ce ce in 
    ce, true 
  else ce, false


let propagate_scrambling_for_let rewrite_ctxt ce = 
  let changed = ref false in
  let _ = 
    match ce.pcexpr_desc with
    | CEFLWOR ([CEFOR(t_x,n_x,None,e1)], None, None, e3) 
    | CEFLWOR ([CELET(t_x,n_x,e1)], None, None, e3) 
      when annot_lt e3 ce ->
	(set_scrambling_annot e3.pcexpr_annot (get_scrambling_annot ce.pcexpr_annot);
	 changed := true)
    | _ -> ()
  in
  if !changed 
  then 
    let _ = debug_apply "propagate scrambling in return clause (for/let)" ce ce in 
    ce, true 
  else ce, false

let propagate_scrambling_for rewrite_ctxt ce = 
  let changed = ref false in
  let _ = 
    match ce.pcexpr_desc with
    | CEFLWOR ([CEFOR(t_x,n_x,None,e1)], None, None, e3) ->
	begin
	  match get_scrambling_annot e1.pcexpr_annot with
	  | List ->
	      begin
		match get_scrambling_annot ce.pcexpr_annot with
		| List -> ()
		| _ -> 
		    (set_scrambling_annot e1.pcexpr_annot Set;
		     changed := true)
	      end
	  | _ -> ()
	end
    | _ -> ()
  in
  if !changed 
  then 
    let _ = debug_apply "propagate scrambling in for clause" ce ce in 
    ce, true 
  else ce, false

let propagate_scrambling_if rewrite_ctxt ce = 
  let changed = ref false in
  let _ = 
    match ce.pcexpr_desc with
    | CEIf (e1, e2, empty) when annot_lt e2 ce ->
	begin
	  set_scrambling_annot e2.pcexpr_annot (get_scrambling_annot ce.pcexpr_annot);
	  changed := true
	end;
    | _ -> ()
  in
  if !changed 
  then 
    let _ = debug_apply "propagate scrambling if" ce ce in 
    ce, true 
  else ce, false



(* *** *** PHASE 3 : Structural Manipulation *** *** *)

let remove_scrambled_sbdo rewrite_ctxt cexpr = 
  match cexpr.pcexpr_desc with
  | CECall (fname, [arg], sign,u, selfrecur) ->
    if  rqname_equal fname fs_distinct_docorder 
        || rqname_equal fname fs_distinct_docorder_or_atomic_sequence 
    then
      begin
      match get_scrambling_annot cexpr.pcexpr_annot with
      | List -> cexpr, false
      | Set | Bag -> 
	  let _ = debug_apply "remove scrambled distinct-docorder" cexpr arg in 
	  arg, true
      end
    else cexpr, false
  | _ -> cexpr, false
	
(* ** Substitution ** 
 *
 * *let $x := e1 return e2 == e1[$x/e2]
 *
 * if e1 and e2 are in CXQ+
 * *)

(* Q: Should be apply substitution only when
      the use count is one?
   A: No, see paper
*)

(* Small Hack to avoid substitution of doc/root calls *)
let is_doc_or_root_call ce =
  match ce.pcexpr_desc with
  | CECall (fname, _, _, _, _) ->
      begin
	rqname_equal fname fn_doc
      ||rqname_equal fname fn_root
      end
  | _ -> false  

let subst_count = ref 0

let new_subst_var () =
  let v = !subst_count in incr subst_count;
  (Namespace_builtin.glx_prefix, Namespace_builtin.glx_uri, ("subst" ^ (string_of_int v)))

(* rename a variable into a new, unique var name *)
(* it is the user's responsability to make sure  *)
(* that new variable is not used so far, by      *)
(* creating one with new_subst_var()             *)
let rec tpnf_rename ctxt e x y =
  if is_in_cxq_plus ctxt e then
    begin
      match e.pcexpr_desc with
      | CEVar v when rqname_equal x v  ->
	  let e' = fmkacexpr (CEVar y) 
	      (empty_ast_annot()) e.pcexpr_origin e.pcexpr_loc
	  in e', true
      | CEForwardAxis (v, a, nt)
	when rqname_equal v x ->
	  fmkacexpr (CEForwardAxis (y, a, nt)) 
	    e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, true
      | CEReverseAxis (v, a, nt)
	when rqname_equal v x ->
	  fmkacexpr (CEReverseAxis (y, a, nt)) 
	    e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, true
      | CEIf (ce1, ce2, ce3) ->
	  let ce1', ch1 = tpnf_rename ctxt ce1 x y in
	  let ce2', ch2 = tpnf_rename ctxt ce2 x y in
	  let ce3', ch3 = tpnf_rename ctxt ce3 x y in
	  fmkacexpr (CEIf (ce1', ce2', ce3'))
	    e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, (ch1 || ch2 || ch3)
      | CEFLWOR ([CEFOR(t,v,None,ce1)], None, None, ce2) ->
	  begin
	    let ce1', ch1 = tpnf_rename ctxt ce1 x y in
	    if rqname_equal v x then
	      begin
		if rqname_equal v y then
		  fmkacexpr (CEFLWOR ([CEFOR(t,y,None,ce1')], None, None, ce2))
		    e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, true
		else
		  let ce2', ch2 = tpnf_rename ctxt ce2 x y in
		  fmkacexpr (CEFLWOR ([CEFOR(t,y,None,ce1')], None, None, ce2'))
		    e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, true
	      end
	    else
	      let ce2', ch2 = tpnf_rename ctxt ce2 x y in
	      fmkacexpr (CEFLWOR ([CEFOR(t,v,None,ce1')], None, None, ce2'))
		e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, (ch1 || ch2) 
	  end
      | CEFLWOR ([CELET(t,v,ce1)], None, None, ce2) ->
	  begin
	    let ce1', ch1 = tpnf_rename ctxt ce1 x y in
	    if rqname_equal v x then
	      begin
		if rqname_equal v y then
		  fmkacexpr (CEFLWOR ([CELET(t,y,ce1')], None, None, ce2))
		    e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, true
		else
		  let ce2', ch2 = tpnf_rename ctxt ce2 x y in
		  fmkacexpr (CEFLWOR ([CELET(t,y,ce1')], None, None, ce2'))
		    e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, true
	      end
	    else
	      let ce2', ch2 = tpnf_rename ctxt ce2 x y in
	      fmkacexpr (CEFLWOR ([CELET(t,v,ce1')], None, None, ce2'))
		e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, (ch1 || ch2) 
	  end
      | CECall (fname, [arg], sign,u, selfrecur)
	when rqname_equal fname fs_distinct_docorder || 
	rqname_equal fname fs_distinct_docorder_or_atomic_sequence ||
	rqname_equal fname fn_boolean ->
	  begin
	    let se, ch = tpnf_rename ctxt arg x y in
	    fmkacexpr (CECall (fname, [se], sign, u, selfrecur))
	      e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, ch
	  end
      | _ -> e, false
    end
  else
    e, false

let rec tpnf_subst ctxt e x e' =
  if is_in_cxq_plus ctxt e && is_in_cxq_plus ctxt e' then
    begin
    match e.pcexpr_desc with
    | CEVar v ->
	begin
	  if rqname_equal x v 
	  then e', true
	  else e, false
	end
    | CEForwardAxis (v, a, nt) ->
	begin
	  match e'.pcexpr_desc with
	  | CEVar z when rqname_equal v x -> 
              (* distinction between variable renaming and substitution *)
	      begin
		fmkacexpr (CEForwardAxis (z, a, nt)) 
		  e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, true
	      end
	  | _ when rqname_equal v x ->
	      begin
		let axis = fmkacexpr (CEForwardAxis (fs_dot, a, nt)) 
		    e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc
		in
		fmkacexpr (CEFLWOR ([CEFOR(None,fs_dot,None,e')], None, None, axis))
		  e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, true
	      end
	  | _ -> e, false
	end
    | CEReverseAxis (v, a, nt) ->
	begin
	  match e'.pcexpr_desc with
	  | CEVar z when rqname_equal v x -> 
              (* distinction between variable renaming and substitution *)
	      begin
		fmkacexpr (CEReverseAxis (z, a, nt)) 
		  e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, true
	      end
	  | _ when rqname_equal v x ->
	      begin
		let axis = fmkacexpr (CEReverseAxis (fs_dot, a, nt)) 
		    e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc
		in
		fmkacexpr (CEFLWOR ([CEFOR(None,fs_dot,None,e')], None, None, axis))
		  e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, true
	      end
	  | _ -> e, false
	end
    | CEIf (b_ce1, ce2, ce3) ->
	begin
	  match b_ce1.pcexpr_desc, ce3.pcexpr_desc with 
	  | CECall (fname, [ce1], sign, u, selfrecur), CEEmpty 
	    when rqname_equal fname fn_boolean ->
	      begin
		let ce1', ch1 = tpnf_subst ctxt ce1 x e' in
		let ce2', ch2 = tpnf_subst ctxt ce2 x e' in
		let b_ce1' = fmkacexpr (CECall(fname, [ce1'], sign, u, selfrecur))
		    b_ce1.pcexpr_annot b_ce1.pcexpr_origin b_ce1.pcexpr_loc
		in
		fmkacexpr (CEIf (b_ce1', ce2', ce3))
		  e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, (ch1 || ch2)
	      end
	  | _ -> e, false
	end
    | CEFLWOR ([CEFOR(t,y,None,ce1)], None, None, ce2) ->
	begin
	  let ce1', ch1 = tpnf_subst ctxt ce1 x e' in
	  if rqname_equal x y then
	    fmkacexpr (CEFLWOR ([CEFOR(t,y,None,ce1')], None, None, ce2))
	      e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, ch1
	  else
	    let z = new_subst_var () in
	    let ce2', ch2 = tpnf_rename ctxt ce2 y z in
	    let ce2'', ch2' = tpnf_subst ctxt ce2' x e' in
	    fmkacexpr (CEFLWOR ([CEFOR(t,z,None,ce1')], None, None, ce2''))
	      e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, (ch1 || ch2') 
	end
    | CEFLWOR ([CELET(t,y,ce1)], None, None, ce2) ->
	begin
	  let ce1', ch1 = tpnf_subst ctxt ce1 x e' in
	  if rqname_equal x y then
	    fmkacexpr (CEFLWOR ([CELET(t,y,ce1')], None, None, ce2) )
	    e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, ch1
	  else
	    let z = new_subst_var () in
	    let ce2', ch2 = tpnf_rename ctxt ce2 y z in
	    let ce2'', ch2' = tpnf_subst ctxt ce2' x e' in
	    fmkacexpr (CEFLWOR ([CELET(t,z,ce1')], None, None, ce2'') )
	      e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, (ch1 || ch2') 
	end
    | CECall (fname, [arg], sign,u, selfrecur)
      when rqname_equal fname fs_distinct_docorder || 
      rqname_equal fname fs_distinct_docorder_or_atomic_sequence ||
      rqname_equal fname fn_boolean ->
	begin
	  let se, ch = tpnf_subst ctxt arg x e' in
	  fmkacexpr (CECall (fname, [se], sign,u, selfrecur))
	    e.pcexpr_annot e.pcexpr_origin e.pcexpr_loc, ch
	end
    | _ -> e, false
    end
  else e, false

let substitution rewrite_ctxt ce =
  match ce.pcexpr_desc with
  | CEFLWOR ([CELET(t,x,ce1)], None, None, ce2) 
    when has_set_sem ce (* && has_set_sem ce2 *) ->
      begin
	let r, c = tpnf_subst rewrite_ctxt ce2 x ce1 in
	if c then
	  let _ = debug_apply "substitution" ce r in
	  r, true
	else
	  ce, false
      end
  | _ -> ce, false
	
(* ** LOOP SPLIT ** 
 *
 *  *for $dot in e1      =    *for $dot in
 *   return              =       *for $dot in e1
 *     *for $dot in e2   =        return e2
 *      return e3        =     return e3
 *
 *)

let loop_split rewrite_ctxt flwor1 =
  let fail = flwor1, false in
  match flwor1.pcexpr_desc with
  | CEFLWOR ([CEFOR(t_ce1,x,None,ce1)], None, None, flwor2) 
    when has_set_sem flwor1 &&  has_set_annot ce1 && rqname_equal x fs_dot ->
      begin
	match flwor2.pcexpr_desc with
	| CEFLWOR ([CEFOR(t_ce2,y,None,ce2)], None, None, ce3) 
	  when has_set_sem flwor2 && has_set_annot ce2 && has_set_sem ce3 && 
	    rqname_equal y fs_dot ->
	      let flwor1' = 
		fmkacexpr (CEFLWOR ([CEFOR(t_ce1,x,None,ce1)], None, None, ce2)) 
		  (empty_ast_annot ()) flwor1.pcexpr_origin flwor1.pcexpr_loc in
	      let flwor2' = 
		fmkacexpr (CEFLWOR ([CEFOR(t_ce2,y,None,flwor1')], None, None, ce3)) 
		  (empty_ast_annot ()) flwor2.pcexpr_origin flwor2.pcexpr_loc in
	      let annot = get_scrambling_annot flwor1.pcexpr_annot in
	      let _ = set_scrambling_annot flwor1'.pcexpr_annot annot in
	      let _ = set_scrambling_annot flwor2'.pcexpr_annot annot in
	      let _ = debug_apply "loop split" flwor1 flwor2' in
	      flwor2', true
	| _ -> fail
      end
  | _ -> fail

(* ** NESTED LOOP SPLIT ** 
 *
 *  *for $dot in e1        =    *for $dot in
 *   return                =       *for $dot in e1
 *     *if e2 ^ ... ^ en   =        return 
 *     then                =          if e2 ^ ... ^ en
 *      for $dot in en+1   =          then en+1
 *      return en+2        =     return en+2
 *)
let nested_loop_split rewrite_ctxt flwor1 =
  let fail = flwor1, false in
  match flwor1.pcexpr_desc with
  | CEFLWOR ([CEFOR(t_ce1,x,None,ce1)], None, None, cond) 
    when has_set_sem flwor1 && has_set_annot ce1 && has_set_annot cond
	&& rqname_equal x fs_dot ->
      begin
	let boolean_list = conjunction_to_list cond in
	if List.length boolean_list > 1 then
	  let rev_list = List.rev boolean_list in
	  let rev_tail = List.tl rev_list in
	  let flwor2, _, _, _ = List.hd rev_list in
	  match flwor2.pcexpr_desc with
	  | CEFLWOR ([CEFOR(t_ce2,y,None,ce_n1)], None, None, ce_n2) 
	    when has_set_annot flwor2 && has_set_annot ce_n1 && rqname_equal y fs_dot ->
	      let r = (ce_n1, ce_n1.pcexpr_annot, ce_n1.pcexpr_origin, ce_n1.pcexpr_loc) in
	      let new_cond = List.rev (r :: rev_tail) in
	      let ceif' = list_to_conjunction new_cond false in
	      let flwor1' = 
		fmkacexpr (CEFLWOR ([CEFOR(t_ce1,x,None,ce1)], None, None, ceif')) 
		  (empty_ast_annot()) flwor1.pcexpr_origin flwor1.pcexpr_loc in
	      let flwor2' = 
		fmkacexpr (CEFLWOR ([CEFOR(t_ce2,y,None,flwor1')], None, None, ce_n2)) 
		  (empty_ast_annot()) flwor2.pcexpr_origin flwor2.pcexpr_loc in

	      let _ = set_scrambling_annot flwor1'.pcexpr_annot Set in
	      let _ = set_scrambling_annot flwor2'.pcexpr_annot 
		  (get_scrambling_annot flwor1.pcexpr_annot) in
	      let _ = debug_apply "nested loop split" flwor1 flwor2' in
	      flwor2', true
	  | _ -> fail
	else fail
      end
  | _ -> fail
	

(* *** Loop Fusion ***
 *  *for $y in       = *for $x in e1 
 *     *for $x in e1 = return
 *      return e2    =   *for $y in e2
 *   return e3       =   return e3
 *
 *  $x not in FV(e3) || $x = $y
 *)
let loop_fusion  ctxt ce =
  let fail = ce, false in
  match ce.pcexpr_desc with
  | CEFLWOR ([CEFOR(t_ce,y,None,ce')], None, None, e3) 
    when has_set_sem ce && rqname_equal y fs_dot && has_set_sem e3 ->
      begin
	match ce'.pcexpr_desc with
	| CEFLWOR ([CEFOR(t_e1,x,None,e1)], None, None, e2) 
	  when has_set_annot ce' && rqname_equal x fs_dot 
               && has_set_annot e1 && has_set_annot e2 ->
	    begin
	      let flwor1 = fmkacexpr (CEFLWOR ([CEFOR(t_ce,y,None,e2)], None, None, e3))
		  (empty_ast_annot()) ce.pcexpr_origin ce.pcexpr_loc in
	      let r =  fmkacexpr (CEFLWOR ([CEFOR(t_e1,x,None,e1)], None, None, flwor1))
		  (empty_ast_annot()) ce'.pcexpr_origin ce'.pcexpr_loc in
	      let _ = set_scrambling_annot flwor1.pcexpr_annot 
		  (get_scrambling_annot ce.pcexpr_annot) in
	      let _ = set_scrambling_annot r.pcexpr_annot 
		  (get_scrambling_annot ce.pcexpr_annot) in
	      let _ = debug_apply "loop fusion" ce r in 
	      r, true  
	    end
	| _ -> fail
      end
  | _ -> fail
	

(* ** CONDITION DETECTION ** 
 * 
 *  for $x in e1  = if e1 
 *  return e2     = then e2
 *
 * if $x not in FV(e2)
 * **)
let condition_detection rewrite_ctxt flwor =
  let fail = flwor, false in
  match flwor.pcexpr_desc with
  | CEFLWOR ([CEFOR(t_ce1,x,None,ce1)], None, None, ce2) 
    when has_set_annot ce1 && has_set_sem flwor ->
      if  not(is_free_var_of x ce2)
      then
	let ce_empty = fmkacexpr CEEmpty 
	    (empty_ast_annot()) flwor.pcexpr_origin flwor.pcexpr_loc in
	let if_expr = fmkacexpr (CEIf ((wrap_in_fn_boolean rewrite_ctxt) ce1, ce2, ce_empty)) 
	    (empty_ast_annot()) flwor.pcexpr_origin flwor.pcexpr_loc in
	let _ = set_scrambling_annot ce1.pcexpr_annot Bag in
	let _ = set_scrambling_annot if_expr.pcexpr_annot 
	    (get_scrambling_annot flwor.pcexpr_annot) in
	let _ = debug_apply "condition detection" flwor if_expr in 
	if_expr, true
      else fail
  | _ -> fail
	
(* ** CONDITION SHIFT **
 *
 *  *if( *if e1 then e2) = *if e1 then
 *  then e3             =   *if e2 then e3 
 * **)
let condition_shift rewrite_ctxt ce =
  let fail = ce, false in
  match ce.pcexpr_desc with
  | CEIf (b_cif,ce3,cempty) ->
      begin
	match b_cif.pcexpr_desc with
	| CECall (fname, [cif], sign,u, selfrecur) 
	  when rqname_equal fname fn_boolean ->
	    begin
	      match cif.pcexpr_desc, cempty.pcexpr_desc with
	      | CEIf (ce1,ce2,cempty'), CEEmpty 
		when has_bag_sem ce1 && has_bag_sem ce2 ->
		  begin
		    match cempty'.pcexpr_desc with 
		    | CEEmpty ->
			let b_cif' = fmkacexpr (CECall(fname, [ce2], sign,u, selfrecur))
			    b_cif.pcexpr_annot b_cif.pcexpr_origin b_cif.pcexpr_loc in
			let nest_if = fmkacexpr (CEIf (b_cif', ce3, cempty)) 
			    cif.pcexpr_annot cif.pcexpr_origin cif.pcexpr_loc in
			let r = fmkacexpr (CEIf (ce1, nest_if, cempty')) 
			    ce.pcexpr_annot ce.pcexpr_origin ce.pcexpr_loc in
			let _ = debug_apply "condition shift" ce r in 
			r, true
		    | _ -> fail
		  end
	      | _ -> fail
	    end
	| _ -> fail
      end
  | _ -> fail


(* ** RETURN CONDITION LIFT **
 *
 *  *for $x in e1             = *if e2 then
 *  return ( *if e2 then e3 ) =   *for $x in e1 return e3
 *
 * if $x not in FV(e2)
 * **)
   
let return_condition_lift  rewrite_ctxt ce =
   let fail = ce, false in
   match ce.pcexpr_desc with
   | CEFLWOR ([CEFOR(t_ce1,x,None,ce1)], None, None, cif) 
     when has_set_sem ce && has_set_sem cif ->
       begin
	 match cif.pcexpr_desc with
	 | CEIf (ce2,ce3,cempty) 
	   when has_set_sem ce3 ->
	     begin
	       match cempty.pcexpr_desc with
	       | CEEmpty when not(is_free_var_of x ce2) ->
		   let cefor = fmkacexpr (CEFLWOR ([CEFOR(t_ce1,x,None,ce1)], None, None, ce3)) 
		       (empty_ast_annot()) ce.pcexpr_origin ce.pcexpr_loc in
		   let _ = set_scrambling_annot cefor.pcexpr_annot 
		       (get_scrambling_annot ce.pcexpr_annot) in
		   let r = fmkacexpr (CEIf (ce2, cefor, cempty)) 
		      (empty_ast_annot()) cif.pcexpr_origin cif.pcexpr_loc in
		   let _ = set_scrambling_annot r.pcexpr_annot 
		       (get_scrambling_annot cif.pcexpr_annot) in
		   let _ = debug_apply "return condition lift" ce r in 
		   r, true
	       | _ -> fail
	     end
	 | _ -> fail
       end
   | _ -> fail

(* ** NESTED RETURN CONDITION LIFT **
 *
 * *for $x in e1       = *if en then
 * return              =   *for $x in e1
 *   *if e2 ^ ... ^ en =   return
 *   then en+1         =     *if e2 ^ ... ^ en-1 then en+1
 *
 * if n > 2 amd $x not in FV(en)
 * **)
let nested_return_condition_lift  rewrite_ctxt ce =
   let fail = ce, false in
   match ce.pcexpr_desc with
   | CEFLWOR ([CEFOR(t_ce1,x,None,ce1)], None, None, cond) 
     when has_set_sem ce && has_set_sem cond ->
       begin
	 let boolean_list = conjunction_to_list cond in
	 if List.length boolean_list > 1 then
	   let rev_list = List.rev boolean_list in
	   let rev_tail = List.tl rev_list in
	   (* find expressions e in rev_tail for which FV(e) =/= $x *)
	   let lifted = List.filter (fun (e,_,_,_) -> not(is_free_var_of x e)) rev_tail in
	   if List.length lifted > 0 then
	     begin
	       let non_lifted = List.filter (fun (e,_,_,_) -> is_free_var_of x e) rev_tail in
	       let non_lifted' = List.rev ((List.hd rev_list) :: non_lifted) in

	       let nested_if = list_to_conjunction non_lifted' false in
	       let for_expr  = fmkacexpr 
		   (CEFLWOR ([CEFOR(t_ce1,x,None,ce1)], None, None, nested_if)) 
		   (empty_ast_annot()) ce.pcexpr_origin ce.pcexpr_loc in
	       let _ = set_scrambling_annot for_expr.pcexpr_annot 
		   (get_scrambling_annot ce.pcexpr_annot) in
	       let new_res = (for_expr,ce.pcexpr_annot,ce.pcexpr_origin,ce.pcexpr_loc) in
	       let lifted' = List.rev (new_res::lifted) in
	       let r = list_to_conjunction lifted' false in
	       let _ = debug_apply "nested return condition lift" ce r in 
	       r, true
	     end
	   else
	     fail
	 else
	   fail
       end
   | _ -> fail

(* ** RETURN RESULT LIFT **
 * *for $x in e1    = *if ( *for $x in e1 return e2)
 * return           = then e3
 *   *if e2 then e3 
 *
 * if $x not in FV(e3)
 * **)
let return_result_lift  rewrite_ctxt ce =
   let fail = ce, false in
   match ce.pcexpr_desc with
   | CEFLWOR ([CEFOR(t_ce1,x,None,ce1)], None, None, cif) 
     when has_set_sem ce && has_set_sem cif ->
       begin
	 match cif.pcexpr_desc with
	 | CEIf (b_ce2,ce3,cempty) 
	   when has_set_sem ce3 ->
	     begin
	       match cempty.pcexpr_desc with
	       | CEEmpty when not(is_free_var_of x ce3) ->
		   let ce2 =
		     match b_ce2.pcexpr_desc with
		     | CECall (fname, [arg], sign, u, selfrecur) when rqname_equal fname fn_boolean ->
			 arg
		     | _ -> b_ce2
		   in
		   let cefor = fmkacexpr (CEFLWOR ([CEFOR(t_ce1,x,None,ce1)], None, None, ce2)) 
		       (empty_ast_annot()) ce.pcexpr_origin ce.pcexpr_loc in
		   let _ = set_scrambling_annot cefor.pcexpr_annot
		       (get_scrambling_annot ce.pcexpr_annot) in
		   let r = fmkacexpr 
		       (CEIf ((wrap_in_fn_boolean rewrite_ctxt cefor), ce3, cempty)) 
		      cif.pcexpr_annot cif.pcexpr_origin cif.pcexpr_loc in
		   let _ = debug_apply "return result lift" ce r in 
		   r, true
	       | _ -> fail
	     end
	 | _ -> fail
       end
   | _ -> fail


(* ** NESTED RETURN RESULT LIFT **
 * *for $x in e1        = *if (
 * return               =   *for $x in e1
 *   *if e2 ^ ... ^ en  =   return
 *   then en+1          =     *if e2 ^ ... ^ en-1
 *                            then en
 *                        then en+1
 *
 * if n > 2 and $x not in FV(en+1)
 * **)
let nested_return_result_lift  rewrite_ctxt ce =
   let fail = ce, false in
   match ce.pcexpr_desc with
   | CEFLWOR ([CEFOR(t_ce1,x,None,ce1)], None, None, cond) 
     when has_set_sem ce && has_set_sem cond ->
       begin
	 let boolean_list = conjunction_to_list cond in
	 if List.length boolean_list > 1 then
	   begin
	   let rev_list = List.rev boolean_list in
	   let rev_tail = List.tl rev_list in
	   let (c, c_annot, c_fi, c_eh) = List.hd rev_list in
	   if not(is_free_var_of x c) then
	       let nested_if = list_to_conjunction (List.rev rev_tail) false in
	       let for_expr  = fmkacexpr
		   (CEFLWOR ([CEFOR(t_ce1,x,None,ce1)], None, None, nested_if)) 
		   (empty_ast_annot()) ce.pcexpr_origin ce.pcexpr_loc in
	       let _ = set_scrambling_annot for_expr.pcexpr_annot
		   (get_scrambling_annot ce.pcexpr_annot) in
	       let ce_empty = fmkacexpr CEEmpty c_annot c_fi c_eh in
	       let r = fmkacexpr 
		   (CEIf ((wrap_in_fn_boolean rewrite_ctxt for_expr), c, ce_empty)) 
		   c_annot c_fi c_eh in
	       let _ = debug_apply "nested return result lift" ce r in 
	       r, true
	   else
	     fail
	   end
	 else
	   fail
       end
   | _ -> fail


(* ** FOR CONDITION LIFT **
 * 
 * *for $x in          = *if e1 then
 *   ( *if e1 then e2) =   *for $x in e2 
 *  return e3          =   return e3
 *
 * **)
let for_condition_lift rewrite_ctxt ce =
   let fail = ce, false in
   match ce.pcexpr_desc with
   | CEFLWOR ([CEFOR(t_x,x,None,cif)], None, None, ce3) 
     when has_set_sem ce && has_set_sem ce3 ->
       begin
	 match cif.pcexpr_desc with
	 | CEIf (ce1, ce2, cempty) when has_set_annot cif && has_set_annot ce2 ->
	     begin
	       match cempty.pcexpr_desc with
	       | CEEmpty  ->
		   let cefor = fmkacexpr (CEFLWOR ([CEFOR(t_x,x,None,ce2)], None, None, ce3)) 
		       ce.pcexpr_annot ce.pcexpr_origin ce.pcexpr_loc in
		   let r = fmkacexpr 
		       (CEIf (ce1, cefor, cempty)) 
		       cif.pcexpr_annot cif.pcexpr_origin cif.pcexpr_loc in
		   let _ = debug_apply "for condition lift" ce r in 
		   r, true
	       | _ -> fail
	     end
	 | _ -> fail
       end
   | _ -> fail

(* ** TRIVIAL DOT CONDITION **
 * if $dot then e2 = e2
 * **)
let trivial_dot_condition rewrite_ctxt ce =
   match ce.pcexpr_desc with
   | CEIf (c1, c2, c3) ->
       begin
         match c1.pcexpr_desc, c3.pcexpr_desc with
         | CEVar dot, CEEmpty when rqname_equal dot fs_dot ->
	     let _ = debug_apply "trivial dot condition" ce c2 in
             c2, true
         | _ -> ce, false
       end
   | _ -> ce, false
   


(* ** TRIVIAL LOOP **
 * *for $x in e return *$x = *e
 * **)
let trivial_loop rewrite_ctxt ce =
   let fail = ce, false in
    match ce.pcexpr_desc with
   | CEFLWOR ([CEFOR(t_ce,x,None,ce_r)], None, None, cevar)
     when has_set_sem ce && has_set_annot ce_r ->
       begin
	 match cevar.pcexpr_desc with
	 | CEVar v when rqname_equal x v && has_set_sem cevar ->
	     let _ = set_scrambling_annot ce_r.pcexpr_annot Set in
	     let _ = debug_apply "trivial loop" ce ce_r in 
	     ce_r, true
	 | _ -> fail
       end
   | _ -> fail

(* ** DOT INTRODUCTION **
 * for $x in e1 return e2 = for $dot in e1 return e2[$x/$dot]
 *
 * $dot not in FV(e2) and $x =/= $dot
 * **)
let dot_introduction ctxt ce =
  match ce.pcexpr_desc with
   | CEFLWOR ([CEFOR(t,x,None,e1)], None, None, e2)
     (* dropped precondition 'has_set_sem e1', because it *)
     (* may break the rewrite and does not seem necessary *)
     when is_in_cxq_plus ctxt ce &&
       not (is_free_var_of fs_dot e2) && not(rqname_equal x fs_dot)-> 
       begin
	 let e2', ch = tpnf_rename ctxt e2 x fs_dot in
	 let r =
	   fmkacexpr (CEFLWOR ([CEFOR(t,fs_dot,None,e1)], None, None, e2'))
	     ce.pcexpr_annot ce.pcexpr_origin ce.pcexpr_loc
	 in
	 let _ = debug_apply "$dot introduction" ce r in
	 r, true
       end
   | _ -> ce, false

(* ** DOT LOOP **
 * for $x in $x return e = e
 *
 * if has_max_one $x
 * **)
let dot_loop rewrite_ctxt ce =
   let fail = ce, false in
   match ce.pcexpr_desc with
   | CEFLWOR ([CEFOR(t_ce,x1,None,cevar)], None, None, ce') ->
       begin
	 match cevar.pcexpr_desc with
	 | CEVar x2 when rqname_equal x1 x2 && rqname_equal x1 fs_dot ->
	     let _ = debug_apply "$dot loop" ce ce' in
	     ce', true
	 | _ -> fail
       end
   | _ -> fail

(* ** SHORTENING CONDITION **
 * 
 * **)
let shortening_condition rewrite_ctxt ce =
  let fail = ce, false in
  match ce.pcexpr_desc with
  | CEIf (e1, e2, empty) 
    when (* is_in_cxq_plus rewrite_ctxt ce && *)
      has_bag_sem ce && has_bag_sem e1 && has_bag_sem e2 -> 
      begin
	match e2.pcexpr_desc, empty.pcexpr_desc with
	| CEVar d, CEEmpty 
	  when rqname_equal d fs_dot -> 
	    let e1' = 
	      match e1.pcexpr_desc with
	      | CECall (fn, [a], s, u, selfrecur) when rqname_equal fn fn_boolean -> a
	      | _ -> e1
	    in
	    let _ = debug_apply "condition shortening" ce e1' in
	    e1', true
	| _ -> fail
      end
  | _ -> fail

(* *** Filter Fusion *** 
 *
 * for $dot in (            = for $dot in e1
 *   for $dot in e1 return  = return
 *     if e2 ^ ... ^ en   =   if e2 ^ ... ^ en
 *     then $dot            =   then en+1
 * return en+1)           = 
 *
 *)
let filter_fusion ctxt ce =
  let fail = ce, false in
  match ce.pcexpr_desc with
  | CEFLWOR ([CEFOR(t,x,None,ce')], None, None, en1) 
    when has_set_sem ce && rqname_equal x fs_dot ->
      begin
	match ce'.pcexpr_desc with
	| CEFLWOR ([CEFOR(t_e1,y,None,e1)], None, None, cond) 
	  when has_set_sem ce' && rqname_equal y fs_dot && has_set_sem cond ->
	    begin
	      let boolean_list = conjunction_to_list cond in
	      let rev_list = List.rev boolean_list in
	      (* wrap last condition in the list into an fn_boolean call *)
	      let head,a,b,c = List.hd rev_list in
	      match head.pcexpr_desc with
	      | CEVar z when rqname_equal z fs_dot ->
		  begin
		    let rev_list' =  (en1,a,b,c)::(List.tl rev_list) in
		    let cond' = list_to_conjunction (List.rev rev_list') false in
		    let r = fmkacexpr 
			(CEFLWOR ([CEFOR(t_e1,y,None,e1)], None, None, cond'))
			(empty_ast_annot()) ce.pcexpr_origin ce.pcexpr_loc in
		    let annot = get_scrambling_annot ce.pcexpr_annot in
		    let _ = set_scrambling_annot cond'.pcexpr_annot annot in
		    let _ = set_scrambling_annot r.pcexpr_annot annot in
		    let _ = debug_apply "filter fusion" ce r in 
		    r, true
		  end
	      | _ -> fail
	    end
	| _ -> fail
      end
  | _ -> fail


(* ** ** PHASE 4 -- The sanity Check ** ** *)
let check_normal_form ctxt ce =

  let rec skip_redundant_let ce =
    match ce.pcexpr_desc with
    | CEFLWOR ([CELET(t_ce1,n_ce1,ce1)], None, None, ce2) ->
	skip_redundant_let ce2
    | _ -> ce
  in
  if not(!Conf.print_algebra_optimization_rewrite) then
    ce, false
  else
    begin
      match ce.pcexpr_desc with
      | CECall (fname, [arg], sign,u, selfrecur) 
	when rqname_equal fname fs_distinct_docorder ||
	rqname_equal fname fs_distinct_docorder_or_atomic_sequence ->
	  begin
	    let ff = !Conf.core_expr_formatter in 
	    let print_annot ff a =
	      begin
		let (ta, da, fv, sc, st)  = Xquery_core_ast_annotation.annot_components a in 
		Format.fprintf ff "[sc:%s]%!" (Xquery_core_ast_annotation.print_scrambling_annot sc);
		Format.fprintf ff "[type: ";
		(match ta with
		| None -> Format.fprintf ff "None]"
		| Some m -> Format.fprintf ff "%a]%!" Print_type_core.print_cxtype m);
	      end
	    in
	    let ce' = skip_redundant_let arg in
	    if not(is_in_tpnf' ce') then
	      begin
		Format.fprintf ff "  **************** WARNING ******************\n";
		Format.fprintf ff "  * The following expression is not in TPNF *\n";
		Format.fprintf ff "  *******************************************\n";
		Print_xquery_core.print_cexpr ff ce' print_annot;
		Format.fprintf ff "\n\n";
		ce, false
	      end
	    else 
	      begin
		Format.fprintf ff "  ******************* NOTE ***************\n";
		Format.fprintf ff "  * The following expression is in TPNF  *\n";
		Format.fprintf ff "  ****************************************\n";
		Print_xquery_core.print_cexpr ff ce' print_annot;
		Format.fprintf ff "\n\n";
		ce, false
	      end
	  end
      | _ -> ce, false
    end
	      
(* ** ** PHASE 5 -- Fix whatever got messed up ** ** *)

(* ** reintroduce where clauses **
 * for $x in e1        = for $x in e1 
 * return              = where if e2 ^ ... ^ en-1 then en
 *   if e2 ^ ... ^ en  = return en+1
 *   then en+1         
 *)
let shorten_conjunction ctxt cond =
  let boolean_list = conjunction_to_list cond in
  if List.length boolean_list > 1 then
    begin
      let rev_list = List.rev boolean_list in
      let ret, _, _, _ = List.hd rev_list in 
      let rev_tail = List.tl rev_list in
      let new_cond = list_to_conjunction (List.rev rev_tail) true in
      Some (new_cond, ret)
    end
  else None
    
let reintroduce_where ctxt ce =
  let fail = ce, false in
  match ce.pcexpr_desc with
  | CEFLWOR ([CEFOR(t_e1,x,None,e1)], None, None, cond) ->
      begin
	match shorten_conjunction ctxt cond with
	| Some (new_cond, ret) ->
	    let r = fmkacexpr 
		(CEFLWOR ([CEFOR(t_e1,x,None,e1)], 
			  Some new_cond, None, ret))
		ce.pcexpr_annot ce.pcexpr_origin ce.pcexpr_loc in
	    let _ = debug_apply "reintroduce where clause" ce r in 
	    r, true
	| _ ->  fail
      end
  | CEFLWOR ([CELET(t_e1,x,e1)], None, None, cond) ->
      begin
	match shorten_conjunction ctxt cond with
	| Some (new_cond, ret) ->
	    let r = fmkacexpr 
		(CEFLWOR ([CELET(t_e1,x,e1)], 
			  Some new_cond, None, ret))
		ce.pcexpr_annot ce.pcexpr_origin ce.pcexpr_loc in
	    let _ = debug_apply "reintroduce where clause" ce r in 
	    r, true
	| _ ->  fail
      end
  | _ -> fail

let remove_redundant_sbdo ctxt ce =
  match ce.pcexpr_desc with
  | CECall (fname, [arg], sign,u, selfrecur) 
    when rqname_equal fname fs_distinct_docorder ||
    rqname_equal fname fs_distinct_docorder_or_atomic_sequence ->
      let symtab = Hashtbl.create 10 in 
      let no2d, gen, ord, nodup = get_properties ctxt arg symtab in
      if ord && nodup then
	let _ = debug_apply "remove distinct-docorder (ord/nodup holds)" ce arg in 
	arg, true
      else 
	ce, false
  | _ -> ce, false


let group_flwor_block ctxt ce =
  match ce.pcexpr_desc with
  | CEFLWOR ([single_fl], None, None, e2) ->
      begin
	match e2.pcexpr_desc with
	| CEFLWOR (fl_list, w, ob, ret) ->
	    let r = 
	      fmkacexpr (CEFLWOR (single_fl::fl_list, w, ob, ret))
		ce.pcexpr_annot ce.pcexpr_origin ce.pcexpr_loc
	    in
	    let _ = debug_apply "group FLWOR block" ce r in 
	    r, true
	| _ -> ce, false
      end
  | _ -> ce, false

let join_support_hack ctxt ce =
  let rec has_no_let fl_list =
    match fl_list with
    | [] -> true
    | (CELET _ )::tl -> false
    | (CEFOR _ )::tl -> has_no_let tl
  in
  match ce.pcexpr_desc with
  | CEFLWOR (fl_list, None, None, ret1) 
    when has_no_let fl_list ->
      begin
	match ret1.pcexpr_desc with
	| (CEFLWOR ([CELET(t,v,expr)], None, None, ret2)) ->
	    let r = 
	      fmkacexpr (CEFLWOR (fl_list@[CELET(t,v,expr)], None, None, ret2))
		ce.pcexpr_annot ce.pcexpr_origin ce.pcexpr_loc
	    in
	    let _ = debug_apply "Join support hack" ce r in 
	    r, true
	| _ -> ce, false
      end
  | _ -> ce, false
