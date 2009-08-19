(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_typeswitch.ml,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_typeswitch
   Description:
     This module contains code building for the typeswitch operator.
*)

open Code_selection_context
open Code_util_matching
open Cs_util_coercion
open Cs_util
open Error
open Physical_sequence
open Xquery_algebra_ast
open Xquery_algebra_ast_util

let allocate_typeswitch_params compile_ctxt params = 
  let alloc_fold (oref,ctxt) (_, ovn) = 
    match ovn with 
      None -> (oref,ctxt)
    | Some vn ->
	let (oref, ctxt) =
	  match oref with
	    None   -> 
	      let ctxt' = add_variable_to_current_context ctxt vn in
	      let vr  = get_variable_reference ctxt' vn in
	      ((Some vr), ctxt')
	  | Some vr -> 
	      let ctxt' = add_variable_with_ref ctxt vn vr in
	      (oref, ctxt')
	in
	(oref, ctxt)
  in
  
  let param_list = Array.to_list params in 
  let oref, ctxt = List.fold_left alloc_fold (None,compile_ctxt) param_list in
  oref, ctxt


(* CETypeSwitch *)
let build_default_cetypeswitch_code code_ctxt pattern_var_pairs vr = 
  let pv_length = Array.length pattern_var_pairs in
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
    (* Typeswitchs all use the same var - also we bind everytime *)
    
  let add_fn = 
    match vr with	
	Some vr -> 
	  build_add_var_xml_value_with_ref code_ctxt vr 
      | None ->
	  (fun pv -> ())
  in

  (fun dep_args eval alg_ctxt arg ->
    if (Array.length dep_args) != pv_length then
      raise (Query (Code_Selection ("Number of Typeswitch arguments does not match number of cases")))
    else
      begin
	let i = ref 0 in
	let bDone = ref false in
	let depa_opt = ref None in 
	while (!i < pv_length) && (not !bDone)do
	  let p,_ = pattern_var_pairs.(!i) in	      
	  begin
	    match p.papattern_desc with
	    | ACase dt ->
		if (boolean_dynamic_type_check stat_ctxt dt arg) then
		  begin
		    add_fn (Physical_xml_value.xml_value_of_dom_value (materialized_of_list arg));
		    depa_opt := Some (dep_args.(!i));
		    bDone := true
		  end 				
	    | ADefault ->
		add_fn (Physical_xml_value.xml_value_of_dom_value (materialized_of_list arg));
		depa_opt := Some (dep_args.(!i));
		bDone := true
		    (* | _ ->
		       raise (Query (Code_Selection "No DEFAULT clause in TYPESWITCH expression"))
		     *)
	  end;
	  incr i
	done;
	  let e_opt =
	    match !depa_opt with
		None -> raise (Query (Code_Selection ("No Default clause in typeswitch expression")))
	      | Some dep -> dep
	  in
	eval alg_ctxt e_opt;
      end
  )

let build_typeswitch_code code_ctxt algop pattern= 
  let vr, code_ctxt = allocate_typeswitch_params code_ctxt pattern in
  let fn = build_default_cetypeswitch_code code_ctxt pattern vr in
  let dep = access_manysub algop.pdep_sub_expression in 
  (coerce_manydep fn dep coerce_unary_item_list_to_physical_value), code_ctxt


