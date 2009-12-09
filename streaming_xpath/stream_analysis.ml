(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: stream_analysis.ml,v 1.5 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Stream_analysis
   Description:
     This module provides a whole-program analysis function for
     performing a stream analysis on a specified XQuery core module,
     consisting of

   1) building an XQuery core data flow graph for that module
   2) determining for each potential stream source in the graph wether it
      can be streamed or not
   3) adding a corresponding indicator flag annotation to the core ast node.

   At the top level of the analysis, data flow properties of interest are those
   of type 'for each data flow path originating at a certain source, ...', which
   make use of more low level observations on individual nodes in the graph,
   and, in turn, of those nodes' payload (i.e., XQuery core ast handles).

   - Michael *)


open Xquery_common_ast
open Xquery_core_ast

open Namespace_builtin

open Df_struct
open Df_analysis

open Stream_analysis_builtin

open Error


type use =
  | Single
  | Multiple


let default_iteration_context () = []

let open_iteration_context affiliates iter_ctxt =
  (*print_string "opening...\n";*)
  (affiliates, ()) :: iter_ctxt

let close_iteration_context affiliates iter_ctxt =
  (*print_string "closing...\n";*)
  List.remove_assoc affiliates iter_ctxt
  
let is_matched_iteration_context iter_ctxt =
  (*print_string ("match? " ^ (string_of_bool (iter_ctxt = [])) ^ "\n");*)
  iter_ctxt = []


(********************)
(* Helper functions *)
(********************)

let is_funcall_of_cfname cexpr cfname =
  match cexpr.pcexpr_desc with
    | CECall (x, _, _, _, _) when x = cfname -> 
	true
    | _ -> false

let access_arg1 cexpr =
  match cexpr.pcexpr_desc with
    | CECall (cfname, arg1 :: args, _, _,_) -> arg1
    | _ -> raise (Query (Streaming_XPath "Was expecting a function call with at least one argument."))
 
let get_fn_doc_static_uri ac_handle =
  let (opt_cexpr, _) = ac_handle in
    match opt_cexpr with
      | Some cexpr when is_funcall_of_cfname cexpr fn_doc ->
	  let fn_doc_arg1 = access_arg1 cexpr in
	    if is_funcall_of_cfname fn_doc_arg1 fs_convert_simple_operand then
	      let fs_cso_arg1 = access_arg1 fn_doc_arg1 in
		if is_funcall_of_cfname fs_cso_arg1 fn_data then
		  let fn_data_arg1 = access_arg1 fs_cso_arg1 in
		    match fn_data_arg1.pcexpr_desc with
		      | CEScalar av ->
			  begin
			    try
			      let uri_str = string_of_literal av in
				Some uri_str
			    with
			      | Query (Datamodel _) -> None
			  end
		      | _ -> None
		else None
	    else None
      | _ -> None

let is_single_use_uri uri_str uri_uses =
  try
    begin
      match List.assoc uri_str uri_uses with
	| Single -> true
	| Multiple -> false
    end
  with
    | Not_found -> raise (Query (Streaming_XPath ("Couldn't find use information for uri " ^ uri_str)))

let register_uri_use uri_str uri_uses =
  try
    begin
      match List.assoc uri_str uri_uses with
	| Single ->
	    (* It is not necessary to remove the old 'Single' mapping, since the new
	       one will proceed it in the resulting association list and thus be the
	       one returned by subsequent calls to List.assoc. *)
	    (uri_str, Multiple) :: uri_uses
	| Multiple ->
	    uri_uses
    end
  with
    | Not_found -> (uri_str, Single) :: uri_uses


(**********************************)
(* Local predicates on ac_handles *)
(**********************************)

let is_fn_doc ac_handle =
  let (opt_cexpr, _) = ac_handle in
    match opt_cexpr with
      | Some cexpr ->
	  begin
	    match cexpr.pcexpr_desc with
	      | CECall (x, _, _, _, _) when x = fn_doc -> 
		  true
	      | _ -> false
	  end
      | None -> false

let is_malicious_axis_step ac_handle =
  let (opt_cexpr, _ ) = ac_handle in
    match opt_cexpr with
      | Some cexpr ->
	  begin
	    match cexpr.pcexpr_desc with
	      | CEReverseAxis _ -> true
	      | CEForwardAxis (v,axis, _) when axis = Following_sibling -> true
	      | _ -> false
	  end
      | None -> false
	  
let is_potential_iteration ac_handle =
  let (opt_cexpr, opt_ac_hint) = ac_handle in
    match opt_cexpr with
      | Some cexpr ->
	  begin
	    match cexpr.pcexpr_desc with
	      | CESome _
	      | CEEvery _ -> true
	      | CEFLWOR _ ->
		  begin
		    match opt_ac_hint with
		      | Some ac_hint ->
			  begin
			    match ac_hint with
			      | ACHFor _ -> true
			      | _ -> false
			  end
		      | _ -> false
		  end
	      | _ -> false
	  end
      | None -> false
	  
let is_potential_variable_binding ac_handle =
  let (opt_cexpr, opt_ac_hint) = ac_handle in
    match opt_cexpr with
      | Some cexpr ->
	  begin
	    match cexpr.pcexpr_desc with
	      | CESome _
	      | CEEvery _
	      | CETypeswitch _ -> true
	      | CEFLWOR _ ->
		  begin
		    match opt_ac_hint with
		      | Some ACHLet _
		      | Some ACHFor _ -> true
		      | _ -> false
		  end
	      | _ -> false
	  end
      | None ->
	  begin
	    match opt_ac_hint with
	      | Some ACHVar _ -> true
	      | _ -> false
	  end
	  

(*******************************)
(* Local predicates on dfnodes *)
(*******************************)

let is_binding_variable dfnode =
  let ac_handle = get_dfnode_value dfnode in
  let res = is_dffork dfnode && is_potential_variable_binding ac_handle in
    (*print_string ("is_binding: " ^ (string_of_bool res) ^ "\n");*)
    res

let is_unbinding_variable dfnode =
  let ac_handle = get_dfnode_value dfnode in
  let res = is_dfpass DFMerge dfnode && is_potential_variable_binding ac_handle in
    (*print_string ("is_unbinding: " ^ (string_of_bool res) ^ "\n");*)
    res

let is_iteration dfnode =
  let ac_handle = get_dfnode_value dfnode in
    (is_dfpass DFMerge dfnode || is_dfsink DFControl dfnode)  && is_potential_iteration ac_handle


(**************************************************************************)
(* Predicates on all paths in the dfgraph originating at a certain dfnode *)
(**************************************************************************)

let exists_malicious_navigation_or_funcall dfnode =
  let ctxt = () in
  let neu = false in

  let g n x = x in

  let f bs dfnode ctxt =
    let b = List.fold_left (||) neu bs in
    let ac_handle = get_dfnode_value dfnode in
      b || is_malicious_axis_step ac_handle || is_malicious_builtin_funcall ac_handle || is_malicious_builtin_funcall ac_handle
  in
    fold_left_dfs f g neu ctxt dfnode

let exists_malicious_iteration dfnode =
  let ctxt = default_iteration_context () in
  let neu = false in

  let g dfnode ctxt =
    let affiliates = get_affiliates dfnode in
      if is_binding_variable dfnode then open_iteration_context affiliates ctxt
      else if is_unbinding_variable dfnode then close_iteration_context affiliates ctxt
      else ctxt
  in

  let f bs dfnode ctxt =
    let b = List.fold_left (||) neu bs in
      b || (is_iteration dfnode && is_matched_iteration_context ctxt)
  in
    fold_left_dfs f g neu ctxt dfnode


(***********)
(* Exposed *)
(***********)

let stream_analysis_of_xmodule cxmodule =

  (***********************************************************************************************)
  (* An individual fn:doc call is streamable iff
     1) it's static uri is used only once
     2) on every data flow path originating at that call, no malicious XPath navigation / builtin
     function call does exist
     3) the call is not executed repeatedly, i.e., does not undergo iteration.                   *)
  (***********************************************************************************************)
    
  let is_streamable uri_uses dfnode opt_uri =

    let is_single_use_opt_uri opt_uri =
      match opt_uri with
	| Some uri -> is_single_use_uri uri uri_uses
	| _ -> false
    in

      is_single_use_opt_uri opt_uri && not (exists_malicious_navigation_or_funcall dfnode || exists_malicious_iteration dfnode)
  in

  let is_fn_doc_dfnode dfnode =
    is_dfpass DFMerge dfnode && is_fn_doc (get_dfnode_value dfnode) 
  in

  let set_stream_annot b dfnode =
    let (opt_cexpr, _) = get_dfnode_value dfnode in
      match opt_cexpr with
	| Some cexpr ->
	    Xquery_core_ast_annotation.set_stream_annot cexpr.pcexpr_annot b
	| None -> ()
  in

  let set_streamed_iff_streamable uri_uses dfnode opt_uri =
    if is_streamable uri_uses dfnode opt_uri then set_stream_annot true dfnode
    else set_stream_annot false dfnode
  in


  (******************************************************************)
  (* Perform the actual analysis, and annotate the XQuery core ast. *)
  (******************************************************************)

  let dfgraph = df_analysis_of_xmodule cxmodule in

  (* Output the XQuery core data flow graph in dot format. *)  
  let _ =
    if !Conf.print_dfgraph then
      Df_analysis.print_dot_dfgraph !Conf.dfgraph_formatter dfgraph
  in
	
  let fn_doc_calls = find_all is_fn_doc_dfnode dfgraph in
  let opt_static_uris = List.map (fun x -> get_fn_doc_static_uri (get_dfnode_value x)) fn_doc_calls in
  let user_def_funs = cxmodule.pcmodule_prolog.pcprolog_functions in

    (* If every fn:doc call uses a static uri, proceed with the analysis; if not, give up.
       Similarly, conservatively do not stream in case user-defined functions are present. *)
    if List.for_all (fun x -> match x with | Some _ -> true | _ -> false) opt_static_uris
      && user_def_funs = []
    then
      begin

	let reg_fun uri_uses opt_uri =
	  match opt_uri with
	    | Some uri -> register_uri_use uri uri_uses
	    | _ -> uri_uses
	in

	let uri_uses = List.fold_left reg_fun [] opt_static_uris in
	  List.iter2 (set_streamed_iff_streamable uri_uses) fn_doc_calls opt_static_uris
      end
    else
      List.iter (set_stream_annot false) fn_doc_calls
