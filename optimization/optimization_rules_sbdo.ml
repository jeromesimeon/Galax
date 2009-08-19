(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_rules_sbdo.ml,v 1.9 2007/07/05 08:35:54 simeon Exp $ *)

(* Module: Optimization_rules_sbdo
   Description:
     This module implements the SBDO optimization, described in

       Optimizing Sorting and Duplicate Elimination 
       in XQuery Path Expressions
       
       Mary Fernández, Jan Hidders, Philippe Michiels, 
       Jérôme Siméon, Roel Vercammen
       
       DEXA 2005
       http://www.adrem.ua.ac.be/bibrem/pubs/fernandez.1.pdf
*)

open Error
open Namespace_builtin

open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_algebra_ast_annotation_util

open Compile_annotate
open Compile_context

open Norm_context
open Processing_context

open Optimization_util
open Optimization_rules_sbdo_automaton

let unsup = Query(Internal_Error("This SBDO mode is not supported any more."))

(*
 *  WARNING: Before being able to apply the SBDO optimization
 *           to a TTP, we must be sure that the input op the
 *           operator itself is always sorted by document 
 *           order and free from duplicates.
 *           -- Ph
 *)
let set_sbdo pattern index action_needed = 
  match action_needed with 
  | Nothing          -> 
      let _ = if Debug.default_debug() then Debug.print_default_debug "Action is NOTHING" in
      pattern.(index).requires_sbdo <- (false,false)
  | Docorder         -> 
      let _ = if Debug.default_debug() then Debug.print_default_debug "Action is ORD\n" in
      pattern.(index).requires_sbdo <- (true ,false)
  | Distinct         -> 
      let _ = if Debug.default_debug() then Debug.print_default_debug "Action is NODUP\n" in
      pattern.(index).requires_sbdo <- (false,true )
  | DistinctDocorder -> 
      let _ = if Debug.default_debug() then Debug.print_default_debug "Action is ORD/NODUP\n" in
      pattern.(index).requires_sbdo <- (true ,true )

let is_last pattern index =
  match pattern.(index).child_twig with
  | Some _ -> false
  | None -> true

let treepatterndepth = ref 0
let is_toplevel_ttp () = !treepatterndepth = 0

let process_tree_pattern pattern sbdo_kind startstate =

  let rec process_step opt_axis index state action =
    match opt_axis with
    | Some axis ->
	begin
	  let state', action' = 
	    do_transition sbdo_kind state (axis_adapter axis) 
	      ((is_last pattern index) && is_toplevel_ttp())
	  in
	  let _ = set_sbdo pattern index action' in
	  let s = init_state sbdo_kind in
	  (* let _ = Printf.printf "Action needed after step: %s\n" (print_action action') in *)
	  let _ = List.map 
	      (fun (a, i) -> process_step (Some a) i s Nothing) 
	      pattern.(index).pred_twigs in
	  match pattern.(index).child_twig with
	  | Some (axis', index') -> process_step (Some axis') index' state' action'
	  | None -> state', action'
	end
    | None -> 
	begin
	  match pattern.(index).child_twig with
	  | Some (axis', index') -> process_step (Some axis') index' state action
	  | None -> state, action
	end
  in
  process_step None 0 startstate Nothing
  

(*
 * Records and passes on the sbdo state of a supported algebra fragment:
 *
 * fs:ddo, MapFromItem, MapToItem, TupleTreePattern, TreeJoin?, Select, Parse, ...
 *)
let rec get_set_sbdo_state sbdo_kind state action op =
  match op.palgop_expr_name with
  | AOEProject _ | AOEMapIndex _ -> 
      begin
	let indep = access_onesub op.psub_expression in
	get_set_sbdo_state sbdo_kind state action indep
      end
  | AOEInputTuple 
  | AOEAccessTuple _ -> 
      (* init_state sbdo_kind, Nothing *)
      state, action
  | AOECallBuiltIn ((fn,arity), x, y, z) 
    when (Namespace_names.rqname_equal fn fs_distinct_docorder) ->
      begin
	let arg = access_manysub op.psub_expression in
	get_set_sbdo_state sbdo_kind state action arg.(0)
      end
  | AOEMapFromItem v ->
      begin
	(* Limited coverage: deal with (positional) predicates only   *)
        (* implies: dep subexpressions construct tuples with at most  *)
        (* one item in the fields                                     *)
	let map_from_indep = access_onesub op.psub_expression in
	let map_from_dep   = access_onesub op.pdep_sub_expression in
	match map_from_dep.palgop_expr_name with
	| AOECreateTuple a when (Array.length a) = 1 ->
	    begin
	      let dep_sub = access_manysub map_from_dep.psub_expression in
	      match dep_sub.(0).palgop_expr_name with
	      | AOEVar v' when Namespace_names.rqname_equal v v' -> 
		  let s,a = get_set_sbdo_state sbdo_kind state action map_from_indep in
  		  (* let _ = Printf.printf "MFI -- returning %s, %s\n" (print_state s) (print_action a) in *)
		  s,a
	      | _ -> 
		  (* let _ = Printf.printf "Unsupported indep in MFI, returning sink" in *)
		  sink_state sbdo_kind, DistinctDocorder
	    end
	| _ -> 
	    (* let _ = Printf.printf "Unsupported indep in MFI, returning sink" in *)
	    sink_state sbdo_kind, DistinctDocorder
      end
  | AOEMapToItem | AOEMapConcat ->
      (* TTP's in the dependent subexpr operate over one tuple *)
      (* but the MFI returns a mapping, so we must compose the *)
      (* axis sequences to get the state obtained from the MTI *)
      let mapto_dep = access_onesub op.pdep_sub_expression in
      let mapto_indep = access_onesub op.psub_expression in
      let state', action' = get_set_sbdo_state sbdo_kind state action mapto_indep in

      (* first round : get state above TTP*)
      let s,a = get_set_sbdo_state sbdo_kind state' action' mapto_dep in
      (* second round : state for dependent branch *)
      let _ = get_set_sbdo_state sbdo_kind (init_state sbdo_kind) Nothing mapto_dep in
      (* let _ = Printf.printf "MTI -- returning %s, %s\n" (print_state s) (print_action a) in *)
      s,a
  | AOETupleTreePattern (i, p) ->
      let _ = incr treepatterndepth in
      let state', action' = get_set_sbdo_state sbdo_kind state action (access_onesub op.psub_expression) in
      let _ = decr treepatterndepth in
      (* process tree pattern and manipulate sbdo-annots on the go *)
      let s,a = process_tree_pattern p sbdo_kind state' in
      (* let _ = Printf.printf "TTP -- returning %s, %s\n" (print_state s) (print_action a) in *)
      s,a
  | AOESelect _ -> 
      (* we just care about the argument *)
      let s,a = get_set_sbdo_state sbdo_kind state action (access_onesub op.psub_expression) in
      (* let _ = Printf.printf "Select -- returning %s, %s\n" (print_state s) (print_action a) in *)
      s,a
  | AOECallBuiltIn ((fn,arity), _, _, _) 
    when (Namespace_names.rqname_equal fn fn_subsequence ||
          Namespace_names.rqname_equal fn fs_first ||
          Namespace_names.rqname_equal fn fs_last_fn) ->
      let args = access_manysub op.psub_expression in
      let s,a = get_set_sbdo_state sbdo_kind state action args.(0) in
      (* let _ = Printf.printf "fn:subseq -- returning %s, %s\n" (print_state s) (print_action a) in*)
      s,a
  (* FIXME: use typing for max_one property here *)
  | AOEParse _ -> (init_state sbdo_kind, Nothing)
  | AOECallBuiltIn ((fn,arity), _, _, _) ->
      if (Namespace_names.rqname_equal fn fn_subsequence ||
          Namespace_names.rqname_equal fn fn_doc)
      then 
	(* let _ = Printf.printf "BuiltIn call -- returning %s, %s\n" (print_state (init_state sbdo_kind)) (print_action Nothing)in *)
	(init_state sbdo_kind, Nothing)
      else 
	let args = access_manysub op.psub_expression in
	let _ = Array.map (fun x -> get_set_sbdo_state sbdo_kind state action x) args in
	sink_state sbdo_kind, DistinctDocorder
  (* Needed for robustness -- tricky *)
  | AOECreateTuple a when (Array.length a) = 1 ->
      begin
	let arg = (access_manysub op.psub_expression).(0) in
	match arg.palgop_expr_name with
	| AOEAccessTuple _ -> init_state sbdo_kind, Nothing
	| _ -> sink_state sbdo_kind, DistinctDocorder
      end
  | _ ->
      (* let _ = Printf.printf "Unsupported expr, returning sink\n" in *)
      (sink_state sbdo_kind, DistinctDocorder)


(*
 * Top level rewrite rule for the SBDO optim 
 *)
let sbdo_rewrite ctxt op =
  let norm_ctxt = norm_context_from_compile_context ctxt in
  let proc_ctxt = (processing_context_from_norm_context norm_ctxt) in
  let sbdo_kind = proc_ctxt.sbdo_kind in
  match sbdo_kind with
  | SBDO_Remove   ->
      begin
	match op.palgop_expr_name with
	| AOETupleTreePattern (i, p) -> 
	    let _ = Array.map (fun x -> x.requires_sbdo <- (false, false)) p in 
	    op, true
	| AOECallBuiltIn ((fn,arity), x, y, z) 
	  when Namespace_names.rqname_equal fn fs_distinct_docorder ->
	    let arg = access_manysub op.psub_expression in
	    arg.(0), true
	| _ -> op, false
      end
  | SBDO_Preserve -> op, false
  | SBDO_Tidy | SBDO_DupTidy | SBDO_Sloppy -> 
      begin
	match op.palgop_expr_name with
	| AOETupleTreePattern(i, p) -> 
	    begin
	      (* action NOT USED? - Jerome *)
	      (* state NOT USED? - Jerome *)
	      let _ = 
		get_set_sbdo_state (sbdo_kind) (init_state sbdo_kind) Nothing  op in
	      op, false
	    end
	| AOECallBuiltIn ((fn,arity), x, y, z) 
	  when Namespace_names.rqname_equal fn fs_distinct_docorder ->
	    begin
	      let arg = access_manysub op.psub_expression in
	      let state, action = get_set_sbdo_state (sbdo_kind) (init_state sbdo_kind) Nothing arg.(0) in
	      (* let _ = Printf.printf "Action needed for ddo: %s\n" (print_action action) in	*)
	      let eh = op.palgop_expr_origin in
	      let fi = op.palgop_expr_loc in
	      match action with
	      | DistinctDocorder -> op, false
	      | Nothing          -> arg.(0), true
	      | Docorder         -> 
		  let op' = logical_aalgop_mkop (AOECallBuiltIn ((fs_docorder,arity), x, y, z) ) (ManySub arg) NoSub None eh fi in
		  op', true
	      | Distinct         -> 
		  let op' = logical_aalgop_mkop (AOECallBuiltIn ((fs_distinct,arity), x, y, z) ) (ManySub arg) NoSub None eh fi in
		  op', true
	    end
	| _ -> op, false
      end
  | _ -> raise unsup


(*
 * fs:ddo(MapToItem{In#f}(TTP[i,p](Op))) == MapToItem{In#f}(TTP[i,p](Op))
 *)
let sbdo_rewrite_two ctxt op =
  let is_ttp o =
    if not(is_tupletreepattern o) then
      match o.palgop_expr_name with
      | AOEProject _ -> is_tupletreepattern (access_onesub o.psub_expression)
      | _ -> false
    else true
  in

  match op.palgop_expr_name with
  | AOECallBuiltIn ((fn,arity), x, y, z) 
    when Namespace_names.rqname_equal fn fs_distinct_docorder 
      || Namespace_names.rqname_equal fn fs_distinct 
      || Namespace_names.rqname_equal fn fs_docorder ->
      begin
	let arg = (access_manysub op.psub_expression).(0) in
	match arg.palgop_expr_name with
	| AOEMapToItem ->
	    begin
	      let mapto_dep = access_onesub arg.pdep_sub_expression in
	      let mapto_indep = access_onesub arg.psub_expression in
	      if (is_access_tuple mapto_dep && is_ttp mapto_indep)
	      then arg, true
	      else op, false
	    end
	| _ -> op, false
      end
  | _ -> op, false

