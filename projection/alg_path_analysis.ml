(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: alg_path_analysis.ml,v 1.21 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Alg_path_analysis
   Description:
     Path analysis for document projection over the XQuery algebra.
*)

(* Path analysis (for document projection) over the XQuery algebra.
   The analysis proceeds over the algebraic plan in total analogy
   to the analysis over the XQuery core defined in Path_analysis.

   The sole difference is the treatment of variable bindings, since
   these are represented by tuples on the algebraic levels, and no
   longer by environmental mappings. As a consequence, the data
   structure that holds (intermediate) analysis results is a triple
   that, in addition to 'returned' and 'used' paths (as is the case
   for the core-level analysis), stores an associative mapping
   between tuple field names and sets of paths (see Alg_path_struct).

   An additional environment is needed in order to handle global
   variables, AOELet, typeswitches and quantifiers.


    - Michael *)


open Format

open Error

open Xquery_common_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util

open Function_analysis

open Ast_path_struct
open Alg_path_structutil

open Alg_analysis_context


let mk_source_id_map () =
  ([], 0)

(* Duplicate uris are permitted and needed in the mapping, because
   associated annotation references must be kept individually. *)
let register_document (map, ctr) uri annot =
  ((Document_id uri, annot) :: map, ctr)

(* Constructor ids are guaranteed to be unique by construction. *)
let register_constructor (map, ctr) annot =
  ((Constructor_id ctr, annot) :: map, ctr + 1), ctr


let print_intermediate_analysis formatter paths =
  let print_rptf (crname, path) =
    Print_common.print_rqname formatter crname;
    fprintf formatter ": ";
    print_path_sequence formatter path;
    fprintf formatter "\n";
  in
  let (rp, rptf, up, modif) = paths in
    fprintf formatter "INTERMEDIATE PATH ANALYSIS, ALGEBRA LEVEL: \n";
    fprintf formatter "------------------------------------------ \n";
    fprintf formatter "\nRETURNED PATHS:\n";
    fprintf formatter "------------------------------------------ \n";
    print_path_sequence formatter rp;
    fprintf formatter "\nTUPLE FIELD PATHS:\n";
    fprintf formatter "------------------------------------------ \n";
    List.iter print_rptf rptf;
    fprintf formatter "\nUSED PATHS:\n";
    fprintf formatter "------------------------------------------ \n";
    print_path_sequence formatter up;
    fprintf formatter "\nMODIFIED PATHS:\n";
    fprintf formatter "------------------------------------------ \n";
    print_path_sequence formatter modif

let add_tuple_field_paths tuple_field_paths crname paths =
  (crname, paths) :: tuple_field_paths

let get_tuple_field_paths tuple_field_paths crname =
  try
    List.assoc crname tuple_field_paths
  with
    | _ ->
	raise (Query (Prototype ("Cannot find variable $"^(Namespace_names.prefixed_string_of_rqname crname)^" in tuple field paths.")))


(* Make sure an intermediate analysis result does not yield any returned paths. *)
let expect_tuple_field_paths (paths, source_id_map) =
  match paths with
    | ([], rptf, up, _) -> (paths, source_id_map)
    | _ ->
	raise (Query (Prototype "Expected tuple field paths instead of returned paths."))

(* Make sure an intermediate analysis result does not yield any tuple field paths. *)
let expect_returned_paths (paths, source_id_map) =
  match paths with
    | (rp, [], up, modif) -> (paths, source_id_map)
    | _ ->
	raise (Query (Prototype "Expected returned paths instead of tuple field paths."))

(* Make sure an intermediate analysis result does not yield any tuple field or used paths. *)
let expect_returned_no_used_paths (paths, source_id_map) =
  match paths with
    | (rp, [], [], _) -> (paths, source_id_map)
    | _ ->
	raise (Query (Prototype "Expected returned paths instead of tuple field/used paths."))

let serialize paths =
  let (rp, rptf, up, modif) = paths in
  let rp' = imposes_subtree rp in
  let paths_in_tf = List.concat (List.map snd rptf)
  in
    Gmisc.remove_duplicates (rp' @ up @ modif @ paths_in_tf)
(*
  Nicola: this is incorrect because it uses "=" instead of
  symbol_equal
*)


let identity x op = x
let set_annot  ((analysis_annot, new_src_map) as res) aalgop_expr =
  aalgop_expr.annotation := Some { path_analysis = Some analysis_annot; streaming_annot = None };
  res


(*****************************************)
(* Path Analysis of a single aalgop-expr *)
(*****************************************)

(* This function deals with two types of contexts:
   1) The analysis context is passed on 'top-down', i.e., from parent
      to child operators, only, and never returned. It's scope is thus
      the 'current' subtree.
   2) The source id map is a global data structure of query-level scope;
      it stores a mapping from data flow source identifiers (document uris,
      artificial constructor identifiers) to AST annotations. It is both
      passed from parents to children and returned. *)
let rec stub_path_analysis_of_logical_algop_expr_internal process_result analysis_ctxt source_id_map aalgop_expr =
  (*  We manipulate quadruples of paths:
      (returned paths of an expression, 
      returned paths contained in tuple fields, 
      used paths of an expression, 
      paths modified by an expression)
  *)
  let my_expect_returned_paths_path_analysis_of_logical_algop_expr = expect_returned_paths_path_analysis_of_logical_algop_expr process_result in
  let my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr = expect_tuple_field_paths_path_analysis_of_logical_algop_expr process_result in
  let my_expect_returned_paths_path_analysis_of_logical_algop_expr_fold = expect_returned_paths_path_analysis_of_logical_algop_expr_fold process_result in

  let res = 

    match aalgop_expr.palgop_expr_name with
      
      (*****************)
      (* 'XQuery core' *)
      (*****************)
      
    | AOELetvar (_, cvname) ->
	let expr1 = access_onesub aalgop_expr.psub_expression in
	let expr2 = access_onesub aalgop_expr.pdep_sub_expression in
	let (rp1, _, up1, modif1), source_id_map' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map expr1 in
	let analysis_ctxt' = add_var_paths analysis_ctxt cvname rp1 in
	let (rp2, _, up2, modif2), source_id_map''  = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt' source_id_map' expr2 in
	(rp2, [], up1 @ up2, modif1 @ modif2), source_id_map''
	  
    | AOEIf ->
	let expr1 = access_onesub aalgop_expr.psub_expression in
	let (expr2, expr3) = access_twosub aalgop_expr.pdep_sub_expression in
	let (rp1, _, up1, modif1), source_id_map' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map expr1 in
	let (rp2, _, up2, modif2), source_id_map'' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map' expr2 in
	let (rp3, _, up3, modif3), source_id_map''' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map'' expr3 in
	  (rp2 @ rp3, [], rp1 @ up1 @ up2 @ up3, modif1 @ modif2 @ modif3), source_id_map'''

    | AOETypeswitch (clauses) ->
	let expr = access_onesub aalgop_expr.psub_expression in
	let exprs = access_manysub aalgop_expr.pdep_sub_expression in
	let (rp, _, up, modif), source_id_map' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map expr in
	let (rp1, _, up1, modif1), source_id_map'' = path_analysis_of_ts_clauses process_result analysis_ctxt source_id_map' (List.combine (Array.to_list clauses) (Array.to_list exprs)) rp in
	  (rp1, [], rp @ up @ up1, modif @ modif1), source_id_map''

    | AOEVar (cvname) ->
	let _ = access_nosub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in
	let rp = get_var_paths analysis_ctxt cvname in
	  (rp, [], [], []), source_id_map
	    
    | AOEError ->
	let exprs = access_manysub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in
	let f = my_expect_returned_paths_path_analysis_of_logical_algop_expr_fold analysis_ctxt in
	let (rp, _, up, modif), sorce_id_map' = Array.fold_right f exprs (([], [], [], []), source_id_map) in
	  ([], [], rp @ up, modif), sorce_id_map'

    | AOEEmpty
    | AOEScalar _ ->
	let _ = access_nosub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in
	  ([], [], [], []), source_id_map

    | AOESeq ->
	let (expr1, expr2) = access_twosub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in
	let (rp1, _, up1, modif1), source_id_map' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map expr1 in
	let (rp2, _, up2, modif2), source_id_map'' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map' expr2 in
	  (rp1 @ rp2, [], up1 @ up2, modif1 @ modif2), source_id_map''
	    
    | AOETreat _ ->
	let indep = access_onesub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in
	  my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map indep

    | AOEValidate _
    | AOECast _
    | AOECastable _ ->
	let indep = access_onesub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in
	let (rp, _, up, modif), source_id_map' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map indep in
	  ([], [], rp @ up, modif), source_id_map'

    | AOESome (_, cvname)
    | AOEEvery (_, cvname) ->
	let expr1 = access_onesub aalgop_expr.psub_expression in
	let expr2 = access_onesub aalgop_expr.pdep_sub_expression in
	let (rp1, _, up1, modif1), source_id_map' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map expr1 in
	let analysis_ctxt' = add_var_paths analysis_ctxt cvname rp1 in
	let (rp2, _, up2, modif2), source_id_map'' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt' source_id_map' expr2 in
	  (rp2, [], rp1 @ up1 @ up2, modif1 @ modif2), source_id_map''

    (******************)
    (* Function calls *)
    (******************)
	    
    | AOECallBuiltIn ((cfname, _), _, _, _)
    | AOECallOverloaded ((cfname, _), _) ->
        let exprs = Array.to_list (access_manysub aalgop_expr.psub_expression) in
        let _ = access_nosub aalgop_expr.pdep_sub_expression in
        let f = my_expect_returned_paths_path_analysis_of_logical_algop_expr_fold analysis_ctxt in
        let (rp, _, up, modif), source_id_map' = List.fold_right f exprs (([], [], [], []), source_id_map) in            


(*           if Namespace_names.rqname_equal cfname fn_doc then *)
(*             begin *)
(*               let docarg = List.hd exprs in *)
(*               let uri = get_doc_uri docarg in *)
(*               let root = mk_rooted_path_sequence_document uri in  *)
(*                 match get_path_to_doc_from_env uri source_id_map with *)
(*                   | None ->  *)
(*                       	let source_id_map'' = register_document source_id_map' uri aalgop_expr.annotation in *)
(* 	                      (root, [], up, modif), source_id_map'' *)
(*                   | Some x ->  *)
(*                       (root, [], up, modif), source_id_map' *)
(*             end *)
(*           else *)

            let paths =
	          match get_fun_analysis_type cfname with 
	            | UsedOnly ->
		            ([], [], up, modif)
	            | UsedReturnSimple ->
		            ([], [], rp @ up, modif)
	            | UsedReturnSubtree ->
		            let rp_subtree = imposes_subtree rp in
		              ([], [], rp_subtree @ up, modif)
	            | ReturnsPaths ->
		            (rp, [], up, modif)
	            | ReturnsDefault ->
		            let rp_subtree = imposes_subtree rp in
		              (rp_subtree, [], up, modif)
            in
	          paths, source_id_map'
                
    | AOEConvertSimple _ 
    | AOEPromoteNumeric _
    | AOEUnsafePromoteNumeric _ 
    | AOEPromoteAnyString ->
	    let expr = access_onesub aalgop_expr.psub_expression in
	    let _ = access_nosub aalgop_expr.pdep_sub_expression in
	    let (rp, _, up, modif), source_id_map' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map expr in
	      ([], [], rp @ up, modif), source_id_map'


            
(*
  TODO: make that robust!
  | AOECallUserDefined( (fname, arity), optintypes, outtype,_) ->
*)	


    (****************)
    (* Constructors *)
    (****************)

    | AOETextComputed
    | AOECommentComputed
    | AOEDocument ->       

	let expr= access_onesub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in

	let source_id_map', id = register_constructor source_id_map aalgop_expr.annotation in
	let root = mk_rooted_path_sequence_constructor id in

	let (rp, _, up, modif), source_id_map'' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map expr in

	let rp' = imposes_subtree rp in
	  (root, [], rp' @ up, modif), source_id_map''
	    
    | AOEText _
    | AOEComment _
    | AOEPI _ ->
	let source_id_map', id = register_constructor source_id_map aalgop_expr.annotation in
	let root = mk_rooted_path_sequence_constructor id in
	  (root, [], [], []), source_id_map'

    | AOEAnyElem _
    | AOEAnyAttr _ ->
	let (expr1, expr2) = access_twosub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in

	let source_id_map', id = register_constructor source_id_map aalgop_expr.annotation in
	let root = mk_rooted_path_sequence_constructor id in

	let (rp1, _, up1, modif1), source_id_map'' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map' expr1 in
	let (rp2, _, up2, modif2), source_id_map''' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map'' expr2 in

	let rp1' = imposes_subtree rp1 in
	let rp2' = imposes_subtree rp2 in
	  (root, [], rp1' @ rp2' @ up1 @ up2, modif1 @ modif2), source_id_map'''

    | AOEPIComputed ->
	let (expr1, expr2) = access_twosub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in

	let source_id_map', id = register_constructor source_id_map aalgop_expr.annotation in
	let root = mk_rooted_path_sequence_constructor id in

	let (rp1, _, up1, modif1), source_id_map'' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map' expr1 in
	let (rp2, _, up2, modif2), source_id_map''' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map'' expr2 in
	  (root @ rp1 @ rp2, [], up1 @ up2, modif1 @ modif2), source_id_map'''

    | AOEAttr _
    | AOEElem _ ->
	let exprs = access_manysub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in

	let source_id_map', id = register_constructor source_id_map aalgop_expr.annotation in
	let root = mk_rooted_path_sequence_constructor id in

	let f = my_expect_returned_paths_path_analysis_of_logical_algop_expr_fold analysis_ctxt in
	let (rp, _, up, modif), source_id_map'' = Array.fold_right f exprs (([], [], [], []), source_id_map') in
	  (root, [], rp @ up, modif), source_id_map''

	    
    (**********)
    (* Tuples *)
    (**********)
	    
    | AOEInputTuple ->
	let _ = access_nosub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in
	let input_tuple_paths = get_input_tuple_paths analysis_ctxt in
	(*let _ = print_string "INPUT TUPLE!!!\n" in*)
	  ([], input_tuple_paths, [], []), source_id_map

    | AOEMapConcat ->
	let indep = access_onesub aalgop_expr.psub_expression in
	let dep = access_onesub aalgop_expr.pdep_sub_expression in
	let (_, rptf, up, modif), source_id_map' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map indep in
	let analysis_ctxt' = set_input_tuple_paths analysis_ctxt rptf in
	let (_, rptf', up', modif'), source_id_map'' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt' source_id_map' dep in
	  (* Does AOEMapConcat perform tuple concatenation? Assumed here: yes. *)
	  (* TODO: verify that (duplicates)! *)

	  ([], rptf @ rptf', up @ up', modif @ modif'), source_id_map''

    | AOEConcatTuples
    | AOEProduct ->
	let (expr1, expr2) = access_twosub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in
	let (_, rptf, up, modif), source_id_map' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map expr1 in
	let (_, rptf', up', modif'), source_id_map'' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map' expr2 in
	  (* TODO: verify that (duplicates)! *)
	  ([], rptf @ rptf', up @ up', modif @ modif'), source_id_map''

    | AOEProject crnames ->
	let indep = access_onesub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in
	let (_, rptf, up, modif), source_id_map' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map indep in
	let rptf' = List.filter (fun (f, p) -> List.exists (fun x -> Namespace_names.rqname_equal f x) (Array.to_list crnames)) rptf in
	  ([], rptf', up, modif), source_id_map'

    | AOEOuterMapConcat crname ->
	let indep = access_onesub aalgop_expr.psub_expression in
	let dep = access_onesub aalgop_expr.pdep_sub_expression in
	let (_, rptf, up, modif), source_id_map' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map indep in
	let analysis_ctxt' = set_input_tuple_paths analysis_ctxt rptf in
	let (_, rptf', up', modif'), source_id_map'' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt' source_id_map' dep in
	let rptf'' = add_tuple_field_paths rptf' crname [] in
	  (* TODO: verify that (duplicates)! *)
	  ([], rptf @ rptf' @ rptf'', up @ up', modif @ modif'), source_id_map''

    | AOECreateTuple names ->
	    let exprs = access_manysub aalgop_expr.psub_expression in
	    let _ = access_nosub aalgop_expr.pdep_sub_expression in
          
	    (* Use ..._fold_... here? - Michael *)
	    let f expr ((rps, _, ups, modifs), source_id_map) =
	      let (rp, _, up, modif), source_id_map' = 
            my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map expr in
	        (rp:: rps, [], up :: ups, modif :: modifs), source_id_map'
	    in
	    let (rps, _, ups, modifs), source_id_map' = Array.fold_right f exprs (([], [], [], []), source_id_map) in
	    let rptf = List.map2 (fun (odt, name) path -> (name, path)) (Array.to_list names) rps in
	      ([], rptf, List.concat ups, List.concat modifs), source_id_map'

    | AOEAccessTuple crname ->
	(* This assumes that AOEAccessTuple has no indep-subexpression, but
	   has builtin input tuple access. *)
	let _ = access_nosub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in
	let input_tuple_paths = get_input_tuple_paths analysis_ctxt in
	let rp = get_tuple_field_paths input_tuple_paths crname in
	(*let _ = print_string "ACCESS TUPLE: "; print_string (Namespace_names.prefixed_string_of_rqname crname); print_string "\n" in*)
	  (rp, [], [], []), source_id_map

    | AOEMap ->
	let indep = access_onesub aalgop_expr.psub_expression in
	let dep = access_onesub aalgop_expr.pdep_sub_expression in
	let (_, rptf, up, modif), source_id_map' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map indep in
	let analysis_ctxt' = set_input_tuple_paths analysis_ctxt rptf in
	let (_, rptf', up', modif'), source_id_map'' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt' source_id_map' dep in
	  ([], rptf', up @ up', modif @ modif'), source_id_map''
	
    (* TODO: verify AOENullMap semantics! *)
    | AOENullMap crname
    | AOEMapIndexStep crname
    | AOEMapIndex crname ->
	let indep = access_onesub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in
	let (_, rptf, up, modif), source_id_map' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map indep in
	let rptf' = add_tuple_field_paths rptf crname [] in
	  ([], rptf', up, modif), source_id_map'

    | AOEMapFromItem cvname ->
	let indep = access_onesub aalgop_expr.psub_expression in
	let dep = access_onesub aalgop_expr.pdep_sub_expression in
	let (rp, _, up, modif), source_id_map' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map indep in
	let analysis_ctxt' = add_var_paths analysis_ctxt cvname rp in
	let (_, rptf, up', modif'), source_id_map'' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt' source_id_map' dep in
	  ([], rptf, rp @ up @ up', modif @ modif'), source_id_map''

    | AOEMapToItem ->
	let indep = access_onesub aalgop_expr.psub_expression in
	let dep = access_onesub aalgop_expr.pdep_sub_expression in
	let (_, rptf, up, modif), source_id_map' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map indep in
 	let analysis_ctxt' = set_input_tuple_paths analysis_ctxt rptf in
	let (rp', _, up', modif'), source_id_map'' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt' source_id_map' dep in
	  (rp', [], up @ up', modif @ modif'), source_id_map''

    | AOEJoin _ ->
	let (expr1, expr2) = access_twosub aalgop_expr.psub_expression in
	let exprs = access_manysub aalgop_expr.pdep_sub_expression in
	let (_, rptf1, up1, modif1), source_id_map' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map expr1 in
	let (_, rptf2, up2, modif2), source_id_map'' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map' expr2 in
	(* TODO: verify that (duplicates!) *)
	let rptf' = rptf1 @ rptf2 in
 	let analysis_ctxt' = set_input_tuple_paths analysis_ctxt rptf' in
	let f = my_expect_returned_paths_path_analysis_of_logical_algop_expr_fold analysis_ctxt' in
	let (rp, _, up, modif), source_id_map''' = Array.fold_right f exprs (([], [], [], []), source_id_map'') in
	  ([], rptf', rp @ up1 @ up2 @ up, modif1 @ modif2 @ modif), source_id_map'''

    | AOELeftOuterJoin (crname, _) ->
	let (expr1, expr2) = access_twosub aalgop_expr.psub_expression in
	let exprs = access_manysub aalgop_expr.pdep_sub_expression in
	let (_, rptf1, up1, modif1), source_id_map' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map expr1 in
	let (_, rptf2, up2, modif2), source_id_map'' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map' expr2 in
	(* TODO: verify that (duplicates!) *)
	(* What is the semantics of that crname? *)
	let rptf' = add_tuple_field_paths (rptf1 @ rptf2) crname [] in
 	let analysis_ctxt' = set_input_tuple_paths analysis_ctxt rptf' in
	let f = my_expect_returned_paths_path_analysis_of_logical_algop_expr_fold analysis_ctxt' in
	let (rp, _, up, modif), source_id_map''' = Array.fold_right f exprs (([], [], [], []), source_id_map'') in
	  ([], rptf', rp @ up1 @ up2 @ up, modif1 @ modif2 @ modif), source_id_map'''

    (* TODO: check semantics! *)
    (* Finish that! - Michael *)
    | AOEGroupBy gds ->
	let expr = access_onesub aalgop_expr.psub_expression in
	let exprs = access_manysub aalgop_expr.pdep_sub_expression in
	let (_, rptf, up, modif), source_id_map' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map expr in
	let analysis_ctxt' = set_input_tuple_paths analysis_ctxt rptf in
	let f = my_expect_returned_paths_path_analysis_of_logical_algop_expr_fold analysis_ctxt' in
	let (rp', _, up', modif'), source_id_map'' = Array.fold_right f exprs (([], [], [], []), source_id_map') in

	let aggregate_paths = List.map (fun gd -> (let (_, a) = gd.aggregate_name in (a, []))) gds in
	  ([],  aggregate_paths @ rptf, rp' @ up @ up', modif @ modif'), source_id_map''

    | AOEOrderBy _
    | AOESelect _ ->
	let expr = access_onesub aalgop_expr.psub_expression in
	let exprs = access_manysub aalgop_expr.pdep_sub_expression in
	let (_, rptf, up, modif), source_id_map' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map expr in
	let analysis_ctxt' = set_input_tuple_paths analysis_ctxt rptf in
	let f = my_expect_returned_paths_path_analysis_of_logical_algop_expr_fold analysis_ctxt' in
	let (rp', _, up', modif'), source_id_map'' = Array.fold_right f exprs (([], [], [], []), source_id_map') in
	  ([], rptf, rp' @ up @ up', modif @ modif'), source_id_map''

(*
    | AOEDistinct crname ->
	let indep = access_onesub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in
	  my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map indep
*)

    (*********)
    (* XPath *)
    (*********)
	    
    | AOEParse uri ->
	let _ = access_nosub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in
	let source_id_map' = register_document source_id_map uri aalgop_expr.annotation in
	let root = mk_rooted_path_sequence_document uri in
	  (root, [], [], []), source_id_map'

    | AOETreeJoin (axis, anode_test) ->
	let indep = access_onesub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in

	(* Why are used paths not allowed, here? - Michael *)
	(* let (rp, _, _), source_id_map' = my_expect_returned_no_used_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map indep in *)

	let (rp, _, _, modif), source_id_map' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map indep in
	let add_step (root, (path, subtree)) = (root, (path @ [(axis, anode_test)], subtree)) in
	(*let _ = print_string "TREE JOIN\n" in*)
	let rp' = List.map add_step rp in
	  (rp', [], [], modif), source_id_map'

    (* Finish that! - Michael *)
    | AOETupleTreePattern (input, pattern) ->
	let indep = access_onesub aalgop_expr.psub_expression in
	let _ = access_nosub aalgop_expr.pdep_sub_expression in

	let (_, rptf, up, modif), source_id_map' = my_expect_tuple_field_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map indep in
	let rp_lists = List.map (fun (f, paths) -> if (Namespace_names.rqname_equal input f) then paths else []) rptf in
	let rp = List.concat rp_lists in

	let (rp', up'), output = path_analysis_of_twig_pattern (rp, up) pattern 0 None in
	  begin
	    match output with
	      | Some f -> ([], (f, rp') :: rptf, up', modif), source_id_map'
	      | None -> raise (Query (Prototype "Expected output field in path analysis of tuple tree pattern"))
	  end

(*
    | AOEPrune (field, axis) ->
*)
	    

(**********)
(* Update *)
(**********)

    | AOESnap _ -> (* Nicola: we have to see if we need to make a distinction or not between pending updates and applied updates *)
	let _ = access_nosub aalgop_expr.psub_expression in
	let dep = access_onesub aalgop_expr.pdep_sub_expression in
	  my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map dep       

        (* -- Nicola: the part about updates is more of a hack until we finish the path analysis for
           the algebra with updates. *)
    | AOECopy ->
        let indep = access_onesub aalgop_expr.psub_expression in
	    let _ = access_nosub aalgop_expr.pdep_sub_expression in
	    let (rp, _, up, modif), source_id_map' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map indep in
          (* Nicola: not so sure about what copy{} should return *)
	    let source_id_map', id = register_constructor source_id_map aalgop_expr.annotation in
	    let root = mk_rooted_path_sequence_constructor id in
	      (root, [], up, modif), source_id_map'
    | AOEDelete  ->
        let indep = access_onesub aalgop_expr.psub_expression in
	    let _ = access_nosub aalgop_expr.pdep_sub_expression in
	    let (rp, _, up, modif), source_id_map' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map indep in
          (* here, the `real' path analysis says that delete modifies
             modif @ rp/descendend-or-self::* *)
	      ([], [], up, modif @ rp), source_id_map'        
    | AOEInsert loc -> 
	    let (expr1, expr2) = access_twosub aalgop_expr.psub_expression in
        let _ = access_nosub aalgop_expr.pdep_sub_expression in
	    let (rp1, _, up1, modif1), source_id_map' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map expr1 in
	    let (rp2, _, up2, modif2), source_id_map'' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map' expr2 in

          (* here, according to the `real' path analysis, we should also add
             rp1/descendant-or-self::* to used nodes and 
             rp2/descendant-or-self::* to the modifed nodes
             Nevertheless, when inserting as a sibling, we should do something different... 
             This will need to be fixed when implementing a full analysis of updates. *)
          begin
            match loc with                 
              | AOUAfter | AOUBefore -> (* insert as a sibling *)
                  (* this is not entirely correct, because rp2/parent::* is also modified *)
                  ([], [], up1 @ up2 @ rp1, modif1 @ modif2 @ rp2), source_id_map''
              | _ -> (* insert as a child *)
                  ([], [], up1 @ up2 @ rp1, modif1 @ modif2 @ rp2), source_id_map''
          end
    | AOEReplace _ | AOERename _ ->         
	    let (expr1, expr2) = access_twosub aalgop_expr.psub_expression in
        let _ = access_nosub aalgop_expr.pdep_sub_expression in
	    let (rp1, _, up1, modif1), source_id_map' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map expr1 in
	    let (rp2, _, up2, modif2), source_id_map'' = my_expect_returned_paths_path_analysis_of_logical_algop_expr analysis_ctxt source_id_map' expr2 in
          ([], [], up1 @ up2 @ rp2, modif1 @ modif2 @ rp1), source_id_map''
    | AOESet _ ->
        raise (Query (Prototype "Path analysis not supported for variable assignment."))
    | AOEImperativeSeq ->
        raise (Query (Prototype "Path analysis not supported for imperative sequence."))

          (* end of the Update portion *)

    | _ ->
        raise (Query (Prototype ("Path analysis not supported for this algebraic operator: " ^
                                    (string_of_algop_expr_name aalgop_expr.palgop_expr_name))))
  in
    process_result res aalgop_expr
      
      
and simple_path_analysis_of_logical_algop_expr_internal  analysis_ctxt source_id_map aalgop_expr  = 
  stub_path_analysis_of_logical_algop_expr_internal identity  analysis_ctxt source_id_map aalgop_expr
      
      
and path_analysis_of_logical_algop_expr_internal analysis_ctxt source_id_map aalgop_expr = 
    stub_path_analysis_of_logical_algop_expr_internal set_annot analysis_ctxt source_id_map aalgop_expr

and path_analysis_of_ts_clauses process_result analysis_ctxt source_id_map cls_exprs rp =
  match cls_exprs with
    | [] -> ([], [], [], []), source_id_map
    | ((_, ocvn), expr) :: cls_exprs_rest ->
	let analysis_ctxt' = 
	  match ocvn with
	    | Some cvn -> add_var_paths analysis_ctxt cvn rp
	    | None -> analysis_ctxt
	in
	let (rp, _, up, modif), source_id_map' = expect_returned_paths_path_analysis_of_logical_algop_expr process_result analysis_ctxt' source_id_map expr in
	let (rp', _, up', modif'), source_id_map'' = path_analysis_of_ts_clauses process_result analysis_ctxt source_id_map' cls_exprs_rest rp in
		(rp @ rp', [], up @ up', modif @ modif'), source_id_map''

(* Does not yet handle predicates! - Michael *)
and path_analysis_of_twig_pattern (rp, up) pattern index opt_axis =
  let add_step axis anode_test (root, (path, subtree)) = (root, (path @ [(axis, anode_test)], subtree)) in

  let output = pattern.(index).out in
    if (index < Array.length pattern)
    then
      let (rp', up') =
	let node_test = pattern.(index).node_test in
	  match opt_axis with
	    | Some a ->
		begin
		  match node_test with
		    | Some nt -> (List.map (add_step a nt) rp, [])
		    | None -> raise (Query (Prototype "Expected node test in path_analysis_of_twig_pattern"))
		end
	    | None ->
		begin
		  match node_test with
		    | Some nt -> raise (Query (Prototype "Found axis without node test in path_analysis_of_twig_pattern"))
		    | None -> (rp, up)
		end
      in

	match pattern.(index).child_twig with
	    
	  (* There is some child twig *)
	  | Some (axis, child_index) -> path_analysis_of_twig_pattern (rp', up') pattern child_index (Some axis)
		
	  (* There is no child twig *)
	  | None -> (rp', up'), output
    else (rp, up), output

and wrap_path_analysis_of_logical_algop_expr process_result wrap_fun analysis_ctxt source_id_map aalgop_expr =
  wrap_fun (stub_path_analysis_of_logical_algop_expr_internal process_result analysis_ctxt source_id_map aalgop_expr)

and expect_returned_paths_path_analysis_of_logical_algop_expr process_result analysis_ctxt source_id_map aalgop_expr =
  wrap_path_analysis_of_logical_algop_expr process_result expect_returned_paths analysis_ctxt source_id_map aalgop_expr

and expect_returned_no_used_paths_path_analysis_of_logical_algop_expr process_result  analysis_ctxt source_id_map aalgop_expr =
  wrap_path_analysis_of_logical_algop_expr process_result expect_returned_no_used_paths analysis_ctxt source_id_map aalgop_expr

and expect_tuple_field_paths_path_analysis_of_logical_algop_expr process_result  analysis_ctxt source_id_map aalgop_expr =
  wrap_path_analysis_of_logical_algop_expr process_result expect_tuple_field_paths analysis_ctxt source_id_map aalgop_expr

and expect_returned_paths_path_analysis_of_logical_algop_expr_fold process_result analysis_ctxt expr ((rp, _, up, modif), source_id_map)  =
  let (rp1, _, up1, modif1), source_id_map' = expect_returned_paths_path_analysis_of_logical_algop_expr process_result analysis_ctxt source_id_map expr in
    (rp @ rp1, [], up @ up1, modif @ modif1), source_id_map'


(**********)
(* Prolog *)
(**********)

(* TODO: make that robust! *)
let path_analysis_of_logical_algop_function_decls analysis_ctxt source_id_map aalgop_function_decls =
  (analysis_ctxt, [], []), source_id_map (* Nicola: this needs to be fixed *)

let path_analysis_of_logical_algop_decl analysis_ctxt source_id_map aalgop_decl =
  match aalgop_decl.alg_decl_name with
  | AOEVarDecl (oast, cvname) ->
      let indep = access_onesub aalgop_decl.alg_decl_indep in
      let _ = access_nosub aalgop_decl.alg_decl_dep in
      let (rp, _, up, modif), source_id_map' = expect_returned_paths_path_analysis_of_logical_algop_expr set_annot analysis_ctxt source_id_map indep in
      let analysis_ctxt' = add_var_paths analysis_ctxt cvname rp in
	(analysis_ctxt', up, modif), source_id_map'
  | _ ->
      raise (Query (Prototype "Do not know yet what to do with external variables/indices during projection!"))

let rec path_analysis_of_logical_algop_decls analysis_ctxt source_id_map aalgop_decls =
  match aalgop_decls with
    | [] -> (analysis_ctxt, [], []), source_id_map
    | hd :: tl ->
	    let (analysis_ctxt', up', modif'), source_id_map' = path_analysis_of_logical_algop_decl analysis_ctxt source_id_map hd in
	    let (analysis_ctxt'', up'', modif''), source_id_map'' = path_analysis_of_logical_algop_decls analysis_ctxt' source_id_map' tl in
	      (analysis_ctxt'', up' @ up'', modif' @ modif''), source_id_map''

(*
  NOTE:
  This function had to be disabled due to the Galax infrastructure not properly handling
  generated prototype exceptions during precompilation of standard library modules.
  Seems to work now. - Michael
*)
(*
let path_analysis_of_logical_algop_prolog_internal analysis_ctxt source_id_map aalgop_prolog =
  (analysis_ctxt, []), source_id_map
*)

let path_analysis_of_logical_algop_prolog_internal analysis_ctxt source_id_map aalgop_prolog =
  let (analysis_ctxt', up', modif'), source_id_map' = path_analysis_of_logical_algop_function_decls analysis_ctxt source_id_map aalgop_prolog.palgop_prolog_functions in
  let (analysis_ctxt'', up'', modif''), source_id_map'' = path_analysis_of_logical_algop_decls analysis_ctxt' source_id_map' aalgop_prolog.palgop_prolog_vars in
    (analysis_ctxt'', up' @ up'', modif' @ modif''), source_id_map''


(**********)
(* Module *)
(**********)

let path_analysis_of_logical_algop_xmodule_internal analysis_ctxt source_id_map aalgop_xmodule =
  let (analysis_ctxt', up, modif), source_id_map' = path_analysis_of_logical_algop_prolog_internal analysis_ctxt source_id_map aalgop_xmodule.palgop_module_prolog in
  let f = expect_returned_paths_path_analysis_of_logical_algop_expr_fold set_annot analysis_ctxt' in
  let (rp', _, up', modif'), sorce_id_map'' = List.fold_right f aalgop_xmodule.palgop_module_statements (([], [], [], []), source_id_map) in
    (rp', [], up' @ up, modif' @ modif), sorce_id_map''


(**************)
(* Annotation *)
(**************)
      
let annotate_paths paths source_id_map =

  (* TODO: implement more efficiently (sorted lists, partitioning)! *)
  let filter_paths id paths =
    List.filter (fun (root_id, _) -> (root_id = id)) paths
  in

  let annotate_paths_single_op (id, annotation) =
    let path_streaming_annot = Some (filter_paths id paths) in
      match !annotation with
        | None ->  annotation := Some { path_analysis = None; streaming_annot = path_streaming_annot }
        | Some a -> annotation := Some { a with streaming_annot = path_streaming_annot }
  in

  let (ids_to_annotations, _) = source_id_map in
    List.iter annotate_paths_single_op ids_to_annotations


(***********************************************************************************************)
(* ---------------------------------------- Exposed ------------------------------------------ *)
(***********************************************************************************************)

(*****************)
(* Path analysis *)
(*****************)

let path_analysis_of_logical_algop_expr logical_algop_expr =
  let analysis_ctxt = build_analysis_context () in
  let source_id_map = mk_source_id_map () in
  let paths, source_id_map' = path_analysis_of_logical_algop_expr_internal analysis_ctxt source_id_map logical_algop_expr in
    annotate_paths (serialize paths) source_id_map'

let path_analysis_of_logical_algop_prolog logical_algop_prolog =
  let analysis_ctxt = build_analysis_context () in
  let source_id_map = mk_source_id_map () in
  let (_, up, modif), source_id_map' = path_analysis_of_logical_algop_prolog_internal analysis_ctxt source_id_map logical_algop_prolog in
    annotate_paths (serialize ([], [], up, modif)) source_id_map'

let path_analysis_of_logical_algop_xmodule logical_algop_xmodule =
  let analysis_ctxt = build_analysis_context () in
  let source_id_map = mk_source_id_map () in
  let paths, source_id_map' = path_analysis_of_logical_algop_xmodule_internal analysis_ctxt source_id_map logical_algop_xmodule in
    annotate_paths (serialize paths) source_id_map'(*;

    (* TODO: remove that - debugging! *)
    print_intermediate_analysis Format.std_formatter paths*)

(* This function is used in a hackish way by Galax-project.
   It should be removed. - Michael *)
let path_analysis_of_statement compiled_statement =
  let _ = build_analysis_context () in
  let source_id_map = mk_source_id_map () in
(*   let paths, source_id_map' = simple_path_analysis_of_logical_algop_expr_internal analysis_ctxt source_id_map compiled_statement in *)
  let paths, source_id_map' = ([],[],[],[]) , source_id_map in
  let paths' = serialize paths in
(*     print_intermediate_analysis Format.std_formatter paths; *)
    paths'


(**************************)
(* Streamability analysis *)
(**************************)

let is_streaming_prohibitive paths =

  (* All reverse axes are prohibitive. *)
  let is_streaming_prohibitive_axis (axis, _) =
    match axis with
      | Ancestor
      | Ancestor_or_self
      | Following_sibling
      | Preceding_sibling
      | Parent -> true
      | _ -> false
  in

  (* A single prohibitive axis step is prohibitiv. *)
  let is_streaming_prohibitive_path (_, (path, _)) =
    List.exists is_streaming_prohibitive_axis path
  in
    List.exists is_streaming_prohibitive_path paths


(************)
(* Printing *)
(************)
let print_full_analysis formatter paths =
  fprintf formatter "FULL PATH ANALYSIS, ALGEBRA LEVEL:\n";
  fprintf formatter "----------------------------------\n";
  print_path_sequence formatter paths;
