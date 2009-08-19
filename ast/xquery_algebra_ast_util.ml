(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_algebra_ast_util.ml,v 1.70 2008/02/01 02:45:26 mff Exp $ *)

(* Module: Xquery_algebra_ast_util
   Description:
     This module implements some useful operations the XQuery Algebra
     AST.
*)

open Finfo

open Xquery_common_ast
open Xquery_algebra_ast

open Error


(*************************)
(* Algebra AST utilities *)
(*************************)

let fmkapattern apattern_desc fi =
    { papattern_desc = apattern_desc;
      papattern_loc = fi }

let fmkasequencetype c fi =
    { pasequencetype_desc = c;
      pasequencetype_loc  = fi }

let algop_mkop eval_code eval_sig op_name indep_expressions dep_expressions annot comp_annot e_handle expr_loc =
  { palgop_expr_name = op_name;
    palgop_expr_eval = ref eval_code;
    palgop_expr_eval_sig = eval_sig;
    annotation = annot;
    compile_annotations = comp_annot;
    psub_expression = indep_expressions;
    pdep_sub_expression = dep_expressions;
    palgop_expr_origin = e_handle;  (* Handle to the original expr *)
    palgop_expr_loc = expr_loc;}  

let algop_decl_mkop eval_code op_name indep_expressions dep_expressions annot expr_loc =
  { alg_decl_name 	  = op_name;
    alg_decl_eval 	  = ref eval_code;
    alg_decl_indep 	  = indep_expressions;
    alg_decl_dep   	  = dep_expressions;
    alg_decl_annotation   = annot;
    alg_decl_loc   	  = expr_loc }

let fmkalgop_function_body argnames log_plan phys_plan return_type = 
  { palgop_func_formal_args = argnames;
    palgop_func_output_type = return_type;
    palgop_func_optimized_logical_plan = ref log_plan;
    palgop_func_physical_plan = ref phys_plan;
  }

let fmkalgop_function_decl desc fi =
  { palgop_function_decl_desc = desc;
    palgop_function_decl_loc = fi }

let fmkalgop_prolog fns vars indices =
  { palgop_prolog_functions = fns;
    palgop_prolog_vars      = vars;
    palgop_prolog_indices   = indices }

let fmkalgop_xmodule prolog statements = 
  { palgop_module_prolog = prolog;
    palgop_module_statements = statements }


(* A more specific function adding some "dummy" code *)

let dummy_code = None (* No code for now *)
let dummy_sig = None 

let aalgop_mkop op_name indep_expressions dep_expressions e_handle expr_loc =
  algop_mkop dummy_code dummy_sig op_name indep_expressions dep_expressions (ref None) e_handle expr_loc

let logical_aalgop_mkop op_name indep_expressions dep_expressions e_handle expr_loc =
  algop_mkop () dummy_sig op_name indep_expressions dep_expressions (ref None) e_handle expr_loc

let aalgop_decl_mkop op_name indep_expressions dep_expressions expr_loc =
  algop_decl_mkop dummy_code op_name indep_expressions dep_expressions (ref None) expr_loc

let logical_aalgop_decl_mkop op_name indep_expressions dep_expressions expr_loc =
  algop_decl_mkop () op_name indep_expressions dep_expressions (ref None) expr_loc

let replace_aalgop_name algop op_name =
  aalgop_mkop op_name algop.psub_expression algop.pdep_sub_expression None algop.palgop_expr_origin algop.palgop_expr_loc

let rec copy_algop op = 
  algop_mkop !(op.palgop_expr_eval) op.palgop_expr_eval_sig op.palgop_expr_name 
    (copy_subexpr op.psub_expression) (copy_subexpr op.pdep_sub_expression)
    op.annotation op.compile_annotations 
    op.palgop_expr_origin op.palgop_expr_loc

and copy_subexpr se = 
  match se with
      NoSub -> 
	NoSub
    | OneSub s ->
	OneSub (copy_algop s)
    | TwoSub (s0,s1) ->
	TwoSub ((copy_algop s0), (copy_algop s1))
    | ManySub sa ->
	ManySub (Array.map copy_algop sa)

(* Generate a fresh variable name in the 'fs:' namespace, and a
   corresponding variable algop. *)

let mk_annotation fc bc af rt tfuc = 
    { use_counts             = fc;
      bound_usage_counts     = bc;
      accessed_fields        = af;
      returned_fields        = rt;
      tuple_field_use_counts = tfuc;
      returned_path          = Empty;
      accessed_path          = Empty;
      updated_path           = Empty; 
    } 

let variable_comp v = 
  Some (mk_annotation [(v,(1,Once))] [] [] [] ([], [], NoTable))
 
(* This is slightly abusive to return unit in both cases *)
let access_nosub x = 
  match x with
  | NoSub -> ()        
  | _ -> raise (Query (Malformed_Algebra_Expr("Incorrect number of sub expressions: expected NoSub")))

let access_unitsub x =
  match x with
    | NoSub -> ()
    | _ -> raise (Query (Malformed_Algebra_Expr("Incorrect number of sub expressions: expected (UnitSub)")))

let access_onesub x = 
  match x with
    | OneSub y -> y
    | NoSub -> raise (Query (Malformed_Algebra_Expr("Incorrect number of sub expressions: expected (OneSub). Found NoSub")))
    | TwoSub _ -> raise (Query (Malformed_Algebra_Expr("Incorrect number of sub expressions: expected (OneSub). Found TwoSub")))
    | ManySub _ -> raise (Query (Malformed_Algebra_Expr("Incorrect number of sub expressions: expected (OneSub). Found ManySub")))

let access_twosub x = 
  match x with
    | TwoSub(x,y) -> (x,y) 
    | _ -> raise (Query (Malformed_Algebra_Expr ("Incorrect number of sub expressions: expected (TwoSub)")))

let access_manysub x =
  match x with
    | ManySub y -> y
    | _ -> raise (Query (Malformed_Algebra_Expr ("Incorrect number of sub expressions: expected (ManySub)")))

(*************************)
(* Operations on modules *)
(*************************)

let empty_statement = logical_aalgop_mkop (AOEEmpty) NoSub NoSub None None Finfo.bogus

let empty_prolog_plan =
    { palgop_prolog_functions = [];
      palgop_prolog_vars = [];
      palgop_prolog_indices = [] }

let empty_xmodule = { palgop_module_prolog = empty_prolog_plan;
		      palgop_module_statements = [] }

let merge_alg_prologs prolog1 prolog2 =
  { palgop_prolog_functions =
      prolog1.palgop_prolog_functions @ prolog2.palgop_prolog_functions;
    palgop_prolog_vars =
      prolog1.palgop_prolog_vars @ prolog2.palgop_prolog_vars;
    palgop_prolog_indices =
      prolog1.palgop_prolog_indices @ prolog2.palgop_prolog_indices }

let subexpr_to_list se = 
  match se with
    | NoSub         -> []
    | OneSub x      -> x :: []
    | TwoSub (x,y)  -> x :: y :: []
    | ManySub x     -> (Array.to_list x)

let get_group_names gd = 
  gd.group_by_names

let get_induced_group gd = 
  gd.induced_groups

let get_aggregate_name gd = 
  let odt, name = gd.aggregate_name in
    name

let get_aggregate_type gd = 
  let odt, name = gd.aggregate_name in
    odt

let get_aggregate_name_and_type gd =
  gd.aggregate_name

let get_valid_names gd =
  gd.must_be_valid

let mk_group_desc nms ind valid agg =
  { group_by_names = nms;
    induced_groups = ind;
    must_be_valid  = valid;
    aggregate_name = agg }

let split_main_module_plan xmod =
  (xmod.palgop_module_prolog,xmod.palgop_module_statements)


(**
 * Twig Pattern utils
 **)
let get_select_path p =
  let rec get_select_path_aux index =
    match p.(index).child_twig with
    | Some (a,ch) ->
	ch :: (get_select_path_aux ch)
    | None -> []
  in
  Array.of_list (get_select_path_aux 0)

let get_all_outputs_from_twig_pattern p =
  let get_output twignode =
    match twignode.out with 
    | Some field ->
	[field]
    | None -> 
	[]
  in
  let pattern_list = Array.to_list p in
  List.flatten (List.map get_output pattern_list)

let get_restored_outputs_from_twig_pattern p =
  let get_output twignode =
    match twignode.out with 
    | Some field ->
	if twignode.restore_out then [field] else []
    | None -> 
	[]
  in
  let pattern_list = Array.to_list p in
  List.flatten (List.map get_output pattern_list)


let get_node_tests_from_twig_pattern p =
  let get_nt n = 
    match n.node_test with
    | Some nt -> [nt]
    | None -> []
  in 
  List.flatten (List.map get_nt (Array.to_list p))

(* does not include predicates *)
let rec get_leaf_twig_node p index = 
  match p.(index).child_twig with
  | None -> p.(index)
  | Some (twig_type, twig_index) -> 
      get_leaf_twig_node p twig_index

(* does include predicates *)
let rec get_first_leaf_node_index p index =
  match p.(index).child_twig with
  | None -> 
      begin
	match  p.(index).pred_twigs with
	| (_, i) :: tl -> get_first_leaf_node_index p i
	| [] -> index
      end
  | Some (_, i) -> get_first_leaf_node_index p i

let increase_twig_index offset twignode =
  let _ = 
    match twignode.child_twig with
    | Some (twig_type, twig_index) ->
	twignode.child_twig <- Some (twig_type, twig_index + offset)
    | None -> ()
  in
  twignode.pred_twigs <- List.map 
      (fun (tt,ti) -> (tt, ti + offset)) 
      twignode.pred_twigs


let find_attach_point pattern vname =
  let index = ref (-1) in
  for i = 0 to Array.length pattern -1 do
    match pattern.(i).out with
    | Some o when o = vname -> index := i
    | _ -> ()
  done;
  if !index > 0 then
    !index
  else
     raise (Query (Malformed_Algebra_Expr ("Cannot find attachpoint for twig merge")))

let append_twigs p1 p2 vname =
  let leaf = p1.(find_attach_point p1 vname) in
(*  let _ = Array.iter (fun x -> x.out <- None) p1 in *)
  let _ = Array.iter (fun x -> x.restore_out <- false) p1 in 
  let offset = (Array.length p1) -1 in  

  (* 1. increase all indices in p2 with Array.length p1 *)
  let _ = Array.map (increase_twig_index offset) p2 in

  (* 2. add the list of corresponding indices to the twiglist of the leaf of p1 *)
  let _ =
    match leaf.child_twig with
    | None -> leaf.child_twig <- p2.(0).child_twig
    | Some _ -> raise (Query (Malformed_Algebra_Expr ("Attempt to add a second child twig (append_twigs).")))
  in
	
  (* 3. concatenate arrays p1 and p2 -- except for the root of p2 *) 
  let len =  (Array.length p2) -1 in
  let res = Array.append p1 (Array.sub p2 1 len) in
  res

let rec merge_twigs p1 p_list =
  match p_list with
  | (a2,p2) :: tl ->
      let i = find_attach_point p1 a2 in
      let offset = (Array.length p1) -1 in  
      let _ = Array.map (increase_twig_index offset) p2 in
      let _ = Array.map (fun tn -> tn.restore_out <- false) p2 in
      let p2_child_twig = 
	match p2.(0).child_twig with
	| Some n -> n
	| None -> raise (Query (Malformed_Algebra_Expr ("Attempt to merge an empty twig (merge_twigs).")))
      in
      let _ = p1.(i).pred_twigs <- p1.(i).pred_twigs @ [ p2_child_twig ] @ p2.(0).pred_twigs in
      let len =  (Array.length p2) -1 in
      let res = Array.append p1 (Array.sub p2 1 len) in
      merge_twigs res tl
  | [] -> p1


let is_leaf_node pattern node =
  match node.child_twig, node.pred_twigs with
  | None,[]  -> true
  | _    -> false

let is_root_node pattern node =
  match node.node_test with
  | Some _ -> false
  | None   -> true

let rec get_parent_node_index pattern node =
  begin
    let parent_index = ref (-1) in
    for i = 0 to (Array.length pattern) -1 do
      begin
	let match_twig (_, ind) =
	  if pattern.(ind) = node
	  then parent_index := i
	in
	(match pattern.(i).child_twig with
	| Some twig -> match_twig twig
	| None -> ();
	);
	let _ = List.map match_twig pattern.(i).pred_twigs in ()
      end
    done;
    !parent_index
  end

let get_child_node_indices pattern node =
  let ch =
    match node.child_twig with
    | Some (_, i) -> [i]
    | _ -> []
  in
  let get_ch_node (_, i) = i in
  ch @ List.map get_ch_node node.pred_twigs

let rec get_sub_node_indices pattern node =
  let children_indices = get_child_node_indices pattern node in
  let f lst child_index =
    let sub_indices = get_sub_node_indices pattern pattern.(child_index) in
    lst @ sub_indices
  in
  children_indices @ List.fold_left f [] children_indices
	

let replace_field_in_pattern p cur_name replacement_name =
  let changed = ref true in
  (* start at index one, since the first outfield is always empty *)
  for i = 1 to Array.length p -1 do
    match p.(i).out with
    | Some field 
      when field = cur_name ->
	begin
	  p.(i).out <- Some replacement_name;
	  changed := true
	end
    | _ -> ()
  done;
  !changed


(*
 * a treepattern is streamable when
 * - there are no branches (predicates)
 * - it contains only child, descendant or descentant-or-self twigs
 *)
let is_streamable_treepattern p =
  let is_streamable_twig_node node =
    if (List.length node.pred_twigs) = 0 then
      match node.child_twig with
      | Some (Descendant,_)
      | Some (Descendant_or_self, _)
      | Some (Child, _) 
      | Some (Attribute, _) 
      | None -> 
	  true
      | _ -> 
	  false
    else
      false
  in
  let fold_fun a b = a && b in
  Array.fold_left fold_fun true (Array.map is_streamable_twig_node p)

let is_single_step_twig p =
  Array.length p = 2

(* build axis array 
 * - Returns an array telling us for each corresponding 
 *   twig node how it was reached
 *)
let get_axis_array pattern =
  let axis_array = Array.create (Array.length pattern) Xquery_common_ast.Child  in

  let rec build_axis_array_aux i =
    let ch_t = match pattern.(i).child_twig with
               | Some t -> [t]
	       | None -> []
    in
    let twigs = ch_t @ pattern.(i).pred_twigs in
    List.iter
      (fun (ax,ind) ->  
	axis_array.(ind) <- ax; 
	build_axis_array_aux ind
      ) twigs
  in 

  build_axis_array_aux 0; 
  axis_array

let pname_of_algop algop =
  match algop.palgop_expr_eval_sig with
  | None ->
      raise (Query (Internal_Error "Accessing the physical operator name before it has been selected"))
  | Some (name,_,_) -> name

let string_of_algop_expr_name algop_expr_name = 
  match algop_expr_name with
  | AOEIf 	-> "AOEIf"
  | AOEWhile 	-> "AOEWhile"
  | AOETypeswitch _ -> "AOETypeswitch"
  | AOELetvar _	-> "AOELetvar"
  | AOEVar _ 	-> "AOEVar"
  | AOEScalar _ -> "AOEScalar"
  | AOESeq      -> "AOESeq"
  | AOEEmpty    -> "AOEEmpty"
  | AOEDocument -> "AOEDocument"
  | AOEPI _ 	-> "AOEPI"
  | AOEPIComputed  -> "AOEPIComputed"
  | AOEComment _   -> "AOEComment"
  | AOECommentComputed -> "AOECommentComputed"
  | AOEText _ 	    -> "AOEText"
  | AOECharRef _    -> "AOECharRef"
  | AOETextComputed -> "AOETextComputed"
  | AOEElem _ 	    -> "AOEElem"
  | AOEAnyElem _    -> "AOEAnyElem"
  | AOEAttr _ 	    -> "AOEAttr"
  | AOEAnyAttr _    -> "AOEAnyAttr"
  | AOEError 	    -> "AOEError"
  | AOETreat _ 	    -> "AOETreat"
  | AOEValidate _   -> "AOEValidate"
  | AOECast _ 	    -> "AOECast"
  | AOECastable _   -> "AOECastable"
  | AOESome _ 	    -> "AOESome"
  | AOEEvery _ 	    -> "AOEEvery"
  | AOEInputTuple   -> "AOEInputTuple"
  | AOECallBuiltIn _-> "AOECallBuiltIn"
  | AOECallOverloaded _	 -> "AOECallOverloaded"
  | AOECallUserDefined _ -> "AOECallUserDefined"
  | AOEConvertSimple _ 	 -> "AOEConvertSimple"
  | AOEPromoteNumeric _  -> "AOEPromoteNumeric"
  | AOEPromoteAnyString  -> "AOEPromoteAnyString"
  | AOEUnsafePromoteNumeric _ -> "AOEUnsafePromoteNumeric"
  | AOEServerImplements	_ -> "AOEServerImplements"
  | AOEForServerClose	_ -> "AOEForServerClose"
  | AOEEvalClosure      -> "AOEEvalClosure"
  | AOEExecute _ 	-> "AOEExecute"
  | AOEASyncExecute _ 	-> "AOEASyncExecute"
  | AOECreateTuple _ 	-> "AOECreateTuple"
  | AOEAccessTuple _ 	-> "AOEAccessTuple"
  | AOEConcatTuples 	-> "AOEConcatTuples"
  | AOEMapFromItem _ 	-> "AOEMapFromItem"
  | AOEMapToItem    	-> "AOEMapToItem"
  | AOEMap           	-> "AOEMap"
  | AOENullMap _ 	-> "AOENullMap"
  | AOEMapIndex _ 	-> "AOEMapIndex"
  | AOEMapIndexStep _ 	-> "AOEMapIndexStep"
  | AOEMapConcat       	-> "AOEMapConcat"
  | AOEOuterMapConcat _ -> "AOEOuterMapConcat"
  | AOEProduct          -> "AOEProduct"
  | AOESelect _ 	-> "AOESelect"
  | AOEJoin _ 	        -> "AOEJoin"
  | AOELeftOuterJoin _ 	-> "AOELeftOuterJoin"
  | AOEGroupBy _ 	-> "AOEGroupBy"
  | AOEOrderBy _ 	-> "AOEOrderBy"
  | AOECopy             -> "AOECopy"
  | AOEDelete           -> "AOEDelete"
  | AOEInsert _ 	-> "AOEInsert"
  | AOERename _         -> "AOERename"
  | AOEReplace _ 	-> "AOEReplace"
  | AOESnap _ 	        -> "AOESnap"
  | AOESet _ 	        -> "AOESet"
  | AOEImperativeSeq  -> "AOEImperativeSeq"
  | AOEParse _ 	        -> "AOEParse"
  | AOETreeJoin _ 	-> "AOETreeJoin"
  | AOETupleTreePattern _-> "AOETupleTreePattern"
  | AOEProject _        -> "AOEProject"
(*
  | AOEPrune _ 	        -> "AOEPrune"
  | AOEDistinct _ 	-> "AOEDistinct"
*)


(* for the path analysis *)
let disjoint_attr_tests a1 a2 = 
  let get_rattr_symbol at = match at with
    | ASchemaAttributeTest r -> r
    | AAttributeTest None -> Namespace_symbols.anyrattr
    | AAttributeTest (Some (r,_)) -> r
  in
  let r1 = get_rattr_symbol a1 in
  let r2 = get_rattr_symbol a2 in
    if r1 = Namespace_symbols.anyrattr || r2 = Namespace_symbols.anyrattr
    then false
    else not(Namespace_symbols.rattr_equal r1 r2)

let disjoint_element_tests a1 a2 = 
  let get_relem_symbol at = match at with
    | ASchemaElementTest r -> r
    | AElementTest None -> Namespace_symbols.anyrelem
    | AElementTest (Some (r,_)) -> r
  in
  let r1 = get_relem_symbol a1 in
  let r2 = get_relem_symbol a2 in
    if r1 = Namespace_symbols.anyrelem || r2 = Namespace_symbols.anyrelem
    then false
    else not(Namespace_symbols.relem_equal r1 r2)

let disjoint_node_tests k1 k2 = match k1,k2 with
  | AAnyKind, _ -> false
  | _, AAnyKind -> false
  | ACommentKind, x -> x != ACommentKind
  | ATextKind, x -> x != ATextKind 
  | APIKind p, x -> (match x with | APIKind s -> p!=s | _ -> true)
  | AAttributeKind a1, AAttributeKind a2 -> disjoint_attr_tests a1 a2
  | AAttributeKind _, _ -> true
  | _, AAttributeKind _ -> true
  | AElementKind a1, AElementKind a2 -> disjoint_element_tests a1 a2
  | AElementKind _, _ -> true
  | _, AElementKind _ -> true
  | ADocumentKind None, ADocumentKind _ -> false
  | ADocumentKind None, _ -> true
  | ADocumentKind _, ADocumentKind None -> false      
  | ADocumentKind (Some e1), ADocumentKind (Some e2) -> disjoint_element_tests e1 e2
  | ADocumentKind (Some _), _ -> true

(*
let rec disjoint_paths p1 p2 = 
  match (p1,p2) with
    | Empty, _ -> true
    | _, Empty -> true
    | Variable v1, Variable v2 -> !we need a path environment here!!
    | Variable _, _ -> false
    | _, Variable _ -> false
    | Step(ax1,t1), Step(ax2,t2) ->
        if (ax2 = Child || ax1 = Descendant || ax1 = Descendant_or_self) &&
          (ax2 = Child || ax2 = Descendant || ax2 = Descendant_or_self) then 
          match t1, t2 with
            | APNameTest s1, APNameTest s2 ->
                not( Namespace_symbols.symbol_equal s1 s2 )
            | APNodeKindTest k1, APNodeKindTest k2 ->
                disjoint_node_tests k1 k2
            | APNameTest _, APNodeKindTest _ -> false (* we don't know *)
            | APNodeKindTest _, APNameTest _ -> false
        else false
    | Step _, _ -> true (* ??? *)
    | _, Step _ -> true (* ??? *)
    | Union (p1, p2), Union(p3, p4) ->
        (disjoint_paths p1 p3) && (disjoint_paths p1 p4) &&
          (disjoint_paths p2 p3) && (disjoint_paths p2 p4) 
    | Union (p1,p2), x -> (disjoint_paths p1 x) && (disjoint_paths p2 x)
    | x, Union (p1,p2) -> (disjoint_paths x p1) && (disjoint_paths x p2)
    | JoinChild (t11, t12), JoinChild (t21, t22) ->
        if (disjoint_paths t11 t21) then true
        else disjoint_paths t12 t22
(*    | JoinChild (t1, t2), x -> disjoint_paths t1 x
    | x, JoinChild (t1, t2) -> disjoint_paths x t1 *)

*)


let get_annotation_if_exists msg algop = 
  match algop.compile_annotations with
    | None ->
        raise (Query
	              (Internal_Error (msg^": Attempt to retrieve annotation from operator"
				                    ^ " but no annotations set")))
    | Some s ->
          s

let symbol_opt_equal sopt1 sopt2 = match (sopt1, sopt2) with
  | None, None -> true
  | None, _ -> false
  | _, None -> false
  | Some s1, Some s2 -> Namespace_symbols.symbol_equal s1 s2  

let aelement_test_equal e1 e2 = match (e1,e2) with
  | ASchemaElementTest _, AElementTest _ -> false
  | AElementTest _, ASchemaElementTest _ -> false
  | ASchemaElementTest r1, ASchemaElementTest r2 -> Namespace_symbols.symbol_equal r1 r2
  | AElementTest None, AElementTest None -> true
  | AElementTest None, AElementTest _ -> false
  | AElementTest _, AElementTest None -> false
  | AElementTest (Some (s1, topt1)), AElementTest (Some (s2, topt2)) ->
      (Namespace_symbols.symbol_equal s1 s2) && symbol_opt_equal topt1 topt2

let aattribute_test_equal e1 e2 = match (e1,e2) with
  | ASchemaAttributeTest _, AAttributeTest _ -> false
  | AAttributeTest _, ASchemaAttributeTest _ -> false
  | ASchemaAttributeTest r1, ASchemaAttributeTest r2 -> Namespace_symbols.symbol_equal r1 r2
  | AAttributeTest None, AAttributeTest None -> true
  | AAttributeTest None, AAttributeTest _ -> false
  | AAttributeTest _, AAttributeTest None -> false
  | AAttributeTest (Some (s1, topt1)), AAttributeTest (Some (s2, topt2)) ->
      (Namespace_symbols.symbol_equal s1 s2) && symbol_opt_equal topt1 topt2
        

let akind_test_equal k1 k2 = match (k1,k2) with
  | ADocumentKind None, ADocumentKind None -> true
  | ADocumentKind None, ADocumentKind _ -> false
  | ADocumentKind _, ADocumentKind None -> false
  | ADocumentKind (Some e1), ADocumentKind (Some e2) -> aelement_test_equal e1 e2
  | _, ADocumentKind _ -> false      
  | ADocumentKind _, _ -> false       
  | AElementKind e1, AElementKind e2 -> aelement_test_equal e1 e2
  | _, AElementKind _ -> false      
  | AElementKind _, _ -> false       
  | AAttributeKind a1, AAttributeKind a2 -> aattribute_test_equal a1 a2
  | _, AAttributeKind _ -> false      
  | AAttributeKind _, _ -> false       
  | APIKind s1, APIKind s2 -> s1 = s2
  | _, APIKind _ -> false      
  | APIKind _, _ -> false       
  | ACommentKind, ACommentKind -> true
  | ACommentKind, _ -> false
  | _, ACommentKind -> false
  | ATextKind, ATextKind -> true
  | _, ATextKind -> false      
  | ATextKind, _ -> false     
  | AAnyKind, AAnyKind -> true
(*   | AAnyKind, _ -> false *)
(*   | _, AAnyKind -> false *)

let anode_test_equal t1 t2 = match (t1,t2) with
  | APNameTest _, APNodeKindTest _ -> false
  | APNodeKindTest _ , APNameTest _ -> false      
  | APNameTest n1, APNameTest n2 ->  Namespace_symbols.symbol_equal n1 n2
  | APNodeKindTest k1, APNodeKindTest k2 -> akind_test_equal k1 k2

let is_access_tuple op =
  match op.palgop_expr_name with
  | AOEAccessTuple _ -> true
  | _ -> false

let is_empty_tuple op =
  match op.palgop_expr_name with
  | AOECreateTuple names ->
      names = [||]
  | _ -> false

let is_any_join op =
  match op.palgop_expr_name with
  | AOELeftOuterJoin _
  | AOEJoin _ -> true
  | _ -> false

let is_outer_join op = 
  match op.palgop_expr_name with
  | AOELeftOuterJoin _ -> true
  | _ -> false

let is_regular_join op =
  match op.palgop_expr_name with
  | AOEJoin _ -> true
  | _ -> false

let is_input_tuple op =
  match op.palgop_expr_name with
  | AOEInputTuple  -> true	
  | _ -> false

let is_a_map_concat op =
  match op.palgop_expr_name with
  | AOEMapConcat -> 
      true
  | _ -> false

let is_any_map_concat op =
  match op.palgop_expr_name with
  | AOEOuterMapConcat _
  | AOEMapConcat -> 
      true
  | _ -> false

let is_an_outer_mapconcat op =
  match op.palgop_expr_name with
  | AOEOuterMapConcat _ -> 
      true
  | _ -> false

let is_a_sep_sequence op = 
  match op.palgop_expr_name with
  | AOEMapIndexStep _ -> true
  | _ -> false

let is_a_map_index op = 
  match op.palgop_expr_name with
  | AOEMapIndex _ -> true
  | _ -> false

let is_a_map op = 
  match op.palgop_expr_name with
  | AOEMapConcat 
      (* Tuple operations *)
  | AOEMapFromItem _
  | AOEMapToItem                        (* Tuple to item iteration *)
  | AOEMap                                 (* Tuple iteration *)
  | AOEMapIndex _ ->  true
  | _ -> 
      false

(* is a dependent map *)
let is_a_dep_map op = 
  match op.palgop_expr_name with
  | AOEMapConcat 
      (* Tuple operations *)
  | AOEMapFromItem _
  | AOEMapToItem                        (* Tuple to item iteration *)
  | AOEMap                                 (* Tuple iteration *)
    ->  true
  | _ -> 
      false

let is_a_non_concat_map op = 
  (is_a_map op) && (not (is_a_map_concat op))

let is_a_map_from_item op =
  match op.palgop_expr_name with
  | AOEMapFromItem _ -> true
  | _ -> false

let is_a_map_to_item op =
  match op.palgop_expr_name with
  | AOEMapToItem -> true
  | _ -> false

let is_a_tuple_input_map op =
  (is_a_dep_map op) && (not (is_a_map_from_item op))

let is_group op = 
  match op.palgop_expr_name with 
  | AOEGroupBy _ -> true
  | _ -> false

let is_select op =
  match op.palgop_expr_name with
  | AOESelect _ -> true
  | _ -> false

let is_project op =
  match op.palgop_expr_name with
  | AOEProject _ -> true
  | _ -> false

let is_product op =
  match op.palgop_expr_name with
  | AOEProduct -> true
  | _ -> false

let is_a_select op =
  match op.palgop_expr_name with
  | AOESelect _ -> true
  | _ -> false

let is_var op = 
  match op.palgop_expr_name with
  | AOEVar _ -> true
  | _ -> false

let is_some op =
  match op.palgop_expr_name with
  | AOESome _ -> true
  | _ -> false

let is_null_map op =
  match op.palgop_expr_name with
  | AOENullMap _ -> true
  | _ -> false


(*************)
(* Functions *)
(*************)

(* get the function name or None if it 
   isn't a function *)
let get_function_name op =
  match op.palgop_expr_name with
  | AOECallBuiltIn ((cfname,_),_,_,_)
  | AOECallOverloaded ((cfname,_), _) 
  | AOECallUserDefined ((cfname,_),_,_,_,_) -> Some cfname
  | _ -> None

let match_name op name =
  match get_function_name op with
  | None -> false
  | Some n ->
      Namespace_names.rqname_equal n name

let is_boolean op        = match_name op Namespace_builtin.fn_boolean
let is_distinct_value op = match_name op Namespace_builtin.fn_distinct_values
let is_fn_data op        = match_name op Namespace_builtin.fn_data

let is_equal op = 
  let overloaded_equality = 
    [Namespace_builtin.op_equal;
     Namespace_builtin.op_equal_left_empty;
     Namespace_builtin.op_equal_right_empty] 
  in
  let builtin_equality = 
    [Namespace_builtin.op_anyURI_equal ;
     Namespace_builtin.op_string_equal ;
     Namespace_builtin.op_boolean_equal;
     Namespace_builtin.op_decimal_equal;
     Namespace_builtin.op_integer_equal;
     Namespace_builtin.op_double_equal;
     Namespace_builtin.op_float_equal;]
  in
  let names = overloaded_equality @ builtin_equality in
  List.exists (match_name op) names

let is_gt op = 
  let overloaded_gt = 
    [Namespace_builtin.op_gt;
     Namespace_builtin.op_gt_left_empty;
     Namespace_builtin.op_gt_right_empty] 
  in
  let builtin_gt = 
    [Namespace_builtin.op_integer_gt;
     Namespace_builtin.op_date_gt;
     Namespace_builtin.op_time_gt;
     Namespace_builtin.op_dateTime_gt;
     Namespace_builtin.op_yearMonthDuration_gt;
     Namespace_builtin.op_dayTimeDuration_gt;
     Namespace_builtin.op_string_gt;
     Namespace_builtin.op_boolean_gt;
     Namespace_builtin.op_decimal_gt;
     Namespace_builtin.op_double_gt;
     Namespace_builtin.op_float_gt;] in
  let names = overloaded_gt @ builtin_gt in
  List.exists (match_name op) names


let is_ge op = 
  let overloaded_ge = 
    [Namespace_builtin.op_ge;
     Namespace_builtin.op_ge_left_empty;
     Namespace_builtin.op_ge_right_empty] 
  in
  let builtin_ge = 
    [Namespace_builtin.op_integer_ge;
     Namespace_builtin.op_date_ge;
     Namespace_builtin.op_time_ge;
     Namespace_builtin.op_dateTime_ge;
     Namespace_builtin.op_yearMonthDuration_ge;
     Namespace_builtin.op_dayTimeDuration_ge;
     Namespace_builtin.op_string_ge;
     Namespace_builtin.op_boolean_ge;
     Namespace_builtin.op_decimal_ge;
     Namespace_builtin.op_double_ge;
     Namespace_builtin.op_float_ge;] in
  let names = overloaded_ge @ builtin_ge in
  List.exists (match_name op) names

let is_lt op = 
  let overloaded_lt = 
    [Namespace_builtin.op_lt;
     Namespace_builtin.op_lt_left_empty;
     Namespace_builtin.op_lt_right_empty] 
  in
  let builtin_lt = 
    [Namespace_builtin.op_integer_lt;
     Namespace_builtin.op_date_lt;
     Namespace_builtin.op_time_lt;
     Namespace_builtin.op_dateTime_lt;
     Namespace_builtin.op_yearMonthDuration_lt;
     Namespace_builtin.op_dayTimeDuration_lt;
     Namespace_builtin.op_string_lt;
     Namespace_builtin.op_boolean_lt;
     Namespace_builtin.op_decimal_lt;
     Namespace_builtin.op_double_lt;
     Namespace_builtin.op_float_lt;] in
  let names = overloaded_lt @ builtin_lt in
  List.exists (match_name op) names

let is_le op = 
  let overloaded_le = 
    [Namespace_builtin.op_le;
     Namespace_builtin.op_le_left_empty;
     Namespace_builtin.op_le_right_empty] 
  in
  let builtin_le = 
    [Namespace_builtin.op_integer_le;
     Namespace_builtin.op_date_le;
     Namespace_builtin.op_time_le;
     Namespace_builtin.op_dateTime_le;
     Namespace_builtin.op_yearMonthDuration_le;
     Namespace_builtin.op_dayTimeDuration_le;
     Namespace_builtin.op_string_le;
     Namespace_builtin.op_boolean_le;
     Namespace_builtin.op_decimal_le;
     Namespace_builtin.op_double_le;
     Namespace_builtin.op_float_le;] in
  let names = overloaded_le @ builtin_le in
  List.exists (match_name op) names

let is_fs_untyped_to_any op =
  match_name op Namespace_builtin.fs_untyped_to_any

let is_tupletreepattern op =
  match op.palgop_expr_name with
  | AOETupleTreePattern _ -> true
  | _ -> false

let get_outer_map_name op =
  match op.palgop_expr_name with 
  | AOEOuterMapConcat vn ->
      vn
  | _ -> raise (Query (Malformed_Algebra_Expr ("Attempted to get name of op we thought was an outer map but is not")))

let get_map_from_item_name op =
  match op.palgop_expr_name with 
  | AOEMapFromItem vn -> vn
  | _ -> raise (Query (Malformed_Algebra_Expr ("Attempted to get name of op we thought was a mapfromitem but is not")))

let get_null_map_name op =
  match op.palgop_expr_name with
  | AOENullMap v -> v
  | _ -> raise (Query (Malformed_Algebra_Expr ("Attempted to get name of op we thought was a null map but is not")))

let get_sep_sequence_name op =
  match op.palgop_expr_name with
  | AOEMapIndexStep v -> v
  | _ -> raise (Query (Malformed_Algebra_Expr ("Attempted to get name of op we thought was a sep sequence but is not")))

let get_outer_join_name op =
  match op.palgop_expr_name with
  | AOELeftOuterJoin (vn,pd) -> vn
  | _ -> raise (Query (Malformed_Algebra_Expr ("Attempted to get name of op we thought was an outer join but is not")))

  
(* 
   Returns all functions called within a plan.  
   Should these be static annotations on the plan?
*)
let rec calls_functions bltin algop =
  let apply_subexprs algop_sub = 
    match algop_sub with
    | NoSub -> []
    | OneSub algop -> calls_functions bltin algop
    | TwoSub (algop1, algop2) -> (calls_functions bltin algop1) @ (calls_functions bltin algop2)
    | ManySub algop_array -> List.concat(List.map (calls_functions bltin) (Array.to_list algop_array))
  in
  (match algop.palgop_expr_name with
  | AOECallUserDefined ((name, arity), _, _, _,_) -> 
      let (prefix, uri, _) = name in
      (Debug.print_default_debug ("CallUser "^(Namespace_names.prefixed_string_of_rqname name)^"\n"); 
       if (not bltin && not (prefix = Namespace_builtin.glx_prefix)) then [name] else [])
  | AOECallBuiltIn ((name, _), _, _, _) 
  | AOECallOverloaded ((name, _), _) -> 
      (Debug.print_default_debug ("CallBuilt/Over "^(Namespace_names.prefixed_string_of_rqname name)^"\n"); 
       if bltin then [name] else [])
  | _ -> [])
  @ (apply_subexprs algop.psub_expression)
  @ (apply_subexprs algop.pdep_sub_expression)

let calls_builtin_functions algop = calls_functions true algop
let calls_user_defined_functions algop = calls_functions false algop
