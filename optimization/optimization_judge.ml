(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_judge.ml,v 1.46 2007/11/07 22:00:15 mff Exp $ *)

(* Module: Optimization_judge
   Description:
     This module contains judgments used during algebraic
     optimization.
 *)
open Error
open Namespace_builtin

open Xquery_common_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_algebra_ast_annotation_util

(*
open Optimization_util
*)

open Compile_context
open Norm_context
open Processing_context

open Alg_path_structutil

(****************)
(* Independence *)
(****************)

(* NOTE: Those judgments are used to decide whether turning a
   MapConcat into a Product is possible. *)

let debug_returned_fields fields =
  if Debug.join_debug()
  then
    begin
      Debug.print_join_debug "\t---> Independent branch returned fields:";
      List.iter (fun x -> Debug.print_join_debug ("\t\t" ^ Namespace_names.prefixed_string_of_rqname x)) fields;
    end

let debug_accessed_fields fields =
  if Debug.join_debug()
  then
    begin
      Debug.print_join_debug "\t---> Dependent branch accessed fields:";
      List.iter (fun x -> Debug.print_join_debug ("\t\t" ^ Namespace_names.prefixed_string_of_rqname x)) fields;
    end

(* t1 is indep of t2 if t1 accesses 
   no fields returned by t2.
   returned(t2) intersect accessed(t1) = empty_set
   ---
   (returned (t2) - returned (t2.indep)) intersect accessed (t1)
*)
let subexpr_get_returned se =
  List.concat 
    (match se with
    | NoSub -> []
    | OneSub s0 ->
	(algop_get_returned_fields s0) :: []
    | TwoSub (s0,s1) ->
	(algop_get_returned_fields s0) :: 
	(algop_get_returned_fields s1) :: []
    | ManySub sa ->
	List.map algop_get_returned_fields (Array.to_list sa))

(* Dep independant of Indep(Op1,...Opn)
     ==
   Accessed(Dep) /\ (Returned(Indep) - (Returned(Op1...Opn)) == 0
*)

let map_tuple_independent t1 t2 =
  let af = algop_get_accessed_fields t1 in
  let rf = algop_get_returned_fields t2 in
  let t2_indep = subexpr_get_returned t2.psub_expression in 
  let t2_intro = Gmisc.difference_list rf t2_indep in      
  let int_map = Gmisc.intersect_list af t2_intro in
  let result  = [] = int_map in
  result

(* Dep independant of Indep
     ==
   Accessed(Dep) /\ Returned(Indep) == 0
*)

let natural_tuple_independent t1 t2 =
  let af = algop_get_accessed_fields t1 in
  let rf = algop_get_returned_fields t2 in
  let int_map = Gmisc.intersect_list af rf in
  let result = [] = int_map in
  begin
    debug_accessed_fields af;
    debug_returned_fields rf;
    result
  end


(****************)
(* Side effects *)
(****************)

(* NOTE: Those judgments are used to decide whether some rewritings
   are safe wrt. to side-effects. *)

(* Atomic test *)
let rec op_contains_side_effects ((visited_hash, comp_ctxt)) op = 
  match op.palgop_expr_name with      
    | AOEDelete
    | AOEInsert _ 
    | AOERename _
    | AOEReplace _ -> true
	  (* We know that overloaded functions are side-effect free,
	     also because of phase ordering (they are listed in
	     normalization not factorization) we do not include them in
	     the table. This is hackish but, works for now - Chris *)
    | AOECallOverloaded (((prefix,uri,local), _), _) -> false
    | AOECallBuiltIn    ((_,_),_,_,upd) ->
        upd = Updating
    | AOECallUserDefined  (_, _,_,upd,_) -> 
        upd = Updating
      (* Raising an error has a side effect!! *)
    | AOEError -> true
      (* Constructors do have some form of side-effect!! *)
    | AOEDocument 
    | AOEPI _ 
    | AOEPIComputed 
    | AOEComment _ 
    | AOECommentComputed 
    | AOEText _ 
    | AOETextComputed 
    | AOEAttr _ 
    | AOEAnyAttr _ 
    | AOEElem _ 
    | AOEAnyElem _ -> true
    | _ -> false

(* This judgment determines if the result has side-effects, we need to
   process the user-defined functions to mark them as well. This has a
   placeholder for the envirnonment for now. *)

(* has_side_effect_internal,
   has_non_trivial_snap_subexpr, 
   subexpr_has_side_effect,
   contains_update should all be defined in terms of:
   should use Optimization_walker.fold_over_algop
*)
and has_side_effect_internal ((visited_hash, comp_ctxt) as env) algop = 
  let has_side_effect_subexpr env sexpr = 
    match sexpr with 
    | NoSub -> false
    | OneSub op -> has_side_effect_internal env op
    | TwoSub (op1,op2) ->
	(has_side_effect_internal env op1) ||
	(has_side_effect_internal env op2) 
      | ManySub ops ->
	  Array.fold_left 
	    (fun b op -> b || has_side_effect_internal env op) false ops
  in
  let is_update = op_contains_side_effects env algop in
  is_update || 
  (has_side_effect_subexpr env algop.psub_expression) ||
  (has_side_effect_subexpr env algop.pdep_sub_expression) 

let has_side_effect comp_ctxt algop =
  let visited_hash = Hashtbl.create 1 in 
  has_side_effect_internal (visited_hash, comp_ctxt) algop

let side_effect_free comp_ctxt algop = not(has_side_effect comp_ctxt algop)

(* Just walk for a snap, then see if there are side-effects inside *)
(* NOTE: This is not *quite* right because we regard trace as a
   side-effect function..  Perhaps we should pass in the env of
   functions with side-effects.  *)
let rec has_non_trivial_snap comp_ctxt algop =
  match algop.palgop_expr_name with
  | AOESnap sm ->
      has_side_effect comp_ctxt algop
  | _ ->
      (has_non_trivial_snap_subexpr comp_ctxt algop.psub_expression) ||
      (has_non_trivial_snap_subexpr comp_ctxt algop.pdep_sub_expression)
and has_non_trivial_snap_subexpr comp_ctxt sexpr =
  match sexpr with
  | NoSub -> false
  | OneSub op -> has_non_trivial_snap comp_ctxt op
  | TwoSub (op1,op2) ->
      ((has_non_trivial_snap comp_ctxt op1) ||
      (has_non_trivial_snap comp_ctxt op2))
  | ManySub ops ->
      Array.fold_left 
	(fun b op -> b || has_non_trivial_snap comp_ctxt op) false ops

let has_trivial_snap comp_ctxt algop =
  not(has_non_trivial_snap comp_ctxt algop)

let is_update_op op =
  match op.palgop_expr_name with
    (*| AOEGroupBy _ | AOEOrderBy _ *) (* why?? -- Philippe *)
  | AOECopy  | AOEDelete 
  | AOEInsert _
  | AOERename _ | AOEReplace _ -> true
  | AOECallUserDefined(_,_,_,Updating,_) -> true
  | AOECallBuiltIn(_,_,_,Updating) -> true
  | _ -> false

let rec contains_update op =
  if is_update_op op then true
  else
    let fold_fun b x = b || contains_update x in
    let dep =
      match op.pdep_sub_expression with
      | NoSub -> false
      | OneSub op' -> contains_update op'
      | TwoSub (op1,op2) -> (contains_update op1 || contains_update op2)
      | ManySub ops -> Array.fold_left fold_fun false ops
    in
    let indep =
      match op.psub_expression with
      | NoSub -> false
      | OneSub op' ->  contains_update op'
      | TwoSub (op1,op2) -> (contains_update op1 || contains_update op2)
      | ManySub ops -> Array.fold_left fold_fun false ops
    in
    (indep || dep)

let subexpr_has_side_effect comp_ctxt sexpr =
  match sexpr with
  | NoSub -> false
  | OneSub op -> has_side_effect comp_ctxt op
  | TwoSub (op1,op2) -> 
      ((has_side_effect comp_ctxt op1) ||
      (has_side_effect comp_ctxt op2))
  | ManySub ops ->
      Array.fold_left 
	(fun b o -> b || (has_side_effect comp_ctxt o))
	false ops

(* This tests if the op
 (i)  has a dependent side-effect op 
 (ii) itself is a side-effect op

  This is used when deciding to push down a selection.
*)
let has_dependent_side_effect comp_ctxt op =
  (* cond (i) *)
  let dep_cond = subexpr_has_side_effect comp_ctxt op.pdep_sub_expression in
  (* cond (ii) *)
  let side_effect_cond = has_side_effect comp_ctxt  op in 
  (dep_cond || side_effect_cond)


(* conservative test that two expressions commute, modulo the order of the values in the result *)
let commute_logical comp_ctxt op1 op2 = 
  let norm_ctxt = norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = (processing_context_from_norm_context norm_ctxt) in
  if Conf.is_xquery() then
    true (* expressions without side effects always commute, modulo order of values in the result *)
  else
    if not (proc_ctxt.infer_independence) then
      false
    else
      let (_,_,access1,modif1) = Alg_path_structutil.paths_from_path_annotation "Optimization_judge.commute_logical" op1.annotation in
      let (_,_,access2,modif2) = Alg_path_structutil.paths_from_path_annotation "Optimization_judge.commute_logical" op2.annotation in
      (path_sequences_with_disjoint_roots modif1 access2) && (path_sequences_with_disjoint_roots modif2 access1) &&
      (path_sequences_with_disjoint_roots modif1 modif2)


let commute_logical_with_array comp_ctxt op1 op_array = 
  Array.fold_left
    (fun b op -> b && commute_logical comp_ctxt op1 op) 
    true op_array
    

(**********)
(* Typing *)
(**********)

(* NOTE: Those judgments are used as a poor-man's form of static
   typing for the algebra, to decide simple cardinality properties. *)

let rec is_singleton_tuple comp_ctxt algop = 
  match algop.palgop_expr_name with
    (* Input tuple *)
  | AOEInputTuple -> true

  (**************************)
  (* Basic Tuple operations *)
  (**************************)
  | AOECreateTuple _ -> true
  | AOEAccessTuple _ -> false (* Returns items *)
  | AOEConcatTuples  -> true                    

  (**************)
  (* Tuple Maps *)
  (**************)
(* | AOEMapFromItem   (* Need a singleton judgment about the item. which we don't have *) *)
  | AOEMap                                (* Tuple iteration *)
  | AOEMapConcat                          (* Map with built-in tuple concatenation *)
  | AOEOuterMapConcat _ ->                (* Same with null value on the given name *)
      (* For these maps need indep and dep are singleton *)
      let indep = access_onesub algop.psub_expression in 
      let dep   = access_onesub algop.pdep_sub_expression in 
      (is_singleton_tuple comp_ctxt indep) && 
      (is_singleton_tuple comp_ctxt dep)

  | AOESelect _ -> false
	
  (* Singleton Maps *)
  | AOENullMap _ 
  | AOEMapIndex _ 
  | AOEMapIndexStep _ -> 
      let indep = access_onesub algop.psub_expression in 
      is_singleton_tuple comp_ctxt indep

  (* Map Concats *)

  (******************************)
  (* Products, Joins Selections *)
  (******************************)
  | AOEProduct           
  | AOEJoin   _          
  | AOELeftOuterJoin _ ->
      let i1, i2 = access_twosub algop.psub_expression in 
      (is_singleton_tuple comp_ctxt i1) && (is_singleton_tuple comp_ctxt i2)

  (************************************)
  (* Grouping and Ordering operations *)
  (************************************)
  | AOEGroupBy _ -> (* If indep is singleton *)
      let indep = access_onesub algop.psub_expression in 
      is_singleton_tuple comp_ctxt indep
  | AOEOrderBy _  -> (* if indep is *)
      let indep = access_onesub algop.psub_expression in 
      is_singleton_tuple comp_ctxt indep
  | AOEProject p ->
      let indep = access_onesub algop.psub_expression in 
      is_singleton_tuple comp_ctxt indep
  | _ -> false


(******************)
(* Document order *)
(******************)

(* NOTE: Those judgments are used as a poor-man's form of DDO
   analysis, to decide whether document order operations can be
   removed. *)

type distinct_order_matters =
  | Manipulating_Ord
  | Mainpulating_Distinct
  | Manipulating_Ord_Distinct
  | Observing
  | Neutral

(* FUNCTIONS *)
(* o All user defined functions are considered to be Observing              *)
(* o All functions not mentioned below that are in FN FS or GLX namespaces   *)
(*   are considered to be neutral                                            *)
(* o Below should be all the functions that take node sequences              *)
(*   i.e., node()+ or node()*                                                *)
(*   as an argument, because those are only ones that can observe/manipulate *)
(*   order and duplicates.                                                   *)
let distinct_order_handle_fun fname =
  if 
    fname = fn_reverse || fname = fn_unordered || fname = fs_docorder
  then Manipulating_Ord
  else if 
    fname = fs_distinct
  then Mainpulating_Distinct
  else if (fname = fn_boolean 
	 || fname = fn_empty 
	 || fname = fn_distinct_values
	 || fname = fn_zero_or_one
	 || fname = fn_exactly_one
	 || fname = fn_one_or_more
	 || fname = fn_deep_equal
	 || fname = fs_distinct_docorder_or_atomic_sequence
	 || fname = fs_distinct_docorder
	 || fname = glx_deep_distinct)
  then Manipulating_Ord_Distinct
  else if (fname = fn_data
	 || fname = fn_index_of
	 || fname = fn_subsequence
	 || fname = fs_subsequence
	 || fname = fn_count
	 || fname = fn_position
	 || fname = fn_last
	 || fname = fs_first
	 || fname = fs_last_fn
	 || fname = glx_get_order
	 || fname = fs_convert_simple_operand)
  (* to cover all of min/max/sum/avg, we consider the function
     fs:convert-simple-operand() and fn:data() above to be observing
     order/dupl. -- Philippe *)
  then Observing
  else if (fname = fn_trace
	 || fname = fn_insert_before
	 || fname = fn_remove
	 || fname = op_union
	 || fname = op_intersect
	 || fname = op_except
	 || fname = glx_get_docid)
  then Neutral
  (* NOTE: we could play safe here and consider everything else to
  observing -- Philippe *)
  else Neutral

(* OPERATORS *)
let distinct_order_handle op =
  match op with
  (* Depends on function *) 
  | AOECallBuiltIn ((name,arity), optintypes, opttypes,_) ->
      distinct_order_handle_fun name
  (* Manipulating order *)
  | AOEGroupBy _ | AOEOrderBy _ 
    -> Manipulating_Ord
  (* Manipulating duplicates *)
(*  | AOEDistinct _ |AOEPrune _ 
    -> Mainpulating_Distinct
*)
  (* Manipulating order and duplicates *)
  | AOETreeJoin _ | AOETupleTreePattern _ 
    -> Manipulating_Ord_Distinct
  (* observing order/duplicates *)
  | AOETypeswitch _ | AOEDocument | AOEPI _ | AOEPIComputed 
  | AOEComment _ | AOECommentComputed | AOEText _ | AOECharRef _ | AOETextComputed
  | AOEElem _ | AOEAnyElem _ | AOEAttr _ | AOEAnyAttr _ | AOEValidate _
  | AOECast _ |  AOECastable _ | AOEConvertSimple _ | AOEPromoteNumeric _ | AOEPromoteAnyString
  | AOEUnsafePromoteNumeric _ | AOEMapIndex _ | AOEMapIndexStep _ 
  | AOECopy | AOEDelete | AOEInsert _ | AOERename _
  | AOEReplace _ | AOESnap _ 
  | AOESome _  (* Type subexpr - O is safe *)
  | AOEEvery _ (* Type subexpr - O is safe *)
  | AOECallUserDefined _ 
  | AOECallOverloaded _ 
    -> Observing
  (* neutral *)
  | AOEIf | AOEWhile | AOELetvar _ | AOEVar _ | AOEScalar _ | AOESeq | AOEEmpty 
  | AOEError | AOETreat _ | AOEInputTuple 
  | AOEServerImplements _ | AOEExecute _ | AOEASyncExecute _ | AOEForServerClose _ | AOEEvalClosure
  | AOECreateTuple _ | AOEAccessTuple _ | AOEConcatTuples
  | AOEMapFromItem _ | AOEMapToItem  | AOEMap 
  | AOENullMap _ | AOEMapConcat | AOEOuterMapConcat _ 
  | AOEProduct | AOESelect _ 
  | AOEJoin _ | AOELeftOuterJoin _  |  AOEParse _  | AOEProject _
  | AOESet _ | AOEImperativeSeq 
    -> Neutral

let rec get_anc_op_list op_node op =
  if op_node == op then
    [], true
  else
    let dep_subexprs = subexpr_to_list op_node.pdep_sub_expression in
    let indep_subexprs = subexpr_to_list op_node.psub_expression in
    let subexprs = Array.of_list (dep_subexprs @ indep_subexprs) in
    let list = ref [] in 
    let found = ref false in
    let pos = ref 0 in
    while not(!found) && !pos < (Array.length subexprs) do
      let op_node' = subexprs.(!pos) in
      let (list', found') = get_anc_op_list op_node' op in
      list := list'; found := found'; incr(pos)
    done;
    if !found then
      op_node::!list , true
    else
      [], false

(* All of this can be done with annotations on the syntax tree
 * but, I'll wait until the dust settles on the reorganising
 * everything before switching to that approach -- Philippe
 *)
let ord_dup_matters root op =
  let not_is_order_dupfree_op (o,d) op =
    match distinct_order_handle op.palgop_expr_name with
    | Manipulating_Ord -> (false, d)
    | Mainpulating_Distinct -> (d, false)
    | Manipulating_Ord_Distinct -> (false, false)
    | Observing -> (true, true)
    | Neutral -> (o,d)
  in
  let ancs, _ = get_anc_op_list root op in
  let o,d =  List.fold_left not_is_order_dupfree_op (true, true) ancs in
  o || d


(********************)
(* HACK Name checks *)
(********************)

(* NOTE: Yeck. - Jerome. May 31, 2006 *)

(*** HACK CONDITION
 * Several of the optimization rules use the singleton
 * judgement. Until it is actually available, we will use
 * the name of the field to determine the cardinality
 **)

(*** HACK CONDITION
 * Is this a glx:sequence field?
 *)
let is_seq_field f =
  let (prefix, uri, name) = f in
  let len = String.index name '_' in
  let substr = String.sub name 0 len in
  (uri = Namespace_builtin.glx_uri) && (substr = "sequence")


