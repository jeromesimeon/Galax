(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cs_code_typing_top.ml,v 1.47 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Cs_code_typing_top.
   Description:
     This module implements the physical typing and physical-operator
     assignment phase.
*)

open Code_selection_context
open Code_typing_context

open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_physical_type_ast
open Xquery_physical_type_ast_util
open Xquery_physical_algebra_ast

open Error

(*
Architecture
------------

1. We are decoupling the code selection into two phases:

 Phase1: physical operator assignment. This decide which physical
 operator to use and does the corresponding physical typing.

 Phase2: actual code selection, which:
    (1) allocate variable and tuple resources on the stack frame
    (2) builds the code for each physical operator

2. Code selection should remain (mostly) unchanged, except that
it will be cleaned up of physical algorithm decision, and of the
corresponding annotation / signature computation.

3. There is a new phase, which does physical assignment and type
checking. Here is how the recursion will proceed for type checking and
physical assignment:

  For Op{D1,...Dn}(I1,...Ik) in environment PEnv

   (1) Typecheck I1 : T1 ... Ik : Tk in environment PEnv
   (2) For Op, and T1....Tk pick up the corresponding physical
       operator POp.
   (3) From POp, compute the type T_INPUT of the Input (item or tuple)
   (4) Extend environment to PEnv' with the type T_INPUT
   (5) Typecheck D1 : T1' ... Dn : Tn' in PEnv'
   (6) Apply type checking for possible additional constraints on the
       (type of POp), and compute the ouput physical type T_OUT). This
       (yields the physical type signature) P_Sig
   (7) Annotate the AST with the POp and P_Sig.


    PEnv |- I1 : POp1 ; Sig1                (1) above
             ...
    PEnv |- Ik : POp1 ; Sigk
    T_OUTPUT = EXTRACT(Sig1,...Sigk)
    Assign(Op,T_OUTPUT) = POp,T_INPUT       (2) & (3) above
    PEnv' = PEnv + INPUT : T_INPUT          (4) above
    PEnv' |- D1 : POp1' ; Sig1'             (5) above
             ...
    PEnv' |- Dn : POpn' ; Sign'
    T_OUTPUT' = EXTRACT(Sig1',...Sign')

    Sig = TypeCheck(POp,T_OUTPUT,T_OUTPUT')
   ----------------------------------------------
    PEnv |- Op{D1,...Dn}(I1,...Ik) : POp ; Sig
*)

let lookup_field_type field tuple_type = 
  try
    List.assoc field tuple_type
  with 
  | Not_found -> 
      raise(Query(Physical_Type_Error(
		  "Tuple field "^(Namespace_names.prefixed_string_of_rqname field)^" not found in physical type "^(Print_xquery_physical_type.string_of_physical_tuple_type tuple_type))))

let extract_output_signature algop = 
  match algop.palgop_expr_eval_sig with 
  | None -> 
      raise (Query(Malformed_Algebra_Expr("Physical type signature missing for algebraic operator: "^
					  (Print_xquery_algebra.bprintf_physical_algstatement "" algop))))
  | Some (_, _, output_physical_type) -> output_physical_type

(* For an arbitrary algebraic operator, compute the physical type of its input *)
let compute_input_sig subexprs = 
  match subexprs with 
  | NoSub -> NoInput
  | OneSub subexpr -> OneInput (extract_output_signature subexpr) 
  | TwoSub (subexpr1, subexpr2) -> TwoInput (extract_output_signature subexpr1, extract_output_signature subexpr2) 
  | ManySub subexpr_array -> ManyInput (Array.map extract_output_signature subexpr_array)

(* 
   Select physical operator for logical operator.
   Most operators have one physical implementation. 
*)
let rec select_physical_op ctc code_ctxt algop_expr input_types =
  let code_ctxt = store_annotation code_ctxt algop_expr.compile_annotations in
    match algop_expr.palgop_expr_name with
	
      (*************************)
      (* Functional operations *)
      (*************************)
    | AOEIf ->  POIf
    | AOEWhile ->  POWhile
    | AOELetvar (odt, vn) ->
	POLetvar (Code_binding.select_physical_variable_binding code_ctxt input_types (odt, vn))
    | AOETypeswitch pat_vn_array -> POTypeswitch (Array.map snd pat_vn_array)
    | AOEVar (vn) -> POVar vn 
    | AOECallBuiltIn ((cfname,arity), optintypes, outtype, _) ->
	Code_builtin_fn.select_physical_op code_ctxt input_types algop_expr ((cfname,arity), optintypes, outtype)
    | AOECallUserDefined((fname, arity), optintypes, outtype, _, _) -> POCallUserDefined
    | AOECallOverloaded((cfname,arity),table) -> POCallOverloaded
    | AOEConvertSimple atomic_type -> POConvertSimple
    | AOEPromoteNumeric atomic_type -> POPromoteNumeric
    | AOEPromoteAnyString -> POPromoteAnyString
    | AOEUnsafePromoteNumeric atomic_type -> POUnsafePromoteNumeric

	  (*****************)
	  (* Constructors  *)
	  (*****************)
    | AOEScalar dmv -> POScalar
    | AOEEmpty ->  POEmpty
	  
    | AOESeq
    | AOEImperativeSeq
    | AOEDocument
    | AOEPI _
    | AOEPIComputed
    | AOEComment _
    | AOECommentComputed
    | AOEText _
    | AOECharRef _
    | AOETextComputed
    | AOEElem _
    | AOEAnyElem _
    | AOEAttr _
    | AOEAnyAttr _ ->
	(* Now distinguishing between streamed and materialized versions
	   of constructors although these do not exist as alternative
	   implementations. This 'virtual' distinction is needed
	   for proper propagation of 'non-streamability' in case
	   data flow starts with a constructor. - Michael *)
	Code_constructors.select_physical_op code_ctxt input_types algop_expr
	  
    | AOEError -> POError
	  
	  (******************)
	  (* Type operators *)
	  (******************)
    | AOETreat cmodel1 -> POTreat
    | AOEValidate vmode -> POValidate
    | AOECast (nsenv, cmodel1) -> POCast
    | AOECastable (nsenv, cmodel1) -> POCastable
	  
	  (************************) 
	  (* Item/Tuple operators *)
	  (************************)
    | AOESome (odt, vn) ->
	POSome (Code_binding.select_physical_variable_binding code_ctxt input_types (odt, vn))
	  
    | AOEEvery (odt, vn) ->
	POEvery (Code_binding.select_physical_variable_binding code_ctxt input_types (odt, vn))
	  
    | AOEMapToItem -> POMapToItem
    | AOEMapFromItem vn ->
	POMapFromItem (Code_binding.select_physical_variable_binding code_ctxt input_types (None, vn))
	  
	  (*******************)
	  (* Tuple operators *)
	  (*******************)
    | AOEInputTuple -> POInputTuple
    | AOECreateTuple names ->
	POCreateTuple (Code_binding.select_physical_tuple_binding code_ctxt input_types names)
	  
    | AOEAccessTuple crname1 -> POAccessTuple crname1
    | AOEConcatTuples -> POConcatTuples
    | AOEProduct -> POProduct 
    | AOESelect pred -> POSelect
    | AOEProject fields -> POProject fields
	  
	  (**********)
	  (* Joins  *)
	  (**********)
    | AOEJoin pred_desc -> 
	Code_util_join.select_physical_op Code_util_join.StandardJoin algop_expr 
	  
    | AOELeftOuterJoin (null_name,pred_desc) -> 
	Code_util_join.select_physical_op (Code_util_join.OuterJoin null_name) algop_expr 
	  
	  (*****************)
	  (* Map operators *)
	  (*****************)
    | AOEMapConcat -> POMapConcat 
    | AOEOuterMapConcat vn -> POOuterMapConcat vn
    | AOEMap -> POMap
    | AOEMapIndex vname -> POMapIndex  vname
    | AOEMapIndexStep vname -> POMapIndexStep vname
    | AOENullMap v -> PONullMap v
	  
	  (*********************)
	  (* Update operations *)
	  (*********************)
	  
    | AOECopy -> POCopy 
    | AOEDelete -> PODelete
    | AOEInsert insert_location -> POInsert
    | AOERename _ -> PORename
    | AOEReplace value_flag -> POReplace
    | AOESnap sm -> POSnap
    | AOESet (vn) -> POSet vn 
    | AOEParse uri -> 
	Code_parse.select_physical_op code_ctxt algop_expr uri
	  
    | AOETreeJoin (axis, anode_test) ->
	(* Now passing on the input signature to treejoin code selection. - Michael *)
	Code_treejoin.select_physical_op code_ctxt input_types algop_expr (axis, anode_test) 
	  
    | AOETupleTreePattern (input, pattern) ->
	Code_tuple_tree_pattern.select_physical_op code_ctxt algop_expr (input, pattern)
	  
	  (* Group/order *)
	  (* For group operators, we just need the names of the new aggregate fields *)
    | AOEGroupBy gd_list -> POGroupBy (List.map Xquery_algebra_ast_util.get_aggregate_name gd_list)
    | AOEOrderBy (stablekind, sort_spec_list, osig) -> POOrderBy
	  
	  (*********)
	  (* Other *)
	  (*********)
    (* DXQ *)
    | AOEServerImplements (ncname, uri) ->
	begin
	  let _ = access_onesub algop_expr.psub_expression in 
	  let dep_expr = access_onesub algop_expr.pdep_sub_expression in 
	  let _ = code_typing_expr code_ctxt ctc dep_expr in
	  match (dep_expr.palgop_expr_eval_sig) with
	  | Some (_, _, output_type) ->
	      begin
		match output_type with 
		| PT_XML _ -> POServerImplementsTree
		| PT_Table _ -> POServerImplementsTuple
	      end
	  | None -> raise(Query(Internal_Error("AOEServerImplements input does not have physical type")))
	end
    (* for server P close E always returns a tree... *)
    | AOEForServerClose (ncname, uri) -> 
Debug.print_dxq_debug ("cs_code_typing_top : Before for-server access_onesub\n");
	let indep_expr = access_onesub algop_expr.psub_expression in 
Debug.print_dxq_debug ("cs_code_typing_top : After for-server access_onesub\n");
	let _ = code_typing_expr code_ctxt ctc indep_expr in
	POForServerCloseTree
    (* eval closure E *)
    | AOEEvalClosure ->
Debug.print_dxq_debug ("cs_code_typing_top : Before eval-box access_onesub\n");
Debug.print_dxq_debug ((Print_xquery_algebra.bprintf_logical_algstatement "" algop_expr)^"\n");
	let indep_expr = access_onesub algop_expr.psub_expression in 
Debug.print_dxq_debug ("cs_code_typing_top : After eval-box access_onesub\n");
	let _ = code_typing_expr code_ctxt ctc indep_expr in
	(* We have no bloody idea what type the evaluation of a closure returns, so we assume a tree *)
	POEvalClosureTree
    | AOEExecute (ncname, uri) 
    | AOEASyncExecute (ncname, uri)  -> (* Logic is the same for async execute, even though we throw the value away *)
	begin
	  (* Physical typing phase. 
             1. Examine the physical type of the second argument to AOEExecute 
	   *)
	  let (indep_expr1, indep_expr2) = access_twosub algop_expr.psub_expression in 
	  match (indep_expr2.palgop_expr_eval_sig) with
	  | Some (_, _, output_type) ->
	      begin
		match output_type with 
		| PT_XML _ -> POExecuteTree
		| PT_Table _ -> POExecuteTuple
	      end
	  | None -> raise(Query(Internal_Error("AOEExecute's input does not have physical type")))
	end
(* 
   Type check operator and compute output type.
 *)
and type_check_physical_op code_ctxt ctc pop indep_expr_types dep_exprs =
  (* 3. From POp, compute the type T_INPUT of the Input (item or tuple) *)
  (* 4. Extend environment to PEnv' with the type T_INPUT *)
  (* 5. Typecheck D1 : T1' ... Dn : Tn' in PEnv' *)
  try 
    let output_type = 
      match pop with

	(* Function Calls 
	   
	   CTC |- Indep_1 : PT_1 , PT_1 <: XML
	   ...
	   CTC |- Indep_n : PT_n , PT_n <: XML
	   ---------------------------------------------------
	   CTC |- FuncCallOp{}(Indep_1,...,Indep_n) : DOM LIST
	*)
	| POCallBuiltIn 
	| POCallUserDefined 
	| POCallOverloaded ->
	    let _ = access_nosub dep_exprs in 
	    let _ = access_many_non_discarded_xml_types indep_expr_types in
	      dom_list_xml_type
		
	(* Special treatment of streamed fn:count function *)
	| POCallBuiltIn_Fn_Count_Stream ->
	    let _ = access_nosub dep_exprs in
	      
	    (* Should assert sax stream input here! - Michael *)
	    let _ = access_many_non_discarded_xml_types indep_expr_types in
	      dom_list_xml_type

	(* Special treatment of streamed fs:first-item function *)
	| POCallBuiltIn_Fs_First_Stream ->
	    let _ = access_nosub dep_exprs in
	      
	    (* Should assert sax stream input here! - Michael *)
	    let _ = access_many_non_discarded_xml_types indep_expr_types in
	      sax_stream_xml_type
		
	(* Special treatment of streamed fs:item-to-node() function *)

	| POCallBuiltIn_Fs_Item2Node_Stream ->
	    let _ = access_nosub dep_exprs in
	      
	    (* Should assert sax stream input here! - Michael *)
	    let _ = access_many_non_discarded_xml_types indep_expr_types in
	      sax_stream_xml_type

	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  -----------------------------------------------
	  CTC |- SpecialFunCallOp{}(Indep_1) : DOM CURSOR
	*)
	| POConvertSimple 
	| POPromoteNumeric 
	| POPromoteAnyString 
	| POUnsafePromoteNumeric -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      dom_cursor_xml_type
		
	(* Var
	   ----------------------------
	   CTC |- VarOp[v]{}() : CTC[v]
	*)
	| POVar vn -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_no_type indep_expr_types in
	      (* Must assert non-discarded XML type at binding time. *)
	      (* Forces materialization for global variables -- we need to do better, but this is broken right now *)
	      dom_list_xml_type (* PT_XML (get_variable_type ctc vn) *)

	(* Constructors 
	   
	   CTC |- Indep_1 : PT_1, PT_1 <: XML
	   -------------------------------------------------
	   CTC |- DocumentOp{}(Indep1) : SAX STREAM/DOM LIST
	*)
	| PODocument_Stream -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      sax_stream_xml_type
		
	| PODocument_Materialized -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      dom_list_xml_type
		
	(*
	  ----------------------------------------------------
	  CTC |- PIOp[ncname,target]{}() : SAX STREAM/DOM LIST
	*)
	| POPI_Stream -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_no_type indep_expr_types in
	      sax_stream_xml_type
		
	| POPI_Materialized -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_no_type indep_expr_types in
	      dom_list_xml_type
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  CTC |- Indep_2 : PT_2, PT_2 <: XML
	  -----------------------------------------------------------
	  CTC |- PIComputedOp{}(Indep1, Indep2) : SAX STREAM/DOM LIST
	*)
	| POPIComputed_Stream -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_two_non_discarded_xml_types indep_expr_types in
	      sax_stream_xml_type
		
	| POPIComputed_Materialized -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_two_non_discarded_xml_types indep_expr_types in
	      dom_list_xml_type
		
	(*
	  -----------------------------------------------
	  CTC |- CommentOp[str]{}() : SAX STREAM/DOM LIST
	*)
	| POComment_Stream -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_no_type indep_expr_types in
	      sax_stream_xml_type
		
	| POComment_Materialized -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_no_type indep_expr_types in
	      dom_list_xml_type
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  --------------------------------------------------------
	  CTC |- CommentComputedOp{}(Indep1) : SAX STREAM/DOM LIST
	*)
	| POCommentComputed_Stream -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      sax_stream_xml_type
		
	| POCommentComputed_Materialized -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      dom_list_xml_type

	(*
	  --------------------------------------------
	  CTC |- TextOp[str]{}() : SAX STREAM/DOM LIST
	*)
	| POText_Stream -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_no_type indep_expr_types in
	      sax_stream_xml_type
		
	| POText_Materialized -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_no_type indep_expr_types in
	      dom_list_xml_type
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  -----------------------------------------------------
	  CTC |- TextComputedOp{}(Indep1) : SAX STREAM/DOM LIST
	*)
	| POTextComputed_Stream -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      sax_stream_xml_type
		
	| POTextComputed_Materialized -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      dom_list_xml_type
		
	(*
	  CTC |- Indep_1 : PT_1 , PT_1 <: XML
	  ...
	  CTC |- Indep_n : PT_n , PT_n <: XML
	  ----------------------------------------------------------------
	  CTC |- ElemOp[name]{}(Indep_1,...,Indep_n) : SAX STREAM/DOM LIST
	*)
	| POElem_Stream -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_many_non_discarded_xml_types indep_expr_types in
	    sax_stream_xml_type
		
	| POElem_Materialized -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_many_non_discarded_xml_types indep_expr_types in
	    dom_list_xml_type

	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  CTC |- Indep_2 : PT_2, PT_2 <: XML
	  -------------------------------------------------------
	  CTC |- AnyElemOp{}(Indep1,Indep2) : SAX STREAM/DOM LIST
	*)
	| POAnyElem_Stream -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_two_non_discarded_xml_types indep_expr_types in
	      sax_stream_xml_type
		
	| POAnyElem_Materialized -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_two_non_discarded_xml_types indep_expr_types in
	      dom_list_xml_type
		
	(*
	  CTC |- Indep_1 : PT_1 , PT_1 <: XML
	  ...
	  CTC |- Indep_n : PT_n , PT_n <: XML
	  ----------------------------------------------------------------
	  CTC |- AttrOp[name]{}(Indep_1,...,Indep_n) : SAX STREAM/DOM LIST
	*)
	| POAttr_Stream -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_many_non_discarded_xml_types indep_expr_types in
	      sax_stream_xml_type
		
	| POAttr_Materialized -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_many_non_discarded_xml_types indep_expr_types in
	      dom_list_xml_type
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  CTC |- Indep_2 : PT_2, PT_2 <: XML
	  -------------------------------------------------------
	  CTC |- AnyAttrOp{}(Indep1,Indep2) : SAX STREAM/DOM LIST
	*)
	| POAnyAttr_Stream -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_two_non_discarded_xml_types indep_expr_types in
	      sax_stream_xml_type
		
	| POAnyAttr_Materialized -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_two_non_discarded_xml_types indep_expr_types in
	      dom_list_xml_type
		
	(* But all other constructors yield materialized items *)
	(*
	  CTC |- Indep_1 : PT_1 , PT_1 <: XML
	  ...
	  CTC |- Indep_n : PT_n , PT_n <: XML
	  ------------------------------------------------
	  CTC |- ErrorOp{}(Indep_1,...,Indep_n) : DOM LIST
	*)
	| POError -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_many_non_discarded_xml_types indep_expr_types in
	      (* The output type depends on the indep. input types! - Michael *)
	      dom_list_xml_type
		
	(*
	  -----------------------------
	  CTC |- EmptyOp{}() : DOM LIST
	*)
	| POEmpty -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_no_type indep_expr_types in
	      dom_list_xml_type
		
	(*
	  ---------------------------------
	  CTC |- ScalarOp[a]{}() : DOM LIST
	*)
	| POScalar -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_no_type indep_expr_types in
	      dom_list_xml_type
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  CTC |- Indep_2 : PT_2, PT_2 <: XML
	  -----------------------------------------
	  CTC |- SeqOp{}(Indep1, Indep2) : DOM LIST
	*)
	| POSeq_Materialized 
	| POImperativeSeq_Materialized ->
	    let _ = access_nosub dep_exprs in 
	    let (_,_) = access_two_non_discarded_xml_types indep_expr_types in
	    dom_list_xml_type
	| POSeq_Stream 
	| POImperativeSeq_Stream ->
	    let _ = access_nosub dep_exprs in 
	    let (_,_) = access_two_non_discarded_xml_types indep_expr_types in
	    sax_stream_xml_type

	(* Basic Tuple operations *)
	(*
	  PT = CTC[INPUT]#q
	  --------------------------------
	  CTC |- AccessTupleOp[q]{}() : PT
	*)
	| POAccessTuple field_name -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_no_type indep_expr_types in
	      (* Must assert non-discarded XML type at binding time. *)
	      PT_XML(lookup_field_type field_name (get_input_type ctc))

	(*
	  CTC |- Indep_1 : PT_1 , PT_1 <: XML
	  ...
	  CTC |- Indep_n : PT_n , PT_n <: XML
	  PTT = [q_1 : PTFT_1, ... , q_n : PTFT_n]
	  ---------------------------------------------------------------------
	  CTC |- CreateTupleOp[[PTT]][q_1,...,q_n]{}(Indep_1,...,Indep_n) : PTT
	*)
	| POCreateTuple names_types -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_many_non_discarded_xml_types indep_expr_types in
	      PT_Table(names_types)

	(*
	  PT = CTC[INPUT] <: Table
	  ----------------------------
	  CTC |- InputTupleOp{}() : PT
	*)
	| POInputTuple -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_no_type indep_expr_types in 
	      (* Must assert non-discarded XML type at binding time. *)
	      PT_Table(get_input_type ctc)

	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: Table
	  CTC |- Indep_1 : PT_2, PT_2 <: Table
	  -----------------------------------------------------
	  CTC |- ConcatTupleOp{}(Indep_1,Indep_2) : PT_1 @ PT_2
	*)
	| POConcatTuples ->
	    let _ = access_nosub dep_exprs in
	    let (t1, t2) = access_two_table_types indep_expr_types in
	      (PT_Table (t1 @ t2))
		
	(*
	  Need auxilliary judgement for mat(PT) according to function
	  Xquery_physical_type_ast_util.materialize_xml_type. - Michael

	  CTC |- Indep_1 : PT_1, PT_1 <: Table
	  CTC |- Indep_2 : PT_2, PT_2 <: Table
	  -----------------------------------------------------------
	  CTC |- ProductTupleOp{}(Indep_1,Indep_2) : PT_1 @ mat(PT_2)
	*)
	| POProduct ->
	    let _ = access_nosub dep_exprs in
	    let (t1, t2) = access_two_table_types indep_expr_types in
	      
	    (* The right side is materialized to an array of DOM values. *)
	    let t2' = List.map (fun (n, t) -> (n, materialize_xml_type t)) t2 in
	      (PT_Table (t1 @ t2'))

	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  CTC |- Dep_2 : PT_2, 
	  -----------------------------------------------
	  CTC |- ServerImplements{Dep_2}(Indep_1) : PT_2

          Why is this rule different than all others? 
	*)
	| POServerImplementsTree 
	| POServerImplementsTuple -> 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
            let dep_expr = access_onesub dep_exprs in 
            begin
              match (dep_expr.palgop_expr_eval_sig) with
       	      | Some (_, _, output_type) -> output_type
              | None -> raise(Query(Internal_Error("POServerImplements input does not have physical type")))
            end

	(*
          We could have the result of a remote expression be a SAX
          type...

	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  CTC |- Indep_2 : PT_2, PT_2 <: XML
	  -----------------------------------------------
	  CTC |- Execute(Indep_1, Indep_2) : DOM CURSOR
	*)
	| POExecuteTree -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_two_non_discarded_xml_types indep_expr_types in
	      dom_cursor_xml_type
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  CTC |- Indep_2 : PT_2, PT_2 <: Table
	  -----------------------------------------------
	  CTC |- Execute(Indep_1, Indep_2) : Table
	 *)

	| POExecuteTuple -> 
	    let _ = access_nosub dep_exprs in 
	    let (indep_type1, indep_type2) = access_two_types indep_expr_types in
            let _ = assert_non_discarded_xml_type indep_type1 in
            let _ = assert_table_type indep_type2 in
	    indep_type2
		
	(*
          We have no bloody idea what the physical type of a closure is...

	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  ----------------------------------------
	  CTC |- EvalClosure(Indep_1) : XML
	*)
	| POEvalClosureTree -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      dom_cursor_xml_type
	(*
          We have no bloody idea what the physical type of a closure is...

	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  ----------------------------------------
	  CTC |- EvalClosure(Indep_1) : Table
	*)
	| POEvalClosureTuple -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      raise (Query (Prototype "No physical typing for eval-box that yields tuples."))
		
	(*
          We have no bloody idea what the physical type of a closure is...

	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  ----------------------------------------
	  CTC |- ForServerClose(Indep_1) : XML
	*)
	| POForServerCloseTree -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      dom_cursor_xml_type
	(*
          We have no bloody idea what the physical type of a closure is...

	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  ----------------------------------------
	  CTC |- ForServerClose(Indep_1) : Table
	*)
	| POForServerCloseTuple -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      raise (Query (Prototype "No physical typing for for-server-box that yields tuples."))
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: Table
	  ------------------------------------
	  CTC |- DistinctOp{}(Indep_1) : PT_1
	   Does internal materialization of some sort.- Michael 
	| PODistinct name 
	  -> 
	    let _ = access_nosub dep_exprs in 
	  (PT_Table(access_one_table_type indep_expr_types))
	*)
	    
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: Table
	  --------------------------------------------
	  CTC |- PruneOp[field,axis]{}(Indep_1) : PT_1
	| POPrune    field 
	  -> 
	    let _ = access_nosub dep_exprs in 
	      (PT_Table(access_one_table_type indep_expr_types))
	*)
		
	(* XPath evaluation *)
	(*
	  ------------------------------------------
	  CTC |- ParseStreamOp[uri]{}() : SAX STREAM
	*)
	| POParse_Stream -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_no_type indep_expr_types in 
	      sax_stream_xml_type
		
	(*
	  ----------------------------------------
	  CTC |- ParseLoadOp[uri]{}() : DOM CURSOR
	*)
	| POParse_Load -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_no_type indep_expr_types in 
	      dom_cursor_xml_type

	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  ---------------------------------------
	  CTC |- TreeJoin_SortOp{}(Indep_1) : DOM
	*)
	| POTreeJoin_Sort -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      (* This is dealing with tables! - Michael *)
	      dom_list_xml_type
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  ----------------------------------------------
	  CTC |- TreeJoin_SortOp{}(Indep_1) : SAX STREAM
	*)
	| POTreeJoin_Stream -> 
	    let _ = access_nosub dep_exprs in 

	    (* Make sure the input is a Sax Stream as expected. - Michael *)
	    let _ = access_one_sax_stream_xml_type indep_expr_types in
	      sax_stream_xml_type
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  ----------------------------------------------------
	  CTC |- TreeJoin_NestedLoopOp{}(Indep_1) : DOM CURSOR
	*)
	| POTreeJoin_NestedLoop -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      dom_cursor_xml_type
		
	(* Type operators *)
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  --------------------------------------
	  CTC |- TreatOp{}(Indep_1) : DOM CURSOR
	*)
	| POTreat -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      dom_cursor_xml_type
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  -----------------------------------
	  CTC |- CastOp{}(Indep_1) : DOM LIST
	*)
	| POCast -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      dom_list_xml_type
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  -----------------------------------
	  CTC |- CastOp{}(Indep_1) : DOM LIST
	*)
	| POCastable -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      dom_list_xml_type
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  -----------------------------------------
	  CTC |- ValidateOp{}(Indep_1) : SAX STREAM
	*)
	| POValidate -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      sax_stream_xml_type

	(* Tuple Maps *)
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: Table
	  PT = [q : DOM LIST] @ PT_1
	  ----------------------------------------
	  CTC |- NullMapOp[q]{}(Indep_1) : PT
	  CTC |- MapIndexOp[q]{}(Indep_1) : PT
	  CTC |- MapIndexStepOp[q]{}(Indep_1) : PT
	*)
	| PONullMap   name   
	| POMapIndex  name   
	| POMapIndexStep name -> 
	    let _ = access_nosub dep_exprs in 
	      (PT_Table((name, dom_list_type) :: access_one_table_type indep_expr_types))

	(* Update operations *)
	(* All update operators yield materialized XML *)
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  ----------------------------------
	  CTC |- CopyOp{}(Indep_1) : DOM
	  CTC |- DeleteOp{}(Indep_1) : DOM
	  CTC |- DetachOp{}(Indep_1) : DOM
	*)
	| POCopy
	| PODelete
	  -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	      dom_list_xml_type
		
	(*
	  CTC |- Dep : PT, PT <: XML
	  --------------------------
	  CTC |- SnapOp{Dep}() : PT
	*)
	| POSnap ->
	    let _ = access_no_type indep_expr_types in
	    let e = access_onesub dep_exprs in 
	    let _ = assert_non_discarded_xml_type(code_typing_expr code_ctxt ctc e) in
	      dom_list_xml_type
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  CTC |- Indep_2 : PT_2, PT_2 <: XML
	  ----------------------------------------
	  CTC |- RenameOp{}(Indep_1,Indep_2) : DOM
	  CTC |- InsertOp{}(Indep_1,Indep_2) : DOM
	  CTC |- ReplaceOp{}(Indep_1,Indep_2) : DOM
	*)
	| PORename
	| POInsert 
	| POReplace  
	  -> 
	    let _ = access_nosub dep_exprs in 
	    let _ = access_two_non_discarded_xml_types indep_expr_types in
	      dom_list_xml_type
		
	| POSet vn -> 
	    let _ = access_nosub dep_exprs in 
            let _ = access_one_non_discarded_xml_type indep_expr_types in
	    PT_XML (get_variable_type ctc vn)

	(* XPath evaluation *)
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: Table
	  PT = [q_1 : DOM; ..., q_n : DOM] @ PT_1
	  --------------------------------------------------------
	  CTC |- TupleTreePatternOp[q_1,...,q_n]{}(Indep_1) : PT
	*)
	| POTupleTreePattern_NestedLoop output_fields 
	| POTupleTreePattern_TwigJoin output_fields 
	| POTupleTreePattern_SCJoin output_fields 
	  -> 
	    let _ = access_nosub dep_exprs in 
	      PT_Table((List.map (fun f -> (f, dom_list_type)) output_fields) @ (access_one_table_type indep_expr_types))
	| POTupleTreePattern_IndexSortJoin output_fields -> 
	    raise (Query (Prototype "Index-sort join not supported for Twigs yet."))
	| POTupleTreePattern_Streaming output_fields -> 
	    raise (Query (Prototype "Streaming not supported for Twigs yet."))
	      
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  CTC |- Dep_2 : PT_2, PT_2 <: XML
	  -----------------------------------------
	  CTC |- WhileOp{Dep_2}{Indep_1} : PT_2
	*)
	| POWhile ->
	    let _ = access_no_type indep_expr_types in
	    let dep_sub_expr_types = code_typing_sub_exprs code_ctxt ctc dep_exprs in
	    let t1,t2 = access_two_non_discarded_xml_types dep_sub_expr_types in
	    PT_XML(t2)
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  CTC |- Dep_2 : PT_2, PT_2 <: XML
	  CTC |- Dep_3 : PT_3, PT_3 <: XML
	  PT = LeastUpperBound(PT_2,PT_3)
	  ---------------------------------------
	  CTC |- IfOp{Dep_2, Dep_3}{Indep_1} : PT
	*)
	| POIf ->
	    let _ = access_one_non_discarded_xml_type indep_expr_types in
	    let dep_sub_expr_types = code_typing_sub_exprs code_ctxt ctc dep_exprs in
	    let (t2, t3) = access_two_non_discarded_xml_types dep_sub_expr_types in
	    PT_XML(least_upper_xml_type t2 t3)
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  PVT = [v : PT_1']
	  CTC + (v : PT_1') |- Dep_2 : PT_2
	  PT_2 <: Table
	  -----------------------------------------------------
	  CTC |- MapFromItemOp[[PVT]][v]{Dep_2}{Indep_1} : PT_2
	*)
	| POMapFromItem (v, t) -> 
            let _ = access_one_non_discarded_xml_type indep_expr_types in
	    let ctc' = add_variable_type ctc v t in
	    let e1 = access_onesub dep_exprs in
	      assert_table_type(code_typing_expr code_ctxt ctc' e1)
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  PVT = [v : PT_1']
	  CTC + (v : PT_1') |- Dep_2 : PT_2
	  PT_2 <: XML
	  ---------------------------------------------
	  CTC |- LetOp[[PVT]][v]{Dep_2}{Indep_1} : PT_2
	*)
	| POLetvar (v, t) -> 
            let _ = access_one_non_discarded_xml_type indep_expr_types in
	    let ctc' = add_variable_type ctc v t in
	    let e1 = access_onesub dep_exprs in 
	      PT_XML(assert_non_discarded_xml_type(code_typing_expr code_ctxt ctc' e1))
		
	(*
	  CTC |- Indep : PT, PT <: XML
	  CTC + (v : PT_1) |- Dep_1 : PT_1, PT_1 <: XML
	  ...
	  CTC + (v : PT_n) |- Dep_n : PT_n, PT_n <: XML
	  ---------------------------------------------------------
	  CTC |- Typeswitch[v1,...,vn]{Dep_1,...Dep_n}{Indep} : DOM
	*)
	| POTypeswitch vn_array
	  ->
	    (* This needs to be based on use count information.
	       May return 'too small' types right now! - Michael *)
	    begin
	      try
		let t1 = access_one_non_discarded_xml_type indep_expr_types in
		let earray = access_manysub dep_exprs in 
		let vn_expr_list = List.combine (Array.to_list vn_array) (Array.to_list earray) in
		  PT_XML(
  		    List.fold_left
		      (fun t (vopt, e) -> 
			 let t' = 
			   assert_non_discarded_xml_type(
  			     match vopt with 
			       | None -> 
				   code_typing_expr code_ctxt ctc e
			       | Some v -> 
				   let ctc' = add_variable_type ctc v t1 in
				     code_typing_expr code_ctxt ctc' e)
			 in
			   least_upper_xml_type t t'
		      ) (PT_Sax PT_Stream) vn_expr_list)
	      with
		| Invalid_argument msg -> raise (Query(Code_Selection("In type_check_physical_op, Typeswitch arguments do not match. "^msg)))
	    end

	(*
	  CTC |- Indep : PT, PT <: Table
	  CTC + (INPUT : PT) |- Dep_1 : PT_1, PT_1 <: XML
	  ...
	  CTC + (INPUT : PT) |- Dep_n : PT_n, PT_n <: XML
	  -----------------------------------------------
	  CTC |- Select{Dep_1,...,Dep_n}(Indep) : PT
	*)
	| POSelect ->
	    let t1 = access_one_table_type indep_expr_types in
	    let earray = access_manysub dep_exprs in 
	    let ctc' = add_input_type ctc t1 in
	    let _ = Array.map (fun ei -> assert_non_discarded_xml_type(code_typing_expr code_ctxt ctc' ei)) earray in
	      PT_Table(t1)
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: Table
	  CTC + (INPUT : PT_1) |- Dep_2   : PT_2, PT_2 <: Table
	  -----------------------------------------------------
	  CTC |- Map{Dep_2}(Indep_1) : PT_2
	*)
	| POMap -> 
	    let t1 = access_one_table_type indep_expr_types in
	    let e2 = access_onesub dep_exprs in 
	    let ctc' = add_input_type ctc t1 in
	      assert_table_type(code_typing_expr code_ctxt ctc' e2) 
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: Table
	  CTC + (INPUT : PT_1) |- Dep_2 : PT_2, PT_2 <: Table
	  ---------------------------------------------------
	  CTC |- MapConcatOp{Dep_2}{Indep_1) : PT_1 @ PT_2
	*)
	| POMapConcat -> 
	    let t1 = access_one_table_type indep_expr_types in
	    let e2 = access_onesub dep_exprs in 
	    let ctc' = add_input_type ctc t1 in
	    let t2 = assert_tuple_type(code_typing_expr code_ctxt ctc' e2) in
	      PT_Table (t1 @ t2)

	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: Table
	  CTC + (INPUT : PT_1) |- Dep_2 : PT_2, PT_2 <: Table
	  PT_3 = [v : DOM LIST] @ PT_1 @ PT2
	  ---------------------------------------------------
	  CTC |- OMapConcatOp[v]{Dep_2}{Indep_1) : PT_3
	*)
	| POOuterMapConcat v -> 
	    let t1 = access_one_table_type indep_expr_types in
	    let e2 = access_onesub dep_exprs in 
	    let ctc' = add_input_type ctc t1 in
	    let t2 = assert_tuple_type(code_typing_expr code_ctxt ctc' e2) in
	      PT_Table ((v, dom_list_type) :: t1 @ t2)
		
	(*
	  Need auxilliary judgement for concat(PT) according to function
	  Xquery_physical_type_ast_util.concat_xml_type. - Michael

	  CTC |- Indep_1 : PT_1, PT_1 <: Table
	  CTC + (INPUT : PT_1) |- Dep_2 : PT_2, PT_2 <: XML
	  -------------------------------------------------
	  CTC |- MapToItemOp{Dep_2}{Indep_1) : concat(PT_2)
	*)
	| POMapToItem -> 
	    let t1 = access_one_table_type indep_expr_types in
	    let e2 = access_onesub dep_exprs in 
	    let ctc' = add_input_type ctc t1 in
	    let t2 = assert_non_discarded_xml_type(code_typing_expr code_ctxt ctc' e2) in
	      
	    (* A tuple to item map never returns an item list, even if the return
	       expression evaluates to an item list for each single tuple. - Michael *)
	    let t2' = concat_xml_type t2 in
	      PT_XML(t2')
		
	(*
	  NB: These rules do not correspond to paper, which compiles
	      into tuple operators!

	  Must return item list! - Michael
	  
	  CTC |- Indep_1 : PT_1, PT_1 <: XML
	  PVT = [v : PT_1']
	  CTC + (v : PT_1') |- Dep_2 : PT_2, PT_2 <: XML
	  -----------------------------------------------
	  CTC |- SomeOp[[PVT]][v]{Dep_2}{Indep_1) : PT_2 
	  CTC |- EveryOp[[PVT]][v]{Dep_2}{Indep_1) : PT_2
	*)
	| POSome (v, t) -> 
            let _ = access_one_non_discarded_xml_type indep_expr_types in
	    let ctc' = add_variable_type ctc v t in
	    let e2 = access_onesub dep_exprs in 
	    let t2 = assert_non_discarded_xml_type(code_typing_expr code_ctxt ctc' e2) in
	      PT_XML(t2) (* dom_list_xml_type *)
		
	| POEvery (v, t) -> 
            let _ = access_one_non_discarded_xml_type indep_expr_types in
	    let ctc' = add_variable_type ctc v t in
	    let e2 = access_onesub dep_exprs in 
	    let t2 = assert_non_discarded_xml_type(code_typing_expr code_ctxt ctc' e2) in
	      PT_XML(t2) (* dom_list_xml_type *)

	(*
	  Need auxilliary judgement for mat(PT) according to function
	  Xquery_physical_type_ast_util.materialize_xml_type. - Michael

	  CTC |- Indep : PT, PT <: Table
	  CTC + (INPUT : PT) |- Dep_1 : PT_1, PT_1 <: XML
	  ...
	  CTC + (INPUT : PT) |- Dep_n : PT_n, PT_n <: XML
	  ----------------------------------------------------------
	  CTC |- OrderBy[v1,...,vn]{Dep_1,...Dep_n}{Indep} : mat(PT)
	*)
	| POOrderBy -> 
	    let t1 = access_one_table_type indep_expr_types in

	    (* The tuple cursor is materialized to an array of DOM values. *)
	    let t1' = List.map (fun (n, t) -> (n, materialize_xml_type t)) t1 in
	    let earray = access_manysub dep_exprs in 
	    let ctc' = add_input_type ctc t1 in
	    let _ = Array.map (fun ei -> assert_non_discarded_xml_type(code_typing_expr code_ctxt ctc' ei)) earray in
	      PT_Table(t1')
		
	(* Operators with TWO Table inputs *)
	(*
	  Need auxilliary judgement for mat(PT) according to function
	  Xquery_physical_type_ast_util.materialize_xml_type. - Michael

	  CTC |- Indep_1 : PT_1, PT_1 <: Table
	  CTC |- Indep_2 : PT_2, PT_2 <: Table
	  PT = PT_1 @ mat(PT_2)
	  CTC + (INPUT : PT) |- Dep_1 : PT_1, PT_1 <: XML
	  ...
	  CTC + (INPUT : PT) |- Dep_n : PT_n, PT_n <: XML
	  ----------------------------------------------------
	  CTC |- JoinOp{Dep_1,...,Dep_n}(Indep_1,Indep_2) : PT
	*)
	| POJoin_Hash
	| POJoin_Sort 
	| POJoin_NestedLoop ->
	    let (t1, t2) = access_two_table_types indep_expr_types in

	    (* The right side is materialized to an array of DOM values. *)
	    let t2' = List.map (fun (n, t) -> (n, materialize_xml_type t)) t2 in
	    let pt = (t1 @ t2') in
	    let earray = access_manysub dep_exprs in 
	    let ctc' = add_input_type ctc pt in
	    let _ = Array.map (fun ei -> assert_non_discarded_xml_type(code_typing_expr code_ctxt ctc' ei)) earray in
	      PT_Table(pt)
		
	(*
	  CTC |- Indep_1 : PT_1, PT_1 <: Table
	  CTC |- Indep_2 : PT_2, PT_2 <: Table
	  PT = PT_1 @ PT_2
	  CTC + (INPUT : PT) |- Dep_1 : PT_1, PT_1 <: XML
	  ...
	  CTC + (INPUT : PT) |- Dep_n : PT_n, PT_n <: XML
	  PT' = [v : DOM] @ PT
	  ------------------------------------------------------
	  CTC |- LeftOuterJoinOp[v]{Dep_1,...,Dep_n}(Indep_1,Indep_2) : PT'
	*)
	| POLeftOuterJoin_Hash v
	| POLeftOuterJoin_Sort v
	| POLeftOuterJoin_NestedLoop v
	  ->
	    let (t1, t2) = access_two_table_types indep_expr_types in
	    let pt = (t1 @ t2) in
	    let earray = access_manysub dep_exprs in 
	    let ctc' = add_input_type ctc pt in
	    let _ = Array.map (fun ei -> assert_non_discarded_xml_type(code_typing_expr code_ctxt ctc' ei)) earray in
	      PT_Table((v, dom_list_type) :: pt)

	(* Grouping and Ordering operations *)
	(*
	  CTC |- Indep : PT, PT <: Table
	  CTC + (INPUT : PT) |- Dep_1 : PT_1, PT_1 <: XML
	  ...
	  CTC + (INPUT : PT) |- Dep_n : PT_n, PT_n <: XML
	  PT' = [v_1 : DOM;...;v_k : DOM] @ PT
	  ------------------------------------------------------
	  CTC |- GroupByOp[v_1,...,v_k]{Dep_1,...,Dep_n}(Indep) : PT'
	*)
	(* ??Question:?? Are only the aggregate fields added to the result tuple ?? *)
	| POGroupBy agg_name_list ->
	    let t1 = access_one_table_type indep_expr_types in
	    let earray = access_manysub dep_exprs in 
	    let ctc' = add_input_type ctc t1 in
	    let _ = Array.map (fun ei -> assert_non_discarded_xml_type(code_typing_expr code_ctxt ctc' ei)) earray in
	      PT_Table((List.map (fun vi -> (vi, dom_list_type)) agg_name_list) @ t1)

	| POProject fields ->
	    let _ = access_nosub dep_exprs in 
	    PT_Table(List.map (fun f -> (f, dom_list_type)) (Array.to_list fields))
		       (* @ (access_one_table_type indep_expr_types)) *)

    in (pop, indep_expr_types, output_type)
  with exn -> (print_string ((Print_xquery_physical_type.string_of_physop_expr_name pop)^" XXXXX\n"); raise exn) 

and code_typing_expr_parts code_ctxt ctc algop_expr =
  try
    let indep_sub_exprs = algop_expr.psub_expression in
    let dep_sub_exprs = algop_expr.pdep_sub_expression in
    (* 1. Typecheck I1 : T1 ... Ik : Tk in environment PEnv *)
    (* 2. For Op, and T1....Tk pick up the corresponding physical operator POp. *)
    let indep_sub_expr_types = code_typing_sub_exprs code_ctxt ctc indep_sub_exprs in
    let pop = select_physical_op ctc code_ctxt algop_expr indep_sub_expr_types in
    (* 3-6. Apply type checking for possible additional constraints on the
       (type of POp), and compute the output physical type T_OUT). This
       (yields the physical type signature) P_Sig *)
    type_check_physical_op code_ctxt ctc pop indep_sub_expr_types dep_sub_exprs
  with 
  | exn -> (raise (error_with_file_location algop_expr.palgop_expr_loc exn))

and code_typing_expr code_ctxt ctc algop_expr =
  let phys_sig = code_typing_expr_parts code_ctxt ctc algop_expr in
  (* 7. Annotate the AST with the physical operator and types. *)
  algop_expr.palgop_expr_eval_sig <- (Some phys_sig);
  let (phys_op, input_types, output_type) = phys_sig in
  output_type

(* sax_stream_xml_type *)

(* Recursively type sub-expressions, which has the side effect of
   annotating sub-expressions with their physical operators and
   physical types.  *)
and code_typing_sub_exprs code_ctxt ctc sub_exprs =
  match sub_exprs with 
  | NoSub -> 
      NoInput 
  | OneSub op1 -> 
      OneInput (code_typing_expr code_ctxt ctc op1)
  | TwoSub (op1,op2) ->
      TwoInput(code_typing_expr code_ctxt ctc op1, code_typing_expr code_ctxt ctc op2)
  | ManySub op_array -> 
      ManyInput (Array.map (code_typing_expr code_ctxt ctc) op_array)

let code_typing_statement code_ctxt astmt = 
  (* For a top-level statement, we store the compilation annotations
     as both global and local annotations *)
  let code_ctxt' = store_annotation code_ctxt astmt.compile_annotations in
  let code_ctxt'' = store_global_annotation code_ctxt' astmt.compile_annotations in
  let ctc = (code_type_context_from_code_selection_context code_ctxt) in
    ignore(code_typing_expr code_ctxt'' ctc astmt);
    ctc

(* Variable or index declaration *)
(* Physical typing of the top-level variable/index declarations
extends the code-type context *)

let code_typing_decl code_ctxt ctc var_decl = 
  match var_decl.alg_decl_name with 
    | AOEVarDecl (_, cvname) ->  
	let e1 = access_onesub var_decl.alg_decl_indep in
	let _ = access_nosub var_decl.alg_decl_dep in
	let code_ctxt' = store_global_annotation code_ctxt e1.compile_annotations in
	let output_type = code_typing_expr code_ctxt' ctc e1 in  
	  add_variable_type ctc cvname (assert_non_discarded_xml_type output_type)
    | AOEVarDeclExternal (_, cvname) 
    | AOEVarDeclImported (_, cvname) -> 
	(* TODO! What is the physical type of externally declared and imported variables? 
           A completely materialized XML value? 
	*) 
	let _ = access_nosub var_decl.alg_decl_indep in
	let _ = access_nosub var_decl.alg_decl_dep in
	  add_variable_type ctc cvname dom_list_type
    | AOEValueIndexDecl str -> 
	let e1 = access_onesub var_decl.alg_decl_indep in
	let e2 = access_onesub var_decl.alg_decl_dep in
	let _ = code_typing_expr code_ctxt ctc e1 in  
	let _ = code_typing_expr code_ctxt ctc e2 in  
	  ctc
    | AOENameIndexDecl  rsym ->       
	let _ = access_nosub var_decl.alg_decl_indep in
	let _ = access_nosub var_decl.alg_decl_dep in
	  ctc
	    
(* Function declaration *)
let code_typing_function_decl code_ctxt ctc func_decl = 
  let ((fname, ct), sign, func_defn,_) = func_decl.palgop_function_decl_desc in
    
  (* Now we have to assign physical types to the formal vars and
     compute the physical types for the function body *)
  let ctc' = Array.fold_left (fun ctc v -> add_variable_type ctc v dom_list_type) ctc func_defn.palgop_func_formal_args in
  match !(func_defn.palgop_func_optimized_logical_plan) with 
  | AOEFunctionImported -> ()
  | AOEFunctionUser userbody -> 
      let code_ctxt' = store_global_annotation code_ctxt userbody.compile_annotations in
      ignore(code_typing_expr code_ctxt' ctc' userbody)

(* Prolog *)
let code_typing_prolog_aux code_ctxt ctc aprolog = 
  let ctc' = List.fold_left (code_typing_decl code_ctxt) ctc aprolog.palgop_prolog_vars in
  let ctc'' = List.fold_left (code_typing_decl code_ctxt) ctc' aprolog.palgop_prolog_indices in
  let _ = List.iter (code_typing_function_decl code_ctxt ctc'') aprolog.palgop_prolog_functions in
  ctc''

let code_typing_prolog code_ctxt aprolog = 
(*
   print_string "In code_typing_prolog\n";
   let x = Print_xquery_algebra.bprintf_logical_algprolog  ">>>>>>>>>>>>>>\n" aprolog
   in print_string (x^"\n<<<<<<<<<<\n\n");
*)
  let ctc = (code_type_context_from_code_selection_context code_ctxt) in
  code_typing_prolog_aux code_ctxt ctc aprolog 

(*  | exn -> (printf_error_safe "\nIn Cs_code_typing_top.code_typing_prolog " exn; ctc) *)

(* Module *)
let code_typing_module code_ctxt amodule = 
(* print_string "In code_typing_module\n";*)
  let ctc = (code_type_context_from_code_selection_context code_ctxt) in
  let ctc' = code_typing_prolog_aux code_ctxt ctc amodule.palgop_module_prolog in 
  let _ = List.map (code_typing_expr code_ctxt ctc') amodule.palgop_module_statements in
  ctc'

(*  | exn -> (printf_error_safe "\nIn Cs_code_typing_top.code_typing_module " exn; ctc) *)
