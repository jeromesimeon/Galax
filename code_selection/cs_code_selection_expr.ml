(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cs_code_selection_expr.ml,v 1.72 2008/02/01 02:45:26 mff Exp $ *)

(* Module: Cs_code_selection_expr
   Description:
     This module performs code selection for a logical plan.
*)

open Error 

open Xquery_common_ast
open Xquery_algebra_ast
open Xquery_physical_type_ast
open Xquery_algebra_ast_util


open Algebra_type
open Cs_util_coercion

open Code_group_order
open Code_update
open Cs_code_top

open Compile_context
open Code_selection_context   
open Code_util_materialize

open Physical_value_util


(**********************************)
(* Code selection for expressions *)
(**********************************)

(* Goal:
   Given an undecorated algebra, instantiate the code
   so that it can run 
*)
(* All variables in the type switch are mutually exclusive in terms of scoping..
   we assign them to the same spot *)
let should_flush_tuples algop =
  match algop.palgop_expr_name with
      AOEMapToItem
    | AOEAccessTuple _  -> true
    | _ -> false

let rec single_op_default_code_selection module_uri code_ctxt algop  =

  (* The algebraic operator contains the file location that should be
     correlated with errors, so we catch any exceptions here and
     rewrap the exceptions with the file location. *)
  try
    begin
      let code_ctxt = store_annotation code_ctxt algop.compile_annotations in
          (* Modify the code *)
        let eval_code, ret_ctxt =
	match algop.palgop_expr_name with
          (*************************)
	  (* Functional operations *)
          (*************************)
	| AOELetvar (odt,vn) ->
	    Code_functional_ops.build_let_code code_ctxt algop (odt, vn)
	| AOEIf ->
	    Code_functional_ops.build_if_code code_ctxt algop 
	| AOEWhile ->
	    Code_functional_ops.build_while_code code_ctxt algop 
	| AOETypeswitch(pattern) ->
	    Code_typeswitch.build_typeswitch_code code_ctxt algop pattern
	| AOEVar (vn) ->
	    begin
	      try
		let imported_code_ctxt = get_imported_variable_context code_ctxt vn in
		  (* Here, we need the code_selection_context of the imported module to do selection 
		     for the variable reference, but we return the code_selection_context of 
		     the *calling* module 
		  *)
		let (physical_code, _) = Code_functional_ops.build_var_code imported_code_ctxt algop vn
		in (physical_code, code_ctxt)
	      with
	      | Not_found -> Code_functional_ops.build_var_code code_ctxt algop vn
	    end
	| AOECallBuiltIn ((cfname,arity), optintypes, outtype, _) ->
	    Code_builtin_fn.build_builtin_fn_code code_ctxt algop ((cfname,arity), optintypes, outtype) 
	| AOECallUserDefined((fname, arity), optintypes, outtype, _, selfrecur) ->
	    begin
	      try
		let imported_code_ctxt = get_imported_function_context code_ctxt (fname, arity) in
		begin
(* print_string ("Call imported "^(Namespace_names.prefixed_string_of_rqname fname)^":"^(string_of_int arity)^"\n"); *)
		  (* Here, we need the code_selection_context of the imported module to do selection 
		     for the function call, but we return the code_selection_context of 
		     the *calling* module 
		  *)
                  let (physical_code, _) = Code_user_defined_fn.build_user_defined_fn_code 
                             imported_code_ctxt algop ((fname, arity), optintypes, outtype) in
		  (physical_code, code_ctxt)
		end
	      with
	      | Not_found -> 
(*(print_string ("Call local "^(Namespace_names.prefixed_string_of_rqname fname)^":"^(string_of_int arity)^"\n"); *)
		  Code_user_defined_fn.build_user_defined_fn_code code_ctxt algop ((fname, arity), optintypes, outtype)
	    end
	| AOECallOverloaded((cfname,arity),table) ->
	    Code_overloaded_fn.build_overloaded_fn_code code_ctxt algop ((cfname,arity),table)
	| AOEConvertSimple atomic_type ->
	    Code_builtin_fn.build_convert_simple_code code_ctxt algop atomic_type
	| AOEPromoteNumeric atomic_type ->
	    Code_builtin_fn.build_promote_numeric_code code_ctxt algop atomic_type
	| AOEPromoteAnyString ->
	    Code_builtin_fn.build_promote_anystring_code code_ctxt algop
	| AOEUnsafePromoteNumeric atomic_type ->
	    Code_builtin_fn.build_unsafe_promote_numeric_code code_ctxt algop atomic_type
          (*****************)
	  (* Constructors  *)
          (*****************)
	| AOEScalar( dmv ) -> 
	    Code_constructors.build_scalar_code code_ctxt algop dmv
	| AOESeq ->
	    Code_constructors.build_seq_code code_ctxt algop 
	| AOEEmpty -> 
	    Code_constructors.build_empty_code code_ctxt algop 
	| AOEDocument ->
	    Code_constructors.build_document_code code_ctxt algop 
	| AOEPI(ncname,str) ->
	    Code_constructors.build_pi_code code_ctxt algop (ncname,str) 
	| AOEPIComputed ->
	    Code_constructors.build_picomputed_code code_ctxt algop 
	| AOEComment str ->
	    Code_constructors.build_comment_code code_ctxt algop str
	| AOECommentComputed ->
	    Code_constructors.build_commentcomputed_code code_ctxt algop
	| AOEText text ->
	    Code_constructors.build_text_code code_ctxt algop text
	| AOECharRef i ->
	    Code_constructors.build_charref_code code_ctxt algop i
	| AOETextComputed ->
	    Code_constructors.build_textcomputed_code code_ctxt algop
	| AOEElem (relem_sym,nsenv) ->
	    Code_constructors.build_elem_code code_ctxt algop (relem_sym,nsenv) 
	| AOEAnyElem (nsenv1,nsenv2) ->
	    Code_constructors.build_anyelem_code code_ctxt algop nsenv1 nsenv2
	| AOEAttr (rattr_sym,nsenv) -> 
	    Code_constructors.build_attr_code code_ctxt algop rattr_sym nsenv
	| AOEAnyAttr nsenv ->
	    Code_constructors.build_anyattr_code code_ctxt algop nsenv
	| AOEError ->
	    Code_constructors.build_error_code code_ctxt algop
          (******************)
	  (* Type operators *)
          (******************)
	| AOETreat cmodel1 ->
	    Code_type_operators.build_treat_code code_ctxt algop cmodel1 
	| AOEValidate vmode ->
	    Code_type_operators.build_validate_code code_ctxt algop vmode
	| AOECast (nsenv, cmodel1) ->
	    Code_type_operators.build_cast_code code_ctxt algop (nsenv,cmodel1)
	| AOECastable (nsenv, cmodel1) ->
	    Code_type_operators.build_castable_code code_ctxt algop (nsenv,cmodel1)
          (************************)
	  (* Item/Tuple operators *)
          (************************)
	| AOESome (ocdt,vname1) ->
	    Code_item_tuple.build_some_code code_ctxt algop (ocdt, vname1)
	| AOEEvery (ocdt,vname1) ->
	    Code_item_tuple.build_every_code code_ctxt algop (ocdt, vname1)
	| AOEMapToItem ->
	    Code_item_tuple.build_tuple_to_item_map_code code_ctxt algop 
	| AOEMapFromItem vname ->
	    Code_item_tuple.build_item_to_tuple_map_code code_ctxt algop vname
          (*******************)
	  (* Tuple operators *)
          (*******************)
	| AOEInputTuple ->
	    Code_tuple.build_input_tuple_code code_ctxt algop 
	| AOECreateTuple names ->
	    Code_tuple.build_create_tuple_code code_ctxt algop names
	| AOEAccessTuple crname1 ->
	    Code_tuple.build_access_tuple_code code_ctxt algop crname1 
	| AOEConcatTuples ->
	    Code_tuple.build_tuple_concat_code code_ctxt algop 
	| AOEProduct ->
	    Code_tuple.build_tuple_product_code code_ctxt algop 
	| AOESelect pred ->
	    Code_tuple.build_tuple_select_code code_ctxt algop pred
          (**********)
	  (* Joins  *)
          (**********)
	| AOEJoin pred_desc ->
	    Code_join.build_join_code code_ctxt algop (default_code_selection module_uri)
	| AOELeftOuterJoin (null_name,pred_desc) ->
	      (********************************************)
	      (* NOTICE THE CODE IS THE SAME AS JOIN      *)            
	      (*   BUT THE SEMANTIC IS DIFFERENT          *)
	      (********************************************)
	    let code_ctxt = add_tuple_reference code_ctxt null_name in	
	    Code_join.build_join_code code_ctxt algop (default_code_selection module_uri)
          (*****************)
	  (* Map operators *)
          (*****************)
	| AOEMapConcat ->
	    Code_map.build_tuple_map_concat_code code_ctxt algop 
	| AOEOuterMapConcat vn ->
	    Code_map.build_outer_tuple_map_concat_code code_ctxt algop vn
	| AOEMap ->
	    Code_map.build_tuple_map_code code_ctxt algop 
	| AOEMapIndex vname ->
	    Code_map.build_tuple_map_index_code code_ctxt algop vname
	| AOEMapIndexStep vname ->
	      (********************************************)
	      (* NOTICE THE CODE IS THE SAME AS MAP INDEX *)            
	      (*   BUT THE SEMANTIC IS DIFFERENT          *)
	      (********************************************)
	    Code_map.build_tuple_map_index_code code_ctxt algop vname
	| AOENullMap v ->
	    Code_map.build_null_map_code code_ctxt algop v 
          (*********************)
	  (* Group/order       *)
          (*********************)
	| AOEGroupBy gd_list ->
	   (* THIS IS AWFUL - we are passing the function inside the grouping code...*)
	   (* We do this to do code selection for the sort operands over sequences *)
	    Code_group_order.build_group_code code_ctxt algop (single_op_default_code_selection module_uri) gd_list 
	| AOEOrderBy (stablekind, sort_spec_list, gt_table) ->
	    Code_group_order.build_order_by_code code_ctxt algop (stablekind, sort_spec_list, gt_table)
          (*********************)
	  (* Update operations *)
          (*********************)
	| AOECopy ->
	    Code_update.build_copy_code code_ctxt algop 
	| AOEDelete ->
	    Code_update.build_delete_code code_ctxt algop 
	| AOEInsert insert_location ->
	    Code_update.build_insert_code code_ctxt algop insert_location
	| AOERename nsenv ->
	    Code_update.build_rename_code code_ctxt algop nsenv 
	| AOEReplace value_flag ->
	    Code_update.build_replace_code code_ctxt algop value_flag
	| AOESnap sm ->
	    Code_update.build_snap_code code_ctxt algop sm
	| AOESet (vn) ->
	    begin
	      try
		let imported_code_ctxt = get_imported_variable_context code_ctxt vn in
(*print_string ("Imported var"^(Namespace_names.prefixed_string_of_rqname vn)^"\n"); *)
  	        Code_functional_ops.build_set_code imported_code_ctxt algop vn
	      with
	      | Not_found -> 
(* print_string ("Local var"^(Namespace_names.prefixed_string_of_rqname vn)^"\n"); *)
		  Code_functional_ops.build_set_code code_ctxt algop vn
	    end
	| AOEImperativeSeq ->
	    Code_constructors.build_imperative_seq_code code_ctxt algop 
          (********************)
	  (* XPath evaluation *)
          (********************)
	| AOEParse uri ->
	    Code_parse.build_parse_code code_ctxt algop uri 
	| AOETreeJoin (axis, anode_test) ->
	    Code_treejoin.build_treejoin_code code_ctxt algop (axis, anode_test)
	| AOETupleTreePattern (input, pattern) ->
	    Code_tuple_tree_pattern.build_tuple_tree_pattern_code code_ctxt algop (input, pattern)
	| AOEProject fields ->
	    let fn = fun () eval alg_ctxt input_cursor -> input_cursor in
	    (coerce_unitdep fn () coerce_unary_tuple_cursor_to_tuple_cursor), code_ctxt
          (*********)
	  (* DXQ   *)
          (*********)
	| AOEServerImplements (ncname,uri) ->
	    Code_execute.build_server_implements_code code_ctxt algop (ncname, uri) 
	| AOEExecute (ncname,uri) ->
	    Code_execute.build_execute_code code_ctxt algop (false,ncname,uri)
	| AOEASyncExecute (ncname,uri) ->
	    Code_execute.build_execute_code code_ctxt algop (true,ncname,uri)
	| AOEForServerClose (ncname,uri) ->
	    Code_execute.build_server_close_code code_ctxt algop (ncname, uri) 
	| AOEEvalClosure ->
	    Code_execute.build_eval_closure_code module_uri code_ctxt algop
      in
      algop.palgop_expr_eval := eval_code;
      ret_ctxt
    end
  with
  | exn -> raise (Error.error_with_file_location algop.palgop_expr_loc exn)

(*

   Order matters!

   For example the concat operation. Conceptually we must 
   evaluate the outer  most operation first.
   so $x ++ $y -> eval($y), eval($x), eval(++)2
   For variable hiding (see ConcatTuple)
   - chris

*)  

and sub_expr_default_code_selection module_uri code_ctxt sub_exprs =
  match sub_exprs with
    | NoSub -> code_ctxt
    | OneSub algop1 -> default_code_selection module_uri code_ctxt algop1
    | TwoSub (algop1,algop2) ->
	let code_ctxt = default_code_selection module_uri code_ctxt algop2 in
	default_code_selection module_uri code_ctxt algop1 
    | ManySub algops ->
	let alg_list = Array.to_list algops in
	  (* Right folding here as well *)
	List.fold_right (fun op ctxt ->
	  default_code_selection module_uri ctxt op) 
	  alg_list code_ctxt

and default_code_selection module_uri code_ctxt algop =
  let sub_exprs = algop.psub_expression in
  let dep_sub_exprs = algop.pdep_sub_expression in
    begin
      (* We must walk the try in order now *)
      let enter_info = enter_scope code_ctxt in 

      (* set the annotations for materialization of output of suboperators*)
      let _ = annotate_materialization code_ctxt algop in 

      (* Should store the variables here, since any declared in the indep
	 expression, should only the variables currently in the context
	 should be visible *)
      (* Also should save here the tuples that are visible to the parent
	 since if this is an unwrap operation, only those tuples should
	 be visible.
      *)
      let in_remote = get_in_remote_execute_operator code_ctxt in
      let _ = 
	match algop.palgop_expr_name with
	  AOEForServerClose _
	| AOEExecute _
	| AOEASyncExecute _ -> set_in_remote_execute_operator code_ctxt true
	| _ -> ()
      in	  

      (* Do code selection for Independent Sub-expressions *)
      let code_ctxt = sub_expr_default_code_selection module_uri code_ctxt sub_exprs in
      set_in_remote_execute_operator code_ctxt in_remote;

      (* Variable scoping *)

      (* As above, after walking, we need to remove whatever variables were defined *)
      let code_ctxt = restore_variables enter_info code_ctxt in 

      (* Do code selection for this operator *)
      let code_ctxt = single_op_default_code_selection module_uri code_ctxt algop in        

      (* Do code selection for Dedependent Sub-expressions *)
      let code_ctxt = sub_expr_default_code_selection module_uri code_ctxt dep_sub_exprs in

      let code_ctxt = store_annotation code_ctxt algop.compile_annotations in

      let _ = materialize_if_needed algop code_ctxt in
	    
      (* Here we should be removing any variables defined (say by this op)
	 - and if it is a tuple unwrap call, we need to restore the scoping
	 of the tuples from above.
      *)
let _ = 
match algop.palgop_expr_name with
	  AOEExecute _
	| AOEASyncExecute _ -> Debug.print_default_debug("In code selection : "^(Print_xquery_algebra.bprintf_physical_algstatement "" algop))
	| _ -> ()
in	  

      let code_ctxt = exit_scope (should_flush_tuples algop) enter_info code_ctxt in 
	code_ctxt
    end

and materialize_if_needed algop code_ctxt = 
  let add_materialization_to_code eval code = 
    let mat_f =  build_materialize_table_code algop code_ctxt eval in
      match code with 
        | AOECUnit eu1 -> AOECUnit 
            ( fun algctxt () -> 
              let r = eu1 algctxt () in
                physical_value_of_tuple_cursor
                  (mat_f algctxt (tuple_cursor_of_physical_value r))
            )
        | AOECUnary eu2 -> AOECUnary
            ( fun algctxt v -> 
              let r = eu2 algctxt v in
                physical_value_of_tuple_cursor
                  (mat_f algctxt (tuple_cursor_of_physical_value r))
            )            
        | AOECBinary eb -> AOECBinary
            ( fun algctxt v1 v2 -> 
              let r = eb algctxt v1 v2 in
                physical_value_of_tuple_cursor
                  (mat_f algctxt (tuple_cursor_of_physical_value r))
            )            
        | AOECMany em -> AOECMany
            ( fun algctxt varray -> 
              let r = em algctxt varray in
                physical_value_of_tuple_cursor
                  (mat_f algctxt (tuple_cursor_of_physical_value r))
            )                        
  in
  let add_materialization_to_code_dep code_dep = 
    if Debug.materialization_debug() then
      Debug.print_materialization_debug ("Code selected to materialize output of operator " ^ 
                                            (string_of_algop_expr_name algop.palgop_expr_name) ^ "\n");    
    match code_dep with
        NoDep (funcdep, tailcode) -> NoDep ((fun eval -> add_materialization_to_code eval (funcdep eval)), tailcode)
      | SomeDep (funcdep, tailcode) -> SomeDep ((fun eval -> add_materialization_to_code eval (funcdep eval)), tailcode)
  in
  if produces_a_table algop && should_materialize algop then    
    begin
      algop.palgop_expr_eval := add_materialization_to_code_dep !(algop.palgop_expr_eval);
      ()
    end
