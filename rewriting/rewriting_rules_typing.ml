(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: rewriting_rules_typing.ml,v 1.39 2008/03/25 01:47:28 mff Exp $ *)

(* Module: Rewriting_rules_typing
   Description:
     This module contains the rewriting rules that rely on typing
     information.
*)

open Error

open Namespace_names
open Namespace_builtin

open Datatypes

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

open Xquery_type_core_ast_util
open Schema_builtin
open Schema_util
open Subtyping_top

open Print_top

let node_star_or_atomic_star_type = (make_choice_cxtypes cxtype_node_star cxtype_anyAtomic_star)
(* 
  subtype_check : 

  If strong typing is enabled, then all sub-type checks are redundant
  and trivially return true.

  If weak typing is enabled, then we have to check explicitly that the
  actual type is a subtype of the expected.
*)

let subtype_check rewrite_ctxt actual expected = 
  let stat_ctxt = get_context rewrite_ctxt in
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  let schema = Norm_context.cxschema_from_norm_context norm_ctxt in
  match proc_ctxt.typing_kind with 
  | Typing_Strong -> true
  | _ -> is_subtype_of schema actual expected

(* 
  For any function call whose argument type matches its formal type, the
  dynamic type assertion on the actual argument is removed.
*)
	
let typesafe_call_rewrite rewrite_ctxt ce =
  let ah = ce.pcexpr_annot in
  let eh = ce.pcexpr_origin in
  let loc = ce.pcexpr_loc in
  match ce.pcexpr_desc with
  | CECall (fname, arguments, (input_types, output_type), upd, selfrecur) ->
      let type_check_one_argument expr opt_type = 
	begin
	  match opt_type with
	  | None -> None
	  | Some (_, t2) ->
	      let t1 = get_type_annotation_from_cexpr expr in 
	      if (subtype_check rewrite_ctxt t1 t2) then None
	      else opt_type
	end
      in 
      let some_types = List.filter Gmisc.is_some input_types in
      if (List.length some_types > 0) then
	let input_types' = List.map2 type_check_one_argument arguments input_types in 
	let some_types' = List.filter Gmisc.is_some input_types' in
	if (List.length some_types = List.length some_types') then 
	  raise Not_applied
	else
	  (fmkacexpr (CECall (fname, arguments, (input_types', output_type), upd, selfrecur)) ah eh loc, true)
      else
	raise Not_applied
  | _ -> raise Not_applied

let boolean_cdt_cxtype fi = (fmkcsequencetype (CITAtomic Namespace_builtin.xs_boolean,None) fi, cxtype_boolean)
let float_cdt_cxtype fi = (fmkcsequencetype (CITAtomic Namespace_builtin.xs_float,None) fi, cxtype_float)
let double_cdt_cxtype fi = (fmkcsequencetype (CITAtomic Namespace_builtin.xs_double,None) fi, cxtype_double)
let integer_cdt_cxtype fi = (fmkcsequencetype (CITAtomic Namespace_builtin.xs_integer,None) fi, cxtype_integer)
let string_cdt_cxtype fi =  (fmkcsequencetype (CITAtomic Namespace_builtin.xs_string,None) fi, cxtype_string)

let double_opt_cdt_cxtype fi = (fmkcsequencetype (CITAtomic Namespace_builtin.xs_double,Some Occurrence.optional) fi, cxtype_double_optional)
let integer_opt_cdt_cxtype fi = (fmkcsequencetype (CITAtomic Namespace_builtin.xs_integer,Some Occurrence.optional) fi, cxtype_integer_optional)
let string_opt_cdt_cxtype fi = (fmkcsequencetype (CITAtomic Namespace_builtin.xs_string,Some Occurrence.optional) fi, cxtype_string_optional)

(*
   for $v in Expr1         for $v in Expr1 	 
   FLWR_CLAUSES 	       FLWR_CLAUSES 	 
   return Expr2		       return Expr2		 
   			       
   Expr1 cannot fail	   Expr1 : Type1	     
   Expr1 : Type1	   Type1 <: Item	     
   Type1 <: ()		   ==>
   ==>	                   let $v := Expr1 	     
   ()			   FLWR_CLAUSES 	     
                           return Expr2   

   Under strong typing, all type assertions are removed.  But for
   simplicity, we have one rule for both strong & weak typing that
   does the subtype check anyway.

      for $v as Type in Expr1
      Expr1 : Type1 		
      prime(Type1) <: Type	
      ==>
      for $v in Expr1 return
*)

(* Function to apply for simplification *)

let for_clause_type_rewrite (rewrite_ctxt) (cfl_clause_list) (cfl_clause) (ce) =
  let ah = ce.pcexpr_annot in
  let eh = ce.pcexpr_origin in
  let loc = ce.pcexpr_loc in
  let stat_ctxt = get_context rewrite_ctxt in
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  let schema = Norm_context.cxschema_from_norm_context norm_ctxt in
    match cfl_clause with 
      | CEFOR (odt, vname, oposvar, cexpr1) ->
          (* Note: Here cexpr1 should not fail for first rule to be applicable; *)
          let ctype = get_type_annotation_from_cexpr cexpr1 in
            if (is_empty_cxtype ctype && not(can_fail proc_ctxt cexpr1) &&
                  (side_effect_free_judge cexpr1)) then 
              ([], fmkacexpr (CEEmpty) ah eh loc, true)
	            (* This is an explicit subtype check, because the input-type could be any type *)
            else if (is_subtype_of schema ctype cxtype_item) then 
	          let posvar_letclause = 
	            begin
	              match oposvar with
	                | None -> []
	                | Some posvar -> 
		                let annot = Xquery_core_ast_annotation.empty_ast_annot() in
		                  (set_type_annot annot cxtype_integer;
		                   [CELET(None, posvar, (fmkacexpr (CEScalar (IntegerLiteral (Big_int.big_int_of_int  1))) annot eh loc))])
	            end
	          in (cfl_clause_list @ [CELET (odt, vname, cexpr1)] @ posvar_letclause, ce, true)
            else
	          begin 
	            match odt with 
	              | None -> (cfl_clause_list@[cfl_clause], ce, false)
	              | Some (_, declt) ->
                      let ctype = get_type_annotation_from_cexpr cexpr1 in
	                  let (p1, _, _) = factor ctype in
	                    if (subtype_check rewrite_ctxt p1 declt) then 
		                  (cfl_clause_list @ [CEFOR (None, vname, oposvar, cexpr1)], ce, true)
	                    else 
		                  (cfl_clause_list@[cfl_clause], ce, false)
	          end
      | _ -> (cfl_clause_list@[cfl_clause], ce, false)
      
(*
   Under strong typing, all type assertions are removed: subtype_check
   determines whether strong or weak typing is in effect and applies
   subtype check accordingly.

      let $v as Type := Expr1
      Expr1 : Type1
      Type1 <: Type
      ==>
      let $v := Expr1
*)
let let_clause_type_rewrite (rewrite_ctxt) (cfl_clause_list) (cfl_clause) (ce) =
  match cfl_clause with 
  | CELET (Some (_, declt), vname, cexpr1) ->
      let ctype = get_type_annotation_from_cexpr cexpr1 in
      if (subtype_check rewrite_ctxt ctype declt) then 
	(cfl_clause_list @ [CELET (None, vname, cexpr1)], ce, true)
      else (cfl_clause_list@[cfl_clause], ce, false)
  | _ ->
      (cfl_clause_list@[cfl_clause], ce, false)

(*
   Removing existential/universal quantification 

   some $v in Expr1 return Expr2         some $v in Expr1 satisfies Expr2    
   Expr1 cannot fail                     Expr1 : Type1		       
   Expr1 <: ()                           Type1 <: item		       
   -----------------------------         ----------------------------	       
   false                                 let $v := Expr1 return Expr2        
   
   every $v in Expr1 return Expr2        every $v in Expr1 satisfies Expr2 
   Expr1 cannot fail                     Expr1 : Type1		     
   Expr1 <: ()                           Type1 <: item		     
   -----------------------------	     ----------------------------	     
   true                                  let $v := Expr1 return Expr2      
*)

let quantified_type_rewrite (rewrite_ctxt) (ce) =
  let ah = ce.pcexpr_annot in
  let eh = ce.pcexpr_origin in
  let loc = ce.pcexpr_loc in
  let stat_ctxt = get_context rewrite_ctxt in
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  let (ce', changed) = 
    match ce.pcexpr_desc with 
    | CESome (odt, vname, cexpr1, cexpr2) ->
	let cexpr1_type = get_type_annotation_from_cexpr cexpr1 in
        (* This is an explicit subtype check, because the input-type could be any type *)
	(* We don't want to turn some into lets until we can deal with lets at Join detection level -- Jerome
	if (is_subtype_of schema cexpr1_type cxtype_item) then 
	  (make_let_flwor [CELET (odt, vname, cexpr1)] cexpr2 ah eh loc, true)
	
	else  *)
	if (not(can_fail proc_ctxt cexpr1) &&
		 is_empty_cxtype cexpr1_type &&
         side_effect_free_judge cexpr1) then
	  (fmkacexpr (CEScalar (BooleanLiteral false)) ah eh loc, true)
	else raise Not_applied
    | CEEvery (odt, vname, cexpr1, cexpr2) ->
	let cexpr1_type = get_type_annotation_from_cexpr cexpr1 in
        (* This is an explicit subtype check, because the input-type could be any type *)
	(* We don't want to turn some into lets until we can deal with lets at Join detection level -- Jerome
	  if (is_subtype_of schema cexpr1_type cxtype_item) then
	  (make_let_flwor [CELET (odt, vname, cexpr1)] cexpr2 ah eh loc, true)
	else
	 *)
	if (not(can_fail proc_ctxt cexpr1) &&
		 is_empty_cxtype cexpr1_type &&
         side_effect_free_judge cexpr1) then
	  (fmkacexpr (CEScalar (BooleanLiteral true)) ah eh loc, true)
	else raise Not_applied
    | _ -> raise Not_applied
  in 
  (ce', changed)

(* 
  Expr cast as Type'
  Expr : Type
  Type = Type'
  -----------------
  Expr 
*)

let cast_type_rewrite rewrite_ctxt ce =
  match ce.pcexpr_desc with
  | CECast (cexpr, _, (_, ctype')) -> 
      let ctype = get_type_annotation_from_cexpr cexpr in
      if (ctype = ctype') then (cexpr, true)
      else raise Not_applied
  | _ -> raise Not_applied

(*
  Dynamic dispatch to static dispatch optimization 

  overloaded_type_rewrite replaces an overloaded, dynamically dispatched
  function call by a statically dispatched function call.

  statEnv |- QName => qname
  statEnv.funcType(qname) = { FuncDecl1, ..., FuncDeclm }
  FuncDecli = declare function qname'(Type1, ..., Typen) as Type
  statEnv |- Expr1 : Type1'     Type1' <: Type1''     Type1'' can be promoted to Type1
  ...
  statEnv |- Exprn : Typen'     Typen' <: Typen''     Typen'' can be promoted to Typen

  QName (Expr1, ..., Exprn) 
  ------------------------------------------------------------------------------------
  qname'(promote-to-Type(Expr1, Type1), ..., promote-to-Type(Exprn, Typen))

*)
let overloaded_type_rewrite (rewrite_ctxt) (ce) =
  let ah = ce.pcexpr_annot in
  let eh = ce.pcexpr_origin in
  let loc = ce.pcexpr_loc in
  let stat_ctxt = get_context rewrite_ctxt in
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  let schema = Norm_context.cxschema_from_norm_context norm_ctxt in
  let numeric_sequence_type = fmkcsequencetype (CITNumeric, None) loc in
  (* The logic in this function should been the same as that in static
     typing of overloaded functions *)
  match ce.pcexpr_desc with
  | CEOverloadedCall (fn, arguments, overloaded_signature_table) ->
      let argument_types = List.map (fun a -> get_type_annotation_from_cexpr a) arguments in
      (* This logic follows that for static typing in
	 Typing_call.compute_type_overloaded_function_call *)
      let (fn', argument_types', output_type', upd) =
	Typing_call.match_overloaded_function_signature stat_ctxt fn argument_types overloaded_signature_table
      in    
      if (not(Namespace_names.rqname_equal fn fn'))
      then
	(* When converting from an overloaded call to a non-overloaded
           call, it is necessary to explicitly inject promotions that
           the overloaded call would not evaluate until evaluation
           time. 
	*)
	begin
	  let arguments' =
	    List.map
	      (fun (form_type, ce) ->
		let (p,m,n) = factor form_type in
		(* If actual type is subtype of formal type, we're done *)
		if (is_subtype_of schema (get_type_annotation_from_cexpr ce) form_type) then ce
		else if (is_subtype_of_anynumeric schema p) then
		  begin
		    let proto_value = fmkacexpr (CEProtoValue (Schema_judge.atomic_type_of_cxtype schema p)) ah eh loc in
		    fmkacexpr (CECall(fs_promote_to_numeric, [ce; proto_value], 
				      ([None; None], (numeric_sequence_type, cxtype_numeric)), upd, false)) ah eh loc
		  end
		else if (is_subtype_of_anystring schema p) then
		  let nsenv  = Norm_context.nsenv_from_norm_context norm_ctxt in
		  let (vname,cve) = Norm_context.gen_new_cvar_typed norm_ctxt ah eh loc in
  		  let cast = fmkacexpr (CECast(cve, nsenv, string_cdt_cxtype loc)) ah eh loc in
		  make_for_flwor [CEFOR (None, vname, None, ce)] cast ah eh loc
		else
		  ce)
	      (List.combine argument_types' arguments)
	  in
	  let optinput_types = List.map (fun _ -> None) arguments in 
	  (* Mary: ??? This is a hack --- we don't have the original
	     sequence type for the output type but it shouldn't matter *)
	  (fmkacexpr (CECall(fn', arguments', (optinput_types, (numeric_sequence_type, output_type')), upd, false)) ah eh loc, true)
	end
      else 
	begin
	  raise Not_applied
	end
  | _ ->
      raise Not_applied

(*


   fn:boolean(Expr)     fn:boolean(Expr)      fn:boolean(Expr)
   Expr : ()	        Expr : boolean	      Expr : Type	   
   ----------------     ----------------      Type <: node+   
   false()	        Expr                  ----------------   
					      true()          

   It's not worth applying the rule below, as the "NaN" value must be
   treated differently than other float/double values, c.f.
   Code_fn._fn_boolean().  No point in replicating that special case
   here.

   fn:boolean(Expr)   
   Expr : Type	       
   Type <: numeric    
   ---------------------------   
   not(Expr = 0 or Expr = NaN)    
*)

let atomicFalse = BooleanLiteral false
let atomicTrue = BooleanLiteral true

let fn_boolean_type_rewrite rewrite_ctxt ce arguments =
  let ah = ce.pcexpr_annot in
  let eh = ce.pcexpr_origin in
  let loc = ce.pcexpr_loc in
  let stat_ctxt = get_context rewrite_ctxt in
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  let schema = Norm_context.cxschema_from_norm_context norm_ctxt in
  let false_expr = fmkacexpr (CEScalar atomicFalse) ah eh loc in
  let true_expr = fmkacexpr (CEScalar(atomicTrue)) ah eh loc in
  let cexpr = List.hd arguments in 
  let ctype = get_type_annotation_from_cexpr cexpr in
  if (is_subtype_of_empty_sequence schema ctype && side_effect_free_judge cexpr) then
    (false_expr, true)
  else if (is_subtype_of schema ctype cxtype_boolean) then
    (cexpr, true)
  else if (is_subtype_of schema ctype cxtype_node_plus && not(can_fail proc_ctxt cexpr)
           && side_effect_free_judge cexpr) then
    (true_expr, true)
(* SKIP THIS ONE -- requires special treatment of NaN
  else if (is_subtype_of schema ctype cxtype_numeric) then
    let double_annot = Xquery_core_ast_annotation.empty_ast_annot() in
    set_type_annot double_annot cxtype_double;
    let string_annot = Xquery_core_ast_annotation.empty_ast_annot() in
    set_type_annot string_annot cxtype_string;
    let zero_expr = fmkacexpr (CEScalar (DoubleLiteral 0.0)) double_annot eh loc in 
    let nan_expr = fmkacexpr (CECast (fmkacexpr (CEScalar (StringLiteral "NaN")) string_annot eh loc, nsenv, double_cdt_cxtype loc)) double_annot eh loc in 
    let sig_table = Norm_overloaded.table_for_overloaded_function norm_ctxt (op_equal,2) in
    let eq_expr1 = fmkacexpr (CEOverloadedCall(op_equal, [ cexpr; zero_expr], sig_table)) ah eh loc in
    let eq_expr2 = fmkacexpr (CEOverloadedCall(op_equal, [ cexpr; nan_expr], sig_table)) ah eh loc in
    let eq_expr =  fmkacexpr (CEIf (eq_expr1,  true_expr, eq_expr2)) ah eh loc in 
    (fmkacexpr (CECall (fn_not, [ eq_expr ], ([None], boolean_cdt_cxtype loc))) ah eh loc, true)
*)
  else
    raise Not_applied

(*
   fn:data function simplification 

   fn:data(Expr)
   Expr : Type
   Type <: xs:anySimpleType
   ------------------------
   Expr

*)
let fn_data_type_rewrite rewrite_ctxt ce arguments = 
  let stat_ctxt = get_context rewrite_ctxt in
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  let schema = Norm_context.cxschema_from_norm_context norm_ctxt in
  let cexpr = List.hd arguments in 
  (* We don't have to check for failure here, because expression is always evaluated *)
  let cexpr_type = get_type_annotation_from_cexpr cexpr in
  if (is_subtype_of_anyatomic_sequence schema cexpr_type) then (cexpr, true)
  else raise Not_applied

(*
   fn:exactly-one(Expr)
   Expr : Type
   Type <: item
   --------------------
   Expr
*)
let fn_exactly_one_rewrite rewrite_ctxt ce arguments = 
  let cexpr = List.hd arguments in 
  let ctype = get_type_annotation_from_cexpr cexpr in
  if (subtype_check rewrite_ctxt ctype cxtype_item) then (cexpr, true)
  else raise Not_applied

(*
   fn:one-or-more(Expr)
   Expr : Type
   Type <: item+
   --------------------
   Expr
*)
let fn_one_or_more_rewrite rewrite_ctxt ce arguments = 
  let cexpr = List.hd arguments in 
  let ctype = get_type_annotation_from_cexpr cexpr in
  if (subtype_check rewrite_ctxt ctype cxtype_item_plus) then (cexpr, true)
  else raise Not_applied

(*
   fn:zero-or-one(Expr)
   Expr : Type
   Type <: item?
   --------------------
   Expr
*)
let fn_zero_or_one_rewrite rewrite_ctxt ce arguments = 
  let cexpr = List.hd arguments in 
  let ctype = get_type_annotation_from_cexpr cexpr in
  if (subtype_check rewrite_ctxt ctype cxtype_item_optional) then (cexpr, true)
  else raise Not_applied

(*
   fs_untyped_to_atomic_type_rewrite implements following rewrite
   rules for functions fs:untyped_to_{double,string,integer}.

   fs:untyped_to_double(Expr)             fs:untyped_to_double(Expr)  
   Expr : Type			          Expr : Type		       
   not(xs:untypedAtomic <: Type)         Type <: xs:untypedAtomic ? 
   ------------------------	          ------------------------    
   Expr				          (Expr) cast as xs:double    

   fs:untyped_to_integer(Expr)            fs:untyped_to_integer(Expr)  
   Expr : Type			          Expr : Type		       
   not(xs:untypedAtomic <: Type)         Type <: xs:untypedAtomic ? 
   ------------------------	          ------------------------    
   Expr				          (Expr) cast as xs:integer    

   fs:untyped_to_string(Expr)             fs:untyped_to_string(Expr)  
   Expr : Type			          Expr : Type		       
   not(xs:untypedAtomic <: Type)         Type <: xs:untypedAtomic ? 
   ------------------------	          ------------------------    
   Expr				          (Expr) cast as xs:string    
*)

let fs_untyped_to_atomic_type_rewrite (rewrite_ctxt) (ce) =
  let ah = ce.pcexpr_annot in
  let eh = ce.pcexpr_origin in
  let loc = ce.pcexpr_loc in
  let stat_ctxt = get_context rewrite_ctxt in
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  let nsenv  = Norm_context.nsenv_from_norm_context norm_ctxt in
  let schema = Norm_context.cxschema_from_norm_context norm_ctxt in
  match ce.pcexpr_desc with
  | CECall (fname, arguments, sign, upd, selfrecur) ->
      let target_type =
	if (fname = fs_untyped_to_double) then double_opt_cdt_cxtype loc
	else if (fname = fs_untyped_to_integer) then integer_opt_cdt_cxtype loc
	else if (fname = fs_untyped_to_string) then string_opt_cdt_cxtype loc
	else raise Not_applied
      in
      begin
	match arguments with
	| cexpr :: [] -> 
	    begin
	      let cexpr_type = get_type_annotation_from_cexpr cexpr in
	      if not(is_subtype_of schema cxtype_untypedAtomic cexpr_type) then (cexpr, true)
	      else if (is_subtype_of schema cexpr_type cxtype_untypedAtomic_optional) then 
		(fmkacexpr (CECast(cexpr, nsenv, target_type)) ah eh loc, true)
	      else raise Not_applied
	    end
	| _ -> raise (Norm_util.incorrect_arg_count fname (List.length arguments) 1)
      end
  | _ -> raise Not_applied

(*

   fs:untyped_to_any(Expr1, Expr2)             fs:untyped_to_any(Expr1, Expr2)		
   Expr1 : Type1			       Expr1 : Type1				
   not(xs:untypedAtomic <: Type1)	       Type1 <: xs:untypedAtomic 			
   ---------------------------------------     Expr2 : Type2				
   Expr1				       Type2 <: xs:untypedAtomic | xsd:string	
					       ---------------------------------------	
					       Expr cast as xsd:string                      


   fs:untyped_to_any(Expr1, Expr2)                fs:untyped_to_any(Expr1, Expr2)	      
   Expr1 : Type1			          Expr1 : Type1			      
   Type1 <: xs:untypedAtomic 		          Type1 <: xs:untypedAtomic 		      
   Expr2 : Type2			          Expr2 : Type2			      
   Type2 <: numeric			          Type2 <: xs:anyAtomicType		      
   ---------------------------------------        ---------------------------------------    
   Expr1 cast as xsd:double		          Expr cast as Type2                         

*)

let fs_untyped_to_any_type_rewrite rewrite_ctxt ce arguments = 
  let ah = ce.pcexpr_annot in
  let eh = ce.pcexpr_origin in
  let loc = ce.pcexpr_loc in
  let stat_ctxt = get_context rewrite_ctxt in
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  let nsenv  = Norm_context.nsenv_from_norm_context norm_ctxt in
  let schema = Norm_context.cxschema_from_norm_context norm_ctxt in
  let cexpr1 = List.hd arguments in 
  let cexpr2 = List.hd (List.tl arguments) in 
  let ctype1 = get_type_annotation_from_cexpr cexpr1 in
  if not (is_subtype_of schema cxtype_untypedAtomic ctype1) then 
    (cexpr1, true)
  else if (is_subtype_of schema ctype1 cxtype_untypedAtomic) then 
    let ctype2 = get_type_annotation_from_cexpr cexpr2 in
    let untyped_or_string_type = make_choice_cxtypes cxtype_untypedAtomic cxtype_string in
    if (is_subtype_of_anynumeric schema ctype2) then
      (fmkacexpr (CECast(cexpr1, nsenv, double_cdt_cxtype loc)) ah eh loc, true)
    else if (is_subtype_of schema ctype2 untyped_or_string_type) then 
      (fmkacexpr (CECast(cexpr1, nsenv, string_cdt_cxtype loc)) ah eh loc, true)
(*
   ??? Mary: This is really annoying.  We have to reverse engineer the sequence_type from the atomic type,
   which is a pain, so we can't apply the rule. 
   else if (is_subtype_of schema ctype2 cxtype_anyAtomic) then
   (fmkacexpr (CECast(cexpr1, ctype2)) ah eh loc, true)
*)
    else
      raise Not_applied
  else
    raise Not_applied


(*
  fs:convert-simple-operand(Expr, _)
  Expr : Type 
  not (xs:untypedAtomic <: prime(Type))
  -------------------------------------
  Expr
*)

let fs_convert_simple_operand_type_rewrite rewrite_ctxt ce arguments =
  let cexpr1 = List.hd arguments in
  let stat_ctxt = get_context rewrite_ctxt in
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  let schema = Norm_context.cxschema_from_norm_context norm_ctxt in
  let (p,_,_) = factor (get_type_annotation_from_cexpr cexpr1) in
  if (not(is_subtype_of schema cxtype_untypedAtomic p)) then 
    (cexpr1, true)
  else raise Not_applied

(*
  fs:distinct_docorder_or_atomic_sequence(Expr)   fs:distinct_docorder_or_atomic_sequence(Expr)	
  Expr : Type 					  Expr : Type 					
  Type <: xs:anyAtomicType*			  Type <: node()*				
  -------------------------------------		  -------------------------------------		
  (Expr)					  fs:distinct_docorder(Expr)                    

*)
let fs_distinct_docorder_or_atomic_rewrite rewrite_ctxt ce arguments =
  let ah = ce.pcexpr_annot in
  let eh = ce.pcexpr_origin in
  let loc = ce.pcexpr_loc in
  let stat_ctxt = get_context rewrite_ctxt in
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  let schema = Norm_context.cxschema_from_norm_context norm_ctxt in
  let cexpr = List.hd arguments in
  let ctype = get_type_annotation_from_cexpr cexpr in
  (* Input type must be either xs:anyAtomicType* or node()* *)
  if (is_subtype_of schema ctype cxtype_anyAtomic_star) then 
    (cexpr, true)
  else if (subtype_check rewrite_ctxt ctype cxtype_node_star) then 
    let (input_types, output_type), opt_fun_kind, upd = 
      Norm_context.one_sig_from_norm_context norm_ctxt (fs_distinct_docorder, 1) in
    let input_types' = List.map (fun _ -> None) input_types in
	(fmkacexpr (CECall(fs_distinct_docorder, arguments, (input_types', output_type), upd, false)) ah eh loc, true)
  else raise Not_applied

(*
  fs:node_sequence_or_atomic_sequence(Expr)       fs:node_sequence_or_atomic_sequence(Expr)	
  Expr : Type 					  Expr : Type 					
  Type <: xs:anyAtomicType*			  Type <: node()*				
  -------------------------------------		  -------------------------------------		
  (Expr)					  (Expr)                    
*)
let fs_node_sequence_or_atomic_rewrite rewrite_ctxt ce arguments =
  let cexpr = List.hd arguments in
  let ctype = get_type_annotation_from_cexpr cexpr in
  if (subtype_check rewrite_ctxt ctype node_star_or_atomic_star_type) then 
    (cexpr, true)
  else raise Not_applied

(*
  fs:node_sequence(Expr)
  Expr : Type 		
  Type <: node()*
  ----------------------
  (Expr)		
*)
let fs_node_sequence_rewrite rewrite_ctxt ce arguments =
  let cexpr = List.hd arguments in
  let ctype = get_type_annotation_from_cexpr cexpr in
  if (subtype_check rewrite_ctxt ctype cxtype_node_star) then 
    (cexpr, true)
  else raise Not_applied

(* Added by Philippe
 
  fs:distinct-doc-order(Expr)
  Expr : Type
  Type <: node()
  ---------------------------
  Expr 
*)
let sbdo_singleton_rewrite rewrite_ctxt ce arguments =
  let cexpr = List.hd arguments in
  let ctype = get_type_annotation_from_cexpr cexpr in
  if (subtype_check rewrite_ctxt ctype cxtype_item_optional) then 
    (cexpr, true)
  else raise Not_applied
  
(*

   Under strong typing, all type assertions are removed: subtype_check
   determines whether strong or weak typing is in effect and applies
   subtype check accordingly.

   some $v as Type in Expr1 satisfies Expr2    every $v as Type in Expr1 satisfies Expr2	   
   Expr1 : Type1                               Expr1 : Type1 				   
   prime(Type1) <: Type	                       prime(Type1) <: Type			   
   =================================	       =================================		   
   some $v in Expr1 satisfies Expr2            every $v in Expr1 satisfies Expr2          
*)
let quantified_type_assert_rewrite (rewrite_ctxt) (ce) =
  let ah = ce.pcexpr_annot in
  let eh = ce.pcexpr_origin in
  let loc = ce.pcexpr_loc in
  match ce.pcexpr_desc with
  | CESome (Some (_, declt), vname, cexpr1, cexpr2) -> 
      let ctype1 = get_type_annotation_from_cexpr cexpr1 in
      let (p1, _, _) = factor ctype1 in
      if (subtype_check rewrite_ctxt p1 declt) then 
	(fmkacexpr (CESome (None, vname, cexpr1, cexpr2)) ah eh loc, true)
      else raise Not_applied
  | CEEvery (Some (_, declt), vname, cexpr1, cexpr2) -> 
      let ctype1 = get_type_annotation_from_cexpr cexpr1 in
      let (p1, _, _) = factor ctype1 in
      if (subtype_check rewrite_ctxt p1 declt) then 
	(fmkacexpr (CEEvery (None, vname, cexpr1, cexpr2)) ah eh loc, true)
      else raise Not_applied
  | _ -> raise Not_applied

(***************************)
(* TYPESWITCH OPTIMIZATION *)
(***************************)

(*
   Rule 1. Remove all empty case clauses:

   typeswitch Expr_0
   ...
   case Type_k $vk return Expr_k
   ...
   default $vd return Expr_n
   and 
   Expr_0 : Type_0  
   and
   Type_k intersect Type_0 = None
   ----------------------------------------
   typeswitch Expr_0 
   ...
   case Type_(k-1) $v(k-1) return Expr_(k-1)
   case Type_(k+1) $v(k+1) return Expr_(k+2)
   ...
   default $vd return Expr_n
*)
let rec remove_empty_clauses rewrite_ctxt ctype0 cases =
  let stat_ctxt = get_context rewrite_ctxt in
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  let schema = Norm_context.cxschema_from_norm_context norm_ctxt in
  match cases with
  | [] -> ([],false)
  | (patternk, ovn, case_cexpr) :: remaining_cases ->
      begin
	match patternk.pcpattern_desc with
	| CCase (_, ctype) ->
	    let (new_cases1, changed) = remove_empty_clauses rewrite_ctxt ctype0 remaining_cases in
	    let res = 
	      if (intersects_with schema ctype0 ctype) then 
	      ((patternk, ovn, case_cexpr) :: new_cases1, changed)
	    else 
( (* print_string ("Removing case : empty intersection"); *)
	      (new_cases1, true))
	    in
	    res
	| CDefault ->
	    if remaining_cases = [] then
	      ([ (patternk, ovn, case_cexpr) ], false)
	    else raise (Query (Rewriting "Rewrite_rule : mal-formed typeswitch!!"))
      end

(* 
   Rule 2. Remove never-evaluated clauses: 

   typeswitch Expr_0 
   case $v1 as Type_1 return Expr_1
   ...
   case $vk as Type_k return Expr_k
   ...
   case $v(n-1) as Type_(n-1) return Expr_(n-1)
   default $vd return Expr_n
   and
   Type_0 <: (Type_1 | ... | Type_k)
   -----------------------------------------
   typeswitch Expr_0
   case $v1 as Type_1 return Expr_1
   ...
   case $v(k-1) as Type_(k-1) return Expr_(k-1)
   default $v(k) return Expr_k

*)
let rec remove_never_evaluated_clauses rewrite_ctxt ctype0 cases union_of_visited_types =
  let stat_ctxt = get_context rewrite_ctxt in
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  let schema = Norm_context.cxschema_from_norm_context norm_ctxt in
  match cases with
  | [] -> ([], false)
  | (patternk, ovn, case_cexpr) :: remaining_cases ->
      begin
	match patternk.pcpattern_desc with
	| CCase (_, ctype) ->
	    let bigger_union_type = make_choice_cxtypes ctype union_of_visited_types in
	    if not(is_subtype_of schema ctype0 bigger_union_type) then
	      let (new_cases1, changed) =
		remove_never_evaluated_clauses rewrite_ctxt ctype0 remaining_cases bigger_union_type
	      in
	      ((patternk, ovn, case_cexpr) :: new_cases1, changed)
	    else
((* print_string ("Removing never-evaluated clauses : not subtype"); *)
	      ([ (fmkcpattern CDefault patternk.pcpattern_loc, ovn, case_cexpr) ], true))
	| CDefault ->
	    if remaining_cases = []  then
	      ([ (patternk, ovn, case_cexpr) ], false)
	    else
	      raise (Query (Rewriting "Rewrite_rule : mal-formed typeswitch!!"))
      end

(* 
   Rules 1 & 2: See above
   Rule 3. Typeswitch elimination

   typeswitch Expr_0
   default $vd return Expr
   ------------------------------
   let $vd := Expr_0 return $Expr
*)
let typeswitch_rewrite rewrite_ctxt (ce) =
  let ah = ce.pcexpr_annot in
  let eh = ce.pcexpr_origin in
  let loc = ce.pcexpr_loc in
  let stat_ctxt = get_context rewrite_ctxt in
  let norm_ctxt = Typing_context.norm_context_from_stat_context stat_ctxt in
  match ce.pcexpr_desc with
  | CETypeswitch (cexpr0, cases) ->
      let ctype0 = get_type_annotation_from_cexpr cexpr0 in
      (* Initializes the union_of_visited_types to none() to apply Rule: 1 *)
      let (subtype_cases, changed) = (remove_empty_clauses rewrite_ctxt ctype0 cases) in
      let (subtype_cases', changed') = (remove_never_evaluated_clauses rewrite_ctxt ctype0 subtype_cases cxtype_none) in
      if (changed || changed') then 
	begin
	  match subtype_cases' with
	  | (_, ovn, case_expr) :: [] ->  (* Note: Really here it's only one case so it *MUST* work *)
	      let vname = 
		match ovn with
		| None -> let (vname, _) = Norm_context.gen_new_cvar norm_ctxt None loc in vname
		| Some vname -> vname
	      in
	      (make_let_flwor [CELET (None, vname, cexpr0)] case_expr ah eh loc, true)
	  | _ ->  (fmkacexpr (CETypeswitch (cexpr0, subtype_cases')) ah eh loc, changed || changed')
      end
	  else raise Not_applied
  | _ -> raise Not_applied

let any_typing_rule_set = 
  [ 

    Rewriting_rules_notyping.flwr_rewrite for_clause_type_rewrite let_clause_type_rewrite;
    Rewriting_rules_notyping.call_rewrite (fn_boolean, 1, fn_boolean_type_rewrite);  
    Rewriting_rules_notyping.call_rewrite (fn_data, 1, fn_data_type_rewrite);
    Rewriting_rules_notyping.call_rewrite (fn_exactly_one, 1, fn_exactly_one_rewrite);
    Rewriting_rules_notyping.call_rewrite (fn_one_or_more, 1, fn_one_or_more_rewrite); 
    Rewriting_rules_notyping.call_rewrite (fn_zero_or_one, 1, fn_zero_or_one_rewrite); 
    Rewriting_rules_notyping.call_rewrite (fs_convert_simple_operand, 2, fs_convert_simple_operand_type_rewrite);
    Rewriting_rules_notyping.call_rewrite (fs_distinct_docorder_or_atomic_sequence, 1, fs_distinct_docorder_or_atomic_rewrite);
    Rewriting_rules_notyping.call_rewrite (fs_node_sequence_or_atomic_sequence, 1, fs_node_sequence_or_atomic_rewrite);
    Rewriting_rules_notyping.call_rewrite (fs_node_sequence, 1, fs_node_sequence_rewrite);
(*    Rewriting_rules_notyping.call_rewrite (fs_distinct_docorder, 1, sbdo_singleton_rewrite); 
-- temporarily disabled for TPNF rewrites -- Philippe *)
    fs_untyped_to_atomic_type_rewrite; 
    Rewriting_rules_notyping.call_rewrite (fs_untyped_to_any, 2, fs_untyped_to_any_type_rewrite);
(*    Rewriting_rules_notyping.call_rewrite (fs_promote_to_numeric, 2, fs_promote_to_numeric_rewrite);   *)

    cast_type_rewrite;  
    overloaded_type_rewrite;
    quantified_type_rewrite;
    quantified_type_assert_rewrite;
    typesafe_call_rewrite;
    typeswitch_rewrite; 
  ]

(*****************

  All of these functions have special typing rules and therefore are
  likely to have an associated rewriting rule.  Double-check all of
  them.

  ((fs_promote_to_numeric), 2), _fs_promote_to_numeric;

        7.1.5 The fs:item-sequence-to-node-sequence function
        7.1.6 The fs:item-sequence-to-untypedAtomic function
        7.1.7 The fs:item-sequence-to-untypedAtomic-PI function
        7.1.8 The fs:item-sequence-to-untypedAtomic-text function
        7.1.9 The fs:item-sequence-to-untypedAtomic-comment function        
        7.1.10 The fs:apply-ordering-mode function

   MISSING SPECIAL TYPE RULES:
        7.2.5 The fn:collection and fn:doc functions
        7.2.8 The fn:unordered function
******************)

