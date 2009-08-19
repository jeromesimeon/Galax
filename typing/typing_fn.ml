(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_fn.ml,v 1.23 2007/02/01 22:08:55 simeon Exp $ *)

(* Module: Typing_fn
   Description:
     This module implements static typing for F&O functions.
*)

open Format
open Error

open Namespace_names
open Namespace_util
open Namespace_builtin
open Namespace_symbols

open Datatypes
open Datatypes_util

open Occurrence

open Schema_util
open Schema_builtin
open Schema_simplification

open Subtyping_top

open Typing_context
open Typing_errors
open Typing_util

open Xquery_common_ast

open Xquery_type_core_ast
open Xquery_type_core_ast_util


(* FS 7.2 Standard functions with specific typing rules

   Typing for built-in functions types *)

type bltin_type_rule = static_context -> Finfo.finfo -> cxtype list -> cxtype -> cxtype

let factor_input_type static_ctxt fi arguments_types  output_type =
  let argument_type = Args.get_param1 arguments_types in
  let schema = schema_from_static_context static_ctxt in
  let input_type = simplify_ty schema argument_type in

  if (is_subtype_of_empty_sequence schema input_type) then cxtype_empty
  else if (is_subtype_of_empty_choice schema input_type) then cxtype_none
  else
    let (p,m,n) = factor input_type in
    fmkcxtype (CBound(p,ub_min m (occurs 1),n)) fi

(* 
  7.1.4 The fs:distinct-doc-order-or-atomic-sequence function
  fs:distinct-doc-order-or-atomic-sequence($item as node* ) as item*

  statEnv  |-  Type <: node*
  -----------------------------------------------------------------------------------------------------
  statEnv |-  (FS-URI,"distinct-doc-order-or-atomic-sequence") ( Type ) : prime(Type) · quantifier(Type)
  
  statEnv  |-  Type <: xs:anyAtomicType*
  ----------------------------------------------------------------------------
  statEnv |-  (FS-URI,"distinct-doc-order-or-atomic-sequence") ( Type ) : Type
*)
let node_star_or_atomic_star_type = (make_choice_cxtypes cxtype_node_star cxtype_anyAtomic_star)

let _fs_distinct_docorder_or_atomic static_ctxt fi arguments_types output_type = 
  let argument_type = Args.get_param1 arguments_types in
  let schema = schema_from_static_context static_ctxt in let
  input_type = simplify_ty schema argument_type in

  if (is_subtype_of_empty_sequence schema input_type) then cxtype_empty
  else if (is_subtype_of_empty_choice schema input_type) then cxtype_none
  else
    let (p,m,n) = factor input_type in
    if (is_subtype_of schema p Schema_builtin.cxtype_node) then 
      fmkcxtype (CBound(p,ub_min m (occurs 1),n)) fi
    else if (is_subtype_of schema p Schema_builtin.cxtype_anyAtomic) then
      input_type
    else
      (* Output type is input type *)
      raise_wrong_expected_type_error static_ctxt input_type node_star_or_atomic_star_type 

(*
  7.1.3 The fs:distinct-doc-order, fs:distinct, and fs:docorder functions

  statEnv |- Type <: node*
  ----------------------------------------------------------------------------------
  statEnv |-  F ( Type ) : prime(Type) · quantifier(Type)
*)
let _fs_distinct_docorder fn_name static_ctxt fi arguments_types output_type =
  let argument_type = Args.get_param1 arguments_types in
  let schema = schema_from_static_context static_ctxt in
  let input_type = simplify_ty schema argument_type in

  if (is_subtype_of_empty_sequence schema input_type) then cxtype_empty
  else if (is_subtype_of_empty_choice schema input_type) then cxtype_none
  else
    let (p,m,n) = factor input_type in
    if (is_subtype_of schema p Schema_builtin.cxtype_node) then 
      fmkcxtype (CBound(p,ub_min m (occurs 1),n)) fi
    else
      (* Output type is input type *)
      raise_wrong_expected_type_error static_ctxt input_type cxtype_node_star

(*
   7.1.5 The fs:item-sequence-to-node-sequence function

    The static type rule in FS CR spec for
    fs:item_sequence_to_node_sequence, used in element and document
    node construction, is wrong.  The spec says "If the content
    sequence contains a document node, the document node is replaced
    in the content sequence by its children.", therefore the typing
    rule should include document nodes in the premise.  Corrected
    rule:

   NB: A more precise rule would keep the node-content type and map all the atomic values to text nodes. 

   statEnv |-  Type : attribute*, (document|element|text|PI|comment|xs:string|xs:float| ...|xs:NOTATION)*
   Type_1 = document_nodes_to_document_content Type
   ---------------------------------------------------------------------------------------------
   statEnv |-  (FS-URI,"item-sequence-to-node-sequence") (Type) : Type_1
*)
let element_constructor_content =  
  make_sequence_cxtypes 
    (make_zeroormore_type cxtype_attribute) 
    (make_zeroormore_type 
       (make_choice_cxtypes
	  cxtype_anyAtomic
	  (make_choice_cxtypes 
	     cxtype_documentnode
	     (make_choice_cxtypes 
		cxtype_element 
		(make_choice_cxtypes cxtype_text
		   (make_choice_cxtypes cxtype_pi cxtype_comment))))))

let _fs_item_seq_to_node_seq static_ctxt fi arguments_types output_type =
  let argument_type = Args.get_param1 arguments_types in
  let schema = schema_from_static_context static_ctxt in
  let input_type = simplify_ty schema argument_type in

  if (is_subtype_of_empty_sequence schema input_type) then cxtype_empty
  else if (is_subtype_of_empty_choice schema input_type) then cxtype_none
  else if (is_subtype_of schema input_type element_constructor_content) then
    let (plist,m,n) = factor_with_units input_type in
    let p' = choice_of_list(
      List.map
	(fun t -> match t.pcxtype_desc with 
	| CDocument t' -> t' 
	| CAtomicRef _ -> cxtype_text
	| _ -> t ) 
	plist) in
    fmkcxtype (CBound(p',m,n)) fi
  else 
    begin
      ignore(raise_wrong_expected_type_error static_ctxt input_type element_constructor_content);
      output_type
    end

(* 

  7.2.4 The fn:boolean function

   statEnv |-  Type <: (empty | NodeType+ | xs:boolean | xs:string | xs:untypedAtomic | fs:numeric)
   -------------------------------------------------------------------------------------------------
   statEnv  |-  fn:boolean(Type) : xs:boolean
*)
let eftype = 
  make_choice_cxtypes cxtype_untypedAtomic_optional
    (make_choice_cxtypes cxtype_boolean
       (make_choice_cxtypes cxtype_string
	  (make_choice_cxtypes cxtype_anyURI
	     (make_choice_cxtypes cxtype_numeric cxtype_node_plus))))

let _effective_boolean_type static_ctxt fi arguments_types output_type =
  let schema = schema_from_static_context static_ctxt in
  let input_type = Args.get_param1 arguments_types in
  if (is_subtype_of schema input_type eftype) then output_type
  else 
    begin
      ignore(raise_wrong_expected_type_error static_ctxt input_type eftype);
      output_type
    end
(*
        7.2.6 The fn:data function
*)
let _data_type static_ctxt fi arguments_types output_type =
  let input_type = Args.get_param1 arguments_types in
  let schema = schema_from_static_context static_ctxt in
  Typing_util.data_on_judge schema input_type 

(*  apply_to_unit_types schema data_types_fun input_type *)

let _copy_type schema fi arguments_types output_type =
  Args.get_param1 arguments_types

let _copy_first_type static_ctxt fi arguments_types output_type =
  let (argument_type1, argument_type2) = Args.get_param2 arguments_types in
  argument_type1

(*
   7.2.7 The fn:distinct-values function
   7.2.12 The fn:reverse function
 
   both call factor_input_type
*)

(*
   7.2.11 The fn:remove function

   statEnv |-   Type1 <: xs:integer
   ----------------------------------------------------------------------------
   statEnv |-  (FN-URI,"remove")(Type, Type1) : prime(Type) · quantifier(Type)?

*)
let _remove_type static_ctxt fi arguments_types  output_type =
  let (argument_type1, argument_type2) = Args.get_param2 arguments_types in
  let schema = schema_from_static_context static_ctxt in
  let input_type = simplify_ty schema argument_type1 in
  if not(is_subtype_of schema argument_type2 Schema_builtin.cxtype_integer) then 
    raise_wrong_expected_type_error static_ctxt argument_type2 cxtype_integer
  else
    if (is_subtype_of_empty_sequence schema input_type) then cxtype_empty
    else if (is_subtype_of_empty_choice schema input_type) then cxtype_none
    else
      let (p,m,n) = factor input_type in
      fmkcxtype (CBound(p, occurs 0, n)) fi

(*
   7.2.13 The fn:subsequence function
*)
let _fn_subsequence nargs static_ctxt fi arguments_types output_type =
  let schema = schema_from_static_context static_ctxt in
  let (input_type1, input_type2, input_type3) = 
    if (nargs = 2) then 
      let (a1, a2) = Args.get_param2 arguments_types in
      let i1 = simplify_ty schema a1 in
      let i2 = simplify_ty schema a2 in
      (i1, i2, None)
    else
      let (a1, a2, a3) = Args.get_param3 arguments_types in
      let i1 = simplify_ty schema a1 in
      let i2 = simplify_ty schema a2 in
      let i3 = simplify_ty schema a3 in
      (i1, i2, Some i3)
  in
  if (is_subtype_of schema input_type2 Schema_builtin.cxtype_integer) &&
    (match input_type3 with
    | None -> true
    | Some i3 -> is_subtype_of schema i3 Schema_builtin.cxtype_integer)
  then
    if (is_subtype_of_empty_sequence schema input_type1) then cxtype_empty
    else if (is_subtype_of_empty_choice schema input_type1) then cxtype_none
    else
      let (p,m,n) = factor input_type1 in
      fmkcxtype (CBound(p,m,n)) fi
  else
    begin
      ignore(raise_wrong_expected_type_error static_ctxt input_type2 cxtype_integer);
      output_type
    end
(*  7.2.1 The fn:last context function 

   fs:first_last is a special FS function that implement the semantics
   of XPath predicate expressions [1] and [last()]
*)
let _fs_first_last static_ctxt fi arguments_types output_type =
  let argument_type = Args.get_param1 arguments_types in
  let schema = schema_from_static_context static_ctxt in
  let input_type = simplify_ty schema argument_type in

  if (is_subtype_of_empty_sequence schema input_type) then cxtype_empty
  else if (is_subtype_of_empty_choice schema input_type) then cxtype_none
  else
    let (p,m,n) = factor input_type in
    match m with
    | UP_INT 0 -> fmkcxtype (CBound(p,m,(occurs 1))) fi
    | _ -> p

(*
    _untyped_to_expected implements fs:untyped-to-{double,integer,string}
    and fs:convert-simple-operand
 
    Convert xs:untypedAtomic in prime type to expected type
   
    u2e(T1 | T2, E)   = u2e(T1 | T2, E)  = u2e(T1, E) | u2e(T2, E)
    u2e(U, E)         = U, if U is unit type and not xs:untypedAtomic
    u2e(xs:untypedAtomic, E) = E

                   Expr : Type
           Type' = u2e(prime(Type), ExpectedType) 
    ---------------------------------------------------------------------
    _untyped_to_expected(Expr); ExpectedType :  Type' o quantifier(Type)

*)
let _untyped_to_expected static_ctxt fi input_type expected_type = 
   let schema = schema_from_static_context static_ctxt in
   if (is_subtype_of_empty_sequence schema input_type) then cxtype_empty
   else if (is_subtype_of_empty_choice schema input_type) then cxtype_none
   else 
     begin
       let (pl,m,n) = factor_with_units input_type in
       let (ul, p1l') = 
	 List.partition (fun u -> is_subtype_of schema u Schema_builtin.cxtype_untypedAtomic) pl in
       (* Convert xs:untypedAtomic to expected type;
          Other unit types remain unchanged. *)
       if (List.length ul > 0) then 
	 let p' = simplify_ty schema (choice_of_list (expected_type :: p1l')) in
	 fmkcxtype (CBound(p', m, n)) fi 
       else
	 input_type
     end

let _fs_untyped_to_double static_ctxt fi arguments_types output_type = 
   let argument_type = Args.get_param1 arguments_types in
   let schema = schema_from_static_context static_ctxt in
   let input_type = simplify_ty schema argument_type in
   if (is_subtype_of_anyatomic_optional schema input_type) then 
     _untyped_to_expected static_ctxt fi input_type Schema_builtin.cxtype_double
   else 
     begin
       ignore(raise_wrong_expected_type_error static_ctxt input_type cxtype_anyAtomic_optional);
       output_type
     end
let _fs_untyped_to_integer static_ctxt fi arguments_types output_type = 
   let argument_type = Args.get_param1 arguments_types in
   let schema = schema_from_static_context static_ctxt in
   let input_type = simplify_ty schema argument_type in
   if (is_subtype_of_anyatomic_optional schema input_type) then 
     _untyped_to_expected static_ctxt fi input_type Schema_builtin.cxtype_integer
   else 
     begin
       ignore(raise_wrong_expected_type_error static_ctxt input_type cxtype_anyAtomic_optional);
       output_type
     end
let _fs_untyped_to_string static_ctxt fi arguments_types output_type = 
   let argument_type = Args.get_param1 arguments_types in
   let schema = schema_from_static_context static_ctxt in
   let input_type = simplify_ty schema argument_type in
   if (is_subtype_of_anyatomic_optional schema input_type) then 
     _untyped_to_expected static_ctxt fi input_type Schema_builtin.cxtype_string
   else 
     begin
       ignore(raise_wrong_expected_type_error static_ctxt input_type cxtype_anyAtomic_optional);
       output_type
     end
(*
   fs:convert-simple-operand(Expr1, Expr2):
     See definition of _untyped_to_expected above.
*)
let _fs_convert_simple_operand static_ctxt fi arguments_types output_type = 

  let (argument_type1, argument_type2) = Args.get_param2 arguments_types in
   let schema = schema_from_static_context static_ctxt in
  let input_type1 = simplify_ty schema argument_type1 in
  let input_type2 = simplify_ty schema argument_type2 in
   if (not(is_subtype_of_anyatomic_sequence schema input_type1)) then  
     raise_wrong_expected_type_error static_ctxt input_type1 cxtype_anyAtomic_star
   else if (not(is_subtype_of_anyatomic schema input_type2)) then 
     begin
       ignore(raise_wrong_expected_type_error static_ctxt input_type1 cxtype_anyAtomic);
       output_type
     end
   else
     _untyped_to_expected static_ctxt fi input_type1 input_type2

(*
       Expr1 : Type1   Type1 <: xs:anyAtomicType 
       Expr2 : Type2   Type2 <: xs:anyAtomicType 

   Type3 = Union of:
     for each UnitType in prime(Type2)
        let ExpectedType = 
          If UnitType is xs:untypedAtomic or xs:string then xs:string
          Else if UnitType is numeric then  xs:double
          Otherwise UnitType 
        in untyped_to_expected(Type1, ExpectedType)  

    -----------------------------------------------------------------
                fs:untyped-to-any(Expr1, Expr2) : Type3

   If both args are untypedAtomic, we cast both to string.

   If one arg is untypedAtomic and the other is numeric, we cast
   untypedAtomic to double.

   If one arg is untypedAtomic and the other is string, we cast
   untypedAtomic to string.  Type promotion from xs:untypedAtomic to
   other target types is handled automatically during function
   application.

*)

let _fs_untyped_to_any static_ctxt fi arguments_types output_type = 
   let (argument_type1, argument_type2) = Args.get_param2 arguments_types in
   let schema = schema_from_static_context static_ctxt in
   let input_type1 = simplify_ty schema argument_type1 in
   let input_type2 = simplify_ty schema argument_type2 in
   if (not(is_subtype_of_anyatomic schema input_type1)) then 
     raise_wrong_expected_type_error static_ctxt input_type1 cxtype_anyAtomic
   else if (not(is_subtype_of_anyatomic schema input_type2)) then
     begin
       ignore(raise_wrong_expected_type_error static_ctxt input_type2 cxtype_anyAtomic);
       output_type
     end
   else
    begin
      let (p1l,_,_) = factor_with_units input_type1 in
      let (ul, p1l') = List.partition 
	  (fun u -> is_subtype_of schema u Schema_builtin.cxtype_untypedAtomic) p1l in
      if (List.length ul > 0) then 
	let (p2l,_,_) = factor_with_units input_type2 in
	let ul' = 
	  List.map 
	    (fun u2 -> 
	      if (is_subtype_of schema u2 Schema_builtin.cxtype_untypedAtomic) ||
	      (is_subtype_of schema u2 Schema_builtin.cxtype_string) then
		Schema_builtin.cxtype_string
	      else if (is_subtype_of_anynumeric schema u2) then
		Schema_builtin.cxtype_double
	      else 
		u2)
	    p2l
	in simplify_ty schema (choice_of_list (ul' @ p1l'))
      else
	input_type1
   end

(*
         Expr1 : Type1   Type1 <: numeric *
         Expr2 : Type2   Type2 <: numeric

         Each UnitType in prime(Type1) can_be_promoted_to Type2
    -----------------------------------------------------------------
   fs:promote_to_numeric(Expr1, Expr2) : Type2 o quantifier(Type1)

*)
let _fs_promote_to_numeric static_ctxt fi arguments_types output_type = 
   let (argument_type1, argument_type2) = Args.get_param2 arguments_types in
   let schema = schema_from_static_context static_ctxt in
   let input_type1 = simplify_ty schema argument_type1 in
   let input_type2 = simplify_ty schema argument_type2 in
   if (not(is_subtype_of schema input_type1 Schema_builtin.cxtype_numeric_star)) then 
     begin
       ignore(raise_wrong_expected_type_error static_ctxt input_type1 cxtype_numeric_star);
       output_type
     end
   else if (not(is_subtype_of_anynumeric schema input_type2)) then 
     begin
       ignore(raise_wrong_expected_type_error static_ctxt input_type2 cxtype_numeric);
       output_type
     end
   else 
    let input_type2_plus = make_oneormore_type input_type2 in 
    if (Gmisc.is_some (Typing_util.can_be_promoted_to_judge schema input_type1 input_type2_plus))  then
      let (_,m,n) = factor input_type1 in
      fmkcxtype (CBound(input_type2, m, n)) fi
    else
     begin
       ignore(raise_wrong_expected_type_error static_ctxt input_type1 input_type2);
       output_type
     end
(* This is really a type promotion error 
      "Cannot promote "^(Print_top.bprintf_cxtype "" input_type1)^" to "^(Print_top.bprintf_cxtype "" input_type2)
*)

(*
  _node_seq_type implements a more precise type rule for fs:node-sequence:

          Expr : Type
         Type <: Node*
  ------------------------------
  fs:node-sequence(Expr) : Type

*)
let  _fs_node_seq static_ctxt fi arguments_types  output_type =
  let argument_type = Args.get_param1 arguments_types in
  let schema = schema_from_static_context static_ctxt in
  let input_type = simplify_ty schema argument_type in

  if (is_subtype_of_empty_sequence schema input_type) then cxtype_empty
  else if (is_subtype_of_empty_choice schema input_type) then cxtype_none
  else if (is_subtype_of_anynode_sequence schema input_type) then input_type 
  else 
     begin
       ignore(raise_wrong_expected_type_error static_ctxt input_type cxtype_node_star);
       output_type
     end
(*
  _fs_node_sequence_or_atomic 

      Expr : Type
  Type <: Node* or Type <: xs:anyAtomicType*
  ------------------------------------------
  fs:node-sequence-or-atomic-type(Expr) : Type

*)
let  _fs_node_seq_or_atomic static_ctxt fi arguments_types  output_type =
  let argument_type = Args.get_param1 arguments_types in
  let schema = schema_from_static_context static_ctxt in
  let input_type = simplify_ty schema argument_type in

  if (is_subtype_of_empty_sequence schema input_type) then cxtype_empty
  else if (is_subtype_of_empty_choice schema input_type) then cxtype_none
  else if (is_subtype_of_anynode_sequence schema input_type || is_subtype_of_anyatomic_sequence schema input_type) then input_type
  else 
    begin
      ignore(raise_wrong_expected_type_error static_ctxt input_type node_star_or_atomic_star_type);
      output_type
    end

(*
  _unary_type implements more precise type rules for unary arithmetic operators:

        Expr : empty
   --------------------
   UnaryOp Expr : empty

       Expr <: numeric
  ----------------------------
  UnaryOp Expr : type(UnaryOp)

*)
let _unary_arithmetic_type static_ctxt fi arguments_types  output_type =
  let argument_type = Args.get_param1 arguments_types in
  let schema = schema_from_static_context static_ctxt in
  let input_type = simplify_ty schema argument_type in

  if (is_subtype_of_empty_sequence schema input_type) then cxtype_empty
  else if (is_subtype_of_anynumeric schema input_type) then input_type
  else output_type

(* 

  7.2.10 The fn:min, fn:max, fn:avg, and fn:sum  functions

  NB: The "convert_untypedAtomic" judgments and the "Type0 in ..."
  judgments in the following rules both occur in
  Typing_call.match_function_signature BEFORE this special typing rule
  is applied, which permits us to coalesce all these rules into one
  aggregation rule.

  Type1 = prime(Type)
  Type2 = convert_untypedAtomic(Type1, xs:double)   
  ItemType1, ...,ItemTypen = Type2
  Type0 in { xs:string, xs:integer, xs:decimal, xs:float, xs:double, 
             xs:date, xs:time, xs:dateTime, xs:yearMonthDuration, xs:dayTimeDuration }
  statEnv |-  ItemTypei can be promoted to Type0      1 <= i <= n
  --------------------------------------------------------------------------------
  statEnv |-  (FN-URI,"min,max")(Type) : Type0 · aggregate_quantifier(quantifier(Type))

  Type1 = prime(Type)
  Type2 = convert_untypedAtomic(Type1, xs:double)
  ItemType1, ...,ItemTypen = Type2
  Type0 in { xs:decimal, xs:float, xs:double, xs:yearMonthDuration, xs:dayTimeDuration }
  statEnv |-  ItemTypei can be promoted to Type0      1 <= i <= n
  --------------------------------------------------------------------------------
  statEnv |-  (FN-URI,"avg")(Type) : Type0 · aggregate_quantifier(quantifier(Type))

*)
let _agg_type static_ctxt fi arguments_types output_type =
  let argument_type = Args.get_param1 arguments_types in
  let schema = schema_from_static_context static_ctxt in
  let input_type = simplify_ty schema argument_type in
  if (is_subtype_of_empty_sequence schema input_type) then cxtype_empty
  else
    let (p,m,n) = factor_with_units input_type in
  (*
     aggregate_quantifier( ? )	  =  	?
     aggregate_quantifier( * )	  =  	?
     aggregate_quantifier( 1 )	  =  	1
     aggregate_quantifier( + )	  =  	1
  *)
    if (Occurrence.is_optional (m,n) || Occurrence.is_star (m,n)) then 
      Schema_util.make_optional_type (Schema_util.choice_of_list p)
    else (Schema_util.choice_of_list p)

(* 
  Whereas fn:min(), fn:max() propogate their input types, fn:avg()
  propogates its output, because the output type can be different than
  the input.
*)
let _avg_type static_ctxt fi arguments_types  output_type =
  let argument_type = Args.get_param1 arguments_types in
  let schema = schema_from_static_context static_ctxt in
  let input_type = simplify_ty schema argument_type in

  if (is_subtype_of_empty_sequence schema input_type) then cxtype_empty
  else
    let (p,m,n) = factor_with_units output_type in
  (*
     aggregate_quantifier( ? )	  =  	?
     aggregate_quantifier( * )	  =  	?
     aggregate_quantifier( 1 )	  =  	1
     aggregate_quantifier( + )	  =  	1
  *)
    if (Occurrence.is_optional (m,n) || Occurrence.is_star (m,n)) then 
      Schema_util.make_optional_type (Schema_util.choice_of_list p)
    else (Schema_util.choice_of_list p)

(*
  NB: The "convert_untypedAtomic" judgments and the "Type0 in ..."
  judgments in the following rules both occur in
  Typing_call.match_function_signature BEFORE this special typing rule
  is applied, which permits us to coalesce all these rules into one
  aggregation rule.

  statEnv |-  Type2 <: xs:anyAtomicType ?
  Type3 = prime(Type1)
  Type4 = convert_untypedAtomic(Type3, xs:double)
  ItemType1, ...,ItemTypen = Type4
  Type0 in { xs:integer, xs:decimal, xs:float, xs:double, xs:yearMonthDuration }
  statEnv |-  ItemTypei can be promoted to Type0      1 <= i <= n
  statEnv |-  Type2 <: Type0
  -----------------------------------------------------------------------------------------
  statEnv |-  (FN-URI,"sum")(Type1,Type2) : Type0 · aggregate_quantifier(quantifier(Type1))
*)
let _sum_type_one static_ctxt fi arguments_types output_type =
  output_type

let _sum_type static_ctxt fi arguments_types output_type =
  let (argument_type1, argument_type2) = Args.get_param2 arguments_types in
  let schema = schema_from_static_context static_ctxt in
  let input_type1 = simplify_ty schema argument_type1 in
  if (is_subtype_of_empty_sequence schema input_type1) then argument_type2
  else if (is_subtype_of_empty_choice schema input_type1) then cxtype_none
  else 
    let (p,m,n) = factor_with_units input_type1 in
    if (not(is_subtype_of schema argument_type2 input_type1)) then 
      begin
	ignore(raise_wrong_expected_type_error static_ctxt argument_type2 input_type1);
	output_type
      end
  (*
     aggregate_quantifier( ? )	  =  	?
     aggregate_quantifier( * )	  =  	?
     aggregate_quantifier( 1 )	  =  	1
     aggregate_quantifier( + )	  =  	1
  *)
    else if (Occurrence.is_optional (m,n) || Occurrence.is_star (m,n)) then 
      Schema_util.make_optional_type (Schema_util.choice_of_list p)
    else (Schema_util.choice_of_list p)

(* 
  7.2.14 The op:union, op:intersect, and op:except operators

  statEnv  |-  (OP-URI,"union")(Type1, Type2) : prime(Type1 , Type2) · quantifier(Type1 , Type2)

  statEnv  |-  (OP-URI,"intersect")(Type1, Type2) : prime(Type1, Type2) · quantifier(Type1,Type2) · ?

  statEnv  |-  (OP-URI,"except")(Type1, Expr2) : prime(Type1) · quantifier(Type1) · ?
*)
let _op_union static_ctxt fi arguments_types  output_type =
  let (argument_type1, argument_type2) = Args.get_param2 arguments_types in
  let argument_type3 = make_sequence_cxtypes argument_type1 argument_type2 in 
  let (p,m,n) = factor argument_type3 in
  fmkcxtype (CBound(p, m, n)) fi

let _op_intersect static_ctxt fi arguments_types  output_type =
  let (argument_type1, argument_type2) = Args.get_param2 arguments_types in
  let argument_type3 = make_sequence_cxtypes argument_type1 argument_type2 in 
  let (p,_,n) = factor argument_type3 in
  fmkcxtype (CBound(p, Occurrence.occurs 0, n)) fi

let _op_except static_ctxt fi arguments_types  output_type =
  let (argument_type, _) = Args.get_param2 arguments_types in
  let schema = schema_from_static_context static_ctxt in
  let input_type = simplify_ty schema argument_type in
  if (is_subtype_of_empty_sequence schema input_type) then cxtype_empty
  else
    let (p,_,n) = factor input_type in
    fmkcxtype (CBound(p, Occurrence.occurs 0, n)) fi

(* 
  7.2.15 The fn:insert-before function

  statEnv |-  Type2 <: xs:integer
  Type4 = (Type1,Type3)
  -----------------------------------------------------------------------------------------
  statEnv |-  (FN-URI,"insert-before")(Type1,Type2,Type3) : prime(Type4) · quantifier(Type4)
*)
let _insert_before static_ctxt fi arguments_types  output_type =
  let (argument_type1, argument_type2, argument_type3) = Args.get_param3 arguments_types in
  let schema = schema_from_static_context static_ctxt in

  if (is_subtype_of schema argument_type2 cxtype_integer) then 
    let argument_type4 = make_sequence_cxtypes argument_type1 argument_type3 in 
    let (p,m,n) = factor argument_type4 in
    fmkcxtype (CBound(p, m, n)) fi
  else
    begin
      ignore(raise_wrong_expected_type_error static_ctxt argument_type2 cxtype_integer);
      output_type
    end

(*
   7.2.16 The fn:zero-or-one, fn:one-or-more, and fn:exactly-one functions
*)
let _zero_or_one static_ctxt fi arguments_types  output_type =
  let argument_type = Args.get_param1 arguments_types in
  let schema = schema_from_static_context static_ctxt in
  let input_type = simplify_ty schema argument_type in

  if (is_subtype_of_empty_sequence schema input_type) then cxtype_empty
  else if (is_subtype_of_empty_choice schema input_type) then cxtype_none
  else
    let (p,m,n) = factor input_type in
    if (Occurrence.equal m (occurs 1)) then p 
    else fmkcxtype (CBound(p, (occurs 0), (occurs 1))) fi

(* MF: 
   We use approximate occurrences in the type-refief functions, so
   even we compute a type T(m,n) where m and n are known integers
   s.t. m > 1, we approximate as (1, unbounded).
*)
let _exactly_one static_ctxt fi arguments_types  output_type =
  let argument_type = Args.get_param1 arguments_types in
  let schema = schema_from_static_context static_ctxt in
  let input_type = simplify_ty schema argument_type in

  if (is_subtype_of_empty_sequence schema input_type) then 
    begin
      ignore(raise_wrong_expected_type_error static_ctxt cxtype_empty cxtype_item);
      output_type
    end
  else if (is_subtype_of_empty_choice schema input_type) then 
    cxtype_none
  else
    let (p,m,n) = factor input_type in 
    p

let _one_or_more static_ctxt fi arguments_types  output_type =
  let argument_type = Args.get_param1 arguments_types in
  let schema = schema_from_static_context static_ctxt in
  let input_type = simplify_ty schema argument_type in

  if (is_subtype_of_empty_sequence schema input_type) then 
    begin
      ignore(raise_wrong_expected_type_error static_ctxt cxtype_empty cxtype_item_plus);
      output_type
    end
  else if (is_subtype_of_empty_choice schema input_type) then 
    cxtype_none
  else
    let (p,m,n) = factor input_type in
    fmkcxtype (CBound(p, (occurs 1), n)) fi

(* creation of the table for built-in functions *)

let bltin_fctns_type_rules : (bltin_type_rule) RQNameIntHashtbl.t =
  RQNameIntHashtbl.create 137

let add_bltin_fctn_type_rule ((cfname,i), type_rule) = RQNameIntHashtbl.add bltin_fctns_type_rules (cfname,i) type_rule

let lookup_bltin_fctn_type_rule (rfname,i) =
  try
    RQNameIntHashtbl.find bltin_fctns_type_rules (rfname,i)
  with _ ->
    raise (Query (Static_Internal ("Looking up type rule for a built-in function. This error should be trapped and never be visible")))

let has_special_type_rule (rfname, i) = 
  RQNameIntHashtbl.mem bltin_fctns_type_rules (rfname,i)

let type_rule_table = [

  (* 7.1 FORMAL SEMANTICS FUNCTIONS *)
  ((fs_convert_simple_operand), 2), _fs_convert_simple_operand;
  ((fs_untyped_to_any), 2), _fs_untyped_to_any;
  ((fs_untyped_to_double), 1), _fs_untyped_to_double;
  ((fs_untyped_to_integer), 1), _fs_untyped_to_integer;
  ((fs_untyped_to_string), 1), _fs_untyped_to_string;
  ((fs_promote_to_numeric), 2), _fs_promote_to_numeric;
  (* document order functions *)
  ((fs_distinct_docorder),1), (_fs_distinct_docorder fs_distinct_docorder);
  ((fs_docorder),1), (_fs_distinct_docorder fs_docorder);
  ((fs_distinct),1), (_fs_distinct_docorder fs_distinct);
  ((fs_distinct_docorder_or_atomic_sequence),1), _fs_distinct_docorder_or_atomic;
  ((fs_node_sequence),1), _fs_node_seq;
  ((fs_node_sequence_or_atomic_sequence),1), _fs_node_seq_or_atomic;  
  ((fs_item_sequence_to_node_sequence),1), _fs_item_seq_to_node_seq;
  (*
     7.1.7 The fs:item-sequence-to-untypedAtomic-PI function
     7.1.8 The fs:item-sequence-to-untypedAtomic-text function
     7.1.9 The fs:item-sequence-to-untypedAtomic-comment function        
     7.1.10 The fs:apply-ordering-mode function
  *)

  (* 7.2 STANDARD FUNCTIONS WITH SPECIFIC TYPING RULES *)
  ((fs_first), 1), _fs_first_last;
  ((fs_last_fn), 1), _fs_first_last;

  (* 7.2.3 
     The fn:abs, fn:ceiling, fn:floor, fn:round, and
     fn:round-half-to-even functions
  *)

  ((fn_floor), 1), _unary_arithmetic_type;
  ((fn_floor_double), 1), _unary_arithmetic_type;
  ((fn_floor_float), 1), _unary_arithmetic_type;
  ((fn_floor_decimal), 1), _unary_arithmetic_type;
  ((fn_floor_integer), 1), _unary_arithmetic_type; 

  ((fn_ceiling), 1), _unary_arithmetic_type;
  ((fn_ceiling_double), 1), _unary_arithmetic_type;
  ((fn_ceiling_float), 1), _unary_arithmetic_type;
  ((fn_ceiling_decimal), 1), _unary_arithmetic_type;
  ((fn_ceiling_integer), 1), _unary_arithmetic_type; 

  ((fn_abs), 1), _unary_arithmetic_type;
  ((fn_abs_double), 1), _unary_arithmetic_type;
  ((fn_abs_float), 1), _unary_arithmetic_type;
  ((fn_abs_decimal), 1), _unary_arithmetic_type;
  ((fn_abs_integer), 1), _unary_arithmetic_type; 

  ((fn_round), 1), _unary_arithmetic_type;
  ((fn_round_double), 1), _unary_arithmetic_type;
  ((fn_round_float), 1), _unary_arithmetic_type;
  ((fn_round_decimal), 1), _unary_arithmetic_type;
  ((fn_round_integer), 1), _unary_arithmetic_type; 

  ((fn_boolean), 1), _effective_boolean_type; 

  (* MISSING : fn_round_half_to_even *)

  ((fn_data), 1), _data_type;
  ((fn_distinct_values),1), factor_input_type;

  (* fn:unordered unimplemented *)

  (* AGGREGATE FUNCTIONS *)
  ((fn_min), 1),        _agg_type;  
  ((fn_min_double), 1), _agg_type; 
  ((fn_min_float), 1),  _agg_type; 
  ((fn_min_decimal), 1),_agg_type; 
  ((fn_min_integer), 1),_agg_type;
  ((fn_min_string), 1), _agg_type;   
  ((fn_min_yearMonthDuration), 1),   _agg_type;   
  ((fn_min_dayTimeDuration), 1),   _agg_type;   
  ((fn_min_date), 1),   _agg_type;   
  ((fn_min_time), 1),   _agg_type;   
  ((fn_min_dateTime), 1),   _agg_type;   

  ((fn_max), 1),        _agg_type;  
  ((fn_max_double), 1), _agg_type; 
  ((fn_max_float), 1),  _agg_type; 
  ((fn_max_decimal), 1),_agg_type; 
  ((fn_max_integer), 1),_agg_type; 
  ((fn_max_string), 1), _agg_type;   
  ((fn_max_yearMonthDuration), 1),   _agg_type;   
  ((fn_max_dayTimeDuration), 1),   _agg_type;   
  ((fn_max_date), 1),   _agg_type;   
  ((fn_max_time), 1),   _agg_type;   
  ((fn_max_dateTime), 1),   _agg_type;   

  ((fn_avg), 1),        _avg_type; 
  ((fn_avg_double), 1), _avg_type; 
  ((fn_avg_float), 1),  _avg_type; 
  ((fn_avg_decimal), 1),_avg_type; 
  ((fn_avg_integer), 1),_avg_type; 
  ((fn_avg_yearMonthDuration), 1),   _avg_type;   
  ((fn_avg_dayTimeDuration), 1),   _avg_type;   

  ((fn_sum), 1),        _sum_type_one; 
  ((fn_sum_double), 1), _sum_type_one; 
  ((fn_sum_float), 1),  _sum_type_one; 
  ((fn_sum_decimal), 1),_sum_type_one; 
  ((fn_sum_integer), 1),_sum_type_one; 
  ((fn_sum_yearMonthDuration), 1), _sum_type_one;   
  ((fn_sum_dayTimeDuration), 1),   _sum_type_one;   

  ((fn_sum), 2),        _sum_type; 
  ((fn_sum_double), 2), _sum_type; 
  ((fn_sum_float), 2),  _sum_type; 
  ((fn_sum_decimal), 2),_sum_type; 
  ((fn_sum_integer), 2),_sum_type; 
  ((fn_sum_yearMonthDuration), 2), _sum_type;   
  ((fn_sum_dayTimeDuration), 2),   _sum_type;   

  (* collection operations *)
  ((fn_remove), 2), _remove_type;
  ((fn_reverse),1), factor_input_type;
  ((fn_subsequence),2), (_fn_subsequence 2);
  ((fn_subsequence),3), (_fn_subsequence 3);

  ((op_union),2), _op_union;
  ((op_intersect),2), _op_intersect;
  ((op_except),2), _op_except;

  ((fn_insert_before),3), _insert_before; 

  ((fn_exactly_one),1), _exactly_one;
  ((fn_zero_or_one),1), _zero_or_one;
  ((fn_one_or_more),1), _one_or_more;
  ((fn_trace),2), _copy_first_type;

(* 
   MISSING
        7.2.5 The fn:collection and fn:doc functions
        7.2.8 The fn:unordered function
*)
]

let _ = 
  try
    List.iter add_bltin_fctn_type_rule type_rule_table
  with
  | e ->
      begin
	eprintf_error "  " e;
	fprintf (!Conf.glx_err_formatter) "@."
      end



