(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: norm_util.ml,v 1.89 2008/03/21 19:02:32 simeon Exp $ *)

(* Module: Norm_util
   Description:
     This module implements utilities used during normalization.
*)
open Debug
open Error
open Finfo

open Namespace_names
open Namespace_symbols
open Namespace_builtin
open Namespace_util

open Schema_util

open Datatypes

open Norm_context

open Xquery_common_ast

open Xquery_ast
open Xquery_ast_util

open Xquery_core_ast
open Xquery_core_ast_annotation
open Xquery_core_ast_util
open Xquery_type_core_ast
open Xquery_type_core_ast_util

open Schema_builtin
open Schema_judge

let incorrect_arg_count fn actual_ct formal_ct = 
  (Query (Type_Error(("Incorrect number of arguments ("
 			 ^ (string_of_int actual_ct)
			 ^ ") for function "
			 ^ (Namespace_names.prefixed_string_of_rqname fn)
			 ^ " ("^ (string_of_int formal_ct) ^") "))))

(*******************************)
(* Variable name management    *)
(*******************************)

let resolve_variable_qname_register norm_ctxt vname =
  let nsenv = nsenv_from_norm_context norm_ctxt in
  let cvname = Namespace_resolve.resolve_variable_qname nsenv vname in
  let nc = Norm_context.register_var norm_ctxt cvname in
  (nc,cvname)

let resolve_global_qname_register norm_ctxt vname acexpr =
  let nsenv = nsenv_from_norm_context norm_ctxt in
  let cvname = Namespace_resolve.resolve_variable_qname nsenv vname in
  let nc = Norm_context.register_global_var norm_ctxt cvname acexpr in
  (nc,cvname)

let resolve_variable_qname_check norm_ctxt fi vname =
  let nsenv = nsenv_from_norm_context norm_ctxt in
  let cvname = Namespace_resolve.resolve_variable_qname nsenv vname in
  (cvname, Norm_context.check_var norm_ctxt fi cvname)

(**************************************)

let map_fun_kind fun_body = 
  match fun_body with 
  | EFunctionBltIn -> CEFunctionBltInKind
  | EFunctionInterface -> CEFunctionInterfaceKind
  | EFunctionImported -> CEFunctionImportedKind
  | EFunctionUser _ -> CEFunctionUserKind

let map_cfun_kind imported_as_module fun_body = 
  match fun_body with 
  | CEFunctionBltIn -> (CEFunctionBltInKind, fun_body)
  | CEFunctionInterface -> 
      if (imported_as_module) then (CEFunctionImportedKind, CEFunctionImported) else 
      (CEFunctionInterfaceKind, fun_body)
  | CEFunctionImported -> (CEFunctionImportedKind, fun_body)
  | CEFunctionUser _ -> (CEFunctionUserKind, fun_body)

let map_cvar_body imported_as_module var_body = 
  match var_body with 
  | CEVarInterface ->
      if (imported_as_module) then CEVarImported else CEVarInterface
  | _ -> var_body

let string_of_cfun_kind cfun_kind = 
  match cfun_kind with 
  | CEFunctionBltInKind -> "Builtin"
  | CEFunctionInterfaceKind -> "Interface"
  | CEFunctionImportedKind -> "Imported"
  | CEFunctionUserKind -> "User"

(*******************************)
(* Core AST creation functions *)
(*******************************)

(* Build a core some expression *)

let build_core_some norm_ctxt rodt v e1 e2 eh fi =
  fmkcexpr (CESome (rodt, v, e1, e2)) eh fi

(* Build a core every expression *)

let build_core_every norm_ctxt rodt v e1 e2 eh fi =
  fmkcexpr (CEEvery (rodt, v, e1, e2)) eh fi

(* Build a core if expression *)

let build_core_if norm_ctxt cond e1 e2 eh fi =
  fmkcexpr (CEIf (cond, e1, e2)) eh fi

(* Build a core while expression *)

let build_core_while norm_ctxt cond e1 eh fi =
  fmkcexpr (CEWhile (cond, e1)) eh fi

(* Build a core attribute constructor *)

let build_core_attribute_constructor norm_context caname nsenv celist eh fi =
  fmkcexpr (CEAttr (caname, nsenv, celist)) eh fi

(* Build a core element constructor *)

let build_core_element_constructor norm_context cename nsenv celist eh fi =
  fmkcexpr (CEElem (cename, nsenv, celist)) eh fi

let predefined_funs_with_side_effects = 
  [ (fn_trace,2); (glx_sleep,1); (glx_save_document,2); 
    (glx_print_item_err,1); (glx_print_string_err,1);
    (glx_print_item,1); (glx_print_string,1); ]
    
let bltin_with_side_effects = RQNameIntHashtbl.create 11

let _ = 
  List.iter 
    (fun cfname_arity -> 
      RQNameIntHashtbl.add bltin_with_side_effects cfname_arity true) 
    predefined_funs_with_side_effects
    
let is_bltin_with_side_effects cfname_arity = RQNameIntHashtbl.mem bltin_with_side_effects cfname_arity
  
(* Build a core function call *)
let build_core_call norm_ctxt fn args eh fi =
  let fn_arity = fn, List.length args in
  let (input_types, output_type), opt_fun_kind, upd = one_sig_from_norm_context norm_ctxt fn_arity in
  let input_types' = 
    List.map (fun t -> Some t) input_types   in
  let upd_info = if is_bltin_with_side_effects fn_arity then Updating else upd
  in 
    fmkcexpr (CECall (fn, args, (input_types', output_type), upd_info, false)) eh fi

(* Build a core overloaded function call *)

let build_core_overloaded_call norm_ctxt fn args eh fi =
  let arity = List.length args in
  let sig_table = Norm_overloaded.table_for_overloaded_function norm_ctxt (fn,arity) in
  (* Printf.printf "[DEBUG] Building overloaded call for %s\n" (prefixed_string_of_rqname fn); *)
  fmkcexpr (CEOverloadedCall (fn, args, sig_table)) eh fi

(* Build a core expressions for true() and false () *)

let build_core_true norm_ctxt eh fi =
  build_core_call norm_ctxt Namespace_builtin.fn_true [] eh fi

let build_core_false norm_ctxt eh fi =
  build_core_call norm_ctxt Namespace_builtin.fn_false [] eh fi

(* Cast 

   From F&O 17.1.1 Casting from xs:string and xs:untypedAtomic:

   Casting is permitted from xs:string and xs:untypedAtomic to any
   primitive atomic type or any atomic type derived by restriction,
   except xs:QName or xs:NOTATION. Casting to xs:NOTATION is not
   permitted because it is an abstract type.

   Casting is permitted from xs:string literals to xs:QName and types
   derived from xs:NOTATION. If the argument to such a cast is
   computed dynamically, [err:XPTY0004]XP is raised if the value is of
   any type other than xs:QName or xs:NOTATION respectively (including
   the case where it is an xs:string).
*)
let build_core_cast norm_ctxt cexpr1 (cdt, cty) eh fi =
  (* 1. accesses the schema from the normalization context *)
  let cxschema = cxschema_from_norm_context norm_ctxt in
  (* 2. checks that the sequence type is an optional atomic type *)
  let _ = Schema_norm_util.check_optional_atomic_type_csequencetype cxschema cdt in
  let at = Schema_norm_util.atomic_type_of_csequencetype cxschema cdt in 
  (* Here, we have to check that type is derived from xs:NOTATION *)
  if (at = ATNOTATION) then 
    raise (Query(Cast_Error("err:XPTY0004: Cast to xs:NOTATION not permitted")))
(*
  else if (at = ATQName && not is_string_literal) then 
    raise (Query(Cast_Error("err:XPTY0004: Cast to xs:QName only permitted on string literals")))
*)
  else
  (* 3. builds the core cast expression *)
    let nsenv  = nsenv_from_norm_context norm_ctxt in
    fmkcexpr (CECast (cexpr1, nsenv, (cdt, cty))) eh fi

(*

  From XQuery 3.12.4 Castable

  The target type must be an atomic type that is in the in-scope
  schema types and is not xs:NOTATION or xs:anyAtomicType, optionally
  followed by the occurrence indicator "?" to denote that an empty
  sequence is permitted [err:XPST0080].

*)
let build_core_castable norm_ctxt cexpr1 (cdt, cty) eh fi =
  (* 1. accesses the schema from the normalization context *)
  let cxschema = cxschema_from_norm_context norm_ctxt in
  (* 2. checks that the sequence type is an optional atomic type *)
  let _ = Schema_norm_util.check_optional_atomic_type_csequencetype cxschema cdt in
  let at = Schema_norm_util.atomic_type_of_csequencetype cxschema cdt in 
  if (at = ATNOTATION || at = ATAnyAtomic) then 
    raise (Query(Cast_Error("err:XPST0080: Castable cannot be applied to xs:NOTATION or xs:anyAtomicType")))
  else
  (* 3. builds the core castable expression *)
    let nsenv  = nsenv_from_norm_context norm_ctxt in
    fmkcexpr (CECastable (cexpr1, nsenv, (cdt, cty))) eh fi


(*********************************************)
(* Normalizes a core sequencetypes to a type *)
(*********************************************)

(* Mapping table from common sequence types to core types *)

let normalize_element_test_to_cxtype cxschema celement_test fi =
  match celement_test with
  | (CSchemaElementTest cename) ->
      let cename = Namespace_symbols.relem_symbol cename in
      fmkcxtype (CElementRef cename) fi
  | (CElementTest (Some (cename,Some ctname))) ->
      let cename = Namespace_symbols.relem_symbol cename in
      let ctname = Namespace_symbols.rtype_symbol ctname in
      fmkcxtype (CElementLocal (cename,NonNillable,ctname)) fi
  | (CElementTest (Some (cename,None))) ->
      let cename = Namespace_symbols.relem_symbol cename in
      let ctname = Namespace_symbols_builtin.xs_anyType in
      fmkcxtype (CElementLocal (cename,Nillable,ctname)) fi
  | CElementTest None -> cxtype_element

let normalize_attribute_test_to_cxtype cxschema cattribute_test fi =
  match cattribute_test with
  | (CSchemaAttributeTest caname) ->
      let caname = Namespace_symbols.rattr_symbol caname in
      fmkcxtype (CAttributeRef caname) fi
  | (CAttributeTest (Some (caname,None))) ->
      let caname = Namespace_symbols.rattr_symbol caname in
      let ctname = Namespace_symbols_builtin.xs_anySimpleType in
      fmkcxtype (CAttributeLocal (caname,ctname)) fi
  | (CAttributeTest (Some (caname,Some ctname))) ->
      let caname = Namespace_symbols.rattr_symbol caname in
      let ctname = Namespace_symbols.rtype_symbol ctname in
      fmkcxtype (CAttributeLocal (caname,ctname)) fi
  | CAttributeTest None -> cxtype_attribute

let normalize_ckind_test_to_cxtype cxschema ckind_test fi =
  match ckind_test with
  | CDocumentKind None -> cxtype_documentnode
  | CDocumentKind (Some celement_test) ->
      let elem_model = normalize_element_test_to_cxtype cxschema celement_test fi in
      fmkcxtype (CDocument elem_model) fi
  | CElementKind celement_test ->
      normalize_element_test_to_cxtype cxschema celement_test fi
  | CAttributeKind cattribute_test ->
      normalize_attribute_test_to_cxtype cxschema cattribute_test fi
  | CPIKind None -> cxtype_pi
  | CPIKind (Some n) -> cxtype_pi_name n
  | CCommentKind -> cxtype_comment
  | CTextKind -> cxtype_text
  | CAnyKind -> cxtype_node

let normalize_core_sequencetype_to_cxtype cxschema dt =
  (* Map the sequencetype kind *)
  let (dtk,occ) = dt.pcsequencetype_desc in
  let mapped_dtk = 
    match dtk with
    | CITEmpty -> cxtype_empty; 
    | CITNumeric -> cxtype_numeric;
    | CITAnyString -> cxtype_anystring;
    | CITItem -> cxtype_item;
    | CITAtomic ctname ->
	let a = Namespace_symbols.rtype_symbol ctname in
	Schema_util.make_atomic_type cxschema a
    | CITKindTest ckind_test ->
	let fi = dt.pcsequencetype_loc in
	normalize_ckind_test_to_cxtype cxschema ckind_test fi
    | CITTypeRef ctname ->
	raise (Query (Static_Internal ("Cannot deal with type references in sequencetype")))
  in
  match occ with
  | None ->
      mapped_dtk
  | Some (m,n) ->
      (* Cheating a bit here as we know that sequence types are already in their simplest form *)
      fmkcxtype_builtin (CBound (mapped_dtk,m,n)) 

(***********************************************)
(* Normalize a kind test into a core kind test *)
(***********************************************)

let normalize_type_qname norm_context tname = 
  let nsenv  = nsenv_from_norm_context norm_context in
  let ctname = Namespace_resolve.resolve_type_qname nsenv tname in
  let cxschema  = cxschema_from_norm_context norm_context in
  let _ = Schema_judge.lookup_type_decl cxschema (Namespace_symbols.rtype_symbol ctname) in
  ctname

let normalize_element_test norm_context element_test =
  let nsenv  = nsenv_from_norm_context norm_context in
  match element_test with
  | SchemaElementTest ename ->
      let cename = Namespace_resolve.resolve_element_qname nsenv ename in
      CSchemaElementTest cename
  | ElementTest None ->
      CElementTest None
  | ElementTest (Some (ename,None)) ->
      let cename = Namespace_resolve.resolve_element_qname nsenv ename in
      CElementTest (Some (cename,None))
  | ElementTest (Some (ename,Some tname)) ->
      let cename = Namespace_resolve.resolve_element_qname nsenv ename in
      let ctname = normalize_type_qname norm_context tname in 
      CElementTest (Some (cename,Some ctname))

let normalize_attribute_test norm_context attribute_test =
  let nsenv  = nsenv_from_norm_context norm_context in
  match attribute_test with
  | SchemaAttributeTest aname ->
      let caname = Namespace_resolve.resolve_attribute_qname nsenv aname in
      CSchemaAttributeTest caname
  | AttributeTest None ->
      CAttributeTest None
  | AttributeTest (Some (aname,None)) ->
      let caname = Namespace_resolve.resolve_attribute_qname nsenv aname in
      CAttributeTest (Some (caname,None))
  | AttributeTest (Some (aname,Some tname)) ->
      let caname = Namespace_resolve.resolve_attribute_qname nsenv aname in
      let ctname = normalize_type_qname norm_context tname in
      CAttributeTest (Some (caname,Some ctname))

let normalize_kind_test norm_context kind_test =
  let schema  = cxschema_from_norm_context norm_context in
  let ckind_test = 
    match kind_test with
    | DocumentKind None ->
	CDocumentKind None
    | DocumentKind (Some et) ->
	CDocumentKind (Some (normalize_element_test norm_context et))
    | ElementKind et ->
	CElementKind (normalize_element_test norm_context et)
    | AttributeKind at ->
	CAttributeKind (normalize_attribute_test norm_context at)
    | PIKind pik ->
	CPIKind pik
    | CommentKind ->
	CCommentKind
    | TextKind ->
	CTextKind
    | AnyKind ->
	CAnyKind
  in (ckind_test, normalize_ckind_test_to_cxtype schema ckind_test Finfo.bogus)


(*******************************************************)
(* Normalize a sequence type into a core sequence type *)
(*******************************************************)

let normalize_sequencetype norm_context dt =
  let schema  = cxschema_from_norm_context norm_context in
  let (dtk,occ) = dt.psequencetype_desc in
  let mapped_dtk =
    match dtk with
    | ITKindTest kind_test ->
	let (ckt, cty) = (normalize_kind_test norm_context kind_test) in
	CITKindTest ckt
    | ITTypeRef tname ->
	let ctname = normalize_type_qname norm_context tname in
	CITTypeRef ctname
    | ITItem ->
	CITItem
    | ITNumeric ->
	CITNumeric
    | ITAnyString ->
	CITAnyString
    | ITAtomic st ->
	let ctname = normalize_type_qname norm_context st in
      	CITAtomic ctname
    | ITEmpty ->
	CITEmpty
  in
  let cdt = fmkcsequencetype (mapped_dtk,occ) dt.psequencetype_loc in 
  let cty = normalize_core_sequencetype_to_cxtype schema cdt in 
  (cdt, cty)

let normalize_optional_sequencetype norm_context odt = 
  match odt with
  | None -> None
  | Some dt -> Some (normalize_sequencetype norm_context dt)

let make_sequencetype d =
  fmksequencetype
    (d, Some Occurrence.star)
    Finfo.bogus

let anytype = make_sequencetype ITItem

let normalize_optional_sequencetype_strong norm_context odt = 
  match odt with
  | None -> normalize_sequencetype norm_context anytype
  | Some dt -> normalize_sequencetype norm_context dt


(*************************)
(* Useful for predicates *)
(*************************)

type predicate_kind = 
  | First
  | Last
  | Numeric
  | Other

let get_predicate_kind ce = 
  match ce.pcexpr_desc with
  | CEScalar l ->
      begin
	match l with
	| IntegerLiteral i ->
	    if (Decimal._integer_eq i Decimal._integer_one) then First else Numeric
	| DoubleLiteral _
	| DecimalLiteral _ (* OtherNumeric *)
	| BooleanLiteral _
	| StringLiteral _
	| URILiteral _ -> Other
      end
  | CEVar vname -> if vname = fs_last then Last else Other
  | _ -> Other


(***************)
(* Atomization *)
(***************)

(* normalize_atomize is a call to fn:data() *)

let normalize_atomize norm_ctxt cexpr1 eh fi = 
  build_core_call norm_ctxt Namespace_builtin.fn_data [ cexpr1 ] eh fi

(* 
   normalize_simple_operand is a call to fs:convert-simple-operand(). 

   It takes a model of the expected type, and looks up a prototypical
   value of this expected type.  If a prototype value exists,
   fs:convert-simple-operand is applied to the actual argument and the
   prototype value. If no such prototype value exists, it simply
   returns the argument.  Right now, we only handle expected types:
   string, all numerics, and boolean.  

   This function assumes that the expected type, m, is a singleton
   atomic type (not a union of atomic types), otherwise it would not
   be possible to select a prototype value.

*)

let normalize_simple_operand norm_ctxt cexpr1 atomic_type eh fi = 
  (* If the target type is xs:anyAtomicType, then we are done: *)
  if (atomic_type = ATAnyAtomic)
  then cexpr1
  else
    let proto_value = fmkcexpr (CEProtoValue atomic_type) eh fi in 
    build_core_call norm_ctxt Namespace_builtin.fs_convert_simple_operand [ cexpr1; proto_value] eh fi

(***********)
(* Casting *)
(***********)

let normalize_ident_to_let norm_ctxt vname1 cexpr1 cexpr2 eh fi =
  fmkcexpr (CEFLWOR ([CELET (None, vname1, cexpr1)],None,None,cexpr2)) eh fi

(* normalize_to_cast does atomization; build_core_cast does not *)

let normalize_to_cast norm_ctxt cexpr1 ctpair eh fi =
  (* 1. atomize the input expression *)
  let atom_cexpr = normalize_atomize norm_ctxt cexpr1 eh fi in
  (* 2. build a new variable expression *)
  let (e1v, e1var) = gen_new_cvar norm_ctxt eh fi in
  (* 3. build the cast as expression with that variable *)
  let cast_cexpr = build_core_cast norm_ctxt e1var ctpair eh fi in
  (* 4. build the final let expression *)
  normalize_ident_to_let norm_ctxt e1v atom_cexpr cast_cexpr eh fi

(* normalize_to_castable does atomization; build_core_cast does not *)

let normalize_to_castable norm_ctxt cexpr1 ctpair eh fi = 
  (* 1. atomize the input expression *)
  let atom_cexpr = normalize_atomize norm_ctxt cexpr1 eh fi in
  (* 2. build a new variable expression *)
  let (e1v, e1var) = gen_new_cvar norm_ctxt eh fi in
  (* 3. build the castable as expression with that variable *)
  let castable_cexpr = build_core_castable norm_ctxt e1var ctpair eh fi in
  (* 4. build the final let expression *)
  normalize_ident_to_let norm_ctxt e1v atom_cexpr castable_cexpr eh fi


(****************)
(* FS functions *)
(****************)

let item_seq_to_untyped norm_ctxt cexpr eh fi =
  build_core_call norm_ctxt Namespace_builtin.fs_item_sequence_to_untypedAtomic [ cexpr ] eh fi

let item_seq_to_untyped_optional norm_ctxt cexpr eh fi =
  build_core_call norm_ctxt Namespace_builtin.fs_item_sequence_to_untypedAtomic_optional [ cexpr ] eh fi

let item_seq_to_node_seq norm_ctxt cexpr eh fi =
  build_core_call norm_ctxt Namespace_builtin.fs_item_sequence_to_node_sequence [ cexpr ] eh fi

(**************)
(* Predicates *)
(**************)

(* normalize_effective_boolean_value takes an expression and returns a
   boolean value. It is nothing but a call to fn:boolean.

   -- see also: normalize_predicate_truth_value

  The EBV of an expression is false if the expression is:
  * The empty sequence
  * The singleton boolean value false
  * The singleton string value ""
  * A singleton numeric value that is numerically equal to zero or a float or double equal to NaN
  Otherwise, it is true.

  We have not implemented comparison ($v != NaN)
*)

let normalize_effective_boolean_value nc e1 eh fi = 
  build_core_call nc Namespace_builtin.fn_boolean [ e1 ] eh fi

(* normalize_predicate_truth_value implements the semantics for
   StepQualifiers.  -- see also: normalize_effective_boolean_value

  The predicate truth value is derived by applying the following
  rules, in order:

    1. If the value of the predicate expression is one simple value of
      a numeric type, the value is rounded to an integer using the
      rules of the round function, and the predicate truth value is
      true if and only if the resulting integer is equal to the value
      of the context position.

    2. Otherwise, the predicate truth value is the effective boolean
       value of the predicate expression.

  Formally: 

    [Expr]_PredicateTruthValue = 
    typeswitch ([Expr]) 
    case integer $v return position() = $v
    case numeric $v return xsd:double(position()) = round(cast as xsd:double $v)
    default $v return [$v]_EffectiveBooleanValue

*)

let normalize_predicate_truth_value norm_ctxt e1 pos_var eh fi = 
(*  let (v0, var0) = gen_new_cvar eh fi in *)
  let (v1, var1) = gen_new_cvar norm_ctxt eh fi in
  let (v2, var2) = gen_new_cvar norm_ctxt eh fi in

  let double_type  = (fmkcsequencetype (CITAtomic Namespace_builtin.xs_double, None) fi, Schema_builtin.cxtype_double) in
  let numeric_sequencetype = (fmkcsequencetype (CITNumeric, None) fi, Schema_builtin.cxtype_numeric) in

  let cast_expr  = build_core_cast norm_ctxt var1 double_type eh fi in
(*  let round_expr = build_core_call norm_ctxt Namespace_builtin.fn_round_double [cast_expr] eh fi in *)
  let cast_expr' = build_core_cast norm_ctxt pos_var double_type eh fi in 
  let pos_expr   =
    build_core_call norm_ctxt Namespace_builtin.op_double_equal [cast_expr'; cast_expr] eh fi
  in
  let default_expr = normalize_effective_boolean_value norm_ctxt var2 eh fi in
  let patlist = [
(*  (fmkcpattern (CCase integer_type) fi, Some v0, pos_integer_expr); *)
    (fmkcpattern (CCase numeric_sequencetype) fi, Some v1, pos_expr);
    (fmkcpattern CDefault fi, Some v2, default_expr)
  ]
  in
  fmkcexpr (CETypeswitch (e1, patlist)) eh fi


(************************)
(* Quantification/Tests *)
(************************)

let normalize_to_if norm_ctxt cond e1 e2 eh fi = 
  let cond' = normalize_effective_boolean_value norm_ctxt cond eh fi in
  build_core_if norm_ctxt cond' e1 e2 eh fi

let normalize_to_while norm_ctxt cond e1 eh fi = 
  let cond' = normalize_effective_boolean_value norm_ctxt cond eh fi in
  build_core_while norm_ctxt cond' e1 eh fi

let normalize_to_some norm_ctxt rodt v e1 e2 eh fi = 
  let e2' = normalize_effective_boolean_value norm_ctxt e2 eh fi in
  build_core_some norm_ctxt rodt v e1 e2' eh fi

let normalize_to_every norm_ctxt rodt v e1 e2 eh fi = 
  let e2' = normalize_effective_boolean_value norm_ctxt e2 eh fi in 
  build_core_every norm_ctxt rodt v e1 e2' eh fi


(*************)
(* Operators *)
(*************)

(* normalize_unary_operator 

    [UnaryOp Expr] ==
    let $e := op:numeric-UnaryOp(fs:untyped-to-double(fn:data(Expr)))
*)

let normalize_unary_operator nc uop e1 eh fi = 
  let fn =
    begin
      match uop with 
      | UEPlus  -> Namespace_builtin.op_numeric_unary_plus        (* +e1 *)
      | UEMinus -> Namespace_builtin.op_numeric_unary_minus       (* -e1 *) 
    end
  in
  let atom_expr = normalize_atomize nc e1 eh fi  in
  let conv_expr = build_core_call nc Namespace_builtin.fs_untyped_to_double [ atom_expr ] eh fi in
  build_core_overloaded_call nc fn [ conv_expr ] eh fi

(* normalize_node_comp_operator

3.5.3 Node Comparisons

   The result of a node comparison is defined by applying the
   following rules, in order:

   1. Each operand must be either a single node or an empty sequence;
      otherwise a type error is raised.

   2. If either operand is an empty sequence, the result of the
      comparison is an empty sequence.

   3. A comparison with the is operator is true if the two operands
      are nodes that have the same identity; otherwise it is false. A
      comparison with the isnot operator is true if the two operands
      are nodes that have different identities; otherwise it is
      false. See [XQuery 1.0 and XPath 2.0 Data Model] for a
      discussion of node identity.

3.5.4 Order Comparisons

   The result of an order comparison is defined by applying the
   following rules, in order:

   1. Both operands must be either a single node or an empty sequence;
      otherwise a type error is raised.

   2. If either operand is an empty sequence, the result of the
      comparison is an empty sequence.

   3. A comparison with the << operator returns true if the first
      operand node is earlier than the second operand node in document
      order; otherwise it returns false.

   4.  A comparison with the >> operator returns true if the first
       operand node is later than the second operand node in document
       order; otherwise it returns false.

  [ Expr1 NodeOp Expr2 ] == op:NodeOp($v1, $v2)

*)

let normalize_node_comp_operator nc fn e1 e2 eh fi = 
  build_core_call nc fn [ e1; e2 ] eh fi 

(*

What the spec says:

  3.5.2 General Comparisons

  Atomization is applied to each operand of a general comparison.  The
  magnitude relationship between two atomic values is determined as
  follows:

  1. If either atomic value has the dynamic type xs:untypedAtomic,
     that value is cast to a required type, which is determined as
     follows:
     1. If the dynamic type of the other atomic value is a numeric
        type, the required type is xs:double.
     2. If the dynamic type of the other atomic value is
        xs:untypedAtomic, the required type is xs:string.
     3. Otherwise, the required type is the dynamic type of the other
        atomic value.

      If the cast to the required type fails, a dynamic error is
      raised.[err:XP0021]

What we implement:
(If both args are untypedAtomic, we cast both to string.
 If one arg is untypedAtomic and the other is numeric, we cast untypedAtomic to double.
 If one arg is untypedAtomic and the other is string, we cast untypedAtomic to string.
 Type promotion during function application does the rest of the work for us.)

  [Expr1 GeneralOp Expr2] ==  
  some $e1v in fn:data([Expr1]) satisfies
    some $e2v in fn:data([Expr2]) satisfies
        [GeneralOp]GeneralOp (fs:untyped_to_any($e1v, $e2v),
                              fs:untyped_to_any($e2v, $e1v))
*)

let normalize_general_comp_operator nc fn e1 e2 eh fi = 
  let (e1v, e1var) = gen_new_cvar nc eh fi in
  let (e2v, e2var) = gen_new_cvar nc eh fi in

  let conv_fn1 = build_core_call nc Namespace_builtin.fs_untyped_to_any [ e1var; e2var ] eh fi in 
  let conv_fn2 = build_core_call nc Namespace_builtin.fs_untyped_to_any [ e2var; e1var ] eh fi in 
  let comp_fn = build_core_overloaded_call nc fn [ conv_fn1; conv_fn2 ] eh fi in

  let some_expr2 = normalize_to_some nc None e2v (normalize_atomize nc e2 eh fi) comp_fn eh fi in
  let some_expr1 = normalize_to_some nc None e1v (normalize_atomize nc e1 eh fi) some_expr2 eh fi in
  some_expr1

(*  normalize_value_comp_operator

  3.5.1 Value Comparisons

  Value comparisons are intended for comparing single values. The
  result of a value comparison is defined by applying the following
  rules, in order:
  
  1. Atomization is applied to each operand. If the result, called an
     atomized operand, is not a single atomic value, a type error is
     raised.

  2. If either atomized operand is an empty sequence, a type error is raised. 

  3. If either atomized operand has type xs:untypedAtomic, that
     atomized operand (those atomized operands) is (are) cast to
     xs:string.

  4. The result of the comparison is true if the first atomized
     operand is (equal, not equal, less than, less than or equal,
     greater than, greater than or equal) to the second atomized
     operand; otherwise the result of the comparison is false. B.2
     Operator Mapping describes which combinations of atomic types are
     comparable, and how comparisons are performed on values of
     various types. If the first atomized operand is not comparable
     with the second atomized operand, a type error is raised.

  ==========================================

    [Expr1 ValueEqOp Expr2] ==

    op:ValueEqOp(fs:untyped-to-string(fn:data(Expr1)),
                 fs:untyped-to-string(fn:data(Expr2))

*)

let normalize_value_comp_operator nc fn e1 e2 eh fi = 

  let atom_expr1 = (normalize_atomize nc e1 eh fi) in
  let conv_expr1 = build_core_call nc Namespace_builtin.fs_untyped_to_string [ atom_expr1 ] eh fi in 
  let atom_expr2 = (normalize_atomize nc e2 eh fi) in
  let conv_expr2 = build_core_call nc Namespace_builtin.fs_untyped_to_string [ atom_expr2 ] eh fi in 

  build_core_overloaded_call nc fn [ conv_expr1; conv_expr2 ] eh fi

(*  
  3.4 Arithmetic Expressions

  An arithmetic expression is evaluated by applying the following
  rules, in order, until an error is raised or a value is computed:

  1. Atomization is applied to each operand. If the resulting value is
     not an empty sequence or a single atomic value, then a type error
     is raised.

  2. If either operand is an empty sequence, the result of the
     operation is an empty sequence.

  3. If an operand has type xs:untypedAtomic, it is
     cast to xs:double. If the cast fails, a type error is raised.

  4. If the operand type(s) are valid for the given operator, the
     operator is applied to the operand(s), resulting in an atomic
     value or a dynamic error (for example, an error might result from
     dividing by zero.)  If the operand type(s) are not valid for the
     given operator, a type error is raised.

  normalize_arithmetic_operator
      
  We use fs:untyped-to-double(Expr) in lieu of fs:convert-operand (Expr, double) 

      [Expr1 ArithOp Expr2] ==
      op:numeric-ArithOp(fs:untyped-to-double(fn:data(Expr1)),
                         fs:untyped-to-double(fn:data(Expr2))

*)

let normalize_arithmetic_operator nc fn e1 e2 eh fi = 
  let atom_expr1 = (normalize_atomize nc e1 eh fi) in
  let conv_expr1 = build_core_call nc Namespace_builtin.fs_untyped_to_double [ atom_expr1 ] eh fi in 

  let atom_expr2 = (normalize_atomize nc e2 eh fi) in
  let conv_expr2 = build_core_call nc Namespace_builtin.fs_untyped_to_double [ atom_expr2 ] eh fi in 

  build_core_overloaded_call nc fn [ conv_expr1; conv_expr2 ] eh fi

(* 
   normalize_idiv_operator

      [Expr1 idiv Expr2] ==
      op:numeric-idivide(fs:untyped-to-integer(fn:data(Expr1)),
               fs:untyped-to-integer(fn:data(Expr2))

*)

let normalize_idiv_operator nc e1 e2 eh fi = 
  let atom_expr1 = (normalize_atomize nc e1 eh fi) in
  let conv_expr1 = build_core_call nc Namespace_builtin.fs_untyped_to_integer [ atom_expr1 ] eh fi in 

  let atom_expr2 = (normalize_atomize nc e2 eh fi) in
  let conv_expr2 = build_core_call nc Namespace_builtin.fs_untyped_to_integer [ atom_expr2 ] eh fi in 

  build_core_overloaded_call nc Namespace_builtin.op_numeric_idivide [ conv_expr1; conv_expr2 ] eh fi

let normalize_binary_operator nc b e1 e2 eh fi = 
  begin
    match b with
          (* Sequence operators *)
          (* e1 intersect e2 *)
    | BEIntersect ->
	let intersect_expr = build_core_call nc Namespace_builtin.op_intersect [e1;e2] eh fi  in
	build_core_call nc Namespace_builtin.fs_distinct_docorder [intersect_expr] eh fi
          (* e1 union e2 *)
    | BEUnion ->
	let union_expr = build_core_call nc Namespace_builtin.op_union [e1;e2] eh fi in
	build_core_call nc Namespace_builtin.fs_distinct_docorder [union_expr] eh fi
          (* e1 except e2 *)
    | BEExcept ->
	let except_expr = build_core_call nc Namespace_builtin.op_except [e1;e2] eh fi in
	build_core_call nc Namespace_builtin.fs_distinct_docorder [except_expr] eh fi
          (* e1 | e2 *)
    | BEBar ->
	let union_expr = build_core_call nc Namespace_builtin.op_union [e1;e2] eh fi in
	build_core_call nc Namespace_builtin.fs_distinct_docorder [union_expr] eh fi
          (* Logical operators : use short circuit semantics *)
    | BEAnd ->
          (* e1 and e2 == if (e1) then e2 else false() *)
	  (* normalize_to_if applies EBV to e1 *)
        let e2' = normalize_effective_boolean_value nc e2 eh fi in 
	normalize_to_if nc e1 e2' (build_core_false nc eh fi) eh fi 
    | BEOr -> 
          (* e1 or e2 == if (e1) then true() else e2 *)
	  (* normalize_to_if applies EBV to e1 *)
        let e2' = normalize_effective_boolean_value nc e2 eh fi in 
	normalize_to_if nc e1 (build_core_true nc eh fi) e2' eh fi 
          (* Order operators *)
          (* e1 << e2 *)
    | BEPrecedes -> 
	normalize_node_comp_operator nc Namespace_builtin.op_node_before e1 e2 eh fi 
          (* e1 >> e2 *)
    | BEFollows -> 
	normalize_node_comp_operator nc Namespace_builtin.op_node_after e1 e2 eh fi 
          (* ValueComp operators *)
          (* e1 eq e2 *)
    | BEEq -> 
	normalize_value_comp_operator nc Namespace_builtin.op_equal e1 e2 eh fi
           (* e1 neq e2 *)
    | BENEq -> 
	normalize_value_comp_operator nc Namespace_builtin.op_nequal e1 e2 eh fi
          (* e1 lt e2 *)
    | BELtOp -> 
	normalize_value_comp_operator nc Namespace_builtin.op_lt e1 e2 eh fi                     
          (* e1 gt e2 *)
    | BEGtOp -> 
	normalize_value_comp_operator nc Namespace_builtin.op_gt e1 e2 eh fi                     
          (* e1 le e2 *)
    | BELte -> 
	normalize_value_comp_operator nc Namespace_builtin.op_le e1 e2 eh fi                     
          (* e1 ge e2 *)
    | BEGte -> 
	normalize_value_comp_operator nc Namespace_builtin.op_ge e1 e2 eh fi                     
	  
          (* GeneralComp operators *)
          (* e1 = e2 *)
    | BEEqual -> 
        normalize_general_comp_operator nc Namespace_builtin.op_equal e1 e2 eh fi 
          (* e1 != e2 *)
    | BENEqual ->  
	normalize_general_comp_operator nc Namespace_builtin.op_nequal e1 e2 eh fi 
          (* e1 < e2 *)
    | BELt -> 
	normalize_general_comp_operator nc Namespace_builtin.op_lt e1 e2 eh fi 
          (* e1 > e2 *)
    | BEGt -> 
	normalize_general_comp_operator nc Namespace_builtin.op_gt e1 e2 eh fi 
          (* e1 <= e2 *)
    | BELteq -> 
	normalize_general_comp_operator nc Namespace_builtin.op_le e1 e2 eh fi 
          (* e1 >= e2 *)
    | BEGteq -> 
	normalize_general_comp_operator nc Namespace_builtin.op_ge e1 e2 eh fi 
          (* NodeComp operators *)
          (* e1 == e2 *)
    | BEIs -> 
	normalize_node_comp_operator nc Namespace_builtin.op_is_same_node e1 e2 eh fi 
          (* e1 !== e2 *)
    (* Overloaded Arithmetic *)
    | BEMult ->
        normalize_arithmetic_operator nc Namespace_builtin.op_numeric_multiply e1 e2 eh fi
          (* e1 + e2 *)
    | BEPlus ->
        normalize_arithmetic_operator nc Namespace_builtin.op_numeric_add e1 e2 eh fi
          (* e1 - e2 *)
    | BEMinus ->
        normalize_arithmetic_operator nc Namespace_builtin.op_numeric_subtract e1 e2 eh fi
          (* e1 div e2 *)
    | BEDiv ->
        normalize_arithmetic_operator nc Namespace_builtin.op_numeric_divide e1 e2 eh fi
          (* e1 idiv e2 *)
    | BEIDiv ->
        normalize_idiv_operator nc e1 e2 eh fi
          (* e1 mod e2 *)
    | BEMod ->
        normalize_arithmetic_operator nc Namespace_builtin.op_numeric_mod e1 e2 eh fi
  end


(******************)
(* Function calls *)
(******************)

(*
  Non-overloaded function calls

  1. Look up signature of function using function name

    QName -> (SequenceType1, ..., SequenceTypen)

  2. Normalize function call


                    [  QName (Expr1, ..., Exprn) ]Expr   
                                  ==  
    QName  ([Expr1]FunctionArgument(SequenceType1) : SequenceType1,  
             ..., 
            [Expr1]FunctionArgument(SequenceTypen) : SequenceTypen)

    The argument type assertions are included to guarantee that the
    run-time type of the argument matches the expected parameter type.


                       [Expr]FunctionArgument(SequenceType)   
                                        ==  
    [[[Expr]AtomizeAtomic(SequenceType)]Convert(SequenceType)]Promote(SequenceType)

  3. Normalize each function argument, based on known parameter type

    3a. Atomization

      [Expr]AtomizeAtomic(SequenceType) denotes

        fn:data(Expr)  If SequenceType <: xs:anyAtomic* and Expr : Type and not(Type=empty)
        Expr           Otherwise

      which specifies that if the function expects atomic parameters, then fn:data is called to obtain them.

    3b. Convert untypedAtomic to target type

      [Expr]Convert(SequenceType) denotes
  
        fs:convert-simple-operand(Expr,PrototypicalValue) If SequenceType <: xs:anyAtomicType*
                                                          and prime(SequenceType) = (baseType, m, n)
        Expr                                              Otherwise

      where PrototypicalValue is an instance of baseType.

    3c. Promote argument to target type, if necessary

      [Expr]Promote(SequenceType) denotes  

        fs:promote-operand(Expr,PrototypicalValue) If SequenceType <: fs:numeric 
        Expr                                       Otherwise

    NB: [Expr]Convert(SequenceType) and [Expr]Promote(SequenceType)
    could be elided into one function. 

*)
let convert_function_input_output norm_context ((v, var), (act_cexpr, (form_cdt, form_ty))) cexpr =
  let eh = act_cexpr.pcexpr_origin in
  let fi = act_cexpr.pcexpr_loc in
  let cxschema = Norm_context.cxschema_from_norm_context norm_context in

(* Working with csequenctypes here instead of the actual types if very
   messy.  We should get away from using csequencetypes -Mary *)

  let normalize_atomize_arg norm_context act_cexpr =
    if (Schema_norm_util.is_simple_type_csequencetype cxschema form_cdt)
    then
      let cexpr_atomize =
	normalize_atomize norm_context act_cexpr eh fi
      in
      if (Schema_norm_util.is_atomic_type_csequencetype cxschema form_cdt)
      then
	begin
	  let atomic_type =
	    Schema_norm_util.atomic_type_of_csequencetype cxschema form_cdt
	  in

	  let opd = normalize_simple_operand norm_context cexpr_atomize atomic_type eh fi in
	  if (Datatypes_util.atomic_is_numeric atomic_type)
	  then
	    begin
	      let proto_value = fmkcexpr (CEProtoValue atomic_type) eh fi in
	      build_core_call norm_context Namespace_builtin.fs_promote_to_numeric [ opd; proto_value ] eh fi
	    end
	  else if (Datatypes_util.atomic_is_anyURI atomic_type)
	  then opd
	  else if (Datatypes_util.atomic_is_anystring atomic_type)
	  then
	    build_core_call norm_context
	      Namespace_builtin.fs_promote_to_anystring [ opd ] eh fi
	  else
	    opd
	end
      else
	cexpr_atomize
    else
      act_cexpr
  in
  fmkcexpr (CEFLWOR ([CELET (None, v, normalize_atomize_arg norm_context act_cexpr)],None,None,cexpr)) eh fi

let check_server_implementation norm_context fi ncv = 
  let nsenv = nsenv_from_norm_context norm_context in
  let uri = string_of_uri(Namespace_context.get_ns_of_prefix nsenv (NSServerPrefix ncv)) in
  let (nc1v, _) = 
    try 
      resolve_variable_qname_check norm_context fi (fs_prefix, ncv)
    with
    | Query(Undefined_Variable _) -> 
	raise (Query(DXQ_Error("Server-implementation namespace "^ncv^" undefined.")))
  in (nc1v, uri)

let check_interface norm_context fi ncv = 
  try
    let nsenv = nsenv_from_norm_context norm_context in
    string_of_uri(Namespace_context.get_ns_of_prefix nsenv (NSInterfacePrefix ncv))
  with
  | _ ->  raise (Query(DXQ_Error("Interface namespace "^ncv^" undefined.")))

let normalize_function_call norm_context rfname elist eh fi  =
  try
    let fn_arity = rfname, List.length elist in
    let all_sigs = Norm_context.all_sigs_from_norm_context norm_context fn_arity in
    Debug.print_dxq_debug ("Normalize Call "^(prefixed_string_of_rqname rfname)^" "); 
    Debug.print_dxq_debug ("# sigs =  "^(string_of_int(List.length(all_sigs)))^"\n");
    List.iter (fun (cfname, sign, kind, upd) ->   Debug.print_dxq_debug ((prefixed_string_of_rqname cfname)^":"^(string_of_cfun_kind kind))) all_sigs;
    if (List.length(all_sigs)) > 1 then dump_norm_context norm_context else (); 
    let (input_types, output_type), fun_kind, upd = Norm_context.one_sig_from_norm_context norm_context fn_arity in
    let optinput_types = List.map (fun dtm -> Some dtm) input_types in
    let expr_types = List.combine elist input_types in
    let vars = List.map (fun _ -> gen_new_cvar norm_context eh fi) expr_types in
    let var_expr_types = List.combine vars expr_types in
    let varlist = List.map snd vars in
    let upd_info = if is_bltin_with_side_effects fn_arity then Updating else upd in
    let funapp_expr = fmkcexpr (CECall(rfname, varlist, (optinput_types, output_type), upd_info, false)) eh fi in
    let cecall = List.fold_right (convert_function_input_output norm_context) var_expr_types funapp_expr in
    let (ncv, _, _) = rfname in 
    let cecall' = 
      match (fun_kind, ncv) with
	(* Imported function interface with no definition *) 
      | CEFunctionInterfaceKind, NSServerPrefix ncv ->
	  let (nc1v, uri) = check_server_implementation norm_context fi ncv in
          let nc1var = fmkcexpr (CEVar nc1v) eh fi in 
          (* If not nested within an execute expression, then wrap the function call *)
	  Debug.print_dxq_debug ("In execute expr "^(string_of_bool (get_in_execute_expr norm_context))^"\n"); 
	  if (get_in_execute_expr norm_context) then cecall
	  else fmkcexpr (CEExecute (false, ncv, uri, nc1var, cecall)) eh fi
	      (* What about imported functions with definitions? *) 
      | CEFunctionInterfaceKind, _ ->
	  raise (Query(DXQ_Error("Cannot refer to interface function "^
				 (prefixed_string_of_rqname rfname)^" in expression.")))
      | k, (NSPrefix _ | NSDefaultFunctionPrefix) -> (print_dxq_debug ((string_of_cfun_kind k)^"\n"); cecall)
      |	k, p -> raise (Query(Internal_Error("Malformed reference to function "^
					    (prefixed_string_of_rqname rfname)^"/"^
					    (string_of_prefix p)^"/"^(string_of_cfun_kind k))))
    in
    (* Added - Philippe - sbdo optim
       fn:document and fn:root need an appropriate xpath annot
       
       Note that the annotation is not placed onto the function call
       itself but it is added to the outermost operand conversion. This
       is intentional behavior, not a bug.
     *)
    if Debug.default_debug()
    then Debug.print_default_debug ("End Normalize Call "^(prefixed_string_of_rqname rfname)); 
    cecall'
  with
    e ->
      raise e

(* 

  Overloaded function calls.

  ONLY built-in functions may be overloaded.  Their FIRST argument is
  always a sequence of anyAtomicType and is the polymorphic argument.
  Any other arguments are monomorphic.

  Like overloaded functions, atomization and conversion of
  untypedAtomic to a known target type is applied during
  normalization.

  Unlike non-overloaded functions, for which we can determine the
  expected type at normalization time, we cannot determine the
  expected type of overloaded functions, so promotion is delayed until
  evaluation for these functions. 

  1. Look-up signature of overloaded function

    QName -> (SequenceType1_1, ..., SequenceTypen_1)
             ...
             (SequenceType1_k, ..., SequenceTypen_k)

  2. Normalize function call

                              [  QName (Expr1, ..., Exprn) ]Expr   
                                                ==  
    let $v1 := [Expr1]FunctionArgument(SequenceType1) return
    ... 
    let $vn := [Expr1]FunctionArgument(SequenceTypen) return
    QName  ($v1 , ..., $vn)

    The normalization of an overloaded function call does NOT include
    type assertions, because we do not know until run-time what the
    expected type will be.

                               [Expr]FunctionArgument(SequenceType)   
                                               ==  
                         [[Expr]AtomizeAtomic(SequenceType)]Convert(SequenceType)

  3. Normalize each function argument, based on known parameter type

    3a. Atomization

      [Expr]AtomizeAtomic(SequenceType) denotes

        fn:data(Expr)    If SequenceType <: xs:anyAtomic* and Expr : Type and not(Type=empty)
        Expr             Otherwise

      which specifies that if the function expects atomic parameters, then fn:data is called to obtain them.

    3b. Convert untypedAtomic to target type (which is fixed, based on
        operator or overloaded-function name.)

      [Expr]Convert(SequenceType) denotes

      fs:convert-operand(Expr,PrototypicalValue) 

      where PrototypicalValue is an instance of SequenceType.

*)
let normalize_overloaded_function_call norm_context rfname elist expected_atomic_type eh fi  = 
  let arity = List.length elist in
  let cxschema = Norm_context.cxschema_from_norm_context norm_context in
  let normalize_atomize_arg act_expr (form_cdt, form_cty) = 
    if (Schema_norm_util.is_simple_type_csequencetype cxschema form_cdt) then
      normalize_atomize norm_context act_expr eh fi 
    else act_expr
  in
  let normalize_actual_arg ((v, var), (act_expr, form_ty)) = 
    CELET (None, v, normalize_atomize_arg act_expr form_ty)
  in
  let (input_types, output_type), opt_fun_kind, upd = Norm_context.one_sig_from_norm_context norm_context (rfname,arity) in
  let expr_types = List.combine elist input_types in
  let vars = List.map (fun _ -> gen_new_cvar norm_context eh fi) expr_types in
  let var_expr_types = List.combine vars expr_types in
  let varlist = List.map snd vars in
  (* The FIRST operand is always the polymorphic one *)
  let varlist' = (normalize_simple_operand norm_context (List.hd varlist) expected_atomic_type eh fi):: (List.tl varlist) in
  let funapp_expr = build_core_overloaded_call norm_context rfname varlist' eh fi in
  fmkcexpr (CEFLWOR (List.map normalize_actual_arg var_expr_types,None,None,funapp_expr)) eh fi

let normalize_function_app norm_context rfname elist eh fi  = 
    Debug.print_dxq_debug ("Normalize function app "^(prefixed_string_of_rqname rfname)^"\n"); 
  let arity = List.length elist in
  if (Norm_overloaded.is_overloaded (rfname, arity))
  then 
    let default_atomic_type = Norm_overloaded.lookup_default_atomic_type rfname in
    normalize_overloaded_function_call norm_context rfname elist default_atomic_type eh fi 
  else
    normalize_function_call norm_context rfname elist eh fi 

let normalize_atomic_constructor norm_context rfname celist eh fi  = 
  if (List.length celist) != 1 then
    raise (incorrect_arg_count rfname (List.length celist) 1)
  else
    let cxschema = Norm_context.cxschema_from_norm_context norm_context in
    let sqty = fmkcsequencetype (CITAtomic rfname, Some Occurrence.optional) fi in
    let cty = normalize_core_sequencetype_to_cxtype cxschema sqty in
    normalize_to_cast norm_context (List.nth celist 0) (sqty, cty) eh fi

let normalize_ident_function_app norm_context rfname elist eh fi  = 
  let arity = List.length elist in
  if (Norm_overloaded.is_overloaded (rfname, arity))
  then 
    build_core_overloaded_call norm_context rfname elist eh fi 
  else
    build_core_call norm_context rfname elist eh fi 


let rec expr_may_generate_updates expr = 
  begin
    match expr.pcexpr_desc with
      | CEUnordered exp -> expr_may_generate_updates  exp
      | CEOrdered exp -> expr_may_generate_updates  exp
      | CEFLWOR (fl_expr_lst, Some x', _, x'') -> (List.exists (fl_expr_may_generate_updates ) fl_expr_lst) || expr_may_generate_updates   x' || expr_may_generate_updates   x''
      | CEFLWOR (fl_expr_lst, None, _, x) -> (List.exists (fl_expr_may_generate_updates ) fl_expr_lst) || expr_may_generate_updates  x
      | CEVar _ -> false
      | CEIf (x', x'', x''') -> expr_may_generate_updates  x' || expr_may_generate_updates  x'' || expr_may_generate_updates  x'''
      | CEWhile (x', x'') -> expr_may_generate_updates  x' || expr_may_generate_updates  x''
      | CETypeswitch (x', cases) ->
	  expr_may_generate_updates  x' || (List.exists (fun (_,_,x'') -> expr_may_generate_updates  x'') cases)
      | CECall(_, _, _, Updating,_) -> true
      | CECall(_, exp_list, _, NonUpdating,_) -> 
	  expr_list_may_generate_updates  exp_list
      | CEOverloadedCall(f,exp_list,sigs) -> (* suppose that overloaded internal functions cannot return deltas *)
	  expr_list_may_generate_updates  exp_list
      | CEScalar _ -> false
      | CESeq(x',x'') -> expr_may_generate_updates x' || expr_may_generate_updates x''
      | CEEmpty -> false
      | CEDocument exp -> expr_may_generate_updates exp
      | CEPI(_,_) -> false
      | CEPIComputed(x',x'') -> expr_may_generate_updates x' || expr_may_generate_updates x''
      | CEComment _ -> false	  
      | CECommentComputed exp -> expr_may_generate_updates exp
      | CEText _ -> false
      | CECharRef _ -> false
      | CETextComputed exp -> expr_may_generate_updates exp
      | CEElem(_,_,exp_list) 
      | CEAttr(_, _, exp_list) -> expr_list_may_generate_updates  exp_list
      | CEAnyElem(x',_,_,x'') 
      | CEAnyAttr(x',_,x'') -> expr_may_generate_updates x' || expr_may_generate_updates x'' 
      | CEError(exp_list) -> expr_list_may_generate_updates  exp_list 
      | CETreat(x, _) 
      | CEValidate(_, x) 
      | CECast(x,_,_) 
      | CECastable(x,_,_) -> expr_may_generate_updates  x 
      | CEForwardAxis _  | CEReverseAxis _ -> false	 	  
      | CESome (_, _, x', x'') 
      | CEEvery (_, _, x', x'')
      | CELetServerImplement (_, _, x', x'') 
      | CEExecute (_, _, _, x', x'') -> expr_may_generate_updates x' || expr_may_generate_updates x'' 
      | CEForServerClose (_, _, x)
      | CEEvalClosure x 
      | CECopy x -> expr_may_generate_updates  x
      | CEDelete _ -> true
      | CEInsert(_,_) -> true
      | CERename(_,_,_) -> true
      | CEReplace(_,_,_) -> true
      | CESnap(_,x) -> expr_may_generate_updates x 
      | CEProtoValue _ -> false
      | CELetvar(_,_,x',x'') -> expr_may_generate_updates x' || expr_may_generate_updates x''
      | CESet(_,x) -> expr_may_generate_updates x
      | CEImperativeSeq(x',x'') -> expr_may_generate_updates x' || expr_may_generate_updates x''
  end

and expr_list_may_generate_updates   = function
    [] -> false
  | exp::rest -> expr_may_generate_updates  exp || expr_list_may_generate_updates  rest
      
and fl_expr_may_generate_updates = function
  | CELET(_,_,x) -> expr_may_generate_updates  x
  | CEFOR(_,_,_,x) -> expr_may_generate_updates  x

(* 

   For imported module interface and server declaration, map QNames of
   global functions and variables in the target namespace into names
   that use the import/server prefix.  

   For imported modules, map declarations to indicate that the symbols
   have been imported.

   MFF: It's gross that names in both the declarations and
   norm context need to be mapped!

*)
let map_qnames_in_cinterface (import_prefix, target_uri) (nc, cinterface) = 
  let imported_as_module = 
    match import_prefix with
    | NSPrefix _ -> true
    | _ -> false
  in
  let nc' =  
    build_norm_context
      (processing_context_from_norm_context nc) 
      (nsenv_from_norm_context nc)  
      (cxschema_from_norm_context nc)  
      []
  in
  let map_qname (prefix,uri,ncname) = 
    match prefix with
    | NSInterfacePrefix _ 
    | NSPrefix _ -> (import_prefix,uri,ncname)
    | _ -> (prefix,uri,ncname)
  in
  let map_qname_in_function_decl fdef (nc, fdefs) = 
    let ((qname,arity), args, funsig, funbody, upmod) = fdef.pcfunction_def_desc in
    let (prefix, uri, ncname) = qname in 
    if (uri = target_uri) then 
      begin
	print_dxq_debug ("Mapping "^(prefixed_string_of_rqname qname)^"\n");
      let qname' = map_qname qname in
      let fun_kind', fun_body' = map_cfun_kind imported_as_module funbody in
      let _ = add_sig_to_norm_context_in_place nc ((qname', arity), (qname', funsig, fun_kind', upmod)) in 
      let fdef' = ((qname', arity), args, funsig, fun_body', upmod) in
      (nc,
       { pcfunction_def_desc = fdef';
	 pcfunction_def_loc = fdef.pcfunction_def_loc;
       } :: fdefs)
      end
    else 
      begin
	print_dxq_debug ("Omitting "^(prefixed_string_of_rqname qname)^"\n");
	(nc, fdefs)
      end
  in
  let map_qname_in_variable_decl vdef (nc, vdefs) = 
    let (qname,odt,varbody) = vdef.pcvar_decl_desc in
    let (prefix, uri, ncname) = qname in 
    if (uri = target_uri) then 
      begin
    print_dxq_debug ("Mapping "^(prefixed_string_of_rqname qname)^"\n");
      let qname' = map_qname qname in
      let varbody' = (map_cvar_body imported_as_module varbody) in
      let vdef' = (qname', odt, varbody') in
      let nc' = register_global_var nc qname' varbody' in
      (nc',
       { pcvar_decl_desc = vdef'; 
	 pcvar_decl_loc  = vdef.pcvar_decl_loc
       } :: vdefs)
      end
    else 
      begin
    print_dxq_debug ("Omitting "^(prefixed_string_of_rqname qname)^"\n");
     (nc, vdefs)
      end
  in
  let (nc'', fun_decls) = List.fold_right (map_qname_in_function_decl) cinterface.pcprolog_functions (nc', [] ) in 
  (* Variables are excluded from server declarations...*)
  let (nc''', var_decls) = List.fold_right (map_qname_in_variable_decl) cinterface.pcprolog_vars (nc'', []) 
  in 
  (nc''',
   { pcprolog_functions = fun_decls;
     pcprolog_vars = var_decls;
     pcprolog_servers = [];  (* Server declarations and indices should never occur in interfaces *)
     pcprolog_indices = []
   })

(* Re-map prefixes of global names in a cinterface, which are in imported namespace *)
let lookup_and_map_interface norm_context new_prefix (prefix_kind,uri) = 
	(* Interface and Module imports both return Core interfaces *)
  let interface_table  = interface_table_from_norm_context norm_context in 
  Debug.print_default_debug ("Lookup interface "^uri^"\n");
  try 
    let (modloc, norm_ctxt', cinterface) = Hashtbl.find interface_table (prefix_kind,uri) in 
    map_qnames_in_cinterface (new_prefix, NSUri uri) (norm_ctxt', cinterface) 
  with
  | Not_found -> raise (Query(Internal_Error("Module Interface '"^uri^"' not defined")))

let extend_server_environment global norm_context nc1 nc2 (e, ce) =
  let fi   = e.pexpr_loc in
  let norm_context = if global then norm_context else copy_norm_context norm_context in
  let nsenv = nsenv_from_norm_context norm_context in
  let string_type = 
    (fmkcsequencetype ((CITAtomic Namespace_builtin.xs_string), None) fi, Schema_builtin.cxtype_string)  in
  let ce = normalize_to_cast norm_context ce string_type (Some e) fi in
    (* Lookup the interface for the given server. 
       Add all functions in the interface to the normalization context. 
    *)
  let uri = string_of_uri(Namespace_context.get_ns_of_prefix nsenv (NSInterfacePrefix nc2)) in
  let (interface_norm_ctxt, cinterface) = 
    lookup_and_map_interface norm_context (NSServerPrefix nc1) (NSInterfacePrefix "", uri) in
  merge_imported_norm_context norm_context interface_norm_ctxt;
  
    (* Add server's namespace to namespace environment *)
  let nsenv = Namespace_context.add_all_ns nsenv [(NSServerPrefix nc1, NSUri uri)] in 
  let norm_context = Norm_context.replace_namespace_env_in_norm_context nsenv norm_context in 

  (norm_context, uri, cinterface, ce)

