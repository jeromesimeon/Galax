(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_expr.ml,v 1.49 2008/02/01 02:45:26 mff Exp $ *)

(* Module: Typing_expr
   Description:
     This module implements the static typing feature for expressions.
 *)

open Format

open Error
open Occurrence

open Namespace_names

open Datatypes

open Xquery_common_ast
open Xquery_common_ast_util

open Xquery_core_ast
open Xquery_core_ast_util

open Xquery_type_core_ast
open Xquery_type_core_ast_util

open Schema_builtin
open Schema_util

open Typing_context
open Typing_errors

open Print_top


(***************************)
(* Main static typing code *)
(***************************)

(* Note:
     The following is a large recursive function performing static
     typing inference, and based on the XQuery 1.0 and XPath 2.0
     Formal Semantics.
    - Jerome *)

let check_optional_seqtype typing_ctxt seqtype_opt cetype = 
  begin 
    match seqtype_opt with
    | None -> cetype
    | Some (seqtype, cetype') -> check_type_replace typing_ctxt cetype cetype'
  end
(*****************************)
(* Main dispatching function *)
(*****************************)

let rec compute_type_expr typing_ctxt cexpr =
  let cexpr_desc = cexpr.pcexpr_desc in
  let fi = cexpr.pcexpr_loc in
  let schema = schema_from_static_context typing_ctxt in

  (* The core expression contains the file location that should be
      correlated with errors, so we catch any exceptions here and
      rewrap the exceptions with the file location. *)
  try
    begin
      (* Expressions presented in order in the XQuery Formal Semantics *)
      match cexpr_desc with

     (* FS Section 4.1.1 Literals
	--------------------------------------
	statEnv |- Literal : LiteralType
     *)
      | CEScalar l ->
	  let atomic_type = atomic_type_of_literal l in
	  let atomic_type_name = Datatypes_util.symbol_of_primitive_type atomic_type in
	  make_atomic_type schema atomic_type_name

      | CEProtoValue atomic_type ->
	  let atomic_type_name = Datatypes_util.symbol_of_primitive_type atomic_type in
	  make_atomic_type schema atomic_type_name
	    
     (* FS Section 4.1.2 Variable References
   
	NOTE: In Galax, namespace resolution has already been done at this
	point.  - Jerome

	statEnv |- VarName of var expands to expanded-QName
	statEnv.varType(expanded-QName) = Type
	---------------------------------------------------
	statEnv |- $ VarName : Type
     *)
    | CEVar v ->
	var_from_static_context typing_ctxt v fi
	  
     (* FS Section 4.1.3 Parenthesized Expressions

	NOTE: In Galax, parenthesized expression do not appear, they are
	dealt with at parsing time. The empty sequence is actually
	constructing an empty sequence and appears in the core.

	----------------------
	statEnv |- ( ) : empty
     *)
    | CEEmpty -> cxtype_empty
	  
     (* FS Section 4.1.5 Function Calls

	See typing_call.ml
     *)
    | CECall (fname, arguments, sign, _, _) -> 
	let arguments_types = List.map (type_cexpr typing_ctxt) arguments in
	Typing_call.compute_type_normal_function_call
	  typing_ctxt
	  fname
	  arguments_types
	  sign

    | CEOverloadedCall (fname, arguments, overloaded_signature_table) -> 
	let arguments_types = List.map (type_cexpr typing_ctxt) arguments in
	Typing_call.compute_type_overloaded_function_call
	  typing_ctxt
	  fname
	  arguments_types
	  overloaded_signature_table

    (* FS Section 4.2 Path Expressions 
    
      | CEForwardAxis (axis,node_test)
        let dottype = Lookup type of $fs:dot
        compute_axis_type typing_ctxt axis dottype
    
      | CEReverseAxis (axis, node_test)
        let dottype = Lookup type of $fs:dot
        compute_axis_type typing_ctxt axis dottype
    *)
    | CEReverseAxis (v, axis, node_test) 
    | CEForwardAxis (v, axis, node_test) ->
	Typing_step.compute_type_axis_node_test typing_ctxt axis node_test fi

     (* FS Section 4.3.1 Constructing Sequences
     
        statEnv |- Expr1 : Type1       statEnv |- Expr2 : Type2
        -------------------------------------------------------
        statEnv |- Expr1 , Expr2 : Type1, Type2
     *)
    | CESeq (e1,e2) ->
	let t1 = type_cexpr typing_ctxt e1
	and t2 = type_cexpr typing_ctxt e2 in
	make_sequence_cxtypes t1 t2

    (* [FS 4.7.3.1] Element Constructors 
       See type_element_constructor
    *)
    | CEElem (qname,nsenv,ce_list) ->
	type_element_constructor typing_ctxt qname ce_list fi

    (* In case the element name is computed, the name expression
        must be of type xs:QName, xs:string, or xs:untypedAtomic. *)
    | CEAnyElem (ce_qname,nsenv1,nsenv2,ce_content) ->
	type_cexpr_with_discard typing_ctxt ce_qname cxtype_computed_qname; 
	type_element_constructor typing_ctxt Namespace_builtin.wild_rqname [ce_content] fi

    (* [FS 4.7.3.2] Attribute Constructors 
       See type_attribute_constructor
    *)
    | CEAttr (qname,nsenv,ce_list) ->
	type_attribute_constructor typing_ctxt qname ce_list fi 
    (* In case the attribute name is computed, the name expression
        must be of type xs:QName, xs:string, or xs:untypedAtomic. *)
    | CEAnyAttr (ce_qname,nsenv,ce_content) ->
	type_cexpr_with_discard typing_ctxt ce_qname cxtype_computed_qname; 
	type_attribute_constructor typing_ctxt Namespace_builtin.wild_rqname [ce_content] fi
    (*
        4.7.3.3 Document Node Constructors
    
       NB: The static typing rules for document constructors works in
       two steps. The normalized content of the document constructor
       calls fs:item-sequence-to-node-sequence(), which takes care of
       converting atomic values to nodes, then this rule guarantees
       that the resulting node sequence does not contain attributes.

       statEnv |-  Expr : Type
       statEnv  |-  Type_1 <: (element | text | processing-instruction | comment)*
       ---------------------------------------------------------------------------
       statEnv |-  document { Expr } : document-node

    *)
    | CEDocument e ->
	let _ = type_cexpr_with_replace typing_ctxt e cxtype_documentcontent in
	cxtype_documentnode
(* More precise type	fmkcxtype (CDocument m) fi  *)
    (*
        4.7.3.4 Text Node Constructors
    
        The static semantics checks that the argument expression has type
        xs:string or empty. The type of the entire expression is an
        optional text node type, as the text node constructor returns the
        empty sequence if its argument is the empty sequence.
    
        statEnv |- Expr : xs:string?
        ----------------------------
        statEnv |- text { Expr } : text?
    
       The normalization rules for text-node constructors guarantees that
       the type is xs:string?.  
    *)
    | CEText t ->
	cxtype_text_optional

    | CECharRef t ->
	cxtype_text_optional

    | CETextComputed e1 ->
	let _ = type_cexpr typing_ctxt e1 in
	cxtype_text_optional
    (*
        4.7.3.5 Computed Processing Instruction Constructors
    
        The static typing rules for processing-instruction constructors
        are straightforward.
    
        statEnv |-   Expr : xs:untypedAtomic
        ---------------------------------------------------------------------------
        statEnv |-  processing-instruction NCName { Expr } : processing-instruction

        statEnv |-  Expr1 : (xs:NCName | xs:string | xs:untypedAtomic)      
        statEnv |-  Expr2 : xs:untypedAtomic
        -------------------------------------------------------------------------------
        statEnv |-  processing-instruction { Expr1 } { Expr2 } : processing-instruction

    *)
          (* Both ncn and str are strings *)      
    | CEPI (ncn,str) -> cxtype_pi
    | CEPIComputed (ce1, ce2) -> 
	begin
	  let cxtype_computed_piname  = make_choice_cxtypes cxtype_string (make_choice_cxtypes cxtype_NCName cxtype_untypedAtomic) in
	  type_cexpr_with_discard typing_ctxt ce1 cxtype_computed_piname;
	  type_cexpr_with_discard typing_ctxt ce2 cxtype_untypedAtomic; 
	  cxtype_pi
	end
    (*
        4.7.3.6 Computed Comment Constructors
    
        The static typing rule for computed comment constructors is
        straightforward.
    
        statEnv |- Expr : xs:untypedAtomic
        -------------------------------------
        statEnv |- comment { Expr } : comment
    
    *)
    | CEComment s -> cxtype_comment

    | CECommentComputed ce1 -> 
	begin
	  type_cexpr_with_discard typing_ctxt ce1 cxtype_untypedAtomic; 
	  cxtype_comment
	end
     (* fn:error($error as xs:QName?, $description as xs:string, $error-object as item()* ) as none *)

    | CEError(celist) -> 
	begin
	  if (List.length celist > 0) then 
	    type_cexpr_with_discard typing_ctxt (List.nth celist 0) cxtype_QName;
	  if (List.length celist > 1) then 
	    type_cexpr_with_discard typing_ctxt (List.nth celist 1) cxtype_string; 
	  cxtype_none
	end
    (*
    
     [FS 4.8 FLWOR expressions]
    
     See type_fl_clause below 
    *)
     | CEFLWOR(cfl_expr_list, cwhere_expr_opt, corder_by_opt, creturn_expr) -> 
	 let (typing_ctxt', (lbound, ubound)) = List.fold_left type_fl_clause (typing_ctxt, Occurrence.one) cfl_expr_list in
	 let (lbound', ubound') = 
	   begin
	     match cwhere_expr_opt with
	     | None -> (lbound, ubound)
	     | Some cwhere_expr -> 
	       (* Check that where type is subtype of boolean *)
		 let _ = type_cexpr_with_replace typing_ctxt' cwhere_expr cxtype_boolean in 
		 (Occurrence.occurs_zero, ubound)
	   end
	 in
	 begin
	   (match corder_by_opt with
	   | None -> ()
	   | Some (_, orderby_list, sig_table) -> 
	       List.iter (type_orderby_spec typing_ctxt') orderby_list);
	   let rettype = type_cexpr typing_ctxt' creturn_expr in
	   fmkcxtype (CBound(rettype, lbound', ubound')) fi 
	 end
    (*
      4.9 Ordered and Unordered Expressions
    
      OrderedExpr and UnorderedExpr expressions set the ordering mode in
      the static context to ordered or unordered.
    
      statEnv1 = statEnv + orderingMode(ordered)
      statEnv1 |- Expr : Type
      ------------------------------------------
      statEnv |- ordered { Expr } : Type
      
      statEnv1 = statEnv + orderingMode(unordered)
      statEnv1 |- Expr : Type
      ------------------------------------------
      statEnv |- unordered { Expr } : Type

      NB : We don't do anything with the ordering mode right now.
    *)
     | CEUnordered ce 
     | CEOrdered ce   -> type_cexpr typing_ctxt ce
    (*
      4.10 Conditional Expressions
    
      statEnv |- Expr1 : xs:boolean      
        statEnv |- Expr2 : Type2      
          statEnv |- Expr3 : Type3
      ------------------------------------------------------------
      statEnv |- if (Expr1) then Expr2 else Expr3 : (Type2 | Type3)
    *)
     | CEIf(cond, ce1, ce2) ->
	 let _ = type_cexpr_with_replace typing_ctxt cond cxtype_boolean in
	 let t1 = type_cexpr typing_ctxt ce1 in
	 let t2 = type_cexpr typing_ctxt ce2 in
	 make_choice_cxtypes t1 t2
     | CEWhile(cond, ce1) ->
	 let _ = type_cexpr_with_replace typing_ctxt cond cxtype_boolean in
	 let t1 = type_cexpr typing_ctxt ce1 in
	 make_zeroormore_type t1

    (*
      4.11 Quantified Expressions
    
      The static semantics of the quantified expressions uses the prime
      operator on types, which is explained in [8.4 Judgments for FLWOR
      and other expressions on sequences]. These rules are similar to
      those for For expressions in [4.8.2 For expression].
    
      statEnv |- Expr1 : Type1
      statEnv |- VarRef1 of var expands to Variable1      
        statEnv + varType(Variable1 : prime(Type1)) |- Expr2 : xs:boolean
      -------------------------------------------------------------------
      statEnv |- some VarRef1 in Expr1 satisfies Expr2 : xs:boolean
      
      The next rule is for SomeExpr with the optional type declaration.
    
      statEnv |- Expr1 : Type1
      Type0 = [ SequenceType ]sequencetype
      prime(Type1) <: Type0
      statEnv |- VarRef1 of var expands to Variable1      
        statEnv + varType(Variable1 : Type0) |- Expr2 : xs:boolean
      -------------------------------------------------------------------
      statEnv |- some VarRef1 as SequenceType in Expr1 satisfies Expr2 : xs:boolean
      
      The next rule is for EveryExpr without the optional type declaration.
    
      statEnv |- Expr1 : Type1
      statEnv |- VarRef1 of var expands to Variable1      
        statEnv + varType(Variable1 : prime(Type1)) |- Expr2 : xs:boolean
      -------------------------------------------------------------------
      statEnv |- every VarRef1 in Expr1 satisfies Expr2 : xs:boolean
      
      The next rule is for EveryExpr with the optional type declaration.
    
      statEnv |- Expr1 : Type1
      Type0 = [ SequenceType ]sequencetype
      prime(Type1) <: Type0
      statEnv |- VarRef1 of var expands to Variable1      
        statEnv + varType(Variable1 : Type0) |- Expr2 : xs:boolean
      -------------------------------------------------------------------
      statEnv |- every VarRef1 as SequenceType in Expr1 satisfies Expr2 : xs:boolean
    
    *)
     | CESome(seqtype_opt,  vname, ce1, ce2)
     | CEEvery(seqtype_opt, vname, ce1, ce2) ->
	 let cetype = type_cexpr typing_ctxt ce1 in
	 let (prime_type, minoccurs, maxoccurs) = factor cetype in 
	 let prime_type' = check_optional_seqtype typing_ctxt seqtype_opt prime_type in 
	 let typing_ctxt' = add_var_to_static_context typing_ctxt (vname, prime_type') in
	 type_cexpr_with_replace typing_ctxt' ce2 cxtype_boolean 

    (*
      [FS 4.12.2 Typeswitch]

      The static typing rules for the typeswitch
      expression are simple. Each case clause and the default clause of
      the typeswitch is typed independently. The type of the entire
      typeswitch expression is the union of the types of all the clauses.
    
      statEnv |- Expr0 : Type0
      statEnv |- Type0 case case VarRef1 as SequenceType1 return Expr1 : Type1
          ···
      statEnv |- Type0 case case VarRefn as SequenceTypen return Exprn : Typen
      statEnv |- Type0 case default VarRefn+1 return Exprn : Typen+1
      ------------------------------------------------------------------------
      statEnv |-   	
      (typeswitch (Expr0)
        case VarRef1 as SequenceType1 return Expr1
          ···
        case VarRefn as SequenceTypen return Exprn
        default VarRefn+1 return Exprn+1)
      	: (Type1 | ... | Typen+1)
     *)
     | CETypeswitch (ce, case_clause_list) ->
	 begin
	   let t1 = type_cexpr typing_ctxt ce in 
	   let t' =
	     choice_of_list (List.map (type_case_clause typing_ctxt t1) case_clause_list)
	   in t'
	 end
    (* 
      4.12.3 Cast
    
      The static typing rule of cast expression is as follows. The type of
      a core cast expression is always the target type. Note that a cast
      expression can fail at run-time if the given value cannot be cast to
      the target type.
    
      -----------------------------------------------
      statEnv |- Expr cast as AtomicType : AtomicType
    
      | CECast of acexpr * csequencetype
    
      --------------------------------------------------
      statEnv |- Expr castable as AtomicType : xs:boolean
    
      | CECastable of acexpr * csequencetype
    *)
     | CECast (ce, nsenv, (seqtype, ty)) ->
	 let _ = type_cexpr typing_ctxt ce in ty
     | CECastable (ce, nsenv, (seqtype, ty)) -> 
	 let _ = type_cexpr typing_ctxt ce in cxtype_boolean
     | CETreat (ce, (seqtype, ty)) -> 
	 let _ = type_cexpr typing_ctxt ce in ty
    (* 
      4.13 Validate Expressions
    
      Static typing of the validate operation is defined by the following
      rule. Note the use of a subtyping check to ensure that the type of
      the expression to validate is either an element or a well-formed
      document node (i.e., with only one root element and no text
      nodes). The type of the expression to validate may be a union of
      more than one element type. We apply the with mode judgment to each
      element type to determine the meaning of that element type with the
      given validation mode, which yields a new element type. The result
      type is the union over all new element types.
    
      statEnv |- Expr : Type
      statEnv |- Type <: (element | document { ElementType })
      statEnv |- prime(Type) = ElementType1 ... ElementTypen
      ElementType1 = element ElementName1? TypeSpecifier1?
      ···
      ElementTypen = element ElementNamen? TypeSpecifiern?
      statEnv |- ElementName1 ? with mode ValidationMode resolves to ElementType1
      ···
      statEnv |- ElementNamen ? with mode ValidationMode resolves to ElementTypen
      Type1 = ElementType1 | ... | ElementTypen
      ---------------------------------------------------------------------------
      statEnv |- validate ValidationMode { Expr } : Type1
    
    *)
     | CEValidate (validation_mode, ce) ->  
	 let cetype = type_cexpr typing_ctxt ce in 
	 check_type_ignore typing_ctxt cetype (make_choice_cxtypes cxtype_element cxtype_documentnode);
	 let (prime_types, minoccurs, maxoccurs) = factor_with_units cetype in 
	 let elem_types = List.map (Typing_util.validate_element_resolves_to schema validation_mode) prime_types in
	 choice_of_list (elem_types) 
     | CESnap (_,ce) -> type_cexpr typing_ctxt ce 

     | CELetvar (seqtype_opt, vname, ce1, ce2) -> 
         let cetype = type_cexpr typing_ctxt ce1 in
	     let (prime_type, minoccurs, maxoccurs) = factor cetype in 
	     let prime_type' = check_optional_seqtype typing_ctxt seqtype_opt prime_type in 
         let typing_ctxt' = add_var_to_static_context typing_ctxt (vname, prime_type') in
	       type_cexpr_with_replace typing_ctxt' ce2 cxtype_boolean 

     | CESet (vname, ce) -> 
         let cetype = type_cexpr typing_ctxt ce in
(* 	     let (prime_type, minoccurs, maxoccurs) = factor cetype in  *)
(* 	     let prime_type' = check_optional_seqtype typing_ctxt seqtype_opt prime_type in  *)
           cetype
             (* This will need to be changed. 
                We haven't defined yet the typing rules for <Set> i.e. whether it is dynamic or not etc 
                --Nicola *)             

     | CELetServerImplement (nc1,nc2,loc,ce) -> 
	   begin
	     type_cexpr_with_discard typing_ctxt loc cxtype_string;
	     let body_type = type_cexpr typing_ctxt ce in
	     body_type
	   end
     | CEExecute (async, ncname,uri, ce1, ce2) -> 
	   begin
	     let body_type = type_cexpr typing_ctxt ce2 in
	     if (async) then cxtype_empty 
	     else body_type
	   end
     | CEForServerClose (nc1,uri,ce) -> 
	 (* The closure itself must be well-typed, but its own type is element() *)
	 ignore(type_cexpr typing_ctxt ce);
	 cxtype_element
     | CEEvalClosure (ce) ->  
	 (* We have no bloody idea what the type of this value will be...so it's "item()*" *)
	 ignore(type_cexpr typing_ctxt ce);
	 cxtype_item_star 
  (* Update-related expressions *)
     | CECopy ce -> type_cexpr typing_ctxt ce 
    (* Update expressions *)
     | CEDelete ce1 ->
	 Error.eprintf_warning ("Static typing incompletely implemented for '"^(Print_top.bprintf_acexpr "" cexpr)^"'");
	 ignore(type_cexpr typing_ctxt ce1);
	 cxtype_empty 
     | CERename (nsenv, ce1, ce2)  -> 
	 Error.eprintf_warning ("Static typing incompletely implemented for '"^(Print_top.bprintf_acexpr "" cexpr)^"'");
	 ignore(type_cexpr typing_ctxt ce1);
	 ignore(type_cexpr typing_ctxt ce2);
	 cxtype_empty 
     | CEReplace (Normal_Replace, ce1, ce2) -> 
	 Error.eprintf_warning ("Static typing incompletely implemented for '"^(Print_top.bprintf_acexpr "" cexpr)^"'");
	 ignore(type_cexpr typing_ctxt ce1);
	 ignore(type_cexpr typing_ctxt ce2);
	 cxtype_empty 
     | CEReplace (Value_Of_Replace, ce1, ce2) ->
	 Error.eprintf_warning ("Static typing incompletely implemented for '"^(Print_top.bprintf_acexpr "" cexpr)^"'");
	 ignore(type_cexpr typing_ctxt ce1);
	 ignore(type_cexpr typing_ctxt ce2);
	 cxtype_empty 
     | CEInsert (ce1, insert_loc) ->
	 begin
	   Error.eprintf_warning ("Static typing incompletely implemented for '"^(Print_top.bprintf_acexpr "" cexpr)^"'");
	   ignore(type_cexpr typing_ctxt ce1);
	   match insert_loc with
	   | CUAsLastInto ce2
	   | CUAsFirstInto ce2
	   | CUInto ce2
	   | CUAfter ce2
	   | CUBefore ce2 -> ignore(type_cexpr typing_ctxt ce2);
	       cxtype_empty 
	 end
     | CEImperativeSeq (ce1,ce2) ->
	 let _ = type_cexpr typing_ctxt ce1
	 and t2 = type_cexpr typing_ctxt ce2 in
         t2
    end
  with
    | exn -> raise (error_with_file_location fi exn)

(*
  4.8.4 Order By and Return Clauses

  An OrderByClause is normalized to a Let clause, nested For
  expressions, and atomization, which guarantees that the
  OrderSpecList is well typed. Note that if evaluated dynamically, the
  normalization of OrderByClause given here does not express the
  required sorting semantics, but this normalization does provide the
  correct static type. Notably, the normalization rule uses the gt
  operation, which implies that the ordering criteria is typed using
  the same static typing rules, taking into account existential
  quantification, atomization and type promotion.


  XQuery Language 3.8.3:

  # If the value of an orderspec has the dynamic type
    xs:untypedAtomic (such as character data in a schemaless
    document), it is cast to the type xs:string.

    Note:

    Consistently treating untyped values as strings enables the
    sorting process to begin without complete knowledge of the types
    of all the values to be sorted.  

  # All the non-empty orderspec values must be convertible to a common
    type by subtype substitution and/or type promotion. The ordering
    is performed in the least common type that has a gt operator. If
    two or more non-empty orderspec values are not convertible to a
    common type that has a gt operator, a type error is raised
    [err:XPTY0004].

    * Example: The orderspec values include a value of type hatsize,
      which is derived from xs:integer, and a value of type shoesize,
      which is derived from xs:decimal. The least common type
      reachable by subtype substitution and type promotion is
      xs:decimal.

    * Example: The orderspec values include a value of type xs:string
      and a value of type xs:anyURI. The least common type reachable
      by subtype substitution and type promotion is xs:string.

*)
and type_orderby_spec typing_ctxt (ce, _, _) = 
  let schema = (schema_from_static_context typing_ctxt) in 
  (* Check that all order-by spec types can be promoted to a common
     type, which has a greater-than operator.  *)
  let ctype = type_cexpr typing_ctxt ce in
  let (p, m, n) = factor_with_units ctype in
  if (n = Occurrence.occurs_one) then
    try
      let lct = Typing_util.least_common_promoted_type schema p in 
      check_type_ignore typing_ctxt lct cxtype_greater_than_types
    with 
    | Not_found -> 
	ignore(raise_wrong_expected_type_error typing_ctxt ctype cxtype_greater_than_types)
  else 
    ignore(raise_wrong_expected_type_error typing_ctxt ctype cxtype_greater_than_types)

and type_fl_clause (typing_ctxt, occurs) cfl_clause =
  match cfl_clause with 
  (*    
     4.8.3 Let Expression
  
     Static Type Analysis
  
     A let expression extends the type environment statEnv with Variable1
     of type Type1 inferred from Expr1, and infers the type of Expr2 in
     the extended environment to produce the result type Type2.
  
     statEnv |- Expr1 : Type1      
       statEnv |- VarRef of var expands to Variable      
         statEnv + varType(Variable1 : Type1) |- Expr2 : Type2  
     --------------------------------------------------------- 
     statEnv |- let VarRef := Expr1 return Expr2 : Type2
  
     When a type declaration is present, the static semantics also checks
     that the type of the input expression is a subtype of the declared
     type and extends the static environment by typing Variable1 with
     type Type0. This semantics is specified by the following static
     rule.
  
     statEnv |- Expr1 : Type1
       Type0 = [ SequenceType ]sequencetype
         Type1 <: Type0
     statEnv |- VarRef of var expands to Variable      
     statEnv + varType(Variable1 : Type0 ) |- Expr2 : Type2
     -------------------------------------------------------------------
     statEnv |- let VarRef1 as SequenceType := Expr1 return Expr2 : Type
  *)
  | CELET (seqtype_opt, vname, ce) -> 
      let cetype = type_cexpr typing_ctxt ce in
      let cetype' = check_optional_seqtype typing_ctxt seqtype_opt cetype in 
      let typing_ctxt' = add_var_to_static_context typing_ctxt (vname, cetype') in
      (typing_ctxt', occurs)
  (* 
   [FS 4.8.2] For expression
  
    A single for expression is typed as follows: First Type1 of the
    iteration expression Expr1 is inferred. Then the prime type of
    Type1, prime(Type1), is computed. This is a union over all item
    types in Type1 (See [8.4 Judgments for FLWOR and other expressions
    on sequences]). With the variable component of the static
    environment statEnv extended with VarRef1 as type prime(Type1), the
    type Type2 of Expr2 is inferred. Because the for expression iterates
    over the result of Expr1, the final type of the iteration is Type2
    multiplied with the possible number of items in Type1 (one, ?, *, or
    +). This number is determined by the auxiliary type-function
    quantifier(Type1).
  
     statEnv |- Expr1 : Type1
     statEnv + varType(VarRef1 : prime(Type1)) |- Expr2 : Type2
     -----------------------------------------------------------------------
     statEnv |- for VarRef1 in Expr1 return Expr2 : Type2 · quantifier(Type1)
  
     When a positional variable Variablepos is present, the static
     environment is also extended with the positional variable typed as
     an xs:integer.
  
     statEnv |- Expr1 : Type1
     statEnv + varType(VarRef1 : prime(Type1), VarRefpos : xs:integer) |- Expr2 : Type2
     -----------------------------------------------------------------------
     statEnv |- for VarRef1 at VarRefpos in Expr1 return Expr2 : Type2 · quantifier(Type1)
  
     When a type declaration is present, the static semantics also checks
     that the type of the input expression is a subtype of the declared
     type and extends the static environment by typing VarRef1 with type
     Type0. This semantics is specified by the following typing rule.
  
     statEnv |- Expr1 : Type1
     Type0 = [ SequenceType ]sequencetype
     prime(Type1) <: Type0
     statEnv + varType(VarRef1 : Type0)) |- Expr2 : Type2
     -----------------------------------------------------------------------
     statEnv |- for VarRef1 as SequenceType in Expr1 return Expr2 : Type2 · quantifier(Type1)
  
     The last rule contains a For expression that contains a type
     declaration and a positional variable. When the positional variable
     is present, the static environment is also extended with the
     positional variable typed as an integer.
  
     statEnv |- Expr1 : Type1
     Type0 = [ SequenceType ]sequencetype
     prime(Type1) <: Type0
     statEnv + varType(VarRef1 : Type0, VarRefpos : xs:integer) |- Expr2 : Type2
     --------------------------------------------------------------------------------------------------
     statEnv |- for VarRef1 as SequenceType at VarRefpos in Expr1 return Expr2 : Type2 quantifier(Type1)
  *)
  | CEFOR (seqtype_opt, vname, posvar_opt, ce) -> 
      let cetype = type_cexpr typing_ctxt ce in
      let (prime_type, minoccurs, maxoccurs) = factor cetype in 
      let prime_type' = check_optional_seqtype typing_ctxt seqtype_opt prime_type in 
      let typing_ctxt' = add_var_to_static_context typing_ctxt (vname, prime_type') in
      let typing_ctxt'' = 
	begin
	  match posvar_opt with
	  | None -> typing_ctxt'
	  | Some vname -> add_var_to_static_context typing_ctxt' (vname, cxtype_integer) 
	end
      in (typing_ctxt'', mult_occurrences (minoccurs, maxoccurs) occurs)

(*

  [FS 4.12.2 Typeswitch case] 
  
  To type one case clause, the case variable is assigned the type of
  the case clause CaseType and the body of the clause is typed in the
  extended environment. Thus, the type of a case clause is independent
  of the type of the input expression.

  CaseType = [ SequenceType ]sequencetype
  statEnv(VarRef : CaseType ) |- Expr : Type
  ---------------------------------------------------------
  Type0 case case VarRef as SequenceType return Expr : Type
  
  To type the default clause, the variable is assigned the type of the
  input expression and the body of the default clause is typed in the
  extended environment.

  statEnv(VarRef : Type0 ) |- Expr : Type
  --------------------------------------------
  Type0 case default VarRef return Expr : Type
  
*)
and type_case_clause typing_ctxt t (pat, vname_opt, ce) =
  let case_type = 
    match pat.pcpattern_desc with
    | CCase (seqtype, ty) -> ty
    | CDefault -> t
  in
  let typing_ctxt' = 
    match vname_opt with
    | None -> typing_ctxt
    | Some vname -> add_var_to_static_context typing_ctxt (vname, case_type) 
  in type_cexpr typing_ctxt' ce

(* [FS 4.7.3.1] Element Constructors

  A computed element constructor creates a new element with either the
  type annotationXQ xs:untyped (in strip construction mode), or with
  the type annotationXQ xs:anyType (in preserve construction
  mode). The content expression must return a sequence of nodes with
  attribute nodes at the beginning.

  *** NB: Galax only implements 'strip' constructor mode ***
  
  statEnv.constructionMode = strip
  statEnv |- Expr : Type
  statEnv |- Type <: attribute *, (element | text | comment | processing-instruction) *
  -------------------------------------------------------------------------------------
  statEnv |- element QName { Expr } : element QName of type xs:untyped
  
  statEnv.constructionMode = strip
  statEnv |- Expr1 : Type1
  statEnv |- Type1 <: (xs:QName | xs:string | xs:untypedAtomic)
  statEnv |- Expr2 : Type2
  statEnv |- Type2 <: attribute *, (element | text | comment | processing-instruction) *
  -------------------------------------------------------------------------------------
  statEnv |- element { Expr1 } { Expr2 } : element of type xs:untyped
*)
and type_element_constructor typing_ctxt qname ce_list fi =
  let content_type = 
    List.fold_left 
      (fun tys ce -> 
	let t' = type_cexpr typing_ctxt ce in make_sequence_cxtypes t' tys) 
      cxtype_empty ce_list 
  in
  check_type_ignore typing_ctxt content_type cxtype_element_content;
  let relem_sym = Namespace_symbols.relem_symbol qname in
  fmkcxtype (CElementLocal (relem_sym, NonNillable, Namespace_symbols_builtin.xs_untyped)) fi

(* Attribute construction 

   Static Type Analysis

  The normalization rules for direct attribute constructors leave us
  with only the computed form of the attribute constructors. Like in a
  computed element constructor, a computed attribute constructor has
  two forms: one in which the attribute name is a literal QName, and
  the other in which the attribute name is a computed expression.

  In the case of attribute constructors, the type annotationXQ is
  always xs:untypedAtomic.

  statEnv |- Expr : Type
  Type <: xs:untypedAtomic
  -------------------------------------------------------------------------------
  statEnv |- attribute QName { Expr } : attribute QName of type xs:untypedAtomic
  
  statEnv |- Expr1 : Type1
  statEnv |- Type1 <: (xs:QName | xs:string | xs:untypedAtomic)
  statEnv |- Expr2 : Type2
  statEnv |- Type2 <: xs:untypedAtomic
  -----------------------------------------------------------------------------
  statEnv |- attribute { Expr1 } { Expr2 } : attribute of type xs:untypedAtomic

  *** NB (Mary) : We check that Type2 <: xs:untypedAtomic, because
  the normalized content of attribute constructors is always converted
  to an xs:untypedAtomic value.

*)
and type_attribute_constructor typing_ctxt qname ce_list fi = 
  let content_type = 
    List.fold_left 
      (fun tys ce -> 
	let t' = type_cexpr typing_ctxt ce in make_sequence_cxtypes t' tys) 
      cxtype_empty ce_list 
  in
  check_type_ignore typing_ctxt content_type cxtype_untypedAtomic;
  let rattr_sym = Namespace_symbols.rattr_symbol qname in
  fmkcxtype (CAttributeLocal (rattr_sym, Namespace_symbols_builtin.xs_untypedAtomic)) fi

and type_cexpr typing_ctxt ce =
  let check_type_fn = (fun c r -> r)
  in type_cexpr_aux typing_ctxt check_type_fn ce

and type_cexpr_with_replace typing_ctxt ce expected_type =
  (* 2. Checks that the type is a subtype of the expected type and
  returns expected type *)
  let check_type_fn = (fun c r -> check_type_replace c r expected_type)
  in  type_cexpr_aux typing_ctxt check_type_fn ce

and type_cexpr_with_discard typing_ctxt ce expected_type =
  (* 2. Checks that the type is a subtype of the expected type and discards expected type *)
  let check_type_fn = (fun c r -> check_type_discard c r expected_type) in  
  let _ = type_cexpr_aux typing_ctxt check_type_fn ce in
  ()

and type_cexpr_aux typing_ctxt check_type_fn ce =
  let schema = (schema_from_static_context typing_ctxt) in 
  let fi = ce.pcexpr_loc in 
  try
  (* 1. Computes the type for the expression *)
    let result_type = compute_type_expr typing_ctxt ce in
(*
    Print_top.printf_acexpr "\nExpression " ce ; 
    Print_top.printf_cxtype " has type " result_type ; 
    print_string "\n";
*)
  (* 2. Applies function for checking type *)
    let result_type' = check_type_fn typing_ctxt result_type in
  (* 3. Checks that the type is not empty, except for the empty sequence *)
    let result_type'' = check_empty_type typing_ctxt result_type' ce in
    begin
    (* 3. Updates the type annotation for that expression *)
      set_type_annotation_for_cexpr ce result_type'';
    (* 4. Returns the simplified inferred type *)
      let result_type''' = Schema_simplification.simplify_ty schema result_type'' in
      result_type'''
    end
  with
  | exn -> raise (error_with_file_location fi exn)

