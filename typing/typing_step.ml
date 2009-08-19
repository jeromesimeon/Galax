(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_step.ml,v 1.25 2007/02/01 22:08:55 simeon Exp $ *)

(* Module: Typing_step
   Description:
     This module implements static typing for XPath steps (axis and
     node tests).
 *)

open Error

open Namespace_names

open Xquery_ast
open Xquery_common_ast
open Xquery_common_ast_util
open Xquery_core_ast
open Xquery_core_ast_util
open Xquery_type_core_ast
open Xquery_type_core_ast_util

open Schema_builtin
open Schema_util

open Typing_errors

open Dm_types

(* 

  [FS 8.2.2.1.3] Auxiliary inference rules for the child axis

  statEnv |- Type1 has-node-content Type2

  applies to a type that is a valid element content type and holds
  when Type1 has the content type Type2. The judgment separates the
  attribute types from the other node or atomic-valued types of the
  element content type and yields the non-attribute types.

  If the type is a sequence of attributes, then the content type is empty.
  statEnv |- Type <: attribute*
  -----------------------------
  statEnv |- Type has-node-content empty

  If the type is attributes followed by a simple type, the content
  type is zero-or-one text. The resulting type is optional since an
  expression returning the empty sequence results in no text node
  being constructed.

  Type = Type1, Type2
  statEnv |- Type1 <: attribute*
  statEnv |- Type2 <: xs:anyAtomicType*
  ---------------------------------------
  statEnv |- Type has-node-content text ?
  
  In the case of an element type with complex content type, the
  content type is simply the non-attribute part of the complex content
  type.

  Type = Type1, Type2
  statEnv |- Type1 <: attribute*
  statEnv |- Type2 <: ElementContentType*
  ---------------------------------------
  statEnv |- Type has-node-content Type2

*)
let has_node_content schema (attribute_type, content_kind, cmixed) = 
  let node_content_type = 
    match content_kind with 
    | ComplexKind ct -> 
	if (Subtyping_top.is_subtype_of schema ct cxtype_node_star) then ct
	else if (Subtyping_top.is_subtype_of schema ct cxtype_anyAtomic_star) then 
	  cxtype_text_optional
	else
	(* In this case, element could contain atomic content, which
	   will be converted to text, and therefore could contain anything  *)
	  cxtype_element_node_content
    | SimpleKind st -> Schema_builtin.cxtype_text_optional
  in
(*  Print_top.printf_cxtype "\nNode_content_type " node_content_type; *)
  node_content_type 

(* [FS 8.2.2.1.3] Auxiliary inference rules for the child axis

   statEnv |- Type1 has-attribute-content Type2

   only applies to a type that is a valid element content type and
   holds when Type1 has attribute types Type2. The judgment yields the
   attribute types of the element content type.

  Type = (Type1, Type2)
  statEnv |- Type1 <: attribute*
  statEnv |- Type2 <: ElementContentType* | xs:anyAtomicType*
  ------------------------------------------------------------
  statEnv |- Type has-attribute-content Type1
*)
let has_attribute_content (attribute_type, content_kind, cmixed) = attribute_type

(**************************)
(* axis_type_of Judgement *)
(**************************)

(*
  [FS 8.2.2.1] Static semantics of axes

  The following judgment

  -------------------------------------
  statEnv |- axis Axis of Type1 : Type2

  holds when applying the axis Axis on type Type1 yields the type Type2.
*)
let rec axis_type_of_judge typing_ctxt axis t = 
  let schema = (Typing_context.schema_from_static_context typing_ctxt) in 

  (*
    ------------------------------------- 
    statEnv |- axis Axis of empty : empty 
  *)
  if (Subtyping_top.is_subtype_of_empty_sequence schema t) then 
    Schema_util.cxtype_empty
  (*
    ------------------------------------- 
    statEnv |- axis Axis of none : none 
  *)
  else if (Subtyping_top.is_subtype_of_empty_choice schema t) then 
    Schema_util.cxtype_none
  (*
     
     [FS 8.2.2.1.1] Inference rules for all axis
     
     The following rules compute the type of the axis expression when
     applied to each item type in the content model.
     
     statEnv |- axis Axis of Type1 : Type2
     -----------------------------------------------------------  
     statEnv |- axis Axis of Type1 Occurrence : Type2 Occurrence
     
     statEnv |- axis Axis of Type1 : Type3
     statEnv |- axis Axis of Type2 : Type4
     -------------------------------------  
     statEnv |- axis Axis of Type1,Type2 : Type3,Type4
     
     statEnv |- axis Axis of Type1 : Type3
     statEnv |- axis Axis of Type2 : Type4
     -------------------------------------  
     statEnv |- axis Axis of Type1|Type2 : Type3|Type4
     
     statEnv |- axis Axis of Type1 : Type3
     statEnv |- axis Axis of Type2 : Type4
     -------------------------------------  
     statEnv |- axis Axis of Type1&Type2 : Type3&Type4
     
  *)
  else 
    begin
      match t.pcxtype_desc with 
      | CBound(t, m, n) ->
	  let t' = axis_type_of_judge typing_ctxt axis t
	  in fmkcxtype (CBound (t', m, n)) t.pcxtype_loc
      | CSequence(t1, t2) -> 
	  let t1' = axis_type_of_judge typing_ctxt axis t1 in
	  let t2' = axis_type_of_judge typing_ctxt axis t2 in
	  fmkcxtype (CSequence (t1', t2')) t.pcxtype_loc 
      | CChoice(t1, t2) -> 
	  let t1' = axis_type_of_judge typing_ctxt axis t1 in
	  let t2' = axis_type_of_judge typing_ctxt axis t2 in
	  fmkcxtype (CChoice (t1', t2')) t.pcxtype_loc 
      | CInterleave(t1, t2) -> 
	  let t1' = axis_type_of_judge typing_ctxt axis t1 in
	  let t2' = axis_type_of_judge typing_ctxt axis t2 in
	  fmkcxtype (CInterleave (t1', t2')) t.pcxtype_loc 
      | _ -> 
	  begin
	    match axis with 
      (*
         [FS 8.2.2.1.3] Inference rules for the child axis
	 
         ------------------------------------------------
         statEnv |- axis child:: of AttributeType : empty
         
         ---------------------------------------
         statEnv |- axis child:: of text : empty
         
         ------------------------------------------
         statEnv |- axis child:: of comment : empty
         
         ---------------------------------------------------------
         statEnv |- axis child:: of processing-instruction : empty
         
         ---------------------------------------------------------------------------------------
         statEnv |- axis child:: of document { Type } : Type & processing-instruction* & comment*
      *)
	    | Child -> 
		begin
		  match t.pcxtype_desc with 
		  | CAttributeRef   _
		  | CAttributeLocal _
		  | CText
		  | CComment 
		  | CPI _ -> Schema_util.cxtype_empty
		  | CDocument t1 -> 
		      (Schema_util.make_interleave_cxtypes t1
			 (Schema_util.make_interleave_cxtypes 
			    (Schema_util.make_zeroormore_type Schema_builtin.cxtype_pi) 
			    (Schema_util.make_zeroormore_type Schema_builtin.cxtype_comment)))
        (*
           In the case of an element type, the static type of the child axis is
           obtained by type lookup and expansion of the resulting type. Note that
           the expands to judgment yields the type that corresponds to a given
           type name. Because the meaning of a type name includes the definitions
           of all type names derived by extension and restriction from the given
           type name, expands to yields the union of all the type definitions of
           all type names derived from the input type name. Each type in the
           union contains the complete definition of the type name, i.e., it
           includes built-in attributes and, if necessary,
           processing-instruction, comment, and text types.
           
           After type expansion, the judgment has-node-content is applied to
           each type in the union. The resulting type is the union of all
           non-attribute types in the expanded type.
           
           statEnv |- ElementType type lookup Nillable? TypeReference
           statEnv |- Nillable? TypeReference expands to Type1 | · · · | Typen
           statEnv |- Type1 has-node-content Type1'
           · · ·
           statEnv |- Typen has-node-content Typen'
           -------------------------------------------------------------
           statEnv |- axis child:: of ElementType : Type1' | ... | Typen'
           
        *)  
		  | CElementRef _ 
		  | CElementLocal _ -> 
		      begin
			let (_, nillable, ctname) = Schema_judge.lookup_element_type schema t in 
(*	Print_top.printf_cxtype "\nType " t;
	print_string ("\n has element type "^(Namespace_symbols.symbol_prefix_string ctname)^"\n"); *)
			let expansion = (Schema_judge.expands_to schema (nillable, ctname)) in
			has_node_content schema expansion
		      end
		  | _ -> raise_axis_type_error typing_ctxt axis t
		end
        (*  
           [FS 8.2.2.1.4] Inference rules for the attribute axis
           
           ----------------------------------------------------
           statEnv |- axis attribute:: of AttributeType : empty
           
           -------------------------------------------
           statEnv |- axis attribute:: of text : empty
           
           ----------------------------------------------
           statEnv |- axis attribute:: of comment : empty
           
           -------------------------------------------------------------
           statEnv |- axis attribute:: of processing-instruction : empty
           
           --------------------------------------------------------
           statEnv |- axis attribute:: of document { Type } : empty
        *)
	    | Attribute -> 
		begin
		  match t.pcxtype_desc with 
		  | CAttributeRef   _
		  | CAttributeLocal _
		  | CText
		  | CComment 
		  | CPI _
		  | CDocument       _ -> Schema_util.cxtype_empty
        (*
           statEnv |- ElementType type lookup Nillable? TypeReference
           statEnv |- Nillable? TypeReference expands to Type1 | · · · | Typen
           statEnv |- Type1 has-attribute-content Type1'
           · · ·
           statEnv |- Typen has-attribute-content Typen'
           -----------------------------------------------------------------
           statEnv |- axis attribute:: of ElementType : Type1' | ... | Typen'
        *)
		  | CElementRef _ 
		  | CElementLocal _ -> 
		      let (_, nillable, ctname) = Schema_judge.lookup_element_type schema t in 
		      has_attribute_content(Schema_judge.expands_to schema (nillable, ctname))
		  | _ -> raise_axis_type_error typing_ctxt axis t
		end
        (*  
           [FS 8.2.2.1.5] Inference rules for the parent axis
           
           -----------------------------------------------------------
           statEnv |- axis parent:: of 
           (element|text|processing-instruction|comment) : (element | document)?
           
           -----------------------------------------------------
           statEnv |- axis parent:: of AttributeType : element?
           
           ------------------------------------------------
           statEnv |- axis parent:: of DocumentType : empty
           
        *)
	    | Parent -> 
		begin
		  match t.pcxtype_desc with 
		  | CElementRef _ 
		  | CElementLocal _ 
		  | CText
		  | CComment 
		  | CPI _ -> Schema_builtin.cxtype_element_or_documentnode_optional
		  | CAttributeRef   _
		  | CAttributeLocal _ ->
		      Schema_builtin.cxtype_element_optional
		  | CDocument t1 -> 
		      Schema_util.cxtype_empty
		  | _ -> raise_axis_type_error typing_ctxt axis t
		end
        (*  
           [FS 8.2.2.1.7] Inference rules for the descendant axis
           
           The types for the descendant axis is obtained as the closure of the
           type of the child axis. This is expressed by the following inference
           rule.
           
           statEnv |- axis child:: of Type : Type1
           statEnv |- axis child:: of prime(Type1) : Type2
           ...
           statEnv |- axis child:: of prime(Typen) : Typen+1
           statEnv |- prime(Typen+1) <: prime(Type1) | ... | prime(Typen)
           ---------------------------------------------------------------------------
           statEnv |- axis descendant:: of Type : (prime(Type1) | ... | prime(Typen))*
           
           Note that the last premise in the above rule terminates the
           recursion. The rule computes the n-th type Typen such that applying
           the child axis one more time does not add any new item type to the
           union. This condition is guaranteed to hold at some point, because
           the number of item types is bounded by all of the item types defined
           in the in-scope schema definitions.
        *)
           | Descendant -> 
	       let rec rec_child prime_type_list t1 =
		 let cts = axis_type_of_judge typing_ctxt Child t1 in
		 let (pts, _, _) = factor_with_units cts in 
		 (* If all the new types are already in the prime_type_list, we're done *)
		 if (List.for_all (fun c -> List.mem c prime_type_list) pts) then
		   prime_type_list
		 else
		   rec_child (pts @ prime_type_list) cts 
	       in
               let first_step = axis_type_of_judge typing_ctxt Child t in
	       let (pts, _, _) = factor_with_units first_step in 
	       Schema_util.make_zeroormore_type(Schema_util.choice_of_list(rec_child pts first_step))
        (*
           
           [FS 8.2.2.1.8] Inference rules for the descendant-or-self axis
           
           The type for the descendant-or-self axis is the union of the type
           for the self axis and for the descendant axis.
           
           statEnv |- axis descendant:: of Type1 : Type2
           ---------------------------------------------
           statEnv |- axis descendant-or-self:: of Type1 : (prime(Type1) | prime(Type2))*
        *)
           | Descendant_or_self | Following | Preceding -> 
	       let (t1, _, _) = factor t in 
	       let (t2, _, _) = factor (axis_type_of_judge typing_ctxt Descendant t) in
	       Schema_util.make_zeroormore_type(Schema_util.make_choice_cxtypes t1 t2)
        (*
           [FS 8.2.2.1.9] Inference rules for the ancestor axis
           
           The type for the ancestor axis is computed similarly as for the descendant axis.
           
           --------------------------------------------------------------
           statEnv |- axis ancestor:: of NodeType : (element | document)*
           
           Note that this rule will always result in the type (element |
           document)* type, but this formulation is prefered for consistency,
           and in case the static typing for the parent axis gets improved in a
           future version.
           
           statEnv |- axis parent:: of Type : Type1
           statEnv |- axis parent:: of prime(Type1) : Type2
           ...
           statEnv |- axis parent:: of prime(Typen) : Typen+1
           statEnv |- prime(Typen+1) <: prime(Type1) | ... | prime(Typen)
           ------------------------------------------------------------------------
           statEnv |- axis ancestor:: of Type : (prime(Type1) | ... | prime(Typen))*
        *)  
	    | Ancestor -> 
		Schema_util.make_zeroormore_type
		  (Schema_util.make_choice_cxtypes Schema_builtin.cxtype_element Schema_builtin.cxtype_documentnode)
        (*
           [FS 8.2.2.1.10] Inference rules for the ancestor-or-self axis
           
           The type for the ancestor-or-self axis is the union of the
           type for the self axis and for the ancestor axis.
           
           statEnv |- axis ancestor:: of Type1 : Type2
           ---------------------------------------------------------------------------
           statEnv |- axis ancestor-or-self:: of Type1 : (prime(Type1) | prime(Type2))*
           
        *)
	    | Ancestor_or_self -> 
		let (prime_type1, _, _) = factor t in 
		let (prime_type2, _, _) = factor (axis_type_of_judge typing_ctxt Ancestor t)
		in  Schema_util.make_zeroormore_type (Schema_util.make_choice_cxtypes prime_type1 prime_type2)
		  
        (* 

	   There are no formal static typing rules for following_sibling or
	   preceding_sibling because they are optional axes.

           The following-sibling(preceding-sibling) axis contains the context node's
           following(preceding) siblings, those children of the context node's
           parent that occur after(before) the context node in document order;
           if the context node is an attribute or namespace node, the
           following-sibling(preceding-sibling) axis is empty.

           ----------------------------------------------------
           statEnv |- axis *-sibling:: of AttributeType : empty

           ---------------------------------------------------------------------------
           statEnv |- axis following-sibling:: of Type1 : (element|text|comment|processing-instruction)*
        *)
           | Following_sibling 
           | Preceding_sibling -> 
	       begin
		 match t.pcxtype_desc with 
		 | CElementRef _ 
		 | CElementLocal _ 
		 | CText
		 | CComment 
		 | CPI _ -> 
		     Schema_util.make_zeroormore_type 
		       (Schema_util.make_choice_cxtypes Schema_builtin.cxtype_element
			  (Schema_util.make_choice_cxtypes Schema_builtin.cxtype_text
			     (Schema_util.make_choice_cxtypes Schema_builtin.cxtype_pi
				Schema_builtin.cxtype_comment)))
		 | CAttributeRef   _
		 | CAttributeLocal _
		 | CDocument _ -> 
		     Schema_util.cxtype_empty
		  | _ -> raise_axis_type_error typing_ctxt axis t
	       end
        (*
           [FS 8.2.2.1.2] Inference rules for the self axis
           
           Applying the self axis to a node type results in the same node type.
           
           ---------------------------------------------
           statEnv |- axis self:: of NodeType : NodeType
        *)
	    | Self -> t
	  end
    end

let make_new_type_with_symbol schema rqname t = 
  match (t.pcxtype_desc) with
  | (CElementRef cename) -> 
      let (_, _, cnillable, ctname) = Schema_judge.lookup_element schema cename 
      in fmkcxtype (CElementLocal (rqname,cnillable,ctname)) t.pcxtype_loc
  | (CElementLocal (_, cnillable, ctname)) -> 
      fmkcxtype (CElementLocal (rqname,cnillable,ctname)) t.pcxtype_loc
  | (CAttributeRef caname) -> 
      let (_, ctname) = Schema_judge.lookup_attribute schema caname in
      fmkcxtype (CAttributeLocal (rqname,ctname)) t.pcxtype_loc
  | (CAttributeLocal (_, ctname)) -> 
      fmkcxtype (CAttributeLocal (rqname,ctname)) t.pcxtype_loc
  | _ -> raise(Query(Internal_Error("Applying match_on_name_test to invalid type "^(Print_top.bprintf_cxtype "" t))))

let make_new_type schema rqname t = 
  make_new_type_with_symbol schema (Namespace_symbols.relem_symbol rqname) t
(*
  match_on_name_test takes a NameTest QName, the QName of the element
  or attribute type to which it is applied, and the type itself and
  applies the name-matching rules for constructing a new type, which
  may use the TypeSpecifier part of the input type.
*)
let match_on_name_test schema rqname2 rqname1 t =
  let (prefix2, uri2, localpart2) = Namespace_symbols.relem_name rqname2 in
  let (prefix1, uri1, localpart1) = Namespace_symbols.relem_name rqname1 in 
  match ((prefix2, uri2, localpart2), (prefix1, uri1, localpart1)) with
   (* 
     I. Wildcard Prefix/Uri in NameTest 

      ------------------------------------------------------------------------------------
      statEnv |- test * with NODE of NODE QName TypeSpecifier? : NODE QName TypeSpecifier?
   *)
  | ((NSWildcardPrefix, NSWildcardUri, "*"), _) -> t
   (*
      ---------------------------------------------------------------------------------------------------------
      statEnv |- test *:LocalPart2 with NODE of NODE TypeSpecifier? : NODE *:LocalPart2 TypeSpecifier?
   *)
  | ((NSWildcardPrefix, NSWildcardUri, _), (NSWildcardPrefix, NSWildcardUri, "*")) ->
      make_new_type schema (NSWildcardPrefix, NSWildcardUri, localpart2) t
   (*
      LocalPart1 = LocalPart2
      ----------------------------------------------------------------------------------------------------------------------
      statEnv |- test *:LocalPart2 with NODE of NODE *:LocalPart1 TypeSpecifier? : NODE *:LocalPart2 TypeSpecifier?
   *)
   | ((NSWildcardPrefix, NSWildcardUri, _), (NSWildcardPrefix, NSWildcardUri, _))  -> 
       if (localpart2 = localpart1) then t else Schema_util.cxtype_empty
   (* 
      ------------------------------------------------------------------------------------------------------------------------
      statEnv |- test *:LocalPart2 with NODE of NODE Prefix1:* TypeSpecifier? : NODE Prefix1:LocalPart2 TypeSpecifier?
   *)
  | ((NSWildcardPrefix, NSWildcardUri, _), (_, _, "*")) ->  
      make_new_type schema (prefix1, uri1, localpart2) t
   (*
      fn:namespace-uri-from-QName( expanded-QName1 ) = statEnv.namespace(Prefix1)
      LocalPart2 = fn:local-name-from-QName( expanded-QName1 )
      ----------------------------------------------------------------------------------------------------------
      statEnv |- test *:LocalPart2 with NODE of NODE QName1 TypeSpecifier? : NODE QName1 TypeSpecifier?
   *)
   | ((NSWildcardPrefix, NSWildcardUri, _), (_, _, _))  -> 
       if (localpart2 = localpart1) then t else Schema_util.cxtype_empty
  (*
     II. Wildcard Prefix/Uri in Input Type

     ---------------------------------------------------------------------------------------------
     statEnv |- test QName2 with NODE of NODE TypeSpecifier? : (NODE QName2 TypeSpecifier?) "?"

     Shouldn't this yield an optional type because the actual node might not match the name test?
  *)
   | ((_, _, _), (NSWildcardPrefix, NSWildcardUri, "*")) -> make_builtin_optional_type(make_new_type schema (prefix2, uri2, localpart2) t)
  (*
     ----------------------------------------------------------------------------------------------------------------
     statEnv |- test Prefix2:* with NODE of NODE *:LocalPart1 TypeSpecifier? : (NODE Prefix2:LocalPart1 TypeSpecifier?) "?"
  *)
   | ((_, _, "*"), (NSWildcardPrefix, NSWildcardUri, _))  -> 
       make_builtin_optional_type(make_new_type schema (prefix2, uri2, localpart1) t)
  (*
     fn:local-name-from-QName(expanded-QName2) = LocalPart1
     --------------------------------------------------------------------------------------------
     statEnv |- test QName2 with NODE of NODE *:LocalPart1 TypeSpecifier? : NODE QName2 TypeSpecifier? "?"
  *)
   | ((_, _, _), (NSWildcardPrefix, NSWildcardUri, _)) ->  
     if (localpart2 = localpart1) then make_builtin_optional_type(make_new_type schema (prefix2, uri2, localpart2) t)
     else Schema_util.cxtype_empty

  (* 
     III.  No Wildcard Prefix/Uri in NameTest or Input Type

     Following two rules covered by the same case:
    
     statEnv.namespace(Prefix1) = statEnv.namespace(Prefix2)
     ----------------------------------------------------------------------------------------------------
     statEnv |- test Prefix2:* with NODE of NODE Prefix1:* TypeSpecifier? : NODE Prefix1:* TypeSpecifier?

     ---------------------------------------------------------------------------------------------------
     statEnv |- test Prefix2:* with NODE of NODE TypeSpecifier? : NODE Prefix2:* TypeSpecifier?
  *)
   | ((_, _, "*"), (_, _, "*"))  -> 
       if (uri1 = uri2) then t else Schema_util.cxtype_empty
  (*
     fn:namespace-uri-from-QName( expanded-QName1) = statEnv.namespace(Prefix2)
     ----------------------------------------------------------------------------------------------
     statEnv |- test Prefix2:* with NODE of NODE QName1 TypeSpecifier? : NODE QName1 TypeSpecifier?
  *)
   | ((_, _, "*"), (_, _, _))  -> 
       if (uri1 = uri2) then t else Schema_util.cxtype_empty
  (*
     fn:namespace-uri-from-QName(expanded-QName2) = statEnv.namespace(Prefix1)
     -------------------------------------------------------------------------------------------------------------------
     statEnv |- test QName2 with NODE of NODE Prefix1:* TypeSpecifier? : NODE Prefix1:LocalPart2 TypeSpecifier?
  *)
   | ((_, _, _), (_, _, "*")) ->  
       if (uri1 = uri2) then make_new_type schema (prefix1, uri1, localpart2) t else Schema_util.cxtype_empty
  (*
     QName1 = QName2
     ----------------------------------------------------------------------------------------------------
     statEnv |- test QName2 with NODE of NODE QName1 TypeSpecifier? : NODE QName1 TypeSpecifier?
  *)
   | ((_, _, _), (_, _, _))  -> 
       if (rqname1 = rqname2) then t else Schema_util.cxtype_empty

(*
  Like match_on_name_test, match_on_kind_test generalizes the rules
  for applying a KindTest to an element or an attribute node type.
  The function takes the type associated with the kind test and the
  input type, applies the kind-matching rules for constructing a new
  type, which may use the TypeSpecifier part of the input type.
*)
let match_on_kind_test schema pnode_kind kind_test_type t =
  let result_type = 
  (*
     If the type of the expression is a subtype of the NODE kind
     test, then we are guaranteed that during evaluation, the
     expression's NODE value will always match the NODE kind
     test, and therefore the type of the entire expression is the type
     of the input expression.
     
     statEnv |- [NODETest]sequencetype = NODEType
     statEnv |- Type1 <: NODEType
     --------------------------------------------------------
     statEnv |- test NODETest with NODE of Type1 : Type1
  *)
  if (Subtyping_top.is_subtype_of schema t kind_test_type) then 
    (t)
  (*

     Conversely, if the type of the NODE kind test is a subtype of
     the expression, then during evaluation, the expression's NODE
     value may or may not match the NODE kind test, and therefore
     the type of the entire expression is zero-or-one of the type of
     the NODE kind test.

     statEnv |- [NODETest]sequencetype = NODEType
     statEnv |- NODEType <: Type1
     -----------------------------------------------------------------
     statEnv |- test NODETest with NODE of Type1 : NODEType ?
  *)
  else if (Subtyping_top.is_subtype_of schema kind_test_type t) then 
    (Schema_util.make_optional_type kind_test_type)
  (*
     If the types of the expression and NODE kind test are unrelated
     (i.e., neither type is a subtype of the other), then we must
     compare the structure of the type of the NODE test with the type
     of the NODE expression, as a NODE type or test may contain
     wildcards.
  *)
  else 
    let ((kind_name, kind_content_type), (input_name, input_content_type)) = 
      match pnode_kind with
      |	PrincipalElement -> 
	  let (name1, cnillable1, ctname1) = Schema_judge.lookup_element_type schema kind_test_type in
	  let (name2, cnillable2, ctname2) = Schema_judge.lookup_element_type schema t in
	  (* 
	     The trick here is to construct a local type with the QName used in the test and
             the type specifier used in the input type (and vice versa), then do the sub-type check.  
	  *)
	  let t1 = fmkcxtype (CElementLocal (name2,cnillable1,ctname1)) t.pcxtype_loc in
	  let t2 = fmkcxtype (CElementLocal (name1,cnillable2,ctname2)) t.pcxtype_loc in
	  ((name1, t1), (name2, t2))
      | PrincipalAttribute ->
	  let (name1, ctname1) = Schema_judge.lookup_attribute_type schema kind_test_type in
	  let (name2, ctname2) = Schema_judge.lookup_attribute_type schema t in
	  (* 
	     See comment above
	  *)
	  let t1 = fmkcxtype (CAttributeLocal (name2,ctname1)) t.pcxtype_loc in
	  let t2 = fmkcxtype (CAttributeLocal (name1,ctname2)) t.pcxtype_loc in
	  ((name1, t1), (name2, t2))
    in
    begin
      (*
	 In the first case, the NODE kind test contains an NODE name
	 and a type name and the input expression's type contains only a
	 type name. If the input expression's content type is a subtype of
	 the NODE kind test's content type, then the type of the entire
	 expression is zero-or-one of an NODE with the given name and
	 the input expression's content type.

	 statEnv |- [NODETest]sequencetype = NODE NODEName1 TypeSpecifier1      
	 statEnv |- TypeSpecifier1 expands to Type1
	 statEnv |- TypeSpecifier2 expands to Type2
	 statEnv |- Type2 <: Type1
	 -------------------------------------------------------------------------------------------
	 statEnv |- test NODETest with NODE of NODE TypeSpecifier2 : NODE NODEName1 TypeSpecifier2 ?
      *)
    if (input_name = Namespace_symbols_builtin.wild_symbol && 
	Subtyping_top.is_subtype_of schema input_content_type kind_content_type) then
      (Schema_util.make_optional_type (make_new_type_with_symbol schema kind_name input_content_type))
      (*
	 In the second case, the structure of the input types is
	 reversed: The input expression's type contains an NODE name
	 and a type name and the NODE kind test's type contains only
	 a type name. If the NODE kind test's content type is a
	 subtype of the input expression's content type, then the type
	 of the entire expression is zero-or-one of an NODE with the
	 given name and the NODE kind test's content type.

	 statEnv |- [NODETest]sequencetype = element TypeSpecifier1      
	 statEnv |- TypeSpecifier1 expands to Type1
	 statEnv |- TypeSpecifier2 expands to Type2
	 statEnv |- Type1 <: Type2
         --------------------------------------------------------------------------------------------------------------
	 statEnv |- test NODETest with element of element NODEName2 TypeSpecifier2 : element NODEName2 TypeSpecifier1 ?
      *)	
    else if (kind_name = Namespace_symbols_builtin.wild_symbol && 
	     Subtyping_top.is_subtype_of schema kind_content_type input_content_type) then
      (Schema_util.make_optional_type (make_new_type_with_symbol schema input_name kind_content_type))
    (*
       Lastly, if none of the above rules holds, then the type of the input expression is empty.
       statEnv |- [NODETest]sequencetype = NODENameOrWildcard1 TypeSpecifier1
       statEnv |- not(NODENameOrWildcard1 TypeSpecifier1 <: NODENameOrWildcard2 TypeSpecifier2)
       statEnv |- not(NODENameOrWildcard2 TypeSpecifier2 <: NODENameOrWildcard1 TypeSpecifier1)
       statEnv |- TypeSpecifier1 expands to Type1
       statEnv |- TypeSpecifier2 expands to Type2
       statEnv |- not(Type1 <: Type2)
       statEnv |- not(Type2 <: Type1)
       ----------------------------------------------------------------------------------------
       statEnv |- test NODETest with element of NODENameOrWildcard2 TypeSpecifier2 : empty
    *)
    else Schema_util.cxtype_empty
    end
  in 
  result_type
(*

  8.2.3.1 Static semantics of node tests
  
  ----------------------------------------------------------------
  statEnv |- test NodeTest with PrincipalNodeKind of Type1 : Type2
  
  holds when applying the node test NodeTest on the type Type1 in the
  context of the given principal node kind, yields the type Type2

*)
let rec node_test_type_of_judge typing_ctxt node_test pnode_kind t = 
   let schema = (Typing_context.schema_from_static_context typing_ctxt) in 
  (*
     ---------------------------------------------------------------  
     statEnv |- test NodeTest with PrincipalNodeKind of empty : empty
  *)
   if (Subtyping_top.is_subtype_of_empty_sequence schema t) then 
     Schema_util.cxtype_empty
  (*
    -------------------------------------------------------------- 
    statEnv |- test NodeTest with PrincipalNodeKind of none : none
  *)
   else if (Subtyping_top.is_subtype_of_empty_choice schema t) then 
     Schema_util.cxtype_none
  (* 
     statEnv |- test NodeTest with PrincipalNodeKind of Type1 : Type2
     --------------------------------------------------------------------------------------
     statEnv |- test NodeTest with PrincipalNodeKind of Type1 Occurrence : Type2 Occurrence
     
     statEnv |- test NodeTest with PrincipalNodeKind of Type1 : Type3
     statEnv |- test NodeTest with PrincipalNodeKind of Type2 : Type4
     --------------------------------------------------------------------------------
     statEnv |- test NodeTest with PrincipalNodeKind of Type1 & Type2 : Type3 & Type4
  
     statEnv |- test NodeTest with PrincipalNodeKind of Type1 : Type3
     statEnv |- test NodeTest with PrincipalNodeKind of Type2 : Type4
     --------------------------------------------------------------------------------
     statEnv |- test NodeTest with PrincipalNodeKind of Type1 , Type2 : Type3 , Type4
     
     statEnv |- test NodeTest with PrincipalNodeKind of Type1 : Type3
     statEnv |- test NodeTest with PrincipalNodeKind of Type2 : Type4
     ----------------------------------------------------------------------------
     statEnv |- test NodeTest with PrincipalNodeKind of Type1|Type2 : Type3|Type4
  *)
   else 
   begin
   match t.pcxtype_desc with 
      | CBound(t, m, n) ->   
	  let t' = node_test_type_of_judge typing_ctxt node_test pnode_kind t 
	  in fmkcxtype (CBound (t', m, n)) t.pcxtype_loc 
      | CSequence(t1, t2) -> 
	  let t1' = node_test_type_of_judge typing_ctxt node_test pnode_kind t1 in
	  let t2' = node_test_type_of_judge typing_ctxt node_test pnode_kind t2 in
	  fmkcxtype (CSequence (t1', t2')) t.pcxtype_loc 
      | CChoice(t1, t2) -> 
	  let t1' = node_test_type_of_judge typing_ctxt node_test pnode_kind t1 in
	  let t2' = node_test_type_of_judge typing_ctxt node_test pnode_kind t2 in
	  fmkcxtype (CChoice (t1', t2')) t.pcxtype_loc 
      | CInterleave(t1, t2) -> 
	  let t1' = node_test_type_of_judge typing_ctxt node_test pnode_kind t1 in
	  let t2' = node_test_type_of_judge typing_ctxt node_test pnode_kind t2 in
	  fmkcxtype (CInterleave (t1', t2')) t.pcxtype_loc 
   | _ -> 
   begin
     match (node_test, pnode_kind, t.pcxtype_desc) with
  (* [FS 8.2.3.1.1] Name Tests *)
     | (CPNameTest (prefix, uri, ncname), PrincipalElement, CElementRef cename) 
     | (CPNameTest (prefix, uri, ncname), PrincipalElement, CElementLocal (cename, _, _)) ->
	 match_on_name_test schema (Namespace_symbols.relem_symbol (prefix, uri, ncname)) cename t
     | (CPNameTest (prefix, uri, ncname), PrincipalAttribute, CAttributeRef caname)
     | (CPNameTest (prefix, uri, ncname), PrincipalAttribute, CAttributeLocal (caname, _)) ->
	 match_on_name_test schema (Namespace_symbols.relem_symbol (prefix, uri, ncname)) caname t
           (*
	      Lastly, if none of the above rules holds, then the type of the input expression is empty.
	      statEnv |- [NameTest]sequencetype = ElementNameOrWildcard1 TypeSpecifier1
	      statEnv |- not(ElementNameOrWildcard1 TypeSpecifier1 <: ElementNameOrWildcard2 TypeSpecifier2)
	      statEnv |- not(ElementNameOrWildcard2 TypeSpecifier2 <: ElementNameOrWildcard1 TypeSpecifier1)
	      statEnv |- TypeSpecifier1 expands to Type1
	      statEnv |- TypeSpecifier2 expands to Type2
	      statEnv |- not(Type1 <: Type2)
	      statEnv |- not(Type2 <: Type1)
	      -------------------------------------------------------------------------------------
	      statEnv |- test NameTest with element of ElementNameOrWildcard2 TypeSpecifier2 : empty
	    *)
     | (CPNameTest _,_,_) -> Schema_util.cxtype_empty
   (* [FS 8.2.3.1.2] Kind Tests *)
        (*
           Document kind test

	     If the type of the expression is a subtype of the document kind test,
	     then we are guaranteed that during evaluation, the expression's value
	     will always match the document kind test, and therefore the type of
	     the entire expression is the type of the input expression.

	     statEnv |- [DocumentTest]sequencetype = DocumentType
	     statEnv |- Type1 <: DocumentType
             ----------------------------------------------------------
	     statEnv |- test DocumentTest with element of Type1 : Type1
	*)
     | (CPNodeKindTest(CDocumentKind celement_test_opt, doctype), PrincipalElement, (CDocument elemtype)) -> 
	 if (Subtyping_top.is_subtype_of schema t doctype) then 
	   (t)
       (*
             Conversely, if the type of the document kind test is a
             subtype of the expression, then during evaluation, the
             expression's value may or may not match the document kind
             test, and therefore the type of the entire expression is
             zero-or-one of the type of the document kind test.

	     statEnv |- [DocumentTest]sequencetype = DocumentType
	     statEnv |- DocumentType <: Type1
             ----------------------------------------------------------
	     statEnv |- test DocumentTest with element of Type1 : DocumentType ?
       *)
	 else if (Subtyping_top.is_subtype_of schema doctype t) then 
	   (Schema_util.make_optional_type doctype)
       (*
	     If the types of the expression and document kind test are
	     unrelated, then we apply the kind test rule recursively
	     on the element types, which may yield a non-empty type.

	     statEnv |- [document-node (ElementTest)]sequencetype = DocumentType
	     statEnv |- not(Type1 <: DocumentType or DocumentType <: Type1)
	     statEnv |- test ElementTest with element of Type1 : Type2      
             not(Type2 <: empty)
             ---------------------------------------------------------------------------------------------------
	     statEnv |- test document-node (ElementTest) with element of document { Type1 } : document { Type2 }

	     If there is no non-empty type, then the kind test yields the empty type.

	     statEnv |- [document-node (ElementTest)]sequencetype = DocumentType
	     statEnv |- not(Type1 <: DocumentType or DocumentType <: Type1)
	     statEnv |- test ElementTest with element of Type1 : Type2      
	     Type2 <: empty
             -----------------------------------------------------------------------------
	     statEnv |- test document-node (ElementTest) with element of document { Type1 } : empty
       *)
	 else
	   begin
	     match (celement_test_opt, doctype.pcxtype_desc) with
	     |	(Some celement_test, CDocument(content_type)) ->
		 (let node_type =  
		   node_test_type_of_judge typing_ctxt 
		     (CPNodeKindTest(CElementKind(celement_test), content_type)) PrincipalElement elemtype
		 in  
		 if (Subtyping_top.is_subtype_of_empty_sequence schema node_type) then 
		   (Schema_util.cxtype_empty)
		 else
		   (fmkcxtype (CDocument(node_type)) t.pcxtype_loc ))
	     |	_ -> Schema_util.cxtype_empty
	   end
    (*  Element kind test *)
     | (CPNodeKindTest(CElementKind celement_test, elemtype), PrincipalElement, CElementRef _) 
     | (CPNodeKindTest(CElementKind celement_test, elemtype), PrincipalElement, CElementLocal _) ->
	 match_on_kind_test schema pnode_kind elemtype t
    (*	Attribute kind test *)
     | (CPNodeKindTest(CAttributeKind cattribute_test, attrtype), PrincipalAttribute, (CAttributeRef _))
     | (CPNodeKindTest(CAttributeKind cattribute_test, attrtype), PrincipalAttribute, (CAttributeLocal _)) ->
	 match_on_kind_test schema pnode_kind attrtype t
    (*
       -----------------------------------------------------------------------------------------------------------------
       statEnv |- test processing-instruction() with PrincipalNodeKind of processing-instruction : processing-instruction
       
       A processing-instruction node test with a string literal or
       NCName matches a processing instruction whose target has the
       given name. Since target matching cannot be checked
       statically, the static type of the node test is zero-or-one
       processing instruction.

       --------------------------------------------------------------------------------------
       statEnv |- test processing-instruction(StringLiteral | NCName) 
                   with PrincipalNodeKind of processing-instruction : processing-instruction ?

    *)
     | (CPNodeKindTest(CPIKind None, _), _, CPI _) -> t
     | (CPNodeKindTest(CPIKind(Some _), _), _, CPI _) -> Schema_util.make_optional_type t
   (*
      --------------------------------------------------------------------	
      statEnv |- test comment() with PrincipalNodeKind of comment : comment
   *)	
     | (CPNodeKindTest(CCommentKind, _), _, CComment) -> t
   (*
      --------------------------------------------------------------------	
      statEnv |- test text() with PrincipalNodeKind of text : text
   *)	
     | (CPNodeKindTest(CTextKind, _), _, CText) -> t
   (*
      --------------------------------------------------------------------	
      statEnv |- test node() with PrincipalNodeKind of NodeType : NodeType
   *)
     | (CPNodeKindTest(CAnyKind, _), _, _) -> t
   (*
      If none of the above rules apply, then the node test returns
      the empty sequence.
   *)	
     | (CPNodeKindTest _, _, _) -> Schema_util.cxtype_empty
    end
end
(*

   FS Section 4.2.1 Steps

                    statEnv.varType($fs:dot) = Type1
               statEnv |- Type1 <: [node()]sequencetype
                statEnv |- axis Axis of Type1 : Type2
                   Axis principal PrincipalNodeKind
  statEnv |- test NodeTest with PrincipalNodeKind of Type2 : Type3
  ----------------------------------------------------------------
                   statEnv |- Axis NodeTest : Type3
*)
let compute_type_axis_node_test typing_ctxt axis node_test fi = 
  let type1 = Typing_context.var_from_static_context typing_ctxt fs_dot fi in
  let type1' = Typing_errors.check_type_discard typing_ctxt type1 Schema_builtin.cxtype_node in
  let type2 = axis_type_of_judge typing_ctxt axis type1' in 
  let pnk = principal_node_kind axis in 
  let type3 = node_test_type_of_judge typing_ctxt node_test pnk type2 in
  type3 
  
