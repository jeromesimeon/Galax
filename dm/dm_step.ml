(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm_step.ml,v 1.8 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Dm_step
   Description:
     This module contains some generic evaluation code for XPath
     steps.
*)


open Error
open Datatypes
open Datatypes_util

open Xquery_ast
open Xquery_algebra_ast
open Xquery_common_ast

open Dm_types


(*****************************************)
(* Auxiliary functions for type matching *)
(*****************************************)

let node_kind_of_principal_node_kind pnk =
  match pnk with
  | Xquery_common_ast.PrincipalElement -> ElementNodeKind
  | Xquery_common_ast.PrincipalAttribute -> AttributeNodeKind


(* Note:
     This item_match function is really a hack right now, and will
     have to be replaced when the named typing support from the data
     model is in place.
   - Jerome *)

(* Note:
     item_matched_named_type (loosely) tests whether a value is in the
     domain of a type.  For elements and attributes, it *only*
     compares for equality the element/attribute value's name with the
     type's name.  It does not recursively check the node's content.

     For simple values, it *only* checks that the type of the value
     and sequencetype are equal.  It should use type subsumption on simple
     types.

     Group references and 'element of type' are not supported.
   - Jerome and Mary
 *)

let item_matches_element_test access_ops cxschema dtk node_object =
  match dtk with
  | AElementTest None -> true
  | AElementTest (Some (relem_sym2,None)) ->
      let relem_sym1 = access_ops.get_elem_node_name node_object in
      if not(Namespace_symbols.relem_subtag relem_sym1 relem_sym2)
      then
	false
      else
	true
  | AElementTest (Some (relem_sym2,Some rtype_sym2)) ->
      let (relem_sym1,rtype_sym1) =
	access_ops.get_elem_node_name_with_type node_object
      in
      if not(Namespace_symbols.relem_subtag relem_sym1 relem_sym2)
      then
	false
      else
	Schema_judge.derives_from cxschema rtype_sym1 rtype_sym2
  | ASchemaElementTest relem_sym2 ->
      (* For global elements, we must look at
	 substitution groups - Jerome *)
      let (relem_sym1,rtype_sym1) =
	access_ops.get_elem_node_name_with_type node_object
      in
      let elementref_content = Schema_judge.lookup_element_with_substitution_group cxschema relem_sym1 relem_sym2 in
      begin
	match elementref_content with
	| None -> false
	| Some (nillable,rtype_sym2) ->
	    (Schema_judge.derives_from cxschema rtype_sym1 rtype_sym2)
      end

let item_matches_attribute_test access_ops cxschema dtk node_object =
  match dtk with
  | AAttributeTest None -> true
  | AAttributeTest (Some (rattr_sym2,None)) ->
      let rattr_sym1 = access_ops.get_attr_node_name node_object in
      if not(Namespace_symbols.rattr_subtag rattr_sym1 rattr_sym2)
      then false
      else true
  | AAttributeTest (Some (rattr_sym2,Some rtype_sym2)) ->
      let (rattr_sym1,rtype_sym1) =
	access_ops.get_attr_node_name_with_type node_object
      in
      if not(Namespace_symbols.rattr_subtag rattr_sym1 rattr_sym2)
      then false
      else Schema_judge.derives_from cxschema rtype_sym1 rtype_sym2
  | ASchemaAttributeTest rattr_sym2 ->
      let (rattr_sym1,rtype_sym1) =
	access_ops.get_attr_node_name_with_type node_object
      in
      if not(Namespace_symbols.rattr_equal rattr_sym1 rattr_sym2)
      then
	false
      else
	begin
	  let (_,rtype_sym2) = Schema_judge.lookup_attribute cxschema rattr_sym2 in
	  (Schema_judge.derives_from cxschema rtype_sym1 rtype_sym2)
	end

let item_matches_kind_test access_ops cxschema dtk node_object =
  if dtk = AAnyKind then true else
  begin
    match access_ops.get_node_kind node_object with
    | DocumentNodeKind ->
	begin
	  match dtk with
	  | ADocumentKind None -> true
	  | ADocumentKind (Some dtk) ->
	      (* We need to get the single element node in there - Jerome *)
	      let n = access_ops.get_single_element_node node_object in
	      item_matches_element_test access_ops cxschema dtk n
	  | _ ->
	      false
	end
    | ElementNodeKind ->
	begin
	  match dtk with
	  | AElementKind dtk ->
	      item_matches_element_test access_ops cxschema dtk node_object
	  | _ -> false
	end
    | AttributeNodeKind ->
	begin
	  match dtk with
	  | AAttributeKind dtk ->
	      item_matches_attribute_test access_ops cxschema dtk node_object
	  | _ -> false
	end
    | TextNodeKind ->
	begin
	  match dtk with
	  | ATextKind -> true
	  | _ -> false
	end
    | ProcessingInstructionNodeKind ->
	begin
	  match dtk with
	  | APIKind None ->
	      true
	  | APIKind (Some t) ->
	      let target_string = access_ops.get_pi_target node_object in
	      (t = target_string)
	  | _ -> false
	end
    | CommentNodeKind ->
	begin
	  match dtk with
	  | ACommentKind -> true
	  | _ -> false
	end
  end

let eval_node_test_gen access_ops ocxschema a nt node_object =
  let cxschema =
    match ocxschema with
    | None -> Schema_builtin.built_in_cxschema
    | Some cxschema -> cxschema
  in
  let pnk = Xquery_common_ast_util.principal_node_kind a in
  let pnk = node_kind_of_principal_node_kind pnk in
  match nt with
  | APNameTest test_symbol ->
      begin
	match access_ops.get_node_kind node_object with
	| ElementNodeKind ->
	    if pnk = ElementNodeKind
	    then
	      let relem_symbol = access_ops.get_elem_node_name node_object in
	      Namespace_symbols.anon_subtag relem_symbol test_symbol
	    else
	      false
	| AttributeNodeKind ->
	    if pnk = AttributeNodeKind
	    then
	      let rattr_symbol = access_ops.get_attr_node_name node_object in
	      Namespace_symbols.anon_subtag rattr_symbol test_symbol
	    else
	      false
	| _ ->
	    false
      end
  | APNodeKindTest node_kind ->
      (* New code to support kind tests 09/06/2005 - Jerome *)
      item_matches_kind_test access_ops cxschema node_kind node_object

