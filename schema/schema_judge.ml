(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_judge.ml,v 1.36 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_judge
   Description:
     This modules implements basic judgments on the type system.
 *)

open Error

open Datatypes

open Namespace_symbols
open Namespace_symbols_util
open Namespace_symbols_builtin

open Occurrence

open Xquery_common_ast
open Xquery_core_ast
open Xquery_type_core_ast
open Xquery_type_core_ast_util

open Schema_util
open Schema_builtin

(*********************************************************)
(* [FS 8.1.4] Element and attribute type lookup (Static) *)
(*********************************************************)

let find_element cdecls cename =
  if SQNameHashtbl.mem cdecls cename
  then
    let (_, (csubstitutes_for, cnillable, ctname)) =
      SQNameHashtbl.find cdecls cename in
    (cename, csubstitutes_for, cnillable, ctname)
  else
    let en = symbol_prefix_string cename in
    raise (Query (Schema ("Global declaration for element " ^ en ^ " not found")))

let lookup_element cxschema cename =
  find_element cxschema.cxschema_element_declarations cename

let rec lookup_element_with_substitution_group cxschema cename1 cename2 =
  if symbol_equal cename1 cename2
  then
    let (_,_,cnillable,ctname) = lookup_element cxschema cename2 in
    Some (cnillable,ctname)
  else
    begin
      let (_,csubstitutes_for,_,_) = lookup_element cxschema cename2 in
      match csubstitutes_for with
      | CNonSubstitutesFor ->
	  None
      | CSubstitutesFor cename3 ->
	  try
	    lookup_element_with_substitution_group cxschema cename1 cename3
	  with
	  | _ ->
	      None
    end

let substitutes_for cxschema cename1 cename2 =
  try
    match lookup_element_with_substitution_group cxschema cename1 cename2 with
    | None -> false
    | Some _ -> true
  with
  | _ -> false

let lookup_element_type schema cxtype = 
  match cxtype.pcxtype_desc with 
  | CElementRef cename -> 
      let (cename, csubstitutes_for, cnillable, ctname) = lookup_element schema cename in (cename, cnillable, ctname)
  | CElementLocal (cename, cnillable, ctname) -> (cename, cnillable, ctname)
  | _ -> raise(Query(Internal_Error("lookup_element_type judgment applied to non-element type "^(Print_top.bprintf_cxtype "" cxtype))))


(*********************************************************)
(* [FS 8.1.4] Element and attribute type lookup (Static) *)
(*********************************************************)

let find_attribute cdecls caname =
  if SQNameHashtbl.mem cdecls caname
  then
    let (_, ctname) = SQNameHashtbl.find cdecls caname in
    (caname, ctname)
  else
    let an = symbol_prefix_string caname in
    raise (Query (Schema ("Global declaration for attribute " ^ an ^ " not found")))

let lookup_attribute cxschema caname =
  find_attribute cxschema.cxschema_attribute_declarations caname

let lookup_attribute_type schema cxtype = 
  match cxtype.pcxtype_desc with 
  | CAttributeRef cename -> 
      let (caname, ctname) = lookup_attribute schema cename in (cename, ctname)
  | CAttributeLocal (caname, ctname) -> (caname, ctname)
  | _ -> raise(Query(Internal_Error("lookup_attribute_type judgment applied to non-attribute type "^(Print_top.bprintf_cxtype "" cxtype))))

(* Type lookup *)  

let lookup_type_decl cxschema ctname =
  try 
    SQNameHashtbl.find cxschema.cxschema_type_declarations ctname
  with
  | Not_found ->
      let tn = symbol_prefix_string ctname in
      raise (Query (Schema ("Type " ^ tn ^ " undeclared")))

let check_declared_type cxschema ctname =
  ignore(lookup_type_decl cxschema ctname)

let lookup_type_deriv cxschema ctname =
  let decl = lookup_type_decl cxschema ctname in
  decl.ctypedecl_deriv

(* Lookup atomic type *)

let directly_derives_from cxschema ctname1 =
  let (ctname2, _, _) = lookup_type_deriv cxschema ctname1 in
  ctname2

(********************************************)
(* Computes content models for simple types *)
(********************************************)

let rec build_atomic_simple_type cxschema ctname =
  if is_built_in_atomic_type cxschema ctname
  then
    ctname
  else
    let decl = lookup_type_decl cxschema ctname in
    let (ctname2,cattribute_content,cchildren_content) = decl.ctypedecl_deriv in 
    match (cattribute_content,cchildren_content) with
    | (None,CAtomicTypeRestriction) ->
	(build_atomic_simple_type cxschema ctname2)
    | (Some _, _) ->
	raise (Query (Schema "XML Schema Type hierarchy is wrong: user defined atomic type definition has attributes!"))
    | (_,_) ->
	raise (Query (Schema "XML Schema Type hierarchy is wrong: user defined atomic type does not derives from a built-in atomic type"))

and build_union_simple_type cxschema ctname_list =
  let get_atomic_type ctname =
    if is_built_in_atomic_type cxschema ctname
    then
      build_atomic_simple_type cxschema ctname
    else
      let decl = lookup_type_decl cxschema ctname in
      let (ctname2,cattribute_content,cchildren_content) = decl.ctypedecl_deriv in
      match (cattribute_content,cchildren_content) with
      | (None,CAtomicTypeRestriction) ->
	  build_atomic_simple_type cxschema ctname2
      | (None,CSimpleTypeUnion ctname_list3) ->
	  raise (Query (Schema "Cannot have a union simple type derive from another union simple type (See XML Schema Part 2 : Datatypes [4.1.1 The Simple Type Definition Schema Component])"))
      | (None,CSimpleTypeList ctname3) ->
	  raise (Query (Schema "Cannot have a union simple type derive from another list simple type (See XML Schema Part 2 : Datatypes [4.1.1 The Simple Type Definition Schema Component])"))
      | (Some _, _) ->
	  raise (Query (Schema "XML Schema Type hierarchy is wrong: user defined simple type definition has attributes!"))
      | (_,_) ->
	  raise (Query (Schema "XML Schema Type hierarchy is wrong: user defined simple type does not derives from a simple type"))
  in
  List.map get_atomic_type ctname_list

and build_list_simple_type cxschema ctname =
  if is_built_in_atomic_type cxschema ctname
  then
    [build_atomic_simple_type cxschema ctname]
  else
    let decl = lookup_type_decl cxschema ctname in
    let (ctname2,cattribute_content,cchildren_content) = decl.ctypedecl_deriv in 
    match (cattribute_content,cchildren_content) with
    | (None,CAtomicTypeRestriction) ->
	[build_atomic_simple_type cxschema ctname2]
    | (None,CSimpleTypeUnion ctname_list3) ->
	build_union_simple_type cxschema ctname_list3
    | (None,CSimpleTypeList _) ->
	raise (Query (Schema "Cannot have a list simple type derive from another list simple type (See XML Schema Part 2 : Datatypes [4.1.1 The Simple Type Definition Schema Component])"))
    | (Some _, _) ->
	raise (Query (Schema "XML Schema Type hierarchy is wrong: user defined simple type definition has attributes!"))
    | (_,_) ->
	raise (Query (Schema "XML Schema Type hierarchy is wrong: user defined simple type does not derives from a simple type"))


(* [FS 8.1.1] Derives from *)

let rec derives_from cxschema ctname1 ctname2 =
  if symbol_equal ctname1 ctname2
  then
    true
  else
    if is_xs_anytype ctname1
    then
      false
    else
      derives_from cxschema (directly_derives_from cxschema ctname1) ctname2

(* extension [FS 8.1.5] *)

let extended_by_is cxschema cxtype1 cxtype2 =
  make_interleave_cxtypes cxtype1 cxtype2

(* [FS 8.1.7] Type adjustment *)

let adjusts_to cxschema (cmixed,content_cxtype) =
  let cxtype_extensions =
    match cmixed with
    | Mixed ->
	mixed_content
    | NonMixed ->
	Schema_util.cxtype_empty
  in
  let t = extended_by_is cxschema content_cxtype cxtype_extensions
  in
(*  Print_top.printf_cxtype ("\nIn adjusts_to: ") content_cxtype;
  Print_top.printf_cxtype ("\n ==> ") t; *)
  t
  

let adjusts_attributes_to cxschema opt_attributes_cxtype =
  let cxtype_extensions = built_in_attributes in
  match opt_attributes_cxtype with
  | None ->
      cxtype_extensions
  | Some attributes_cxtype ->
      extended_by_is cxschema attributes_cxtype cxtype_extensions


(* [FS 8.1.9] Type expansion 

   From FS:

   The expands to judgment is one of the most important static
   judgments. It is used in the static semantics of the child axis
   [8.2.2.1 Static semantics of axes], which is used in the definition
   of many other rules that extract element types from an arbitrary
   content type.

   The judgment takes a type name and computes the union of all types
   derived from the given type. If the type is nillable, it also makes
   sure the content model allows the empty sequence. If the type is
   mixed, it also adjusts the type to include the mixed content
   model. The judgment depends on the extended with union
   interpretation of judgment to recursively compute all derived
   types.

*)

let compatible_mixed cmixed cmixed2 =
  if (cmixed = cmixed2)
  then cmixed
  else raise (Query (Schema "Derivation from extension with incompatible mixed types"))

let rec expands_restriction_to cxschema ctname (cmixed,content_cxtype,cattribute_content) =
  let decl = lookup_type_decl cxschema ctname in
  let (ctname2,cattribute_content2,celement_content2) = decl.ctypedecl_deriv in
  match celement_content2 with
  | CComplexTypeRestriction(cmixed2,content_cxtype2) ->
      let cmixed3 = compatible_mixed cmixed cmixed2 in
      let content_cxtype3 = extends_element_content content_cxtype2 content_cxtype in
      let cattribute_content3 = extends_attribute_content cattribute_content2 cattribute_content in
      (None,cmixed3,content_cxtype3,cattribute_content3)
  | CComplexTypeExtension(cmixed2,content_cxtype2) ->
	let (complex,cmixed3,content_cxtype',cattribute_content') =
	  expands_restriction_to
	    cxschema
	    ctname2
	    (cmixed2,content_cxtype2,cattribute_content2)
	in
	let content_cxtype3 = extends_element_content content_cxtype2 content_cxtype' in
	let cattribute_content3 = extends_attribute_content cattribute_content2 cattribute_content' in
	(None,cmixed3,content_cxtype3,cattribute_content3)

  | CAtomicTypeRestriction ->
	let x =
	  (SimpleKind (AtomicKind (build_atomic_simple_type cxschema ctname)),NonMixed,cattribute_content)
	in
	if not(is_simple_cxtype content_cxtype)
	then
	  raise (Query (Schema "Complex content in extension from simple type"))
	else
	  (Some x,cmixed,content_cxtype,cattribute_content)
  | CSimpleTypeList ctname3 ->
	let x =
	  (SimpleKind (ListKind (build_list_simple_type cxschema ctname3)),NonMixed,cattribute_content)
	in
	if not(is_simple_cxtype content_cxtype)
	then
	  raise (Query (Schema "Complex content in extension from simple type"))
	else
	  (Some x,cmixed,content_cxtype,cattribute_content)
  | CSimpleTypeUnion ctname_list ->
	let x =
	(SimpleKind (UnionKind (build_union_simple_type cxschema ctname_list)),NonMixed,cattribute_content)
	in
	if not(is_simple_cxtype content_cxtype)
	then
	  raise (Query (Schema "Complex content in extension from simple type"))
	else
	  (Some x,cmixed,content_cxtype,cattribute_content)

and expands_to cxschema (cnillable,ctname) =
  let decl = lookup_type_decl cxschema ctname in
  let (ctname2,cattribute_content,celement_content) = decl.ctypedecl_deriv in 
  let nillable_wrapper =
    match cnillable with 
    | Nillable -> (fun t -> make_optional_type t)
    | NonNillable -> (fun t -> t)
  in
  let (content_cxtype_result,cmixed,cattribute_content') =
    match celement_content with
    | CComplexTypeRestriction(cmixed,content_cxtype) ->
	(ComplexKind (adjusts_to cxschema (cmixed, nillable_wrapper content_cxtype)),cmixed,cattribute_content)
    | CComplexTypeExtension(cmixed,content_cxtype) ->
	let (complex,cmixed',content_cxtype',cattribute_content') =
	  expands_restriction_to
	    cxschema
	    ctname2
	    (cmixed, nillable_wrapper content_cxtype,cattribute_content)
	in
	begin
	  match complex with
	  | None ->
	      (ComplexKind (adjusts_to cxschema (cmixed', content_cxtype')),cmixed',cattribute_content')
	  | Some x ->
	      x
	end
    | CAtomicTypeRestriction ->
	(SimpleKind (AtomicKind (build_atomic_simple_type cxschema ctname)),NonMixed,cattribute_content)
    | CSimpleTypeList ctname3 ->
	(SimpleKind (ListKind (build_list_simple_type cxschema ctname3)),NonMixed,cattribute_content)
    | CSimpleTypeUnion ctname_list ->
	(SimpleKind (UnionKind (build_union_simple_type cxschema ctname_list)),NonMixed,cattribute_content)
  in
  let attributes_cxtype_result = adjusts_attributes_to cxschema cattribute_content' in
  (attributes_cxtype_result,content_cxtype_result,cmixed)

(*
  let (attrtype,elemtype,mixed) = 
    match cnillable with 
    | Nillable -> 
	(attributes_cxtype_result, make_optional_type(content_cxtype_result),NonMixed)
    | NonNillable -> 
	(attributes_cxtype_result,content_cxtype_result,cmixed)
  in
(*  Print_top.printf_cxtype ("\nIn expands_to: "^(Namespace_symbols.symbol_prefix_string ctname)^"\n ==> attrtype: ") attrtype; *)
  (attrtype,elemtype,mixed) 
*)

let expands_attribute_to_simple_kind cxschema ctname =
  let decl = lookup_type_decl cxschema ctname in
  let (ctname2,noattribute_content,cattribute_content) = decl.ctypedecl_deriv in 
  match noattribute_content with
  | Some _ ->
      raise (Query (Schema "Attribute type should not contain attributes!"))
  | None ->
      let content_cxtype_result =
	match cattribute_content with
	| CComplexTypeRestriction(cmixed,content_cxtype) ->
	    raise (Query (Schema "Attribute type should not be of a complex type"))
	| CComplexTypeExtension(cmixed,content_cxtype) ->
	    raise (Query (Schema "Attribute type should not be of a complex type"))
	| CAtomicTypeRestriction ->
	    AtomicKind (build_atomic_simple_type cxschema ctname)
	| CSimpleTypeList ctname3 ->
	    ListKind (build_list_simple_type cxschema ctname3)
	| CSimpleTypeUnion ctname_list ->
	    UnionKind (build_union_simple_type cxschema ctname_list)
      in
      content_cxtype_result

let atomic_type_of_simple_kind schema sk = 
  match sk with
  | AtomicKind at -> make_atomic_type schema at 
  | ListKind atl  -> 
      let itemtype = 
	begin
	  match atl with
	  | [] ->  cxtype_empty
	  | [a] -> make_atomic_type schema a
	  | a :: atl' -> 
	    (List.fold_left (fun t a -> make_choice_cxtypes (make_atomic_type schema a) t) (make_atomic_type schema a) atl')
	end
      in make_zeroormore_type itemtype
  | UnionKind atl -> 
      begin
	match atl with
	| [] ->  cxtype_empty
	| [a] -> make_atomic_type schema a
	| a :: atl' -> 
	    (List.fold_left (fun t a -> make_choice_cxtypes (make_atomic_type schema a) t) (make_atomic_type schema a) atl')
      end

let expands_attribute_to schema ctname = 
  atomic_type_of_simple_kind schema (expands_attribute_to_simple_kind schema ctname)

(************************)
(* Additional judgments *)
(************************)

(* Does the type contain empty ? *)

let rec empty cxtype =
  match cxtype.pcxtype_desc with
  | CAtomicRef _ ->
      false
  | CElementRef _ ->
      false
  | CElementLocal _ ->
      false
  | CAttributeRef _ ->
      false
  | CAttributeLocal _ ->
      false
  | CDocument _ ->
      false
  | CText ->
      false
  | CPI _ ->
      false
  | CComment ->
      false
  | CBound (cxtype',min,max) ->
      if min = Occurrence.occurs_zero
      then
	true
      else
	(empty cxtype')
  | CSequence (cxtype1, cxtype2) ->
      ((empty cxtype1) && (empty cxtype2))
  | CEmpty ->
      true
  | CChoice (cxtype1, cxtype2) ->
      ((empty cxtype1) || (empty cxtype2))
  | CNone ->
      false
  | CInterleave (cxtype1, cxtype2) ->
      ((empty cxtype1) && (empty cxtype2))

let type_contains_empty cxtype =
  empty cxtype


(***************)
(* Transitions *)  
(***************)

let is_nilled_element cnillable xsi_nil =
  match cnillable with
  | Nillable ->
      xsi_nil
  | NonNillable ->
      if xsi_nil
      then
	raise (Query (Validation "Element has xsi:nil set to true, but is not nillable"))
      else
	false

let compute_ctname_with_xsi_type cxschema ctname0 xsi_type =
  match xsi_type with
  | None -> ctname0
  | Some ctname1 ->
      if (derives_from cxschema ctname1 ctname0)
      then
	ctname1
      else
	let tn0 = symbol_prefix_string ctname0 in
	let tn1 = symbol_prefix_string ctname1 in
	raise (Query (Validation ("Element has xsi:type set to " ^ tn1 ^ " which is not derived from " ^ tn0)))

let rec element_transition cxschema cxtype0 cename xsi_nil xsi_type =
  match cxtype0.pcxtype_desc with
  | CAtomicRef _ ->
      []
  | CElementRef cename' ->
      begin
	match lookup_element_with_substitution_group cxschema cename cename' with
	| None ->
	    []
	| Some (cnillable,ctname) ->
	    let nilled = is_nilled_element cnillable xsi_nil in
	    let ctname' = compute_ctname_with_xsi_type cxschema ctname xsi_type in
	    [((nilled,ctname'),Schema_util.cxtype_empty)]
      end
  | CElementLocal (cename', cnillable, ctname) ->
      begin
	if (symbol_equal cename cename')
	then
	  let nilled = is_nilled_element cnillable xsi_nil in
	  let ctname' = compute_ctname_with_xsi_type cxschema ctname xsi_type in
	  [((nilled, ctname'),Schema_util.cxtype_empty)]
	else
	  []
      end
  | CAttributeRef _ ->
      []
  | CAttributeLocal _ ->
      []
  | CDocument _ ->
      []
  | CText ->
      []
  | CPI _ ->
      []
  | CComment ->
      []
  | CBound (cxtype1,min,max) ->
      let inner_element_transitions = element_transition cxschema cxtype1 cename xsi_nil xsi_type in
      let min' = Occurrence.minus 1 min in
      let max' = Occurrence.minus 1 max in
      let add_on_cxtype =
	if max' = Occurrence.occurs_zero
	then
	  Schema_util.cxtype_empty
	else
	  fmkcxtype (CBound (cxtype1,min',max')) Finfo.bogus
      in
      List.map (fun (x,y) -> (x,make_sequence_cxtypes y add_on_cxtype)) inner_element_transitions
  | CSequence (cxtype1, cxtype2) ->
      let first_part_element_transitions =
	(element_transition cxschema cxtype1 cename xsi_nil xsi_type)
      in
      let first_part_element_transitions_result =
	List.map (fun (x,r) -> (x,make_sequence_cxtypes r cxtype2)) first_part_element_transitions
      in
      if (type_contains_empty cxtype1)
      then first_part_element_transitions_result@(element_transition cxschema cxtype2 cename xsi_nil xsi_type)
      else first_part_element_transitions_result
  | CEmpty ->
      []
  | CChoice (cxtype1, cxtype2) ->
      (element_transition cxschema cxtype1 cename xsi_nil xsi_type)@(element_transition cxschema cxtype2 cename xsi_nil xsi_type)
  | CNone ->
      []
  | CInterleave (cxtype1, cxtype2) ->
      let first_part_element_transitions =
	(element_transition cxschema cxtype1 cename xsi_nil xsi_type)
      in
      let first_part_element_transitions_result =
	List.map (fun (x,r) -> (x,make_interleave_cxtypes r cxtype2)) first_part_element_transitions
      in
      let second_part_element_transitions =
	(element_transition cxschema cxtype2 cename xsi_nil xsi_type)
      in
      let second_part_element_transitions_result =
	List.map (fun (x,r) -> (x,make_interleave_cxtypes cxtype1 r)) second_part_element_transitions
      in
      first_part_element_transitions_result@second_part_element_transitions_result

let rec attribute_transition cxschema cxtype0 caname =
  match cxtype0.pcxtype_desc with
  | CAtomicRef caname' ->
      []
  | CElementRef _ ->
      []
  | CElementLocal _ ->
      []
  | CAttributeRef caname' ->
      if (symbol_equal caname caname')
      then
	let (_,ctname) = lookup_attribute cxschema caname' in
	[(ctname,Schema_util.cxtype_empty)]
      else
	[]
  | CAttributeLocal (caname',ctname) ->
      if (symbol_equal caname caname')
      then
	[(ctname,Schema_util.cxtype_empty)]
      else
	[]
  | CDocument _ ->
      []
  | CText ->
      []
  | CPI _ ->
      []
  | CComment ->
      []
  | CBound (cxtype1,min,max) ->
      let inner_attribute_transitions = attribute_transition cxschema cxtype1 caname in
      let min' = Occurrence.minus 1 min in
      let max' = Occurrence.minus 1 max in
      let add_on_cxtype =
	if max' = Occurrence.occurs_zero
	then
	  Schema_util.cxtype_empty 
	else
	  fmkcxtype (CBound (cxtype1,min',max')) Finfo.bogus
      in
      List.map (fun (x,y) -> (x,make_sequence_cxtypes y add_on_cxtype)) inner_attribute_transitions
  | CSequence (cxtype1, cxtype2) ->
      let first_part_attribute_transitions =
	(attribute_transition cxschema cxtype1 caname)
      in
      let first_part_attribute_transitions_result =
	List.map (fun (x,r) -> (x,make_sequence_cxtypes r cxtype2)) first_part_attribute_transitions
      in
      if (type_contains_empty cxtype1)
      then first_part_attribute_transitions_result@(attribute_transition cxschema cxtype2 caname)
      else first_part_attribute_transitions_result
  | CEmpty ->
      []
  | CChoice (cxtype1, cxtype2) ->
      (attribute_transition cxschema cxtype1 caname)@(attribute_transition cxschema cxtype2 caname)
  | CNone ->
      []
  | CInterleave (cxtype1, cxtype2) ->
      let first_part_attribute_transitions =
	(attribute_transition cxschema cxtype1 caname)
      in
      let first_part_attribute_transitions_result =
	List.map (fun (x,r) -> (x,make_interleave_cxtypes r cxtype2)) first_part_attribute_transitions
      in
      let second_part_attribute_transitions =
	(attribute_transition cxschema cxtype2 caname)
      in
      let second_part_attribute_transitions_result =
	List.map (fun (x,r) -> (x,make_interleave_cxtypes cxtype1 r)) second_part_attribute_transitions
      in
      first_part_attribute_transitions_result@second_part_attribute_transitions_result


(* Checks for duplicates in the result of a first_rest *)

let element_transition_final cxschema cxtype0 cename xsi_nil xsi_type =
  let transitions = element_transition cxschema cxtype0 cename xsi_nil xsi_type in
  match transitions with
  | [] ->
      let en = symbol_prefix_string cename in
      raise (Query (Validation ("No matching declaration for element " ^ en ^ " in content model")))
  | [((nilled,ctname),sibling_cxtype)] ->
      let cnillable = if nilled then Nillable else NonNillable in 
      let (attributes_cxtype,content_cxtype,cmixed) = expands_to cxschema (cnillable,ctname) in
      (sibling_cxtype,ctname,cmixed,attributes_cxtype,content_cxtype,nilled)
  | _ ->
      let en = symbol_prefix_string cename in
      raise (Query (Validation ("Multiple matching declarations for element " ^ en ^ " in content model")))

let attribute_transition_final cxschema cxtype0 caname =
  let transitions = attribute_transition cxschema cxtype0 caname in
  match transitions with
  | [] ->
      let an = symbol_prefix_string caname in
      raise (Query (Validation ("No matching declaration for attribute " ^ an ^ " in content model")))
  | [(ctname,sibling_cxtype)] ->
      let content_cxtype = expands_attribute_to_simple_kind cxschema ctname in
      (sibling_cxtype,ctname,content_cxtype)
  | _ ->
      let an = symbol_prefix_string caname in
      raise (Query (Validation ("Multiple matching declarations for attribute " ^ an ^ " in content model")))


(* ???!!!HACK Alert! 
   The order here matters!!!! Most specific types must come first!!! 
*)
let closest_builtin_integer_type cxschema ty =
  if (derives_from cxschema ty xs_negativeInteger) then xs_negativeInteger
  else if (derives_from cxschema ty xs_negativeInteger) then xs_negativeInteger
  else if (derives_from cxschema ty xs_byte) then xs_byte
  else if (derives_from cxschema ty xs_short) then xs_short
  else if (derives_from cxschema ty xs_int) then xs_int
  else if (derives_from cxschema ty xs_long) then xs_long
  else if (derives_from cxschema ty xs_unsignedByte) then xs_unsignedByte
  else if (derives_from cxschema ty xs_unsignedShort) then xs_unsignedShort
  else if (derives_from cxschema ty xs_unsignedInt) then xs_unsignedInt
  else if (derives_from cxschema ty xs_unsignedLong) then xs_unsignedLong
  else if (derives_from cxschema ty xs_positiveInteger) then xs_positiveInteger
  else if (derives_from cxschema ty xs_nonNegativeInteger) then xs_nonNegativeInteger
  else xs_integer

let atomic_type_of_typename cxschema ty =
  if (derives_from cxschema ty xs_integer) then ATInteger
  else if (derives_from cxschema ty xs_string) then ATString
  else if (derives_from cxschema ty xs_boolean) then ATBoolean
  else if (derives_from cxschema ty xs_decimal) then ATDecimal
  else if (derives_from cxschema ty xs_float) then ATFloat
  else if (derives_from cxschema ty xs_double) then ATDouble
  else if (derives_from cxschema ty xs_dayTimeDuration) then ATDayTimeDuration
  else if (derives_from cxschema ty xs_yearMonthDuration) then ATYearMonthDuration
  else if (derives_from cxschema ty xs_duration) then ATDuration
  else if (derives_from cxschema ty xs_dateTime) then ATDateTime
  else if (derives_from cxschema ty xs_time) then ATTime
  else if (derives_from cxschema ty xs_date) then ATDate
  else if (derives_from cxschema ty xs_gYearMonth) then ATGYearMonth
  else if (derives_from cxschema ty xs_gYear) then ATGYear
  else if (derives_from cxschema ty xs_gMonthDay) then ATGMonthDay
  else if (derives_from cxschema ty xs_gDay) then ATGDay
  else if (derives_from cxschema ty xs_gMonth) then ATGMonth
  else if (derives_from cxschema ty xs_hexBinary) then ATHexBinary
  else if (derives_from cxschema ty xs_base64Binary) then ATBase64Binary
  else if (derives_from cxschema ty xs_anyURI) then ATAnyURI
  else if (derives_from cxschema ty xs_QName) then ATQName
  else if (derives_from cxschema ty xs_NOTATION) then ATNOTATION
  else if (derives_from cxschema ty xs_untypedAtomic) then ATUntypedAtomic
  else if (derives_from cxschema ty xs_anyAtomicType) then ATAnyAtomic
  else raise (Query (Schema_Internal ("Typename "^(Namespace_symbols.symbol_prefix_string ty ^" is not derived from xs:anyAtomicType"))))

let atomic_type_of_cxtype cxschema cxtype =
  match cxtype.pcxtype_desc with
  | CAtomicRef ctname -> atomic_type_of_typename cxschema ctname
  | _ -> raise (Query (Static_Internal("Trying to extract an atomic type from a complex type")))

(*
  compare_cxtypes: A total order function on cxtypes. 
*)
let rec compare_cxtypes schema t1 t2 = 
  match (t1.pcxtype_desc, t2.pcxtype_desc) with 
  | CInterleave(u1, v1), CInterleave(u2, v2) ->
      let c1 = (compare_cxtypes schema u1 u2) in
      if (c1 = 0) then 
	(compare_cxtypes schema v1 v2) 
      else c1
  | CInterleave(u1, v1), _ -> 1
  | CChoice(u1, v1), CChoice(u2, v2) ->
      let c1 = (compare_cxtypes schema u1 u2) in
      if (c1 = 0) then (compare_cxtypes schema v1 v2) else c1
  | CChoice(u1, v1), _ -> 1
  | CSequence(u1, v1), CSequence(u2, v2) ->
      let c1 = (compare_cxtypes schema u1 u2) in
      if (c1 = 0) then (compare_cxtypes schema v1 v2) else c1
  | CSequence(u1, v1), _ -> 1
  | CBound (t1, min1, max1), CBound (t2, min2, max2) ->
      let c1 = compare min1 min2 in
      if (c1 = 0) then 
	let c2 = compare max1 max2 in 
	if (c2 = 0) then 
	  compare_cxtypes schema t1 t2 
	else c2
      else c1
  | CBound (t1, min1, max1), _ -> 1
  | CElementRef r1, CElementRef r2 ->
      compare r1 r2
  | CElementRef r1, _ -> 1
  | CElementLocal (re1, n1, rt1), CElementLocal (re2, n2, rt2) ->
      let c1 = compare re1 re2 in
      if (c1 = 0) then 
	let c2 = compare n1 n2 in
	if (c2 = 0) then compare rt1 rt2
	else c2
      else
	c1
  | CElementLocal (re1, n1, rt1), _ -> 1
  | CAttributeRef r1, CAttributeRef r2 ->
      compare r1 r2 
  | CAttributeRef r1, _ -> 1
  | CAttributeLocal (ra1, rt1), CAttributeLocal (ra2, rt2) ->
      let c1 = compare ra1 ra2 in
      if (c1 = 0) then 
	compare rt1 rt2
      else
	c1
  | CAttributeLocal (ra1, rt1), _ -> 1
  | CDocument t1, CDocument t2 ->
      compare_cxtypes schema t1 t2
  | CDocument t1, _ -> 1
  | CText, CText -> 0
  | CText, _ -> 1
  | CPI s1, CPI s2 -> compare s1 s2
  | CPI _, _ -> 1
  | CComment, CComment -> 0
  | CComment, _ -> 1
  | CAtomicRef r1, CAtomicRef r2 -> 
      if (derives_from schema r1 r2) then -1
      else if (derives_from schema r2 r1) then 1
      else compare r1 r2 
  | CAtomicRef _, _ -> 1
  | CEmpty, CEmpty -> 0
  | CEmpty, _ -> 1
  | CNone, CNone -> 0
  | CNone, _ -> 1

let equal_cxtypes schema t1 t2 = 
  (compare_cxtypes schema t1 t2) = 0

let is_numeric_cxtype schema t = 
  match t.pcxtype_desc with
  | CAtomicRef r -> 
      derives_from schema r Namespace_symbols.decimalsym || 
      derives_from schema r Namespace_symbols.floatsym || 
      derives_from schema r Namespace_symbols.doublesym
  | _ -> false

let debug_print_types msg smallModel bigModel =
  if Debug.typing_debug()
  then
    begin
      Print_top.printf_cxtype ("\n"^msg^"\nSmall type:") smallModel;
      Print_top.printf_cxtype "\nBig type:" bigModel;
      flush stdout
    end

let is_syntactic_subtype_of schema cxtype1 cxtype2 =
  let (p,min,max) = Schema_util.factor_with_units cxtype1 in
  let (p2,min2,max2) = Schema_util.factor cxtype2 in
  let subtype_of = 
    (* Syntactic short-circuit rules for common types *)
    (* ??Question?? Do we need to check for Empty or None in p? *)
    (* BIG TYPE *)
    if (cxtype2 = cxtype_expanded_anytype) then
      not(Occurrence.equal min unbounded && Occurrence.equal max occurs_zero)
	(* empty-sequence() *)
    else if (cxtype2 = cxtype_empty) then 
      Occurrence.equal min occurs_zero &&
      Occurrence.equal max occurs_zero
	(* item() *)
    else if (cxtype2 = cxtype_item) then 
      (* (p, 1, 1) <: item() *)
      Occurrence.equal min occurs_one &&
      Occurrence.equal max occurs_one 
	(* item()? *)
    else if (cxtype2 = cxtype_item_optional) then 
      (* max <= 1, t2 = item()? *)
      Occurrence.le max occurs_one 
	(* item()* *)
    else if (cxtype2 = cxtype_item_star) then true
	(* item()+ *)
    else if (cxtype2 = cxtype_item_plus) then
      Occurrence.le occurs_one min 
	(* node() *)
    else if (cxtype2 = cxtype_node) then
      (* p does not contain atomic, min = 1, max = 1 *)
      (not(List.exists is_atomic_cxtype p)) &&
      Occurrence.equal min occurs_one &&
      Occurrence.equal max occurs_one 
	(* node()? *)
    else if (cxtype2 = cxtype_node_optional) then
      (not(List.exists is_atomic_cxtype p)) &&
      Occurrence.le max occurs_one 
	(* node()* *)
    else if (cxtype2 = cxtype_node_star) then
      (not(List.exists is_atomic_cxtype p)) 
	(* node()+ *)
    else if (cxtype2 = cxtype_node_plus) then
      (not(List.exists is_atomic_cxtype p)) &&
      Occurrence.le occurs_one min 
	(* xs:anyAtomicType *)
    else if (cxtype2 = cxtype_atomic || cxtype2 = cxtype_anyAtomic) then
      (not(List.exists is_node_cxtype p)) &&
      Occurrence.equal min occurs_one &&
      Occurrence.equal max occurs_one 
	(* xs:anyAtomicType? *)
    else if (cxtype2 = cxtype_atomic_optional || cxtype2 = cxtype_anyAtomic_optional) then
      (not(List.exists is_node_cxtype p)) &&
      Occurrence.le max occurs_one 
	(* xs:anyAtomicType* *)
    else if (cxtype2 = cxtype_atomic_star || cxtype2 = cxtype_anyAtomic_star) then
      (not(List.exists is_node_cxtype p))
	(* xs:anyAtomicType+ *)
    else if (cxtype2 = cxtype_atomic_plus || cxtype2 = cxtype_anyAtomic_plus) then
      (not(List.exists is_node_cxtype p)) &&
      Occurrence.le occurs_one min 
	(* none() : The factored None type has min=unbounded, max=0*)
    else if (cxtype2 = cxtype_none) then 
      Occurrence.equal min unbounded &&
      Occurrence.equal max occurs_zero
	(* document-node() *)
    else if (cxtype2 = cxtype_documentnode) then
      List.for_all is_document_cxtype p &&
      Occurrence.equal min occurs_one &&
      Occurrence.equal max occurs_one 
	(* document-node()? *)
    else if (cxtype2 = cxtype_documentnode_optional) then
      List.for_all is_document_cxtype p &&
      Occurrence.le max occurs_one 
	(* element() *)
    else if (cxtype2 = cxtype_element) then
      List.for_all is_element_cxtype p &&
      Occurrence.equal min occurs_one &&
      Occurrence.equal max occurs_one 
	(* element()? *)
    else if (cxtype2 = cxtype_element_optional) then
      List.for_all is_element_cxtype p &&
      Occurrence.le max occurs_one 
	(* attribute() *)
    else if (cxtype2 = cxtype_attribute) then
      List.for_all is_attribute_cxtype p &&
      Occurrence.equal min occurs_one &&
      Occurrence.equal max occurs_one 
	(* attribute()? *)
    else if (cxtype2 = cxtype_attribute_optional) then
      List.for_all is_attribute_cxtype p &&
      Occurrence.le max occurs_one 
    else if (cxtype2 = cxtype_numeric_optional || cxtype2 = cxtype_numeric_star) then 
      List.for_all (is_numeric_cxtype schema) p 
    else if (cxtype2 = cxtype_numeric || cxtype2 = cxtype_numeric_plus) then 
      List.for_all (is_numeric_cxtype schema) p &&
      Occurrence.le occurs_one min 
	(* SMALL TYPE *)
    else if (cxtype1 = cxtype_empty) then
      Occurrence.equal min2 occurs_zero
    else 
      begin
	let (p1,min,max) = Schema_util.factor cxtype1 in
	match (p1.pcxtype_desc, p2.pcxtype_desc) with 
	| (CAtomicRef r1, CAtomicRef r2) -> 
	    if (derives_from schema r1 r2) then
	      Occurrence.le min2 min && Occurrence.le max max2 
	    else false
	| _ -> 
	    if (equal_cxtypes schema cxtype1 cxtype2) then true
	    else
	      raise
		(Query (Unknown
			  ("No syntactic subtype relationship between "
			   ^(Print_top.bprintf_cxtype "" cxtype1)
			   ^" and "^(Print_top.bprintf_cxtype "" cxtype2))))
      end
  in
  debug_print_types ("In syntactic is_subtype_of ("^(string_of_bool subtype_of)^")\n") cxtype1 cxtype2;
  subtype_of

