(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_dtd_import.ml,v 1.4 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_dtd_import
   Description:
     This module imports a DTD as a set of XML Schema declarations.
*)

open Pxp_types
open Pxp_dtd

open Error

open Namespace_builtin
open Namespace_names

open Xquery_type_ast
open Xquery_type_ast_util

let import_error msg =
  raise (Query(Schema_Import msg))

let rec import_regexp_spec rs =
  match rs with
  | Optional rs' ->
      let td =
	TBound (import_regexp_spec rs',Occurrence.occurs_zero,Occurrence.occurs_one)
      in
      fmkxtype td Finfo.bogus
  | Repeated rs' ->
      let td =
	TBound (import_regexp_spec rs',Occurrence.occurs_zero,Occurrence.unbounded)
      in
      fmkxtype td Finfo.bogus
  | Repeated1 rs' ->
      let td =
	TBound (import_regexp_spec rs',Occurrence.occurs_one,Occurrence.unbounded)
      in
      fmkxtype td Finfo.bogus
  | Alt rslist ->
      import_choice rslist
  | Seq  rslist ->
      import_sequence rslist
  | Child name ->
      let qname = uqname_element_of_string name in
      fmkxtype (TElementRef qname) Finfo.bogus

and import_choice rslist =
  match rslist with
  | [] -> fmkxtype TNone Finfo.bogus
  | [rs] -> import_regexp_spec rs
  | rs1 :: rslist' ->
      let t1 = import_regexp_spec rs1 in
      fmkxtype (TChoice (t1,import_choice rslist')) Finfo.bogus

and import_sequence rslist =
  match rslist with
  | [] -> fmkxtype TEmpty Finfo.bogus
  | [rs] -> import_regexp_spec rs
  | rs1 :: rslist' ->
      let t1 = import_regexp_spec rs1 in
      fmkxtype (TSequence (t1,import_sequence rslist')) Finfo.bogus

let rec import_mixed_elem_spec_list mixed_spec_list =
  match mixed_spec_list with
  | [] -> fmkxtype TEmpty Finfo.bogus
  | [MChild name] -> 
      let qname = uqname_element_of_string name in
      fmkxtype (TElementRef qname) Finfo.bogus
  | (MChild name) :: mixed_spec_list' ->
      let qname = uqname_element_of_string name in
      let ter = fmkxtype (TElementRef qname) Finfo.bogus in
      fmkxtype (TChoice(ter,import_mixed_elem_spec_list mixed_spec_list')) Finfo.bogus
  | _ ->
      import_error "Mixed spec from PXP shouldn't have PCDATA during DTD import"

let import_mixed_spec_list mixed_spec_list =
  let mixed_spec_list = List.filter (fun x -> not(x = MPCDATA)) mixed_spec_list in
  fmkxtype (TBound (import_mixed_elem_spec_list mixed_spec_list,
		    Occurrence.occurs_zero,Occurrence.unbounded)) Finfo.bogus

let import_content_model axtype cm =
  let import_content_model_desc =
    match cm with
    | Unspecified ->
	import_error "Element without a declaration"
    | Empty ->
	TAnonymous (None,axtype,Xquery_common_ast.NonMixed,fmkxtype TEmpty Finfo.bogus)
    | Any ->
	TTypeRef uxs_anyType
    | Mixed mixed_spec_list ->
	TAnonymous (None,axtype,Xquery_common_ast.Mixed,import_mixed_spec_list mixed_spec_list)
    | Regexp rs ->
	TAnonymous (None,axtype,Xquery_common_ast.NonMixed,import_regexp_spec rs)
  in
  let ctsd = fmkctype_specifier import_content_model_desc Finfo.bogus in
  (TNonSubstitutesFor,Xquery_common_ast.NonNillable,TSpecComplex ctsd)

let make_attribute (aname,atype,arequired) =
  let aqname = uqname_element_of_string aname in
  let attribute_single =
    let attribute_single_desc =
      match atype with
      | A_cdata -> STypeRef uxs_untypedAtomic (* Is that right ? - Jerome *)
      | A_id -> STypeRef uxs_ID
      | A_idref -> STypeRef uxs_IDREF
      | A_idrefs -> STypeRef uxs_IDREFS
      | A_entity -> STypeRef uxs_ENTITY
      | A_entities -> STypeRef uxs_ENTITIES
      | A_nmtoken -> STypeRef uxs_NMTOKEN
      | A_nmtokens -> STypeRef uxs_NMTOKENS
      | A_notation string_list -> STypeRef uxs_NOTATION (* Should be fixed ? *)
      | A_enum string_list ->
	  import_error "Enumerated types not supported (in DTD)"
    in
    let attribute_single = fmkstype_specifier attribute_single_desc Finfo.bogus in
    fmkxtype (TAttributeLocal (aqname,attribute_single)) Finfo.bogus
  in
  if arequired
  then attribute_single
  else
    let td = TBound (attribute_single,Occurrence.occurs_zero,Occurrence.occurs_one) in
    fmkxtype td Finfo.bogus

let rec import_some_attributes attributes =
  match attributes with
  | [] -> fmkxtype TEmpty Finfo.bogus
  | [(aname,atype,arequired)] ->
      make_attribute (aname,atype,arequired)
  | (aname,atype,arequired) :: attributes' ->
      let td =
	TInterleave(make_attribute (aname,atype,arequired),
		    import_some_attributes attributes')
      in
      fmkxtype td Finfo.bogus

let import_attributes attributes =
  match attributes with
  | [] -> None
  | _ -> Some (import_some_attributes attributes)

let import_element (e : Pxp_dtd.dtd_element) =
  (* 1. The element name *)
  let name = e#name in
  let qname = uqname_element_of_string name in
  (* 2. The element attributes *)
  let names_of_required_attributes = e#names_of_required_attributes in
  let is_required x = List.exists (fun y -> x = y) names_of_required_attributes in
  let attributes = List.map (fun x -> (x,fst (e#attribute x),is_required x)) (e#attribute_names) in (* Ignoring the default value for the attribute if present *)
  let axtype = import_attributes attributes in
  (* 3. The element content model *)
  let cm = e#content_model in
  let xtype_desc = import_content_model axtype cm in
  let edd = TElementDecl (qname,xtype_desc) in
  fmkissd edd Finfo.bogus

let import_dtd d =
  let enames = d#element_names in
  let elems = List.map (fun x -> d#element x) enames in
  let decls = List.map import_element elems in
  let nsdecls = ("xs",xs_uri) :: [] in
  fmkschema [] nsdecls decls

