(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_type_core_ast_util.ml,v 1.11 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Xquery_type_core_ast_annotation
   Description:
     This module implements some annotations on the type AST.
*)

open Error

open Namespace_symbols_util

open Xquery_common_ast
open Xquery_type_core_ast
open Xquery_type_core_ast_annotation

(**************************)
(* Core type AST creation *)
(**************************)

let fmkcxtype cxtype_desc finfo =
  { pcxtype_desc = cxtype_desc;
    pcxtype_simplified = false;
    pcxtype_loc  = finfo }

let fmkcxtype_builtin cxtype_desc =
  { pcxtype_desc = cxtype_desc;
    pcxtype_simplified = true;
    pcxtype_loc  = Finfo.bogus }

let fmkctype_decl sym deriv cxtype_opt = 
  { ctypedecl_name   = sym;
    ctypedecl_deriv  = deriv;
    ctypedecl_cxtype = cxtype_opt }


(**************************)
(* Letter Mappings        *)
(**************************)
let check_caname_consistency caname (typeopt1) (typeopt2) =
  match (typeopt1,typeopt2) with
  | (None,None) -> ()
  | (Some ctname1, Some ctname2) ->
      if not(Namespace_symbols.symbol_equal ctname1 ctname2)
      then
	raise (Query (Schema_Internal ("Subtyping: [Attribute Declaration Constraint] not satisfied for "^(Namespace_symbols.symbol_prefix_string ctname1)^" and "^(Namespace_symbols.symbol_prefix_string ctname2))))
      else
	()
  | (Some ctname, _)
  | (_, Some ctname) ->
      raise (Query (Schema_Internal ("Subtyping: [Attribute Declaration Constraint] not satisfied for "^(Namespace_symbols.symbol_prefix_string ctname))))

let make_ca_letter_unit mapping (caname,typeopt) =
  if SQNameHashtbl.mem (snd mapping) caname
  then
    begin
      (* Why are we checking for sub-type consistency here? *)
      let (typeopt2,letternum) = SQNameHashtbl.find (snd mapping) caname in
      check_caname_consistency caname typeopt typeopt2
    end
  else
    begin
      incr (fst mapping);
      let letternum = !(fst mapping) in
      if Debug.typing_debug()
      then Debug.print_typing_debug ("Map "^(Namespace_symbols.symbol_prefix_string caname)^"->"^(string_of_int letternum)^""); 
      SQNameHashtbl.add (snd mapping) caname (typeopt,letternum)
    end

let check_cename_consistency cename (typeopt1,nil1) (typeopt2,nil2) =
  if not(nil1 = nil2)
  then
    raise (Query (Schema_Internal "Subtyping: [Element Declaration Constraint] not satisfied for nil annotations"))
  else
    match (typeopt1,typeopt2) with
    | (None,None) -> ()
    | (Some ctname1, Some ctname2) ->
	if not(Namespace_symbols.symbol_equal ctname1 ctname2)
	then
	  raise (Query (Schema_Internal ("Subtyping: [Element Declaration Constraint] not satisfied for "^(Namespace_symbols.symbol_prefix_string ctname1)^" and "^(Namespace_symbols.symbol_prefix_string ctname2))))
	else
	  ()
    | (Some ctname, _)
    | (_, Some ctname) ->
	raise (Query (Schema_Internal ("Subtyping: [Element Declaration Constraint] not satisfied for "^(Namespace_symbols.symbol_prefix_string ctname))))

let make_ce_letter_unit mapping (cename,typeopt,nil) =
  if SQNameHashtbl.mem (snd mapping) cename
  then
    begin
      let (typeopt2,nil2,letternum) = SQNameHashtbl.find (snd mapping) cename in
      check_cename_consistency cename (typeopt,nil) (typeopt2,nil2)
    end
  else
    begin
      incr (fst mapping);
      let letternum = !(fst mapping) in
      if Debug.typing_debug()
      then Debug.print_typing_debug ("Map "^(Namespace_symbols.symbol_prefix_string cename)^"->"^(string_of_int letternum)^""); 
      SQNameHashtbl.add (snd mapping) cename (typeopt,nil,letternum)
    end

let make_at_letter_unit mapping ctname =
  if SQNameHashtbl.mem (snd mapping) ctname
  then ()
  else
    begin
      incr (fst mapping);
      let letternum = !(fst mapping) in
      if Debug.typing_debug()
      then Debug.print_typing_debug ("Map "^(Namespace_symbols.symbol_prefix_string ctname)^"->"^(string_of_int letternum)^"\n"); 
      SQNameHashtbl.add (snd mapping) ctname letternum
    end

let make_pi_letter_unit mapping piname =
  if Hashtbl.mem (snd mapping) piname
  then ()
  else
    begin
      incr (fst mapping);
      let letternum = !(fst mapping) in
      if Debug.typing_debug()
      then
	let piname2 =
	  match piname with
	  | None -> "pi()"
	  | Some pi -> "pi("^pi^")"
	in Debug.print_typing_debug ("Map "^piname2^"->"^(string_of_int letternum)^"\n"); 
      Hashtbl.add (snd mapping) piname letternum
    end

let rec make_mappings mapping cxtype =
  match cxtype.pcxtype_desc with
  | CElementRef cename ->
      make_ce_letter_unit (elem_letter_map mapping) (cename,None,NonNillable)
  | CElementLocal (cename,nil,ctname) ->
      make_ce_letter_unit (elem_letter_map mapping) (cename,Some ctname,nil)
  | CAttributeRef caname ->
      make_ca_letter_unit (attr_letter_map mapping) (caname,None)
  | CAttributeLocal (caname,ctname) ->
      make_ca_letter_unit (attr_letter_map mapping) (caname,Some ctname)
  | CAtomicRef ctname ->
      make_at_letter_unit (type_letter_map mapping) ctname
  | CDocument m1 ->
      make_mappings mapping m1
  | CPI piname ->
      make_pi_letter_unit (pi_letter_map mapping) piname
  | CText
  | CComment
  | CEmpty
  | CNone -> ()
  | CBound (m1,_,_) ->
      make_mappings mapping m1
  | CSequence (m1,m2) ->
      make_mappings mapping m1;
      make_mappings mapping m2
  | CChoice (m1,m2) ->
      make_mappings mapping m1;
      make_mappings mapping m2
  | CInterleave (m1,m2) ->
      make_mappings mapping m1;
      make_mappings mapping m2

(* This function extracts cxtypes from a type declaration for the
   purpose of building letter mappings *)
let cxtypes_of_ctype_derivation rsym ctype_deriv =
  let (rsym_deriv_from, attr_content, children_content) = ctype_deriv in
  let child_types = 
    match children_content with
    | CComplexTypeRestriction (mxd, ty) -> [ty]
    | CComplexTypeExtension  (mxd, ty) -> [ty]
    | CAtomicTypeRestriction -> [fmkcxtype (CAtomicRef rsym) Finfo.bogus]
    | CSimpleTypeList sym -> [fmkcxtype (CAtomicRef sym) Finfo.bogus]
    | CSimpleTypeUnion sym_list -> 
	List.map (fun sym -> fmkcxtype (CAtomicRef sym) Finfo.bogus) sym_list
  in
  let attr_types = 
    match attr_content with 
    | None -> []
    | Some t -> [t]
  in
  attr_types @ child_types 


 (*******************)
 (* Schema creation *)
 (*******************)

let rqname_of_cxtype_decl decl = fst decl

let add_one_decl cxs (rqname,decl) =
  if SQNameHashtbl.mem cxs rqname
  then
    let (_,decl') = SQNameHashtbl.find cxs rqname in
     (* Using O'Caml structural equivalence to check whether two decls
	are the same. *)
    if (decl = decl') then ()
    else
      let declstr  = Namespace_symbols.symbol_prefix_string rqname in
      raise(Query(Schema ("Distinct declarations for " ^ declstr ^ "' conflict.")))
  else
    SQNameHashtbl.add cxs rqname (rqname,decl)

let add_one_type_decl cxs decl =
  if SQNameHashtbl.mem cxs decl.ctypedecl_name
  then
    let decl' = SQNameHashtbl.find cxs decl.ctypedecl_name in
     (* Using O'Caml structural equivalence to check whether two decls
	are the same. *)
    if (decl = decl') then ()
    else
      let declstr  = Namespace_symbols.symbol_prefix_string decl.ctypedecl_name in
      raise(Query(Schema ("Distinct declarations for " ^ declstr ^ "' conflict.")))
  else
    SQNameHashtbl.add cxs decl.ctypedecl_name decl

let dummy_cxschema =
  { cxschema_letter_mappings        = create_letter_mappings();
    cxschema_element_declarations   = SQNameHashtbl.create 1;
    cxschema_attribute_declarations = SQNameHashtbl.create 1;
    cxschema_type_declarations      = SQNameHashtbl.create 1 }

let fmkcxschema celem_declaration_list cattr_declaration_list ctype_declaration_list =
  try
    begin
      let el = SQNameHashtbl.create 167 in
      let at = SQNameHashtbl.create 167 in
      let ty = SQNameHashtbl.create 167 in
      let mappings = create_letter_mappings() in
      List.iter (fun decl -> add_one_decl el decl) celem_declaration_list;
      List.iter (fun decl -> add_one_decl at decl) cattr_declaration_list;
      List.iter (fun decl -> add_one_type_decl ty decl) ctype_declaration_list;
      let s = 
	{ cxschema_letter_mappings        = mappings;
	  cxschema_element_declarations   = el;
	  cxschema_attribute_declarations = at;
	  cxschema_type_declarations      = ty }
      in s
    end
  with
  | e ->
      begin
	eprintf_error "  " e;
	Format.fprintf (!Conf.glx_err_formatter) "@.";
	dummy_cxschema
      end

let merge_cxschema cx1 cx2 =
  let el = SQNameHashtbl.copy cx1.cxschema_element_declarations in
  let at = SQNameHashtbl.copy cx1.cxschema_attribute_declarations in
  let ty = SQNameHashtbl.copy cx1.cxschema_type_declarations in
  let mappings = copy_letter_mappings cx1.cxschema_letter_mappings in 
  begin
    SQNameHashtbl.iter (fun rqname decl -> add_one_decl el decl) cx2.cxschema_element_declarations; 
    SQNameHashtbl.iter (fun rqname decl -> add_one_decl at decl) cx2.cxschema_attribute_declarations; 
    SQNameHashtbl.iter (fun rqname decl -> add_one_type_decl ty decl) cx2.cxschema_type_declarations;
    let s = 
    { cxschema_letter_mappings = mappings; (* build_name_letter_mappings mappings ty;  *)
      cxschema_element_declarations = el;
      cxschema_attribute_declarations = at;
      cxschema_type_declarations = ty }
    in s
  end

let merge_cxschemas cxl =
  let el = SQNameHashtbl.create 167 in
  let at = SQNameHashtbl.create 167 in
  let ty = SQNameHashtbl.create 167 in
  let mappings = create_letter_mappings() in
  begin
    List.iter 
      (fun cx2 ->
	SQNameHashtbl.iter (fun rqname decl -> add_one_decl el decl) cx2.cxschema_element_declarations;
	SQNameHashtbl.iter (fun rqname decl -> add_one_decl at decl) cx2.cxschema_attribute_declarations;
	SQNameHashtbl.iter (fun rqname decl -> add_one_type_decl ty decl) cx2.cxschema_type_declarations) 
      cxl;
    let s = 
    { cxschema_letter_mappings = mappings; (* build_name_letter_mappings mappings ty; *)
      cxschema_element_declarations = el;
      cxschema_attribute_declarations = at;
      cxschema_type_declarations = ty }
    in s
  end

let is_atomic_cxtype t =
    match t.pcxtype_desc with 
  | CAtomicRef      _ -> true
  | _ -> false

let is_node_cxtype t =
    match t.pcxtype_desc with 
  | CElementRef _
  | CAttributeRef _
  | CElementLocal _
  | CAttributeLocal _
  | CDocument       _
  | CText
  | CPI _
  | CComment -> true
  | _ -> false

let is_document_cxtype t =
    match t.pcxtype_desc with 
  | CDocument _ -> true
  | _ -> false

let is_element_cxtype t =
    match t.pcxtype_desc with 
  | CElementRef _ -> true
  | CElementLocal _ -> true
  | _ -> false

let is_attribute_cxtype t =
    match t.pcxtype_desc with 
  | CAttributeRef _ -> true
  | CAttributeLocal _ -> true
  | _ -> false

let is_empty_cxtype cxtype =
  match cxtype.pcxtype_desc with
  | CEmpty -> true
  | _ -> false

let is_none_cxtype m =
  match m.pcxtype_desc with
  | CNone -> true
  | _ -> false

