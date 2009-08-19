(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: compile_util.ml,v 1.17 2007/05/21 20:22:39 mff Exp $ *)

(* Module: Compile_util
   Description:
     This module contains some utilities used during the compilation
     phase.
*)

open Xquery_core_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util

open Compile_context


(*******************)
(* The Input tuple *)
(*******************)

let compile_inputtuple eh fi =
    logical_aalgop_mkop AOEInputTuple NoSub NoSub None eh fi 

(******************)
(* Sequence types *)
(******************)

let compile_element_test element_test =
  match element_test with
  | CSchemaElementTest cename ->
      let relem_sym = Namespace_symbols.relem_symbol cename in
      ASchemaElementTest relem_sym
  | CElementTest None ->
      AElementTest None
  | CElementTest (Some (cename,None)) ->
      let relem_sym = Namespace_symbols.relem_symbol cename in
      AElementTest (Some (relem_sym,None))
  | CElementTest (Some (cename,Some ctname)) ->
      let relem_sym = Namespace_symbols.relem_symbol cename in
      let rtype_sym = Namespace_symbols.rtype_symbol ctname in
      AElementTest (Some (relem_sym,Some rtype_sym))

let compile_attribute_test attribute_test =
  match attribute_test with
  | CSchemaAttributeTest caname ->
      let rattr_sym = Namespace_symbols.rattr_symbol caname in
      ASchemaAttributeTest rattr_sym
  | CAttributeTest None ->
      AAttributeTest None
  | CAttributeTest (Some (caname,None)) ->
      let rattr_sym = Namespace_symbols.rattr_symbol caname in
      AAttributeTest (Some (rattr_sym,None))
  | CAttributeTest (Some (caname,Some ctname)) ->
      let rattr_sym = Namespace_symbols.rattr_symbol caname in
      let rtype_sym = Namespace_symbols.rtype_symbol ctname in
      AAttributeTest (Some (rattr_sym,Some rtype_sym))

let compile_kind_test ckind_test =
  match ckind_test with
  | CDocumentKind None ->
      ADocumentKind None
  | CDocumentKind (Some et) ->
      ADocumentKind (Some (compile_element_test et))
  | CElementKind et ->
      AElementKind (compile_element_test et)
  | CAttributeKind at ->
      AAttributeKind (compile_attribute_test at)
  | CPIKind pik ->
      APIKind pik
  | CCommentKind ->
      ACommentKind
  | CTextKind ->
      ATextKind
  | CAnyKind ->
      AAnyKind

let compile_citemtype comp_ctxt citemtype =
  match citemtype with
  | CITKindTest ckind_test ->
      AITKindTest (compile_kind_test ckind_test)
  | CITTypeRef ctname ->
      let rtype_sym = Namespace_symbols.rtype_symbol ctname in
      AITTypeRef rtype_sym
  | CITItem ->
      AITItem
  | CITNumeric ->
      AITNumeric
  | CITAnyString ->
      AITAnyString
  | CITEmpty ->
      AITEmpty
  | CITAtomic ctname ->
      let rtype_sym = Namespace_symbols.rtype_symbol ctname in
      AITAtomic rtype_sym

let compile_ctype comp_ctxt (cdt, cty) =
  let (citemtype,occ) = cdt.pcsequencetype_desc in
  let aitemtype = compile_citemtype comp_ctxt citemtype in
  fmkasequencetype (aitemtype,occ) cdt.pcsequencetype_loc

let compile_opt_ctype comp_ctxt ocdt =
  match ocdt with
  | None -> None
  | Some cdt -> Some (compile_ctype comp_ctxt cdt)

let compile_cnode_test cnode_test =
  match cnode_test with
  | CPNameTest rqname ->
      let relem_sym = Namespace_symbols.relem_symbol rqname in
      (APNameTest relem_sym)
  | CPNodeKindTest (ckind_test, ckind_type) ->
      APNodeKindTest (compile_kind_test ckind_test)


let compile_fun_sig comp_ctxt (fn,(sl,s), opt_fun_kind, upd) =
  (fn,(List.map (compile_ctype comp_ctxt) sl,
       compile_ctype comp_ctxt s), upd)

let compile_overloaded_table_sigs comp_ctxt sigs =
  List.map (compile_fun_sig comp_ctxt) sigs

let compile_cfunction_sig comp_ctxt (intypes, outtype) = 
  (List.map (compile_ctype comp_ctxt) intypes, compile_ctype comp_ctxt outtype)


