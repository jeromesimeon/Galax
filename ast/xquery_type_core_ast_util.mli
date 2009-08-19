(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_type_core_ast_util.mli,v 1.9 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Xquery_type_core_ast_util
   Description:
     This module implements some useful operations the XQuery core
     type AST.
*)

open Xquery_common_ast
open Xquery_type_core_ast
open Xquery_type_core_ast_annotation


(***************************************)
(* Core type AST creation and accessors *)
(***************************************)

val fmkcxtype : cxtype_desc -> Finfo.finfo -> cxtype
val fmkcxtype_builtin : cxtype_desc -> cxtype

val fmkctype_decl : Namespace_symbols.rtype_symbol -> cxtype_derivation -> cxtype option -> ctype_declaration 

(*******************)
(* Schema creation *)
(*******************)

val fmkcxschema      : celem_declaration list -> cattr_declaration list -> ctype_declaration list -> cxschema
val merge_cxschema   : cxschema -> cxschema -> cxschema
val merge_cxschemas  : cxschema list -> cxschema

val make_mappings    : Xquery_type_core_ast_annotation.letter_mappings -> cxtype -> unit

val is_empty_cxtype     : cxtype -> bool
val is_none_cxtype      : cxtype -> bool
val is_atomic_cxtype    : cxtype -> bool
val is_node_cxtype      : cxtype -> bool
val is_document_cxtype  : cxtype -> bool
val is_element_cxtype   : cxtype -> bool
val is_attribute_cxtype : cxtype -> bool

