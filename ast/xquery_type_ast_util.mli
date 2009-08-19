(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_type_ast_util.mli,v 1.3 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Xquery_type_ast_util
   Description:
     This module implements some useful operations the XQuery type
     AST.
*)

open Xquery_common_ast
open Xquery_type_ast


(*********************)
(* Type AST creation *)
(*********************)

val mkxtype : xtype_desc -> xtype
val fmkxtype : xtype_desc -> Finfo.finfo -> xtype

val mkstype_specifier : stype_specifier_desc -> stype_specifier
val fmkstype_specifier : stype_specifier_desc -> Finfo.finfo -> stype_specifier

val mkctype_specifier : ctype_specifier_desc -> ctype_specifier
val fmkctype_specifier : ctype_specifier_desc -> Finfo.finfo -> ctype_specifier

val mkissd : xtype_declaration_desc -> xtype_declaration
val fmkissd : xtype_declaration_desc -> Finfo.finfo -> xtype_declaration

val fmkschema : xschema list -> namespace_declaration list -> xtype_declaration list -> xschema

