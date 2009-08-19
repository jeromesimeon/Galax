(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_type_ast_util.ml,v 1.4 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Xquery_type_ast_util
   Description:
     This module implements some useful operations the XQuery type
     AST.
*)

open Finfo

open Namespace_names

open Xquery_common_ast
open Xquery_type_ast


(*********************)
(* Type AST creation *)
(*********************)

let mkxtype xtd =
  { pxtype_desc = xtd;
    pxtype_loc = parsing_locinfo () }
let fmkxtype xtd fi =
  { pxtype_desc = xtd;
    pxtype_loc = fi }

let mkctype_specifier tsd =
  { pctype_specifier_desc = tsd;
    pctype_specifier_loc = parsing_locinfo () }
let fmkctype_specifier tsd fi =
  { pctype_specifier_desc = tsd;
    pctype_specifier_loc = fi }

let mkstype_specifier tsd =
  { pstype_specifier_desc = tsd;
    pstype_specifier_loc = parsing_locinfo () }
let fmkstype_specifier tsd fi =
  { pstype_specifier_desc = tsd;
    pstype_specifier_loc = fi }

let mkissd issd =
  { pxtype_declaration_desc = issd;
    pxtype_declaration_loc  = parsing_locinfo () }
let fmkissd issd fi =
  { pxtype_declaration_desc = issd;
    pxtype_declaration_loc  = fi }


let fmkschema xschema_list namespace_declaration_list xtype_declaration_list =
  { xschema_imported_schemas       = xschema_list;
    xschema_namespace_declarations = namespace_declaration_list;
    xschema_type_declarations      = xtype_declaration_list }


