(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: print_type_core.mli,v 1.4 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Print_core_type
   Description:
     This module implements pretty-printing for the core variant of
     the XQuery type system.
*)

val print_celem_decl : Format.formatter -> Xquery_type_core_ast.celem_declaration -> unit
val print_cattr_decl : Format.formatter -> Xquery_type_core_ast.cattr_declaration -> unit
val print_ctype_decl : Format.formatter -> Xquery_type_core_ast.ctype_declaration -> unit

val print_cxtype     : Format.formatter -> Xquery_type_core_ast.cxtype -> unit
val bprint_cxtype    : Xquery_type_core_ast.cxtype -> string

val print_cxschema   : Format.formatter -> Xquery_type_core_ast.cxschema -> unit
val print_letter_mappings  : Format.formatter -> Xquery_type_core_ast_annotation.letter_mappings -> unit

