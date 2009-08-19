(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: print_top.mli,v 1.6 2007/08/01 17:06:17 mff Exp $ *)

(* Module Print_top
   Description:
     This module provide the top-level pretty-printing functions for
     the XQuery ASTs.
*)

(****************************************)
(* Print functions for the XQuery AST's *)
(****************************************)

(* Type *)

val printf_sequencetype  : string -> Xquery_ast.sequencetype -> unit
val bprintf_sequencetype : string -> Xquery_ast.sequencetype -> string

(* Full XQuery expression *)

val fprintf_expr : Format.formatter -> string -> Xquery_ast.expr -> unit
val printf_expr  : string -> Xquery_ast.expr -> unit
val bprintf_expr : string -> Xquery_ast.expr -> string

(* Statement *)

val fprintf_statement : Format.formatter -> string -> Xquery_ast.statement -> unit
val printf_statement  : string -> Xquery_ast.statement -> unit
val bprintf_statement : string -> Xquery_ast.statement -> string

(* Module *)

val fprintf_interface : Format.formatter -> string -> Xquery_ast.interface -> unit
val fprintf_library_module : Format.formatter -> string -> Xquery_ast.library_module -> unit
val fprintf_main_module : Format.formatter -> string -> Xquery_ast.main_module -> unit
val fprintf_module 	: Format.formatter -> string -> Xquery_ast.xmodule -> unit
val fprintf_prolog 	: Format.formatter -> string -> Xquery_ast.prolog -> unit
val bprintf_interface : string -> Xquery_ast.interface -> string
val bprintf_library_module : string -> Xquery_ast.library_module -> string
val bprintf_main_module : string -> Xquery_ast.main_module -> string
val bprintf_module 	: string -> Xquery_ast.xmodule -> string
val bprintf_prolog 	: string -> Xquery_ast.prolog -> string


(*********************************************)
(* Print functions for the Core XQuery AST's *)
(*********************************************)

(* Node kind *)

val printf_cnode_kind  : string -> Xquery_core_ast.ckind_test -> unit
val bprintf_cnode_kind : string -> Xquery_core_ast.ckind_test -> string

(* core types *)

val printf_cxtype  : string -> Xquery_type_core_ast.cxtype -> unit
val bprintf_cxtype : string -> Xquery_type_core_ast.cxtype -> string

val printf_cxschema  : string -> Xquery_type_core_ast.cxschema -> unit
val bprintf_cxschema : string -> Xquery_type_core_ast.cxschema -> string

(* core type declarations *)

val printf_celem_decl  : string -> Xquery_type_core_ast.celem_declaration -> unit
val bprintf_celem_decl : string -> Xquery_type_core_ast.celem_declaration -> string

val printf_cattr_decl  : string -> Xquery_type_core_ast.cattr_declaration -> unit
val bprintf_cattr_decl : string -> Xquery_type_core_ast.cattr_declaration -> string

val printf_ctype_decl  : string -> Xquery_type_core_ast.ctype_declaration -> unit
val bprintf_ctype_decl : string -> Xquery_type_core_ast.ctype_declaration -> string

(* Sequence type *)

val printf_csequencetype  : string -> Xquery_core_ast.csequencetype -> unit
val bprintf_csequencetype : string -> Xquery_core_ast.csequencetype -> string

val printf_asequencetype  : string -> Xquery_algebra_ast.asequencetype -> unit
val bprintf_asequencetype : string -> Xquery_algebra_ast.asequencetype -> string

(* Annotated core expression *)
val fprintf_acexpr : Format.formatter -> string -> Xquery_core_ast.acexpr -> unit
val printf_acexpr  : string -> Xquery_core_ast.acexpr -> unit
val bprintf_acexpr : string -> Xquery_core_ast.acexpr -> string

val print_acexpr : Format.formatter -> Xquery_core_ast.acexpr -> unit


(* Annotated core XQuery statement *)

val fprintf_acstatement : Format.formatter -> string -> Xquery_core_ast.acstatement -> unit
val printf_acstatement  : string -> Xquery_core_ast.acstatement -> unit
val bprintf_acstatement : string -> Xquery_core_ast.acstatement -> string

(* Annotation core prolog list *)

val fprintf_acprolog : Format.formatter -> string -> Xquery_core_ast.acprolog -> unit
val printf_acprolog  : string -> Xquery_core_ast.acprolog -> unit
val bprintf_acprolog : string -> Xquery_core_ast.acprolog -> string

(* Annotated core query module list *)

val fprintf_acmodule : Format.formatter -> string -> Xquery_core_ast.acxmodule -> unit
val printf_acmodule  : string -> Xquery_core_ast.acxmodule -> unit
val bprintf_acmodule : string -> Xquery_core_ast.acxmodule -> string

(*****************************)
(* Top-level print functions *)
(*****************************)

val fprintf_result_type : Format.formatter -> string -> Xquery_type_core_ast.cxtype -> unit
val printf_result_type  : string -> Xquery_type_core_ast.cxtype -> unit
val bprintf_result_type : string -> Xquery_type_core_ast.cxtype -> string


val print_escaped_output : Format.formatter -> string -> string -> string -> unit

