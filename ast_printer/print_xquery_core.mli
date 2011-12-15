(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: print_xquery_core.mli,v 1.3 2007/02/01 22:08:45 simeon Exp $ *)

(* Module Print_xquery_core
   Description:
     This module implements pretty-printing for the Core XQuery AST.
*)

(***********************)
(* Core Sequence types *)
(***********************)

val print_ckind_test    : Format.formatter -> Xquery_core_ast.ckind_test -> unit
val print_csequencetype : Format.formatter -> Xquery_core_ast.csequencetype -> unit

(***************************)
(* Core XQuery expressions *)
(***************************)

val print_cexpr : Format.formatter -> Xquery_core_ast.acexpr -> (Format.formatter -> Xquery_core_ast_annotation.ast_annot -> unit) -> unit
val bprint_cexpr : string -> Xquery_core_ast.acexpr -> string

(*******************)
(* Core Statements *)
(*******************)

val print_cstatement : Format.formatter -> Xquery_core_ast.acstatement -> (Format.formatter -> Xquery_core_ast_annotation.ast_annot -> unit) -> unit

(***************)
(* Core Module *)
(***************)

val print_cprolog : Format.formatter -> Xquery_core_ast.acprolog -> (Format.formatter -> Xquery_core_ast_annotation.ast_annot -> unit) -> unit

val print_cmodule : Format.formatter -> Xquery_core_ast.acxmodule -> (Format.formatter -> Xquery_core_ast_annotation.ast_annot -> unit) -> unit

