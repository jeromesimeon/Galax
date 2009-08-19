(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parse_xqueryx.mli,v 1.3 2007/08/09 20:21:22 simeon Exp $ *)

(* Module: Norm_xqueryx
   Description:
     This module implements normalization for XQueryX expressions
     using the trivial embedding.
*)

open Error

open Xquery_ast
open Xquery_core_ast

open Norm_context

val normalize_xqx_xquery_expr : expr list -> expr list -> Finfo.finfo -> expr
val normalize_xqx_xquery_main_module : expr list -> expr list -> Finfo.finfo -> main_module
val normalize_xqx_xquery_library_module : expr list -> expr list -> Finfo.finfo -> library_module
val normalize_xqx_xquery_interface_module : expr list -> expr list -> Finfo.finfo -> interface

val parse_xqx_xquery_main_module_from_io : Parse_context.parse_context -> Galax_io.input_spec -> Parse_context.parse_context * main_module

val parse_xqx_xquery_library_module_from_io : Parse_context.parse_context -> Galax_io.input_spec -> Parse_context.parse_context * library_module

val parse_xqx_xquery_interface_module_from_io : Parse_context.parse_context -> Galax_io.input_spec -> Parse_context.parse_context * interface

