(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parse_top.mli,v 1.13 2007/08/09 20:21:22 simeon Exp $ *)

(* Module: Parse_top
   Description:
     This module implements top-level parsing functions.
 *)


(*******************************)
(* Top-level parsing functions *)
(*******************************)

(* Statement *)

val parse_statement_from_io      : Parse_context.parse_context -> Galax_io.input_spec -> Xquery_ast.statement
val parse_statement_from_string  : string -> Xquery_ast.statement

(* Prolog *)

val parse_prolog_from_io         : Parse_context.parse_context -> Galax_io.input_spec -> Parse_context.parse_context * Xquery_ast.prolog

(* Library module *)

val parse_library_module_from_io : Parse_context.parse_context -> Galax_io.input_spec -> Parse_context.parse_context * Xquery_ast.library_module
val parse_library_module_from_string : string -> Parse_context.parse_context * Xquery_ast.library_module

(* Main module *)

val parse_main_module_from_io	 : Parse_context.parse_context -> Galax_io.input_spec -> Parse_context.parse_context * Xquery_ast.main_module
val parse_main_module_from_string  : string -> Parse_context.parse_context * Xquery_ast.main_module

(* Module interface *)

val parse_interface_from_io	 : Parse_context.parse_context -> Galax_io.input_spec -> Parse_context.parse_context * Xquery_ast.interface
val parse_interface_from_string	 : string -> Parse_context.parse_context * Xquery_ast.interface

(* Content models *)

val parse_type_from_io : Parse_context.parse_context -> Galax_io.input_spec -> Xquery_type_ast.xtype option * Xquery_type_ast.xtype

(* Debug functions *)

val ast_from_file      : string -> Xquery_ast.main_module
val ast_from_string    : string -> Xquery_ast.main_module
val tokens_from_file   : string -> (Parse_xquery.token * Lexing_util.lex_state list * bool) list
val tokens_from_string : string -> (Parse_xquery.token * Lexing_util.lex_state list * bool) list

