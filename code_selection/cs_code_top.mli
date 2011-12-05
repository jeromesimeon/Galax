(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cs_code_top.mli,v 1.10 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Cs_code_top
   Description:
     This module contains the actual caml code that is used for
     evaluation of the prolog and closure parts of the algebra.
*)

open Xquery_common_ast
open Xquery_algebra_ast

open Algebra_type

open Physical_value

open Execution_context
open Compile_context
open Code_selection_context

(* This module holds the caml code to implement the algebraic
   operations for the XQuery prolog. *)

val build_default_var_decl_code :
    code_selection_context -> asequencetype option -> cvname -> 
      (algebra_context -> item Cursor.cursor -> algebra_context)

val build_default_var_decl_external_code :
    code_selection_context -> asequencetype option -> cvname -> 
      (algebra_context -> unit -> algebra_context)

val build_default_key_decl_code :
    code_selection_context -> string ->
      (algop_expr -> eval_fun -> algebra_context -> item Cursor.cursor -> algebra_context)

val build_default_name_index_decl_code :
    code_selection_context ->
      (algebra_context -> unit -> algebra_context)

val build_default_twig_index_decl_code :
    code_selection_context ->
      (algebra_context -> unit -> algebra_context)

val extend_var_context :
    code_selection_context * Execution_context.algebra_context ->
      Xquery_common_ast.cvname * Streaming_types.xml_stream ->
	code_selection_context * Execution_context.algebra_context

val extend_tuple_context :
    code_selection_context ->
      Xquery_common_ast.crname * Streaming_types.xml_stream ->
	Xquery_common_ast.crname * Xquery_physical_type_ast.physical_xml_type

