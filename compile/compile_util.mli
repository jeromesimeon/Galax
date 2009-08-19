(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: compile_util.mli,v 1.12 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Compile_util
   Description:
     This module contains some utilities used during the compilation
     phase.
*)

open Xquery_core_ast
open Xquery_algebra_ast

open Logical_algebra_types

open Compile_context

val compile_inputtuple : 
  Xquery_ast.expr_handle -> Finfo.finfo -> logical_algop_expr

val compile_ctype :
  logical_compile_context -> (Xquery_core_ast.csequencetype * Xquery_type_core_ast.cxtype) -> Xquery_algebra_ast.asequencetype
  
val compile_opt_ctype :
  logical_compile_context -> (Xquery_core_ast.csequencetype * Xquery_type_core_ast.cxtype) option -> Xquery_algebra_ast.asequencetype option

val compile_cnode_test :
  Xquery_core_ast.cnode_test -> Xquery_algebra_ast.anode_test

val compile_overloaded_table_sigs :
  logical_compile_context -> Xquery_core_ast.overloaded_signature_table -> Xquery_algebra_ast.aoverloaded_signature_table

val compile_cfunction_sig :
  logical_compile_context -> Xquery_core_ast.cfunction_signature -> Xquery_algebra_ast.afunction_signature

