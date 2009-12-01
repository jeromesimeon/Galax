(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: compile_top.mli,v 1.10 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Compile_top
   Description:
     This module compiles an XQuery toplevel statement or module into
     the ALGEBRA.
*)

open Xquery_core_ast
open Xquery_algebra_ast

open Ast_logical_algebra_types

(* Note:
     During this stage of compilation, we do not put actual running
     code into the structure.  This exception is generated if the
     structure is executed without replacement of the code.
   - Jerome & Chris *)

(***************)
(* Expressions *)
(***************)

val compile_statement : Compile_context.logical_compile_context -> acstatement -> logical_algop_expr
val compile_prolog    : Compile_context.logical_compile_context -> acprolog -> (Compile_context.logical_compile_context * logical_algop_prolog)
val compile_xmodule   : Compile_context.logical_compile_context -> acxmodule   -> (Compile_context.logical_compile_context * logical_algop_xmodule)
