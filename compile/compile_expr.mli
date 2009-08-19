(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: compile_expr.mli,v 1.9 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Compile_expr
   Description:
     The main interface to compilation.
     Takes in typed core expressions and returns a compiled version of
     the algebra *)

open Xquery_core_ast
open Logical_algebra_types

val compile_cexpr : logical_compile_context -> acexpr -> logical_algop_expr 

