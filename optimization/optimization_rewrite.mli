(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_rewrite.mli,v 1.5 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Optimization_rewrite
   Description:
     This module contains algebraic optimizations.
*)

open Logical_algebra_types

val rewrite_expression :
    logical_compile_context -> logical_algop_expr -> logical_algop_expr 

