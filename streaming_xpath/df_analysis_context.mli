(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: df_analysis_context.mli,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Df_analysis_context
   Description:
     This module provides a functional context for the XQuery core
     data flow analysis algorithm by storing a mapping between
     variable names and graphs.
*)


open Xquery_common_ast

open Error

open Df_struct


type ('a, 'b) df_analysis_context


val build_df_analysis_context :
  unit ->  ('a, 'b) df_analysis_context

val add_var_dfgraph :
   ('a, 'b) df_analysis_context -> cvname ->  ('a, 'b) dfgraph ->  ('a, 'b) df_analysis_context

val get_var_dfgraph :
   ('a, 'b) df_analysis_context -> cvname ->  ('a, 'b) dfgraph
