(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_rules_treejoin.mli,v 1.3 2007/02/12 21:15:33 simeon Exp $ *)

(* Module: Optimization_rules_treejoin
   Description:
     This module implements the treejoin optimization rules, as
     described in.

       Put a Tree Pattern in Your Algebra. Philippe Michiels, George
       Mihaila, and Jérôme Siméon

       ICDE 2007
       http://www.adrem.ua.ac.be/bibrem/pubs/michiels07treepattern.pdf
*)

open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_algebra_ast_annotation_util

open Processing_context
open Compile_context

open Optimization_util
open Optimization_walker
open Logical_algebra_types

(*************************************************************************)
(* Introducing Streamable TreeJoins                                      *)
(* To obtain optimal streaming plans, we need plain chained TreeJoins,   *)
(* without Maps in between.                                              *) 
(*************************************************************************)

val pull_up_ttp_as_treejoin    : optimization_rewrite_rule
val push_down_ttp_as_treejoin  : optimization_rewrite_rule
val ttp_to_treejoin_generic    : optimization_rewrite_rule
val map_to_from_rewrite        : optimization_rewrite_rule
val pull_up_conservative       : optimization_rewrite_rule


val treejoin_rewrites :
    ((bool ref -> logical_compile_context ->
      logical_algop_expr ->
	(logical_algop_expr * sub_expr_kind * int) option ->
	  logical_algop_expr ->
	    logical_algop_expr) * string) list

