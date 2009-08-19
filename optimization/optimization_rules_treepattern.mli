(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_rules_treepattern.mli,v 1.4 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Optimization_rules_treepattern
   Description:
     This module implements the TreePattern optimization rules, as
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

(* ** Tree pattern related rewrites
 *  This module implements the rewrites prsented in
 *   Put a Tree Pattern in Your Tuple Algebra; 
 *   Philippe Michiels, George Mihaila, Jérôme Siméon
 *   ICDE 2007;
 * **)

val treejoin_rewrite:               optimization_rewrite_rule
val standalone_treejoin_rewrite:    optimization_rewrite_rule
val item_to_tuple_rewrite:          optimization_removal_rewrite_rule
val twig_inlining_rewrite:          optimization_rewrite_rule
val branching_twig_rewrite:         optimization_rewrite_rule

(* extra galax-specific rules *)
val merge_projects_rewrite:         optimization_rewrite_rule
val push_mapc_through_project:      optimization_rewrite_rule
val mapconcat_rewrite:              optimization_removal_rewrite_rule
val remove_ddo_rewrite:             optimization_rewrite_rule

val tree_pattern_rewrites:  
    ((bool ref -> logical_compile_context ->
      logical_algop_expr ->
      (logical_algop_expr * sub_expr_kind * int) option ->
      logical_algop_expr ->
      logical_algop_expr) * string) list
