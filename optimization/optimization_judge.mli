(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_judge.mli,v 1.12 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Optimization_judge
   Description:
     This module contains judgments used during algebraic
     optimization.
 *)

open Compile_context

open Xquery_common_ast

open Ast_logical_algebra_types


(****************)
(* Independence *)
(****************)

(* NOTE: Those judgments are used to decide whether turning a
   MapConcat into a Product is possible. *)

val map_tuple_independent     : logical_algop_expr -> logical_algop_expr -> bool
val natural_tuple_independent : logical_algop_expr -> logical_algop_expr -> bool


(****************)
(* Side effects *)
(****************)

(* NOTE: Those judgments are used to decide whether some rewritings
   are safe wrt. to side-effects. *)

val has_non_trivial_snap    : logical_compile_context -> logical_algop_expr  -> bool
val has_trivial_snap        : logical_compile_context -> logical_algop_expr  -> bool
val has_side_effect         : logical_compile_context -> logical_algop_expr  -> bool
val side_effect_free        : logical_compile_context -> logical_algop_expr  -> bool
val contains_update         : logical_algop_expr -> bool
val subexpr_has_side_effect : logical_compile_context -> logical_algop_sub_exprs -> bool
val has_dependent_side_effect : logical_compile_context -> logical_algop_expr  -> bool

(* conservative test that two expressions commute, modulo the order of the values in the result *)
val commute_logical         : logical_compile_context -> logical_algop_expr  -> logical_algop_expr  -> bool 
val commute_logical_with_array     : logical_compile_context -> logical_algop_expr  -> logical_algop_expr array  -> bool 


(**********)
(* Typing *)
(**********)

(* NOTE: Those judgments are used as a poor-man's form of static
   typing for the algebra, to decide simple cardinality properties. *)

val is_singleton_tuple    : logical_compile_context -> logical_algop_expr  -> bool


(******************)
(* Document order *)
(******************)

(* NOTE: Those judgments are used as a poor-man's form of DDO
   analysis, to decide whether document order operations can be
   removed. *)

val ord_dup_matters :logical_algop_expr  -> logical_algop_expr -> bool


(********************)
(* HACK Name checks *)
(********************)

(* NOTE: Yeck. - Jerome. May 31, 2006 *)

val is_seq_field : crname -> bool

