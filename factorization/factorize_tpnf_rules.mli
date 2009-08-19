(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_tpnf_rules.mli,v 1.13 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Factorize_tpnf_rules
   Description:
     This module contains the rewriting rules used for the TPNF.
*)

open Ast_walker_rewrite_context
open Xquery_core_ast
open Typing_context

(* *** PHASE ONE *** *)
val insert_sbdo_xpath: 
    static_context rewrite_context -> acexpr -> acexpr * bool
val decompose_flwor:
    static_context rewrite_context -> acexpr -> acexpr * bool
val convert_where_clause:
    static_context rewrite_context -> acexpr -> acexpr * bool

(* *** PHASE TWO *** *)
val insert_scrambling_ddo:
    static_context rewrite_context -> acexpr -> acexpr * bool
val insert_scrambling_if:
    static_context rewrite_context -> acexpr -> acexpr * bool
val propagate_scrambling_bool:
    static_context rewrite_context -> acexpr -> acexpr * bool
val propagate_scrambling_ddo:
    static_context rewrite_context -> acexpr -> acexpr * bool
val propagate_scrambling_for_let:
    static_context rewrite_context -> acexpr -> acexpr * bool
val propagate_scrambling_for:
    static_context rewrite_context -> acexpr -> acexpr * bool
val propagate_scrambling_if:
    static_context rewrite_context -> acexpr -> acexpr * bool

(* *** PHASE THREE *** *)
val remove_scrambled_sbdo: 
    static_context rewrite_context -> acexpr -> acexpr * bool

val substitution:
    static_context rewrite_context -> acexpr -> acexpr * bool

val loop_fusion:
    static_context rewrite_context -> acexpr -> acexpr * bool

val loop_split:
    static_context rewrite_context -> acexpr -> acexpr * bool

val nested_loop_split:
    static_context rewrite_context -> acexpr -> acexpr * bool

val condition_detection:
    static_context rewrite_context -> acexpr -> acexpr * bool

val condition_shift:
    static_context rewrite_context -> acexpr -> acexpr * bool

val return_condition_lift:
    static_context rewrite_context -> acexpr -> acexpr * bool

val nested_return_condition_lift:
    static_context rewrite_context -> acexpr -> acexpr * bool

val return_result_lift:
    static_context rewrite_context -> acexpr -> acexpr * bool

val nested_return_result_lift:
    static_context rewrite_context -> acexpr -> acexpr * bool

val for_condition_lift:
    static_context rewrite_context -> acexpr -> acexpr * bool

val trivial_dot_condition:
    static_context rewrite_context -> acexpr -> acexpr * bool

val trivial_loop:
    static_context rewrite_context -> acexpr -> acexpr * bool

val dot_introduction:
    static_context rewrite_context -> acexpr -> acexpr * bool

val dot_loop:
    static_context rewrite_context -> acexpr -> acexpr * bool

val shortening_condition:
    static_context rewrite_context -> acexpr -> acexpr * bool

val filter_fusion:
    static_context rewrite_context -> acexpr -> acexpr * bool

(* *** PHASES FOUR-FIVE *** *)

val remove_redundant_sbdo: 
    static_context rewrite_context -> acexpr -> acexpr * bool

val check_normal_form:
    static_context rewrite_context -> acexpr -> acexpr * bool

val reintroduce_where:
    static_context rewrite_context -> acexpr -> acexpr * bool

val group_flwor_block:
    static_context rewrite_context -> acexpr -> acexpr * bool
val join_support_hack:
    static_context rewrite_context -> acexpr -> acexpr * bool
