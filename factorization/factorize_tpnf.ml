(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_tpnf.ml,v 1.16 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Factorize_tpnf
   Description:

   This module is a factorization plugin for the TPNF approach
   for normalizing XQuery Core expressions.

   -- Philippe
*)

open Xquery_core_ast

open Ast_walker_rewrite_context
open Ast_walker_rewrite

open Factorize_tpnf_rules

let unique_binding_id = ref 0 

let get_new_factored_name () = 
  let local_name = "tpnf_" ^ (string_of_int !unique_binding_id) in 
    incr unique_binding_id;
    (Namespace_builtin.glx_prefix, Namespace_builtin.glx_uri, local_name)

(* Apply a ruleset -- fixpoint *)
let rewriting_cexpr recurse_rule_set toplevel_rule_set static_ctxt acexpr =
  let recurse_rewrite_ctxt = build_rewrite_context static_ctxt recurse_rule_set [] in
  let toplevel_rewrite_ctxt = build_rewrite_context static_ctxt toplevel_rule_set [] in
  let new_cexpr = rewrite_cexpr (toplevel_rewrite_ctxt, recurse_rewrite_ctxt) acexpr in
  new_cexpr

(* *** Phase one: convert where-clauses and introduce ddo-calls *** *)
let phase_one_recurse = [
  convert_where_clause;
  decompose_flwor;
  insert_sbdo_xpath;
]

(* *** Phase two *** *)
(*  - introduce scrambling annotations *)
(*  - propagate scrambling annotations *)
(*  - structural rewritings            *)
let phase_two_recurse_tpnf = [
  insert_scrambling_ddo;
  insert_scrambling_if;

  propagate_scrambling_bool;
  propagate_scrambling_ddo;
  propagate_scrambling_for_let;
  propagate_scrambling_for;
  propagate_scrambling_if;

  remove_scrambled_sbdo;
  substitution;
  loop_fusion;
  condition_detection; 
  condition_shift;
  return_condition_lift; nested_return_condition_lift;
  return_result_lift; nested_return_result_lift;
  for_condition_lift;
  trivial_dot_condition; 
  trivial_loop;
  dot_introduction;
  dot_loop;
]

let phase_two_recurse_tpnf' = [
  insert_scrambling_ddo;
  insert_scrambling_if;

  propagate_scrambling_bool;
  propagate_scrambling_ddo;
  propagate_scrambling_for_let;
  propagate_scrambling_for;
  propagate_scrambling_if;

  remove_scrambled_sbdo;
  substitution;
  condition_detection; 
  condition_shift;
  return_condition_lift; nested_return_condition_lift;
  return_result_lift; nested_return_result_lift;
  for_condition_lift;
  trivial_dot_condition; 
  trivial_loop;
  dot_introduction;
  dot_loop; 

  (* instead of loop fusion: *) 
  loop_split; 
  nested_loop_split; 
  filter_fusion;
  shortening_condition;
]



(* *** Phase three: verification *** *)
let phase_three_recurse  = [
  check_normal_form;
  remove_redundant_sbdo;
]


(* *** Phase four: pack'n go *** *)
let phase_four_recurse  = [
  reintroduce_where;
  join_support_hack; 
]


let factorize_tpnf stat_ctxt cexpr =
  let nexpr1 = rewriting_cexpr phase_one_recurse       [] stat_ctxt cexpr  in
  let nexpr2 = rewriting_cexpr phase_two_recurse_tpnf' [] stat_ctxt nexpr1 in
  let nexpr3 = rewriting_cexpr phase_three_recurse     [] stat_ctxt nexpr2 in
  let nexpr4 = rewriting_cexpr phase_four_recurse      [] stat_ctxt nexpr3 in
  nexpr4
