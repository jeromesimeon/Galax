(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util_pattern_matcher.ml,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_util_pattern_matcher
   Description:
     This module contains code that performs pattern-matching over the
     algebra AST. This is mostly used for handling predicates (in
     joins notably).
*)

open Xquery_algebra_ast_util
open Optimization_walker
open Algebra_type

(**********************)
(* Symbolic constants *)
(**********************)
type navigation_param = sub_expr_kind * int (* the index in the sub_expr *)
type 'a test_and_apply                  = algop_expr -> 'a -> bool * 'a (* a is arbitrary state *)
type 'a pattern_matcher_pattern_element = navigation_param * 'a test_and_apply
type 'a pattern_matcher_pattern         = 'a pattern_matcher_pattern_element list


type algop_test_function                = algop_expr -> bool 
type 'a state_update_function           = algop_expr -> 'a -> 'a

let mk_dep_child_param i = Dependent, i
let mk_indep_child_param i = Independent, i

let dep_zero   = mk_dep_child_param 0
let indep_zero = mk_indep_child_param 0

(**********************)
(* Extraction helpers *)
(**********************)

(* Do not extract anything, just perform the test
   on the op returnting the success of the test, state *)
let no_extraction test op st =
  if (test op) 
  then (true, st)
  else (false, st)

let extract_term test update_fn op st =
  if (test op)
  then (true, update_fn op st)
  else (false, st)

let boolean_no_extraction op st =
  no_extraction is_boolean op st


(*************************************************************)
(* Generic Walking code                                      *)
(*   parameter:                                              *)
(*     st                 - the state for the walker         *)
(*     pattern_match_list - a list of patterns to be matched *)
(*     op                 - current op                       *)
(*************************************************************)

let get_next_op op nav =
  let kind, index = nav in
    get_op_from_sub_expr op kind index

let match_pattern st (nav,f) op = 
  let next_op = get_next_op op nav in
    (f next_op st), next_op

let rec walk_pattern_match st pattern_match_list op = 
  match pattern_match_list with
  | [] -> (true, st)
  | (head :: tail) ->
      let (matched, st), current_op = match_pattern st head op in
      if matched
      then walk_pattern_match st tail current_op
      else (matched,st)

(* st should not have side-effects *)
let rec walk_patterns st patterns op =
  match patterns with
  | [] -> false, st
  | (cur_pattern :: more_patterns) ->
      let matched, st = walk_pattern_match st cur_pattern op in
      if matched
      then (matched, st)
      else walk_patterns st more_patterns op

