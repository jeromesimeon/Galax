(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util_pattern_matcher.mli,v 1.4 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_util_pattern_matcher
   Description:
     This module contains code that performs pattern-matching over the
     algebra AST. This is mostly used for handling predicates (in
     joins notably).
*)

open Algebra_type
open Xquery_algebra_ast_util
open Optimization_walker (* for the sub_expr_kind type *)

type navigation_param                   = sub_expr_kind * int (* the index in the sub_expr *)
type algop_test_function                = algop_expr -> bool 
type 'a state_update_function           = algop_expr -> 'a -> 'a

type 'a test_and_apply                  = algop_expr -> 'a -> bool * 'a (* a is arbitrary state *)
type 'a pattern_matcher_pattern_element = navigation_param * 'a test_and_apply
type 'a pattern_matcher_pattern         = 'a pattern_matcher_pattern_element list


val mk_dep_child_param   : int -> navigation_param
val mk_indep_child_param : int -> navigation_param
val dep_zero   : navigation_param
val indep_zero : navigation_param

val extract_term         : algop_test_function -> 'a state_update_function -> algop_expr -> 'a -> (bool * 'a)
val no_extraction        : algop_test_function -> algop_expr -> 'a -> (bool * 'a)

(* NOTE: the state functions should not have side effects, since we just reuse the same 'a *)
(* Walks several patterns and returns the first match *)
val walk_patterns : 'a -> 'a pattern_matcher_pattern list -> algop_expr -> bool * 'a
