(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util_join.mli,v 1.6 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_util_join
   Description:
     Some common utilities used for the join algorithms.
*)

open Algebra_type
open Xquery_algebra_ast
open Xquery_common_ast
open Code_selection_context


(******************)
(* Join detection *)
(******************)

type join_kind =
  | NestedLoopJoin
  | HashJoin
  | SortJoin

val get_join_kind : Algebra_type.algop_expr -> join_kind


(****************************)
(* "Pre-compile" predicates *)
(****************************)

val build_sort_cond :
    code_selection_function -> code_selection_context ->
      algop_expr -> Code_util_predicates.predicate_functions

val build_hash_cond :
    code_selection_function -> code_selection_context ->
      algop_expr -> Code_util_predicates.predicate_functions

(**********************)
(* Outer-join support *)
(**********************)

type outer_kind =
  | StandardJoin
  | OuterJoin of crname

type null_functions =
    (bool * (unit -> unit) * (unit -> unit) * Physical_value.dom_value array)

val get_null_functions :
    code_selection_context -> outer_kind -> crname array -> null_functions

val select_physical_op : outer_kind -> algop_expr -> Xquery_physical_algebra_ast.physop_expr_name

