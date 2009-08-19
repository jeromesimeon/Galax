(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util_predicates.mli,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_util_predicates
   Description:
     This module implements some code utilities over predicates.
*)


(**********************)
(* Predicate branches *)
(**********************)

type op_expr_type =
  | RegularOp of Algebra_type.algop_expr
  | Fs_untyped_to_any
  | Invalid_Predicate_Branch

type predicate_branch

val mk_predicate_branch :
    Algebra_type.algop_expr option ->
      op_expr_type ->
	(* Michael - (Physical_value.item Physical_value.sequence -> unit) option -> *)
	(Physical_value.xml_value -> unit) option ->
	  predicate_branch

val invalid_predicate_branch : predicate_branch

val is_fs_untyped_to_any_predicate_branch : predicate_branch -> bool

(**********************)
(* Runtime Evaluation *)
(**********************)

val eval_predicate_desc :
    Algebra_type.eval_fun ->
      Execution_context.algebra_context -> 
	Algebra_type.algop_expr array ->
	  Xquery_algebra_ast.predicate_desc -> bool

val eval_predicate_desc_to_rid_list :
    ('a -> Code_util_ridlist.rid_list) -> (* a function evaluate "predicates" *)
      'a array ->
	Xquery_algebra_ast.predicate_desc ->
	  Code_util_ridlist.rid_list


(************************)
(* These arise in joins *)
(************************)

val evaluate_predicate_branch :
    predicate_branch -> 
      Algebra_type.eval_fun ->
	Execution_context.algebra_context ->
	  Namespace_context.nsenv ->
	    Dm_atomic.atomicValue list


(* Revised join_type *)
type supported_predicates
type supported_scans =
  | Value_Equality
  | Greater_Than
  | Greater_Than_or_Equal
  | Less_Than
  | Less_Than_or_Equal
  | Invalid_Scan (* Like none *)

val switch_predicate : supported_predicates -> supported_predicates
val supported_predicate_supported_scans : supported_predicates -> supported_scans

val predicate_type : Algebra_type.algop_expr -> supported_predicates

val needed_types : Algebra_type.algop_expr -> supported_predicates -> Datatypes.atomic_type list

val predicate_invalid : supported_predicates

type predicate_functions =
    (predicate_branch * predicate_branch * supported_scans) array
    
