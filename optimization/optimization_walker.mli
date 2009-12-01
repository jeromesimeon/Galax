(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_walker.mli,v 1.4 2007/03/09 19:26:33 mff Exp $ *)

(* Module: Optimization_walker
   Description:
     This module implements the generic rule-based rewriter used for
     optimization.
*)

open Error
open Namespace_builtin

open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_algebra_ast_annotation_util
open Ast_logical_algebra_types
open Compile_context
open Compile_annotate
open Processing_context

type optimization_rewrite_rule = 
    (logical_compile_context ->
      logical_algop_expr ->
	logical_algop_expr * bool) 

type optimization_removal_rewrite_rule =
    (logical_compile_context ->
      logical_algop_expr ->
	logical_algop_expr ->
	  logical_algop_expr * bool)

(* Generic stuff  *)
type sub_expr_kind = 
  | Dependent 
  | Independent

type ('a, 'b) child_desc = ('a, 'b) aalgop_expr * sub_expr_kind * int

val get_sub_expr    : ('a,'b) aalgop_expr -> sub_expr_kind -> ('a,'b) aalgop_sub_exprs
val make_child_desc : ('a,'b) aalgop_expr -> sub_expr_kind -> int -> ('a,'b) child_desc
val update_parent   : ('a,'b) child_desc option -> ('a,'b) aalgop_expr  -> unit
val get_op_from_sub_expr :
    ('a,'b) aalgop_expr -> sub_expr_kind -> int -> ('a,'b) aalgop_expr

(* *** Tree Walker *** *)
val descendent_walker: 
    ('a -> bool) -> 
      ('a -> 'a) -> 
	'a option -> 
	  'a -> 'a * 'a option * bool

val rewrite_apply:
    ((logical_algop_expr * sub_expr_kind * int) option ->
      logical_algop_expr ->
	logical_algop_expr) ->
	  (logical_algop_expr * sub_expr_kind * int) option ->
	    logical_algop_expr ->
	      logical_algop_expr

(* [fold_over_algop loc agg v e] recursively walks down e, applying the
   loc function to every node in the AST, then accumulating the result of
   applying the agg function to every value in the tree, from the seed
    value 'd. 
*)

val fold_over_algop  : 
    (('a,'b) aalgop_expr -> 'c) -> 
      ('d -> 'c -> 'd) -> 'd -> 
	('a,'b) aalgop_expr -> 'd


(* rewrite rule wrappers *)

val generic_wrapper: 
    (logical_compile_context ->
       logical_algop_expr ->
       logical_algop_expr * bool) ->
    bool ref ->
    logical_compile_context ->
    logical_algop_expr ->
    (logical_algop_expr * sub_expr_kind * int) option ->
    logical_algop_expr ->
    logical_algop_expr

val removal_wrapper:
    (logical_compile_context ->
       logical_algop_expr ->
       logical_algop_expr ->
       logical_algop_expr * bool) -> 
    bool ref ->
    logical_compile_context ->
    logical_algop_expr ->
    (logical_algop_expr * sub_expr_kind * int) option ->
    logical_algop_expr ->
    logical_algop_expr
