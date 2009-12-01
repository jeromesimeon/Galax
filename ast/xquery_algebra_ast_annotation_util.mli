(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* Module: Xquery_physical_type_ast_util
   Description:
     This module also provides functions access annotations on the algebra AST. 
*)

open Xquery_algebra_ast
open Xquery_physical_type_ast

open Ast_logical_algebra_types

(************************)
(* Annotation accessors *)
(************************)

(* Those fields in the tuple (empty if none) returned by this expression *)
val get_returned_fields       : free_variable_desc -> tuple_fields
val algop_get_returned_fields : ('a, 'b) aalgop_expr -> tuple_fields

(* All tuple fields accessed below this sub-tree *)
val get_accessed_fields : free_variable_desc -> tuple_fields
val algop_get_accessed_fields : ('a, 'b) aalgop_expr -> tuple_fields

(* All tuple field use counts accessed below this sub-tree *)
val get_tuple_field_use_counts       : free_variable_desc -> (tuple_field_use_count list * tuple_fields * cardinality)
val algop_get_tuple_field_use_counts : ('a, 'b) aalgop_expr -> (tuple_field_use_count list * tuple_fields * cardinality)

(* All free variables in this sub-tree *)
val get_free_variables  : free_variable_desc -> Xquery_common_ast.cvname list
val algop_get_free_variables  : ('a, 'b) aalgop_expr -> Xquery_common_ast.cvname list

(* Use counts of free-variables for variables in this sub-tree *)
(* This may not be exposed.. *)
val get_use_counts : free_variable_desc -> variable_use_count list
val algop_get_use_counts : ('a, 'b) aalgop_expr -> variable_use_count list

(* Use counts of variables bound here *)
val get_bound_use_counts : free_variable_desc -> variable_use_count list
val algop_get_bound_use_counts : ('a, 'b) aalgop_expr -> variable_use_count list

val check_signatures : (string -> string -> 'a list) -> 
  expr_eval_sig option -> ('b, 'c) aalgop_sub_exprs -> 'a list

val strip_annotation : ('a,'b) aalgop_expr -> logical_algop_expr

val materialization_check :  physical_type -> physical_type -> (string -> 'a list) -> 'a list

val deep_copy_expr   : ('a,'b) aalgop_expr -> ('a,'b) aalgop_expr


val print_annot : free_variable_desc -> unit
