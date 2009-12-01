(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_util.mli,v 1.25 2007/02/12 21:15:33 simeon Exp $ *)

(* Module: Optimization_util
   Description:
     This module contains some utility functions used during algebraic
     optimization.
 *)

open Namespace_names
open Algebra_type
open Ast_logical_algebra_types
open Xquery_algebra_ast
open Xquery_common_ast

open Optimization_walker

(* tuple reanming functions *)
val replace_tuple_field_name :
    rqname -> rqname -> bool -> logical_algop_expr -> logical_algop_expr * bool

val replace_tuple_name :
    Compile_context.logical_compile_context -> 
      rqname -> 
	rqname ->  
	  logical_algop_expr ->
	    logical_algop_expr * bool

val is_update        	     : logical_algop_expr -> bool
val contain_updates  	     : logical_algop_expr -> bool
val count_tuple_field_access : logical_algop_expr -> (crname * int) list

(* Some generic stuff *)
val wrap_sep_sequence   : ('a,'b) Compile_context.compile_context -> logical_algop_expr -> crname * logical_algop_expr
val wrap_map_index_name : crname -> logical_algop_expr -> logical_algop_expr
val wrap_map_index      : ('a,'b) Compile_context.compile_context -> logical_algop_expr -> crname * logical_algop_expr
val wrap_map_null_named : crname -> logical_algop_expr -> logical_algop_expr
val wrap_map_null       : ('a,'b) Compile_context.compile_context -> logical_algop_expr -> crname * logical_algop_expr

val generic_snap_free_wrapper :
    (Compile_context.logical_compile_context ->
      logical_algop_expr ->
	logical_algop_expr * bool) ->
	  bool ref ->
	    Compile_context.logical_compile_context ->
	      logical_algop_expr ->
    ((unit, Ast_path_struct.path_annotation) aalgop_expr * sub_expr_kind * int) option ->
      logical_algop_expr ->
	logical_algop_expr

