(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: compile_context.mli,v 1.22 2007/05/16 15:32:09 mff Exp $ *)

(* Module: Compile_context
   Description:
     This module contains context information used during algebraic
     compilation.
*)

open Xquery_core_ast
open Xquery_common_ast
open Xquery_algebra_ast

open Typing_context

(***********************)
(* Compilation context *)
(***********************)

type ('a,'b) compile_context

(* Create a new compilation context *)

val build_compile_context : static_context -> ('a,'b) compile_context

(* Default compilation context *)

val default_compile_context : Norm_context.norm_context -> ('a,'b) compile_context

(* Replace the static context *)

val replace_static_context_in_compile_context :
    static_context -> ('a,'b) compile_context -> ('a,'b) compile_context

val replace_namespace_env_in_compile_context :
    Namespace_context.nsenv -> ('a,'b) compile_context -> ('a,'b) compile_context

(* Accesses parts of the static context from the compilation context *)
val static_context_from_compile_context :
    ('a,'b) compile_context -> static_context 
val norm_context_from_compile_context :
    ('a,'b) compile_context -> Norm_context.norm_context

(***************************)
(* Treatement of functions *)
(***************************)

val add_function_to_compile_context :
    ('a,'b) compile_context ->
      (cfname * int) -> ('a,'b) aalgop_function_body -> unit

val get_function_from_compile_context : string -> ('a,'b) compile_context -> (cfname * int) 
  -> ('a,'b) aalgop_function_body

val mem_function_from_compile_context    : ('a,'b) compile_context -> (cfname * int) -> bool
val update_physical_plan_in_compile_context : ('a,'b) compile_context -> (cfname * int) -> ('a,'b) aalgop_expr -> unit

val register_builtin : (cfname * int) -> unit
val is_builtin :  (cfname * int) -> bool


(***************************)
(* Treatement of variables *)
(***************************)

(*
val set_input     : ('a,'b) compile_context -> ('a,'b) compile_context
val unset_input   : ('a,'b) compile_context -> ('a,'b) compile_context
*)
val has_input_set : ('a,'b) compile_context -> bool

val add_variable_field_to_compile_context :
    ('a,'b) compile_context -> cvname -> ('a,'b) compile_context

val hide_variable_field_from_compile_context :
    ('a,'b) compile_context -> cvname -> ('a,'b) compile_context

val get_variable_field_from_compile_context :
    ('a,'b) compile_context -> cvname -> crname option

val get_tuple_field_name :
    ('a,'b) compile_context -> cvname -> crname 


(* Getting fresh variables *)
val get_new_variable_name : ('a, 'b) compile_context -> string -> Namespace_names.rqname
val get_new_group_name    : ('a, 'b) compile_context -> Namespace_names.rqname
val get_new_dot_name      : ('a, 'b) compile_context -> Namespace_names.rqname
val get_new_var_name      : ('a, 'b) compile_context -> Namespace_names.rqname

val no_more_input : ('a,'b) compile_context -> unit

val update_compile_context_from_module : 
  ('a, 'b) compile_context -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_xmodule -> 
  ('a, 'b) compile_context

val copy_compile_context   : ('a,'b) compile_context ->('a,'b) compile_context
val copy_without_functions : ('a,'b) compile_context ->('c,'d) compile_context
val map_function_bodies    : ('a,'b) compile_context -> ((('c,'d) aalgop_function_body) Namespace_util.RQNameIntHashtbl.t -> (cfname * int) -> ('a,'b) aalgop_function_body -> unit) -> ('c,'d) compile_context
val update_compile_context : ('a,'b) compile_context -> ('a,'b) compile_context -> ('a,'b) compile_context


