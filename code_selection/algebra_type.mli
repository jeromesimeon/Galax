(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: algebra_type.mli,v 1.12 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Algebra_type
   Description:
     This module contains type declarations for the evaluation code.
*)

open Dm
open Physical_value

open Xquery_common_ast
open Xquery_algebra_ast

open Execution_context


(************************)
(* Physical annotations *)
(************************)

(* Algebra AST annotations at the physical level *) 

type physical_annotation =
    { has_nested_snap   : bool;
      has_side_effect   : bool;
      has_delta_update  : bool;
      path_annotation   : Ast_path_struct.path_annotation;
      mutable materialize_tuple_stream: bool }

(*******************)
(* Evaluation code *)
(*******************)

(* Evaluation code *)

type eval_code_unit   = algebra_context -> unit -> physical_value
type eval_code_unary  = algebra_context -> physical_value -> physical_value
type eval_code_binary = algebra_context -> physical_value -> physical_value -> physical_value
type eval_code_many   = algebra_context -> physical_value array -> physical_value

type alg_eval_code =
  | AOECUnit of eval_code_unit
  | AOECUnary of eval_code_unary
  | AOECBinary of eval_code_binary
  | AOECMany of eval_code_many

(* Query plans annotated with their evaluation code *)
type eval_fun = algebra_context -> algop_expr -> physical_value
(* Entry code for tail-recursive function call to this function *)
and tail_entry_code =  (Physical_value.physical_value array -> algop_expr)
    (* Exit code for tail-recursive function call to this function *)
and tail_exit_code = (Physical_value.physical_value -> Physical_value.physical_value)

and alg_eval_code_dep =
    (* Nicola: NoDep also needs eval_fun for the case when we do materialization *)
  | NoDep of (eval_fun -> alg_eval_code) * (tail_entry_code * tail_exit_code) option
  | SomeDep of (eval_fun -> alg_eval_code) * (tail_entry_code * tail_exit_code) option
and algop_sub_exprs = (alg_eval_code_dep, physical_annotation) aalgop_sub_exprs
and algop_expr = (alg_eval_code_dep, physical_annotation) aalgop_expr


(*******************************)
(* Evaluation code for modules *)
(*******************************)

(* Evaluation code *)

type eval_code_unit_prolog   = algebra_context -> unit -> algebra_context
type eval_code_unary_prolog  = algebra_context -> physical_value -> algebra_context
type eval_code_binary_prolog =
    algebra_context -> physical_value -> physical_value -> algebra_context
type eval_code_many_prolog   =
    algebra_context -> physical_value array -> algebra_context

type alg_eval_code_prolog =
  | PAOECUnit of eval_code_unit_prolog
  | PAOECUnary of eval_code_unary_prolog
  | PAOECBinary of eval_code_binary_prolog
  | PAOECMany of eval_code_many_prolog
type alg_eval_code_dep_prolog =
  | PNoDep of alg_eval_code_prolog
  | PSomeDep of (eval_fun -> alg_eval_code_prolog) 

(* Eval *)

type eval_fun_prolog = algebra_context -> algop_expr -> algebra_context

(* Query plans annotated with their evaluation code *)

type algop_decl =
    (alg_eval_code_dep, physical_annotation, alg_eval_code_dep_prolog) aalgop_decl
type algop_prolog =
    (alg_eval_code_dep, physical_annotation, alg_eval_code_dep_prolog) aalgop_prolog
type algop_xmodule =
    (alg_eval_code_dep, physical_annotation, alg_eval_code_dep_prolog) aalgop_xmodule
type algop_function_body =
    (alg_eval_code_dep, physical_annotation) aalgop_expr
type alg_compile_context =
    (alg_eval_code_dep, physical_annotation) Compile_context.compile_context


(* Physical signatures *)

(* Physical: [Cursor(item); ... Cursor(item)] -> Cursor(item) *)
type item_cursor_nary_to_item_cursor_code =
    algebra_context -> item Cursor.cursor array -> item Cursor.cursor

type item_list_nary_to_item_list_code =
    algebra_context -> item list array -> item list

