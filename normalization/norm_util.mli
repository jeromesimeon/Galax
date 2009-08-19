(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: norm_util.mli,v 1.35 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Norm_util
   Description:
     This module implements utilities used during normalization.
*)

open Norm_context

open Xquery_common_ast
open Xquery_ast
open Xquery_core_ast
open Xquery_core_ast_annotation
open Xquery_type_core_ast


(*******************************)
(* Core AST creation functions *)
(*******************************)

(* Build a core if *)
val build_core_if :
    norm_context -> acexpr -> acexpr -> acexpr -> expr_handle -> Finfo.finfo -> acexpr

(* Build a core while *)
val build_core_while :
    norm_context -> acexpr -> acexpr -> expr_handle -> Finfo.finfo -> acexpr

(* Build a core some *)
val build_core_some :
    norm_context -> (csequencetype * cxtype) option -> cvname -> acexpr -> acexpr -> expr_handle -> Finfo.finfo -> acexpr

(* Build a core every *)
val build_core_every :
    norm_context -> (csequencetype * cxtype) option -> cvname -> acexpr -> acexpr -> expr_handle -> Finfo.finfo -> acexpr

(* Build a core attribute constructor *)
val build_core_attribute_constructor :
    norm_context -> caname -> acexpr list -> expr_handle -> Finfo.finfo -> acexpr

(* Build a core element constructor *)
val build_core_element_constructor :
    norm_context -> cename -> Namespace_context.nsenv -> acexpr list -> expr_handle -> Finfo.finfo -> acexpr

(* Build a core function call *)
val build_core_call :
    norm_context -> cfname -> acexpr list -> expr_handle -> Finfo.finfo -> acexpr

(* Build a core function call *)
val build_core_overloaded_call :
    norm_context -> cfname -> acexpr list -> expr_handle -> Finfo.finfo -> acexpr

(* Build a core cast expression *)
val build_core_cast :
    norm_context -> acexpr -> (csequencetype * cxtype) -> expr_handle -> Finfo.finfo -> acexpr

(* Build a core castable expression *)
val build_core_castable :
    norm_context -> acexpr -> (csequencetype * cxtype) -> expr_handle -> Finfo.finfo -> acexpr

(* Build a core expressions for true() and false () *)

val build_core_true  : norm_context -> expr_handle -> Finfo.finfo -> acexpr
val build_core_false : norm_context -> expr_handle -> Finfo.finfo -> acexpr


(*************************)
(* Useful for predicates *)
(*************************)

type predicate_kind =
  | First
  | Last
  | Numeric
  | Other
val get_predicate_kind : acexpr -> predicate_kind


(****************)
(* FS functions *)
(****************)

val item_seq_to_untyped :
    norm_context -> acexpr -> expr_handle -> Finfo.finfo -> acexpr

val item_seq_to_untyped_optional :
    norm_context -> acexpr -> expr_handle -> Finfo.finfo -> acexpr

val item_seq_to_node_seq :
    norm_context -> acexpr -> expr_handle -> Finfo.finfo -> acexpr

(***************)
(* Atomization *)
(***************)

val normalize_atomize :
    norm_context -> acexpr -> expr_handle -> Finfo.finfo -> acexpr 


(***********)
(* Casting *)
(***********)

val normalize_to_cast     :
    norm_context -> acexpr -> (csequencetype * cxtype) -> expr_handle -> Finfo.finfo -> acexpr
val normalize_to_castable :
    norm_context -> acexpr -> (csequencetype * cxtype) -> expr_handle -> Finfo.finfo -> acexpr


(**************)
(* Predicates *)
(**************)

val normalize_effective_boolean_value :
    norm_context -> acexpr -> expr_handle -> Finfo.finfo -> acexpr
val normalize_predicate_truth_value :
    norm_context -> acexpr -> acexpr -> expr_handle -> Finfo.finfo -> acexpr


(************************)
(* Quantification/Tests *)
(************************)

val normalize_to_if    :
    norm_context -> acexpr -> acexpr -> acexpr -> expr_handle -> Finfo.finfo -> acexpr
val normalize_to_while :
    norm_context -> acexpr -> acexpr -> expr_handle -> Finfo.finfo -> acexpr
val normalize_to_some  :
    norm_context -> (csequencetype * cxtype) option -> cvname -> acexpr -> acexpr -> expr_handle -> Finfo.finfo -> acexpr
val normalize_to_every :
    norm_context -> (csequencetype * cxtype) option -> cvname -> acexpr -> acexpr -> expr_handle -> Finfo.finfo -> acexpr


(*************)
(* Operators *)
(*************)

val normalize_unary_operator  : norm_context -> unaryop -> acexpr           -> expr_handle -> Finfo.finfo -> acexpr 
val normalize_binary_operator : norm_context -> binop   -> acexpr -> acexpr -> expr_handle -> Finfo.finfo -> acexpr 


(******************)
(* Function calls *)
(******************)

(* Apply rules for converting function input or output to a simple type *)
val convert_function_input_output : 
    Norm_context.norm_context -> (cvname * acexpr) * (acexpr * (csequencetype * cxtype)) -> acexpr -> acexpr
val normalize_function_app :
    Norm_context.norm_context -> cfname -> acexpr list -> expr_handle -> Finfo.finfo -> acexpr
val normalize_atomic_constructor :
    Norm_context.norm_context -> cfname -> acexpr list -> expr_handle -> Finfo.finfo -> acexpr

val normalize_ident_function_app :
    Norm_context.norm_context -> cfname -> acexpr list -> expr_handle -> Finfo.finfo -> acexpr

(*
val normalize_ident_function_app :
    Norm_context.norm_context -> cfname -> acexpr list -> Xquery_ast.expr_handle -> Finfo.finfo -> acexpr
*)

(*****************************)
(* Normalize a sequence type *)
(*****************************)

val normalize_kind_test :
    norm_context -> kind_test -> (ckind_test * cxtype)
val normalize_sequencetype :
    norm_context -> sequencetype -> (csequencetype * cxtype)
val normalize_optional_sequencetype :
    norm_context -> sequencetype option -> (csequencetype * cxtype) option
val normalize_optional_sequencetype_strong :
    norm_context -> sequencetype option -> (csequencetype * cxtype) 

(* Useful error message *)
val incorrect_arg_count : Namespace_names.rqname -> int -> int -> exn 

(*****************************************************************************)
(* Check whether a core expression  may generate updates (deltas or effects) *)
(*****************************************************************************)

val expr_may_generate_updates : acexpr -> bool

val resolve_variable_qname_register : Norm_context.norm_context -> vname -> Norm_context.norm_context * cvname
val resolve_global_qname_register   : Norm_context.norm_context -> vname -> Xquery_core_ast.acvar_body -> Norm_context.norm_context * cvname
val resolve_variable_qname_check    : Norm_context.norm_context -> Finfo.finfo -> vname -> (cvname * Xquery_core_ast.acvar_body option)
val check_server_implementation     : Norm_context.norm_context -> Finfo.finfo -> Namespace_names.ncname -> (cvname * string) 
val check_interface                 : Norm_context.norm_context -> Finfo.finfo -> Namespace_names.ncname -> string

val map_fun_kind :
  Xquery_ast.function_body -> Xquery_core_ast.acfunction_body_kind
val lookup_and_map_interface :
  Norm_context.norm_context ->
  Namespace_names.prefix ->
  (Namespace_names.prefix * string) -> Norm_context.norm_context * Xquery_core_ast.acprolog
val extend_server_environment :
  bool ->
  Norm_context.norm_context ->
  Namespace_names.ncname ->
  Namespace_names.ncname ->
  (Xquery_ast.expr * Xquery_core_ast.acexpr) ->
  Norm_context.norm_context * string * Xquery_core_ast.acinterface *
  Xquery_core_ast.acexpr

