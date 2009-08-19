(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: rewriting_judgments.mli,v 1.6 2007/10/16 01:25:35 mff Exp $ *)

(* Module: Rewriting_judgments
   Description:
     This module implements some auxiliary judgments used within
     rewriting rules.
*)

open Xquery_core_ast

val used_count : Xquery_common_ast.cvname -> acexpr -> int
    (* [used_count] determines the occurrence of variable $x in a
       given core expression *)

val can_fail : Processing_context.processing_context -> acexpr -> bool
    (* [can_fail] determines whether the given core expression can
       raise an error *)

val does_node_care_about_order : acexpr -> bool
    (* [is_ordered] determines whether the semantics of the expression
       depends on document order or not. *)

(* Inlining variables mess *)

val should_inline_variable : Processing_context.processing_context -> acexpr -> bool

(* recursive function detection *)
(*
val is_recursive_function : Norm_context.norm_context -> (Xquery_common_ast.cfname * int) -> bool

*)

val has_side_effect_judge : Xquery_core_ast.acexpr -> bool
(* val register_side_effect_free_function  : Xquery_common_ast.cfname -> unit *)

(* val global_is_side_effect_free_function : Xquery_common_ast.cfname -> bool *)
(* Nicola: this function doesn't seem to be used; 
           isn't the Updating flag sufficient? 
           besides, the implementation here is wrong, as it is comparing
           rqnames for "=" instead of rqname_equal *)

val side_effect_free_judge : Xquery_core_ast.acexpr -> bool

