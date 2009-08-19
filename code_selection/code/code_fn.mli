(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_fn.mli,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_fn
   Description:
     This module implements evaluation of F&O functions.
*)

open Execution_context

type bltin = Algebra_type.item_cursor_nary_to_item_cursor_code
type bltin_mat = Algebra_type.item_list_nary_to_item_list_code

val lookup_bltin_fctn :
    (Xquery_common_ast.cfname * int) -> Code_selection_context.code_selection_context -> bltin

val add_bltin_fctn : (Xquery_common_ast.cfname * int) * (Code_selection_context.code_selection_context -> bltin) -> unit 

(* exporting following functions for reuse in cs_code.ml -- Philippe *)
val _fn_boolean : Algebra_type.alg_compile_context -> bltin 

