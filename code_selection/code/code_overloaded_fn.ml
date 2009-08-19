(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_overloaded_fn.ml,v 1.12 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_overloaded_fn
   Description:
     This module contains code building for operators that implement
     overloaded functions.
*)

open Compile_context
open Cs_util_coercion
open Code_selection_context
open Code_util_matching
open Code_util
open Namespace_names
open Error
open Physical_value_util
open Typing_context
open Xquery_algebra_ast
open Xquery_algebra_ast_util

(* CECall *)
let build_default_overloaded_fn_code code_ctxt cfname arity signatures =
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let match_fun = match_overloaded_function stat_ctxt cfname signatures in
  (fun alg_ctxt eargs ->
    (* Is this necessary ? *)
    let eargs = Array.map Cursor.cursor_of_list eargs in
    if (arity != Array.length eargs)
    then raise (Query (Code_Selection ("Arity mismatch in overloaded function")));
    let res =
      begin
	(* Pick the first function whose signature matches the types
           of the function arguments, after type promotion.  The
           promotion semantics is essentially equivalent to type
           matching, therefore no additional type matching is
           necessary here. *)
	let (fname, eargs) =
	    match_fun (Array.to_list eargs)
	    (* We never have to promote the result value, because all
	       the overloaded functions are built-in and therefore
	       guaranteed to return a value of the expected type. *)
	in
	  let bf = Code_fn.lookup_bltin_fctn (fname,arity) code_ctxt in
	  bf alg_ctxt (Array.of_list eargs)
      end
    in
    (Cursor.list_of_cursor "CSCODE FN OVERLOADED" res))

let build_overloaded_fn_code code_ctxt algop ((cfname,arity),table)  = 
  let _ = access_nosub algop.pdep_sub_expression in 
  let fn = build_default_overloaded_fn_code code_ctxt cfname arity table in
  (coerce_nodep fn coerce_many_item_list_to_item_list), code_ctxt

