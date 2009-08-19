(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_util.ml,v 1.3 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Factorize_util
   Description:
    Utilities used during query factorization.
*)

open Xquery_core_ast
open Xquery_core_ast_util

let update_replace_fun ce =
  let fi = ce.pcexpr_loc in
  let eh = ce.pcexpr_origin in
  let (cvname,cvar_expr) = gen_new_cvar eh fi in
  (CELET (None, cvname, ce),cvar_expr) 

let factored_count = ref 0

let get_new_factored_variable vn =
  let (prefix, uri, ncname) = vn in
    incr factored_count;
    (prefix, uri, (ncname ^ (string_of_int (!factored_count))))
