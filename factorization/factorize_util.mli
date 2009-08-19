(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_util.mli,v 1.3 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Factorize_util
   Description:
    Utilities used during query factorization.
*)

open Xquery_core_ast

val update_replace_fun : acexpr -> (acfl_expr * acexpr)
val get_new_factored_variable : Xquery_common_ast.cvname -> Xquery_common_ast.cvname
