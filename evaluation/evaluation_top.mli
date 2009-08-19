(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: evaluation_top.mli,v 1.9 2007/02/01 22:08:46 simeon Exp $ *)

(* Module Eval_top
   Description:
     This module implements evaluation of top-level statements and of
     the XQuery prolog.
*)

open Algebra_type
open Execution_context
open Physical_value

val eval_cstatement :
    algebra_context  -> algop_expr    -> physical_value
    (* Evaluate a top level statement *)

val eval_prolog :
    algebra_context -> algop_prolog   -> algebra_context * physical_value list
    (* Evaluate global declarations *)

val eval_library_module :
    algebra_context -> algop_xmodule  -> algebra_context * physical_value list
    (* Evaluate global declarations *)

val eval_main_module :
    algebra_context  -> algop_xmodule -> algebra_context * physical_value list
    (* Evaluate the whole query *)

