(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: print_xquery_physical_type.mli,v 1.3 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Print_xquery_physical_type
   Description:
     Prints a physical operator and its signature.
*)

open Xquery_physical_type_ast

val string_of_physical_tuple_type : physical_tuple_type -> string
val string_of_physical_type : physical_type -> string
val string_of_physop_expr_name : Xquery_physical_algebra_ast.physop_expr_name -> string
val string_of_eval_sig : Xquery_algebra_ast.expr_eval_sig -> string
