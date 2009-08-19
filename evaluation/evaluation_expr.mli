(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: evaluation_expr.mli,v 1.6 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Evaluation_expr
   Description:
     Execute the code associated to each algebraic operation
*)

open Algebra_type


(* Executes the chosen execution plan *)

val algebra_execute :
    Execution_context.algebra_context ->
      algop_expr -> Physical_value.physical_value

