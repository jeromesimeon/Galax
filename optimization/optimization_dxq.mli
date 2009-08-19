(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_dxq.mli,v 1.3 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Optimization_dxq
   Description:
     This module implements the optimization rewrite rules for
     Distributed XQuery.
*)

open Optimization_walker

val lift_execute_rules : (optimization_rewrite_rule * string) list
