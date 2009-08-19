(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optim_vars.mli,v 1.5 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Optim_vars
   Description:
     This module implements operations on variable for optimized
     projection.
*)

(* Type representing a set of optimization variables *)

type optim_vars

(* The set of all optimization variables *)

val all_optimvars : optim_vars

(* The empty set *)

val empty_optimvars : optim_vars

(* Creates a singleton set with one optimization variable inside *)

val single_optimvars : Xquery_common_ast.cvname -> optim_vars

(* Is a given variable in the given set of optimization variables ? *)

val member : Xquery_common_ast.cvname -> optim_vars -> bool

(* The union of two sets of optimization variables *)

val union : optim_vars -> optim_vars -> optim_vars

(* The intersection between two sets of optimization variables *)

val intersection : optim_vars -> optim_vars -> optim_vars

(* Removes one variable from a set of optimization variables *)

val remove : Xquery_common_ast.cvname -> optim_vars -> optim_vars
val remove_list : Xquery_common_ast.cvname list -> optim_vars -> optim_vars

