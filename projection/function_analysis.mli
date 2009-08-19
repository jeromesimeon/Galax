(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: function_analysis.mli,v 1.5 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Function_analysis
   Description:
     This module implements the path analysis for built-in functions.
*)


(* Type for the kind of function *)

type function_kind =
  | UsedOnly           (* NOTE: This needs to be documented!! - Jerome *)
  | UsedReturnSimple
  | UsedReturnSubtree
  | ReturnsPaths
  | ReturnsDefault

(* Returns the kind of function *)

val get_fun_analysis_type : Namespace_names.rqname -> function_kind

