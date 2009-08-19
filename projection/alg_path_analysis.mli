(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: alg_path_analysis.mli,v 1.6 2007/05/16 15:32:12 mff Exp $ *)

(* Module: Alg_path_analysis
   Description:
     Path analysis for document projection over the XQuery algebra.
*)

(* Path analysis over the XQuery algebra. Intended for use
   with document projection and streaming. - Michael *)

open Alg_path_struct
open Alg_analysis_context
open Xquery_algebra_ast
open Processing_context

(*****************)
(* Path analysis *)
(*****************)

val path_analysis_of_logical_algop_expr :
  Logical_algebra_types.logical_algop_expr -> unit

val path_analysis_of_logical_algop_prolog :
  Logical_algebra_types.logical_algop_prolog  -> unit

val path_analysis_of_logical_algop_xmodule :
  Logical_algebra_types.logical_algop_xmodule  -> unit

(* This function is used in a hackish way by Galax-project.
   It should be removed. - Michael *)
val path_analysis_of_statement :
  Compiled_program_units.compiled_statement -> rooted_path_sequence 

(**************************)
(* Streamability analysis *)
(**************************)

val is_streaming_prohibitive :
  rooted_path_sequence -> bool


(************)
(* Printing *)
(************)

val print_full_analysis :
  Format.formatter -> rooted_path_sequence -> unit


val print_intermediate_analysis :
  Format.formatter -> Alg_path_struct.paths -> unit
