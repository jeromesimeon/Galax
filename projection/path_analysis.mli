(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: path_analysis.mli,v 1.11 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Path_analysis
   Description:
     This module implements the path analysis algorithm used by
     projection.
*)


open Path_struct
open Analysis_context


(* Applies final serialization operation on the path sequences *)

val serialize :
    rooted_path_sequence * rooted_path_sequence -> rooted_path_sequence


(* Core expression level path analysis *)

(* 
val cexpr_path_analysis :
    analysis_context ->
      'a Xquery_core_ast.cexpr -> rooted_path_sequence * rooted_path_sequence

val cexpr_path_analysis_optim :
    analysis_context ->
      'a Xquery_core_ast.cexpr ->
	rooted_path_sequence * rooted_path_sequence * Optim_vars.optim_vars
*)

(* Query level path analysis *)

val path_analysis_of_cexpr      : analysis_context -> Xquery_core_ast.acexpr   -> rooted_path_sequence
val path_analysis_of_cstatement : analysis_context -> Xquery_core_ast.acstatement -> rooted_path_sequence
val path_analysis_of_cstatement_optim : analysis_context -> Xquery_core_ast.acstatement -> rooted_path_sequence
val path_analysis_of_cxmodule   : Xquery_core_ast.acxmodule   -> rooted_path_sequence * analysis_context
val path_analysis_of_cxmodule_optim   : Xquery_core_ast.acxmodule   -> rooted_path_sequence * analysis_context
(*val path_analysis_of_cupdate    : Xquery_core_ast.acupdate    -> rooted_path_sequence*)
val path_analysis_of_cprolog    : Xquery_core_ast.acprolog    -> rooted_path_sequence * analysis_context

(* Printing of analysis results *)

val print_intermediate_analysis :
    Format.formatter -> rooted_path_sequence * rooted_path_sequence -> unit

val print_full_analysis :
    Format.formatter -> rooted_path_sequence -> unit

