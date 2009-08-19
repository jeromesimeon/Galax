(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: analysis_context.mli,v 1.4 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Analysis_context
   Description:
     This module contains context information used during path
     analysis.
*)


type analysis_context

val build_analysis_context :
    unit -> analysis_context

val add_var_path :
    analysis_context -> Xquery_common_ast.cvname -> Path_struct.rooted_path_sequence -> analysis_context

val get_var_path :
    analysis_context -> Xquery_common_ast.cvname -> Path_struct.rooted_path_sequence

