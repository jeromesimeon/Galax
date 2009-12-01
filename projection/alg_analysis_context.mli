(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: alg_analysis_context.mli,v 1.5 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Alg_analysis_context
   Description:
     Path analysis for document projection over the XQuery algebra.
*)

open Ast_path_struct

type analysis_context


val build_analysis_context :
  unit -> analysis_context

val add_var_paths :
  analysis_context -> Xquery_common_ast.cvname -> rooted_path_sequence -> analysis_context
  
val get_var_paths :
  analysis_context -> Xquery_common_ast.cvname -> rooted_path_sequence

(* Mirrors the implicit input tuple for algebra operations *)
val set_input_tuple_paths :
  analysis_context -> (Xquery_common_ast.cvname * rooted_path_sequence) list -> analysis_context
  
val get_input_tuple_paths :
  analysis_context -> (Xquery_common_ast.cvname * rooted_path_sequence) list
