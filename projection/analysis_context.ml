(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: analysis_context.ml,v 1.5 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Analysis_context
   Description:
     This module contains context information used during path
     analysis.
*)

open Error

open Xquery_common_ast

open Path_struct

type analysis_context =
    (cvname * rooted_path_sequence) list

let build_analysis_context () =
  []

let add_var_path analysis_context var path_seq =
  (var,path_seq) :: analysis_context

let get_var_path analysis_context var =
  try
    List.assoc var analysis_context
  with
  | _ ->
      raise (Query (Prototype ("Cannot find variable $"^(Namespace_names.prefixed_string_of_rqname var)^" in path analysis context")))

