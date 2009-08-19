(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: df_analysis_context.ml,v 1.3 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Df_analysis_context
   Description:
     This module provides a functional context for the XQuery core
     data flow analysis algorithm by storing a mapping between
     variable names and graphs.
*)

open Xquery_common_ast

open Error

open Df_struct


type  ('a, 'b) df_analysis_context = (cvname *  ('a, 'b) dfgraph) list


let build_df_analysis_context () =
  []

let add_var_dfgraph ctxt cvname dfgraph =
  (cvname, dfgraph) :: ctxt

let get_var_dfgraph ctxt cvname =
  try
    List.assoc cvname ctxt
  with
  | _ ->
      raise (Query (Streaming_XPath ("Cannot find variable $"^(Namespace_names.prefixed_string_of_rqname cvname)^" in data flow analysis context.")))
