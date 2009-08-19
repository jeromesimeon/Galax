(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: norm_var_graph.mli,v 1.1 2007/07/18 18:39:45 simeon Exp $ *)

(* Module: Norm_var_graph
   Description:
     This module implements support for keeping track of global
     variable dependencies.
*)

open Error

open Namespace_names
open Namespace_util

open Xquery_common_ast
open Xquery_core_ast

open Processing_context


(* Types for the var graph *)

type var_or_fun =
  | GlobalVariable of rqname
  | FunctionDeclaration of rqname

type var_graph

(* Init a var graph *)

val build_var_graph : unit -> var_graph
val reset_var_graph : var_graph -> unit

(* Operations on the var graph *)

val add_dependency : var_graph -> var_or_fun -> var_or_fun -> unit

val check_cyclic_variables : var_graph -> unit

