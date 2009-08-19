(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: df_analysis.mli,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Df_analysis
   Description:
     This module provides

   1) a concrete instantiation of the generic data flow graph defined in Df_graph
   2) algorithms for deriving such an instance from a given XQuery core module.

   Since the data flow analysis is a whole-program analysis by nature, the module
   does not provide functions operating on individual parts of a query.

   - Michael *)


open Xquery_common_ast
open Xquery_core_ast


(* Just storing a reference to the original XQuery core ast node does not
   provide enough contextual information to uniquely determine the function
   of a dfnode. For some acexpr kinds, additional hints must be stored in
   addition to its origin. *)

type ac_hint =

  (* flwor *)
  | ACHFor of cvname
  | ACHLet of cvname
  | ACHOrderby
  | ACHWhere
      
  (* typeswitch *)
  | ACHTsclause of cvname

  (* module level *)
  | ACHVar of cvname

type ac_handle = acexpr option * ac_hint option


(**********************************)
(* XQuery core data flow analysis *)
(**********************************)

val df_analysis_of_xmodule :
  acxmodule -> (ac_handle, bool) Df_struct.dfgraph


(******************************************)
(* XQuery core data flow graph dot output *)
(******************************************)

val print_dot_dfgraph :
  Format.formatter -> (ac_handle, bool) Df_struct.dfgraph -> unit
