(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_step.mli,v 1.7 2007/02/01 22:08:55 simeon Exp $ *)

(* Module: Typing_step
   Description:
     This module implements static typing for XPath steps (axis and
     node tests).
 *)

open Typing_context

open Xquery_common_ast
open Xquery_core_ast
open Xquery_type_core_ast


(* Takes axis, node test *) 
val compute_type_axis_node_test : static_context -> axis -> cnode_test -> Finfo.finfo -> cxtype
