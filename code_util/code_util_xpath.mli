(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util_xpath.mli,v 1.4 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_util_xpath
   Description:
     This module contains some auxiliary evaluation code for XPath.
*)

open Typing_context

open Xquery_common_ast
open Xquery_algebra_ast

open Cursor
open Dm


(********************************************)
(* Auxiliary functions for XPath navigation *)
(********************************************)

val eval_axis_node_test : static_context -> axis -> anode_test -> node -> node cursor

