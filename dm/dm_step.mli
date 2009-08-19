(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm_step.mli,v 1.3 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Dm_step
   Description:
     This module contains some generic evaluation code for XPath
     steps.
*)

open Xquery_common_ast
open Xquery_type_core_ast
open Xquery_algebra_ast

open Dm_types


(*****************************************)
(* Auxiliary functions for type matching *)
(*****************************************)

val node_kind_of_principal_node_kind : Xquery_common_ast.principal -> _NodeKind

val item_matches_kind_test : 'a access_ops -> cxschema -> akind_test -> 'a -> bool

val eval_node_test_gen : 'a access_ops -> cxschema option -> axis -> anode_test -> 'a -> bool

