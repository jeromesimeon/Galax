(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_common_ast_util.mli,v 1.7 2007/02/12 21:15:33 simeon Exp $ *)

(* Module: Xquery_common_ast_util
   Description:
     This module implements some useful operations the common subset
     of the XQuery AST.
*)

open Xquery_common_ast


(*********)
(* XPath *)
(*********)

(* Computes the princinpal node kind of a given axis *)

val principal_node_kind : Xquery_common_ast.axis -> principal

(* True for a forward axis, false otherwise *)

val forward_axis : axis -> bool

(* Bogus variable name *)

val bogus_cvname : cvname

