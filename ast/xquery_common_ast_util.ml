(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_common_ast_util.ml,v 1.9 2007/02/12 21:15:33 simeon Exp $ *)

(* Module: Xquery_common_ast_util
   Description:
     This module implements some useful operations the common subset
     of the XQuery AST.
*)

open Xquery_common_ast

open Error


(*********)
(* XPath *)
(*********)

(* Computes the princinpal node kind of a given axis *)

let principal_node_kind a =
  match a with
  | Xquery_common_ast.Attribute -> PrincipalAttribute
  | Xquery_common_ast.Ancestor
  | Xquery_common_ast.Ancestor_or_self
  | Xquery_common_ast.Child
  | Xquery_common_ast.Descendant
  | Xquery_common_ast.Descendant_or_self
  | Xquery_common_ast.Following_sibling
  | Xquery_common_ast.Preceding_sibling
  | Xquery_common_ast.Following
  | Xquery_common_ast.Preceding
  | Xquery_common_ast.Parent
  | Xquery_common_ast.Self -> PrincipalElement

(* True for a forward axis, false otherwise *)

let forward_axis axis =
  match axis with 
  | Parent -> false 
  | Ancestor -> false 
  | Ancestor_or_self -> false 
  | Attribute -> true
  | Child -> true
  | Descendant -> true
  | Descendant_or_self -> true
  | Following_sibling -> true
  | Preceding_sibling -> false
  | Self -> true
  | Following -> true
  | Preceding -> false


(* Bogus variable name *)

let bogus_cvname = (Namespace_names.NSDefaultElementPrefix,Namespace_names.NSWildcardUri,"")

