(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_errors.mli,v 1.12 2007/02/01 22:08:55 simeon Exp $ *)

(* Module: Typing_errors
   Description:
     This module implements supports for static typing errors.
 *)

open Typing_context

open Xquery_type_core_ast
open Xquery_core_ast

(***************)
(* Type errors *)
(***************)

(* Wrong expected type error *)

val raise_wrong_expected_type_error : static_context -> cxtype -> cxtype ->  cxtype
val raise_axis_type_error : static_context -> Xquery_common_ast.axis -> cxtype ->  cxtype

(* Empty/None type error *)

val raise_empty_type_error : static_context -> acexpr -> cxtype ->  cxtype
val raise_none_type_error  : static_context -> acexpr -> cxtype ->  cxtype

(* Checks is a type asumption is verified, replacing by the expected type *)

(*  check_type_replace
     STRONG : if Type1 <: Type2 then Type2 else TYPE_ERROR
     WEAK   : Type2
*)
val check_type_replace : static_context -> cxtype -> cxtype ->  cxtype
(* check_type_discard
     STRONG : if Type1 <: Type2 return Type1 else TYPE_ERROR
     WEAK   : Type1
*)
val check_type_discard : static_context -> cxtype -> cxtype ->  cxtype
(*
   check_type_ignore
     STRONG : if Type1 <: Type2 then () else TYPE_ERROR
     WEAK   : ()
*)
val check_type_ignore  : static_context -> cxtype -> cxtype ->  unit
(*
   check_type_branch
     STRONG : if Type1 <: Type2 then Type1 else TYPE_ERROR
     WEAK   : if Type1 <: Type2 then Type1 else Type3
*)
val check_type_branch  : static_context -> cxtype -> cxtype -> cxtype ->  cxtype
(*
   check_empty_type
     Expr : Type1
     if Type1 <: empty & (Expr not Empty) then Type1 else TYPE_ERROR
     if Type1 <: none  & (Expr not Error) then Type1 else TYPE_ERROR
*)
val check_empty_type   : static_context -> cxtype -> acexpr ->  cxtype

(* Check type declaration *)

val check_type_declaration : static_context -> cxtype -> (csequencetype * cxtype) option ->  cxtype

