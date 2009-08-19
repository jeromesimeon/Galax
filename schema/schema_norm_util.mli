(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_norm_util.mli,v 1.2 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_norm_util
   Description:
     This module implements operations on schema which are used during
     XQuery normalization.
*)

open Namespace_names
open Namespace_symbols

open Xquery_common_ast
open Xquery_type_core_ast
open Xquery_core_ast
open Xquery_algebra_ast


(*******************************)
(* Utilities for normalization *)
(*******************************)

val check_optional_atomic_type_csequencetype : cxschema -> csequencetype -> unit
val get_optional_atomic_type_asequencetype   : cxschema -> asequencetype -> (rtype_symbol * bool)

val atomic_type_of_csequencetype : cxschema -> csequencetype -> Datatypes.atomic_type

val is_atomic_type_csequencetype  : cxschema -> csequencetype -> bool
val is_simple_type_csequencetype  : cxschema -> csequencetype -> bool
val is_numeric_type_csequencetype : cxschema -> csequencetype -> bool


