(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_context.mli,v 1.13 2007/05/16 15:32:13 mff Exp $ *)

(* Module: Typing_context
   Description:
     This module implements support for the static typing context.
 *)

open Xquery_common_ast
open Xquery_core_ast
open Xquery_type_core_ast

open Norm_context


(**********************)
(* The static context *)
(**********************)

type static_context

(* Note:
    The static contex contains:
      - The normalization context.
      - Bindings from variables to their static type.
*)

(* Default static context *)

val default_static_context  : Norm_context.norm_context -> static_context

(* Replace the normalization context *)

val replace_norm_context_in_static_context :
    norm_context -> static_context -> static_context

(* Lookup a variable's type in static context *)

val var_from_static_context :static_context -> cvname -> Finfo.finfo -> cxtype

(* Add a variable and its type to static context *)

val add_var_to_static_context : static_context -> (cvname * cxtype) -> static_context

(* Accessors *)

val processing_context_from_static_context : static_context -> Processing_context.module_processing_context
val norm_context_from_stat_context   	   : static_context -> norm_context
val schema_from_static_context       	   : static_context -> cxschema
val schema_namer_from_static_context 	   : static_context -> Schema_namer.t
val vars_from_static_context         	   : static_context -> (cvname * cxtype) list
