(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_call.mli,v 1.12 2007/02/01 22:08:55 simeon Exp $ *)

(* Module: Typing_call
   Description:
     This module implements support for static typing of function
     calls.
 *)

open Typing_context

open Xquery_common_ast
open Xquery_core_ast
open Xquery_type_core_ast


(* Static types for function calls *)

(* 
   match_overloaded_function_signature looks up most specific function
   that implements given overloaded function applied to arguments with
   given types.   It _requires_ that each argument type be a unit type.

   The most-specific function, its argument types, and its output type
   is returned.  If no more specific function exists, the function and
   its arguments types are returned.
*)

val match_overloaded_function_signature :
    static_context -> cfname -> cxtype list ->
      overloaded_signature_table -> (cfname * cxtype list * cxtype * updating_modifier)

val compute_type_overloaded_function_call :
    static_context -> cfname -> cxtype list -> overloaded_signature_table -> cxtype

val compute_type_normal_function_call :
    static_context -> cfname -> cxtype list ->
      ((csequencetype * cxtype) option list * (csequencetype * cxtype)) -> cxtype
