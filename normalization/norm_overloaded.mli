(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: norm_overloaded.mli,v 1.6 2007/09/19 20:01:50 mff Exp $ *)

(* Module: Norm_overloaded
   Description:
     This modules implements some support for built-in overloaded
     functions in XQuery 1.0 and XPath 2.0.
  *)

open Norm_context

open Xquery_common_ast
open Xquery_core_ast


(* Is the function overloaded ? *)
val is_overloaded : (cfname * int) -> bool


(* Lookup an overloaded function *)
val table_for_overloaded_function :
    norm_context -> (cfname * int) -> (cfname * cfunction_signature * acfunction_body_kind * updating_modifier) list

(* The overloaded table for gt is used for order-by *)
val table_for_op_gt : norm_context -> (cfname * cfunction_signature * acfunction_body_kind * updating_modifier) list

(* The overloaded table for eq is used for order-by *)
val table_for_op_equal : norm_context -> (cfname * cfunction_signature * acfunction_body_kind * updating_modifier) list

(* Look the default atomic type for an overloaded function *)
val lookup_default_atomic_type : cfname -> Datatypes.atomic_type

