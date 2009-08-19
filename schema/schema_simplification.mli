(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_simplification.mli,v 1.3 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_simplification
   Description:
     This modules implements simplification rewritings over
     type. Those rewritings preserve the semantics of the type, but
     result in simpler, more readable types.
 *)

open Xquery_type_core_ast

val simplify_one_ty : cxschema -> cxtype -> cxtype * bool
val simplify_ty     : cxschema -> cxtype -> cxtype

