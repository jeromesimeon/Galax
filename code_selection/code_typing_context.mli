(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_typing_context.mli,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_typing_context
   Description:
     This module implements physical typing.
*)

open Xquery_common_ast
type code_type_context

(* Default code type context *)

val default_code_type_context  : code_type_context

(************)
(* Variable *)
(************)

val add_variable_type : code_type_context -> cvname -> Xquery_physical_type_ast.physical_xml_type -> code_type_context
val get_variable_type : code_type_context -> cvname -> Xquery_physical_type_ast.physical_xml_type


(**********************************)
(* Input of dependant expressions *)
(**********************************)

val add_input_type : code_type_context -> Xquery_physical_type_ast.physical_tuple_type -> code_type_context
val get_input_type : code_type_context -> Xquery_physical_type_ast.physical_tuple_type

