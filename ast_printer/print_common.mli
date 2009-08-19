(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: print_common.mli,v 1.9 2007/02/01 22:08:45 simeon Exp $ *)

(* Module Print_common
   Description:
     This module implements pretty-printing for some common parts of
     the ASTs.
*)

open Xquery_common_ast


(**************)
(* Occurrence *)
(**************)

val print_occurence     : Format.formatter -> (Occurrence.occurs * Occurrence.occurs) option -> unit


(************************)
(* Names and namesapces *)
(************************)

(* Unresolved QNames *)

val print_uqname        : Format.formatter -> Namespace_names.uqname -> unit

(* Resolved QNames *)

val print_rqname        : Format.formatter -> Namespace_names.rqname -> unit
val print_uri_rqname    : Format.formatter -> Namespace_names.rqname -> unit


(***********)
(* Symbols *)
(***********)

val print_symbol  : Format.formatter -> Namespace_symbols.symbol -> unit


(*********)
(* Types *)
(*********)

val print_nillable : Format.formatter -> Xquery_common_ast.nillable -> unit
val print_mixed    : Format.formatter -> Xquery_common_ast.mixed -> unit


(***************************)
(* Atomic types and values *)
(***************************)

(* Atomic types *)

val print_literal  : Format.formatter -> Xquery_common_ast.literal -> unit
val string_of_proto_value : Datatypes.atomic_type -> string
val print_proto_value : Format.formatter -> Datatypes.atomic_type -> unit
val print_atomic_type : Format.formatter -> Datatypes.atomic_type -> unit

(***************************)
(* Common XPath structures *)
(***************************)

(* Binary operators *)

val print_bop           : Format.formatter -> Xquery_common_ast.binop   -> unit
val print_uop           : Format.formatter -> Xquery_common_ast.unaryop -> unit

(* XPath Axis *)

val print_axis          : Format.formatter -> Xquery_common_ast.axis -> unit


(****************************)
(* Common XQuery structures *)
(****************************)

(* Validation mode *)

val print_validation_mode : Format.formatter -> Xquery_common_ast.validation_mode -> unit

(* Sort kinds *)

val print_sortkind      : Format.formatter -> Xquery_common_ast.sortkind      -> unit
val print_emptysortkind : Format.formatter -> Xquery_common_ast.emptysortkind -> unit
val print_stablekind    : Format.formatter -> Xquery_common_ast.stablekind    -> unit

(* Should be moved *)
val string_of_axis      : Xquery_common_ast.axis -> string

val print_snap_modifier : Format.formatter -> Xquery_common_ast.snap_modifier -> unit


val updating_flag_to_string : updating_modifier -> string
