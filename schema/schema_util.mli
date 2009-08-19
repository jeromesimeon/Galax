(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_util.mli,v 1.28 2007/09/25 15:12:43 mff Exp $ *)

(* Module: Schema_util
   Description:
     This modules implements some useful basic operations on schemas.
 *)

open Namespace_symbols

open Xquery_common_ast
open Xquery_type_core_ast


(* Empty content *)
val cxtype_empty        : cxtype
val cxtype_none         : cxtype

(*************)
(* Factoring *)
(*************)

val factor_with_units    : cxtype -> cxtype list * Occurrence.occurs * Occurrence.occurs
val factor    		 : cxtype -> cxtype      * Occurrence.occurs * Occurrence.occurs
val defactor  		 : cxtype * Occurrence.occurs * Occurrence.occurs -> cxtype
(* Recursive factoring in both horizontal and vertical dimensions *)
(* val recfactor            : cxschema -> cxtype -> cxtype  *)

(******************)
(* Some utilities *)
(******************)

val is_xs_anytype    : rtype_symbol -> bool
val is_simple_cxtype : cxtype -> bool
val is_really_empty_cxtype : cxtype -> bool

val make_sequence_cxtypes   : cxtype -> cxtype -> cxtype
val make_builtin_sequence_cxtypes   : cxtype -> cxtype -> cxtype
val make_interleave_cxtypes : cxtype -> cxtype -> cxtype
val make_builtin_interleave_cxtypes : cxtype -> cxtype -> cxtype
val make_choice_cxtypes     : cxtype -> cxtype -> cxtype
val make_builtin_choice_cxtypes     : cxtype -> cxtype -> cxtype

val make_atomic_type       : cxschema ->  Namespace_symbols_util.SQNameHashtbl.key -> cxtype
val make_builtin_atomic_type : rtype_symbol -> cxtype

val make_optional_type   : cxtype -> cxtype
val make_builtin_optional_type   : cxtype -> cxtype
val make_zeroormore_type : cxtype -> cxtype
val make_builtin_zeroormore_type : cxtype -> cxtype
val make_oneormore_type  : cxtype -> cxtype
val make_builtin_oneormore_type  : cxtype -> cxtype
val make_builtin_opt_attribute_ref : Namespace_symbols.rattr_symbol -> Xquery_type_core_ast.cxtype

val mixed_content       : cxtype
val text_content        : cxtype

val separate_attributes_from_content : cxtype -> cxtype * cxtype

val is_built_in_atomic_type  : cxschema -> rtype_symbol -> bool

val list_of_choice : cxtype -> cxtype list
val choice_of_list : cxtype list -> cxtype

(***********************)
(* Content model kinds *)
(***********************)

type simple_kind =
  | AtomicKind of rtype_symbol
  | UnionKind of rtype_symbol list
  | ListKind of rtype_symbol list  (* It can be a list in case this is a list over a union *)

type complex_kind = cxtype

type content_kind =
  | ComplexKind of complex_kind
  | SimpleKind of simple_kind


val extends_attribute_content : cattribute_content -> cattribute_content -> cattribute_content
val extends_element_content   : cxtype -> cxtype -> cxtype

