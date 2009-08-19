(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_judge.mli,v 1.21 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_judge
   Description:
     This modules implements basic judgments on the type system.
 *)

open Namespace_symbols

open Xquery_common_ast
open Xquery_type_core_ast


(* Structural equivalence of two types ignoring File locations *)
val compare_cxtypes  : cxschema -> cxtype -> cxtype -> int
val equal_cxtypes    : cxschema -> cxtype -> cxtype -> bool 

(*
  is_syntactic_subtype_of raises Query(Unknown _)) if the two types
  cannot be compared syntactically.
*)
val is_syntactic_subtype_of : cxschema -> cxtype -> cxtype -> bool

(****************)
(* FS judgments *)
(****************)

(*
    8.1 Judgments for accessing types
        8.1.2 Substitutes for
        8.1.6 Mixed content
        8.1.8 Builtin attributes
*)

(* [FS 8.1.1] Derives from *)
val derives_from          : cxschema -> rtype_symbol -> rtype_symbol -> bool
val directly_derives_from : cxschema -> rtype_symbol -> rtype_symbol

(* [FS 8.1.4] Element and attribute type lookup by name (Static) *)
val lookup_element   : cxschema -> relem_symbol -> relem_symbol * csubstitutes_for * nillable * rtype_symbol
val lookup_attribute : cxschema -> rattr_symbol -> rattr_symbol * rtype_symbol
val lookup_type_decl : cxschema -> rtype_symbol -> ctype_declaration
val check_declared_type : cxschema -> rtype_symbol -> unit

val lookup_element_with_substitution_group :
    cxschema -> relem_symbol -> relem_symbol -> (nillable * rtype_symbol) option

val substitutes_for : cxschema -> relem_symbol -> relem_symbol -> bool

(* [FS 8.1.4] Element and attribute type lookup (Static) *)
val lookup_element_type   : cxschema -> cxtype -> (relem_symbol * nillable * rtype_symbol)
val lookup_attribute_type : cxschema -> cxtype -> (rattr_symbol * rtype_symbol)

(********************************************)
(* Computes content models for simple types *)
(********************************************)

val build_atomic_simple_type : cxschema -> rtype_symbol -> rtype_symbol
val build_union_simple_type  : cxschema -> rtype_symbol list -> rtype_symbol list
val build_list_simple_type   : cxschema -> rtype_symbol -> rtype_symbol list

(* [FS 8.1.5] Extension *)
val extended_by_is : cxschema -> cxtype -> cxtype -> cxtype

(* [FS 8.1.7] Type adjustment *)
val adjusts_to            : cxschema -> (mixed * cxtype) -> cxtype
val adjusts_attributes_to : cxschema -> cxtype option -> cxtype

(* [FS 8.1.9] Type expansion *)
(* Mary: Check definition of expands_to, which should yield a union type *)
val expands_to           : cxschema -> (nillable * rtype_symbol) -> (cxtype * Schema_util.content_kind * mixed)
val expands_attribute_to : cxschema -> rtype_symbol -> cxtype

(* [FS 8.1.10] Union interpretation of derived types *)

(************************)
(* Additional judgments *)
(************************)

(* Does the type contain the empty sequence ? *)
val type_contains_empty : cxtype -> bool

(***************)
(* Transitions *)  
(***************)

val element_transition   : cxschema -> cxtype -> relem_symbol -> bool -> rtype_symbol option -> ((bool * rtype_symbol) * cxtype) list
val attribute_transition : cxschema -> cxtype -> rattr_symbol -> (rtype_symbol * cxtype) list

val element_transition_final : cxschema -> cxtype -> relem_symbol -> bool -> rtype_symbol option -> cxtype * rtype_symbol * mixed * cxtype * Schema_util.content_kind * bool

val attribute_transition_final : cxschema -> cxtype -> rattr_symbol -> cxtype * rtype_symbol * Schema_util.simple_kind

(*****************************************)
(* Normalizes a sequencetype into a type *)
(*****************************************)

val atomic_type_of_simple_kind : cxschema -> Schema_util.simple_kind -> cxtype
val closest_builtin_integer_type : cxschema -> rtype_symbol -> rtype_symbol
val atomic_type_of_cxtype : cxschema -> cxtype -> Datatypes.atomic_type

val atomic_type_of_typename      : cxschema -> rtype_symbol -> Datatypes.atomic_type


(* debug *)

val debug_print_types : string -> cxtype -> cxtype -> unit

