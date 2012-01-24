(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: subtyping_top.mli,v 1.7 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Subtyping_top
   Description:
     This module implements top-level subtyping operations used in the
     rest of the system.
*)
   

open Xquery_type_core_ast


(************************)
(* Subtyping operations *)
(************************)

(* Main subtyping function *)

val is_subtype_of : cxschema -> cxtype -> cxtype -> bool
val intersects_with : cxschema -> cxtype -> cxtype -> bool

(* Specific subtyping functions *)

(* <: numeric *)
val is_subtype_of_anynumeric    : cxschema -> cxtype -> bool

(* <: xs:string *)
val is_subtype_of_anystring     : cxschema -> cxtype -> bool

(* <: xs:anyURI *)
val is_subtype_of_anyURI        : cxschema -> cxtype -> bool

(* <: atomic *)
val is_subtype_of_anyatomic     : cxschema -> cxtype -> bool

(* <: atomic? *)
val is_subtype_of_anyatomic_optional : cxschema -> cxtype -> bool

(* <: atomic* *)
val is_subtype_of_anyatomic_sequence : cxschema -> cxtype -> bool

(* <: empty *)
val is_subtype_of_empty_sequence : cxschema -> cxtype -> bool

(* <: none *)
val is_subtype_of_empty_choice   : cxschema -> cxtype -> bool

(* <: node *)
val is_subtype_of_anynode        : cxschema -> cxtype -> bool

(* <: node* *)
val is_subtype_of_anynode_sequence  : cxschema -> cxtype -> bool

(* <: node+ *)
val is_subtype_of_anynode_plus   : cxschema -> cxtype -> bool

(* <: document *)
val is_subtype_of_document       : cxschema -> cxtype -> bool

(* <: element *)
val is_subtype_of_element        : cxschema -> cxtype -> bool

(* <: attribute *)
val is_subtype_of_attribute      : cxschema -> cxtype -> bool

(* <: comment *)
val is_subtype_of_comment        : cxschema -> cxtype -> bool

(* <: processing_instruction *)
val is_subtype_of_processing_instruction : cxschema -> cxtype -> bool

(* <: text *)
val is_subtype_of_text           : cxschema -> cxtype -> bool


