(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: subtyping_letter.mli,v 1.6 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Subtyping_letter
   Description:
     This module implements operations necessary to map item types
     into letters used in the automata for subtyping.
*)

open Namespace_symbols

open Xquery_common_ast
open Xquery_type_core_ast
open Xquery_type_core_ast_annotation
   
type actual_letter =
  | AtomicTypeLetter of int
  | ElementTypeLetter of int
  | AttributeTypeLetter of int
  | DocumentTypeLetter of int
  | PITypeLetter of int
  | TextTypeLetter
  | CommentTypeLetter

val compare_letters : actual_letter -> actual_letter -> int 

val print_letter : actual_letter -> unit

type alphabets

(* Get all element and attribute QNames used in a type *)
val get_names_from_cxtype : cxschema -> cxtype -> alphabets
val get_names_from_cxtypes : cxschema -> cxtype -> cxtype -> alphabets

(* Expands all the wildcard elements using the alphabet of
   element/attribute QNames computed above. *)

val expand_wildcards : cxtype -> alphabets -> cxtype

val letter_of_atomic :
    letter_mappings -> cxschema -> rtype_symbol -> int

val letter_of_element :
    letter_mappings -> cxschema -> (relem_symbol * rtype_symbol option * nillable) -> int

val letter_of_attribute :
    letter_mappings -> cxschema -> (relem_symbol * rtype_symbol option) -> int

