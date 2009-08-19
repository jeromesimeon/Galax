(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_util.mli,v 1.9 2007/02/01 22:08:55 simeon Exp $ *)

(* Module: Typing_util
   Description:
     This modules implements some basic operations used during static
     typing.
 *)

open Xquery_type_core_ast


(***************************)
(* Sequence type factoring *)
(***************************)

val factor_sequencetype      : Xquery_core_ast.csequencetype -> Xquery_core_ast.citemtype * Occurrence.occurs * Occurrence.occurs
val factor_asequencetype     : Xquery_algebra_ast.asequencetype -> Xquery_algebra_ast.aitemtype * Occurrence.occurs * Occurrence.occurs
val is_itemstar_sequencetype : Xquery_core_ast.csequencetype -> bool

(* Functions that just examine a type's syntactic structure when 
   full-blown subtyping is not necessary *)
val is_just_a_complex_type   : cxtype -> bool 

val least_common_promoted_type : cxschema -> cxtype list -> cxtype 

(********************)
(* Typing judgments *)
(********************)
val can_be_promoted_to_judge   : cxschema -> cxtype -> cxtype -> cxtype option 
val data_on_judge              : cxschema -> cxtype -> cxtype
val validate_element_resolves_to : cxschema -> Xquery_common_ast.validation_mode -> cxtype -> cxtype 

val expand_overloaded_arguments : 
    cxschema ->
      Xquery_type_core_ast.cxtype list ->
	Xquery_type_core_ast.cxtype list list * Occurrence.occurs * Occurrence.occurs

val expand_first_overloaded_argument : 
    cxschema ->
      Xquery_type_core_ast.cxtype list ->
	Xquery_type_core_ast.cxtype list * cxtype option * Occurrence.occurs * Occurrence.occurs
