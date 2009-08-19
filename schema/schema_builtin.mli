(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_builtin.mli,v 1.18 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_builtin
   Description:
     This module implements all the built-in XML Schema types.
*)

open Xquery_common_ast
open Xquery_type_core_ast


(***************************)
(* Built-in content models *)
(***************************)

(* Atomic content *)

val cxtype_int          : cxtype
val cxtype_integer      : cxtype
val cxtype_string       : cxtype
val cxtype_boolean      : cxtype
val cxtype_decimal      : cxtype
val cxtype_float        : cxtype
val cxtype_double       : cxtype
val cxtype_duration     : cxtype
val cxtype_dateTime     : cxtype
val cxtype_time         : cxtype
val cxtype_date         : cxtype
val cxtype_gYearMonth   : cxtype
val cxtype_gYear        : cxtype
val cxtype_gMonthDay    : cxtype
val cxtype_gDay         : cxtype
val cxtype_gMonth       : cxtype
val cxtype_hexBinary    : cxtype
val cxtype_base64Binary : cxtype
val cxtype_anyURI       : cxtype
val cxtype_QName        : cxtype
val cxtype_NCName       : cxtype
val cxtype_NOTATION     : cxtype

val cxtype_yearMonthDuration : cxtype (* xs:yearMonthDuration *)
val cxtype_dayTimeDuration   : cxtype (* xs:dayTimeDuration *)

val cxtype_untypedAtomic          : cxtype  (* xs:untypedAtomic *)
val cxtype_untypedAtomic_optional : cxtype  (* xs:untypedAtomic? *)
val cxtype_anyAtomic          	  : cxtype  (* xs:anyAtomicType *)
val cxtype_anyAtomic_optional 	  : cxtype  (* xs:anyAtomicType? *)
val cxtype_anyAtomic_star     	  : cxtype  (* xs:anyAtomicType* *)
val cxtype_anyAtomic_plus     	  : cxtype  (* xs:anyAtomicType+ *)

(* Simple content *)

val cxtype_atomic          : cxtype    (* atomic() *)
val cxtype_atomic_optional : cxtype    (* atomic()? *)
val cxtype_atomic_star     : cxtype    (* atomic()* *)
val cxtype_atomic_plus     : cxtype    (* atomic()+ *)
val cxtype_numeric         : cxtype    (* numeric() *)
val cxtype_anystring       : cxtype    (* anystring() *)
val cxtype_numeric_optional: cxtype    (* numeric()? *)
val cxtype_numeric_star    : cxtype    (* numeric()* *)
val cxtype_numeric_plus    : cxtype    (* numeric()+ *)

val cxtype_greater_than_types : cxtype
 (* Types that support greater than: numeric, boolean, string,
    yearMonthDuration, dayTimeDuration, dateTime, date, time *)

(* Complex content *)

val cxtype_text            : cxtype    (* text() *)
val cxtype_text_optional   : cxtype    (* text()? *)
val cxtype_pi              : cxtype    (* processing-instruction() *)
val cxtype_pi_name         : string -> cxtype    (* processing-instruction(name) *)
val cxtype_comment         : cxtype    (* comment() *)
val cxtype_element         : cxtype    (* element() *)
val cxtype_element_optional : cxtype   (* element()? *)
val cxtype_attribute       : cxtype    (* attribute() *)
val cxtype_attribute_optional : cxtype    (* attribute()? *)
val cxtype_documentnode    : cxtype    (* document-node() *)
val cxtype_documentnode_optional    : cxtype    (* document-node()? *)
val cxtype_documentcontent : cxtype    (* (element|text|comment|processing-instruction)* *)

val cxtype_computed_qname  : cxtype    (* (xs:string | xs:QName | xs:untypedAtomic) *)
val cxtype_untyped_element : cxtype    (* element of type xs:untyped *)
val cxtype_untyped_attribute : cxtype  (* attribute of type xs:untypedAtomic *)

val cxtype_element_node_content : cxtype    (* (element|text|comment|processing-instruction)* *)
val cxtype_element_content : cxtype    (* attribute *, (element|text|comment|processing-instruction)* *)
val cxtype_element_or_documentnode_optional : cxtype   (* (element() | documentnode()) ? *)

val cxtype_node            : cxtype    (* node() *)
val cxtype_node_optional   : cxtype    (* node()? *)
val cxtype_node_star       : cxtype    (* node()* *)
val cxtype_node_plus       : cxtype    (* node()+ *)

val cxtype_item            : cxtype    (* item() *)
val cxtype_item_optional   : cxtype    (* item()? *)
val cxtype_item_star       : cxtype    (* item()* *)
val cxtype_item_plus       : cxtype    (* item()+ *)

val cxtype_expanded_anytype : cxtype   (* expands_to xs:anyType *)

(********************************************)
(* All built-in declarations and attributes *)
(********************************************)

val built_in_xml_schema_elem_decls : celem_declaration list
val built_in_xml_schema_attr_decls : cattr_declaration list
val built_in_xml_schema_type_decls : ctype_declaration list

val built_in_cxschema : cxschema

val built_in_attributes : cxtype

(* val lookup_builtin_cxtype_expression : cxtype -> Occurrence.occurrence_indicator option -> cxtype *)
