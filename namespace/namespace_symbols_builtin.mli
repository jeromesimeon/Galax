(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_symbols_builtin.mli,v 1.5 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Namespace_builtin
   Description:
     This module contains built-in names.
*)
      
open Namespace_symbols

(* Note:
     To ensure consistency, *all* other modules should use those names
     and never define their own names locally.
*)


(* Ur types *)

val xs_anySimpleType : symbol
val xs_anyType       : symbol

(* Primitive types *)
val xs_string  	    : symbol
val xs_boolean 	    : symbol
val xs_decimal 	    : symbol
val xs_float   	    : symbol
val xs_double  	    : symbol
val xs_duration     : symbol
val xs_dateTime     : symbol
val xs_time 	    : symbol
val xs_date 	    : symbol
val xs_gYearMonth   : symbol
val xs_gYear        : symbol
val xs_gMonthDay    : symbol
val xs_gDay   	    : symbol
val xs_gMonth 	    : symbol
val xs_hexBinary    : symbol
val xs_base64Binary : symbol
val xs_anyURI       : symbol
val xs_QName        : symbol
val xs_NOTATION     : symbol

(* Derived types *)
val xs_normalizedString   : symbol
val xs_token              : symbol
val xs_language           : symbol
val xs_NMTOKEN            : symbol
val xs_NMTOKENS           : symbol
val xs_Name               : symbol
val xs_NCName             : symbol
val xs_ID                 : symbol
val xs_IDREF              : symbol
val xs_IDREFS             : symbol
val xs_ENTITY             : symbol
val xs_ENTITIES           : symbol
val xs_integer            : symbol
val xs_nonPositiveInteger : symbol
val xs_negativeInteger    : symbol
val xs_long               : symbol
val xs_int                : symbol
val xs_short              : symbol
val xs_byte               : symbol
val xs_nonNegativeInteger : symbol
val xs_unsignedLong       : symbol
val xs_unsignedInt        : symbol
val xs_unsignedShort      : symbol
val xs_unsignedByte       : symbol
val xs_positiveInteger    : symbol

(* XQuery xs: types *)

val xs_dayTimeDuration   : symbol
val xs_yearMonthDuration : symbol
val xs_untypedAtomic     : symbol
val xs_untyped           : symbol
val xs_anyAtomicType     : symbol

(*************)
(* XML names *)
(*************)

val xml_lang : symbol
val xml_base : symbol
val xml_id   : symbol

val xmlns_prefix : prefix_symbol
val xmlns_uri    : uri_symbol
val xmlns_local  : ncname_symbol


(********************)
(* XML Schema names *)
(********************)

(* xsi: attributes *)

val xsi_nil            		  : symbol
val xsi_type           		  : symbol
val xsi_schemaLocation 		  : symbol
val xsi_noNamespaceSchemaLocation : symbol


(****************)
(* XQuery names *)
(****************)

(* wildcards *)

val wild_symbol : symbol


