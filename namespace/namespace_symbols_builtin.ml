(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_symbols_builtin.ml,v 1.5 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Namespace_symbols_builtin
   Description:
     This module contains built-in names.
*)

open Namespace_symbols


(* Note:
     To ensure consistency, *all* other modules should use those names
     and never define their own names locally.
*)

(*************)
(* XML names *)
(*************)

let xml_lang = rtype_symbol Namespace_builtin.xml_lang
let xml_base = rtype_symbol Namespace_builtin.xml_base
let xml_id   = rtype_symbol Namespace_builtin.xml_id

let (xmlns_prefix,xmlns_uri,xmlns_local) =
  rtype_symbol (Namespace_builtin.xmlns_prefix,Namespace_builtin.xmlns_uri,"xmlns")

(********************)
(* XML Schema names *)
(********************)

(* Ur types *)

let xs_anySimpleType = rtype_symbol Namespace_builtin.xs_anySimpleType
let xs_anyType       = rtype_symbol Namespace_builtin.xs_anyType

(* Built-in types in xs: namespace *)

let xs_string  	    = rtype_symbol Namespace_builtin.xs_string
let xs_boolean 	    = rtype_symbol Namespace_builtin.xs_boolean
let xs_decimal 	    = rtype_symbol Namespace_builtin.xs_decimal
let xs_float   	    = rtype_symbol Namespace_builtin.xs_float
let xs_double  	    = rtype_symbol Namespace_builtin.xs_double
let xs_duration     = rtype_symbol Namespace_builtin.xs_duration
let xs_dateTime     = rtype_symbol Namespace_builtin.xs_dateTime
let xs_time 	    = rtype_symbol Namespace_builtin.xs_time
let xs_date 	    = rtype_symbol Namespace_builtin.xs_date
let xs_gYearMonth   = rtype_symbol Namespace_builtin.xs_gYearMonth
let xs_gYear        = rtype_symbol Namespace_builtin.xs_gYear
let xs_gMonthDay    = rtype_symbol Namespace_builtin.xs_gMonthDay
let xs_gDay   	    = rtype_symbol Namespace_builtin.xs_gDay
let xs_gMonth 	    = rtype_symbol Namespace_builtin.xs_gMonth
let xs_hexBinary    = rtype_symbol Namespace_builtin.xs_hexBinary
let xs_base64Binary = rtype_symbol Namespace_builtin.xs_base64Binary
let xs_anyURI       = rtype_symbol Namespace_builtin.xs_anyURI
let xs_QName        = rtype_symbol Namespace_builtin.xs_QName
let xs_NOTATION     = rtype_symbol Namespace_builtin.xs_NOTATION

let xs_normalizedString   = rtype_symbol Namespace_builtin.xs_normalizedString
let xs_token              = rtype_symbol Namespace_builtin.xs_token
let xs_language           = rtype_symbol Namespace_builtin.xs_language
let xs_NMTOKEN            = rtype_symbol Namespace_builtin.xs_NMTOKEN
let xs_NMTOKENS           = rtype_symbol Namespace_builtin.xs_NMTOKENS
let xs_Name               = rtype_symbol Namespace_builtin.xs_Name
let xs_NCName             = rtype_symbol Namespace_builtin.xs_NCName
let xs_ID                 = rtype_symbol Namespace_builtin.xs_ID
let xs_IDREF              = rtype_symbol Namespace_builtin.xs_IDREF
let xs_IDREFS             = rtype_symbol Namespace_builtin.xs_IDREFS
let xs_ENTITY             = rtype_symbol Namespace_builtin.xs_ENTITY
let xs_ENTITIES           = rtype_symbol Namespace_builtin.xs_ENTITIES
let xs_integer            = rtype_symbol Namespace_builtin.xs_integer
let xs_nonPositiveInteger = rtype_symbol Namespace_builtin.xs_nonPositiveInteger
let xs_negativeInteger    = rtype_symbol Namespace_builtin.xs_negativeInteger
let xs_long               = rtype_symbol Namespace_builtin.xs_long
let xs_int                = rtype_symbol Namespace_builtin.xs_int
let xs_short              = rtype_symbol Namespace_builtin.xs_short
let xs_byte               = rtype_symbol Namespace_builtin.xs_byte
let xs_nonNegativeInteger = rtype_symbol Namespace_builtin.xs_nonNegativeInteger
let xs_unsignedLong       = rtype_symbol Namespace_builtin.xs_unsignedLong
let xs_unsignedInt        = rtype_symbol Namespace_builtin.xs_unsignedInt
let xs_unsignedShort      = rtype_symbol Namespace_builtin.xs_unsignedShort
let xs_unsignedByte       = rtype_symbol Namespace_builtin.xs_unsignedByte
let xs_positiveInteger    = rtype_symbol Namespace_builtin.xs_positiveInteger


(* xsi: attributes *)

let xsi_nil            		  = rtype_symbol Namespace_builtin.xsi_nil
let xsi_type           		  = rtype_symbol Namespace_builtin.xsi_type
let xsi_schemaLocation 		  = rtype_symbol Namespace_builtin.xsi_schemaLocation
let xsi_noNamespaceSchemaLocation = rtype_symbol Namespace_builtin.xsi_noNamespaceSchemaLocation


(****************)
(* XQuery names *)
(****************)

(* wildcards *)

let wild_symbol = rtype_symbol Namespace_builtin.wild_rqname


(* XQuery xs: types *)

let xs_untyped           = rtype_symbol Namespace_builtin.xs_untyped
let xs_yearMonthDuration = rtype_symbol Namespace_builtin.xs_yearMonthDuration
let xs_dayTimeDuration   = rtype_symbol Namespace_builtin.xs_dayTimeDuration
let xs_untypedAtomic     = rtype_symbol Namespace_builtin.xs_untypedAtomic
let xs_anyAtomicType     = rtype_symbol Namespace_builtin.xs_anyAtomicType
