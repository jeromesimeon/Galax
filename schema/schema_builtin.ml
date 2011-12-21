(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_builtin.ml,v 1.30 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_builtin
   Description:
     This module implements all the built-in XML Schema types.
*)

open Error

open Namespace_symbols_builtin

open Datatypes
open Datatypes_util

open Xquery_common_ast
open Xquery_core_ast
open Xquery_type_core_ast
open Xquery_type_core_ast_util

open Schema_util

(*************)
(* Utilities *)
(*************)


(* Short-cut to build an atomic simple types *)

let make_builtin_atomic_type_decl aname bname ctypeopt =  
  fmkctype_decl aname (bname, None, CAtomicTypeRestriction) ctypeopt

(* Short-cut to build a list simple type *)

let make_builtin_list_type aname bname =
    fmkctype_decl aname (xs_anySimpleType, None, CSimpleTypeList bname) None

(****************************)
(* Built-in primitive types *)
(****************************)


let cxtype_string     	= make_builtin_atomic_type xs_string
let xs_string_decl      = make_builtin_atomic_type_decl xs_string xs_anyAtomicType (Some cxtype_string)

let cxtype_boolean    	= make_builtin_atomic_type xs_boolean
let xs_boolean_decl     = make_builtin_atomic_type_decl xs_boolean xs_anyAtomicType (Some cxtype_boolean)

let cxtype_decimal    	= make_builtin_atomic_type xs_decimal
let xs_decimal_decl     = make_builtin_atomic_type_decl xs_decimal xs_anyAtomicType (Some cxtype_decimal)

let cxtype_float      	= make_builtin_atomic_type xs_float
let xs_float_decl       = make_builtin_atomic_type_decl xs_float xs_anyAtomicType (Some cxtype_float)

let cxtype_double     	= make_builtin_atomic_type xs_double
let xs_double_decl      = make_builtin_atomic_type_decl xs_double xs_anyAtomicType (Some cxtype_double)

let cxtype_duration   	= make_builtin_atomic_type xs_duration 
let xs_duration_decl    = make_builtin_atomic_type_decl xs_duration xs_anyAtomicType (Some cxtype_duration)

let cxtype_dateTime   	= make_builtin_atomic_type xs_dateTime
let xs_dateTime_decl    = make_builtin_atomic_type_decl xs_dateTime xs_anyAtomicType (Some cxtype_dateTime)

let cxtype_time       	= make_builtin_atomic_type xs_time
let xs_time_decl        = make_builtin_atomic_type_decl xs_time xs_anyAtomicType (Some cxtype_time)

let cxtype_date       	= make_builtin_atomic_type xs_date
let xs_date_decl        = make_builtin_atomic_type_decl xs_date xs_anyAtomicType (Some cxtype_date)

let cxtype_gYearMonth 	= make_builtin_atomic_type xs_gYearMonth
let xs_gYearMonth_decl  = make_builtin_atomic_type_decl xs_gYearMonth xs_anyAtomicType (Some cxtype_gYearMonth)

let cxtype_gYear      	= make_builtin_atomic_type xs_gYear
let xs_gYear_decl       = make_builtin_atomic_type_decl xs_gYear xs_anyAtomicType (Some cxtype_gYear)

let cxtype_gMonthDay  	= make_builtin_atomic_type xs_gMonthDay 
let xs_gMonthDay_decl   = make_builtin_atomic_type_decl xs_gMonthDay xs_anyAtomicType (Some cxtype_gMonthDay)

let cxtype_gDay       	= make_builtin_atomic_type xs_gDay
let xs_gDay_decl        = make_builtin_atomic_type_decl xs_gDay xs_anyAtomicType (Some cxtype_gDay)

let cxtype_gMonth     	= make_builtin_atomic_type xs_gMonth
let xs_gMonth_decl      = make_builtin_atomic_type_decl xs_gMonth xs_anyAtomicType (Some cxtype_gMonth)

let cxtype_hexBinary  	= make_builtin_atomic_type xs_hexBinary
let xs_hexBinary_decl   = make_builtin_atomic_type_decl xs_hexBinary xs_anyAtomicType (Some cxtype_hexBinary)

let cxtype_base64Binary  = make_builtin_atomic_type xs_base64Binary
let xs_base64Binary_decl = make_builtin_atomic_type_decl xs_base64Binary xs_anyAtomicType (Some cxtype_base64Binary)

let cxtype_anyURI   	= make_builtin_atomic_type xs_anyURI
let xs_anyURI_decl    	= make_builtin_atomic_type_decl xs_anyURI xs_anyAtomicType (Some cxtype_anyURI)

let cxtype_QName    	= make_builtin_atomic_type xs_QName
let xs_QName_decl    	= make_builtin_atomic_type_decl xs_QName xs_anyAtomicType (Some cxtype_QName)

let cxtype_NOTATION 	= make_builtin_atomic_type xs_NOTATION
let xs_NOTATION_decl   	= make_builtin_atomic_type_decl xs_NOTATION xs_anyAtomicType (Some cxtype_NOTATION)

let cxtype_untypedAtomic = make_builtin_atomic_type xs_untypedAtomic
let xs_untypedAtomic_decl = make_builtin_atomic_type_decl xs_untypedAtomic xs_anyAtomicType (Some cxtype_untypedAtomic)

(**************************)
(* Built-in derived types *)
(**************************)

let xs_normalizedString_decl   = make_builtin_atomic_type_decl xs_normalizedString xs_string  None
let xs_token_decl              = make_builtin_atomic_type_decl xs_token xs_normalizedString None
let xs_language_decl           = make_builtin_atomic_type_decl xs_language xs_token None
let xs_NMTOKEN_decl            = make_builtin_atomic_type_decl xs_NMTOKEN xs_token None
let xs_NMTOKENS_decl           = make_builtin_list_type xs_NMTOKENS xs_NMTOKEN  

let xs_Name_decl               = make_builtin_atomic_type_decl xs_Name xs_token None

let cxtype_NCName    	       = make_builtin_atomic_type xs_NCName
let xs_NCName_decl             = make_builtin_atomic_type_decl xs_NCName xs_Name (Some cxtype_NCName) 
let xs_ID_decl                 = make_builtin_atomic_type_decl xs_ID xs_NCName None
let xs_IDREF_decl              = make_builtin_atomic_type_decl xs_IDREF xs_NCName None
let xs_IDREFS_decl             = make_builtin_list_type xs_IDREFS xs_IDREF 
let xs_ENTITY_decl             = make_builtin_atomic_type_decl xs_ENTITY xs_NCName None
let xs_ENTITIES_decl           = make_builtin_list_type xs_ENTITIES xs_ENTITY 

let cxtype_integer     	       = make_builtin_atomic_type xs_integer
let xs_integer_decl            = make_builtin_atomic_type_decl xs_integer xs_decimal (Some cxtype_integer)

let xs_nonPositiveInteger_decl = make_builtin_atomic_type_decl xs_nonPositiveInteger xs_integer None
let xs_negativeInteger_decl    = make_builtin_atomic_type_decl xs_negativeInteger xs_nonPositiveInteger None
let xs_long_decl               = make_builtin_atomic_type_decl xs_long xs_integer None

let cxtype_int     	       = make_builtin_atomic_type xs_int
let xs_int_decl                = make_builtin_atomic_type_decl xs_int xs_long (Some cxtype_int)

let xs_short_decl              = make_builtin_atomic_type_decl xs_short xs_int None
let xs_byte_decl               = make_builtin_atomic_type_decl xs_byte xs_short None
let xs_nonNegativeInteger_decl = make_builtin_atomic_type_decl xs_nonNegativeInteger xs_integer None
let xs_unsignedLong_decl       = make_builtin_atomic_type_decl xs_unsignedLong xs_nonNegativeInteger None
let xs_unsignedInt_decl        = make_builtin_atomic_type_decl xs_unsignedInt xs_unsignedLong None
let xs_unsignedShort_decl      = make_builtin_atomic_type_decl xs_unsignedShort xs_unsignedInt None
let xs_unsignedByte_decl       = make_builtin_atomic_type_decl xs_unsignedByte xs_unsignedShort None
let xs_positiveInteger_decl    = make_builtin_atomic_type_decl xs_positiveInteger xs_nonNegativeInteger None

let cxtype_yearMonthDuration    = make_builtin_atomic_type xs_yearMonthDuration
let xs_yearMonthDuration_decl 	= make_builtin_atomic_type_decl xs_yearMonthDuration xs_duration (Some cxtype_yearMonthDuration)

let cxtype_dayTimeDuration      = make_builtin_atomic_type xs_dayTimeDuration
let xs_dayTimeDuration_decl   	= make_builtin_atomic_type_decl xs_dayTimeDuration xs_duration (Some cxtype_dayTimeDuration)


(*********************)
(* Built-in Ur types *)
(*********************)

(* xs:anyAtomicType *)

let cxtype_anyAtomic       = make_builtin_atomic_type xs_anyAtomicType
let cxtype_atomic =
  (make_builtin_choice_cxtypes cxtype_string
  (make_builtin_choice_cxtypes cxtype_boolean
  (make_builtin_choice_cxtypes cxtype_decimal
  (make_builtin_choice_cxtypes cxtype_float
  (make_builtin_choice_cxtypes cxtype_double
  (make_builtin_choice_cxtypes cxtype_duration
  (make_builtin_choice_cxtypes cxtype_dateTime
  (make_builtin_choice_cxtypes cxtype_time
  (make_builtin_choice_cxtypes cxtype_date
  (make_builtin_choice_cxtypes cxtype_gYearMonth
  (make_builtin_choice_cxtypes cxtype_gYear
  (make_builtin_choice_cxtypes cxtype_gMonthDay
  (make_builtin_choice_cxtypes cxtype_gDay
  (make_builtin_choice_cxtypes cxtype_gMonth
  (make_builtin_choice_cxtypes cxtype_hexBinary
  (make_builtin_choice_cxtypes cxtype_base64Binary
  (make_builtin_choice_cxtypes cxtype_anyURI
  (make_builtin_choice_cxtypes cxtype_QName 
  (make_builtin_choice_cxtypes cxtype_NOTATION cxtype_untypedAtomic)))))))))))))))))))
let cxtype_atomic_optional =  make_builtin_optional_type cxtype_atomic
let cxtype_atomic_star     =  make_builtin_zeroormore_type cxtype_atomic
let cxtype_atomic_plus     =  make_builtin_oneormore_type cxtype_atomic

let cxtype_double_optional   = make_builtin_optional_type cxtype_double
let cxtype_integer_optional   = make_builtin_optional_type cxtype_integer
let cxtype_string_optional   = make_builtin_optional_type cxtype_string

let xs_anyAtomicType_decl = 
  fmkctype_decl xs_anyAtomicType (xs_anySimpleType, None, CSimpleTypeUnion [xs_string; xs_boolean; xs_decimal; xs_float; xs_double; xs_duration; xs_dateTime; xs_time; xs_date; xs_gYearMonth; xs_gYear; xs_gMonthDay; xs_gDay; xs_gMonth; xs_hexBinary; xs_base64Binary; xs_anyURI; xs_QName; xs_NOTATION]) (Some cxtype_atomic)

(* xs:anySimpleType *)

let anySimpleType_decl     =  
  fmkctype_decl xs_anySimpleType (xs_anyType, None, CSimpleTypeList xs_anyAtomicType) (Some cxtype_atomic_star)

(***********************)
(* Built-in attributes *)
(***********************)

let xsi_nil_decl   	    	       = (xsi_nil,xs_boolean)
let xsi_type_decl  	    	       = (xsi_type,xs_QName)
let xsi_schemaLocation_decl 	       = (xsi_schemaLocation,xs_anyURI)
let xsi_noNamespaceSchemaLocation_decl = (xsi_noNamespaceSchemaLocation,xs_anyURI)


(***************************)
(* Built-in content models *)
(***************************)

(* Atomic content *)
let cxtype_untypedAtomic_optional = make_builtin_optional_type cxtype_untypedAtomic
let cxtype_anyAtomic_optional     = make_builtin_optional_type cxtype_anyAtomic
let cxtype_anyAtomic_star         = make_builtin_zeroormore_type  cxtype_anyAtomic
let cxtype_anyAtomic_plus         = make_builtin_oneormore_type  cxtype_anyAtomic

(* Computed QName type = (xs:string | xs:QName | xs:untypedAtomic) *)
let cxtype_computed_qname  = make_builtin_choice_cxtypes cxtype_string (make_builtin_choice_cxtypes cxtype_QName cxtype_untypedAtomic)

(* numeric() *)
let cxtype_numeric =
  make_builtin_choice_cxtypes cxtype_decimal
    (make_builtin_choice_cxtypes cxtype_float cxtype_double)

let cxtype_anystring =
  make_builtin_choice_cxtypes cxtype_anyURI
    (make_builtin_choice_cxtypes cxtype_float cxtype_string)

let cxtype_numeric_optional =
  make_builtin_optional_type cxtype_numeric

let cxtype_numeric_star =
  make_builtin_zeroormore_type cxtype_numeric

let cxtype_numeric_plus =
  make_builtin_oneormore_type cxtype_numeric

let cxtype_greater_than_types =
  make_builtin_choice_cxtypes cxtype_numeric
    (make_builtin_choice_cxtypes cxtype_string 
    (make_builtin_choice_cxtypes cxtype_boolean
    (make_builtin_choice_cxtypes cxtype_yearMonthDuration
    (make_builtin_choice_cxtypes cxtype_dayTimeDuration
    (make_builtin_choice_cxtypes cxtype_dateTime
    (make_builtin_choice_cxtypes cxtype_date cxtype_time))))))

let cxtype_attribute = fmkcxtype_builtin (CAttributeLocal (wild_symbol,xs_anySimpleType)) 
let cxtype_attribute_optional   = make_builtin_optional_type cxtype_attribute

(* See recursive declaration for xs_anyType at end of file *)
let cxtype_element   = fmkcxtype_builtin (CElementLocal (wild_symbol,NonNillable,xs_anyType))
let cxtype_element_optional   = make_builtin_optional_type cxtype_element

let cxtype_text    = fmkcxtype_builtin CText
let cxtype_text_optional = make_builtin_optional_type cxtype_text
let cxtype_pi      = fmkcxtype_builtin (CPI None)
let cxtype_pi_name s = fmkcxtype_builtin (CPI (Some s))
let cxtype_comment = fmkcxtype_builtin CComment

let cxtype_documentcontent =
  let document_content_item =
    (make_builtin_choice_cxtypes cxtype_element
    (make_builtin_choice_cxtypes cxtype_text
    (make_builtin_choice_cxtypes cxtype_pi cxtype_comment)))
  in
  make_builtin_zeroormore_type document_content_item

let cxtype_documentnode = fmkcxtype_builtin  (CDocument cxtype_documentcontent)
let cxtype_documentnode_optional = make_builtin_optional_type cxtype_documentnode
let cxtype_element_or_documentnode_optional = 
  make_builtin_optional_type (make_builtin_choice_cxtypes cxtype_element cxtype_documentnode)

let cxtype_node =
  (make_builtin_choice_cxtypes cxtype_element
  (make_builtin_choice_cxtypes cxtype_attribute
  (make_builtin_choice_cxtypes cxtype_text
  (make_builtin_choice_cxtypes cxtype_pi
  (make_builtin_choice_cxtypes cxtype_comment cxtype_documentnode)))))

let cxtype_node_optional =  make_builtin_optional_type cxtype_node
let cxtype_node_star =  make_builtin_zeroormore_type cxtype_node
let cxtype_node_plus =  make_builtin_oneormore_type cxtype_node

let cxtype_item =
  (make_builtin_choice_cxtypes cxtype_element
  (make_builtin_choice_cxtypes cxtype_attribute
  (make_builtin_choice_cxtypes cxtype_text
  (make_builtin_choice_cxtypes cxtype_pi
  (make_builtin_choice_cxtypes cxtype_comment
  (make_builtin_choice_cxtypes cxtype_documentnode cxtype_atomic))))))

let cxtype_item_optional =
  make_builtin_optional_type cxtype_item

let cxtype_item_star =
  make_builtin_zeroormore_type cxtype_item

let cxtype_item_plus =
  make_builtin_oneormore_type cxtype_item
    
(* 
  xs:anyType is a recursive type:

  define type xs:anyType restricts xs:anyType {
    attribute*,
    ( xs:anyAtomicType* | ( element | text | comment | processing-instruction )* )
  }

  Note:
     The wildcard declarations below should probably be replaced...
   - Jerome 

*)
let cxtype_attribute_content = fmkcxtype_builtin  (CBound (cxtype_attribute,Occurrence.occurs_zero,Occurrence.unbounded))

(* We have two types for element content: one that excludes attributes, which is used in the declaration of xs:anyType: *)
let element_content   =  
  make_builtin_choice_cxtypes 
    (make_builtin_zeroormore_type cxtype_anyAtomic) 
    (make_builtin_zeroormore_type 
       (make_builtin_choice_cxtypes 
	  cxtype_element 
	  (make_builtin_choice_cxtypes cxtype_text
	     (make_builtin_choice_cxtypes cxtype_pi cxtype_comment))))

(* and a second, which includes attributes for use in Typing_expr:  attribute *, (element|text|comment|processing-instruction)* *)
let cxtype_element_node_content = 
  (make_builtin_zeroormore_type 
     (make_builtin_choice_cxtypes 
	cxtype_element 
	(make_builtin_choice_cxtypes cxtype_text
	   (make_builtin_choice_cxtypes cxtype_pi cxtype_comment))))

let cxtype_element_content   =  
  make_builtin_sequence_cxtypes 
    (make_builtin_zeroormore_type cxtype_attribute) 
    cxtype_element_node_content

(* ?!HACK?! : This is a HORRID hack to get around the nasty expansion of xs:anyType *)
let cxtype_expanded_anytype = 
  (make_builtin_zeroormore_type
     (make_builtin_choice_cxtypes 
	cxtype_node
	cxtype_atomic))

let anyType_decl = 
  fmkctype_decl xs_anyType (xs_anyType, Some cxtype_attribute_content, CComplexTypeRestriction (Mixed, element_content)) (Some cxtype_expanded_anytype)
(* xs:untyped *)

let cxtype_untyped_attribute =
  fmkcxtype_builtin  (CAttributeLocal (wild_symbol,xs_untypedAtomic))
let cxtype_untyped_element   =
  fmkcxtype_builtin  (CElementLocal (wild_symbol,NonNillable,xs_untyped))

let untyped_attribute_content =
  fmkcxtype_builtin  (CBound (cxtype_untyped_attribute,Occurrence.occurs_zero,Occurrence.unbounded))

(*
  xs:untyped is a recursive type, and its content type includes comments & PIs:

  define type xs:untyped restricts xs:anyType {
    attribute of type xs:untypedAtomic*,
    ( element of type xs:untyped | text | comment | processing-instruction? )*
  }

  Here is the previous definition, which did not include comments or PIs:

    fmkcxtype_builtin  (CBound (cxtype_untyped_element,Occurrence.occurs_zero,Occurrence.unbounded))
*)
let untyped_element_content = 
  make_builtin_zeroormore_type (make_builtin_choice_cxtypes cxtype_untyped_element (make_builtin_choice_cxtypes cxtype_pi cxtype_comment))

let xs_untyped_decl = 
  fmkctype_decl xs_untyped (xs_anyType, Some untyped_attribute_content, CComplexTypeRestriction (Mixed, untyped_element_content)) None
(*****************************)
(* All built-in declarations *)
(*****************************)

let built_in_xml_schema_attr_decls =
  [ (* Attribute declarations *)
    xsi_nil_decl;
    xsi_type_decl;
    xsi_schemaLocation_decl;
    xsi_noNamespaceSchemaLocation_decl ]

let built_in_xml_schema_elem_decls =
  []

let built_in_xml_schema_type_decls =
  [ (* Ur types definitions *)
    anySimpleType_decl;
    anyType_decl;

    (* Primitive simple types definitions *)
    xs_string_decl;
    xs_boolean_decl;
    xs_decimal_decl;
    xs_float_decl;
    xs_double_decl;
    xs_duration_decl;
    xs_dateTime_decl;
    xs_time_decl;
    xs_date_decl;
    xs_gYearMonth_decl;
    xs_gYear_decl;
    xs_gMonthDay_decl;
    xs_gDay_decl;
    xs_gMonth_decl;
    xs_hexBinary_decl;
    xs_base64Binary_decl;
    xs_anyURI_decl;
    xs_QName_decl;
    xs_NOTATION_decl;

    (* Derived simple types definitions *)
    xs_normalizedString_decl;
    xs_token_decl;
    xs_language_decl;
    xs_NMTOKEN_decl;
    xs_NMTOKENS_decl;
    xs_Name_decl;
    xs_NCName_decl;
    xs_ID_decl;
    xs_IDREF_decl;
    xs_IDREFS_decl;
    xs_ENTITY_decl;
    xs_ENTITIES_decl;
    xs_integer_decl;
    xs_nonPositiveInteger_decl;
    xs_negativeInteger_decl;
    xs_long_decl;
    xs_int_decl;
    xs_short_decl;
    xs_byte_decl;
    xs_nonNegativeInteger_decl;
    xs_unsignedLong_decl;
    xs_unsignedInt_decl;
    xs_unsignedShort_decl;
    xs_unsignedByte_decl;
    xs_positiveInteger_decl;

    (* XQuery xs: types *)
    xs_anyAtomicType_decl;
    xs_untypedAtomic_decl;
    xs_untyped_decl;
    xs_yearMonthDuration_decl;
    xs_dayTimeDuration_decl;
  ]

let built_in_cxschema =
  try
    let s = 
      fmkcxschema
	built_in_xml_schema_elem_decls
	built_in_xml_schema_attr_decls
	built_in_xml_schema_type_decls
    in
(*      Print_type_core.print_cxschema Format.std_formatter s; *)
      s
  with
  | exn -> 
      begin
	eprintf_error "In Schema_builtin:" exn;
	Format.fprintf (!Conf.glx_err_formatter) "@.";
	raise exn
      end

(************************)
(* Builtin Attributes   *)
(************************)
let built_in_attributes =
  make_interleave_cxtypes
    (make_builtin_opt_attribute_ref xsi_type)
    (make_builtin_interleave_cxtypes
       (make_builtin_opt_attribute_ref xsi_nil)
       (make_builtin_interleave_cxtypes
	  (make_builtin_opt_attribute_ref xsi_schemaLocation)
	  (make_builtin_opt_attribute_ref xsi_noNamespaceSchemaLocation)))

      
