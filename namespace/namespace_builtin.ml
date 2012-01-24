(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_builtin.ml,v 1.53 2007/09/04 20:08:09 ndonose Exp $ *)

(* Module: Namespace_builtin
   Description:
     This module contains built-in names.
*)

open Namespace_names


(* Note:
     To ensure consistency, *all* other modules should use those names
     and never define their own names locally.
*)


(***********************)
(* Built-in namespaces *)
(***********************)

(* XML Schema namespaces *)

let empty_prefix = NSPrefix "empty"

let xml_prefix     = NSPrefix "xml"
let xmlns_prefix   = NSPrefix "xmlns"
let xs_prefix      = NSPrefix "xs"
let xsi_prefix     = NSPrefix "xsi"
let fn_prefix      = NSPrefix "fn"
let xqx_prefix     = NSPrefix "xqx"
let op_prefix      = NSPrefix "op"
let fs_prefix      = NSPrefix "fs"
let local_prefix   = NSPrefix "local"
let err_prefix     = NSPrefix "err"
let glx_prefix     = NSPrefix "glx"

(* string value for a prefix *)

let empty_uri 	 = NSUri (Conf.emptyns)

let xml_uri    	 = NSUri (Conf.xmlns)
let xmlns_uri    = NSUri (Conf.xmlnsns)
let xs_uri    	 = NSUri (Conf.xsns)
let xsd_uri    	 = NSUri (Conf.xsdns)
let xsi_uri   	 = NSUri (Conf.xsins)
let fn_uri    	 = NSUri (Conf.fnns)
let xqx_uri	 = NSUri (Conf.xqxns)
let op_uri    	 = NSUri (Conf.opns)
let fs_uri    	 = NSUri (Conf.fsns)
let err_uri    	 = NSUri (Conf.errns)
let local_uri  	 = NSUri (Conf.localns)
let glx_uri    	 = NSUri (Conf.glxns)

(* Wildcard namespace *)

(* Note:
     Is that a hack or is that smart ?
   - J&B
*)

let wild_ns             = (NSWildcardPrefix, NSWildcardUri)

(* Default namespaces *)

let default_element_ns  = (NSDefaultElementPrefix, empty_uri)
let default_function_ns = (NSDefaultFunctionPrefix, fn_uri)

(* Table of default namespaces *)

let default_xquery_namespaces =
  [ default_function_ns ]

(* Table of fixed, built-in namespaces *)


let galax_built_in_namespace =
  [ (glx_prefix,glx_uri) ]

let default_built_in_namespaces = [ default_element_ns ]

let xml_built_in_namespaces =
  [ default_element_ns;
    (xml_prefix, xml_uri);
    (xmlns_prefix, xmlns_uri) ]

let xml_out_built_in_namespaces =
  [ (xml_prefix, xml_uri) ]

let xml_actual_out_built_in_namespaces =
  [ default_element_ns;
    (xml_prefix, xml_uri) ]

let fixed_built_in_namespaces =
  [ (xml_prefix, xml_uri);
    (xmlns_prefix, xmlns_uri);
    (empty_prefix, empty_uri) ] (* Galax HACK! *)

let xquery_built_in_namespaces =
  wild_ns ::
  xml_built_in_namespaces @
  default_xquery_namespaces @
  [ (xs_prefix, xs_uri);
    (xsi_prefix, xsi_uri);
    (fn_prefix, fn_uri);
    (op_prefix, op_uri);
    (fs_prefix, fs_uri);
    (local_prefix, local_uri); ]

let ext_xquery_built_in_namespaces =
  xml_built_in_namespaces @
  default_xquery_namespaces @
  [ (xs_prefix, xs_uri);
    (xsi_prefix, xsi_uri);
    (fn_prefix, fn_uri);
    (op_prefix, op_uri);
    (fs_prefix, fs_uri);
    (local_prefix, local_uri); ]

(* Table of built-in namespaces *)

let all_built_in_namespaces =
  wild_ns ::
  xml_built_in_namespaces @
  default_xquery_namespaces @
  galax_built_in_namespace @
  [ (empty_prefix, empty_uri); (* Galax HACK! *)
    (xs_prefix, xs_uri);
    (xsi_prefix, xsi_uri);
    (fn_prefix, fn_uri);
    (op_prefix, op_uri);
    (fs_prefix, fs_uri);
    (err_prefix, err_uri);
    (local_prefix, local_uri) ]

let built_in_uris =
  List.map snd all_built_in_namespaces

let is_built_in_namespace prefix_uri =
  List.exists (fun x -> x = prefix_uri) all_built_in_namespaces

let is_xquery_built_in_namespace prefix_uri =
  List.exists (fun x -> x = prefix_uri) xquery_built_in_namespaces

let is_xml_built_in_namespace prefix_uri =
  List.exists (fun x -> x = prefix_uri) xml_built_in_namespaces

let is_xml_out_built_in_namespace prefix_uri =
  List.exists (fun x -> x = prefix_uri) xml_out_built_in_namespaces

let is_xml_actual_out_built_in_namespace prefix_uri =
  List.exists (fun x -> x = prefix_uri) xml_actual_out_built_in_namespaces

let is_right_fixed_namespace (prefix,uri) =
  (not(prefix = xml_prefix) &&
   not(prefix = xmlns_prefix) &&
   not(uri = xml_uri) &&
   not(uri = xmlns_uri)) ||
   (List.exists (fun x -> x = (prefix,uri)) fixed_built_in_namespaces)

let is_predefined_namespace uri = 
  (List.exists (fun x -> x = uri) built_in_uris) 

(*************)
(* XML names *)
(*************)

let empty_ID    = (empty_prefix, empty_uri, "ID")
let empty_IDREF = (empty_prefix, empty_uri, "IDREF")

let xml_lang = (xml_prefix,xml_uri,"lang")
let xml_base = (xml_prefix,xml_uri,"base") 
let xml_id   = (xml_prefix,xml_uri,"id") 


(********************)
(* XML Schema names *)
(********************)

(* Ur types *)

let uxs_anyType      = (xs_prefix,"anyType")

let xs_anySimpleType = (xs_prefix,xs_uri,"anySimpleType")
let xs_anyType 	     = (xs_prefix,xs_uri,"anyType")

(* xsi: attributes *)

let xsi_nil            		  = (xsi_prefix,xsi_uri,"nil")
let xsi_type           		  = (xsi_prefix,xsi_uri,"type")
let xsi_schemaLocation 		  = (xsi_prefix,xsi_uri,"schemaLocation")
let xsi_noNamespaceSchemaLocation = (xsi_prefix,xsi_uri,"noNamespaceSchemaLocation")

(* Built-in types in xs: namespace *)

let uxs_string       = (xs_prefix, "string")
let uxs_boolean      = (xs_prefix, "boolean")
let uxs_decimal      = (xs_prefix, "decimal")
let uxs_float        = (xs_prefix, "float")
let uxs_double       = (xs_prefix, "double")
let uxs_duration     = (xs_prefix, "duration")
let uxs_dateTime     = (xs_prefix, "dateTime")
let uxs_time 	     = (xs_prefix, "time")
let uxs_date 	     = (xs_prefix, "date")
let uxs_gYearMonth   = (xs_prefix, "gYearMonth")
let uxs_gYear        = (xs_prefix, "gYear")
let uxs_gMonthDay    = (xs_prefix, "gMonthDay")
let uxs_gDay   	     = (xs_prefix, "gDay")
let uxs_gMonth 	     = (xs_prefix, "gMonth")
let uxs_hexBinary    = (xs_prefix, "hexBinary")
let uxs_base64Binary = (xs_prefix, "base64Binary")
let uxs_anyURI       = (xs_prefix, "anyURI")
let uxs_QName        = (xs_prefix, "QName")
let uxs_NOTATION     = (xs_prefix, "NOTATION")

(* Primitive types *)
let xs_string  	    = (xs_prefix, xs_uri, "string")
let xs_boolean 	    = (xs_prefix, xs_uri, "boolean")
let xs_decimal 	    = (xs_prefix, xs_uri, "decimal")
let xs_float   	    = (xs_prefix, xs_uri, "float")
let xs_double  	    = (xs_prefix, xs_uri, "double")
let xs_duration     = (xs_prefix, xs_uri, "duration")
let xs_dateTime     = (xs_prefix, xs_uri, "dateTime")
let xs_time 	    = (xs_prefix, xs_uri, "time")
let xs_date 	    = (xs_prefix, xs_uri, "date")
let xs_gYearMonth   = (xs_prefix, xs_uri, "gYearMonth")
let xs_gYear        = (xs_prefix, xs_uri, "gYear")
let xs_gMonthDay    = (xs_prefix, xs_uri, "gMonthDay")
let xs_gDay   	    = (xs_prefix, xs_uri, "gDay")
let xs_gMonth 	    = (xs_prefix, xs_uri, "gMonth")
let xs_hexBinary    = (xs_prefix, xs_uri, "hexBinary")
let xs_base64Binary = (xs_prefix, xs_uri, "base64Binary")
let xs_anyURI       = (xs_prefix, xs_uri, "anyURI")
let xs_QName        = (xs_prefix, xs_uri, "QName")
let xs_NOTATION     = (xs_prefix, xs_uri, "NOTATION")

(* Derived types *)
let uxs_normalizedString   = (xs_prefix,"normalizedString")
let uxs_token              = (xs_prefix,"token")
let uxs_language           = (xs_prefix,"language")
let uxs_NMTOKEN            = (xs_prefix,"NMTOKEN")
let uxs_NMTOKENS           = (xs_prefix,"NMTOKENS")
let uxs_Name               = (xs_prefix,"Name")
let uxs_NCName             = (xs_prefix,"NCName")
let uxs_ID                 = (xs_prefix,"ID")
let uxs_IDREF              = (xs_prefix,"IDREF")
let uxs_IDREFS             = (xs_prefix,"IDREFS")
let uxs_ENTITY             = (xs_prefix,"ENTITY")
let uxs_ENTITIES           = (xs_prefix,"ENTITIES")
let uxs_integer            = (xs_prefix,"integer")
let uxs_nonPositiveInteger = (xs_prefix,"nonPositiveInteger")
let uxs_negativeInteger    = (xs_prefix,"negativeInteger")
let uxs_long               = (xs_prefix,"long")
let uxs_int                = (xs_prefix,"int")
let uxs_short              = (xs_prefix,"short")
let uxs_byte               = (xs_prefix,"byte")
let uxs_nonNegativeInteger = (xs_prefix,"nonNegativeInteger")
let uxs_unsignedLong       = (xs_prefix,"unsignedLong")
let uxs_unsignedInt        = (xs_prefix,"unsignedInt")
let uxs_unsignedShort      = (xs_prefix,"unsignedShort")
let uxs_unsignedByte       = (xs_prefix,"unsignedByte")
let uxs_positiveInteger    = (xs_prefix,"positiveInteger")

let xs_normalizedString   = (xs_prefix,xs_uri,"normalizedString")
let xs_token              = (xs_prefix,xs_uri,"token")
let xs_language           = (xs_prefix,xs_uri,"language")
let xs_NMTOKEN            = (xs_prefix,xs_uri,"NMTOKEN")
let xs_NMTOKENS           = (xs_prefix,xs_uri,"NMTOKENS")
let xs_Name               = (xs_prefix,xs_uri,"Name")
let xs_NCName             = (xs_prefix,xs_uri,"NCName")
let xs_ID                 = (xs_prefix,xs_uri,"ID")
let xs_IDREF              = (xs_prefix,xs_uri,"IDREF")
let xs_IDREFS             = (xs_prefix,xs_uri,"IDREFS")
let xs_ENTITY             = (xs_prefix,xs_uri,"ENTITY")
let xs_ENTITIES           = (xs_prefix,xs_uri,"ENTITIES")
let xs_integer            = (xs_prefix,xs_uri,"integer")
let xs_nonPositiveInteger = (xs_prefix,xs_uri,"nonPositiveInteger")
let xs_negativeInteger    = (xs_prefix,xs_uri,"negativeInteger")
let xs_long               = (xs_prefix,xs_uri,"long")
let xs_int                = (xs_prefix,xs_uri,"int")
let xs_short              = (xs_prefix,xs_uri,"short")
let xs_byte               = (xs_prefix,xs_uri,"byte")
let xs_nonNegativeInteger = (xs_prefix,xs_uri,"nonNegativeInteger")
let xs_unsignedLong       = (xs_prefix,xs_uri,"unsignedLong")
let xs_unsignedInt        = (xs_prefix,xs_uri,"unsignedInt")
let xs_unsignedShort      = (xs_prefix,xs_uri,"unsignedShort")
let xs_unsignedByte       = (xs_prefix,xs_uri,"unsignedByte")
let xs_positiveInteger    = (xs_prefix,xs_uri,"positiveInteger")

(* Built-in types in xsd: namespace *)

(* Primitive types *)
let xsd_string       = (xs_prefix,xsd_uri,"string")
let xsd_boolean      = (xs_prefix,xsd_uri,"boolean")
let xsd_decimal      = (xs_prefix,xsd_uri,"decimal")
let xsd_float        = (xs_prefix,xsd_uri,"float")
let xsd_double       = (xs_prefix,xsd_uri,"double")
let xsd_duration     = (xs_prefix,xsd_uri,"duration")
let xsd_dateTime     = (xs_prefix,xsd_uri,"dateTime")
let xsd_time 	     = (xs_prefix,xsd_uri,"time")
let xsd_date 	     = (xs_prefix,xsd_uri,"date")
let xsd_gYearMonth   = (xs_prefix,xsd_uri,"gYearMonth")
let xsd_gYear        = (xs_prefix,xsd_uri,"gYear")
let xsd_gMonthDay    = (xs_prefix,xsd_uri,"gMonthDay")
let xsd_gDay   	     = (xs_prefix,xsd_uri,"gDay")
let xsd_gMonth 	     = (xs_prefix,xsd_uri,"gMonth")
let xsd_hexBinary    = (xs_prefix,xsd_uri,"hexBinary")
let xsd_base64Binary = (xs_prefix,xsd_uri,"base64Binary")
let xsd_anyURI       = (xs_prefix,xsd_uri,"anyURI")
let xsd_QName        = (xs_prefix,xsd_uri,"QName")
let xsd_NOTATION     = (xs_prefix,xsd_uri,"NOTATION")

(* Derived types *)
let xsd_normalizedString   = (xs_prefix, xsd_uri, "normalizedString")
let xsd_token              = (xs_prefix, xsd_uri, "token")
let xsd_language           = (xs_prefix, xsd_uri, "language")
let xsd_NMTOKEN            = (xs_prefix, xsd_uri, "NMTOKEN")
let xsd_NMTOKENS           = (xs_prefix, xsd_uri, "NMTOKENS")
let xsd_Name               = (xs_prefix, xsd_uri, "Name")
let xsd_NCName             = (xs_prefix, xsd_uri, "NCName")
let xsd_ID                 = (xs_prefix, xsd_uri, "ID")
let xsd_IDREF              = (xs_prefix, xsd_uri, "IDREF")
let xsd_IDREFS             = (xs_prefix, xsd_uri, "IDREFS")
let xsd_ENTITY             = (xs_prefix, xsd_uri, "ENTITY")
let xsd_ENTITIES           = (xs_prefix, xsd_uri, "ENTITIES")
let xsd_integer            = (xs_prefix, xsd_uri, "integer")
let xsd_nonPositiveInteger = (xs_prefix, xsd_uri, "nonPositiveInteger")
let xsd_negativeInteger    = (xs_prefix, xsd_uri, "negativeInteger")
let xsd_long               = (xs_prefix, xsd_uri, "long")
let xsd_int                = (xs_prefix, xsd_uri, "int")
let xsd_short              = (xs_prefix, xsd_uri, "short")
let xsd_byte               = (xs_prefix, xsd_uri, "byte")
let xsd_nonNegativeInteger = (xs_prefix, xsd_uri, "nonNegativeInteger")
let xsd_unsignedLong       = (xs_prefix, xsd_uri, "unsignedLong")
let xsd_unsignedInt        = (xs_prefix, xsd_uri, "unsignedInt")
let xsd_unsignedShort      = (xs_prefix, xsd_uri, "unsignedShort")
let xsd_unsignedByte       = (xs_prefix, xsd_uri, "unsignedByte")
let xsd_positiveInteger    = (xs_prefix, xsd_uri, "positiveInteger")


(****************)
(* XQuery names *)
(****************)

(* wildcards *)

let wild_uqname = (NSWildcardPrefix,"*")
let wild_rqname = (NSWildcardPrefix,NSWildcardUri,"*")

(* xs: types *)

let uxs_untypedAtomic    = (xs_prefix,"untypedAtomic")

let xs_untyped           = (xs_prefix,xs_uri,"untyped")
let xs_yearMonthDuration = (xs_prefix,xs_uri,"yearMonthDuration")
let xs_dayTimeDuration   = (xs_prefix,xs_uri,"dayTimeDuration")
let xs_untypedAtomic     = (xs_prefix,xs_uri,"untypedAtomic")
let xs_anyAtomicType     = (xs_prefix,xs_uri,"anyAtomicType")


(*************)
(* F&O names *)
(*************)

(* fn: Built-in functions *)

let fn_abs_decimal     		    = (fn_prefix,fn_uri,"abs-decimal")
let fn_abs_double      		    = (fn_prefix,fn_uri,"abs-double")
let fn_abs_float       		    = (fn_prefix,fn_uri,"abs-float")
let fn_abs             		    = (fn_prefix,fn_uri,"abs")
let fn_abs_integer     		    = (fn_prefix,fn_uri,"abs-integer")
let fn_avg_yearMonthDuration	    = (fn_prefix,fn_uri,"avg-yearMonthDuration")
let fn_avg_dayTimeDuration          = (fn_prefix,fn_uri,"avg-dayTimeDuration")
let fn_avg_decimal     		    = (fn_prefix,fn_uri,"avg-decimal")
let fn_avg_double      		    = (fn_prefix,fn_uri,"avg-double")
let fn_avg_float       		    = (fn_prefix,fn_uri,"avg-float")
let fn_avg             		    = (fn_prefix,fn_uri,"avg")
let fn_avg_integer     		    = (fn_prefix,fn_uri,"avg-integer")
let fn_base_uri        		    = (fn_prefix,fn_uri,"base-uri")
let fn_boolean         		    = (fn_prefix,fn_uri,"boolean")
let fn_ceiling_decimal 		    = (fn_prefix,fn_uri,"ceiling-decimal")
let fn_ceiling_double  		    = (fn_prefix,fn_uri,"ceiling-double")
let fn_ceiling_float   		    = (fn_prefix,fn_uri,"ceiling-float")
let fn_ceiling         		    = (fn_prefix,fn_uri,"ceiling")
let fn_ceiling_integer 		    = (fn_prefix,fn_uri,"ceiling-integer")
let fn_codepoints_to_string	    = (fn_prefix,fn_uri,"codepoints-to-string")
let fn_codepoint_equal  	    = (fn_prefix,fn_uri,"codepoint-equal")
let fn_collection      		    = (fn_prefix,fn_uri,"collection")
let fn_concat          		    = (fn_prefix,fn_uri,"concat")
let fn_contains        		    = (fn_prefix,fn_uri,"contains")
let fn_count   	       		    = (fn_prefix,fn_uri,"count")
let fn_current_datetime             = (fn_prefix,fn_uri,"current-dateTime")
let fn_current_date                 = (fn_prefix,fn_uri,"current-date")
let fn_current_time                 = (fn_prefix,fn_uri,"current-time")
let fn_data    	       		    = (fn_prefix,fn_uri,"data")
let fn_deep_equal      		    = (fn_prefix,fn_uri,"deep-equal")
let fn_distinct_values 		    = (fn_prefix,fn_uri,"distinct-values")
let fn_doc             		    = (fn_prefix,fn_uri,"doc")
let fn_doc_available   		    = (fn_prefix,fn_uri,"doc-available")
let fn_empty           		    = (fn_prefix,fn_uri,"empty")
let fn_ends_with       		    = (fn_prefix,fn_uri,"ends-with")
let fn_error           		    = (fn_prefix,fn_uri,"error")
let fn_exactly_one     		    = (fn_prefix,fn_uri,"exactly-one")
let fn_exists          		    = (fn_prefix,fn_uri,"exists")
let fn_id    	                    = (fn_prefix,fn_uri,"id")
let fn_idref    	            = (fn_prefix,fn_uri,"idref")
let fn_in_scope_prefixes	    = (fn_prefix,fn_uri,"in-scope-prefixes")
let fn_resolve_QName   		    = (fn_prefix,fn_uri,"resolve-QName")
let fn_QName    		    = (fn_prefix,fn_uri,"QName")
let fn_false           		    = (fn_prefix,fn_uri,"false")
let fn_floor_decimal   		    = (fn_prefix,fn_uri,"floor-decimal")
let fn_floor_double    		    = (fn_prefix,fn_uri,"floor-double")
let fn_floor_float     		    = (fn_prefix,fn_uri,"floor-float")
let fn_floor           		    = (fn_prefix,fn_uri,"floor")
let fn_floor_integer   		    = (fn_prefix,fn_uri,"floor-integer")
let fn_local_name_from_QName        = (fn_prefix,fn_uri,"local-name-from-QName")
let fn_prefix_from_QName            = (fn_prefix,fn_uri,"prefix-from-QName")
let fn_namespace_uri_from_QName     = (fn_prefix,fn_uri,"namespace-uri-from-QName")
let fn_namespace_uri_for_prefix     = (fn_prefix,fn_uri,"namespace-uri-for-prefix")
let fn_implicit_timezone            = (fn_prefix,fn_uri,"implicit-timezone")
let fn_static_base_uri              = (fn_prefix,fn_uri,"static-base-uri")
let fn_default_collation            = (fn_prefix,fn_uri,"default-collation")
let fn_document_uri                 = (fn_prefix,fn_uri,"document-uri")
let fn_dateTime                     = (fn_prefix,fn_uri,"dateTime")
let fn_index_of      		    = (fn_prefix,fn_uri,"index-of")
let fn_insert_before 		    = (fn_prefix,fn_uri,"insert-before")
let fn_lang          		    = (fn_prefix,fn_uri,"lang")
let fn_last          		    = (fn_prefix,fn_uri,"last")
let fn_local_name    		    = (fn_prefix,fn_uri,"local-name")
let fn_lower_case    		    = (fn_prefix,fn_uri,"lower-case")
let fn_encode_for_uri  		    = (fn_prefix,fn_uri,"encode-for-uri")
let fn_iri_to_uri  		    = (fn_prefix,fn_uri,"iri-to-uri")
let fn_escape_html_uri 		    = (fn_prefix,fn_uri,"escape-html-uri")
let fn_matches                      = (fn_prefix,fn_uri,"matches")
let fn_tokenize                     = (fn_prefix,fn_uri,"tokenize")
let fn_replace                      = (fn_prefix,fn_uri,"replace")
let fn_max_string                   = (fn_prefix,fn_uri,"max-string")
let fn_max_date	                    = (fn_prefix,fn_uri,"max-date")
let fn_max_time	                    = (fn_prefix,fn_uri,"max-time")
let fn_max_dateTime	            = (fn_prefix,fn_uri,"max-dateTime")
let fn_max_yearMonthDuration	    = (fn_prefix,fn_uri,"max-yearMonthDuration")
let fn_max_dayTimeDuration          = (fn_prefix,fn_uri,"max-dayTimeDuration")
let fn_max_decimal   		    = (fn_prefix,fn_uri,"max-decimal")
let fn_max_double    		    = (fn_prefix,fn_uri,"max-double")
let fn_max_float     		    = (fn_prefix,fn_uri,"max-float")
let fn_max           		    = (fn_prefix,fn_uri,"max")
let fn_max_integer   		    = (fn_prefix,fn_uri,"max-integer")
let fn_min_string                   = (fn_prefix,fn_uri,"min-string")
let fn_min_date	                    = (fn_prefix,fn_uri,"min-date")
let fn_min_time	                    = (fn_prefix,fn_uri,"min-time")
let fn_min_dateTime	            = (fn_prefix,fn_uri,"min-dateTime")
let fn_min_yearMonthDuration	    = (fn_prefix,fn_uri,"min-yearMonthDuration")
let fn_min_dayTimeDuration          = (fn_prefix,fn_uri,"min-dayTimeDuration")
let fn_min_decimal     		    = (fn_prefix,fn_uri,"min-decimal")
let fn_min_double      		    = (fn_prefix,fn_uri,"min-double")
let fn_min_float       		    = (fn_prefix,fn_uri,"min-float")
let fn_min             		    = (fn_prefix,fn_uri,"min")
let fn_min_integer     		    = (fn_prefix,fn_uri,"min-integer")
let fn_name    	       		    = (fn_prefix,fn_uri,"name")
let fn_number  	       		    = (fn_prefix,fn_uri,"number")
let fn_namespace_uri   		    = (fn_prefix,fn_uri,"namespace-uri")
let fn_node_name       		    = (fn_prefix,fn_uri,"node-name")
let fn_nilled       		    = (fn_prefix,fn_uri,"nilled")
let fn_normalize_space 		    = (fn_prefix,fn_uri,"normalize-space")
let fn_normalize_unicode	    = (fn_prefix,fn_uri,"normalize-unicode")
let fn_not           	       	    = (fn_prefix,fn_uri,"not")
let fn_one_or_more   	       	    = (fn_prefix,fn_uri,"one-or-more")
let fn_position      	       	    = (fn_prefix,fn_uri,"position")
let fn_remove        	       	    = (fn_prefix,fn_uri,"remove")
let fn_resolve_uri   	       	    = (fn_prefix,fn_uri,"resolve-uri")
let fn_reverse 	     	       	    = (fn_prefix,fn_uri,"reverse")
let fn_root    	     	       	    = (fn_prefix,fn_uri,"root")
let fn_round_decimal 	       	    = (fn_prefix,fn_uri,"round-decimal")
let fn_round_double  	       	    = (fn_prefix,fn_uri,"round-double")
let fn_round_float   	       	    = (fn_prefix,fn_uri,"round-float")
let fn_round_integer 	       	    = (fn_prefix,fn_uri,"round-integer")
let fn_round         	       	    = (fn_prefix,fn_uri,"round")
let fn_round_half_to_even_decimal   = (fn_prefix,fn_uri,"round-half-to-even-decimal")
let fn_round_half_to_even_double    = (fn_prefix,fn_uri,"round-half-to-even-double")
let fn_round_half_to_even_float     = (fn_prefix,fn_uri,"round-half-to-even-float")
let fn_round_half_to_even_integer   = (fn_prefix,fn_uri,"round-half-to-even-integer")
let fn_round_half_to_even      	    = (fn_prefix,fn_uri,"round-half-to-even")
let fn_sequence_node_identical 	    = (fn_prefix,fn_uri,"sequence-node-identical")
let fn_starts_with   		    = (fn_prefix,fn_uri,"starts-with")
let fn_string        		    = (fn_prefix,fn_uri,"string")
let fn_string_join   		    = (fn_prefix,fn_uri,"string-join")
let fn_string_length 		    = (fn_prefix,fn_uri,"string-length")
let fn_string_to_codepoints	    = (fn_prefix,fn_uri,"string-to-codepoints")
let fn_subsequence   		    = (fn_prefix,fn_uri,"subsequence")
let fn_substring_after  	    = (fn_prefix,fn_uri,"substring-after")
let fn_substring_before 	    = (fn_prefix,fn_uri,"substring-before")
let fn_substring   		    = (fn_prefix,fn_uri,"substring")
let fn_sum_yearMonthDuration	    = (fn_prefix,fn_uri,"sum-yearMonthDuration")
let fn_sum_dayTimeDuration          = (fn_prefix,fn_uri,"sum-dayTimeDuration")
let fn_sum_decimal 		    = (fn_prefix,fn_uri,"sum-decimal")
let fn_sum_double  		    = (fn_prefix,fn_uri,"sum-double")
let fn_sum_float   		    = (fn_prefix,fn_uri,"sum-float")
let fn_sum         		    = (fn_prefix,fn_uri,"sum")
let fn_sum_integer 		    = (fn_prefix,fn_uri,"sum-integer")
let fn_trace   	   		    = (fn_prefix,fn_uri,"trace")
let fn_translate   		    = (fn_prefix,fn_uri,"translate")
let fn_true        		    = (fn_prefix,fn_uri,"true")
let fn_unique_ID   		    = (fn_prefix,fn_uri,"unique_ID")
let fn_unordered   		    = (fn_prefix,fn_uri,"unordered")
let fn_upper_case  		    = (fn_prefix,fn_uri,"upper-case")
let fn_zero_or_one 		    = (fn_prefix,fn_uri,"zero-or-one")


(* op: F&O operators, which are implemented literaly as functions in
   Galax in order to implement the XQuery semantics. *)

let op_anyURI_equal = (op_prefix,op_uri, "anyURI-equal") 
let op_anyURI_nequal = (op_prefix,op_uri, "anyURI-nequal") 
let op_boolean_equal = (op_prefix,op_uri, "boolean-equal") 
let op_boolean_ge = (op_prefix,op_uri, "boolean-ge") 
let op_boolean_gt = (op_prefix,op_uri, "boolean-gt") 
let op_boolean_le = (op_prefix,op_uri, "boolean-le") 
let op_boolean_lt = (op_prefix,op_uri, "boolean-lt") 
let op_boolean_nequal = (op_prefix,op_uri, "boolean-nequal") 
let op_decimal_add = (op_prefix,op_uri, "decimal-add") 
let op_decimal_divide = (op_prefix,op_uri, "decimal-divide") 
let op_decimal_idivide = (op_prefix,op_uri, "decimal-idivide") 
let op_decimal_equal = (op_prefix,op_uri, "decimal-equal") 
let op_decimal_ge = (op_prefix,op_uri, "decimal-ge") 
let op_decimal_gt = (op_prefix,op_uri, "decimal-gt") 
let op_decimal_le = (op_prefix,op_uri, "decimal-le") 
let op_decimal_lt = (op_prefix,op_uri, "decimal-lt") 
let op_decimal_mod = (op_prefix,op_uri, "decimal-mod") 
let op_decimal_multiply = (op_prefix,op_uri, "decimal-multiply") 
let op_decimal_nequal = (op_prefix,op_uri, "decimal-nequal") 
let op_decimal_subtract = (op_prefix,op_uri, "decimal-subtract") 
let op_decimal_unary_minus = (op_prefix,op_uri, "decimal-unary-minus") 
let op_decimal_unary_plus = (op_prefix,op_uri, "decimal-unary-plus") 
let op_double_add = (op_prefix,op_uri, "double-add") 
let op_double_divide = (op_prefix,op_uri, "double-divide") 
let op_double_idivide = (op_prefix,op_uri, "double-idivide") 
let op_double_equal = (op_prefix,op_uri, "double-equal") 
let op_double_ge = (op_prefix,op_uri, "double-ge") 
let op_double_gt = (op_prefix,op_uri, "double-gt") 
let op_double_le = (op_prefix,op_uri, "double-le") 
let op_double_lt = (op_prefix,op_uri, "double-lt") 
let op_double_mod = (op_prefix,op_uri, "double-mod") 
let op_double_multiply = (op_prefix,op_uri, "double-multiply") 
let op_double_nequal = (op_prefix,op_uri, "double-nequal") 
let op_double_subtract = (op_prefix,op_uri, "double-subtract") 
let op_double_unary_minus = (op_prefix,op_uri, "double-unary-minus") 
let op_double_unary_plus = (op_prefix,op_uri, "double-unary-plus") 
let op_equal = (op_prefix,op_uri,"equal")
let op_equal_left_empty = (op_prefix,op_uri,"equal-left-empty")
let op_equal_right_empty = (op_prefix,op_uri,"equal-right-empty")
let op_except =  (op_prefix,op_uri, "except")
let op_float_add = (op_prefix,op_uri, "float-add") 
let op_float_divide = (op_prefix,op_uri, "float-divide") 
let op_float_idivide = (op_prefix,op_uri, "float-idivide") 
let op_float_equal = (op_prefix,op_uri, "float-equal") 
let op_float_ge = (op_prefix,op_uri, "float-ge") 
let op_float_gt = (op_prefix,op_uri, "float-gt") 
let op_float_le = (op_prefix,op_uri, "float-le") 
let op_float_lt = (op_prefix,op_uri, "float-lt") 
let op_float_mod = (op_prefix,op_uri, "float-mod") 
let op_float_multiply = (op_prefix,op_uri, "float-multiply") 
let op_float_nequal = (op_prefix,op_uri, "float-nequal") 
let op_float_subtract = (op_prefix,op_uri, "float-subtract") 
let op_float_unary_minus = (op_prefix,op_uri, "float-unary-minus") 
let op_float_unary_plus = (op_prefix,op_uri, "float-unary-plus") 
let op_ge =  (op_prefix,op_uri,"ge")
let op_ge_left_empty =  (op_prefix,op_uri,"ge-left-empty")
let op_ge_right_empty =  (op_prefix,op_uri,"ge-right-empty")
let op_gt =  (op_prefix,op_uri,"gt")
let op_gt_left_empty  =  (op_prefix,op_uri,"gt-left-empty")
let op_gt_right_empty  =  (op_prefix,op_uri,"gt-right-empty")
let op_integer_add = (op_prefix,op_uri,"integer-add")
let op_integer_divide = (op_prefix,op_uri, "integer-divide") 
let op_integer_idivide = (op_prefix,op_uri, "integer-idivide") 
let op_integer_equal = (op_prefix,op_uri, "integer-equal") 
let op_integer_ge = (op_prefix,op_uri, "integer-ge") 
let op_integer_gt = (op_prefix,op_uri, "integer-gt") 
let op_integer_le = (op_prefix,op_uri, "integer-le") 
let op_integer_lt = (op_prefix,op_uri, "integer-lt") 
let op_integer_mod = (op_prefix,op_uri, "integer-mod") 
let op_integer_multiply = (op_prefix,op_uri, "integer-multiply") 
let op_integer_nequal = (op_prefix,op_uri, "integer-nequal") 
let op_integer_subtract = (op_prefix,op_uri,"integer-subtract")
let op_integer_unary_minus = (op_prefix,op_uri, "integer-unary-minus") 
let op_integer_unary_plus = (op_prefix,op_uri, "integer-unary-plus") 
let op_intersect =  (op_prefix,op_uri, "intersect")

let op_date_equal  = (op_prefix,op_uri, "date-equal")
let op_date_nequal = (op_prefix,op_uri, "date-nequal")
let op_date_lt = (op_prefix,op_uri, "date-lt")
let op_date_le = (op_prefix,op_uri, "date-le")
let op_date_gt = (op_prefix,op_uri, "date-gt")
let op_date_ge = (op_prefix,op_uri, "date-ge")

let op_gYearMonth_equal  = (op_prefix,op_uri, "gYearMonth-equal")
let op_gYearMonth_nequal = (op_prefix,op_uri, "gYearMonth-nequal")

let op_gYear_equal  = (op_prefix,op_uri, "gYear-equal")
let op_gYear_nequal = (op_prefix,op_uri, "gYear-nequal")

let op_gMonthDay_equal  = (op_prefix,op_uri, "gMonthDay-equal")
let op_gMonthDay_nequal = (op_prefix,op_uri, "gMonthDay-nequal")

let op_gDay_equal  = (op_prefix,op_uri, "gDay-equal")
let op_gDay_nequal = (op_prefix,op_uri, "gDay-nequal")

let op_gMonth_equal  = (op_prefix,op_uri, "gMonth-equal")
let op_gMonth_nequal = (op_prefix,op_uri, "gMonth-nequal")

let op_add_yearMonthDuration_to_date = (op_prefix,op_uri, "add-yearMonthDuration-to-date")
let op_add_yearMonthDuration_to_date2 = (op_prefix,op_uri, "add-yearMonthDuration-to-date2")
let op_add_dayTimeDuration_to_date = (op_prefix,op_uri, "add-dayTimeDuration-to-date")
let op_add_dayTimeDuration_to_date2 = (op_prefix,op_uri, "add-dayTimeDuration-to-date2")
let op_subtract_yearMonthDuration_from_date = (op_prefix,op_uri, "subtract-yearMonthDuration-from-date")
let op_subtract_dayTimeDuration_from_date = (op_prefix,op_uri, "subtract-dayTimeDuration-from-date")

let op_time_equal  = (op_prefix,op_uri, "time-equal") 
let op_time_nequal  = (op_prefix,op_uri, "time-nequal")
let op_time_lt = (op_prefix,op_uri, "time-lt")
let op_time_le = (op_prefix,op_uri, "time-le")
let op_time_gt = (op_prefix,op_uri, "time-gt")
let op_time_ge = (op_prefix,op_uri, "time-ge")
let op_add_dayTimeDuration_to_time = (op_prefix,op_uri, "add-dayTimeDuration-to-time")
let op_add_dayTimeDuration_to_time2 = (op_prefix,op_uri, "add-dayTimeDuration-to-time2")
let op_subtract_dateTimes = (op_prefix,op_uri, "subtract-dateTimes")
let op_subtract_dates = (op_prefix,op_uri, "subtract-dates")
let op_subtract_times = (op_prefix,op_uri, "subtract-times")
let op_subtract_dayTimeDuration_from_time = (op_prefix,op_uri, "subtract-dayTimeDuration-from-time")

let op_dateTime_equal  = (op_prefix,op_uri, "dateTime-equal")
let op_dateTime_nequal  = (op_prefix,op_uri, "dateTime-nequal")
let op_dateTime_lt = (op_prefix,op_uri, "dateTime-lt")
let op_dateTime_le = (op_prefix,op_uri, "dateTime-le")
let op_dateTime_gt = (op_prefix,op_uri, "dateTime-gt")
let op_dateTime_ge = (op_prefix,op_uri, "dateTime-ge")
let op_add_yearMonthDuration_to_dateTime = (op_prefix,op_uri, "add-yearMonthDuration-to-dateTime")
let op_add_yearMonthDuration_to_dateTime2 = (op_prefix,op_uri, "add-yearMonthDuration-to-dateTime2")
let op_add_dayTimeDuration_to_dateTime = (op_prefix,op_uri, "add-dayTimeDuration-to-dateTime")
let op_add_dayTimeDuration_to_dateTime2 = (op_prefix,op_uri, "add-dayTimeDuration-to-dateTime2")
let op_subtract_yearMonthDuration_from_dateTime = (op_prefix,op_uri, "subtract-yearMonthDuration-from-dateTime")
let op_subtract_dayTimeDuration_from_dateTime = (op_prefix,op_uri, "subtract-dayTimeDuration-from-dateTime")

let op_yearMonthDuration_equal  = (op_prefix,op_uri, "yearMonthDuration-equal")
let op_yearMonthDuration_nequal  = (op_prefix,op_uri, "yearMonthDuration-nequal")
let op_yearMonthDuration_lt = (op_prefix,op_uri, "yearMonthDuration-lt")
let op_yearMonthDuration_le = (op_prefix,op_uri, "yearMonthDuration-le")
let op_yearMonthDuration_gt = (op_prefix,op_uri, "yearMonthDuration-gt")
let op_yearMonthDuration_ge = (op_prefix,op_uri, "yearMonthDuration-ge")
let op_add_yearMonthDurations = (op_prefix,op_uri, "add-yearMonthDurations")
let op_subtract_yearMonthDurations = (op_prefix,op_uri, "subtract-yearMonthDurations")
let op_multiply_yearMonthDuration = (op_prefix,op_uri, "multiply-yearMonthDuration")
let op_divide_yearMonthDuration = (op_prefix,op_uri, "divide-yearMonthDuration")
let op_multiply_yearMonthDuration2 = (op_prefix,op_uri, "multiply-yearMonthDuration2")
let op_divide_yearMonthDuration_by_yearMonthDuration = (op_prefix,op_uri, "divide-yearMonthDuration-by-yearMonthDuration")

let op_dayTimeDuration_equal  = (op_prefix,op_uri, "dayTimeDuration-equal")
let op_dayTimeDuration_nequal  = (op_prefix,op_uri, "dayTimeDuration-nequal")
let op_dayTimeDuration_lt = (op_prefix,op_uri, "dayTimeDuration-lt")
let op_dayTimeDuration_le = (op_prefix,op_uri, "dayTimeDuration-le")
let op_dayTimeDuration_gt = (op_prefix,op_uri, "dayTimeDuration-gt")
let op_dayTimeDuration_ge = (op_prefix,op_uri, "dayTimeDuration-ge")
let op_add_dayTimeDurations = (op_prefix,op_uri, "add-dayTimeDurations")
let op_subtract_dayTimeDurations = (op_prefix,op_uri, "subtract-dayTimeDurations")
let op_multiply_dayTimeDuration = (op_prefix,op_uri, "multiply-dayTimeDuration")
let op_multiply_dayTimeDuration2 = (op_prefix,op_uri, "multiply-dayTimeDuration2")
let op_divide_dayTimeDuration = (op_prefix,op_uri, "divide-dayTimeDuration")
let op_divide_dayTimeDuration_by_dayTimeDuration = (op_prefix,op_uri, "divide-dayTimeDuration-by-dayTimeDuration")

let op_duration_equal  = (op_prefix,op_uri, "duration-equal")
let op_duration_nequal  = (op_prefix,op_uri, "duration-nequal")

(* Date/time extraction functions *)
let fn_years_from_duration = (fn_prefix,fn_uri, "years-from-duration")
let fn_months_from_duration = (fn_prefix,fn_uri, "months-from-duration")
let fn_days_from_duration = (fn_prefix,fn_uri, "days-from-duration")
let fn_hours_from_duration = (fn_prefix,fn_uri, "hours-from-duration")
let fn_minutes_from_duration = (fn_prefix,fn_uri, "minutes-from-duration")
let fn_seconds_from_duration = (fn_prefix,fn_uri, "seconds-from-duration")
let fn_year_from_dateTime = (fn_prefix,fn_uri, "year-from-dateTime")
let fn_month_from_dateTime = (fn_prefix,fn_uri, "month-from-dateTime")
let fn_day_from_dateTime = (fn_prefix,fn_uri, "day-from-dateTime")
let fn_hours_from_dateTime = (fn_prefix,fn_uri, "hours-from-dateTime")
let fn_minutes_from_dateTime = (fn_prefix,fn_uri, "minutes-from-dateTime")
let fn_seconds_from_dateTime = (fn_prefix,fn_uri, "seconds-from-dateTime")
let fn_timezone_from_dateTime = (fn_prefix,fn_uri, "timezone-from-dateTime")
let fn_year_from_date = (fn_prefix,fn_uri, "year-from-date")
let fn_month_from_date = (fn_prefix,fn_uri, "month-from-date")
let fn_day_from_date = (fn_prefix,fn_uri, "day-from-date")
let fn_timezone_from_date = (fn_prefix,fn_uri, "timezone-from-date")
let fn_hours_from_time = (fn_prefix,fn_uri, "hours-from-time")
let fn_minutes_from_time = (fn_prefix,fn_uri, "minutes-from-time")
let fn_seconds_from_time = (fn_prefix,fn_uri, "seconds-from-time")
let fn_timezone_from_time = (fn_prefix,fn_uri, "timezone-from-time")

let fn_adjust_time_to_timezone = (fn_prefix,fn_uri, "adjust-time-to-timezone")
let fn_adjust_date_to_timezone = (fn_prefix,fn_uri, "adjust-date-to-timezone")
let fn_adjust_dateTime_to_timezone = (fn_prefix,fn_uri, "adjust-dateTime-to-timezone")

let fn_adjust_time_to_timezone_unary = (fn_prefix,fn_uri, "adjust-time-to-timezone-unary")
let fn_adjust_date_to_timezone_unary = (fn_prefix,fn_uri, "adjust-date-to-timezone-unary")
let fn_adjust_dateTime_to_timezone_unary = (fn_prefix,fn_uri, "adjust-dateTime-to-timezone-unary")

let op_le =  (op_prefix,op_uri,"le")
let op_le_left_empty =  (op_prefix,op_uri,"le-left-empty")
let op_le_right_empty =  (op_prefix,op_uri,"le-right-empty")
let op_lt =  (op_prefix,op_uri,"lt")
let op_lt_left_empty =  (op_prefix,op_uri,"lt-left-empty")
let op_lt_right_empty =  (op_prefix,op_uri,"lt-right-empty")
let op_nequal =  (op_prefix,op_uri,"nequal")
let op_nequal_left_empty =  (op_prefix,op_uri,"nequal-left-empty")
let op_nequal_right_empty =  (op_prefix,op_uri,"nequal-right-empty")
let op_node_after =  (op_prefix,op_uri,"node-after")
let op_node_before =  (op_prefix,op_uri,"node-before")
let op_is_same_node =  (op_prefix,op_uri,"is-same-node")
let op_numeric_add =  (op_prefix,op_uri, "numeric-add")
let op_numeric_divide =  (op_prefix,op_uri, "numeric-divide")
let op_numeric_idivide = (op_prefix,op_uri, "numeric-idivide")
let op_numeric_mod =  (op_prefix,op_uri, "numeric-mod")
let op_numeric_multiply =  (op_prefix,op_uri, "numeric-multiply")
let op_numeric_subtract =  (op_prefix,op_uri, "numeric-subtract")
let op_numeric_unary_minus = (op_prefix,op_uri, "numeric-unary-minus")
let op_numeric_unary_plus = (op_prefix,op_uri, "numeric-unary-plus")
let op_QName_equal = (op_prefix,op_uri, "QName-equal") 
let op_QName_nequal = (op_prefix,op_uri, "QName-nequal") 
let op_string_equal = (op_prefix,op_uri, "string-equal") 
let fn_compare = (fn_prefix,fn_uri, "compare")
let op_string_ge = (op_prefix,op_uri, "string-ge") 
let op_string_gt = (op_prefix,op_uri, "string-gt") 
let op_string_le = (op_prefix,op_uri, "string-le") 
let op_string_lt = (op_prefix,op_uri, "string-lt") 
let op_string_nequal = (op_prefix,op_uri, "string-nequal") 
let op_to = (op_prefix,op_uri,"to")
let op_union =  (op_prefix,op_uri, "union")
let op_hexBinary_equal = (op_prefix,op_uri, "hexBinary-equal") 
let op_base64Binary_equal = (op_prefix,op_uri, "base64Binary-equal") 
let op_hexBinary_nequal = (op_prefix,op_uri, "hexBinary-nequal") 
let op_base64Binary_nequal = (op_prefix,op_uri, "base64Binary-nequal") 

(* Default error *)
let err_default_error =  (err_prefix,err_uri, "FOER0000")


(**************************)
(* Formal Semantics names *)
(**************************)

(* Note:
     fs: prefixed functions are Formal Semantics functions used in
     implementing the XQuery semantics.
 *)


let fs_convert_simple_operand 	 = (fs_prefix,fs_uri,"convert-simple-operand")
let fs_distinct_docorder      	 = (fs_prefix,fs_uri,"distinct-docorder")
let fs_distinct_docorder_or_atomic_sequence  = (fs_prefix,fs_uri,"distinct-docorder-or-atomic-sequence")
let fs_distinct 	      	 = (fs_prefix,fs_uri,"distinct")
let fs_distinct_or_atomic_sequence = (fs_prefix,fs_uri,"distinct-or-atomic-sequence")
let fs_docorder 	      	 = (fs_prefix,fs_uri,"docorder")
let fs_docorder_or_atomic_sequence = (fs_prefix,fs_uri,"docorder-or-atomic-sequence")
let fs_first                  	 = (fs_prefix,fs_uri,"first-item")
let fs_item_sequence_to_untypedAtomic
                                 = (fs_prefix,fs_uri,"item-sequence-to-untypedAtomic")
let fs_item_sequence_to_untypedAtomic_optional
                                 = (fs_prefix,fs_uri,"item-sequence-to-untypedAtomic-optional")
let fs_item_sequence_to_node_sequence
                                 = (fs_prefix,fs_uri,"item-sequence-to-node-sequence")
let fs_last_fn       	  	 = (fs_prefix,fs_uri, "last-item")
let fs_node_sequence 	  	 = (fs_prefix,fs_uri,"node-sequence")
let fs_node_sequence_or_atomic_sequence = (fs_prefix,fs_uri,"node-sequence-or-atomic-sequence")
let fs_promote_to_numeric 	 = (fs_prefix,fs_uri,"promote-to-numeric")
let fs_promote_to_anystring 	 = (fs_prefix,fs_uri,"promote-to-anystring")
let fs_unsafe_promote_to_numeric = (fs_prefix,fs_uri,"unsafe-promote-to-numeric")
let fs_untyped_to_any     	 = (fs_prefix,fs_uri,"untyped-to-any")
let fs_untyped_to_double  	 = (fs_prefix,fs_uri,"untyped-to-double")
let fs_untyped_to_integer 	 = (fs_prefix,fs_uri,"untyped-to-integer")
let fs_untyped_to_string  	 = (fs_prefix,fs_uri,"untyped-to-string")
let fs_subsequence  	         = (fs_prefix,fs_uri,"subsequence")

(*****************)
(* XQueryX Names *)
(*****************)

let xqx_xquery = (xqx_prefix,xqx_uri,"xquery")


(***************)
(* Galax names *)
(***************)

(* glx: Galax additional built-in functions. *)

let glx_acos  		  = (glx_prefix,glx_uri,"acos")
let glx_asin  		  = (glx_prefix,glx_uri,"asin")
let glx_atan2 		  = (glx_prefix,glx_uri,"atan2")
let glx_atan  		  = (glx_prefix,glx_uri,"atan")
let glx_cos   		  = (glx_prefix,glx_uri,"cos")
let glx_cosh  		  = (glx_prefix,glx_uri,"cosh")
let glx_distinct_docorder = (glx_prefix,glx_uri,"distinct-docorder")
let glx_docorder      	  = (glx_prefix,glx_uri,"docorder")
let glx_except_values 	  = (glx_prefix,glx_uri,"except-values")
let glx_exp 	      	  = (glx_prefix,glx_uri,"exp")
let glx_exponent      	  = (glx_prefix,glx_uri,"exponent")
let glx_error      	      = (glx_prefix,glx_uri,"error")
let glx_file_exists   	  = (glx_prefix,glx_uri,"file-exists")
let glx_deep_distinct  	  = (glx_prefix,glx_uri,"deep-distinct")
let glx_get_lang          = (glx_prefix,glx_uri,"get-lang")
let glx_intersect_values  = (glx_prefix,glx_uri,"intersect-values")
let glx_keyref 	       	  = (glx_prefix,glx_uri,"keyref")
let glx_log10  	       	  = (glx_prefix,glx_uri,"log10")
let glx_log    	       	  = (glx_prefix,glx_uri,"log")
let glx_print_item_err 	  = (glx_prefix,glx_uri,"print-item-err")
let glx_print_item        = (glx_prefix,glx_uri,"print-item")
let glx_string_of_item    = (glx_prefix,glx_uri,"string-of-item")
let glx_doc_of_string     = (glx_prefix,glx_uri,"doc-of-string")
let glx_print_string_err  = (glx_prefix,glx_uri,"print-string-err")
let glx_print_string  	  = (glx_prefix,glx_uri,"print-string")
let glx_save_document 	  = (glx_prefix,glx_uri,"save-document")
let glx_sin  	   	  = (glx_prefix,glx_uri,"sin")
let glx_sinh 	   	  = (glx_prefix,glx_uri,"sinh")
let glx_soap_call  	  = (glx_prefix,glx_uri,"soap-call")
let glx_sqrt       	  = (glx_prefix,glx_uri,"sqrt")
let glx_stem              = (glx_prefix,glx_uri,"stem") 
let glx_string_pad 	  = (glx_prefix,glx_uri,"string-pad")
let glx_tan  	     	  = (glx_prefix,glx_uri,"tan")
let glx_tanh 	     	  = (glx_prefix,glx_uri,"tanh")
let glx_union_values 	  = (glx_prefix,glx_uri,"union-values")
let glx_sleep             = (glx_prefix,glx_uri,"sleep")
let glx_getdoc            = (glx_prefix,glx_uri,"getdoc")
let glx_gettime           = (glx_prefix,glx_uri,"gettime")
let glx_livewords         = (glx_prefix,glx_uri,"livewords")
let glx_random_int        = (glx_prefix,glx_uri,"random_int")
let glx_http_request      = (glx_prefix,glx_uri,"http-request")
let glx_http_get_request      = (glx_prefix,glx_uri,"http-get-request")

(* Names for special elements constructed by Galax *)

let glx_result = (glx_prefix,glx_uri,"result")

(* Special names for casting functions -- for raising errors *)
let glx_cast_to_elem = (glx_prefix,glx_uri,"cast_to_elem")
let glx_cast_to_attr = (glx_prefix,glx_uri,"cast_to_attr")
let glx_cast_to_bool = (glx_prefix,glx_uri,"cast_to_bool")
let glx_cast = (glx_prefix,glx_uri,"cast")
let glx_castable = (glx_prefix,glx_uri,"castable")
let glx_forest_compare = (glx_prefix,glx_uri,"forest_compare")
let glx_algebra_operator = (glx_prefix,glx_uri,"algebra_operator")
let glx_computed_pi_constructor = (glx_prefix,glx_uri,"computed_pi_constructor")
let glx_computed_comment_constructor = (glx_prefix,glx_uri,"computed_comment_constructor")
let glx_forward_axis = (glx_prefix,glx_uri,"forward_axis")
let glx_reverse_axis = (glx_prefix,glx_uri,"reverse_axis")

let glx_get_order         = (glx_prefix,glx_uri,"get-order") 
let glx_get_docid         = (glx_prefix,glx_uri,"get-docid") 
