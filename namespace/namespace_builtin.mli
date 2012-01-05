(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_builtin.mli,v 1.54 2007/09/04 20:08:09 ndonose Exp $ *)

(* Module: Namespace_builtin
   Description:
     This module contains built-in names.
*)
      
open Namespace_names

(* Note:
     To ensure consistency, *all* other modules should use those names
     and never define their own names locally.
*)


(*******************************)
(* Built-in namespace prefixes *)
(*******************************)

(* XML *)
val xml_prefix     : prefix
val xmlns_prefix   : prefix

(* XML Schema *)
val xs_prefix  	   : prefix
val xsi_prefix 	   : prefix

(* XQuery *)
val fn_prefix      : prefix
val xqx_prefix     : prefix
val op_prefix      : prefix
val fs_prefix  	   : prefix
val local_prefix   : prefix
val empty_prefix   : prefix
val err_prefix     : prefix

(* Galax *)
val glx_prefix     : prefix


(***************************)
(* Built-in namespace URIs *)
(***************************)

(* XML *)
val xml_uri     : uri
val xmlns_uri   : uri

(* XML Schema *)
val xs_uri      : uri
val xsd_uri     : uri
val xsi_uri     : uri

(* XQuery *)
val fn_uri      : uri
val xqx_uri     : uri
val op_uri      : uri
val fs_uri      : uri
val local_uri   : uri
val empty_uri   : uri

(* Galax *)
val glx_uri     : uri


(* Checks whether a namespace binding is built-in or not *)

val is_built_in_namespace     	  : (prefix * uri) -> bool
val is_xml_built_in_namespace 	  : (prefix * uri) -> bool
val is_xml_out_built_in_namespace : (prefix * uri) -> bool
val is_xml_actual_out_built_in_namespace : (prefix * uri) -> bool
val is_xquery_built_in_namespace  : (prefix * uri) -> bool
val is_predefined_namespace       : uri -> bool


(* Built-in namespace bindings *)

val xquery_built_in_namespaces  : (prefix * uri) list
val ext_xquery_built_in_namespaces  : (prefix * uri) list
val default_built_in_namespaces : (prefix * uri) list
val xml_built_in_namespaces     : (prefix * uri) list
val xml_out_built_in_namespaces : (prefix * uri) list
val all_built_in_namespaces     : (prefix * uri) list


(*************)
(* XML names *)
(*************)

(* ID/IDREF *)
val empty_ID    : rqname
val empty_IDREF : rqname

val xml_lang : rqname
val xml_base : rqname
val xml_id   : rqname


(********************)
(* XML Schema names *)
(********************)

(* Ur types *)

val uxs_anyType      : uqname

val xs_anySimpleType : rqname
val xs_anyType 	     : rqname

(* xsi: attributes *)

val xsi_nil            		  : rqname
val xsi_type           		  : rqname
val xsi_schemaLocation 		  : rqname
val xsi_noNamespaceSchemaLocation : rqname

(* xs: namespace *)

val uxs_string       : uqname
val uxs_boolean      : uqname
val uxs_decimal      : uqname
val uxs_float        : uqname
val uxs_double       : uqname
val uxs_duration     : uqname
val uxs_dateTime     : uqname
val uxs_time 	     : uqname
val uxs_date 	     : uqname
val uxs_gYearMonth   : uqname
val uxs_gYear        : uqname
val uxs_gMonthDay    : uqname
val uxs_gDay   	     : uqname
val uxs_gMonth 	     : uqname
val uxs_hexBinary    : uqname
val uxs_base64Binary : uqname
val uxs_anyURI       : uqname
val uxs_QName        : uqname
val uxs_NOTATION     : uqname

(* Primitive types *)
val xs_string  	    : rqname
val xs_boolean 	    : rqname
val xs_decimal 	    : rqname
val xs_float   	    : rqname
val xs_double  	    : rqname
val xs_duration     : rqname
val xs_dateTime     : rqname
val xs_time 	    : rqname
val xs_date 	    : rqname
val xs_gYearMonth   : rqname
val xs_gYear        : rqname
val xs_gMonthDay    : rqname
val xs_gDay   	    : rqname
val xs_gMonth 	    : rqname
val xs_hexBinary    : rqname
val xs_base64Binary : rqname
val xs_anyURI       : rqname
val xs_QName        : rqname
val xs_NOTATION     : rqname

(* xsd: namespace *)

(* Primitive types *)
val xsd_string       : rqname
val xsd_boolean      : rqname
val xsd_decimal      : rqname
val xsd_float        : rqname
val xsd_double       : rqname
val xsd_duration     : rqname
val xsd_dateTime     : rqname
val xsd_time 	     : rqname
val xsd_date 	     : rqname
val xsd_gYearMonth   : rqname
val xsd_gYear        : rqname
val xsd_gMonthDay    : rqname
val xsd_gDay   	     : rqname
val xsd_gMonth 	     : rqname
val xsd_hexBinary    : rqname
val xsd_base64Binary : rqname
val xsd_anyURI       : rqname
val xsd_QName        : rqname
val xsd_NOTATION     : rqname

(* Derived types *)
val uxs_normalizedString   : uqname
val uxs_token              : uqname
val uxs_language           : uqname
val uxs_NMTOKEN            : uqname
val uxs_NMTOKENS           : uqname
val uxs_Name               : uqname
val uxs_NCName             : uqname
val uxs_ID                 : uqname
val uxs_IDREF              : uqname
val uxs_IDREFS             : uqname
val uxs_ENTITY             : uqname
val uxs_ENTITIES           : uqname
val uxs_integer            : uqname
val uxs_nonPositiveInteger : uqname
val uxs_negativeInteger    : uqname
val uxs_long               : uqname
val uxs_int                : uqname
val uxs_short              : uqname
val uxs_byte               : uqname
val uxs_nonNegativeInteger : uqname
val uxs_unsignedLong       : uqname
val uxs_unsignedInt        : uqname
val uxs_unsignedShort      : uqname
val uxs_unsignedByte       : uqname
val uxs_positiveInteger    : uqname

val xs_normalizedString   : rqname
val xs_token              : rqname
val xs_language           : rqname
val xs_NMTOKEN            : rqname
val xs_NMTOKENS           : rqname
val xs_Name               : rqname
val xs_NCName             : rqname
val xs_ID                 : rqname
val xs_IDREF              : rqname
val xs_IDREFS             : rqname
val xs_ENTITY             : rqname
val xs_ENTITIES           : rqname
val xs_integer            : rqname
val xs_nonPositiveInteger : rqname
val xs_negativeInteger    : rqname
val xs_long               : rqname
val xs_int                : rqname
val xs_short              : rqname
val xs_byte               : rqname
val xs_nonNegativeInteger : rqname
val xs_unsignedLong       : rqname
val xs_unsignedInt        : rqname
val xs_unsignedShort      : rqname
val xs_unsignedByte       : rqname
val xs_positiveInteger    : rqname

(* Derived types *)
val xsd_normalizedString   : rqname
val xsd_token              : rqname
val xsd_language           : rqname
val xsd_NMTOKEN            : rqname
val xsd_NMTOKENS           : rqname
val xsd_Name               : rqname
val xsd_NCName             : rqname
val xsd_ID                 : rqname
val xsd_IDREF              : rqname
val xsd_IDREFS             : rqname
val xsd_ENTITY             : rqname
val xsd_ENTITIES           : rqname
val xsd_integer            : rqname
val xsd_nonPositiveInteger : rqname
val xsd_negativeInteger    : rqname
val xsd_long               : rqname
val xsd_int                : rqname
val xsd_short              : rqname
val xsd_byte               : rqname
val xsd_nonNegativeInteger : rqname
val xsd_unsignedLong       : rqname
val xsd_unsignedInt        : rqname
val xsd_unsignedShort      : rqname
val xsd_unsignedByte       : rqname
val xsd_positiveInteger    : rqname


(****************)
(* XQuery names *)
(****************)

(* wildcards *)

val wild_uqname : uqname
val wild_rqname : rqname

(* XQuery types *)

val uxs_untypedAtomic    : uqname

val xs_dayTimeDuration   : rqname
val xs_yearMonthDuration : rqname
val xs_untypedAtomic     : rqname
val xs_untyped           : rqname
val xs_anyAtomicType     : rqname


(*************)
(* F&O names *)
(*************)

(* fn: Built-in functions *)

val fn_abs_decimal     	: rqname
val fn_abs_double      	: rqname
val fn_abs_float       	: rqname
val fn_abs_integer     	: rqname
val fn_abs     	       	: rqname
val fn_avg_yearMonthDuration   	: rqname
val fn_avg_dayTimeDuration     	: rqname
val fn_avg_decimal     	: rqname
val fn_avg_double      	: rqname
val fn_avg_float       	: rqname
val fn_avg_integer     	: rqname
val fn_avg             	: rqname
val fn_base_uri        	: rqname
val fn_boolean         	: rqname
val fn_boolean         	: rqname
val fn_ceiling_decimal 	: rqname
val fn_ceiling_double  	: rqname
val fn_ceiling_float   	: rqname
val fn_ceiling_integer 	: rqname
val fn_ceiling 	       	: rqname
val fn_codepoints_to_string : rqname
val fn_codepoint_equal  : rqname
val fn_collection     	: rqname
val fn_concat  	       	: rqname
val fn_contains        	: rqname
val fn_count            : rqname
val fn_current_datetime : rqname
val fn_current_date 	: rqname
val fn_current_time 	: rqname
val fn_data       	: rqname
val fn_deep_equal 	: rqname
val fn_distinct_values  : rqname
val fn_doc      	: rqname
val fn_doc_available   	: rqname
val fn_empty 	 	: rqname
val fn_empty 	 	: rqname
val fn_ends_with 	: rqname
val fn_error       	: rqname
val fn_exactly_one 	: rqname
val fn_exists           : rqname
val fn_resolve_QName    : rqname
val fn_QName 	        : rqname
val fn_id               : rqname
val fn_idref            : rqname
val fn_in_scope_prefixes : rqname
val fn_false          	: rqname
val fn_floor_decimal  	: rqname
val fn_floor_double  	: rqname
val fn_floor_float   	: rqname
val fn_floor_integer 	: rqname
val fn_floor     	: rqname
val fn_local_name_from_QName : rqname
val fn_prefix_from_QName : rqname
val fn_namespace_uri_from_QName : rqname
val fn_namespace_uri_for_prefix : rqname
val fn_implicit_timezone : rqname
val fn_static_base_uri : rqname
val fn_default_collation : rqname
val fn_document_uri : rqname
val fn_dateTime : rqname
val fn_index_of : rqname
val fn_insert_before : rqname
val fn_lang : rqname
val fn_last : rqname
val fn_local_name : rqname
val fn_lower_case : rqname
val fn_encode_for_uri : rqname
val fn_iri_to_uri : rqname
val fn_escape_html_uri : rqname
val fn_matches : rqname
val fn_tokenize : rqname
val fn_replace : rqname
val fn_max_string : rqname
val fn_max_date : rqname
val fn_max_time : rqname
val fn_max_dateTime : rqname
val fn_max_yearMonthDuration   	: rqname
val fn_max_dayTimeDuration     	: rqname
val fn_max_decimal : rqname
val fn_max_double : rqname
val fn_max_float : rqname
val fn_max_integer : rqname
val fn_max : rqname
val fn_min_string : rqname
val fn_min_date : rqname
val fn_min_time : rqname
val fn_min_dateTime : rqname
val fn_min_yearMonthDuration   	: rqname
val fn_min_dayTimeDuration     	: rqname
val fn_min_decimal : rqname
val fn_min_double : rqname
val fn_min_float : rqname
val fn_min_integer : rqname
val fn_min : rqname
val fn_name : rqname
val fn_number : rqname
val fn_namespace_uri : rqname
val fn_node_name : rqname
val fn_nilled : rqname
val fn_normalize_space : rqname
val fn_normalize_unicode : rqname
val fn_not : rqname
val fn_one_or_more : rqname
val fn_position : rqname
val fn_remove : rqname
val fn_resolve_uri : rqname
val fn_reverse : rqname
val fn_root : rqname
val fn_round_decimal : rqname
val fn_round_double : rqname
val fn_round_float : rqname
val fn_round_integer : rqname
val fn_round : rqname
val fn_round_half_to_even_decimal : rqname
val fn_round_half_to_even_double : rqname
val fn_round_half_to_even_float : rqname
val fn_round_half_to_even_integer : rqname
val fn_round_half_to_even : rqname
val fn_sequence_node_identical : rqname
val fn_starts_with : rqname
val fn_string_join : rqname
val fn_string_length : rqname
val fn_string : rqname
val fn_string_to_codepoints : rqname
val fn_subsequence : rqname
val fn_substring_after : rqname
val fn_substring_before : rqname
val fn_substring : rqname
val fn_sum_yearMonthDuration   	: rqname
val fn_sum_dayTimeDuration     	: rqname
val fn_sum_decimal : rqname
val fn_sum_double : rqname
val fn_sum_float : rqname
val fn_sum_integer : rqname
val fn_sum : rqname
val fn_trace : rqname
val fn_translate : rqname
val fn_true : rqname
val fn_unique_ID : rqname
val fn_unordered : rqname
val fn_upper_case : rqname
val fn_zero_or_one : rqname

(* op: operators *)

val op_anyURI_equal : rqname
val op_anyURI_nequal : rqname
val op_boolean_equal : rqname
val op_boolean_ge : rqname
val op_boolean_gt : rqname
val op_boolean_le : rqname
val op_boolean_lt : rqname
val op_boolean_nequal : rqname
val op_decimal_add : rqname
val op_decimal_divide : rqname
val op_decimal_idivide : rqname
val op_decimal_equal : rqname
val op_decimal_ge : rqname
val op_decimal_gt : rqname
val op_decimal_le : rqname
val op_decimal_lt : rqname
val op_decimal_mod : rqname
val op_decimal_multiply : rqname
val op_decimal_nequal : rqname
val op_decimal_subtract : rqname
val op_decimal_unary_minus : rqname
val op_decimal_unary_plus : rqname
val op_double_add : rqname
val op_double_divide : rqname
val op_double_idivide : rqname
val op_double_equal : rqname
val op_double_ge : rqname
val op_double_gt : rqname
val op_double_le : rqname
val op_double_lt : rqname
val op_double_mod : rqname
val op_double_multiply : rqname
val op_double_nequal : rqname
val op_double_subtract : rqname
val op_double_unary_minus : rqname
val op_double_unary_plus : rqname
val op_equal : rqname
val op_equal_left_empty : rqname
val op_equal_right_empty : rqname
val op_except : rqname
val op_float_add : rqname
val op_float_divide : rqname
val op_float_idivide : rqname
val op_float_equal : rqname
val op_float_ge : rqname
val op_float_gt : rqname
val op_float_le : rqname
val op_float_lt : rqname
val op_float_mod : rqname
val op_float_multiply : rqname
val op_float_nequal : rqname
val op_float_subtract : rqname
val op_float_unary_minus : rqname
val op_float_unary_plus : rqname
val op_ge : rqname
val op_ge_left_empty : rqname
val op_ge_right_empty : rqname
val op_gt : rqname
val op_gt_left_empty : rqname
val op_gt_right_empty : rqname
val op_integer_add : rqname
val op_integer_divide : rqname
val op_integer_idivide : rqname
val op_integer_equal : rqname
val op_integer_ge : rqname
val op_integer_gt : rqname
val op_integer_le : rqname
val op_integer_lt : rqname
val op_integer_mod : rqname
val op_integer_multiply : rqname
val op_integer_nequal : rqname
val op_integer_subtract : rqname
val op_integer_unary_minus : rqname
val op_integer_unary_plus : rqname
val op_intersect : rqname

val op_date_equal : rqname
val op_date_nequal : rqname
val op_date_lt : rqname
val op_date_le : rqname
val op_date_gt : rqname
val op_date_ge : rqname

val op_gYearMonth_equal : rqname
val op_gYearMonth_nequal : rqname

val op_gYear_equal : rqname
val op_gYear_nequal : rqname

val op_gMonthDay_equal : rqname
val op_gMonthDay_nequal : rqname

val op_gDay_equal : rqname
val op_gDay_nequal : rqname

val op_gMonth_equal : rqname
val op_gMonth_nequal : rqname

val op_add_yearMonthDuration_to_date : rqname
val op_add_yearMonthDuration_to_date2 : rqname
val op_add_dayTimeDuration_to_date : rqname
val op_add_dayTimeDuration_to_date2 : rqname
val op_subtract_yearMonthDuration_from_date : rqname
val op_subtract_dayTimeDuration_from_date :  rqname

val op_time_equal : rqname
val op_time_nequal : rqname
val op_time_lt : rqname
val op_time_le : rqname
val op_time_gt : rqname
val op_time_ge : rqname
val op_add_dayTimeDuration_to_time : rqname
val op_add_dayTimeDuration_to_time2 : rqname
val op_subtract_dateTimes : rqname
val op_subtract_dates : rqname
val op_subtract_times : rqname
val op_subtract_dayTimeDuration_from_time : rqname

val op_dateTime_equal : rqname
val op_dateTime_nequal : rqname
val op_dateTime_lt : rqname
val op_dateTime_le : rqname
val op_dateTime_gt : rqname
val op_dateTime_ge : rqname
val op_add_yearMonthDuration_to_dateTime : rqname
val op_add_yearMonthDuration_to_dateTime2 : rqname
val op_add_dayTimeDuration_to_dateTime : rqname
val op_add_dayTimeDuration_to_dateTime2 : rqname
val op_subtract_yearMonthDuration_from_dateTime : rqname
val op_subtract_dayTimeDuration_from_dateTime : rqname

val op_yearMonthDuration_equal : rqname
val op_yearMonthDuration_nequal : rqname
val op_yearMonthDuration_lt : rqname
val op_yearMonthDuration_le : rqname
val op_yearMonthDuration_gt : rqname
val op_yearMonthDuration_ge : rqname
val op_add_yearMonthDurations : rqname
val op_subtract_yearMonthDurations : rqname
val op_multiply_yearMonthDuration : rqname
val op_divide_yearMonthDuration : rqname
val op_multiply_yearMonthDuration2 : rqname
val op_divide_yearMonthDuration_by_yearMonthDuration : rqname

val op_dayTimeDuration_equal : rqname
val op_dayTimeDuration_nequal : rqname
val op_dayTimeDuration_lt : rqname
val op_dayTimeDuration_le : rqname
val op_dayTimeDuration_gt : rqname
val op_dayTimeDuration_ge : rqname
val op_add_dayTimeDurations : rqname
val op_subtract_dayTimeDurations : rqname
val op_multiply_dayTimeDuration : rqname
val op_multiply_dayTimeDuration2 : rqname
val op_divide_dayTimeDuration : rqname
val op_divide_dayTimeDuration_by_dayTimeDuration : rqname

val op_duration_equal : rqname
val op_duration_nequal : rqname

(* Date/time/ extraction functions *)
val fn_years_from_duration : rqname
val fn_months_from_duration : rqname
val fn_days_from_duration : rqname
val fn_hours_from_duration : rqname
val fn_minutes_from_duration : rqname
val fn_seconds_from_duration : rqname
val fn_year_from_dateTime : rqname
val fn_month_from_dateTime : rqname
val fn_day_from_dateTime : rqname
val fn_hours_from_dateTime : rqname
val fn_minutes_from_dateTime : rqname
val fn_seconds_from_dateTime : rqname
val fn_timezone_from_dateTime : rqname
val fn_year_from_date : rqname
val fn_month_from_date : rqname
val fn_day_from_date : rqname
val fn_timezone_from_date : rqname
val fn_hours_from_time : rqname
val fn_minutes_from_time : rqname
val fn_seconds_from_time : rqname
val fn_timezone_from_time : rqname

val fn_adjust_time_to_timezone : rqname
val fn_adjust_date_to_timezone : rqname
val fn_adjust_dateTime_to_timezone : rqname

val fn_adjust_time_to_timezone_unary : rqname
val fn_adjust_date_to_timezone_unary : rqname
val fn_adjust_dateTime_to_timezone_unary : rqname

val op_le : rqname
val op_le_left_empty : rqname
val op_le_right_empty : rqname
val op_lt : rqname
val op_lt_left_empty : rqname
val op_lt_right_empty : rqname
val op_nequal : rqname
val op_nequal_left_empty : rqname
val op_nequal_right_empty : rqname
val op_node_after : rqname
val op_node_before : rqname
val op_is_same_node : rqname
val op_numeric_add : rqname
val op_numeric_divide : rqname
val op_numeric_idivide : rqname
val op_numeric_mod : rqname
val op_numeric_multiply : rqname
val op_numeric_subtract : rqname
val op_numeric_unary_minus : rqname
val op_numeric_unary_plus : rqname
val op_QName_equal : rqname
val op_QName_nequal : rqname
val fn_compare : rqname
val op_string_equal : rqname
val op_string_ge : rqname
val op_string_gt : rqname
val op_string_le : rqname
val op_string_lt : rqname
val op_string_nequal : rqname
val op_to : rqname
val op_union : rqname
val op_hexBinary_equal : rqname
val op_base64Binary_equal : rqname
val op_hexBinary_nequal : rqname
val op_base64Binary_nequal : rqname

val err_default_error : rqname


(**************************)
(* Formal Semantics names *)
(**************************)

(* Note:
     fs: prefixed functions are Formal Semantics functions used in
     implementing the XQuery semantics.
 *)

val fs_convert_simple_operand 	      : rqname
val fs_distinct_docorder      	      : rqname
val fs_distinct_docorder_or_atomic_sequence : rqname
val fs_distinct 	      	      : rqname
val fs_distinct_or_atomic_sequence    : rqname
val fs_docorder 	      	      : rqname
val fs_docorder_or_atomic_sequence    : rqname
val fs_first    	      	      : rqname
val fs_item_sequence_to_untypedAtomic : rqname
val fs_item_sequence_to_untypedAtomic_optional : rqname
val fs_item_sequence_to_node_sequence : rqname
val fs_last_fn       	     	      : rqname
val fs_node_sequence 	     	      : rqname
val fs_node_sequence_or_atomic_sequence : rqname
val fs_promote_to_numeric             : rqname
val fs_promote_to_anystring           : rqname
val fs_unsafe_promote_to_numeric      : rqname
val fs_untyped_to_any     	      : rqname
val fs_untyped_to_double  	      : rqname
val fs_untyped_to_integer 	      : rqname
val fs_untyped_to_string  	      : rqname


(*****************)
(* XQueryX Names *)
(*****************)

val xqx_xquery : rqname

(***************)
(* Galax names *)
(***************)

(* glx: Galax additional built-in functions. *)
(* Most of these are exposing functions from O'Caml standard libraries *)

val glx_acos : rqname
val glx_asin : rqname
val glx_atan2 : rqname
val glx_atan : rqname
val glx_cosh : rqname
val glx_cos : rqname
val glx_except_values : rqname
val glx_exp : rqname
val glx_exponent : rqname
val glx_error : rqname
val glx_file_exists : rqname
val glx_deep_distinct : rqname
val glx_get_lang : rqname
val glx_intersect_values : rqname
val glx_keyref : rqname
val glx_log10 : rqname
val glx_log : rqname
val glx_print_item_err : rqname
val glx_print_item : rqname
val glx_string_of_item : rqname
val glx_doc_of_string : rqname
val glx_print_string_err : rqname
val glx_print_string : rqname
val glx_save_document : rqname
val glx_sinh : rqname
val glx_sin : rqname
val glx_soap_call : rqname
val glx_sqrt : rqname
val glx_stem : rqname
val glx_string_pad : rqname
val glx_tanh : rqname
val glx_tan : rqname
val glx_union_values : rqname
val glx_sleep : rqname
val glx_getdoc : rqname
val glx_gettime : rqname
val glx_livewords : rqname
val glx_random_int : rqname 
val glx_http_request : rqname
val glx_http_get_request : rqname

(* Names for special elements constructed by Galax *)

val glx_result    : rqname

(* Special names for casting functions -- for raising errors *)
val glx_cast_to_elem : rqname
val glx_cast_to_attr : rqname
val glx_cast_to_bool : rqname
val glx_forest_compare : rqname
val glx_algebra_operator : rqname
val glx_computed_pi_constructor : rqname
val glx_computed_comment_constructor : rqname
val glx_cast : rqname
val glx_castable : rqname
val glx_forward_axis : rqname
val glx_reverse_axis : rqname

val glx_get_order : rqname
val glx_get_docid : rqname
