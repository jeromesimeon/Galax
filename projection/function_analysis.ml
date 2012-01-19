(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: function_analysis.ml,v 1.15 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Function_analysis
   Description:
     This module implements the path analysis for built-in functions.
*)
open Format


open Error

open Namespace_names
open Namespace_builtin

open Xquery_ast


(* Contains functions that only need parameters used paths *)

let fun_used_only_table =
  [
    (* Section 5: Functions on numbers *)
		(op_integer_add);
		(op_integer_divide);
		(op_integer_equal);
		(op_integer_ge);
		(op_integer_gt);
		(op_integer_le);
		(op_integer_lt);
		(op_integer_mod);
		(op_integer_multiply);
		(op_integer_nequal);
		(op_integer_subtract);
		(op_integer_unary_minus);
		(op_integer_unary_plus);
		(op_intersect);
		(op_boolean_equal);
		(op_boolean_ge);
		(op_boolean_gt);
		(op_boolean_le);
		(op_boolean_lt);
		(op_boolean_nequal);
		(op_decimal_add);
		(op_decimal_divide);
		(op_decimal_equal);
		(op_decimal_ge);
		(op_decimal_gt);
		(op_decimal_le);
		(op_decimal_lt);
		(op_decimal_mod);
		(op_decimal_multiply);
		(op_decimal_nequal);
		(op_decimal_subtract);
		(op_decimal_unary_minus);
		(op_decimal_unary_plus);
		(op_double_add);
		(op_double_divide);
		(op_double_equal);
		(op_double_ge);
		(op_double_gt);
		(op_double_le);
		(op_double_lt);
		(op_double_mod);
		(op_double_multiply);
		(op_double_nequal);
		(op_double_subtract);
		(op_double_unary_minus);
		(op_double_unary_plus);
		(op_float_add);
		(op_float_divide);
		(op_float_equal);
		(op_float_ge);
		(op_float_gt);
		(op_float_le);
		(op_float_lt);
		(op_float_mod);
		(op_float_multiply);
		(op_float_nequal);
		(op_float_subtract);
		(op_float_unary_minus);
		(op_float_unary_plus);

    (fn_floor);
    (fn_ceiling);
    (fn_abs);
    (fn_round);
    (fn_abs_decimal     	);
		(fn_abs_double      	);
		(fn_abs_float       	);
		(fn_abs_integer     	);
		(fn_abs     	       	);
		(fn_avg_yearMonthDuration   	);
		(fn_avg_dayTimeDuration     	);
		(fn_avg_decimal     	);
		(fn_avg_double      	);
		(fn_avg_float       	);
		(fn_avg_integer     	);
		(fn_avg             	);
		(fn_base_uri        	);

		(* Michael: moved to fun_used_return_simple_table *)
		(*		(fn_boolean         	);*)

		(fn_ceiling_decimal 	);
		(fn_ceiling_double  	);
		(fn_ceiling_float   	);
		(fn_ceiling_integer 	);
		(fn_ceiling 	       	);
		(fn_concat  	       	);
		(fn_contains        	);
		(fn_count           );
		
		(* Michael: moved to fun_used_return_simple_table *)
		(* (fn_empty 	 	); *)

		(fn_ends_with 	);
		(fn_error       	);
		(fn_exactly_one 	);
		(fn_exists          );
		(fn_QName 	);
		(fn_false          	);
		(fn_floor_decimal  	);
		(fn_floor_double  	);
		(fn_floor_float   	);
		(fn_floor_integer 	);
		(fn_floor     	);
		(fn_local_name_from_QName);
		(fn_namespace_uri_from_QName);
		(fn_implicit_timezone);
		(fn_index_of);
		(fn_lang);
		(fn_last);
		(fn_local_name);
		(fn_lower_case);
		(fn_matches);
		(fn_tokenize);
		(fn_replace);
		(fn_max_yearMonthDuration   	);
		(fn_max_dayTimeDuration     	);
		(fn_max_decimal);
		(fn_max_double);
		(fn_max_float);
		(fn_max_integer);
		(fn_max);
		(fn_min_yearMonthDuration   	);
		(fn_min_dayTimeDuration     	);
		(fn_min_decimal);
		(fn_min_double);
		(fn_min_float);
		(fn_min_integer);
		(fn_min);
		(fn_name);
		(fn_number);
		(fn_namespace_uri);
		(fn_node_name);
		(fn_normalize_space);
		(fn_not);
		(fn_one_or_more);
		(fn_position);
		(fn_remove);
		(fn_resolve_uri);
		(fn_reverse);
		(fn_round_decimal);
		(fn_round_double);
		(fn_round_float);
		(fn_round_integer);
		(fn_round);
		(fn_round);
		(fn_sequence_node_identical);
		(fn_starts_with);
		(fn_string_join);
		(fn_string_length);
		
		(fn_sum_yearMonthDuration   	);
		(fn_sum_dayTimeDuration     	);
		(fn_sum_decimal);
		(fn_sum_double);
		(fn_sum_float);
		(fn_sum_integer);
		(fn_sum);
		(fn_trace);
		(fn_translate);
		(fn_true);
		(fn_unique_ID);
		(fn_unordered);
		(fn_upper_case);
(fn_zero_or_one);
(fn_years_from_duration);
(fn_months_from_duration);
(fn_days_from_duration);
(fn_hours_from_duration);
(fn_minutes_from_duration);
(fn_seconds_from_duration);
(fn_year_from_dateTime);
(fn_month_from_dateTime);
(fn_day_from_dateTime);
(fn_hours_from_dateTime);
(fn_minutes_from_dateTime);
(fn_seconds_from_dateTime);
(fn_timezone_from_dateTime);
(fn_year_from_date);
(fn_month_from_date);
(fn_day_from_date);
(fn_timezone_from_date);
(fn_hours_from_time);
(fn_minutes_from_time);
(fn_seconds_from_time);
(fn_timezone_from_time);

(fn_adjust_time_to_timezone);
(fn_adjust_date_to_timezone);
(fn_adjust_dateTime_to_timezone);

(fn_adjust_time_to_timezone_unary);
(fn_adjust_date_to_timezone_unary);
(fn_adjust_dateTime_to_timezone_unary);


    (* Section 6. Functions on Strings *)
    (fn_concat);
    (fn_string_join);
    (fn_starts_with);
    (fn_ends_with);
    (fn_contains);
    (fn_substring);
    (fn_string_length);
    (fn_substring_before);
    (fn_substring_after);
    (fn_normalize_space);
    (fn_upper_case);
    (fn_lower_case);
    (fn_translate);
    (glx_string_pad);

    (* Section 7. Functions and Operators on Booleans *)
    (fn_true);
    (fn_false);
    (fn_not);

    (* Section 9. Functions on QNames *)
    (fn_QName);
    (fn_local_name_from_QName);
    (fn_namespace_uri_from_QName);

    (* Section 10. Functions and Operators for anyURI *)
    (fn_resolve_uri);
    (op_anyURI_equal);
		(op_anyURI_nequal);

    (* Section 14. Functions and operators on sequences *)
    (op_to);
  ] 


(* Contains functions that use used and return paths *)

let fun_used_return_simple_table =
  [
  
    (* Michael: moved from fun_used_only_table*)
    (fn_boolean         	);  	
    (fn_empty 	 	);
  
   (* Section 2. Accessors *)
   (fn_node_name);
   (fn_base_uri);

   (* Section 13. Functions and Operators on Nodes *)
   (fn_name);
   (fn_local_name);
   (fn_namespace_uri);
   (op_is_same_node);
   (op_node_before);
   (op_node_after);

   (* Section 14. Functions and operators on sequences *)
   (fn_index_of);
   (fn_empty);
   (fn_exists);
   (fn_distinct_values);
   (fn_prefix,fn_uri,"count");
   (fn_sequence_node_identical);

 ] 


(* Contains functions that use used and return (with subtree); paths *)

let fun_used_return_subtree_table =
  [
  
    (fn_subsequence);
    (fs_subsequence);
    (fn_substring_after);
    (fn_substring_before);
    (fn_substring);
    (fn_current_datetime);
    (fn_current_date 	);
    (fn_current_time 	);
    (fn_distinct_values );
    (fn_doc      	);
    
    (* Section 2. Accessors *)
    (fn_string);
    (fn_data);
    
    (* Section 13. Functions and Operators on Nodes *)
    (fn_root);
    (fn_deep_equal);
    (fn_insert_before);
    
    (* Section 14. Functions and operators on sequences *)
    (fn_deep_equal);
    (op_equal); 
    (op_nequal);
    (op_le);
    (op_ge);
    (op_gt);
    (op_lt);
    (op_ge);
    (op_ge_left_empty);
    (op_ge_right_empty);
    (op_gt);
    (op_gt_left_empty);
    (op_gt_right_empty);
    (op_equal_left_empty);
    (op_equal_right_empty);
    (op_except);
  ] 
    
    
(* Contains functions that return paths *)
    
let fun_returns_paths_table =
  [ (fs_distinct_docorder);
    (fs_convert_simple_operand 	      );
    
    (* Michael: added function *)
    (fs_distinct_docorder_or_atomic_sequence 	      );
    
    (fs_distinct_docorder      	      );
    (fs_distinct 	      	      );
    (fs_docorder 	      	      );
    (fs_first    	      	      );
    (fs_item_sequence_to_untypedAtomic );
    (fs_last_fn       	     	      );
    (fs_node_sequence 	     	      );
    (fs_promote_to_numeric             );
    (fs_unsafe_promote_to_numeric      );
    (*(fs_untyped_to_any     	      );*)
    (fs_untyped_to_double  	      );
    (fs_untyped_to_integer 	      );
    (fs_untyped_to_string  	      );
    
  ] 


(*******************************************************************************)

(* The following functions are not treated. - Amelie

  
	val xs_anySimpleType : rqname
	val xs_anyType 	     : rqname
	
	(* xsi: attributes *)
	
	val xsi_nil            		  : rqname
	val xsi_type           		  : rqname
	val xsi_schemaLocation 		  : rqname
	val xsi_noNamespaceSchemaLocation : rqname
	
	(* xs: namespace *)
	
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

(* Derived types *)
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

val wild_uqname : uqname
val wild_rqname : rqname

(* xs: types *)

val xs_dayTimeDuration   : rqname
val xs_yearMonthDuration : rqname
val xs_untypedAtomic     : rqname
val xs_untyped           : rqname
val xs_anyAtomicType     : rqname




(***************)
(* Galax names *)
(***************)

(* glx: Galax additional built-in functions. *)

val glx_acos : rqname
val glx_asin : rqname
val glx_atan2 : rqname
val glx_atan : rqname
val glx_cosh : rqname
val glx_cos : rqname
val glx_except_values : rqname
val glx_exp : rqname
val glx_exponent : rqname
val glx_file_exists : rqname
val glx_deep_distinct : rqname
val glx_get_lang : rqname
val glx_intersect_values : rqname
val glx_keyref : rqname
val glx_log10 : rqname
val glx_log : rqname
val glx_print_item_err : rqname
val glx_print_item : rqname
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


*)



(* Function to get XQuery function type for analysis                         *)

type function_kind =
  | UsedOnly
  | UsedReturnSimple
  | UsedReturnSubtree
  | ReturnsPaths
  | ReturnsDefault

let get_fun_analysis_type fname =
	(*	let(_,_,ft) = fname in
		let _ =fprintf Format.std_formatter "Name %s\n" ft in*)
	  
  if (List.exists (fun x -> x = fname) fun_used_only_table)
  then UsedOnly
  else
    if (List.exists (fun x -> x = fname) fun_used_return_simple_table)
    then UsedReturnSimple
    else
      if (List.exists (fun x -> x = fname) fun_used_return_subtree_table)
      then UsedReturnSubtree
      else
	if (List.exists (fun x -> x = fname) fun_returns_paths_table)
	then ReturnsPaths
	else
	(*	let(_,_,ft) = fname in
		let _ =fprintf Format.std_formatter "Name %s\n" ft in*)
	  ReturnsDefault

