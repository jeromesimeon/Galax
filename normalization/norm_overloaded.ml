(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: norm_overloaded.ml,v 1.10 2007/05/21 20:22:40 mff Exp $ *)

(* Module: Norm_overloaded
   Description:
     This modules implements some support for built-in overloaded
     functions in XQuery 1.0 and XPath 2.0.
  *)

open Namespace_names
open Namespace_util
open Namespace_builtin

open Xquery_common_ast
open Xquery_core_ast

open Datatypes

open Error

(* Mapping tables for overloaded functions.

   NB: The order of functions in the lists below is significant!

   For a given overloaded function, its corresponding non-overloaded
   functions are added to the hash table in the _reverse_ order of
   precedence, so the function with the most specific signature is
   returned first and the function with the least specific signature
   (i.e., the overloaded function itself) is returned last.

*)

let op_numeric_add_table =
  [ (* The overloaded function itself is the least specific *)
    (op_numeric_add, 2);
    (op_double_add, 2);
    (op_float_add, 2);
    (op_decimal_add, 2);
    (op_integer_add, 2);
    (op_add_yearMonthDuration_to_date, 2);
    (op_add_yearMonthDuration_to_date2, 2);
    (op_add_dayTimeDuration_to_date, 2);
    (op_add_dayTimeDuration_to_date2, 2);
    (op_add_dayTimeDuration_to_time, 2);
    (op_add_dayTimeDuration_to_time2, 2);
    (op_add_yearMonthDuration_to_dateTime, 2);
    (op_add_yearMonthDuration_to_dateTime2, 2);
    (op_add_dayTimeDuration_to_dateTime, 2);
    (op_add_dayTimeDuration_to_dateTime2, 2);
    (op_add_yearMonthDurations, 2);
    (op_add_dayTimeDurations, 2) ]

let op_numeric_subtract_table =
  [ (* The overloaded function itself is the least specific *)
    (op_numeric_subtract, 2); 
    (op_double_subtract, 2); 
    (op_float_subtract, 2); 
    (op_decimal_subtract, 2); 
    (op_integer_subtract, 2);  
    (op_subtract_yearMonthDuration_from_dateTime, 2);
    (op_subtract_dayTimeDuration_from_dateTime, 2);
    (op_subtract_dateTimes, 2);
    (op_subtract_dates, 2);
    (op_subtract_times, 2);
    (op_subtract_dayTimeDuration_from_time, 2);
    (op_subtract_dayTimeDuration_from_date, 2);
    (op_subtract_yearMonthDuration_from_date, 2);
    (op_subtract_yearMonthDurations, 2);
    (op_subtract_dayTimeDurations, 2) ]

let op_numeric_multiply_table =
  [ (* The overloaded function itself is the least specific *)
    (op_numeric_multiply, 2);
    (op_double_multiply, 2); 
    (op_float_multiply, 2); 
    (op_decimal_multiply, 2); 
    (op_integer_multiply, 2); 
    (op_multiply_yearMonthDuration, 2);
    (op_multiply_yearMonthDuration2, 2);
    (op_multiply_dayTimeDuration, 2);
    (op_multiply_dayTimeDuration2, 2) ]

let op_numeric_divide_table =
  [ (* The overloaded function itself is the least specific *)
    (op_numeric_divide, 2); 
    (op_double_divide, 2); 
    (op_float_divide, 2);  
    (op_decimal_divide, 2); 
    (op_integer_divide, 2);  
    (op_divide_yearMonthDuration, 2);
    (op_divide_yearMonthDuration_by_yearMonthDuration, 2);
    (op_divide_dayTimeDuration, 2);
    (op_divide_dayTimeDuration_by_dayTimeDuration, 2) ]

let op_numeric_mod_table =
  [ (* The overloaded function itself is the least specific *)
    (op_numeric_mod, 2);
    (op_double_mod, 2);
    (op_float_mod, 2);
    (op_decimal_mod, 2);
    (op_integer_mod, 2) ]

let op_numeric_unary_plus_table =
  [ (* The overloaded function itself is the least specific *)
    (op_numeric_unary_plus, 1);
    (op_double_unary_plus, 1);
    (op_float_unary_plus, 1);
    (op_decimal_unary_plus, 1);
    (op_integer_unary_plus, 1) ]

let op_numeric_unary_minus_table =
  [ (* The overloaded function itself is the least specific *)
    (op_numeric_unary_minus, 1);
    (op_double_unary_minus, 1);
    (op_float_unary_minus, 1);
    (op_decimal_unary_minus, 1);
    (op_integer_unary_minus, 1) ]

let op_numeric_idivide_table =
  [ (* The overloaded function itself is the least specific *)
    (op_numeric_idivide, 2);
    (op_double_idivide, 2);
    (op_float_idivide, 2);
    (op_decimal_idivide, 2);
    (op_integer_idivide, 2) ]

let op_equal_table =
  [ (* The overloaded function itself is the least specific *)
    (op_equal, 2);
    (op_equal_left_empty, 2);
    (op_equal_right_empty, 2);
    (op_double_equal, 2);
    (op_float_equal, 2);
    (op_decimal_equal, 2);
    (op_integer_equal, 2);
    (op_string_equal, 2);
    (op_QName_equal, 2);
    (op_anyURI_equal, 2);
    (op_boolean_equal, 2);
    (op_date_equal, 2);
    (op_gYearMonth_equal, 2);
    (op_gYear_equal, 2);
    (op_gMonthDay_equal, 2);
    (op_gDay_equal, 2);
    (op_gMonth_equal, 2);
    (op_time_equal, 2);
    (op_dateTime_equal, 2);
    (op_duration_equal, 2);
    (op_yearMonthDuration_equal, 2);
    (op_dayTimeDuration_equal, 2);
    (op_hexBinary_equal, 2);
    (op_base64Binary_equal, 2) ]

let op_nequal_table =
  [ (* The overloaded function itself is the least specific *)
    (op_nequal, 2);
    (op_nequal_left_empty, 2);
    (op_nequal_right_empty, 2);
    (op_double_nequal, 2);
    (op_float_nequal, 2);
    (op_decimal_nequal, 2);
    (op_integer_nequal, 2);
    (op_string_nequal, 2);
    (op_QName_nequal, 2);
    (op_anyURI_nequal, 2);
    (op_boolean_nequal, 2);
    (op_date_nequal, 2);
    (op_gYearMonth_nequal, 2);
    (op_gYear_nequal, 2);
    (op_gMonthDay_nequal, 2);
    (op_gDay_nequal, 2);
    (op_gMonth_nequal, 2);
    (op_time_nequal, 2);
    (op_dateTime_nequal, 2);
    (op_duration_nequal, 2);
    (op_yearMonthDuration_nequal, 2);
    (op_dayTimeDuration_nequal, 2);
    (op_hexBinary_nequal, 2);
    (op_base64Binary_nequal, 2) ]

let op_lt_table =
  [ (* The overloaded function itself is the least specific *)
    (op_lt, 2);
    (op_lt_left_empty, 2);
    (op_lt_right_empty, 2);
    (op_double_lt, 2);
    (op_float_lt, 2);
    (op_decimal_lt, 2);
    (op_integer_lt, 2);
    (op_string_lt, 2);
    (op_boolean_lt, 2);
    (op_date_lt, 2);
    (op_time_lt, 2);
    (op_dateTime_lt, 2);
    (op_yearMonthDuration_lt, 2);
    (op_dayTimeDuration_lt, 2) ]

let op_gt_table =
  [ (* The overloaded function itself is the least specific *)
    (op_gt, 2);
    (op_gt_left_empty, 2);
    (op_gt_right_empty, 2);
    (op_double_gt, 2);
    (op_float_gt, 2);
    (op_decimal_gt, 2);
    (op_integer_gt, 2);
    (op_string_gt, 2);
    (op_boolean_gt, 2);
    (op_date_gt, 2);
    (op_time_gt, 2);
    (op_dateTime_gt, 2);
    (op_yearMonthDuration_gt, 2);
    (op_dayTimeDuration_gt, 2) ]  

let op_le_table =
  [ (* The overloaded function itself is the least specific *)
    (op_le, 2);
    (op_le_left_empty, 2);
    (op_le_right_empty, 2);
    (op_double_le, 2);
    (op_float_le, 2);
    (op_decimal_le, 2);
    (op_integer_le, 2);
    (op_string_le, 2);
    (op_boolean_le, 2);
    (op_date_le, 2);
    (op_time_le, 2);
    (op_dateTime_le, 2);
    (op_yearMonthDuration_le, 2);
    (op_dayTimeDuration_le, 2) ]

let op_ge_table =
  [ (* The overloaded function itself is the least specific *)
    (op_ge, 2);
    (op_ge_left_empty, 2);
    (op_ge_right_empty, 2);
    (op_double_ge, 2);
    (op_float_ge, 2);
    (op_decimal_ge, 2);
    (op_integer_ge, 2);
    (op_string_ge, 2);
    (op_boolean_ge, 2);
    (op_date_ge, 2);
    (op_time_ge, 2);
    (op_dateTime_ge, 2);
    (op_yearMonthDuration_ge, 2);
    (op_dayTimeDuration_ge, 2) ]

let fn_floor_table =
  [ (* The overloaded function itself is the least specific *)
    (fn_floor, 1);
    (fn_floor_double, 1);
    (fn_floor_float, 1);
    (fn_floor_decimal, 1);
    (fn_floor_integer, 1) ]

let fn_ceiling_table =
  [ (* The overloaded function itself is the least specific *)
    (fn_ceiling, 1); 
    (fn_ceiling_double, 1);
    (fn_ceiling_float, 1);
    (fn_ceiling_decimal, 1);
    (fn_ceiling_integer, 1) ]

let fn_round_table =
  [ (* The overloaded function itself is the least specific *)
    (fn_round, 1); 
    (fn_round_double, 1);
    (fn_round_float, 1);
    (fn_round_decimal, 1);
    (fn_round_integer, 1) ]

let fn_round_half_to_even_table =
  [ (fn_round_half_to_even_double, 2);
    (fn_round_half_to_even_float, 2);
    (fn_round_half_to_even_decimal, 2);
    (fn_round_half_to_even_integer, 2) ]

let fn_abs_table =
  [ (* The overloaded function itself is the least specific *)
(*    (fn_abs, 1); *)
    (fn_abs_double, 1);
    (fn_abs_float, 1);
    (fn_abs_decimal, 1);
    (fn_abs_integer, 1) ]

let fn_avg_table =
  [ (* The overloaded function itself is the least specific *)
(*    (fn_avg, 1); *)
    (fn_avg_double, 1);
    (fn_avg_float, 1);
    (fn_avg_decimal, 1);
    (fn_avg_integer, 1);
    (fn_avg_yearMonthDuration, 1);
    (fn_avg_dayTimeDuration, 1) ]

let fn_max_table =
  [ (* The overloaded function itself is the least specific *)
    (fn_max_double, 1);
    (fn_max_float, 1);
    (fn_max_decimal, 1);
    (fn_max_integer, 1);
    (fn_max_string, 1);
    (fn_max_date, 1);
    (fn_max_time, 1); 
    (fn_max_dateTime, 1); 
    (fn_max_yearMonthDuration, 1);
    (fn_max_dayTimeDuration, 1); ]

let fn_min_table =
  [ (* The overloaded function itself is the least specific *)
    (fn_min_double, 1);
    (fn_min_float, 1);
    (fn_min_decimal, 1);
    (fn_min_integer, 1);
    (fn_min_string, 1);
    (fn_min_date, 1);
    (fn_min_time, 1); 
    (fn_min_dateTime, 1); 
    (fn_min_yearMonthDuration, 1);
    (fn_min_dayTimeDuration, 1) ]

let fn_sum_table_one =
  [ (fn_sum_double, 1);
    (fn_sum_float, 1);
    (fn_sum_decimal, 1);
    (fn_sum_yearMonthDuration, 1);
    (fn_sum_dayTimeDuration, 1);
    (fn_sum_integer, 1) ] (* THIS NEEDS TO BE FIRST AS THIS IS THE TYPE FOR THE DEFAULT VALUE IF THE INPUT IS EMPTY *)

let fn_sum_table =
  [ (fn_sum_double, 2);
    (fn_sum_float, 2);
    (fn_sum_decimal, 2);
    (fn_sum_integer, 2);
    (fn_sum_yearMonthDuration, 2);
    (fn_sum_dayTimeDuration, 2) ]

(* Each function table should be indexed by its input types *)
let overloaded_functions =
  [ (op_numeric_add, 2),      	 op_numeric_add_table;
    (op_numeric_subtract, 2), 	 op_numeric_subtract_table;
    (op_numeric_multiply, 2), 	 op_numeric_multiply_table;
    (op_numeric_divide, 2),   	 op_numeric_divide_table;
    (op_numeric_mod, 2),      	 op_numeric_mod_table;
    (op_numeric_unary_plus, 1),  op_numeric_unary_plus_table;
    (op_numeric_unary_minus, 1), op_numeric_unary_minus_table;
    (op_numeric_idivide, 2), 	 op_numeric_idivide_table;
    (op_equal, 2),   	     	 op_equal_table;
    (op_nequal, 2),  	     	 op_nequal_table;
    (op_lt, 2),      	     	 op_lt_table;
    (op_gt, 2),      	     	 op_gt_table;
    (op_le, 2),      	     	 op_le_table;
    (op_ge, 2),      	     	 op_ge_table;
    (fn_floor, 1),   	     	 fn_floor_table;
    (fn_ceiling, 1), 	     	 fn_ceiling_table;
    (fn_round, 1),           	 fn_round_table;
    (fn_round_half_to_even, 2),  fn_round_half_to_even_table;
    (fn_abs, 1), 	      	 fn_abs_table;
    (fn_avg, 1), 	      	 fn_avg_table;
    (fn_max, 1), 	      	 fn_max_table;
    (fn_min, 1), 	      	 fn_min_table;
    (fn_sum, 1), 	      	 fn_sum_table_one;
    (fn_sum, 2), 	      	 fn_sum_table ]

let overloaded_functions_default_type = [
  (op_numeric_add, ATDouble);
  (op_numeric_subtract, ATDouble);
  (op_numeric_multiply, ATDouble);
  (op_numeric_divide, ATDouble);
  (op_numeric_mod, ATDouble);
  (op_numeric_unary_plus, ATDouble);
  (op_numeric_unary_minus, ATDouble);
  (op_numeric_idivide, ATDouble);
  (op_equal, ATString);
  (op_nequal, ATString);
  (op_lt, ATString);
  (op_gt, ATString);
  (op_le, ATString);
  (op_ge, ATString);
  (fn_avg, ATDouble);
  (fn_max, ATDouble);
  (fn_min, ATDouble);
  (fn_sum, ATDouble);
  (fn_floor, ATDouble);
  (fn_ceiling, ATDouble);
  (fn_round, ATDouble); 
  (fn_round_half_to_even, ATDouble); 
  (fn_abs, ATDouble)
]

let overloaded_default_type_table =
  RQNameHashtbl.create 167

let add_to_overloaded_default_type_table (cfname, default_type) =
  RQNameHashtbl.add overloaded_default_type_table cfname default_type

let bulk_add_to_overloaded_functions_table table =
  List.iter add_to_overloaded_default_type_table table

let _ =
  bulk_add_to_overloaded_functions_table overloaded_functions_default_type

let lookup_default_atomic_type cfname = 
  try 
    RQNameHashtbl.find overloaded_default_type_table cfname
  with
  | Not_found -> raise(Query(Internal_Error("Default type of overloaded function "^(prefixed_string_of_rqname cfname)^"not found")))

(* The internal hashtable for overloaded functions *)

(* Note:
     Those functions are built-in in the semantics of XQuery, so that
     is ok to leave them as a global variable in the system.
   - Jerome
   *)

let overloaded_functions_table =
  RQNameIntHashtbl.create 167

let add_to_overloaded_functions_table cfname1 cfname2 =
  RQNameIntHashtbl.add overloaded_functions_table cfname1 cfname2

let bulk_add_to_overloaded_functions_table (fname1,fnamelist2) =
  List.iter (add_to_overloaded_functions_table fname1) fnamelist2

let _ = 
  List.iter bulk_add_to_overloaded_functions_table overloaded_functions

(* Is a function overloaded ? *)

let is_overloaded cfname_arity =
  RQNameIntHashtbl.mem overloaded_functions_table cfname_arity

(* What are all the functions a given overloaded function corresponds to? *)

let all_overloaded_bindings_for_function cfname_arity =
  RQNameIntHashtbl.find_all overloaded_functions_table cfname_arity

let resolve_non_overloaded_name norm_context fname =
  let (fun_sig, opt_fun_kind, upd)  = Norm_context.one_sig_from_norm_context norm_context fname in
  (fst fname, fun_sig, opt_fun_kind, upd)

let table_for_overloaded_function norm_context fname =
  if is_overloaded fname
  then
    let all_bindings = all_overloaded_bindings_for_function fname in
    (List.map (resolve_non_overloaded_name norm_context) all_bindings)
  else
    raise (Query (Internal_Error "Not constructing an overloaded function!"))

let table_for_op_gt norm_context =
  let fname = (op_gt,2) in
  table_for_overloaded_function norm_context fname

let table_for_op_equal norm_context =
  let fname = (op_equal,2) in
  table_for_overloaded_function norm_context fname

