(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_fn.ml,v 1.54 2007/11/07 22:00:15 mff Exp $ *)

(* Module: Code_fn
   Description:
     This module implements evaluation of F&O functions.
*)

open Decimal
open Error

open Namespace_names
open Namespace_util
open Namespace_builtin

open Decimal
open Datatypes
open Datatypes_util

open Dm_types
open Dm_atomic
open Dm
open Dm_util

open Physical_item
open Physical_item_util

open Galax_url
open Gmisc

open Physical_value
open Physical_list
open Physical_xml_value
open Physical_value_util
open Physical_util

open Xquery_common_ast
open Xquery_algebra_ast 

open Compile_context
open Code_selection_context
open Execution_context 
open Algebra_type

open Http


(***************************************)
(* Signature of all built-in functions *)
(***************************************)

type bltin = algebra_context -> item Cursor.cursor array -> item Cursor.cursor
type bltin_mat = Algebra_type.item_list_nary_to_item_list_code


(***************************)
(* Dummy built-in function *)
(***************************)

let _DUMMY fn code_ctxt alg_ctxt n =
  raise (Query (Prototype ("Function " ^ (fn)^ " not supported in "
			   ^ Conf.system ^ " version " ^ Conf.version)))

(****************************)
(* F&O Section 2. Accessors *)
(****************************)

(* F&O Section 2.` fn:node-name *)

let _fn_node_name code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  if (Cursor.cursor_is_empty p1)
  then
    Cursor.cursor_empty()
  else
    let node = get_singleton_node p1 in
    match node#node_name() with 
    | Some nn ->
	_atomic_value nn
    | None ->
	Cursor.cursor_empty()

let _fn_nilled code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  if (Cursor.cursor_is_empty p1)
  then
    Cursor.cursor_empty()
  else
    let node = get_singleton_node p1 in
    match node#node_kind() with
    | ElementNodeKind ->
	Cursor.cursor_of_singleton (_boolean ((node#getElementNode())#nilled()))
    | _ -> Cursor.cursor_empty()

(* F&O Section 2.2 fn:string *)

let _fn_string code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let string_result =
    if (Cursor.cursor_is_empty p1)
    then ""
    else
      let i = get_item p1 in
      if (isNode i) then 
	(getNode i)#string_value()
      else 
	(getAtomicValue i)#erase_atomic_value()
  in
  Cursor.cursor_of_singleton (_string string_result)

(* F&O Section 2.3 fn:data *)

(* NOTE: Group operator replicates this functionality and can rewrite
         fn:datas away. If the semantic of fn:data changes this needs
         to change as well.  Currently done in
         Group_code.get_atomic_content
  - chris

  NOTE: THIS SEEMS OBSOLETE??! CHECK WITH CHRIS - JEROME 08/02/2005
*)
let _fn_data code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let items_cursor = p1 in
  let get_atomic_content item =
    if (isNode item)
    then
      Cursor.cursor_map (fun a -> (Item_Atomic a)) ((getNode item)#typed_value())
    else
      Cursor.cursor_of_singleton item
  in (Cursor.cursor_map_concat get_atomic_content items_cursor)

(* F&O Section 2.4 fn:base-uri *)

let _fn_base_uri_one code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  if Cursor.cursor_is_empty p1
  then
    Cursor.cursor_empty()
  else
    let node = get_singleton_node p1 in
    match (!(node#base_uri())) with
    | None -> Cursor.cursor_empty()
    | Some astr -> _atomic_value astr

(************************)
(* F&O Section 4. Trace *)
(************************)

let _fn_trace code_ctxt =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  (fun alg_ctxt n ->
    let (ic, s) = Args.get_array_param2  n in
    (* Note:
       Trace requires materialization of the input cursor as it must
       both print it on file, and return is as the result of the
       function call.
       - Jerome *)
    let il = Cursor.list_of_cursor "Cs_code_fn._fn_trace" ic in
    let s' = get_string s in
    let _ =
      begin
	print_string s';
	Serialization.fserialize_datamodel proc_ctxt Format.std_formatter (Cursor.cursor_of_list il);
	flush stdout
      end
    in
    Cursor.cursor_of_list il)

(*****************************************************)
(* F&O Section 5. Functions and Operators on Numbers *)
(*****************************************************)

(* GENERIC POLYMORPHIC FUNCIONS
  
   unwrap_fn : unwraps arguments
   op_fn : applies operators
   wrap_fn : wraps results

Stricter version:
  if (Cursor.cursor_is_empty p1) && (Cursor.cursor_is_empty p2)
  then
    Cursor.cursor_empty()
  else if (Cursor.cursor_is_empty p1)
  then
    let _ = unwrap_fn1 p2 in
    Cursor.cursor_empty()
  else if (Cursor.cursor_is_empty p2)
  then
    let _ = unwrap_fn2 p1 in
    Cursor.cursor_empty()
  else


*)

let _fn_binary_op_uneven (unwrap_fn1, unwrap_fn2, binop_fn, wrap_fn, (zero_test1,zero_test2)) code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  if (Cursor.cursor_is_empty p1)
  then Cursor.cursor_empty()
  else if (Cursor.cursor_is_empty p2)
  then Cursor.cursor_empty()
  else
    let a1 = zero_test1 (unwrap_fn1 p1) in
    let a2 = zero_test2 (unwrap_fn2 p2) in
    let (i1,i2) = (a1, a2) in
    Cursor.cursor_of_singleton (wrap_fn (binop_fn i1 i2))

let _fn_binary_op (unwrap_fn, binop_fn, wrap_fn, (zero_test1,zero_test2)) code_ctxt alg_ctxt n =
  _fn_binary_op_uneven (unwrap_fn, unwrap_fn, binop_fn, wrap_fn, (zero_test1,zero_test2)) code_ctxt alg_ctxt n

let _fn_unary_op (unwrap_fn, unop_fn, wrap_fn) code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  if (Cursor.cursor_is_empty p1) then Cursor.cursor_empty()
  else Cursor.cursor_of_singleton (wrap_fn (unop_fn (unwrap_fn p1)))

let _fn_compare_op (unwrap_fn, compare_fn) code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  let (i1,i2) = (unwrap_fn p1, unwrap_fn p2) in
  Cursor.cursor_of_singleton (_boolean (compare_fn i1 i2))

let _fn_min_poly (cursor_fn, compare_fn, constructor_fn) code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let allVals = cursor_fn p1 in
  match (Cursor.cursor_peek allVals) with
  | None ->
      Cursor.cursor_empty()
  | Some start_min ->
      let min =
	Cursor.cursor_fold_left (fun x0 x1 -> if (compare_fn x0 x1) = -1 then x0 else x1) start_min allVals
      in
      Cursor.cursor_of_singleton (constructor_fn min)

let _fn_max_poly (cursor_fn, compare_fn, constructor_fn)  code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let allVals = cursor_fn p1 in
  match (Cursor.cursor_peek allVals) with
  | None ->
      Cursor.cursor_empty()
  | Some start_max ->
      let max =
	Cursor.cursor_fold_left
	  (fun x0 x1 ->
	    if (compare_fn x0 x1) = 1 then x0 else x1) start_max allVals
      in
      Cursor.cursor_of_singleton (constructor_fn max)

(* xsd:int *)

let raise_zero () = raise (Query (Wrong_Args "Division by zero"))
let raise_nan () = raise (Query (Wrong_Args "NaN in idiv"))
let raise_infinity () = raise (Query (Wrong_Args "Infinity in idiv"))

let def_zero_test = ((fun x -> x), (fun x -> x))
let integer_zero_test =
  ((fun x -> x),
   (fun x -> if (_integer_eq _integer_zero x) then raise_zero() else x))
let decimal_zero_test =
  ((fun x -> x),
   (fun x -> if (_decimal_eq _decimal_zero x) then raise_zero() else x))
let float_nan_zero_test =
  ((fun x ->
    if x == nan then raise_nan()
    else if (x = infinity) or (x = -.infinity) then raise_infinity()
    else x),
   (fun x ->
     if x == nan then raise_nan()
     else if (x = 0.0) then raise_zero() else x))

let _op_hexBinary_equal code_ctxt alg_ctxt n =
  _fn_compare_op(get_hexBinary, hexBinary_equal) code_ctxt alg_ctxt n 

let _op_base64Binary_equal code_ctxt alg_ctxt n =
  _fn_compare_op(get_base64Binary, base64Binary_equal) code_ctxt alg_ctxt n 

let _op_hexBinary_nequal code_ctxt alg_ctxt n =
  _fn_compare_op(get_hexBinary, (fun x1 x2 -> not(hexBinary_equal x1 x2))) code_ctxt alg_ctxt n 

let _op_base64Binary_nequal code_ctxt alg_ctxt n =
  _fn_compare_op(get_base64Binary, (fun x1 x2 -> not(base64Binary_equal x1 x2))) code_ctxt alg_ctxt n 

(* xsd:integer *)
let _fn_integer_add code_ctxt alg_ctxt n =
  _fn_binary_op (get_integer, _integer_add, _integer, def_zero_test) code_ctxt alg_ctxt n

let _fn_integer_sub code_ctxt alg_ctxt n =
  _fn_binary_op (get_integer, _integer_sub, _integer, def_zero_test) code_ctxt alg_ctxt n

let _fn_integer_mult code_ctxt alg_ctxt n =
  _fn_binary_op (get_integer, _integer_mult, _integer, def_zero_test) code_ctxt alg_ctxt n

let _fn_integer_div code_ctxt alg_ctxt n =
  _fn_binary_op (get_integer, _integer_div, _decimal, integer_zero_test) code_ctxt alg_ctxt n

let _fn_integer_idiv code_ctxt alg_ctxt n =
  _fn_binary_op (get_integer, _integer_idiv, _integer, integer_zero_test) code_ctxt alg_ctxt n

let _fn_integer_mod code_ctxt alg_ctxt n =
  _fn_binary_op (get_integer, _integer_mod, _integer, def_zero_test) code_ctxt alg_ctxt n

let _fn_integer_unary_minus code_ctxt alg_ctxt n =
  _fn_unary_op (get_integer, _integer_unary_minus, _integer) code_ctxt alg_ctxt n

let _fn_integer_equal code_ctxt alg_ctxt n =
  _fn_compare_op(get_integer, _integer_eq) code_ctxt alg_ctxt n 

let _fn_integer_nequal code_ctxt alg_ctxt n =
  _fn_compare_op(get_integer, fun i1 i2 -> not(_integer_eq i1 i2)) code_ctxt alg_ctxt n 

let _fn_integer_lt code_ctxt alg_ctxt n =
  _fn_compare_op(get_integer, _integer_lt) code_ctxt alg_ctxt n 

let _fn_integer_gt code_ctxt alg_ctxt n =
  _fn_compare_op(get_integer, _integer_gt) code_ctxt alg_ctxt n 

let _fn_integer_le code_ctxt alg_ctxt n =
  _fn_compare_op(get_integer, _integer_le) code_ctxt alg_ctxt n 

let _fn_integer_ge code_ctxt alg_ctxt n =
  _fn_compare_op(get_integer, _integer_ge) code_ctxt alg_ctxt n 

(* unary plus is a no-op *)
let _fn_integer_unary_plus code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  p1 

(* xsd:decimal *)
let _fn_decimal_add code_ctxt alg_ctxt n =
  _fn_binary_op (get_decimal, _decimal_add, _decimal, def_zero_test) code_ctxt alg_ctxt n

let _fn_decimal_sub code_ctxt alg_ctxt n =
  _fn_binary_op (get_decimal, _decimal_sub, _decimal, def_zero_test) code_ctxt alg_ctxt n

let _fn_decimal_mult code_ctxt alg_ctxt n =
  _fn_binary_op (get_decimal, _decimal_mult, _decimal, def_zero_test) code_ctxt alg_ctxt n

let _fn_decimal_div code_ctxt alg_ctxt n =
  _fn_binary_op (get_decimal, _decimal_div, _decimal, decimal_zero_test) code_ctxt alg_ctxt n

let _fn_decimal_idiv code_ctxt alg_ctxt n =
  _fn_binary_op (get_decimal, _decimal_idiv, _integer, decimal_zero_test) code_ctxt alg_ctxt n

let _fn_decimal_mod code_ctxt alg_ctxt n =
  _fn_binary_op (get_decimal, _decimal_mod, _decimal, def_zero_test) code_ctxt alg_ctxt n

let _fn_decimal_unary_minus code_ctxt alg_ctxt n =
  _fn_unary_op (get_decimal, _decimal_unary_minus, _decimal) code_ctxt alg_ctxt n

let _fn_decimal_equal code_ctxt alg_ctxt n =
  _fn_compare_op(get_decimal, _decimal_eq) code_ctxt alg_ctxt n 

let _fn_decimal_nequal code_ctxt alg_ctxt n =
  _fn_compare_op(get_decimal, fun i1 i2 -> not (_decimal_eq i1 i2)) code_ctxt alg_ctxt n 

let _fn_decimal_lt code_ctxt alg_ctxt n =
  _fn_compare_op(get_decimal, _decimal_lt) code_ctxt alg_ctxt n 

let _fn_decimal_gt code_ctxt alg_ctxt n =
  _fn_compare_op(get_decimal, _decimal_gt) code_ctxt alg_ctxt n 

let _fn_decimal_le code_ctxt alg_ctxt n =
  _fn_compare_op(get_decimal, _decimal_le) code_ctxt alg_ctxt n 

let _fn_decimal_ge code_ctxt alg_ctxt n =
  _fn_compare_op(get_decimal, _decimal_ge) code_ctxt alg_ctxt n 

(* unary plus is a no-op *)
let _fn_decimal_unary_plus code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  p1 

(* xsd:float *)
let _fn_float_add code_ctxt alg_ctxt n =
  _fn_binary_op (get_float, (fun i1 i2 -> i1 +. i2), _float, def_zero_test) code_ctxt alg_ctxt n

let _fn_float_sub code_ctxt alg_ctxt n =
  _fn_binary_op (get_float, (fun i1 i2 -> i1 -. i2), _float, def_zero_test) code_ctxt alg_ctxt n

let _fn_float_mult code_ctxt alg_ctxt n =
  _fn_binary_op (get_float, (fun i1 i2 -> i1 *. i2), _float, def_zero_test) code_ctxt alg_ctxt n

let _fn_float_div code_ctxt alg_ctxt n =
  _fn_binary_op (get_float, (fun i1 i2 -> i1 /. i2), _float, def_zero_test) code_ctxt alg_ctxt n

let _fn_float_idiv code_ctxt alg_ctxt n =
  _fn_binary_op (get_float, _float_idiv, _integer, float_nan_zero_test) code_ctxt alg_ctxt n

let _fn_float_mod code_ctxt alg_ctxt n =
  _fn_binary_op (get_float, _float_mod, _float, def_zero_test) code_ctxt alg_ctxt n

let _fn_float_unary_minus code_ctxt alg_ctxt n =
  _fn_unary_op (get_float, (fun x -> (-.(x))), _float) code_ctxt alg_ctxt n

let _fn_float_equal code_ctxt alg_ctxt n =
  _fn_compare_op(get_float, fun i1 i2 -> i1 = i2) code_ctxt alg_ctxt n 

(* NB! Must use not(i1 = i2).  i1 != i2 checks for physical equivalence. *)
let _fn_float_nequal code_ctxt alg_ctxt n =
  _fn_compare_op(get_float, fun i1 i2 -> not(i1 = i2)) code_ctxt alg_ctxt n 

let _fn_float_lt code_ctxt alg_ctxt n =
  _fn_compare_op(get_float, fun i1 i2 -> i1 < i2) code_ctxt alg_ctxt n 

let _fn_float_gt code_ctxt alg_ctxt n =
  _fn_compare_op(get_float, fun i1 i2 -> i1 > i2) code_ctxt alg_ctxt n 

let _fn_float_le code_ctxt alg_ctxt n =
  _fn_compare_op(get_float, fun i1 i2 -> i1 <= i2) code_ctxt alg_ctxt n 

let _fn_float_ge code_ctxt alg_ctxt n =
  _fn_compare_op(get_float, fun i1 i2 -> i1 >= i2) code_ctxt alg_ctxt n 

(* unary plus is a no-op *)
let _fn_float_unary_plus code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1  n in
  p1 

(* xsd:double *)
let _fn_double_add code_ctxt alg_ctxt n =
  _fn_binary_op (get_double, (fun i1 i2 -> i1 +. i2), _double, def_zero_test) code_ctxt alg_ctxt n

let _fn_double_sub code_ctxt alg_ctxt n =
  _fn_binary_op (get_double, (fun i1 i2 -> i1 -. i2), _double, def_zero_test) code_ctxt alg_ctxt n

let _fn_double_mult code_ctxt alg_ctxt n =
  _fn_binary_op (get_double, (fun i1 i2 -> i1 *. i2), _double, def_zero_test) code_ctxt alg_ctxt n

let _fn_double_div code_ctxt alg_ctxt n =
  _fn_binary_op (get_double, (fun i1 i2 -> i1 /. i2), _double, def_zero_test) code_ctxt alg_ctxt n

let _fn_double_idiv code_ctxt alg_ctxt n =
  _fn_binary_op (get_double, _float_idiv, _integer, float_nan_zero_test) code_ctxt alg_ctxt n

let _fn_double_mod code_ctxt alg_ctxt n =
  _fn_binary_op (get_double, _float_mod, _double, def_zero_test) code_ctxt alg_ctxt n

let _fn_double_unary_minus code_ctxt alg_ctxt n =
  _fn_unary_op (get_double, (fun x -> (-.(x))), _double) code_ctxt alg_ctxt n

let _fn_double_equal code_ctxt alg_ctxt n =
  _fn_compare_op(get_double, fun i1 i2 -> i1 = i2) code_ctxt alg_ctxt n 

(* NB! Must use not(i1 = i2).  i1 != i2 checks for physical equivalence. *)
let _fn_double_nequal code_ctxt alg_ctxt n =
  _fn_compare_op(get_double, fun i1 i2 -> not(i1 = i2)) code_ctxt alg_ctxt n 

let _fn_double_lt code_ctxt alg_ctxt n =
  _fn_compare_op(get_double, fun i1 i2 -> i1 < i2) code_ctxt alg_ctxt n 

let _fn_double_gt code_ctxt alg_ctxt n =
  _fn_compare_op(get_double, fun i1 i2 -> i1 > i2) code_ctxt alg_ctxt n 

let _fn_double_le code_ctxt alg_ctxt n =
  _fn_compare_op(get_double, fun i1 i2 -> i1 <= i2) code_ctxt alg_ctxt n 

let _fn_double_ge code_ctxt alg_ctxt n =
  _fn_compare_op(get_double, fun i1 i2 -> i1 >= i2) code_ctxt alg_ctxt n 

(* unary plus is a no-op *)
let _fn_double_unary_plus code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1  n in
  p1 

(* F&O Section 6.4.1 fn:abs *)
let _fn_abs_double code_ctxt alg_ctxt n = 
  let s  = Args.get_array_param1  n in
  let f  = get_optional_double s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f -> Cursor.cursor_of_singleton (_double (abs_float f))

let _fn_abs_float code_ctxt alg_ctxt n = 
  let s  = Args.get_array_param1 n in
  let f  = get_optional_float s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f -> Cursor.cursor_of_singleton (_float (abs_float f))

let _fn_abs_decimal code_ctxt alg_ctxt n = 
  let s  = Args.get_array_param1 n in
  let f  = get_optional_decimal s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f -> Cursor.cursor_of_singleton (_decimal (Num.abs_num f))

let _fn_abs_integer code_ctxt alg_ctxt n = 
  let s  = Args.get_array_param1 n in
  let i = get_optional_integer s in
  match i with
  | None -> Cursor.cursor_empty()
  | Some i -> Cursor.cursor_of_singleton (_integer (Big_int.abs_big_int i))

(* F&O Section 6.4.2 fn:ceiling *)

let _fn_ceiling_double code_ctxt alg_ctxt n = 
  let s  = Args.get_array_param1 n in
  let f  = get_optional_double s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f ->
      let f' = ceil f in
      Cursor.cursor_of_singleton (_double f')

let _fn_ceiling_float code_ctxt alg_ctxt n = 
  let s  = Args.get_array_param1 n in
  let f  = get_optional_float s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f ->
      let f' = ceil f in
      Cursor.cursor_of_singleton (_float f')

let _fn_ceiling_decimal code_ctxt alg_ctxt n = 
  let s  = Args.get_array_param1 n in
  let f  = get_optional_decimal s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f ->
      let f' = Num.ceiling_num f in
      Cursor.cursor_of_singleton (_decimal f')

let _fn_ceiling_integer code_ctxt alg_ctxt n = 
  let s  = Args.get_array_param1 n in
  let f = get_optional_integer s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f ->
      s

(* F&O Section 6.4.3 fn:floor *)
let _fn_floor_double code_ctxt alg_ctxt n =
  let s  = Args.get_array_param1 n in
  let f  = get_optional_double s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f ->
      let f' = floor f in
      Cursor.cursor_of_singleton (_double f')

let _fn_floor_float code_ctxt alg_ctxt n =
  let s  = Args.get_array_param1 n in
  let f  = get_optional_float s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f ->
      let f' = floor f in
      Cursor.cursor_of_singleton (_float f')

let _fn_floor_decimal code_ctxt alg_ctxt n =
  let s  = Args.get_array_param1 n in
  let f  = get_optional_decimal s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f ->
      let f' = Num.floor_num f in
      Cursor.cursor_of_singleton (_decimal f')

let _fn_floor_integer code_ctxt alg_ctxt n =
  let s  = Args.get_array_param1 n in
  let f = get_optional_integer s in 
  match f with
  | None -> Cursor.cursor_empty()
  | Some f ->
      s

(* F&O Section 6.4.4 fn:round *)

let _fn_round_double code_ctxt alg_ctxt n = 
  let s  = Args.get_array_param1  n in
  let f  = get_optional_double s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f ->
      let f' = floor (f +. 0.5) in
      Cursor.cursor_of_singleton (_double f')

let _fn_round_float code_ctxt alg_ctxt n = 
  let s  = Args.get_array_param1  n in
  let f  = get_optional_float s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f ->
      let f' = floor (f +. 0.5) in
      Cursor.cursor_of_singleton (_float f')

let _fn_round_decimal code_ctxt alg_ctxt n = 
  let s  = Args.get_array_param1  n in
  let f  = get_optional_decimal s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f ->
      let f' = Num.floor_num (_decimal_add f _decimal_onehalf) in
      Cursor.cursor_of_singleton (_decimal f')

let _fn_round_integer code_ctxt alg_ctxt n = 
  let s  = Args.get_array_param1 n in
  let f  = get_optional_integer s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f ->
      s

(* F&O Section 6.4.4 fn:round-half-to-even *)

let _fn_round_half_to_even_double code_ctxt alg_ctxt n =
  let s,p = Args.get_array_param2 n in
  let f   = get_optional_double s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f ->
      let pr  = 10.0 ** (_float_of_integer (get_integer p)) in
      let bef = f *. pr in
      let flo = floor (bef +. 0.5) in
      let cei = ceil (bef -. 0.5) in
      let f'  =
	if (flo = cei)
	then flo
	else if (mod_float flo 2.0) = 0.0 then flo else cei
      in
      let aft = f' /. pr in
      Cursor.cursor_of_singleton (_double aft)

let _fn_round_half_to_even_float code_ctxt alg_ctxt n =
  let s,p = Args.get_array_param2 n in
  let f   = get_optional_float s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f ->
      let pr  = 10.0 ** (_float_of_integer (get_integer p)) in
      let bef = f *. pr in
      let flo = floor (bef +. 0.5) in
      let cei = ceil (bef -. 0.5) in
      let f'  =
	if (flo = cei)
	then flo
	else if (mod_float flo 2.0) = 0.0 then flo else cei
      in
      let aft = f' /. pr in
      Cursor.cursor_of_singleton (_float aft)

let _fn_round_half_to_even_decimal code_ctxt alg_ctxt n =
  let s,p = Args.get_array_param2 n in
  let f   = get_optional_decimal s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f ->
      let pr  = _decimal_power _decimal_ten (_decimal_of_integer (get_integer p)) in
      let bef = _decimal_mult f pr in
      let flo = _decimal_floor (_decimal_add bef _decimal_onehalf) in
      let cei = _decimal_ceil (_decimal_sub bef _decimal_onehalf) in
      let f'  =
	if (_decimal_eq flo cei)
	then flo
	else if _decimal_eq (_decimal_mod flo _decimal_two) _decimal_zero
	then flo
	else cei
      in
      let aft = _decimal_div f' pr in
      Cursor.cursor_of_singleton (_decimal aft)

let _fn_round_half_to_even_integer code_ctxt alg_ctxt n =
  let s,p = Args.get_array_param2 n in
  let f   = get_optional_integer s in
  match f with
  | None -> Cursor.cursor_empty()
  | Some f ->
      s


(***************************************)
(* F&O Section 7. Functions on Strings *)
(***************************************)

(* F&O Section 7.3 equality and comparison of strings *)

(** Gives the unboxed string value of p1, or "" if p1 is passed as ().
   All of the xquery functions on strings require an empty sequence to be
   treated as an empty string. This function is used in place of the get_string
   function in order to do so.
   @param code_ctxt alg_ctxt The algebra context
   @param p1 The string to be unboxed, or the empty sequence
   @return The string value of p1 or else "" if p1 is empty *)

let normalize_optional_string p1 =
  if (Cursor.cursor_is_empty p1)
  then ""
  else get_string p1 

(** Unboxes a dayTimeDuration in order to represent a timezone. If timezone is
   passed as the empty sequence, then it gets the timezone provided in the
   algebra context, which is the specified behavior for many operations on dates
   and times dealing with timezones
   @param code_ctxt alg_ctxt The algebra context
   @param tz1 The timezone
   @return The unboxed timezone or the timezone provided by the algebra context if
   no timezone is provided *)

(*
   A dynamic error is raised [err:FODT0003] if $timezone is less than
   -PT14H or greater than PT14H or if does not contain an integral
   number of minutes.
*)
let assert_valid_timezone tz = 
  let hrs = DateTime.hours_from_duration tz in
  let secs = DateTime.seconds_from_duration tz in 
  if (hrs < -14 || hrs > 14 || not(Decimal._decimal_eq secs Decimal._decimal_zero)) then
    raise(Query(Validation("Invalid timezone value \""^(DateTime.string_of_dayTimeDuration tz)^"\"")))
  else
    tz

let get_optional_timezone code_ctxt alg_ctxt tz1 =
  if (Cursor.cursor_is_empty tz1)
  then None
  else Some (assert_valid_timezone(get_dayTimeDuration tz1))

(* Right now. all collations must be the default  *)
(* See also Processing_context.known_collation *)
let check_collation code_ctxt coll = 
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let mod_proc_ctxt = Norm_context.module_context_from_norm_context norm_ctxt in
  let uri = mod_proc_ctxt.Processing_context.default_collation in
  if (uri = coll) then coll
  else raise(Query(Unknown("[err:FOCH0002] Unsupported collation "^coll)))

let _fn_compare code_ctxt alg_ctxt n = 
  let (p1,p2,p3) = Args.get_array_param3 n in
  let _ = check_collation code_ctxt (get_string p3) in 
  let n' = [|p1;p2|] in 
  let compop i1 i2 = Decimal._integer_of_int (String.compare i1 i2) in
  _fn_binary_op (get_string,compop,_integer,def_zero_test) code_ctxt alg_ctxt n'

let _op_string_equal code_ctxt alg_ctxt n =
  _fn_compare_op(normalize_optional_string, string_equal) code_ctxt alg_ctxt n 

let _op_string_nequal code_ctxt alg_ctxt n =
  _fn_compare_op(normalize_optional_string, fun i1 i2 -> not (string_equal i1 i2)) code_ctxt alg_ctxt n 

let _op_string_lt code_ctxt alg_ctxt n =
  _fn_compare_op(normalize_optional_string, string_lt) code_ctxt alg_ctxt n 

let _op_string_le code_ctxt alg_ctxt n =
  _fn_compare_op(normalize_optional_string, string_lteq) code_ctxt alg_ctxt n 

let _op_string_gt code_ctxt alg_ctxt n =
  _fn_compare_op(normalize_optional_string, string_gt) code_ctxt alg_ctxt n 

let _op_string_ge code_ctxt alg_ctxt n =
  _fn_compare_op(normalize_optional_string, string_gteq) code_ctxt alg_ctxt n 

let _fn_max_string code_ctxt alg_ctxt n =
  let compop i1 i2 = String.compare i1 i2 in
  _fn_max_poly(get_string_cursor, compop, _string) code_ctxt alg_ctxt n

let _fn_min_string code_ctxt alg_ctxt n =
  let compop i1 i2 = String.compare i1 i2 in
  _fn_min_poly(get_string_cursor, compop, _string) code_ctxt alg_ctxt n


(* F&O Section 7.4.1 fn:concat *)

let _fn_concat code_ctxt alg_ctxt n =
  let (s1,s2) = Args.get_array_param2 n in
  let a1 = get_optional_atomic s1 in
  let a2 = get_optional_atomic s2 in
  let s1 =
    match a1 with
    | None -> ""
    | Some at -> (at#cast_to Namespace_context.default_xquery_nsenv Namespace_symbols_builtin.xs_string ATString)#getAtomicString()
  in
  let s2 =
    match a2 with
    | None -> ""
    | Some at -> (at#cast_to Namespace_context.default_xquery_nsenv Namespace_symbols_builtin.xs_string ATString)#getAtomicString()
  in
  Cursor.cursor_of_singleton (_string (s1 ^ s2))

(* F&O Section 7.4.2 fn:string-join *)

let _fn_string_join code_ctxt alg_ctxt n =
  let (s1, s2) = (Args.get_array_param2 n) in
  (* Note:
       fn:string-join is more efficient using the String.concat
       function, which only does one string allocation. This means we
       first do a materialization to a list.
     - Jerome *)
  let l1 = Cursor.list_of_cursor "Cs_code_fn._fn_string_join" (get_string_cursor s1) in
  let concatenated_string = String.concat (get_string s2) l1 in
  Cursor.cursor_of_singleton (_string concatenated_string)

(* F&O Section 7.4.3 fn:substring *)

(** Returns the portion of the value of $sourceString beginning at the
  position indicated by the value of $startingLoc and continuing for
  the number of characters indicated by the value of $length. The
  characters returned do not extend beyond $sourceString. If
  $startingLoc is zero or negative, only those characters in positions
  greater than zero are returned.

  The three argument version of the function
  returns the characters in $sourceString whose position $p obeys:

    fn:round($startingLoc) <= $p < fn:round($startingLoc) + fn:round($length)

  The two argument version of the function assumes that$length is
  infinite and returns the characters in $sourceString whose position
  $p obeys:

    fn:round($startingLoc) <= $p < fn:round(INF)

   @param code_ctxt alg_ctxt The algebra context, unused in this function
   @param n The arguments
   @returns A cursor containing the substring 
*)
let _fn_substring code_ctxt alg_ctxt n =
  let (str, start, len) = Args.get_array_param3 n in
  if Cursor.cursor_is_empty str then 
    Cursor.cursor_of_singleton (_string "")
  else
      (* The three argument version of the function
	 returns the characters in $sourceString whose position $p obeys: 
	 fn:round($startingLoc) <= $p < fn:round($startingLoc) + fn:round($length)

	 The two argument version of the function assumes that$length is
	 infinite and returns the characters in $sourceString whose position
	 $p obeys:

	 fn:round($startingLoc) <= $p < fn:round(INF)
      *)
    begin
      let str' = normalize_optional_string str in
      let startpos' = (floor ((get_double start) +. 0.5)) in
      let endpos' = startpos' +. (floor ((get_double len) +. 0.5)) in
      (* substring indexes start at position 1 in XQuery, but 0 in Caml. - Jerome *)
      let startpos'' = if startpos' > 0.0 then int_of_float(startpos') - 1 else 0 in
      let len' = 
	(if endpos' = nan then 0
 	else if endpos' = infinity then (String.length str')
 	else int_of_float(endpos') - 1) - startpos''
      in
      let sub =
	try
          String.sub str' startpos'' len'
	with
	| Invalid_argument _ -> ""
      in
      Cursor.cursor_of_singleton (_string sub)
    end


(* F&O Section 7.4.4 fn:string-length *)

let _fn_string_length code_ctxt alg_ctxt n =
  let s  = Args.get_array_param1 n in
  if Cursor.cursor_is_empty s then 
    (*Cursor.cursor_empty()*)
    Cursor.cursor_of_singleton(_integer(Decimal._integer_of_int 0))
  else
    begin
      let s' = normalize_optional_string s in
      let i = String.length s' in
      Cursor.cursor_of_singleton (_integer (Decimal._integer_of_int i))
    end

(* F&O Section 7.4.5 fn:normalize-space *)

let _fn_normalize_space code_ctxt alg_ctxt n =
  let s  = Args.get_array_param1 n in
  let str = 
    if Cursor.cursor_is_empty s
    then ""
    else Whitespace.normalize_space (normalize_optional_string s)
  in Cursor.cursor_of_singleton (_string str)

(* F&O Section 7.4.6 fn:normalize-unicode *)

let _fn_normalize_unicode code_ctxt alg_ctxt n =
  let p1,p2 = Args.get_array_param2 n in
  let s1 = get_optional_string p1 in
  let norm_fun =
    let norm = String.uppercase (Whitespace.remove_whitespace (get_string p2)) in
    match norm with
    | "" -> (fun x -> x)
    | "NFC" -> Galax_camomile.nfc
    | "NFD" -> Galax_camomile.nfd
    | "NFKC" -> Galax_camomile.nfkc
    | "NFKD" -> Galax_camomile.nfkd
    | _ -> raise (Query (Wrong_Args ("Unicode normalization " ^norm^ " unknown")))
  in
  let ns =
    match s1 with
    | None -> ""
    | Some s1 ->
	norm_fun s1
  in
  Cursor.cursor_of_singleton (_string ns)

(* F&O Section 7.4.7 fn:upper-case *)

let _fn_upper_case code_ctxt alg_ctxt n =
  let s   = Args.get_array_param1 n in
  let string_result =
  if Cursor.cursor_is_empty s then 
    ""
  else
    let s'  = normalize_optional_string s in
    String.uppercase s' 
  in
  Cursor.cursor_of_singleton (_string string_result)

(* F&O Section 7.4.8 fn:lower-case *)

let _fn_lower_case code_ctxt alg_ctxt n =
  let s   = Args.get_array_param1 n in
  let string_result =
  if Cursor.cursor_is_empty s then 
    ""
  else
    let s'  = normalize_optional_string s in
    String.lowercase s'
  in
  Cursor.cursor_of_singleton (_string string_result)

(* F&O Section 7.4.9 fn:translate *)

let _translate_aux str mapStr transStr =
  let char_stream = Stream.of_string str in
  let buffer = Buffer.create 0 in
  let new_char c = Buffer.add_char buffer c in
  let translate_char c =
    try
      let index = String.index mapStr c in
      begin
	try
	  new_char (String.get transStr index)
	with
	| Invalid_argument _ ->
	    ()
      end
    with
    | Not_found ->
	new_char c
  in
  while not(Stream.peek char_stream = None) do
    translate_char (Stream.next char_stream)
  done;
  Buffer.contents buffer
    
let _fn_translate code_ctxt alg_ctxt n =
  let (str, mapStr, transStr) = Args.get_array_param3 n in
  let string_result =
    if Cursor.cursor_is_empty str then 
      ""
    else
      begin
	let str'       = normalize_optional_string  str      in
	let mapStr'    = normalize_optional_string  mapStr   in
	let transStr'  = normalize_optional_string  transStr in
	_translate_aux str' mapStr' transStr'
    end
  in Cursor.cursor_of_singleton (_string string_result)

(* F&O Section 7.4.10 fn:encode-for-uri *)

let _fn_encode_for_uri code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let s = normalize_optional_string p1 in
  let ns = AnyURI.encode_string_for_uri s in
  Cursor.cursor_of_singleton (_string ns)

(* F&O Section 7.4.11 fn:iri-to-uri *)

let _fn_iri_to_uri code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let s = normalize_optional_string p1 in
  let ns = AnyURI.encode_iri_to_uri s in
  Cursor.cursor_of_singleton (_string ns)

(* F&O Section 7.4.12 fn:escape-html-uri *)

let _fn_escape_html_uri code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let s = normalize_optional_string p1 in
  let ns = AnyURI.encode_html_uri s in
  Cursor.cursor_of_singleton (_string ns)

(* F&O Section 7.2.1 fn:codepoints-to-string 

   Summary: Creates an xs:string from a sequence of [The Unicode
   Standard] code points. Returns the zero-length string if $arg is
   the empty sequence. If any of the code points in $arg is not a
   legal XML character, an error is raised [err:FOCH0001].

   From XML Rec 2.2 Characters

   Character Range

   [2] Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] |
                [#x10000-#x10FFFF] 
   /* any Unicode character, excluding the surrogate blocks, FFFE, and FFFF. */
*)
   

let ix9 = _integer_of_int 0x9
let ixA = _integer_of_int 0xA
let ixD = _integer_of_int 0xD
let ix20 = _integer_of_int 0x20
let ixD7FF = _integer_of_int 0xD7FF
let ixE000 = _integer_of_int 0xE000
let ixFFFD = _integer_of_int 0xFFFD
let ix10000 = _integer_of_int 0x10000
let ix10FFFF = _integer_of_int 0x10FFFF

let valid_xml_unicode c = (_integer_eq c ix9) || (_integer_eq c ixA) || (_integer_eq c ixD) || 
   (_integer_ge c ix20 && _integer_le c ixD7FF) || (_integer_ge c ixE000 && _integer_le c ixFFFD) || 
   (_integer_ge c ix10000 && _integer_le c ix10FFFF)

let _fn_codepts_to_string code_ctxt alg_ctxt n =
  let p1   = Args.get_array_param1 n in
  let ptlist = Cursor.list_of_cursor "" (get_integer_cursor p1) in
  match ptlist with
  | [] -> Cursor.cursor_empty()
  | _ -> 
   begin
     let badc = List.filter (fun c -> not(valid_xml_unicode c)) ptlist in 
     if (List.length badc = 0) then 
       let c = List.map _int_of_integer ptlist in
       let s = Galax_camomile.utf8_string_of_code_points c in
       Cursor.cursor_of_singleton (_string s)
     else
       raise(Query(Unicode_Error("[err:FOCH0001] Illegal XML characters: "^(String.concat " "(List.map _string_of_integer badc)))))
   end

(* F&O Section 7.3.3 fn:codepoint-equal *)
let _fn_codepoint_equal code_ctxt alg_ctxt n =
  let compop x y = ((Galax_camomile.utf8_codepoint_compare x y)= 0) in
  _fn_binary_op (get_string,compop,_boolean,def_zero_test) code_ctxt alg_ctxt n
  

(* F&O Section 7.2.2 fn:string-to-codepoints *)

let _fn_string_to_codepts code_ctxt alg_ctxt n =
  let s   = Args.get_array_param1 n in
  if Cursor.cursor_is_empty s then 
    Cursor.cursor_empty()
  else
    let s'  = get_string s in
    if (s' = "") then Cursor.cursor_empty()
    else
      begin
	let codept_list = Galax_camomile.utf8_code_points_of_string s' in
	Cursor.cursor_of_list (List.map (fun i -> _integer (Decimal._integer_of_int i)) codept_list)
      end
(* F&O Section 7.5.1 fn:contains *)

let _fn_contains code_ctxt alg_ctxt n =
  let (s1, s2, s3) = Args.get_array_param3  n in
  begin
    let s1' = normalize_optional_string s1 in
    let s2' = normalize_optional_string s2 in
    let _ = check_collation code_ctxt (get_string s3) in 
    let b = Regularexp.matches s1' s2' "" in
      Cursor.cursor_of_singleton (_boolean b)
  end

(* F&O Section 7.5.2 fn:starts-with *)

let _fn_starts_with code_ctxt alg_ctxt n = 
  let (s1, s2, s3) = Args.get_array_param3 n in
  begin
      let s1' = normalize_optional_string s1 in
      let s2' = normalize_optional_string s2 in
      let _ = check_collation code_ctxt (get_string s3) in 
      let lens2' = String.length s2' in 
      let b = 
	try 
	  let s' = if (s1' = "") && (s2' = "") then s2' else String.sub s1' 0 lens2' in (s' = s2')
	with _ -> false
      in
      Cursor.cursor_of_singleton (_boolean b)
    end

(* F&O Section 7.5.3 fn:ends-with *)
(* We just ignore collation sequence right now *)
let _fn_ends_with code_ctxt alg_ctxt n = 
  let (s1, s2, s3) = Args.get_array_param3 n in
  begin
    let s1' = normalize_optional_string s1 in
    let s2' = normalize_optional_string s2 in
    let _ = check_collation code_ctxt (get_string s3) in 
    let lens1' = String.length s1' in
    let lens2' = String.length s2' in 
    let pos = lens1' - lens2' in 
    let b = 
      try 
	let s' = if (s1' = "") && (s2' = "") then s2' else String.sub s1' pos lens2' in (s' = s2')
      with _ -> false
    in
    Cursor.cursor_of_singleton (_boolean b)
  end

(* F&O Section 7.5.4 fn:substring-before *)

(** Gives the substring that occurs before the given character in a given string.
   Use the regular expression library to tokenize the string using the given character
   as a delimeter, and then take the head of the returned list as the substring to be returned
   @param comp_ctxt alg_ctxt The algebra context
   @param n The arguments
   @return A cursor containing the substring before the given character *)
let _fn_substring_before code_ctxt alg_ctxt n =
  let (str, substr, s3) = Args.get_array_param3 n in
  begin
    let str' = normalize_optional_string str in
    let substr' = normalize_optional_string substr in
    let _ = check_collation code_ctxt (get_string s3) in 
    if (substr' = "") then 
      Cursor.cursor_of_singleton (_string "")
    else
      let rex = Str.regexp (Str.quote substr') in
      let split = (Str.bounded_split_delim rex str' 2) in
      let resstr = 
	if (List.length split < 2) then ""
	else List.hd split
      in Cursor.cursor_of_singleton (_string resstr)
  end
    
(* F&O Section 7.5.5 fn:substring-after *)

(** Since the delimeter can only be one character based on the definition in
   F&O, the Str.bounded_split function can be used to split the string into
   two separate strings at the character specified. 
   @param comp_ctxt alg_ctxt The algebra context (not used in this case)
   @param n The arguments
   @return A cursor containing the substring after the given chracater *)
(* We just ignore collation sequence right now *)
let _fn_substring_after code_ctxt alg_ctxt n =
  let (str, substr, s3) = Args.get_array_param3 n in
  begin
    let str' = normalize_optional_string str in
    let substr' = normalize_optional_string substr in
    let _ = check_collation code_ctxt (get_string s3) in 
    if (substr' = "") then 
      Cursor.cursor_of_singleton (_string str')
    else
      let rex = Str.regexp (Str.quote substr') in
      let split = (Str.bounded_split_delim rex str' 2) in
      let resstr = 
	if (List.length split < 2) then ""
	else List.hd (List.tl split)
      in Cursor.cursor_of_singleton (_string resstr)
  end

(* glx:string-pad : previously was fn:string-pad *)
 let _glx_string_pad code_ctxt alg_ctxt n  =
   let (str, d) = Args.get_array_param2 n in
     if Cursor.cursor_is_empty str then
       Cursor.cursor_empty()
     else
   let str'   = normalize_optional_string str in
   let d'     = _int_of_integer (get_integer d) in
   let str'' = (String.make d' (String.get str' 0)) ^ str' in
   Cursor.cursor_of_singleton (_string str'')

(* F&0 Section 7.6 String Functions that use Pattern Matching *)

(** The matches, replace, and tokenize functions all use 
   the Regularexp library located in the datatypes folder *)

let _fn_matches code_ctxt alg_ctxt n = 
  let (input, pattern, flags) = Args.get_array_param3 n in
  begin
    let input' = normalize_optional_string input in
    let pattern' = get_string pattern in
    let flags' = get_string flags in
    let res = Regularexp.matches input' pattern' flags' in
    Cursor.cursor_of_singleton (_boolean res)
  end

let _fn_replace code_ctxt alg_ctxt n =
  let (input, pattern, reppattern, flags) = Args.get_array_param4 n in
  begin
    let input' = normalize_optional_string input in
    let pattern' = normalize_optional_string pattern in
    let reppatern' = normalize_optional_string reppattern in
    let flags' = normalize_optional_string flags in
    let res = Regularexp.replace input' pattern' reppatern' flags' in
    Cursor.cursor_of_singleton (_string res)
  end

let _fn_tokenize code_ctxt alg_ctxt n =
  let (input, pattern, flags) = Args.get_array_param3 n in
  begin
    let input' = normalize_optional_string input in
    let pattern' = normalize_optional_string pattern in
    let flags' = normalize_optional_string flags in
    let res = Regularexp.tokenize input' pattern' flags' in
    Cursor.cursor_of_list (List.map _string res)
  end
    
(******************************************************)
(* F&O Section 8. Functions and Operators on Booleans *)
(******************************************************)

(* F&O Section 8.1.1 fn:true *)

let _fn_true code_ctxt alg_ctxt n =
  Cursor.cursor_of_singleton (_boolean true)


(* F&O Section 8.1.2 fn:false *)

let _fn_false code_ctxt alg_ctxt n =
  Cursor.cursor_of_singleton (_boolean false)

(* F&O Section 8.3.1 fn:not *)

let _fn_not code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let b1 = get_boolean p1 in
  Cursor.cursor_of_singleton (_boolean (not b1))

let _op_boolean_equal code_ctxt alg_ctxt n =
  _fn_compare_op(get_boolean, fun i1 i2 -> i1 = i2) code_ctxt alg_ctxt n 

let _op_boolean_nequal code_ctxt alg_ctxt n =
  _fn_compare_op(get_boolean, fun i1 i2 -> i1 != i2) code_ctxt alg_ctxt n 

let _op_boolean_lt code_ctxt alg_ctxt n =
  _fn_compare_op(get_boolean, fun i1 i2 -> i1 < i2) code_ctxt alg_ctxt n 

let _op_boolean_gt code_ctxt alg_ctxt n =
  _fn_compare_op(get_boolean, fun i1 i2 -> i1 > i2) code_ctxt alg_ctxt n 

let _op_boolean_le code_ctxt alg_ctxt n =
  _fn_compare_op(get_boolean, fun i1 i2 -> i1 <= i2) code_ctxt alg_ctxt n 

let _op_boolean_ge code_ctxt alg_ctxt n =
  _fn_compare_op(get_boolean, fun i1 i2 -> i1 >= i2) code_ctxt alg_ctxt n 

(*******************************************************)
(* F&O Section 9. Functions on Dates, Times, Durations *)
(*******************************************************)
let _op_date_equal code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_date, date_equal (Some tz)) code_ctxt alg_ctxt n 

let _op_date_nequal code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_date, (fun x y -> not(date_equal (Some tz) x y))) code_ctxt alg_ctxt n 

let _op_date_lt code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_date, date_lt (Some tz)) code_ctxt alg_ctxt n 

let _op_date_le code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_date, date_lteq (Some tz)) code_ctxt alg_ctxt n 

let _op_date_gt code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_date, date_gt (Some tz)) code_ctxt alg_ctxt n 

let _op_date_ge code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_date, date_gteq (Some tz)) code_ctxt alg_ctxt n 

let _op_gYearMonth_equal code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_gYearMonth, gYearMonth_equal (Some tz)) code_ctxt alg_ctxt n 
let _op_gYearMonth_nequal code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_gYearMonth, (fun x y -> not(gYearMonth_equal (Some tz) x y))) code_ctxt alg_ctxt n 

let _op_gYear_equal code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_gYear, gYear_equal (Some tz)) code_ctxt alg_ctxt n 
let _op_gYear_nequal code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_gYear, (fun x y -> not(gYear_equal (Some tz) x y))) code_ctxt alg_ctxt n 

let _op_gMonthDay_equal code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_gMonthDay, gMonthDay_equal (Some tz)) code_ctxt alg_ctxt n 
let _op_gMonthDay_nequal code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_gMonthDay, (fun x y -> not(gMonthDay_equal (Some tz) x y))) code_ctxt alg_ctxt n 

let _op_gDay_equal code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_gDay, gDay_equal (Some tz)) code_ctxt alg_ctxt n 
let _op_gDay_nequal code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_gDay, (fun x y -> not(gDay_equal (Some tz) x y))) code_ctxt alg_ctxt n 

let _op_gMonth_equal code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_gMonth, gMonth_equal (Some tz)) code_ctxt alg_ctxt n 
let _op_gMonth_nequal code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_gMonth, (fun x y -> not(gMonth_equal (Some tz) x y))) code_ctxt alg_ctxt n 

let _op_add_yearMonthDuration_to_date code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_date, get_yearMonthDuration, DateTime.add_yearMonthDuration_to_date, _date, def_zero_test) code_ctxt alg_ctxt n
let _op_add_yearMonthDuration_to_date2 code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_yearMonthDuration, get_date, (fun x y -> DateTime.add_yearMonthDuration_to_date y x), _date, def_zero_test) code_ctxt alg_ctxt n

let _op_add_dayTimeDuration_to_date code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_date, get_dayTimeDuration, DateTime.add_dayTimeDuration_to_date, _date, def_zero_test) code_ctxt alg_ctxt n
let _op_add_dayTimeDuration_to_date2 code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_dayTimeDuration, get_date, (fun x y -> DateTime.add_dayTimeDuration_to_date y x), _date, def_zero_test) code_ctxt alg_ctxt n

let _op_subtract_yearMonthDuration_from_date code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_date, get_yearMonthDuration, DateTime.subtract_yearMonthDuration_from_date, _date, def_zero_test) code_ctxt alg_ctxt n

let _op_subtract_dayTimeDuration_from_date code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_date, get_dayTimeDuration, DateTime.subtract_dayTimeDuration_from_date, _date, def_zero_test) code_ctxt alg_ctxt n

let _op_time_equal code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_time, time_equal (Some tz)) code_ctxt alg_ctxt n 

let _op_time_nequal code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_time, (fun x y -> not(time_equal (Some tz) x y))) code_ctxt alg_ctxt n 

let _op_time_lt code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_time, time_lt (Some tz)) code_ctxt alg_ctxt n 

(***** LEFT OFF HERE *********)
let _op_time_le code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_time, time_lteq (Some tz)) code_ctxt alg_ctxt n 

let _op_time_gt code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_time, time_gt (Some tz)) code_ctxt alg_ctxt n 

let _op_time_ge code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op(get_time, time_gteq (Some tz)) code_ctxt alg_ctxt n 

let _op_add_dayTimeDuration_to_time code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_time, get_dayTimeDuration, DateTime.add_dayTimeDuration_to_time, _time, def_zero_test) code_ctxt alg_ctxt n
let _op_add_dayTimeDuration_to_time2 code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_dayTimeDuration, get_time, (fun x y -> DateTime.add_dayTimeDuration_to_time y x), _time, def_zero_test) code_ctxt alg_ctxt n

let _op_subtract_dayTimeDuration_from_time code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_time, get_dayTimeDuration, DateTime.subtract_dayTimeDuration_from_time, _time, def_zero_test) code_ctxt alg_ctxt n

let _op_subtract_dateTimes code_ctxt alg_ctxt n =
  _fn_binary_op
    (get_dateTime, DateTime.subtract_dateTimes, _dayTimeDuration, def_zero_test) code_ctxt alg_ctxt n

let _op_subtract_dates code_ctxt alg_ctxt n =
  _fn_binary_op
    (get_date, DateTime.subtract_dates, _dayTimeDuration, def_zero_test) code_ctxt alg_ctxt n

let _op_subtract_times code_ctxt alg_ctxt n =
  _fn_binary_op
    (get_time, DateTime.subtract_times, _dayTimeDuration, def_zero_test) code_ctxt alg_ctxt n

let _fn_min_date code_ctxt alg_ctxt n =
  _fn_min_poly(get_date_cursor, (DateTime.date_compare None), _date) code_ctxt alg_ctxt n

let _fn_min_time code_ctxt alg_ctxt n =
  _fn_min_poly(get_time_cursor, (DateTime.time_compare None), _time) code_ctxt alg_ctxt n

let _fn_min_dateTime code_ctxt alg_ctxt n =
  _fn_min_poly(get_dateTime_cursor, (DateTime.dateTime_compare None), _dateTime) code_ctxt alg_ctxt n

let _fn_max_date code_ctxt alg_ctxt n =
  _fn_max_poly(get_date_cursor, (DateTime.date_compare None), _date) code_ctxt alg_ctxt n

let _fn_max_time code_ctxt alg_ctxt n =
  _fn_max_poly(get_time_cursor, (DateTime.time_compare None), _time) code_ctxt alg_ctxt n

let _fn_max_dateTime code_ctxt alg_ctxt n =
  _fn_max_poly(get_dateTime_cursor, (DateTime.dateTime_compare None), _dateTime) code_ctxt alg_ctxt n

let _op_dateTime_equal code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op (get_dateTime, dateTime_equal (Some tz)) code_ctxt alg_ctxt n

let _op_dateTime_nequal code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op (get_dateTime, (fun x y -> not(dateTime_equal (Some tz) x y))) code_ctxt alg_ctxt n

let _op_dateTime_lt code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op (get_dateTime, dateTime_lt (Some tz)) code_ctxt alg_ctxt n

let _op_dateTime_le code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op (get_dateTime, dateTime_lteq (Some tz)) code_ctxt alg_ctxt n

let _op_dateTime_gt code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op (get_dateTime, dateTime_gt (Some tz)) code_ctxt alg_ctxt n

let _op_dateTime_ge code_ctxt alg_ctxt n =
  let tz = (get_timezone alg_ctxt)#getAtomicDayTimeDuration() in 
  _fn_compare_op (get_dateTime, dateTime_gteq (Some tz)) code_ctxt alg_ctxt n

let _op_add_dayTimeDuration_to_dateTime code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_dateTime, get_dayTimeDuration, DateTime.add_dayTimeDuration_to_dateTime, _dateTime, def_zero_test) code_ctxt alg_ctxt n
let _op_add_dayTimeDuration_to_dateTime2 code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_dayTimeDuration, get_dateTime, (fun x y -> DateTime.add_dayTimeDuration_to_dateTime y x), _dateTime, def_zero_test) code_ctxt alg_ctxt n

let _op_add_yearMonthDuration_to_dateTime code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_dateTime, get_yearMonthDuration, DateTime.add_yearMonthDuration_to_dateTime, _dateTime, def_zero_test) code_ctxt alg_ctxt n
let _op_add_yearMonthDuration_to_dateTime2 code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_yearMonthDuration, get_dateTime, DateTime.add_yearMonthDuration_to_dateTime2, _dateTime, def_zero_test) code_ctxt alg_ctxt n

let _op_subtract_dayTimeDuration_from_dateTime code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_dateTime, get_dayTimeDuration, DateTime.subtract_dayTimeDuration_from_dateTime, _dateTime, def_zero_test) code_ctxt alg_ctxt n

let _op_subtract_yearMonthDuration_from_dateTime code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_dateTime, get_yearMonthDuration, DateTime.subtract_yearMonthDuration_from_dateTime, _dateTime, def_zero_test) code_ctxt alg_ctxt n

let _op_yearMonthDuration_equal code_ctxt alg_ctxt n =
  _fn_compare_op (get_yearMonthDuration, yearMonthDuration_equal) code_ctxt alg_ctxt n

let _op_yearMonthDuration_nequal code_ctxt alg_ctxt n =
  _fn_compare_op (get_yearMonthDuration, (fun x y -> not (yearMonthDuration_equal x y))) code_ctxt alg_ctxt n

let _op_yearMonthDuration_lt code_ctxt alg_ctxt n =
  _fn_compare_op (get_yearMonthDuration, yearMonthDuration_lt) code_ctxt alg_ctxt n

let _op_yearMonthDuration_le code_ctxt alg_ctxt n =
  _fn_compare_op (get_yearMonthDuration, yearMonthDuration_lteq) code_ctxt alg_ctxt n

let _op_yearMonthDuration_gt code_ctxt alg_ctxt n =
  _fn_compare_op (get_yearMonthDuration, yearMonthDuration_gt) code_ctxt alg_ctxt n

let _op_yearMonthDuration_ge code_ctxt alg_ctxt n =
  _fn_compare_op (get_yearMonthDuration, yearMonthDuration_gteq) code_ctxt alg_ctxt n

let _op_subtract_yearMonthDurations code_ctxt alg_ctxt n =
  _fn_binary_op
    (get_yearMonthDuration, DateTime.subtract_yearMonthDurations, _yearMonthDuration, def_zero_test) code_ctxt alg_ctxt n

let _op_add_yearMonthDurations code_ctxt alg_ctxt n =
  _fn_binary_op
    (get_yearMonthDuration, DateTime.add_yearMonthDurations, _yearMonthDuration, def_zero_test) code_ctxt alg_ctxt n

let _op_multiply_yearMonthDuration code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_yearMonthDuration, get_double, DateTime.multiply_yearMonthDuration, _yearMonthDuration, def_zero_test) code_ctxt alg_ctxt n

let _op_multiply_yearMonthDuration2 code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_double, get_yearMonthDuration, DateTime.multiply_yearMonthDuration2, _yearMonthDuration, def_zero_test) code_ctxt alg_ctxt n

let _op_divide_yearMonthDuration code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_yearMonthDuration, get_double, DateTime.divide_yearMonthDuration, _yearMonthDuration, def_zero_test) code_ctxt alg_ctxt n

let _op_divide_yearMonthDuration_by_yearMonthDuration code_ctxt alg_ctxt n = 
  _fn_binary_op
    (get_yearMonthDuration, DateTime.divide_yearMonthDuration_by_yearMonthDuration, _decimal, def_zero_test) code_ctxt alg_ctxt n

let _fn_sum_yearMonthDuration_one code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let all_durs = get_yearMonthDuration_cursor p1 in
  match (Cursor.cursor_peek all_durs) with
  | None ->
      Cursor.cursor_of_singleton (_integer _integer_zero)
  | Some _ ->
      let ymzero = DateTime.mkyearMonthDuration (0, 0) in
      let sum = Cursor.cursor_fold_left (fun x0 x1 -> DateTime.add_yearMonthDurations x0 x1) ymzero all_durs in
      Cursor.cursor_of_singleton (_yearMonthDuration sum)

let _fn_sum_yearMonthDuration code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  let all_durs = get_yearMonthDuration_cursor p1 in
  let zero = get_optional_item p2 in
  match zero with
  | None -> Cursor.cursor_empty ()
  | Some p2 ->
    let ymzero = get_yearMonthDuration (Cursor.cursor_of_singleton p2) in
    let ymsum =
      match (Cursor.cursor_peek all_durs) with
      | None -> _yearMonthDuration ymzero
      | Some _ ->
	  let sum =
	    Cursor.cursor_fold_left
	      (fun x0 x1 -> DateTime.add_yearMonthDurations x0 x1) ymzero all_durs
	  in
	  _yearMonthDuration sum
    in
    Cursor.cursor_of_singleton ymsum

let _fn_min_yearMonthDuration code_ctxt alg_ctxt n =
  _fn_min_poly (get_yearMonthDuration_cursor, DateTime.yearMonthDuration_compare, _yearMonthDuration) code_ctxt alg_ctxt n

let _fn_max_yearMonthDuration code_ctxt alg_ctxt n =
  _fn_max_poly (get_yearMonthDuration_cursor, DateTime.yearMonthDuration_compare, _yearMonthDuration) code_ctxt alg_ctxt n

let _fn_avg_yearMonthDuration code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let allyearMonthDurations = get_yearMonthDuration_cursor p1 in
  match (Cursor.cursor_peek allyearMonthDurations) with
  | None ->
      Cursor.cursor_empty()
  | Some _ ->
      let length = ref 0.0 in
      let ymzero = DateTime.mkyearMonthDuration (0, 0) in
      let sum = Cursor.cursor_fold_left (fun x0 x1 -> length := !length +. 1.0; DateTime.add_yearMonthDurations x0 x1) ymzero allyearMonthDurations in
      Cursor.cursor_of_singleton (_yearMonthDuration (DateTime.divide_yearMonthDuration sum (!length)))

(* xs:dayTimeDurations *)
let _op_dayTimeDuration_equal code_ctxt alg_ctxt n =
  _fn_compare_op (get_dayTimeDuration, dayTimeDuration_equal) code_ctxt alg_ctxt n

let _op_dayTimeDuration_nequal code_ctxt alg_ctxt n =
  _fn_compare_op (get_dayTimeDuration, (fun x y -> not(dayTimeDuration_equal x y))) code_ctxt alg_ctxt n

let _op_dayTimeDuration_lt code_ctxt alg_ctxt n =
  _fn_compare_op (get_dayTimeDuration, dayTimeDuration_lt) code_ctxt alg_ctxt n

let _op_dayTimeDuration_le code_ctxt alg_ctxt n =
  _fn_compare_op (get_dayTimeDuration, dayTimeDuration_lteq) code_ctxt alg_ctxt n

let _op_dayTimeDuration_gt code_ctxt alg_ctxt n =
  _fn_compare_op (get_dayTimeDuration, dayTimeDuration_gt) code_ctxt alg_ctxt n

let _op_dayTimeDuration_ge code_ctxt alg_ctxt n =
  _fn_compare_op (get_dayTimeDuration, dayTimeDuration_gteq) code_ctxt alg_ctxt n

let _op_subtract_dayTimeDurations code_ctxt alg_ctxt n =
  _fn_binary_op
    (get_dayTimeDuration, DateTime.subtract_dayTimeDurations, _dayTimeDuration, def_zero_test) code_ctxt alg_ctxt n

let _op_add_dayTimeDurations code_ctxt alg_ctxt n =
  _fn_binary_op
    (get_dayTimeDuration, DateTime.add_dayTimeDurations, _dayTimeDuration, def_zero_test) code_ctxt alg_ctxt n

let _op_multiply_dayTimeDuration code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_dayTimeDuration, get_double, DateTime.multiply_dayTimeDuration, _dayTimeDuration, def_zero_test) code_ctxt alg_ctxt n
let _op_multiply_dayTimeDuration2 code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_double, get_dayTimeDuration, DateTime.multiply_dayTimeDuration2, _dayTimeDuration, def_zero_test) code_ctxt alg_ctxt n

let _op_divide_dayTimeDuration code_ctxt alg_ctxt n =
  _fn_binary_op_uneven
    (get_dayTimeDuration, get_double, DateTime.divide_dayTimeDuration, _dayTimeDuration, def_zero_test) code_ctxt alg_ctxt n

let _op_divide_dayTimeDuration_by_dayTimeDuration code_ctxt alg_ctxt n = 
  _fn_binary_op
    (get_dayTimeDuration, DateTime.divide_dayTimeDuration_by_dayTimeDuration, _decimal, def_zero_test) code_ctxt alg_ctxt n

(* xs:duration *)

let _op_duration_equal code_ctxt alg_ctxt n =
  _fn_compare_op (get_duration, duration_equal) code_ctxt alg_ctxt n

let _op_duration_nequal code_ctxt alg_ctxt n =
  _fn_compare_op (get_duration, (fun x y -> not (duration_equal x y))) code_ctxt alg_ctxt n

let _fn_sum_dayTimeDuration_one code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let all_durs = get_dayTimeDuration_cursor p1 in
  match (Cursor.cursor_peek all_durs) with
  | None ->
      Cursor.cursor_of_singleton (_integer _integer_zero)
  | Some _ ->
      let dtzero = DateTime.mkdayTimeDuration (0, 0, 0, Decimal._decimal_zero) in
      let sum = Cursor.cursor_fold_left (fun x0 x1 -> DateTime.add_dayTimeDurations x0 x1) dtzero all_durs in
      Cursor.cursor_of_singleton (_dayTimeDuration sum)

let _fn_sum_dayTimeDuration code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  let all_durs = get_dayTimeDuration_cursor p1 in
  let zero = get_optional_item p2 in
  match zero with
  | None -> Cursor.cursor_empty ()
  | Some p2 ->
  let dtzero = get_dayTimeDuration (Cursor.cursor_of_singleton p2) in
  let dtsum =
    match (Cursor.cursor_peek all_durs) with
    | None -> _dayTimeDuration dtzero
    | Some _ ->
	let sum = Cursor.cursor_fold_left (fun x0 x1 -> DateTime.add_dayTimeDurations x0 x1) dtzero all_durs in
	_dayTimeDuration sum
  in
  Cursor.cursor_of_singleton dtsum

let _fn_min_dayTimeDuration code_ctxt alg_ctxt n =
  _fn_min_poly (get_dayTimeDuration_cursor, DateTime.dayTimeDuration_compare, _dayTimeDuration) code_ctxt alg_ctxt n

let _fn_max_dayTimeDuration code_ctxt alg_ctxt n =
  _fn_max_poly (get_dayTimeDuration_cursor, DateTime.dayTimeDuration_compare, _dayTimeDuration) code_ctxt alg_ctxt n

let _fn_avg_dayTimeDuration code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let alldayTimeDurations = get_dayTimeDuration_cursor p1 in
  match (Cursor.cursor_peek alldayTimeDurations) with
  | None ->
      Cursor.cursor_empty()
  | Some _ ->
      let length = ref 0.0 in
      let dtzero = DateTime.mkdayTimeDuration (0, 0, 0, Decimal._decimal_zero) in
      let sum = Cursor.cursor_fold_left (fun x0 x1 -> length := !length +. 1.0; DateTime.add_dayTimeDurations x0 x1) dtzero alldayTimeDurations in
      Cursor.cursor_of_singleton (_dayTimeDuration (DateTime.divide_dayTimeDuration sum (!length)))

(*****************************************)
(* F&O Section 10.5 Date/Time Extraction *)
(*****************************************)

(* 
   *_from_duration functions are defined on all types derived from duration: yearMonthDuration & dayTimeDuration
*)
let _fn_years_from_duration code_ctxt alg_ctxt n =
  _fn_unary_op (get_ymd_from_duration, compose Decimal._integer_of_int DateTime.years_from_duration, _integer) code_ctxt alg_ctxt n

let _fn_months_from_duration code_ctxt alg_ctxt n =
  _fn_unary_op (get_ymd_from_duration, compose Decimal._integer_of_int DateTime.months_from_duration, _integer) code_ctxt alg_ctxt n

let _fn_days_from_duration code_ctxt alg_ctxt n =
  _fn_unary_op (get_dtd_from_duration, compose Decimal._integer_of_int DateTime.days_from_duration, _integer) code_ctxt alg_ctxt n

let _fn_hours_from_duration code_ctxt alg_ctxt n =
  _fn_unary_op (get_dtd_from_duration, compose Decimal._integer_of_int DateTime.hours_from_duration, _integer) code_ctxt alg_ctxt n

let _fn_minutes_from_duration code_ctxt alg_ctxt n =
  _fn_unary_op (get_dtd_from_duration, compose Decimal._integer_of_int DateTime.minutes_from_duration, _integer) code_ctxt alg_ctxt n

let _fn_seconds_from_duration code_ctxt alg_ctxt n =
  _fn_unary_op (get_dtd_from_duration, DateTime.seconds_from_duration, _decimal) code_ctxt alg_ctxt n

let _fn_year_from_dateTime code_ctxt alg_ctxt n =
  _fn_unary_op (get_dateTime, compose Decimal._integer_of_int DateTime.year_from_dateTime, _integer) code_ctxt alg_ctxt n

let _fn_month_from_dateTime code_ctxt alg_ctxt n =
  _fn_unary_op (get_dateTime, compose Decimal._integer_of_int DateTime.month_from_dateTime, _integer) code_ctxt alg_ctxt n

let _fn_day_from_dateTime code_ctxt alg_ctxt n =
  _fn_unary_op (get_dateTime, compose Decimal._integer_of_int DateTime.day_from_dateTime, _integer) code_ctxt alg_ctxt n

let _fn_hours_from_dateTime code_ctxt alg_ctxt n =
  _fn_unary_op (get_dateTime, compose Decimal._integer_of_int DateTime.hours_from_dateTime, _integer) code_ctxt alg_ctxt n

let _fn_minutes_from_dateTime code_ctxt alg_ctxt n =
  _fn_unary_op (get_dateTime, compose Decimal._integer_of_int DateTime.minutes_from_dateTime, _integer) code_ctxt alg_ctxt n

let _fn_seconds_from_dateTime code_ctxt alg_ctxt n =
  _fn_unary_op (get_dateTime, DateTime.seconds_from_dateTime, _decimal) code_ctxt alg_ctxt n

let _fn_timezone_from_dateTime code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  if (Cursor.cursor_is_empty p1)
  then Cursor.cursor_empty()
  else
    let d = get_dateTime p1 in
    match DateTime.opt_timezone_from_dateTime d with
    | None -> Cursor.cursor_empty()
    | Some tz -> Cursor.cursor_of_singleton (_dayTimeDuration tz)

let _fn_year_from_date code_ctxt alg_ctxt n =
  _fn_unary_op (get_date, compose Decimal._integer_of_int DateTime.year_from_date, _integer) code_ctxt alg_ctxt n

let _fn_month_from_date code_ctxt alg_ctxt n =
  _fn_unary_op (get_date, compose Decimal._integer_of_int DateTime.month_from_date, _integer) code_ctxt alg_ctxt n

let _fn_day_from_date code_ctxt alg_ctxt n =
  _fn_unary_op (get_date, compose Decimal._integer_of_int DateTime.day_from_date, _integer) code_ctxt alg_ctxt n

let _fn_timezone_from_date code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  if (Cursor.cursor_is_empty p1)
  then Cursor.cursor_empty()
  else
    let d = get_date p1 in
    match DateTime.opt_timezone_from_date d with
    | None -> Cursor.cursor_empty()
    | Some tz -> Cursor.cursor_of_singleton (_dayTimeDuration tz)

let _fn_hours_from_time code_ctxt alg_ctxt n =
  _fn_unary_op (get_time, compose Decimal._integer_of_int DateTime.hours_from_time, _integer) code_ctxt alg_ctxt n

let _fn_minutes_from_time code_ctxt alg_ctxt n =
  _fn_unary_op (get_time, compose Decimal._integer_of_int DateTime.minutes_from_time, _integer) code_ctxt alg_ctxt n

let _fn_seconds_from_time code_ctxt alg_ctxt n =
  _fn_unary_op (get_time, DateTime.seconds_from_time, _decimal) code_ctxt alg_ctxt n

let _fn_timezone_from_time code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  if (Cursor.cursor_is_empty p1)
  then Cursor.cursor_empty()
  else
    let d = get_time p1 in
    match DateTime.opt_timezone_from_time d with
    | None -> Cursor.cursor_empty()
    | Some tz -> Cursor.cursor_of_singleton (_dayTimeDuration tz)

(* F&O Section 10.7 Timezone adjustments *)

(*

   10.7.3 fn:adjust-time-to-timezone
   fn:adjust-time-to-timezone($arg as xs:time?) as xs:time?
   fn:adjust-time-to-timezone( 	$arg 	 as xs:time?, $timezone 	 as xs:dayTimeDuration?) as xs:time?

   Summary: Adjusts an xs:time value to a specific timezone, or to no
   timezone at all. If $timezone is the empty sequence, returns an
   xs:time without a timezone. Otherwise, returns an xs:time with a
   timezone.

   If $timezone is not specified, then $timezone is the value of the
   implicit timezone in the dynamic context.

   If $arg is the empty sequence, then the result is the empty
   sequence.

   A dynamic error is raised [err:FODT0003] if $timezone is less than
   -PT14H or greater than PT14H or if does not contain an integral
   number of minutes.

   If $arg does not have a timezone component and $timezone is the
   empty sequence, then the result is $arg.

   If $arg does not have a timezone component and $timezone is not the
   empty sequence, then the result is $arg with $timezone as the
   timezone component.

   If $arg has a timezone component and $timezone is the empty
   sequence, then the result is the localized value of $arg without
   its timezone component.

   If $arg has a timezone component and $timezone is not the empty
   sequence, then:

    * Let $srcdt be an xs:dateTime value, with an arbitrary date for
       the date component and time and timezone components that are
       the same as the time and timezone components of $arg.

    * Let $r be the result of evaluating

      fn:adjust-dateTime-to-timezone($srcdt, $timezone)

    * The result of this function will be a time value that has time
      and timezone components that are the same as the time and
      timezone components of $r.
*)

let _fn_adjust_time_to_timezone_unary code_ctxt alg_ctxt n =
  let (t1) = Args.get_array_param1 n in
  if (Cursor.cursor_is_empty t1)
  then Cursor.cursor_empty()
  else
   let time = get_time t1 in
   let timezone = Some((Execution_context.get_timezone alg_ctxt)#getAtomicDayTimeDuration()) in 
   Cursor.cursor_of_singleton(_time (DateTime.adjust_time_to_timezone time timezone))

let _fn_adjust_time_to_timezone code_ctxt alg_ctxt n =
  let (t1, tz1) = Args.get_array_param2 n in
  if (Cursor.cursor_is_empty t1)
  then Cursor.cursor_empty()
  else
    let time = get_time t1 in
    let timezone = get_optional_timezone code_ctxt alg_ctxt tz1 in
    Cursor.cursor_of_singleton(_time (DateTime.adjust_time_to_timezone time timezone))

(*

10.7.2 fn:adjust-date-to-timezone

   fn:adjust-date-to-timezone($arg as xs:date?) as xs:date?
   fn:adjust-date-to-timezone( 	$arg 	 as xs:date?, $timezone 	 as xs:dayTimeDuration?) as xs:date?

   Summary: Adjusts an xs:date value to a specific timezone, or to no
   timezone at all. If $timezone is the empty sequence, returns an
   xs:date without a timezone. Otherwise, returns an xs:date with a
   timezone. For purposes of timezone adjustment, an xs:date is
   treated as an xs:dateTime with time 00:00:00.

   If $timezone is not specified, then $timezone is the value of the
   implicit timezone in the dynamic context.

   If $arg is the empty sequence, then the result is the empty
   sequence.

   A dynamic error is raised [err:FODT0003] if $timezone is less than
   -PT14H or greater than PT14H or if does not contain an integral
   number of minutes.

   If $arg does not have a timezone component and $timezone is the
   empty sequence, then the result is the value of $arg.

   If $arg does not have a timezone component and $timezone is not the
   empty sequence, then the result is $arg with $timezone as the
   timezone component.

   If $arg has a timezone component and $timezone is the empty
   sequence, then the result is the localized value of $arg without
   its timezone component.

   If $arg has a timezone component and $timezone is not the empty
   sequence, then:

    * Let $srcdt be an xs:dateTime value, with 00:00:00 for the time
      component and date and timezone components that are the same as
      the date and timezone components of $arg.

    * Let $r be the result of evaluating
      fn:adjust-dateTime-to-timezone($srcdt, $timezone)

    * The result of this function will be a date value that has date
      and timezone components that are the same as the date and
      timezone components of $r.

*)  
let _fn_adjust_date_to_timezone_unary code_ctxt alg_ctxt n =
  let (d1) = Args.get_array_param1 n in
  if (Cursor.cursor_is_empty d1)
  then Cursor.cursor_empty()
  else
    let date = get_date d1 in
    let timezone = Some((Execution_context.get_timezone alg_ctxt)#getAtomicDayTimeDuration()) in 
    Cursor.cursor_of_singleton (_date (DateTime.adjust_date_to_timezone date timezone))

let _fn_adjust_date_to_timezone code_ctxt alg_ctxt n =
  let (d1, tz1) = Args.get_array_param2 n in
  if (Cursor.cursor_is_empty d1)
  then Cursor.cursor_empty()
  else
    let date = get_date d1 in
    let timezone = get_optional_timezone code_ctxt alg_ctxt tz1 in
    Cursor.cursor_of_singleton (_date (DateTime.adjust_date_to_timezone date timezone))

(*

   10.7.1 fn:adjust-dateTime-to-timezone

   fn:adjust-dateTime-to-timezone($arg as xs:dateTime?) as xs:dateTime?
   fn:adjust-dateTime-to-timezone( 	$arg 	 as xs:dateTime?, $timezone 	 as xs:dayTimeDuration?) as xs:dateTime?

   Summary: Adjusts an xs:dateTime value to a specific timezone, or to
   no timezone at all. If $timezone is the empty sequence, returns an
   xs:dateTime without a timezone. Otherwise, returns an xs:dateTime
   with a timezone.

   If $timezone is not specified, then $timezone is the value of the
   implicit timezone in the dynamic context.

   If $arg is the empty sequence, then the result is the empty
   sequence.

   A dynamic error is raised [err:FODT0003] if $timezone is less than
   -PT14H or greater than PT14H or if does not contain an integral
   number of minutes.

   If $arg does not have a timezone component and $timezone is the
   empty sequence, then the result is $arg.

   If $arg does not have a timezone component and $timezone is not the
   empty sequence, then the result is $arg with $timezone as the
   timezone component.

   If $arg has a timezone component and $timezone is the empty
   sequence, then the result is the localized value of $arg without
   its timezone component.

   If $arg has a timezone component and $timezone is not the empty
   sequence, then the result is an xs:dateTime value with a timezone
   component of $timezone that is equal to $arg.

*)
let _fn_adjust_dateTime_to_timezone_unary code_ctxt alg_ctxt n = 
  let (dt1) = Args.get_array_param1 n in
  if (Cursor.cursor_is_empty dt1)
  then Cursor.cursor_empty()
  else
    let dateTime = get_dateTime dt1 in
    let timezone = Some((Execution_context.get_timezone alg_ctxt)#getAtomicDayTimeDuration()) in 
    Cursor.cursor_of_singleton (_dateTime (DateTime.adjust_dateTime_to_timezone dateTime timezone))

let _fn_adjust_dateTime_to_timezone code_ctxt alg_ctxt n = 
  let (dt1, tz1) = Args.get_array_param2 n in
  if (Cursor.cursor_is_empty dt1)
  then Cursor.cursor_empty()
  else
    let dateTime = get_dateTime dt1 in
    let timezone = get_optional_timezone code_ctxt alg_ctxt tz1 in
    Cursor.cursor_of_singleton (_dateTime (DateTime.adjust_dateTime_to_timezone dateTime timezone))

(***************************************)
(* F&O Section 11. Functions on QNames *)
(***************************************)

(* F&O Section 11.1.2 fn:QName *)

let _fn_resolve_qname code_ctxt alg_ctxt n =
  let (p1, p2) = Args.get_array_param2 n in
  let s1 = get_optional_string p1 in
  match s1 with
  | None -> Cursor.cursor_empty()
  | Some s1 ->
      let e = (get_singleton_node p2)#getElementNode() in
      let nsenv = e#namespace_environment() in
      let qn = ((new atomicQName (qname_of_untyped nsenv s1)) :> atomicValue) in
      _atomic_value qn

let get_prefix p =
  match fst p with
  | NSDefaultElementPrefix -> ""
  | NSPrefix ncname -> ncname
  | NSDefaultFunctionPrefix
  | (NSInterfacePrefix _)
  | (NSServerPrefix _) 
  | NSWildcardPrefix -> raise (Query (Internal_Error "Wrong kind of prefix in fn:in-scope-prefix"))

let _fn_in_scope_prefixes code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let e = (get_singleton_node p1)#getElementNode() in
  let nsenv = e#namespace_environment() in
  let bindings = Namespace_context.flatten_bindings nsenv in
  let bindings = List.filter (fun x -> 
    match x with 
      (NSDefaultElementPrefix,NSUri "") 
    | (NSDefaultFunctionPrefix,_) 
    | (NSWildcardPrefix,_) 
    | (NSInterfacePrefix _, _)
    | (NSServerPrefix _, _) -> false 
    | _ -> true) bindings in
  let prefixes = List.map (fun x -> _string (get_prefix x)) bindings in
  Cursor.cursor_of_list prefixes

let _fn_qname code_ctxt alg_ctxt n =
  let (str1, str2) = Args.get_array_param2 n in
  let uri = normalize_optional_string str1 in
  let (prefix,localname) = uqname_element_of_raw_string (get_string str2) in
  if (uri = "") && (not(prefix = NSDefaultElementPrefix))
  then
    raise (Query (Wrong_Args "QName has a prefix in fn:QName"));
  let qname = Namespace_symbols.anon_symbol (prefix,NSUri uri, localname) in
  Cursor.cursor_of_singleton (_QName qname)


(* F&O Section 11.2.1 equality on QNames *)

let _op_QName_equal code_ctxt alg_ctxt n =
  _fn_compare_op (get_QName, qname_equal) code_ctxt alg_ctxt n

let _op_QName_nequal code_ctxt alg_ctxt n =
  _fn_compare_op (get_QName, (fun q1 q2 -> not(qname_equal q1 q2))) code_ctxt alg_ctxt n

(* F&O Section 11.2.2 fn:get-local-name-from-QName *)

let _fn_local_name_from_qname code_ctxt alg_ctxt n =
  let funqname qname =
    let (_,_,localname) = Namespace_symbols.anon_name qname in localname
  in
  _fn_unary_op (get_QName, funqname, _NCName) code_ctxt alg_ctxt n

let _fn_prefix_from_qname code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  if (Cursor.cursor_is_empty p1) then Cursor.cursor_empty()
  else
    let qn = get_QName p1 in
    let (prefix,_,_) = Namespace_symbols.anon_name qn in
    match prefix with
    | NSDefaultElementPrefix
    | NSDefaultFunctionPrefix
    | NSInterfacePrefix _
    | NSServerPrefix _ 
    | NSWildcardPrefix -> Cursor.cursor_empty()
    | NSPrefix ncname -> Cursor.cursor_of_singleton (_string ncname)

(* F&O Section 11.2.3 fn:get-namespace-uri-from-QName *)

let _fn_namespace_from_qname code_ctxt alg_ctxt n =
  _fn_unary_op (get_QName, 
		(fun qname -> 
		  let (_,uri,_) = Namespace_symbols.anon_name qname in
		  match uri with
		  | NSUri urit -> AnyURI._kinda_uri_of_string urit
		  | _ -> raise (Query (Wrong_Args "Qname contains a wildcard URI"))),
		_anyURI) code_ctxt alg_ctxt n

let _fn_namespace_uri_for_prefix code_ctxt alg_ctxt n = 
  let (p1, p2) = Args.get_array_param2 n in
  let prefix = normalize_optional_string p1 in
  let e = (get_singleton_node p2)#getElementNode() in
  let nsenv = e#namespace_environment() in
  let bindings = Namespace_context.flatten_bindings nsenv in
  let actual_prefix =
    match prefix with
    | "" -> NSDefaultElementPrefix
    | p -> NSPrefix p
  in
  begin
    try
      match List.assoc actual_prefix bindings with
      | NSUri u ->
	  Cursor.cursor_of_singleton (_anyURI (AnyURI._kinda_uri_of_string u))
      | _ -> Cursor.cursor_empty()
    with
    | Not_found -> Cursor.cursor_empty()
  end
  

(******************************************************)
(* F&O Section 11. Functions and Operators for anyURI *)
(******************************************************)

(* F&O Section 11.1 fn:resolve-uri *)

let _fn_resolve_uri code_ctxt alg_ctxt n = 
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let mod_proc_ctxt = Norm_context.module_context_from_norm_context norm_ctxt in
  let (uri1, uri2) = Args.get_array_param2 n in
  let relative_uri_opt = get_optional_string uri1 in
  let base_uri_string = get_string uri2 in
  match relative_uri_opt with
  | None -> Cursor.cursor_empty()
  | Some relative_uri ->
      let fixed_relative_uri =
	try
	  if (relative_uri.[0] = '/') ||
	  (base_uri_string.[(String.length base_uri_string)-1] = '/')
	  then relative_uri
	  else "/" ^ relative_uri
	with
	| _ -> relative_uri
      in
      let final_uri =
	try
	  AnyURI._actual_uri_of_string fixed_relative_uri 
	with
	| _ ->
	    let base_uri =
	      let pre_base_uri = AnyURI._actual_uri_of_string base_uri_string in
	      let static_base_uri  = Processing_context.get_base_uri mod_proc_ctxt in
	      begin
		match static_base_uri with
		| None -> pre_base_uri
		| Some stat_base_uri ->
		    AnyURI._uri_resolve stat_base_uri pre_base_uri
	      end
	  in
	    AnyURI._uri_resolve base_uri (AnyURI._kinda_uri_of_string fixed_relative_uri)
      in
      Cursor.cursor_of_singleton (_string (AnyURI._string_of_uri final_uri))

let _op_anyURI_equal code_ctxt alg_ctxt n =
  _fn_compare_op (get_anyURI, anyURI_equal) code_ctxt alg_ctxt n

let _op_anyURI_nequal code_ctxt alg_ctxt n =
  _fn_compare_op (get_anyURI, (fun q1 q2 -> not(anyURI_equal q1 q2))) code_ctxt alg_ctxt n

(****************************************************)
(* F&O Section 14. Functions and Operators on Nodes *)
(****************************************************)

(* F&O Section 14.1.1 fn:name *)

let _fn_name code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let name_string =
    if (Cursor.cursor_is_empty p1) then 
      ""
    else
      let node = get_singleton_node p1 in
      (get_node_qname node)
  in
  Cursor.cursor_of_singleton (_string name_string)


(* F&O Section 14.1.2 fn:local-name *)

let _fn_local_name code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let local_name_string =
    if (Cursor.cursor_is_empty p1) then 
      ""
    else
      let node = get_singleton_node p1 in
      match get_node_name node with 
      | Some (prefix,uri,local_name_string) ->
	  local_name_string
      | None ->
	  ""
  in Cursor.cursor_of_singleton (_string local_name_string)


(* F&O Section 14.1.3 fn:namespace-uri *)

let _fn_namespace_uri code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let local_name_string =
    if (Cursor.cursor_is_empty p1) then 
      ""
    else
      let node = get_singleton_node p1 in
      match get_node_name node with 
      | Some (prefix,NSUri uri,local_name_string) ->
	  uri
      | Some (prefix,NSWildcardUri,local_name_string) ->
	  raise (Query (Wrong_Args "Wildcard namespace should not appear in node"))
      | None ->
	  ""
  in Cursor.cursor_of_singleton (_string local_name_string)


(* F&O Section 14.1.5 fn:lang *)

let _fn_lang code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  let textlang = normalize_optional_string p1 in
  let node = get_singleton_node p2 in
  let oactuallang = node#node_lang() in
  let result_bool =
    match oactuallang with
    | None -> false
    | Some actuallang ->
	if (String.lowercase actuallang) = (String.lowercase textlang)
	then
	  true
	else
	  let comparelang =
	    try
	      fst (Gmisc.split_left_on_char actuallang '-')
	    with
	    | _ -> actuallang
	  in
	  (String.lowercase comparelang) = (String.lowercase textlang)
  in
  Cursor.cursor_of_singleton (_boolean result_bool)

(* F&O Section 14.1.6 op:is-same-node *)

let _op_is_same_node code_ctxt alg_ctxt n =
  _fn_binary_op (get_singleton_node, node_equal, _boolean, def_zero_test) code_ctxt alg_ctxt n

(* F&O Section 14.1.7 op:node-before *)

let _fn_node_before code_ctxt alg_ctxt n =
  _fn_binary_op (get_singleton_node, node_precedes, _boolean, def_zero_test) code_ctxt alg_ctxt n

(* F&O Section 14.1.8 op:node-after *)

let _fn_node_after code_ctxt alg_ctxt n =
  _fn_binary_op (get_singleton_node, node_follows, _boolean, def_zero_test) code_ctxt alg_ctxt n

(* F&O Section 14.1.9 fn:root *)

let rec get_root (n : node) =
  let parent = n#parent None in
  match parent with
  | None -> (Item_Node n)
  | Some p -> get_root p

let _fn_root code_ctxt alg_ctxt n =
  _fn_unary_op (get_singleton_node, get_root, fun i -> i) code_ctxt alg_ctxt n

(* F&O Section 14.1.14 fn:number *)

let _fn_number code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let opt_item = Cursor.cursor_get_optional p1 in
  let new_base_value =
    match opt_item with
      (* If the input is the empty sequence, return NaN *)
    | None ->
	new atomicDouble nan
    | Some item ->
	let base_value = get_atomic item in
	try
	  base_value#cast_to Namespace_context.default_xquery_nsenv Namespace_symbols_builtin.xs_double ATDouble
	with
	| _ ->
	    new atomicDouble nan
  in
  _atomic_value new_base_value
  

(********************************************************)
(* F&O Section 15. Functions and Operators on Sequences *)
(********************************************************)

(* F&O Section 15.1.1 fn:zero-or-one *)

let _fn_zero_or_one code_ctxt alg_ctxt n =
  let items = Args.get_array_param1 n in
  if Cursor.cursor_is_optional items
  then items
  else 
   raise (Query (Wrong_Args ("Function [fn:zero-or-one]: not a singleton item or ()")))

(* F&O Section 15.1.2 fn:one-or-more *)

let _fn_one_or_more code_ctxt alg_ctxt n =
  let items = Args.get_array_param1 n in
  if Cursor.cursor_is_empty items
  then 
   raise (Query (Wrong_Args ("Function [fn:one-or-more]: not a singleton item or more")))
  else 
    items

(* F&O Section 15.1.3 fn:exactly-one *)

let _fn_exactly_one code_ctxt alg_ctxt n =
  let items = Args.get_array_param1 n in
  if Cursor.cursor_is_singleton items
  then items
  else 
   raise (Query (Wrong_Args ("Function [fn:exactly-one]: not a singleton item")))

(* F&O Section 15.1.1

   fn:boolean($arg as item()* ) as xs:boolean

      Summary: Computes the effective boolean value of the sequence $arg. 

    * If $arg is the empty sequence, fn:boolean returns false.
    * If $arg is a singleton value of type xs:boolean or a derived from xs:boolean, fn:boolean returns $arg.
    * If $arg is a singleton value of type xs:string or a type derived
      from xs:string or xs:untypedAtomic, fn:boolean returns false if
      the operand value has zero length; otherwise it returns true.
    * If $arg is a singleton value of any numeric type or a type
      derived from a numeric type, fn:boolean returns false if the
      operand value is NaN or is numerically equal to zero; otherwise
      it returns true.
    * If $arg is a sequence whose first item is a node, fn:boolean returns true.
    * In all other cases, fn:boolean raises a type error [err:FORG0006].
*)

let empty_string  = new atomicString ""
let empty_untyped = new atomicUntyped ""
let zero_integer  = new atomicInteger _integer_zero
let zero_decimal  = new atomicDecimal _decimal_zero
let zero_float    = new atomicFloat 0.0
let zero_double   = new atomicDouble 0.0

let _fn_boolean code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let effective_boolean_value =
    if (Cursor.cursor_is_empty p1) then  false
    else if (Cursor.cursor_is_singleton p1) then 
      let item = get_item p1 in
      if (isAtomicValue item) then
	let a = getAtomicValue item in
	match a#getAtomicValueKind() with
	| ATBoolean -> a#getAtomicBoolean()
	| ATString  -> not (a#atomic_value_eq empty_string)
	| ATAnyURI  -> not ((a#cast_to Namespace_context.default_xquery_nsenv Namespace_symbols_builtin.xs_string ATString)#atomic_value_eq empty_string)
	| ATUntypedAtomic -> not (a#atomic_value_eq empty_untyped)
	| ATInteger -> not (a#atomic_value_eq zero_integer)
	| ATDecimal -> not (a#atomic_value_eq zero_decimal)
	| ATFloat   -> not ((a#atomic_value_eq zero_float) || ((a#erase_atomic_value()) = "NaN"))
	| ATDouble  -> not ((a#atomic_value_eq zero_double) || ((a#erase_atomic_value()) = "NaN"))
	| _ -> 
	    raise (Query(Type_Error("err:FORG0006: Argument to fn:boolean,"^(a#string_value())^", not string, anyURI, numeric, or node")))
      else
	true
    else
      match (Cursor.cursor_peek p1) with
      | None ->
	  raise (Query(Internal_Error ("Empty cursor in fn:boolean")))
      | Some n ->
	  if not(isAtomicValue n)
	  then
	    true
	  else
	    raise (Query(Type_Error("err:FORG0006: Argument to fn:boolean is a sequence of more than one item")))
  in
  Cursor.cursor_of_singleton (_boolean effective_boolean_value)

(* F&O Section 15.1.7 fn:empty *)

let _fn_empty code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  Cursor.cursor_of_singleton (_boolean (Cursor.cursor_is_empty p1))

(* F&O Section 15.1.8 fn:exists *)

let _fn_exists code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  Cursor.cursor_of_singleton (_boolean (not(Cursor.cursor_is_empty p1)))


(* Functions that support removal of duplicate nodes *)

(* Note:
     The following is now implemented directly over a cursor.  This
     allows it to scale to large sequences of items.
   - Jerome 04/13/2004
*)

let remove_sorted_node_dup (nl : node Cursor.cursor) =
  let rec remove_sorted_node_dup_fun x =
    match Cursor.cursor_peek nl with
    | None -> None
    | Some n1 ->
	begin
	  Cursor.cursor_junk nl;
	  match Cursor.cursor_peek nl with
	  | None -> Some n1
	  | Some n2 ->
	      if (node_equal n1 n2)
	      then
		remove_sorted_node_dup_fun x
	      else
		Some n1
	end
  in
  Cursor.cursor_of_function remove_sorted_node_dup_fun

(* F&O Section 15.1.9 fn:distinct-values *)

let valid_promotion t1 t2 pt =
  (t1 = pt) || (t2 = pt)

let dup_match ht promoted orig_type1 =
  let one_match x =
    let promoted_type = x#getAtomicValueKind() in
    if AtomicValueHash.mem ht x
    then
      let orig_type2 = AtomicValueHash.find ht x in
      valid_promotion orig_type1 orig_type2 promoted_type
    else
      false
  in
  (List.exists one_match promoted)

let remove_atomic_dup nsenv l =
  let ht = AtomicValueHash.create 1 in
  let hasnan = ref false in
  (* They keep their types.. *)
  let dup_remove (v:Physical_value.item) =
    if is_nan v
    then
      if !hasnan then false else begin hasnan := true; true end
    else
    let av = getAtomicValue v in
    let original_type =
      let t = av#getAtomicValueKind () in
      match t with
      | ATYearMonthDuration | ATDayTimeDuration -> ATDuration
      | _ -> t
    in
    let promoted = Cs_util.promote_atomicValue_to_all nsenv av in
    if dup_match ht promoted original_type
    then false
    else
      begin
	List.iter (fun x -> AtomicValueHash.add ht x original_type) promoted;
	true
      end
  in
  List.filter dup_remove l

let _fn_distinct_value code_ctxt alg_ctxt n =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let nsenv = Norm_context.nsenv_from_norm_context norm_ctxt in
  let (p1, p2) = Args.get_array_param2 n in
  let pl1 = Cursor.list_of_cursor "Cs_code_fn._fn_distinct_value" p1 in
  let _ = check_collation code_ctxt (get_string p2) in 
  let res = remove_atomic_dup nsenv pl1
  in (Cursor.cursor_of_list res)

(* F&O Section 15.1.10 fn:insert-before *)

let rec apply_insert n target inserts =
  if n <= 1
  then
    List.append inserts target
  else
    match target with
    | [] ->
	inserts
    | node :: target' ->
	node :: (apply_insert (n-1) target' inserts)

let _fn_insert_before code_ctxt alg_ctxt n =
  let (p1,p2,p3) = Args.get_array_param3 n in
  let pl1 = Cursor.list_of_cursor "Cs_code_fn._fn_insert_before" p1
  and pl3 = Cursor.list_of_cursor "Cs_code_fn._fn_insert_before" p3 in
  let i2 = _int_of_integer (get_integer p2) in
  Cursor.cursor_of_list (apply_insert i2 pl1 pl3)


(* F&O Section 15.1.11 fn:remove *)

let rec apply_remove n target =
  if n <= 0
  then
    target
  else
    match target with
    | [] ->
	[]
    | node :: target' ->
	if n = 1
	then
	  target'
	else
	  node :: (apply_remove (n-1) target')

let _fn_remove code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  let pl1 = Cursor.list_of_cursor "Cs_code_fn._fn_remove" p1 in
  let i2 = _int_of_integer (get_integer p2) in
  Cursor.cursor_of_list (apply_remove i2 pl1)

(* F&O Section 15.1.12 fn:reverse *)
let _fn_reverse code_ctxt alg_ctxt n =
  let (p1) = Args.get_array_param1 n in
  Cursor.cursor_of_list (Cursor.rev_list_of_cursor p1)

(* F&O Section 15.1.13 fn:subsequence *)

let _fn_subsequence_2 code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  let i2 = _int_of_integer (get_integer p2) in
  Cursor.cursor_subsequence2 p1 i2

let _fn_subsequence_3 code_ctxt alg_ctxt n =
  let (p1,p2,p3) = Args.get_array_param3 n in
  let i2 = _int_of_integer(get_integer p2) in
  let i3 = _int_of_integer(get_integer p3) in
  Cursor.cursor_subsequence3 p1 i2 i3

(* fn:unordeded is the identity function for now - Jerome *)

let _fn_unordered code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  p1


(* Note:
     fs:first and fs:last are built-in versions of
     fn:subsequence(Expr, 1/fs:last, 1)
*)

let _fs_first code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  Cursor.cursor_first p1

let _fs_last code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  Cursor.cursor_last p1


let stringval i =
  if (isNode i) then 
    (getNode i)#string_value()
  else
    (getAtomicValue i)#erase_atomic_value()


(* F&O Section 15.2.2 op:union *)
(* 

  From XQuery spec:

  All these operators eliminate duplicate nodes from their result
  sequences based on node identity. If ordering mode is ordered, the
  resulting sequence is returned in document order; otherwise it is
  returned in implementation-dependent order.

  Since Galax does not yet support ordering mode, these functions
  enforce doc order.

  *** NB: We should probably change the normalization of op:{union,
  intersect, except} so that the SBDO operator is explicit.  Then if
  we know the inputs are already in document order, we can use faster
  sorted-merge implementations for each operator and also avoid the
  explicit SBDO.  These rewritings should be added to the rewriting
  rules.

*)

(* Sorting by document order and duplicate removal *)

let item_node_compare n1 n2 = 
  node_compare n1 n2

let _op_union code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  let sorted_list = List.sort item_node_compare (List.append (Cursor.list_of_cursor "Cs_code_op_union" (get_node_cursor p1)) (Cursor.list_of_cursor "Cs_code_op_union" (get_node_cursor p2))) in
  _node_cursor (remove_sorted_node_dup (Cursor.cursor_of_list sorted_list))

(* F&O Section 15.2.3 op:intersect *)

let compare eq x y =
  eq x y

let rec apply_intersect eq nf1 nf2 =
  match nf1 with
  | [] -> []
  | node :: nf1' ->
      let rest_intersect = apply_intersect eq nf1' nf2 in
      if (List.exists (compare eq node) nf2)
      then
	node :: rest_intersect
      else
	rest_intersect

let _op_intersect code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  let nf1 = get_node_cursor p1 in
  let nf2 = get_node_cursor p2 in
  let pl1 = Cursor.list_of_cursor "Cs_code_fn._op_intersect" nf1 in
  let pl2 = Cursor.list_of_cursor "Cs_code_fn._op_intersect" nf2 in
  let sorted_list = List.sort item_node_compare (apply_intersect (fun i1 i2 -> node_equal i1 i2) pl1 pl2)
  in _node_cursor(remove_sorted_node_dup(Cursor.cursor_of_list sorted_list))


(* F&O Section 15.2.4 op:except *)

let rec apply_except eq nf1 nf2 =
  match nf1 with
  | [] -> []
  | node :: nf1' ->
      let rest_except = apply_except eq nf1' nf2 in
      if (List.exists (compare eq node) nf2)
      then
	rest_except
      else
	node :: rest_except

let _op_except code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  let nf1 = get_node_cursor p1 in 
  let nf2 = get_node_cursor p2 in 
  let pl1 = Cursor.list_of_cursor "Cs_code_fn._op_except" nf1 in
  let pl2 = Cursor.list_of_cursor "Cs_code_fn._op_except" nf2 in
  let sorted_list = List.sort item_node_compare (apply_except (fun i1 i2 -> node_equal i1 i2) pl1 pl2)
  in _node_cursor(remove_sorted_node_dup(Cursor.cursor_of_list sorted_list))


(********************************)
(* F&O Section 15.3 Aggregate functions *)
(********************************)

(* F&O Section 15.3.1 fn:count *)
let _fn_count code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  Cursor.cursor_of_singleton (_integer (Decimal._integer_of_int (Cursor.cursor_length p1)))

(* F&O Section 15.3.2 fn:avg *)
(* F&O Section 15.3.3 fn:max *)
(* F&O Section 15.3.4 fn:min *)
(* F&O Section 15.3.5 fn:sum *)
(* Monomorphic version implemented in order:
   xsd:integer, xsd:decimal, xsd:float, xsd:double *)

(* xsd:integer *)

let _fn_avg_integer code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let allintegers = get_integer_cursor p1 in
  match (Cursor.cursor_peek allintegers) with
  | None ->
      Cursor.cursor_empty()
  | Some _ ->
      let length = ref 0 in
      let sum = Cursor.cursor_fold_left (fun x0 x1 -> incr length ; _integer_add x0 x1) _integer_zero allintegers in
      Cursor.cursor_of_singleton (_decimal (_integer_div sum (_integer_of_int !length)))

let _fn_max_integer code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let allintegers = get_integer_cursor p1 in
  match (Cursor.cursor_peek allintegers) with
  | None ->
      Cursor.cursor_empty()
  | Some start_max ->
      let max =
	Cursor.cursor_fold_left (fun x0 x1 -> if _integer_le x0 x1 then x1 else x0) start_max allintegers
      in
      Cursor.cursor_of_singleton (_integer max)

let _fn_min_integer code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let allintegers = get_integer_cursor p1 in
  match (Cursor.cursor_peek allintegers) with
  | None ->
      Cursor.cursor_empty()
  | Some start_min ->
      let min =
	Cursor.cursor_fold_left (fun x0 x1 -> if _integer_lt x0 x1 then x0 else x1) start_min allintegers
      in
      Cursor.cursor_of_singleton (_integer min)

let _fn_sum_integer_one code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let allintegers = get_integer_cursor p1 in
  match (Cursor.cursor_peek allintegers) with
  | None ->
      Cursor.cursor_of_singleton (_integer _integer_zero)
  | Some _ ->
      let sum =
	Cursor.cursor_fold_left (fun x0 x1 -> _integer_add x0 x1) _integer_zero allintegers
      in
      Cursor.cursor_of_singleton (_integer sum)

let _fn_sum_integer code_ctxt alg_ctxt n =
  let (p1, p2) = Args.get_array_param2 n in
  let allintegers = get_integer_cursor p1 in
  let zero = get_optional_item p2 in
  match zero with
  | None -> Cursor.cursor_empty ()
  | Some p2 ->
  let intzero = get_integer (Cursor.cursor_of_singleton p2) in
  let intsum =
    match (Cursor.cursor_peek allintegers) with
    | None -> _integer intzero
    | Some _ ->
	let sum =
	  Cursor.cursor_fold_left (fun x0 x1 -> _integer_add x0 x1) intzero allintegers
	in
	_integer sum
  in
  Cursor.cursor_of_singleton intsum

(* xsd:decimal *)

let _fn_avg_decimal code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let alldecimals = get_decimal_cursor p1 in
  match (Cursor.cursor_peek alldecimals) with
  | None ->
      Cursor.cursor_empty()
  | Some _ ->
      let length = ref 0 in
      let sum = Cursor.cursor_fold_left (fun x0 x1 -> incr length ; _decimal_add x0 x1) _decimal_zero alldecimals in
      Cursor.cursor_of_singleton (_decimal (_decimal_div sum (_decimal_of_int !length)))

let _fn_max_decimal code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let alldecimals = get_decimal_cursor p1 in
  match (Cursor.cursor_peek alldecimals) with
  | None ->
      Cursor.cursor_empty()
  | Some start_max ->
      let max =
	Cursor.cursor_fold_left (fun x0 x1 -> if _decimal_le x0 x1 then x1 else x0) start_max alldecimals
      in
      Cursor.cursor_of_singleton (_decimal max)

let _fn_min_decimal code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let alldecimals = get_decimal_cursor p1 in
  match (Cursor.cursor_peek alldecimals) with
  | None ->
      Cursor.cursor_empty()
  | Some start_min ->
      let min =
	Cursor.cursor_fold_left (fun x0 x1 -> if _decimal_lt x0 x1 then x0 else x1) start_min alldecimals
      in
      Cursor.cursor_of_singleton (_decimal min)

let _fn_sum_decimal_one code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let alldecimals = get_decimal_cursor p1 in
  match (Cursor.cursor_peek alldecimals) with
  | None ->
      Cursor.cursor_of_singleton (_integer _integer_zero)
  | Some _ ->
      let sum = Cursor.cursor_fold_left (fun x0 x1 -> _decimal_add x0 x1) _decimal_zero alldecimals in
      Cursor.cursor_of_singleton (_decimal sum)

let _fn_sum_decimal code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  let alldecimals = get_decimal_cursor p1 in
  let zero = get_optional_item p2 in
  match zero with
  | None -> Cursor.cursor_empty ()
  | Some p2 ->
  let deczero = get_decimal (Cursor.cursor_of_singleton p2) in
  let decsum =
    match (Cursor.cursor_peek alldecimals) with
    | None -> _decimal deczero
    | Some _ ->
	let sum =
	  Cursor.cursor_fold_left (fun x0 x1 -> _decimal_add x0 x1) deczero alldecimals
	in
	_decimal sum
  in
  Cursor.cursor_of_singleton decsum

(* xsd:float *)

let _fn_avg_float code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let allfloats = get_float_cursor p1 in
  match (Cursor.cursor_peek allfloats) with
  | None ->
      Cursor.cursor_empty()
  | Some _ ->
      let length = ref 0 in
      let sum = Cursor.cursor_fold_left (fun x0 x1 -> incr length ; x0 +. x1) 0.0 allfloats in
      let sum = _cast_double_to_float sum in
      Cursor.cursor_of_singleton (_float (sum/.(float_of_int !length)))

let _fn_max_float code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let allfloats = get_float_cursor p1 in
  match (Cursor.cursor_peek allfloats) with
  | None ->
      Cursor.cursor_empty()
  | Some start_max ->
      let max =
	Cursor.cursor_fold_left
	  (fun x0 x1 ->
	    if Decimal.is_nan x0 then x0 else if Decimal.is_nan x1 then x1 else
	    if x0 <= x1 then x1 else x0) start_max allfloats in
      Cursor.cursor_of_singleton (_float max)

let _fn_min_float code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let allfloats = get_float_cursor p1 in
  match (Cursor.cursor_peek allfloats) with
  | None ->
      Cursor.cursor_empty()
  | Some start_min ->
      let min = Cursor.cursor_fold_left
	  (fun x0 x1 ->
	    if Decimal.is_nan x0 then x0 else if Decimal.is_nan x1 then x1 else
	    if x0 < x1 then x0 else x1) start_min allfloats in
      Cursor.cursor_of_singleton (_float min)

let _fn_sum_float_one code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let allfloats = get_float_cursor p1 in
  match (Cursor.cursor_peek allfloats) with
  | None ->
      Cursor.cursor_of_singleton (_integer _integer_zero)
  | Some _ ->
      let sum = Cursor.cursor_fold_left (fun x0 x1 -> x0 +. x1) 0.0 allfloats in
      Cursor.cursor_of_singleton (_float sum)

let _fn_sum_float code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  let allfloats = get_float_cursor p1 in
  let zero = get_optional_item p2 in
  match zero with
  | None -> Cursor.cursor_empty ()
  | Some p2 ->
  let fzero = get_float (Cursor.cursor_of_singleton p2) in
  let fsum =
    match (Cursor.cursor_peek allfloats) with
    | None -> _float fzero
    | Some _ ->
	let sum = Cursor.cursor_fold_left (fun x0 x1 -> x0 +. x1) fzero allfloats in
	_float sum
  in
  Cursor.cursor_of_singleton fsum

(* xsd:double *)

let _fn_avg_double code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let alldoubles = get_double_cursor p1 in
  match (Cursor.cursor_peek alldoubles) with
  | None ->
      Cursor.cursor_empty()
  | Some _ ->
      let length = ref 0 in
      let sum = Cursor.cursor_fold_left (fun x0 x1 -> incr length ; x0 +. x1) 0.0 alldoubles in
      Cursor.cursor_of_singleton (_double (sum/.(float_of_int !length)))

let _fn_max_double code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let alldoubles = get_double_cursor p1 in
  match (Cursor.cursor_peek alldoubles) with
  | None ->
      Cursor.cursor_empty()
  | Some start_max ->
      let max = Cursor.cursor_fold_left
	  (fun x0 x1 ->
	    if Decimal.is_nan x0 then x0 else if Decimal.is_nan x1 then x1 else
	    if x0 <= x1 then x1 else x0) start_max alldoubles in
      Cursor.cursor_of_singleton (_double max)

let _fn_min_double code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let alldoubles = get_double_cursor p1 in
  match (Cursor.cursor_peek alldoubles) with
  | None ->
      Cursor.cursor_empty()
  | Some start_min ->
      let min = Cursor.cursor_fold_left
	  (fun x0 x1 ->
	    if Decimal.is_nan x0 then x0 else if Decimal.is_nan x1 then x1 else
	    if x0 < x1 then x0 else x1) start_min alldoubles in
      Cursor.cursor_of_singleton (_double min)

let _fn_sum_double_one code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let alldoubles = get_double_cursor p1 in
  match (Cursor.cursor_peek alldoubles) with
  | None ->
      Cursor.cursor_of_singleton (_integer _integer_zero)
  | Some _ ->
      let sum = Cursor.cursor_fold_left (fun x0 x1 -> x0 +. x1) 0.0 alldoubles in
      Cursor.cursor_of_singleton (_double sum)

let _fn_sum_double code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  let alldoubles = get_double_cursor p1 in
  let zero = get_optional_item p2 in
  match zero with
  | None -> Cursor.cursor_empty ()
  | Some p2 ->
  let dzero = get_double (Cursor.cursor_of_singleton p2) in
  let dsum =
    match (Cursor.cursor_peek alldoubles) with
    | None -> _double dzero
    | Some _ ->
	let sum = Cursor.cursor_fold_left (fun x0 x1 -> x0 +. x1) dzero alldoubles in
	_double sum
  in
  Cursor.cursor_of_singleton dsum

(* F&O Section 15.4.1 op:to *)

let build_integer_cursor i1 i2 =
  let current_index = ref i1 in
  let build_integer_cursor_fun x =
    if Decimal._integer_gt !current_index i2
    then
      None
    else
      begin
	let i = !current_index in
	current_index := Decimal._integer_add i (_integer_of_int 1);
	Some i
      end
  in
  Cursor.cursor_of_function build_integer_cursor_fun

let _op_to code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  if Cursor.cursor_is_empty p1 || Cursor.cursor_is_empty p2
  then
    Cursor.cursor_empty()
  else
    let i1 = get_integer p1
    and i2 = get_integer p2 in
    _integer_cursor (build_integer_cursor i1 i2)


(* F&O Section 15.5.2 fn:id
   fn:id($arg as xs:string*, $node as node()) as element()*
*)
let _fn_id code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  if Cursor.cursor_is_empty p1 then
    Cursor.cursor_empty()
  else
    begin
      let idvals = Cursor.list_of_cursor "Cs_code_fn._fn_id" (get_string_cursor p1) in
      let n1 = (get_singleton_node p2) in
      let root = getNode(get_root n1) in
      let _ = 
	try root#getDocumentNode() 
	with 
	| _ -> raise (Query (Wrong_Args "Root of context item is not document node"))
      in
      let filter_id_attrs n = 
	 (* This should be changed to also filter attributes whose _type_ is xs:ID *)
	(n#attributes(Some(None, APNodeKindTest(AAttributeKind(AAttributeTest(Some(Namespace_symbols.idsym, None)))))))
      in
      _node_cursor(Cursor.cursor_filter
	(fun n ->
	  Cursor.cursor_exists (fun a -> List.mem (a#string_value()) idvals) (filter_id_attrs n)
	    )
	(root#descendant_or_self(None)))
    end

(* F&O Section 15.5.3 fn:idref
   fn:idref($arg as xs:string*, $node as node()) as node()*
*)
let _fn_idref code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  if Cursor.cursor_is_empty p1 then
    Cursor.cursor_empty()
   else
    raise(Query(Prototype("fn:idref unimplemented\n")))

(* F&O Section 15.4.4 fn:doc *)

let retrieve_doc refresh_doc code_ctxt =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  let mod_proc_ctxt = Norm_context.module_context_from_norm_context norm_ctxt in
  let base_uri = Processing_context.get_base_uri mod_proc_ctxt in
  (fun alg_ctxt n ->
    let p1 = Args.get_array_param1 n in
    if (Cursor.cursor_is_empty p1)
    then Cursor.cursor_empty()
    else
      let uri_string = get_string p1 in
      let absolute_uri_string =
	match base_uri with
	| None -> uri_string
	| Some base_uri ->
	    let uri = AnyURI._actual_uri_of_string uri_string in
	    let absolute_uri = AnyURI._uri_resolve base_uri uri in
	    AnyURI._string_of_uri absolute_uri
      in
      (* check for name indices in code context *)
      
      let retrieve_document_function =
	    Fn_doc.lookup_doc_function absolute_uri_string
      in
      let alive_documents = 
	if refresh_doc then None
	else Some (alive_documents_from_algebra_context alg_ctxt)
      in
        Cursor.cursor_of_list (retrieve_document_function alive_documents proc_ctxt)
  )

(* This function builds an XML stream differently for various kind of
   URIs.  - Jerome *)
let _fn_doc = retrieve_doc false

(* Needed a function that always re-reads the document, in order to 
   be able to call again a web service, for instance. - Nicola *)
let _glx_getdoc = retrieve_doc true


let _fn_collection code_ctxt =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let mod_proc_ctxt = Norm_context.module_context_from_norm_context norm_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  let base_uri = Processing_context.get_base_uri mod_proc_ctxt in
  (fun alg_ctxt n ->
    let p1 = Args.get_array_param1 n in
    if (Cursor.cursor_is_empty p1)
    then Cursor.cursor_empty()
    else
      let uri_string = get_string p1 in
      let absolute_uri_string =
	match base_uri with
	| None -> uri_string
	| Some base_uri ->
	    let uri = AnyURI._actual_uri_of_string uri_string in
	    let absolute_uri = AnyURI._uri_resolve base_uri uri in
	    AnyURI._string_of_uri absolute_uri
      in
      let alive_documents = alive_documents_from_algebra_context alg_ctxt in
      Cursor.cursor_of_list (Fn_doc.lookup_collection_function absolute_uri_string (Some alive_documents) proc_ctxt)
  )

(************************************************)
(* F&O Section 16. Functions on Algebra Context *)
(************************************************)
let _fn_current_datetime code_ctxt alg_ctxt n =
  Cursor.cursor_of_singleton (Item_Atomic (get_current_datetime alg_ctxt))

let _fn_current_date code_ctxt alg_ctxt n =
  Cursor.cursor_of_singleton (_date(DateTime.date_from_dateTime((get_current_datetime alg_ctxt)#getAtomicDateTime())))

let _fn_current_time code_ctxt alg_ctxt n =
  Cursor.cursor_of_singleton (_time(DateTime.time_from_dateTime((get_current_datetime alg_ctxt)#getAtomicDateTime())))

let _fn_implicit_timezone code_ctxt alg_ctxt n =
  Cursor.cursor_of_singleton (Item_Atomic (get_timezone alg_ctxt))

let _fn_static_base_uri code_ctxt alg_ctxt n =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let mod_proc_ctxt = Norm_context.module_context_from_norm_context norm_ctxt in
  match Processing_context.get_base_uri mod_proc_ctxt with
  | None -> Cursor.cursor_empty()
  | Some base_uri ->
      Cursor.cursor_of_singleton (_anyURI base_uri)

let _fn_default_collation code_ctxt alg_ctxt n =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let mod_proc_ctxt = Norm_context.module_context_from_norm_context norm_ctxt in
  let uri = mod_proc_ctxt.Processing_context.default_collation in
  Cursor.cursor_of_singleton (_string uri)

let _fn_document_uri code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  if Cursor.cursor_is_empty p1
  then
    Cursor.cursor_empty()
  else
    let node = get_singleton_node p1 in
    match node#node_kind() with
    | DocumentNodeKind ->
	begin
	  match ((node#getDocumentNode())#document_uri()) with
	  | None -> Cursor.cursor_empty()
	  | Some astr -> _atomic_value astr
	end
    | _ -> Cursor.cursor_empty()

let _fn_dateTime code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  let d = get_optional_date p1 in
  let t = get_optional_time p2 in
  match (d,t) with
  | None,_ -> Cursor.cursor_empty()
  | _,None -> Cursor.cursor_empty()
  | Some d,Some t ->
      Cursor.cursor_of_singleton (_dateTime (DateTime.dateTime_from_date_and_time d t))

(************************)
(* Comparison functions *)
(************************)

(* comparison operations on atomic values *)

let _op_equal_left_empty code_ctxt alg_ctxt n =
  Cursor.cursor_empty()
let _op_equal_right_empty code_ctxt alg_ctxt n =
  Cursor.cursor_empty()

let _op_notequal_left_empty code_ctxt alg_ctxt n =
  Cursor.cursor_empty()
let _op_notequal_right_empty code_ctxt alg_ctxt n =
  Cursor.cursor_empty()

let _op_lteq_left_empty code_ctxt alg_ctxt n =
  Cursor.cursor_empty()
let _op_lteq_right_empty code_ctxt alg_ctxt n =
  Cursor.cursor_empty()

let _op_gteq_left_empty code_ctxt alg_ctxt n =
  Cursor.cursor_empty()
let _op_gteq_right_empty code_ctxt alg_ctxt n =
  Cursor.cursor_empty()

let _op_lt_left_empty code_ctxt alg_ctxt n =
  Cursor.cursor_empty()
let _op_lt_right_empty code_ctxt alg_ctxt n =
  Cursor.cursor_empty()

let _op_gt_left_empty code_ctxt alg_ctxt n =
  Cursor.cursor_empty()
let _op_gt_right_empty code_ctxt alg_ctxt n =
  Cursor.cursor_empty()

(******************************)
(* Formal Semantics functions *)
(******************************)

let _fs_docorder code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in 
  let node_cursor = get_node_cursor p1 in
  let node_list = Cursor.list_of_cursor "Cs_code_fn._fs_docorder" node_cursor in
  _node_list (List.sort item_node_compare node_list)

let _fs_docorder_or_atomic_sequence code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  (* Peek at fnrst item in sequence, then dispatch to fs_distinct_docorder or assert the sequence is atomic()* *)
  let oitem = Cursor.cursor_peek p1 in
  match oitem with 
  | None -> Cursor.cursor_empty()
  | Some item -> 
      if (item_kind item) = AtomicValueKind
      then _atomic_cursor (get_atomic_cursor p1)
      else _fs_docorder code_ctxt alg_ctxt (Array.of_list [p1])

(* Note:
     The following fs:distinct function is an optimization for
     fs:distinct-docorder, which is only applied when we can infer
     that the input is already sorted by document order. This is why
     this is using the remove_sorted_node_dup operation.

     Note that the fs:distinct function is implemented as a cursor
     operation, while the fs:distinct-docorder function requires
     materialization, as it relies on sorting.

   - Jerome and Mary  09/03/2004
 *)
let _fs_distinct code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let node_cursor = (get_node_cursor p1) in
  _node_cursor (remove_sorted_node_dup node_cursor)

let _fs_distinct_or_atomic_sequence code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  (* Peek at fnrst item in sequence, then dispatch to fs_distinct_docorder or assert the sequence is atomic()* *)
  let oitem = Cursor.cursor_peek p1 in
  match oitem with 
  | None -> Cursor.cursor_empty()
  | Some item -> 
      if (item_kind item) = AtomicValueKind
      then _atomic_cursor (get_atomic_cursor p1)
      else _fs_distinct code_ctxt alg_ctxt (Array.of_list [p1])

let _fs_distinct_docorder_aux code_ctxt alg_ctxt p1 =
  let pl1 = Cursor.list_of_cursor "Cs_code_fn._fn_distinct_docorder" (get_node_cursor p1) in
  let sorted_list = List.sort item_node_compare pl1 in
  _node_cursor (remove_sorted_node_dup (Cursor.cursor_of_list sorted_list))

let _fs_distinct_docorder code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  _fs_distinct_docorder_aux code_ctxt alg_ctxt p1

let _fs_distinct_docorder_or_atomic_sequence code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  (* Peek at fnrst item in sequence, then dispatch to fs_distinct_docorder or assert the sequence is atomic()* *)
  let oitem = Cursor.cursor_peek p1 in
  match oitem with
  | None -> Cursor.cursor_empty()
  | Some item ->
      if (item_kind item) = AtomicValueKind
      then _atomic_cursor (get_atomic_cursor p1)
      else _fs_distinct_docorder_aux code_ctxt alg_ctxt p1

(* Asserts that argument is sequence of nodes *)

let _fs_node_sequence code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in p1

let _fs_node_sequence_or_atomic_sequence code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  (* Peek at fnrst item in sequence, then dispatch to fs_distinct_docorder or assert the sequence is atomic()* *)
  let oitem = Cursor.cursor_peek p1 in
  match oitem with
  | None -> Cursor.cursor_empty()
  | Some item ->
      if (item_kind item) = AtomicValueKind
      then _atomic_cursor (get_atomic_cursor p1)
      else _node_cursor (get_node_cursor p1)

(* Element/Attribute construction *)

let _fs_item_sequence_to_node_sequence code_ctxt alg_ctxt n =
  Args.get_array_param1 n

let _fs_item_sequence_to_untypedAtomic code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let items = Cursor.list_of_cursor "Cs_code_fn._fs_item_sequence_to_untypedAtomic" p1 in
  let get_atomic_content item =
    if (isNode item)
    then
      List.map (fun a -> a#string_value()) (Cursor.list_of_cursor "Cs_code_fn._fs_item_sequence_to_untypedAtomic" ((getNode item)#typed_value()))
    else
      [string_value item]
  in
  Cursor.cursor_of_singleton (_untyped (String.concat " " (List.concat (List.map get_atomic_content items))))

let _fs_item_sequence_to_untypedAtomic_optional code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  if (Cursor.cursor_is_empty p1) then Cursor.cursor_empty()
  else
  let items = Cursor.list_of_cursor "Cs_code_fn._fs_item_sequence_to_untypedAtomic_optional" p1 in
  let get_atomic_content item =
    if (isNode item)
    then
      List.map (fun a -> a#string_value()) (Cursor.list_of_cursor "Cs_code_fn._fs_item_sequence_to_untypedAtomic_optional" ((getNode item)#typed_value()))
    else
      [string_value item]
  in
  Cursor.cursor_of_singleton (_untyped (String.concat " " (List.concat (List.map get_atomic_content items))))

(* fs:untyped-to-double is used in the definition of arithmetic operators *)
let _fs_untyped_to_double code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  if (Cursor.cursor_is_empty p1) then Cursor.cursor_empty()
  else
    let a1 = get_singleton_atomic p1 in
      match a1#getAtomicValueKind() with
      | ATUntypedAtomic -> _atomic_value (a1#cast_to Namespace_context.default_xquery_nsenv Namespace_symbols_builtin.xs_double ATDouble)
      | _ -> p1

(* fs:untyped-to-integer is used in the definition of idiv *)
let _fs_untyped_to_integer code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  if (Cursor.cursor_is_empty p1) then Cursor.cursor_empty()
  else
    let a1 = get_singleton_atomic p1 in
      match a1#getAtomicValueKind() with
      | ATUntypedAtomic -> _atomic_value(a1#cast_to Namespace_context.default_xquery_nsenv Namespace_symbols_builtin.xs_integer ATInteger)
      | _ -> p1

(* fs:untyped-to-string is used in the definition of value comparisons *)
let _fs_untyped_to_string code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  if (Cursor.cursor_is_empty p1) then Cursor.cursor_empty()
  else
    let a1 = get_singleton_atomic p1 in
      match a1#getAtomicValueKind() with
      | ATUntypedAtomic -> _atomic_value(a1#cast_to Namespace_context.default_xquery_nsenv Namespace_symbols_builtin.xs_string ATString)
      | _ -> p1

(* fs:untyped-to-any is used in the definition of general comparison
   operators. 

   What we implement:

   If fnrst arg is untypedAtomic and
     the second is untypedAtomic or string then we cast untypedAtomic to string.
     the second is numeric then we cast untypedAtomic to double.
     otherwise we cast untypedAtomic to the type of the other argument

*)
let _fs_untyped_to_any code_ctxt alg_ctxt n =
  let (p1, p2) = Args.get_array_param2 n in
  let a1 = get_singleton_atomic p1 in
  let a2 = get_singleton_atomic p2 in
  match a1#getAtomicValueKind() with
  | ATUntypedAtomic -> 
      begin 
        let a2k = a2#getAtomicValueKind() in
	match a2k with
	| ATUntypedAtomic 
	| ATString ->
	    _atomic_value(a1#cast_to Namespace_context.default_xquery_nsenv Namespace_symbols_builtin.xs_string ATString)
	| ATInteger 
	| ATDecimal
	| ATFloat
	| ATDouble ->
	    _atomic_value(a1#cast_to Namespace_context.default_xquery_nsenv Namespace_symbols_builtin.xs_double ATDouble)
	| _ ->
	    let a2t = a2#atomic_type() in
	    _atomic_value(a1#cast_to Namespace_context.default_xquery_nsenv a2t a2k)
      end
  | _ -> p1


(*******************)
(* Galax functions *)
(*******************)

(* Galax function glx:union-values *)

let _glx_union_values code_ctxt alg_ctxt n =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let nsenv = Norm_context.nsenv_from_norm_context norm_ctxt in
  let (p1,p2) = Args.get_array_param2 n in
  let pl1 = Cursor.list_of_cursor "Cs_code_fn._glx_union_values" p1 in
  let pl2 = Cursor.list_of_cursor "Cs_code_fn._glx_union_values" p2 in
  let big_seq = pl1 @ pl2 in
  let res = remove_atomic_dup nsenv big_seq in
  (Cursor.cursor_of_list res)

(* Galax function glx:intersect-values *)

let _glx_intersect_values code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  let nf1 = get_atomic_cursor p1 in
  let nf2 = get_atomic_cursor p2 in
  let pl1 = Cursor.list_of_cursor "Cs_code_fn._glx_intersect_values" nf1 in
  let pl2 = Cursor.list_of_cursor "Cs_code_fn._glx_intersect_values" nf2 in
  _atomic_list (apply_intersect (fun i1 i2 -> i1#atomic_value_eq(i2)) pl1 pl2)

(* Galax function glx:except-values *)

let _glx_except_values code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  let nf1 = get_atomic_cursor p1 in
  let nf2 = get_atomic_cursor p2 in
  let pl1 = Cursor.list_of_cursor "Cs_code_fn._glx_except_values" nf1 in
  let pl2 = Cursor.list_of_cursor "Cs_code_fn._glx_except_values" nf2 in
  _atomic_list (apply_except (fun i1 i2 -> i1#atomic_value_eq(i2)) pl1 pl2)

(* Galax function glx:print-string *)

let _glx_print_string code_ctxt alg_ctxt n =
  let s1 = Args.get_array_param1 n in
  let s1' = get_string s1 in
  let _ =
    begin
      print_string s1';
      flush stdout
    end
  in
  Cursor.cursor_empty()

(* Galax function glx:print-item *)

let _glx_print_item code_ctxt =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  (fun alg_ctxt n ->
    let i1 = Args.get_array_param1 n in
    begin
      Serialization.fserialize_datamodel proc_ctxt Format.std_formatter i1;
      flush stdout;
      Cursor.cursor_empty()
    end)

let _glx_string_of_item code_ctxt =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  (fun alg_ctxt n ->
    let i1 = Args.get_array_param1 n in
    Cursor.cursor_of_singleton(_string(Serialization.bserialize_datamodel proc_ctxt i1));
    )

(* Galax function glx:save-document *)

let _glx_save_document code_ctxt =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  (fun alg_ctxt n ->
    let (output_file_param, document_param) = Args.get_array_param2 n in
    let output_file_name = get_string output_file_param in
    let output_channel = open_out output_file_name in
    let output_formatter = Format.formatter_of_out_channel output_channel in
    begin
      Serialization.fserialize_datamodel proc_ctxt output_formatter document_param;
      close_out output_channel;
      Cursor.cursor_empty()
    end)

(* Galax function glx:print-string-err *)

let _glx_print_string_err code_ctxt alg_ctxt n =
  let s1 = Args.get_array_param1 n in
  let s1' = get_string s1 in
  let _ =
(*    match !Conf.glx_stderr with
    | None -> ()
    | Some oc ->
*)
	begin
	  output_string !Conf.glx_stderr s1';
	  flush !Conf.glx_stderr
	end
  in
  Cursor.cursor_empty()

(* Galax function glx:print-item-err *)

let _glx_print_item_err code_ctxt =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  (fun alg_ctxt n ->
    let i1 = Args.get_array_param1 n in
    let _ =
      begin
	let formatter = Format.formatter_of_out_channel !Conf.glx_stderr in
	Serialization.fserialize_datamodel proc_ctxt formatter i1;
	flush !Conf.glx_stderr
      end
    in
    Cursor.cursor_empty())

(* Galax function glx:get-lang *)

let _glx_get_lang code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let node = get_singleton_node p1 in
  let olang = node#node_lang() in
  match olang with
  | None -> Cursor.cursor_empty()
  | Some lang -> _atomic_value (new atomicString lang)

(* Galax function glx:exponent *)

let _glx_exponent code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
(* Not clear whether it should work on double? instead of double - Jerome
  if (Cursor.cursor_is_empty p1) || (Cursor.cursor_is_empty p2) then Cursor.cursor_empty()
  else *)
  let (i1,i2) = (get_double p1, get_double p2) in
  Cursor.cursor_of_singleton (_double(i1 ** i2))

(* Galax function glx:sqrt *)

let _glx_sqrt code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
(* Not clear whether it should work on double? instead of double - Jerome
  if (Cursor.cursor_is_empty p1) || (Cursor.cursor_is_empty p2) then Cursor.cursor_empty()
  else *)
  let i1 = get_double p1 in
  Cursor.cursor_of_singleton (_double(sqrt i1))

(* Galax function glx:exp *)

let _glx_exp code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
(* Not clear whether it should work on double? instead of double - Jerome
  if (Cursor.cursor_is_empty p1) || (Cursor.cursor_is_empty p2) then Cursor.cursor_empty()
  else *)
  let i1 = get_double p1 in
  Cursor.cursor_of_singleton (_double(exp i1))

(* Galax function glx:log *)

let _glx_log code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
(* Not clear whether it should work on double? instead of double - Jerome
  if (Cursor.cursor_is_empty p1) || (Cursor.cursor_is_empty p2) then Cursor.cursor_empty()
  else *)
  let i1 = get_double p1 in
  Cursor.cursor_of_singleton (_double(log i1))

(* Galax function glx:log10 *)

let _glx_log10 code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
(* Not clear whether it should work on double? instead of double - Jerome
  if (Cursor.cursor_is_empty p1) || (Cursor.cursor_is_empty p2) then Cursor.cursor_empty()
  else *)
  let i1 = get_double p1 in
  Cursor.cursor_of_singleton (_double(log10 i1))

(* Galax function glx:cos *)

let _glx_cos code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
(* Not clear whether it should work on double? instead of double - Jerome
  if (Cursor.cursor_is_empty p1) || (Cursor.cursor_is_empty p2) then Cursor.cursor_empty()
  else *)
  let i1 = get_double p1 in
  Cursor.cursor_of_singleton (_double(cos i1))

(* Galax function glx:sin *)

let _glx_sin code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
(* Not clear whether it should work on double? instead of double - Jerome
  if (Cursor.cursor_is_empty p1) || (Cursor.cursor_is_empty p2) then Cursor.cursor_empty()
  else *)
  let i1 = get_double p1 in
  Cursor.cursor_of_singleton (_double(sin i1))

(* Galax function glx:tan *)

let _glx_tan code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
(* Not clear whether it should work on double? instead of double - Jerome
  if (Cursor.cursor_is_empty p1) || (Cursor.cursor_is_empty p2) then Cursor.cursor_empty()
  else *)
  let i1 = get_double p1 in
  Cursor.cursor_of_singleton (_double(tan i1))

(* Galax function glx:acos *)

let _glx_acos code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
(* Not clear whether it should work on double? instead of double - Jerome
  if (Cursor.cursor_is_empty p1) || (Cursor.cursor_is_empty p2) then Cursor.cursor_empty()
  else *)
  let i1 = get_double p1 in
  Cursor.cursor_of_singleton (_double(acos i1))

(* Galax function glx:asin *)

let _glx_asin code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
(* Not clear whether it should work on double? instead of double - Jerome
  if (Cursor.cursor_is_empty p1) || (Cursor.cursor_is_empty p2) then Cursor.cursor_empty()
  else *)
  let i1 = get_double p1 in
  Cursor.cursor_of_singleton (_double(asin i1))

(* Galax function glx:atan *)

let _glx_atan code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
(* Not clear whether it should work on double? instead of double - Jerome
  if (Cursor.cursor_is_empty p1) || (Cursor.cursor_is_empty p2) then Cursor.cursor_empty()
  else *)
  let i1 = get_double p1 in
  Cursor.cursor_of_singleton (_double(atan i1))

(* Galax function glx:atan2 *)

let _glx_atan2 code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
(* Not clear whether it should work on double? instead of double - Jerome
  if (Cursor.cursor_is_empty p1) || (Cursor.cursor_is_empty p2) then Cursor.cursor_empty()
  else *)
  let (i1,i2) = (get_double p1, get_double p2) in
  Cursor.cursor_of_singleton (_double(atan2 i1 i2))

(* Galax function glx:cosh *)

let _glx_cosh code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
(* Not clear whether it should work on double? instead of double - Jerome
  if (Cursor.cursor_is_empty p1) || (Cursor.cursor_is_empty p2) then Cursor.cursor_empty()
  else *)
  let i1 = get_double p1 in
  Cursor.cursor_of_singleton (_double(cosh i1))

(* Galax function glx:sinh *)

let _glx_sinh code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
(* Not clear whether it should work on double? instead of double - Jerome
  if (Cursor.cursor_is_empty p1) || (Cursor.cursor_is_empty p2) then Cursor.cursor_empty()
  else *)
  let i1 = get_double p1 in
  Cursor.cursor_of_singleton (_double(sinh i1))

(* Galax function glx:tanh *)

let _glx_tanh code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
(* Not clear whether it should work on double? instead of double - Jerome
  if (Cursor.cursor_is_empty p1) || (Cursor.cursor_is_empty p2) then Cursor.cursor_empty()
  else *)
  let i1 = get_double p1 in
  Cursor.cursor_of_singleton (_double(tanh i1))

let _glx_soap_call code_ctxt alg_ctxt =
  (fun  n ->
    let (p1,p2,p3,p4) = Args.get_array_param4 n in
    (* server URI *)
    let uri_server = get_anyURI p1 in
    let string_server = AnyURI._string_of_uri uri_server in
    (* HTTP method ("GET" or "POST") *)
    let meth = get_string p2 in
    (* soapAction (usually)*)
    let extra_params = get_string p3 in
    (* soap message *)
    let request = Serialization.bserialize_datamodel (Processing_context.default_processing_context()) p4 in
    if Debug.default_debug() then
      Debug.print_default_debug ("SOAP REQUEST:\n"^request);

    let response = 
      match glx_decode_url string_server with
      | Http (host,port,local) ->
	  Http.HTTP.gen_request host port local meth 
	(Some ((Http.HTTP.header_content_type "text/xml") ^ "; charset=utf-8\r\n" ^ extra_params,
	       request))
    | File local ->
	raise (Query (URI_Error ("Method: file not supported in SOAP server URL")))
    | ExternalSource (me,_,_,_) ->
	raise (Query (URI_Error ("Method: " ^ me ^ " not supported in SOAP server URL")))
    in

    if Debug.default_debug() then
      Debug.print_default_debug ("SOAP RESPONSE:\n"^response);
    let _, xml_stream = Streaming_parse.open_xml_stream_from_io (Galax_io.String_Input response) in

    (* First resolve namespaces *)
    let resolved_xml_stream = Streaming_ops.resolve_xml_stream xml_stream in
    (* Then apply type annotations *)
    let typed_xml_stream = Streaming_ops.typed_of_resolved_xml_stream resolved_xml_stream in
    (* Finally, load the document *)
    let nodeid_context = Nodeid_context.default_nodeid_context () in
    Cursor.cursor_of_list (Physical_load.load_xml_document_from_typed_stream nodeid_context typed_xml_stream))

let _glx_file_exists code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  if (Cursor.cursor_is_empty p1)
  then Cursor.cursor_empty()
  else
    let file_string = get_string p1 in
    Cursor.cursor_of_singleton(_boolean(Sys.file_exists(file_string)))

(* Galax function glx:stem *)

let _glx_stem code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  if (Cursor.cursor_is_empty p1)
  then Cursor.cursor_empty()
  else
    let in_word = get_string p1 in
    let case = get_string p2 in
    let s = Stemmer.stem in_word case 
    in Cursor.cursor_of_singleton(_string(s))

(* Added accessor function for pre-order -- Philippe *)
let _glx_get_order code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let item = Cursor.cursor_peek p1 in
  match item with
  | None ->
      raise (Query (Wrong_Args ("Function [glx:pre_order]: expected node(), got ()")))
  | Some i ->
      let ord = (get_node i)#docorder() in
      let (docid, pre) = Nodeid.big_int_pair_of_docorder ord in
      Cursor.cursor_of_singleton (_integer pre)
        
let _glx_get_docid code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let item = Cursor.cursor_peek p1 in
  match item with
  | None ->
      raise (Query (Wrong_Args ("Function [glx:pre_order]: expected node(), got ()")))
  | Some i ->
      let ord = (get_node i)#docorder() in
      let (docid, pre) = Nodeid.big_int_pair_of_docorder ord in
      Cursor.cursor_of_singleton (_integer docid)
        
(* Delay for the specified amount of time (in seconds; accepts non-integer (float) sleep times) *)
let _glx_sleep code_ctxt alg_ctxt tm = 
  let p1 = Args.get_array_param1 tm in
  let f1 = get_float p1 in
    ignore(Unix.select [] [] [] f1);
    Cursor.cursor_empty()

(* Returns the actual current time, not the one from the exec. ctxt, as fn:current-dateTime() does  *)
(* Why are we not using Unix.gettimeofday() here? *)
let _glx_gettime code_ctxt alg_ctxt n =
  Cursor.cursor_of_singleton (_double (Unix.gettimeofday()))
(*   Cursor.cursor_of_singleton (_double (Unix.time())) *)

(* Perform major O'Caml collection, and return number of live words. *)
let _glx_livewords code_ctxt alg_ctxt n =
  let st = (Gc.full_major(); Gc.stat ()) in
  Cursor.cursor_of_singleton (_integer(_integer_of_int (st.Gc.live_words)))

(* Random.int bound  *)
let _glx_random_int code_ctxt alg_ctxt n =
  let p1 = Args.get_array_param1 n in
  let i1 = get_integer p1 in 
  Cursor.cursor_of_singleton (_integer(_integer_of_int(Random.int (_int_of_integer i1))))

(* glx:keyref *)

let _glx_keyref code_ctxt alg_ctxt n =
  let (p1,p2) = Args.get_array_param2 n in
  let kn = get_string p1 in
  let kv = p2 in
  let keyval =
    try
      if (Cursor.cursor_is_empty kv)
      then 
	""
      else
	let i = Physical_util.get_item kv in
	match item_kind i with
	| NodeKind -> (getNode i)#string_value()
	| AtomicValueKind -> (getAtomicValue i)#erase_atomic_value()
    with
    | _ ->
	raise (Query (Code_Selection "key definition: key value not an item"))
  in
  try
    Cursor.cursor_of_list (key_from_algebra_context alg_ctxt (kn,keyval))
  with
  | _ -> Cursor.cursor_empty()


(* F&O Section 15.2.1 fn:deep-equal *)

let not_pi_or_comment n =
  match n#node_kind() with
  | ProcessingInstructionNodeKind
  | CommentNodeKind ->
      false
  | _ ->
      true

let same_name n1 n2 =
  match (n1, n2) with
  | (None, Some _)
  | (Some _, None) ->
      false
  | (None,None) ->
      true
  | (Some name1, Some name2) ->
      name1#atomic_value_eq name2

let compare_node_names n1 n2 =
  match (n1#node_name(), n2#node_name()) with
  | (None, Some _) ->
      -1
  | (Some _, None) ->
      1
  | (None,None) ->
      0
  | (Some name1, Some name2) ->
      name1#atomic_value_compare name2

let sort_attributes al =
  List.sort compare_node_names al

(*******************************)
(* Table of built-in functions *)
(*******************************)

(* creation of the table for built-in functions *)
    
let rec bltin_fctns : (Code_selection_context.code_selection_context -> bltin) RQNameIntHashtbl.t = RQNameIntHashtbl.create 137

and item_cursor_equal context il1 il2 =
  try
    Cursor.cursor_for_all2 (item_equal context) il1 il2
  with
  | _ -> false

and node_cursor_equal context nl1 nl2 =
  try
    Cursor.cursor_for_all2 (node_deep_equal context) nl1 nl2
  with
  | _ ->
      false

and atomic_value_equal (alg_ctxt, code_ctxt, match_op_equal_fun) av1 av2 =
  let at1 = av1#getAtomicValueKind() in 
  let at2 = av2#getAtomicValueKind() in 
  if (at1 = at2) then av1#atomic_value_eq av2
  else
    let c1 = Cursor.cursor_of_singleton (Item_Atomic av1) in
    let c2 = Cursor.cursor_of_singleton (Item_Atomic av2) in
    let (fname, eargs) = match_op_equal_fun [c1; c2] in
    try
      let bf = lookup_bltin_fctn (fname,2) code_ctxt in
      Physical_util.get_boolean(bf alg_ctxt (Array.of_list eargs))
    with
    | _ -> false

and atomic_value_cursor_equal context avl1 avl2 =
  try
    Cursor.cursor_for_all2 (atomic_value_equal context) avl1 avl2
  with
  | _ -> false

and item_equal context i1 i2 = 
  match (item_kind i1,item_kind i2) with
  | (AtomicValueKind,AtomicValueKind) ->
      begin
	(* NB!!! The semantics of this function are asymmetric and oh
	   so stupid: For fn:deep-equal, apply 'op_equal' _unless_
	   both values are NaN, in which case they are equal.

	   However, for fn:index-of (see atomic_value_equal above),
	   only apply 'op_equal'.  Absurd.
	*)
	let av1 = getAtomicValue i1 in
	let av2 = getAtomicValue i2 in 
	let at1 = av1#getAtomicValueKind() in 
	let at2 = av2#getAtomicValueKind() in 
	let nan1 =
	  match at1 with
	  | ATFloat -> Decimal.is_nan(av1#getAtomicFloat())
	  | ATDouble -> Decimal.is_nan(av1#getAtomicDouble())
	  | _ -> false 
	in
	let nan2 =
	  match at2 with
	  | ATFloat -> Decimal.is_nan(av2#getAtomicFloat())
	  | ATDouble -> Decimal.is_nan(av2#getAtomicDouble())
	  | _ -> false 
	in
	if (nan1 && nan2) then true
	else atomic_value_equal context av1 av2
  end
  | (NodeKind,NodeKind) ->
      node_deep_equal context (getNode i1) (getNode i2)
  | _ ->
      false

(* 
   F&O 15.3.1 fn:deep-equal

   If the two nodes are of different kinds, the result is false.

   If the two nodes are both document nodes then they are deep-equal
   if and only if the sequence $i1/( * | text()) is deep-equal to the
   sequence $i2/( * | text()).

   If the two nodes are both element nodes then they are deep-equal if
   and only if all of the following conditions are satisfied:

   1. the two nodes have the same name, that is (node-name($i1) eq
      node-name($i2)).

   2. the two nodes are both annotated as having simple content or
      both nodes are annotated as having complex content.

   3. the two nodes have the same number of attributes, and for every
      attribute $a1 in $i1/@* there exists an attribute $a2 in $i2/@*
      such that $a1 and $a2 are deep-equal.

   4. One of the following conditions holds:

      * Both element nodes have a type annotation that is simple
        content, and the typed value of $i1 is deep-equal to the typed
        value of $i2.

      * Both element nodes have a type annotation that is complex
        content with elementOnly content, and each child element of
        $i1 is deep-equal to the corresponding child element of $i2.

      * Both element nodes have a type annotation that is complex
        content with mixed content, and the sequence $i1/( * | text()) is
        deep-equal to the sequence $i2/( * | text()).

      * Both element nodes have a type annotation that is complex
        content with empty content.

   If the two nodes are both attribute nodes then they are deep-equal
   if and only if both the following conditions are satisfied:

   1. the two nodes have the same name, that is (node-name($i1) eq node-name($i2)).

   2. the typed value of $i1 is deep-equal to the typed value of $i2.

   If the two nodes are both processing instruction nodes or namespace
   bindings, then they are deep-equal if and only if both the
   following conditions are satisfied:

   1. the two nodes have the same name, that is (node-name($i1) eq node-name($i2)).
   2. the string value of $i1 is equal to the string value of $i2.

   If the two nodes are both text nodes or comment nodes, then they
   are deep-equal if and only if their string-values are equal.

   Notes:

   The two nodes are not required to have the same type annotation,
   and they are not required to have the same in-scope
   namespaces. They may also differ in their parent, their base URI,
   and the values returned by the is-id and is-idrefs accessors (see
   Section 5.5 is-id AccessorDM and Section 5.6 is-idrefs
   AccessorDM). The order of children is significant, but the order of
   attributes is insignificant.

   The contents of comments and processing instructions are
   significant only if these nodes appear directly as items in the two
   sequences being compared. The content of a comment or processing
   instruction that appears as a descendant of an item in one of the
   sequences being compared does not affect the result. However, the
   presence of a comment or processing instruction, if it causes a
   text node to be split into two text nodes, may affect the result.

*)

and node_deep_equal context n1 n2 =
  let name1 = n1#node_name()
  and name2 = n2#node_name() in
  if not(same_name name1 name2)
  then
    false
  else
    match (n1#node_kind(),n2#node_kind()) with
    | (DocumentNodeKind, DocumentNodeKind) ->
	let dn1 = n1#getDocumentNode ()
	and dn2 = n2#getDocumentNode () in
	let nl1 = Cursor.cursor_filter not_pi_or_comment (dn1#children None)
	and nl2 = Cursor.cursor_filter not_pi_or_comment (dn2#children None) in
	node_cursor_equal context nl1 nl2
    | (ElementNodeKind, ElementNodeKind) ->
	let en1 = n1#getElementNode ()
	and en2 = n2#getElementNode () in
	let anl1 = Cursor.cursor_map (fun n -> (n :> node)) 
	    (Cursor.cursor_of_list (sort_attributes (Cursor.list_of_cursor "Cs_code_fn._fn_deep_equal" (en1#attributes None))))
	and anl2 = Cursor.cursor_map (fun n -> (n :> node)) 
	    (Cursor.cursor_of_list (sort_attributes (Cursor.list_of_cursor "Cs_code_fn._fn_deep_equal" (en2#attributes None)))) in
	if (node_cursor_equal context anl1 anl2)
	then
	  let nl1 = Cursor.cursor_filter not_pi_or_comment (en1#children None)
	  and nl2 = Cursor.cursor_filter not_pi_or_comment (en2#children None) in
	  node_cursor_equal context nl1 nl2
	else
	  false
    | (AttributeNodeKind, AttributeNodeKind) ->
	let av1 = n1#typed_value()
	and av2 = n2#typed_value() in
	atomic_value_cursor_equal context av1 av2
    | (TextNodeKind, TextNodeKind)
    | (ProcessingInstructionNodeKind, ProcessingInstructionNodeKind)
    | (CommentNodeKind, CommentNodeKind) ->
	n1#string_value() = n2#string_value()
    | _ ->
	false

(* All of this should be known statically (after compilation?), but
   there's no way to get it except from the normalization context *)
and match_op_equal_fun code_ctxt = 
  let logical_ctxt = compile_context_from_code_selection_context code_ctxt in
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let stat_ctxt = static_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let asig = Compile_util.compile_overloaded_table_sigs logical_ctxt (Norm_overloaded.table_for_op_equal norm_ctxt) in
  Code_util.match_overloaded_function stat_ctxt Namespace_builtin.op_equal asig

and _fn_deep_equal code_ctxt alg_ctxt n =
(* This table should be known statically, but there's no way to get it except from the normalization context *)
  let match_fun = match_op_equal_fun code_ctxt in 
  let (p1,p2,p3) = Args.get_array_param3 n in
  let _ = check_collation code_ctxt (Physical_util.get_string p3) in 
  Cursor.cursor_of_singleton (_boolean (item_cursor_equal (alg_ctxt, code_ctxt, match_fun) p1 p2))

(* F&O Section 15.1.6 fn:index-of *)

(* Note:
     The function index-of atomizes its arguments and applies value
     equality to identify index of the second atomic-value argument in
     the first atomic-value-sequence argument.
   -mff *)

and _fn_index_of code_ctxt alg_ctxt n =
  let (p1,p2,p3) = Args.get_array_param3 n in
  let seq = get_atomic_cursor p1 in
  let item = get_singleton_atomic p2 in
  let _ = check_collation code_ctxt (get_string p3) in 
  let match_fun = match_op_equal_fun code_ctxt in 
  let eq_fn (idx, idxlist) av =
    (* converts xs:untypedAtomic to xs:string, to get the 'eq' semantics *)
(*
    let item =
      match item#getAtomicValueKind() with
      | ATUntypedAtomic -> (item#cast_to Namespace_context.default_xquery_nsenv Namespace_symbols_builtin.xs_string ATString)
      | _ -> item
    in
    let av =
      match av#getAtomicValueKind() with
      | ATUntypedAtomic -> (av#cast_to Namespace_context.default_xquery_nsenv Namespace_symbols_builtin.xs_string ATString)
      | _ -> av
    in
    if (item#atomic_value_eq av) then (idx+1, idxlist@[idx])
    else (idx+1, idxlist)
*)
    if (atomic_value_equal (alg_ctxt, code_ctxt, match_fun) item av) then (idx+1, idxlist@[idx])
    else (idx+1, idxlist)
  in
  let (idx', idxlist) = Cursor.cursor_fold_left eq_fn (1, []) seq
  in _integer_cursor (Cursor.cursor_map (fun i -> _integer_of_int i) (Cursor.cursor_of_list idxlist))

and lookup_bltin_fctn (cfname,arity) code_ctxt =
  try
    (RQNameIntHashtbl.find bltin_fctns (cfname,arity)) code_ctxt
  with _ ->
    raise (Query (Unknown ("Builtin function "
			   ^ (prefixed_string_of_rqname cfname)
			   ^ " with arity "
			   ^ (string_of_int arity)
			   ^ " not found")))

module DeepValueHashType = 
  struct 
    type ctxt = (Execution_context.algebra_context *
		   Code_selection_context.code_selection_context *
		   (Physical_value.item Cursor.cursor list ->
		     Xquery_common_ast.cfname * Physical_value.item Cursor.cursor list))
    type t = ctxt * item
    let equal x y = 
      let (ctxt,x') = x in
      let (_,y') = y in
      item_equal ctxt x' y'
    let hash i = 
      let (_,i') = i in
      Hashtbl.hash (stringval i')
  end

module DeepValueHash = Hashtbl.Make(DeepValueHashType)

let remove_deep_dup ctxt l =
  let ht = DeepValueHash.create 1 in    
  (* They keep their types.. *)
  let dup_remove (v:Physical_value.item) = 
    if DeepValueHash.mem ht (ctxt,v) then
      false
    else
      begin
	DeepValueHash.add ht (ctxt,v) ();
	true
      end
  in
  List.filter dup_remove l
      
let _glx_deep_distinct code_ctxt alg_ctxt n =
  let match_fun = match_op_equal_fun code_ctxt in 
  let ctxt = (alg_ctxt, code_ctxt, match_fun) in
  let p1 = Args.get_array_param1 n in
  let pl1 = Cursor.list_of_cursor "Cs_code_fn._fn_deep_distinct" p1 in
  let res = remove_deep_dup ctxt pl1
  in (Cursor.cursor_of_list res)

let _glx_http_request code_ctxt =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  let make_resource_host_port m u = 
    (*    let host, port, resource =  *)
    match Galax_url.glx_decode_url u with
        Galax_url.Http (h,p,res) -> h,p,res
      | _ -> 	raise (Query (URI_Error "Invalid HTTP URL."))          
  in
    (fun alg_ctxt n ->
       let (p1, p2, p3) = Args.get_array_param3 n in
       let meth_name = get_string p1 in
       let url = get_string p2 in
       let extra = 
         let s3 = Serialization.bserialize_datamodel proc_ctxt p3 in (* the message content *) 
           Some (Http.HTTP.header_content_type "application/xml+xhtml", s3)
       in
       let h,p,r = make_resource_host_port meth_name url in
       let payload = Http.HTTP.gen_request h p r meth_name extra in         
         Cursor.cursor_of_singleton(_string(payload)))

let _glx_http_get_request code_ctxt =
  (fun alg_ctxt n ->
     let p1 = Args.get_array_param1 n in
     let url = get_string p1 in
     try
       let ans = Http.HTTP.get url in 
       let _, xml_stream = Streaming_parse.open_xml_stream_from_io (Galax_io.String_Input ans) in
                             (* First resolve namespaces *)
       let resolved_xml_stream = Streaming_ops.resolve_xml_stream xml_stream in
                             (* Then apply type annotations *)
       let typed_xml_stream = Streaming_ops.typed_of_resolved_xml_stream resolved_xml_stream in
                             (* Finally, load the document *)
       let nodeid_context = Nodeid_context.default_nodeid_context () in
       Cursor.cursor_of_list (Physical_load.load_xml_document_from_typed_stream 
                              nodeid_context typed_xml_stream)
     with Failure e -> raise (Query (Protocol_Error ("[glx:http-get-request: "^(e)^"]"))))

(*                              Cursor.cursor_of_singleton(_string(ans))) *)

(* let disable_pipelining pipeline =  *)
(*   pipeline#set_options  *)
(*     {{pipeline#get_options  *)
(*       with Http_client.synchronization=Http_client.Sync} *)
(*      with Http_client.inhibit_persistency=false} *)

(* (\* post_raw always wants to send chunked messages, unfortunately *\) *)
(* let _glx_http_post_request code_ctxt = *)
(*   (fun alg_ctxt n -> *)
(*      let (p1,p2) = Args.get_array_param2 n in *)
(*      let url = get_string p1 in *)
(*      let msg = get_string p2 in *)
(*      let call = new Http_client.post_raw url msg in *)
(*      let h = call#request_header `Base in *)
(*        h#update_field "Content-type" "application/xml+xhtml"; *)
(*        h#update_field "Content-length" (string_of_int (String.length msg)); *)
(*        let pipeline = new Http_client.pipeline in *)
(*          disable_pipelining pipeline; *)
(*          pipeline#add call; *)
(*          Printf.eprintf "Message in the call: %s\n" (call#request_body#value); *)
(*          call#set_request_body (call#request_body); *)
(*          pipeline#run (); *)
(*          match call#status with *)
(*              `Successful -> (let ans = call#response_body#value in          *)
(*                                Cursor.cursor_of_singleton(_string(ans))) *)
(*            | `Unserved -> failwith "Unserved HTTP call" *)
(*            | `Client_error -> failwith "Client error" *)
(*            | `Server_error -> failwith "Server error" *)
(*            | `Http_protocol_error e -> raise e *)
(*            | `Redirection -> failwith "Redirection not supported" ) *)
      

let _glx_doc_of_string code_ctxt =
  (fun alg_ctxt n ->
     let str = get_string( Args.get_array_param1 n ) in
     let _, xml_stream = Streaming_parse.open_xml_stream_from_io (Galax_io.String_Input str) in
       (* First resolve namespaces *)
     let resolved_xml_stream = Streaming_ops.resolve_xml_stream xml_stream in
       (* Then apply type annotations *)
     let typed_xml_stream = Streaming_ops.typed_of_resolved_xml_stream resolved_xml_stream in
       (* Finally, load the document *)
     let nodeid_context = Nodeid_context.default_nodeid_context () in
       Cursor.cursor_of_list (Physical_load.load_xml_document_from_typed_stream nodeid_context typed_xml_stream))
       
let internal_add_bltin_fctn ((cfname,arity) as cfname_arity, f) = 
  (* all internal builtins except trace are side-effect free, so register them *)
  Compile_context.register_builtin cfname_arity;
  RQNameIntHashtbl.add bltin_fctns cfname_arity f
    
let add_bltin_fctn ((rfname, i), f) = 
(*  let g = (fun code_ctxt -> f) in *)
  internal_add_bltin_fctn ((rfname, i), f)

let fun_table = [
  (fn_unique_ID, 1),  (_DUMMY (Namespace_names.prefixed_string_of_rqname fn_unique_ID)) ;   (* Dummy *)
  (* F&O Section 2. Accessors *)
  (fn_node_name, 1),  _fn_node_name; 
  (fn_nilled, 1),     _fn_nilled; 
  (fn_string, 1),     _fn_string;
  (fn_data, 1),       _fn_data;
  (fn_base_uri, 1),   _fn_base_uri_one; 
  (fn_boolean, 1),    _fn_boolean;

  (* F&O Section 5. Functions and Operators on Numbers *)
  (* Note: arithmetic functions are monomorphic in Galax for now... *)

  (op_integer_add, 2),      _fn_integer_add;
  (op_integer_subtract, 2), _fn_integer_sub;
  (op_integer_multiply, 2), _fn_integer_mult;
  (op_integer_divide, 2),   _fn_integer_div;
  (op_integer_idivide, 2),  _fn_integer_idiv;
  (op_integer_mod, 2),      _fn_integer_mod;

  (op_integer_unary_plus, 1),  _fn_integer_unary_plus;
  (op_integer_unary_minus, 1), _fn_integer_unary_minus;

  (op_integer_equal, 2),  _fn_integer_equal;
  (op_integer_nequal, 2), _fn_integer_nequal;
  (op_integer_lt, 2), 	  _fn_integer_lt;
  (op_integer_gt, 2), 	  _fn_integer_gt;
  (op_integer_le, 2), 	  _fn_integer_le;
  (op_integer_ge, 2), 	  _fn_integer_ge;

  (op_decimal_add, 2),      _fn_decimal_add;
  (op_decimal_subtract, 2), _fn_decimal_sub;
  (op_decimal_multiply, 2), _fn_decimal_mult;
  (op_decimal_divide, 2),   _fn_decimal_div;
  (op_decimal_idivide, 2),  _fn_decimal_idiv;
  (op_decimal_mod, 2),      _fn_decimal_mod;

  (op_decimal_unary_plus, 1),  _fn_decimal_unary_plus;
  (op_decimal_unary_minus, 1), _fn_decimal_unary_minus;

  (op_decimal_equal, 2),  _fn_decimal_equal;
  (op_decimal_nequal, 2), _fn_decimal_nequal;
  (op_decimal_lt, 2), 	  _fn_decimal_lt;
  (op_decimal_gt, 2), 	  _fn_decimal_gt;
  (op_decimal_le, 2), 	  _fn_decimal_le;
  (op_decimal_ge, 2), 	  _fn_decimal_ge;

  (op_float_add, 2),      _fn_float_add;
  (op_float_subtract, 2), _fn_float_sub;
  (op_float_multiply, 2), _fn_float_mult;
  (op_float_divide, 2),   _fn_float_div;
  (op_float_idivide, 2),  _fn_float_idiv;
  (op_float_mod, 2),      _fn_float_mod;

  (op_float_unary_plus, 1),  _fn_float_unary_plus;
  (op_float_unary_minus, 1), _fn_float_unary_minus;

  (op_float_equal, 2),  _fn_float_equal;
  (op_float_nequal, 2), _fn_float_nequal;
  (op_float_lt, 2), 	_fn_float_lt;
  (op_float_gt, 2), 	_fn_float_gt;
  (op_float_le, 2), 	_fn_float_le;
  (op_float_ge, 2), 	_fn_float_ge;

  (op_double_add, 2),      _fn_double_add;
  (op_double_subtract, 2), _fn_double_sub;
  (op_double_multiply, 2), _fn_double_mult;
  (op_double_divide, 2),   _fn_double_div;
  (op_double_idivide, 2),  _fn_double_idiv;
  (op_double_mod, 2),      _fn_double_mod;

  (op_double_unary_plus, 1),  _fn_double_unary_plus;
  (op_double_unary_minus, 1), _fn_double_unary_minus;

  (op_double_equal, 2),  _fn_double_equal;
  (op_double_nequal, 2), _fn_double_nequal;
  (op_double_lt, 2), 	 _fn_double_lt;
  (op_double_gt, 2), 	 _fn_double_gt;
  (op_double_le, 2), 	 _fn_double_le;
  (op_double_ge, 2), 	 _fn_double_ge;

  (fn_floor_double, 1),  _fn_floor_double;
  (fn_floor_float, 1),   _fn_floor_float;
  (fn_floor_decimal, 1), _fn_floor_decimal;
  (fn_floor_integer, 1), _fn_floor_integer;

  (fn_abs_double, 1), _fn_abs_double;
  (fn_abs_float, 1), _fn_abs_float;
  (fn_abs_decimal, 1), _fn_abs_decimal;
  (fn_abs_integer, 1), _fn_abs_integer;

  (fn_ceiling_double, 1), _fn_ceiling_double;
  (fn_ceiling_float, 1), _fn_ceiling_float;
  (fn_ceiling_decimal, 1), _fn_ceiling_decimal;
  (fn_ceiling_integer, 1), _fn_ceiling_integer;

  (fn_round_double, 1),  _fn_round_double;
  (fn_round_float, 1),   _fn_round_float;
  (fn_round_decimal, 1), _fn_round_decimal;
  (fn_round_integer, 1), _fn_round_integer;

  (fn_round_half_to_even_double, 2),  _fn_round_half_to_even_double;
  (fn_round_half_to_even_float, 2),   _fn_round_half_to_even_float;
  (fn_round_half_to_even_decimal, 2), _fn_round_half_to_even_decimal;
  (fn_round_half_to_even_integer, 2), _fn_round_half_to_even_integer;

  (* F&O Section 4. Trace *)

  (fn_trace, 2), _fn_trace;

  (* F&O Section 6. Functions on Strings *)

  (* Note:
       (**) indicates the functions which have been partially
       implemented.
     BChoi *)

  (fn_concat, 2),             _fn_concat;
  (fn_string_join, 2),        _fn_string_join;
  (fn_starts_with, 3),        _fn_starts_with;                         (**)
  (fn_ends_with, 3),          _fn_ends_with;                           (**)
  (fn_contains, 3),           _fn_contains;
  (fn_substring, 3),          _fn_substring;                           (**)
  (fn_string_length, 1),      _fn_string_length;
  (fn_substring_before, 3),   _fn_substring_before;
  (fn_substring_after, 3),    _fn_substring_after;
  (fn_normalize_space, 1),    _fn_normalize_space;
  (fn_normalize_unicode, 2),  _fn_normalize_unicode;
  (fn_upper_case, 1),         _fn_upper_case;
  (fn_lower_case, 1),         _fn_lower_case;
  (fn_translate, 3),          _fn_translate;
  (fn_encode_for_uri, 1),     _fn_encode_for_uri;
  (fn_iri_to_uri, 1),         _fn_iri_to_uri;
  (fn_escape_html_uri, 1),    _fn_escape_html_uri;
  (fn_codepoints_to_string, 1), _fn_codepts_to_string;
  (fn_string_to_codepoints, 1), _fn_string_to_codepts;
  (fn_codepoint_equal, 2),      _fn_codepoint_equal;

  (fn_compare, 3),            _fn_compare;
  (op_string_equal, 2),       _op_string_equal;
  (op_string_nequal, 2),      _op_string_nequal;
  (op_string_lt, 2),          _op_string_lt;
  (op_string_gt, 2),          _op_string_gt;
  (op_string_le, 2),          _op_string_le;
  (op_string_ge, 2),          _op_string_ge;
  (fn_min_string, 1),         _fn_min_string;
  (fn_max_string, 1),         _fn_max_string; 
  
  (fn_matches, 3),            _fn_matches;
  (fn_replace, 4),            _fn_replace;
  (fn_tokenize, 3),           _fn_tokenize;

  (* F&O Section 7. Functions and Operators on Booleans *)
  (fn_true, 0),  _fn_true;
  (fn_false, 0), _fn_false;
  (fn_not, 1),   _fn_not;
  (op_boolean_equal, 2), _op_boolean_equal;
  (op_boolean_nequal, 2), _op_boolean_nequal;
  (op_boolean_lt, 2), _op_boolean_lt;
  (op_boolean_gt, 2), _op_boolean_gt;
  (op_boolean_le, 2), _op_boolean_le;
  (op_boolean_ge, 2), _op_boolean_ge;

  (* F&O Section 9. Functions on Dates, Times, Durations *)
  (op_date_equal, 2),       _op_date_equal;
  (op_date_nequal, 2),      _op_date_nequal;
  (op_date_lt, 2), _op_date_lt;
  (op_date_le, 2), _op_date_le;
  (op_date_gt, 2), _op_date_gt;
  (op_date_ge, 2), _op_date_ge;

  (op_gYearMonth_equal, 2),       _op_gYearMonth_equal;
  (op_gYearMonth_nequal, 2),      _op_gYearMonth_nequal;

  (op_gYear_equal, 2),       _op_gYear_equal;
  (op_gYear_nequal, 2),      _op_gYear_nequal;

  (op_gMonthDay_equal, 2),       _op_gMonthDay_equal;
  (op_gMonthDay_nequal, 2),      _op_gMonthDay_nequal;

  (op_gDay_equal, 2),       _op_gDay_equal;
  (op_gDay_nequal, 2),      _op_gDay_nequal;

  (op_gMonth_equal, 2),       _op_gMonth_equal;
  (op_gMonth_nequal, 2),      _op_gMonth_nequal;

  (op_add_yearMonthDuration_to_date, 2), _op_add_yearMonthDuration_to_date; 
  (op_add_yearMonthDuration_to_date2, 2), _op_add_yearMonthDuration_to_date2; 
  (op_add_dayTimeDuration_to_date, 2), _op_add_dayTimeDuration_to_date;
  (op_add_dayTimeDuration_to_date2, 2), _op_add_dayTimeDuration_to_date2;
  (op_subtract_yearMonthDuration_from_date, 2), _op_subtract_yearMonthDuration_from_date;
  (op_subtract_dayTimeDuration_from_date, 2), _op_subtract_dayTimeDuration_from_date;
  (fn_min_date, 1),       _fn_min_date;
  (fn_max_date, 1),       _fn_max_date; 
 
  (op_time_equal, 2), _op_time_equal;
  (op_time_nequal, 2), _op_time_nequal;
  (op_time_lt, 2), _op_time_lt;
  (op_time_le, 2), _op_time_le;
  (op_time_gt, 2), _op_time_gt;
  (op_time_ge, 2), _op_time_ge;
  (op_add_dayTimeDuration_to_time, 2), _op_add_dayTimeDuration_to_time;
  (op_add_dayTimeDuration_to_time2, 2), _op_add_dayTimeDuration_to_time2;
  (op_subtract_dayTimeDuration_from_time, 2), _op_subtract_dayTimeDuration_from_time;
  (op_subtract_dateTimes, 2), _op_subtract_dateTimes;
  (op_subtract_dates, 2), _op_subtract_dates;
  (op_subtract_times, 2), _op_subtract_times;
  (fn_min_time, 1), _fn_min_time;
  (fn_max_time, 1), _fn_max_time;

  (op_dateTime_equal, 2),   _op_dateTime_equal;
  (op_dateTime_nequal, 2),  _op_dateTime_nequal;
  (op_dateTime_lt, 2), _op_dateTime_lt;
  (op_dateTime_le, 2), _op_dateTime_le;
  (op_dateTime_gt, 2), _op_dateTime_gt;
  (op_dateTime_ge, 2), _op_dateTime_ge;
  (op_add_yearMonthDuration_to_dateTime, 2), _op_add_yearMonthDuration_to_dateTime;
  (op_add_yearMonthDuration_to_dateTime2, 2), _op_add_yearMonthDuration_to_dateTime2;
  (op_add_dayTimeDuration_to_dateTime, 2), _op_add_dayTimeDuration_to_dateTime;
  (op_add_dayTimeDuration_to_dateTime2, 2), _op_add_dayTimeDuration_to_dateTime2;
  (op_subtract_yearMonthDuration_from_dateTime, 2), _op_subtract_yearMonthDuration_from_dateTime;
  (op_subtract_dayTimeDuration_from_dateTime, 2), _op_subtract_dayTimeDuration_from_dateTime;
  (fn_min_dateTime, 1), _fn_min_dateTime;
  (fn_max_dateTime, 1), _fn_max_dateTime;

  (op_yearMonthDuration_equal, 2),       _op_yearMonthDuration_equal;
  (op_yearMonthDuration_nequal, 2),       _op_yearMonthDuration_nequal;
  (op_yearMonthDuration_lt, 2), _op_yearMonthDuration_lt;
  (op_yearMonthDuration_le, 2), _op_yearMonthDuration_le;
  (op_yearMonthDuration_gt, 2), _op_yearMonthDuration_gt;
  (op_yearMonthDuration_ge, 2), _op_yearMonthDuration_ge;
  (op_add_yearMonthDurations, 2), _op_add_yearMonthDurations;
  (op_subtract_yearMonthDurations, 2), _op_subtract_yearMonthDurations;
  (op_multiply_yearMonthDuration, 2), _op_multiply_yearMonthDuration;
  (op_divide_yearMonthDuration, 2), _op_divide_yearMonthDuration;
  (op_multiply_yearMonthDuration2, 2), _op_multiply_yearMonthDuration2;
  (op_divide_yearMonthDuration_by_yearMonthDuration, 2), _op_divide_yearMonthDuration_by_yearMonthDuration;
  (fn_min_yearMonthDuration, 1), _fn_min_yearMonthDuration;
  (fn_max_yearMonthDuration, 1), _fn_max_yearMonthDuration;
  (fn_avg_yearMonthDuration, 1), _fn_avg_yearMonthDuration;
  (fn_sum_yearMonthDuration, 1), _fn_sum_yearMonthDuration_one;
  (fn_sum_yearMonthDuration, 2), _fn_sum_yearMonthDuration;

  (op_dayTimeDuration_equal, 2),     _op_dayTimeDuration_equal;
  (op_dayTimeDuration_nequal, 2),    _op_dayTimeDuration_nequal;
  (op_dayTimeDuration_lt, 2), 	     _op_dayTimeDuration_lt;
  (op_dayTimeDuration_le, 2), 	     _op_dayTimeDuration_le;
  (op_dayTimeDuration_gt, 2), 	     _op_dayTimeDuration_gt;
  (op_dayTimeDuration_ge, 2), 	     _op_dayTimeDuration_ge;
  (op_add_dayTimeDurations, 2),      _op_add_dayTimeDurations;
  (op_subtract_dayTimeDurations, 2), _op_subtract_dayTimeDurations;
  (op_multiply_dayTimeDuration, 2),  _op_multiply_dayTimeDuration;
  (op_multiply_dayTimeDuration2, 2), _op_multiply_dayTimeDuration2;
  (op_divide_dayTimeDuration, 2),    _op_divide_dayTimeDuration;
  (op_divide_dayTimeDuration_by_dayTimeDuration, 2), _op_divide_dayTimeDuration_by_dayTimeDuration;

  (op_duration_equal, 2),  _op_duration_equal;
  (op_duration_nequal, 2), _op_duration_nequal;

  (fn_min_dayTimeDuration, 1), _fn_min_dayTimeDuration;
  (fn_max_dayTimeDuration, 1), _fn_max_dayTimeDuration;
  (fn_avg_dayTimeDuration, 1), _fn_avg_dayTimeDuration;
  (fn_sum_dayTimeDuration, 1), _fn_sum_dayTimeDuration_one;
  (fn_sum_dayTimeDuration, 2), _fn_sum_dayTimeDuration;

  (* F&O Section 9.4, Extractions of Dates/Times *)
  (fn_years_from_duration, 1), _fn_years_from_duration;
  (fn_months_from_duration, 1), _fn_months_from_duration;
  (fn_days_from_duration, 1), _fn_days_from_duration;
  (fn_hours_from_duration, 1), _fn_hours_from_duration;
  (fn_minutes_from_duration, 1), _fn_minutes_from_duration;
  (fn_seconds_from_duration, 1), _fn_seconds_from_duration;
  (fn_year_from_dateTime, 1), _fn_year_from_dateTime;
  (fn_month_from_dateTime, 1), _fn_month_from_dateTime;
  (fn_day_from_dateTime, 1), _fn_day_from_dateTime;
  (fn_hours_from_dateTime, 1), _fn_hours_from_dateTime;
  (fn_minutes_from_dateTime, 1), _fn_minutes_from_dateTime;
  (fn_seconds_from_dateTime, 1), _fn_seconds_from_dateTime;
  (fn_timezone_from_dateTime, 1), _fn_timezone_from_dateTime;
  (fn_year_from_date, 1), _fn_year_from_date;
  (fn_month_from_date, 1), _fn_month_from_date;
  (fn_day_from_date, 1), _fn_day_from_date;
  (fn_timezone_from_date, 1), _fn_timezone_from_date;
  (fn_hours_from_time, 1), _fn_hours_from_time;
  (fn_minutes_from_time, 1), _fn_minutes_from_time;
  (fn_seconds_from_time, 1), _fn_seconds_from_time;
  (fn_timezone_from_time, 1), _fn_timezone_from_time;
  (fn_adjust_time_to_timezone, 2), _fn_adjust_time_to_timezone;
  (fn_adjust_date_to_timezone, 2), _fn_adjust_date_to_timezone;
  (fn_adjust_dateTime_to_timezone, 2), _fn_adjust_dateTime_to_timezone;

  (fn_adjust_time_to_timezone_unary, 1), _fn_adjust_time_to_timezone_unary;
  (fn_adjust_date_to_timezone_unary, 1), _fn_adjust_date_to_timezone_unary;
  (fn_adjust_dateTime_to_timezone_unary, 1), _fn_adjust_dateTime_to_timezone_unary;

  (* F&O Section 10. Functions on QNames *)
  (fn_resolve_QName, 2), _fn_resolve_qname;
  (fn_in_scope_prefixes, 1), _fn_in_scope_prefixes;
  (fn_QName, 2), _fn_qname;
  (fn_local_name_from_QName, 1), _fn_local_name_from_qname; 
  (fn_prefix_from_QName, 1), _fn_prefix_from_qname; 
  (fn_namespace_uri_from_QName, 1), _fn_namespace_from_qname;
  (fn_namespace_uri_for_prefix, 2), _fn_namespace_uri_for_prefix;
  (op_QName_equal, 2), _op_QName_equal;
  (op_QName_nequal, 2), _op_QName_nequal;

  (op_hexBinary_equal, 2), _op_hexBinary_equal;
  (op_base64Binary_equal, 2), _op_base64Binary_equal;

  (op_hexBinary_nequal, 2), _op_hexBinary_nequal;
  (op_base64Binary_nequal, 2), _op_base64Binary_nequal;

  (* F&O Section 11. Functions and Operators for anyURI *)
  (fn_resolve_uri, 2), _fn_resolve_uri;
  (op_anyURI_equal, 2), _op_anyURI_equal;
  (op_anyURI_nequal, 2), _op_anyURI_nequal;

  (* F&O Section 13. Functions and Operators on Nodes *)
  (fn_name, 1), _fn_name;
  (fn_local_name, 1), _fn_local_name;
  (fn_namespace_uri, 1), _fn_namespace_uri;
  (fn_lang, 2), _fn_lang;
  (op_is_same_node, 2), _op_is_same_node;
  (op_node_before, 2), _fn_node_before;
  (op_node_after, 2), _fn_node_after;
  (fn_root, 1), _fn_root;
  (fn_number,1), _fn_number;

  (* F&O Section 15. Functions and Operators on Sequences *)
  (fn_exactly_one, 1),           _fn_exactly_one;
  (fn_zero_or_one, 1),           _fn_zero_or_one;
  (fn_one_or_more, 1),           _fn_one_or_more;

  (op_to, 2), _op_to;
  (fn_id, 2), _fn_id;
  (fn_idref, 2), _fn_idref;

  (fn_empty, 1), _fn_empty;
  (fn_exists, 1), _fn_exists;
  (fn_insert_before, 3), _fn_insert_before;
  (fn_remove, 2), _fn_remove;
  (fn_reverse, 1), _fn_reverse;
  (fn_subsequence, 2), _fn_subsequence_2;
  (fn_subsequence, 3), _fn_subsequence_3;
  (fn_unordered,1), _fn_unordered;

  (* fn:distinct-values, fn:index-of, and fn:deep-equal all (recursively) depend on the 
     semantics of op:equal
  *)
  (fn_distinct_values, 2), _fn_distinct_value;
  (fn_index_of, 3), _fn_index_of;
  (fn_deep_equal, 3), _fn_deep_equal;  

  (op_union, 2), _op_union;
  (op_intersect, 2), _op_intersect;
  (op_except, 2), _op_except;

  (fn_count, 1), _fn_count;

  (fn_avg_integer, 1), _fn_avg_integer;
  (fn_max_integer, 1), _fn_max_integer;
  (fn_min_integer, 1), _fn_min_integer;
  (fn_sum_integer, 1), _fn_sum_integer_one;
  (fn_sum_integer, 2), _fn_sum_integer;

  (fn_avg_decimal, 1), _fn_avg_decimal;
  (fn_max_decimal, 1), _fn_max_decimal;
  (fn_min_decimal, 1), _fn_min_decimal;
  (fn_sum_decimal, 1), _fn_sum_decimal_one;
  (fn_sum_decimal, 2), _fn_sum_decimal;

  (fn_avg_float, 1), _fn_avg_float;
  (fn_max_float, 1), _fn_max_float;
  (fn_min_float, 1), _fn_min_float;
  (fn_sum_float, 1), _fn_sum_float_one;
  (fn_sum_float, 2), _fn_sum_float;

  (fn_avg_double, 1), _fn_avg_double;
  (fn_max_double, 1), _fn_max_double;
  (fn_min_double, 1), _fn_min_double;
  (fn_sum_double, 1), _fn_sum_double_one;
  (fn_sum_double, 2), _fn_sum_double;

  (fn_doc,        1), _fn_doc;
  (fn_collection, 1), _fn_collection;

  (* F&O Section 16 : Functions on Dynamic Context *)

  (* Comparison functions *)
  (fn_current_datetime, 0), _fn_current_datetime;
  (fn_current_date, 0), _fn_current_date;
  (fn_current_time, 0), _fn_current_time;
  (fn_implicit_timezone, 0), _fn_implicit_timezone; 
  (fn_static_base_uri, 0), _fn_static_base_uri; 
  (fn_default_collation, 0), _fn_default_collation; 
  (fn_document_uri, 1), _fn_document_uri; 
  (fn_dateTime, 2), _fn_dateTime; 

  (* Note: The following functions are effectively polymorphic over atomic values. *)

  (op_equal_left_empty, 2), _op_equal_left_empty;
  (op_equal_right_empty, 2), _op_equal_right_empty;
  (op_nequal_left_empty, 2), _op_notequal_left_empty;
  (op_nequal_right_empty, 2), _op_notequal_right_empty;
  (op_le_left_empty, 2), _op_lteq_left_empty;
  (op_le_right_empty, 2), _op_lteq_right_empty;
  (op_ge_left_empty, 2), _op_gteq_left_empty;
  (op_ge_right_empty, 2), _op_gteq_right_empty;
  (op_gt_left_empty, 2), _op_gt_left_empty;
  (op_gt_right_empty, 2), _op_gt_right_empty;
  (op_lt_left_empty, 2), _op_lt_left_empty;
  (op_lt_right_empty, 2), _op_lt_right_empty;

  (* Formal Semantics functions *)

  (fs_distinct_docorder, 1), _fs_distinct_docorder;
  (fs_distinct_docorder_or_atomic_sequence, 1), _fs_distinct_docorder_or_atomic_sequence;
  (fs_distinct, 1), _fs_distinct;
  (fs_distinct_or_atomic_sequence, 1), _fs_distinct_or_atomic_sequence;
  (fs_docorder, 1), _fs_docorder;
  (fs_docorder_or_atomic_sequence, 1), _fs_docorder_or_atomic_sequence;
  (fs_node_sequence, 1), _fs_node_sequence;
  (fs_node_sequence_or_atomic_sequence, 1), _fs_node_sequence_or_atomic_sequence;
  (fs_item_sequence_to_node_sequence, 1), _fs_item_sequence_to_node_sequence;
  (fs_item_sequence_to_untypedAtomic, 1), _fs_item_sequence_to_untypedAtomic;
  (fs_item_sequence_to_untypedAtomic_optional, 1), _fs_item_sequence_to_untypedAtomic_optional;

  (fs_untyped_to_double, 1), _fs_untyped_to_double;
  (fs_untyped_to_integer, 1), _fs_untyped_to_integer;
  (fs_untyped_to_string, 1), _fs_untyped_to_string;
  (fs_untyped_to_any, 2), _fs_untyped_to_any;

  (fs_first, 1), _fs_first;
  (fs_last_fn, 1), _fs_last;

  (* Galax functions *)

  (* Note:
       Those functions are not part of the F&O spec. They are added in
       Galax for user's convenience.
     _ Jerome *)
  
  (glx_string_pad, 3),        _glx_string_pad;
  (glx_union_values, 2), _glx_union_values;
  (glx_intersect_values, 2), _glx_intersect_values;
  (glx_except_values, 2), _glx_except_values;

  (glx_print_string, 1), _glx_print_string;
  (glx_print_item, 1), _glx_print_item;
  (glx_string_of_item, 1), _glx_string_of_item;

  (glx_print_string_err, 1), _glx_print_string_err;
  (glx_print_item_err, 1), _glx_print_item_err;

  (glx_save_document, 2), _glx_save_document;

  (glx_get_lang, 1), _glx_get_lang;

  (glx_exponent, 2), _glx_exponent;
  (glx_sqrt, 1), _glx_sqrt;
  (glx_exp, 1), _glx_exp;
  (glx_log, 1), _glx_log;
  (glx_log10, 1), _glx_log10;
  (glx_cos, 1), _glx_cos;
  (glx_sin, 1), _glx_sin;
  (glx_tan, 1), _glx_tan;
  (glx_acos, 1), _glx_acos;
  (glx_asin, 1), _glx_asin;
  (glx_atan, 1), _glx_atan;
  (glx_atan2, 2), _glx_atan2;
  (glx_cosh, 1), _glx_cosh;
  (glx_sinh, 1), _glx_sinh;
  (glx_tanh, 1), _glx_tanh;

  (glx_soap_call, 4), _glx_soap_call;
  (glx_file_exists, 1), _glx_file_exists;
  (glx_deep_distinct, 1), _glx_deep_distinct; 
  (glx_stem, 2), _glx_stem;

  (glx_get_order, 1), _glx_get_order;
  (glx_get_docid, 1), _glx_get_docid;
  (glx_sleep, 1), _glx_sleep;
  (glx_getdoc,1), _glx_getdoc;
  (glx_gettime, 0), _glx_gettime;
  (glx_livewords, 0), _glx_livewords;
  (glx_random_int, 1), _glx_random_int;

  (glx_keyref, 2), _glx_keyref;

  (glx_http_request, 3), _glx_http_request;
  (glx_http_get_request, 1), _glx_http_get_request;
  (glx_doc_of_string, 1), _glx_doc_of_string
]

let _ = 
  try
    List.iter internal_add_bltin_fctn fun_table
  with exn -> eprintf_error " " exn


