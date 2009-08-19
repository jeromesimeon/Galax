(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: datatypes_util.ml,v 1.44 2007/07/05 08:35:53 simeon Exp $ *)

(* Module Datatypes_util
   Description:
     This module contains basic operations on atomic values and atomic
     types, at the Caml level.
 *)

(* Note:
     This module needs to be extended to support XML Schema Datatype
     hierarchy
   - Jerome *)

open Error

open Namespace_context
open Namespace_symbols
open Namespace_symbols_util
open Namespace_symbols_builtin
open Namespace_resolve

open Decimal
open AnyURI

open Datatypes


(*******************************)
(* Parsing from text to values *)
(*******************************)

let string_of_untyped t =
  t

let ncname_of_untyped t = 
  try
    Gmisc.wrap_lexer Datatypes_lexer.parse_ncname t
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not an NCNAME")))

let boolean_of_untyped t =
  try
    Gmisc.wrap_lexer Datatypes_lexer.parse_boolean t
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not a boolean value")))

let decimal_of_untyped t =
  try
    Gmisc.wrap_lexer Datatypes_lexer.parse_decimal t
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not a decimal value")))

let float_of_untyped t =
  try
    _cast_double_to_float (Gmisc.wrap_lexer Datatypes_lexer.parse_float t)
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not a float value")))

let double_of_untyped t =
  try
    Gmisc.wrap_lexer Datatypes_lexer.parse_double t
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not a double value")))

let yearMonthDuration_of_untyped t = 
  try
    Gmisc.wrap_lexer Datatypes_lexer.parse_yearMonthDuration t
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not a yearMonthDuration value")))

let dayTimeDuration_of_untyped t = 
  try 
    Gmisc.wrap_lexer Datatypes_lexer.parse_dayTimeDuration t
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not a dayTimeDuration value")))

let get_sign t =
  if (String.get t 0) = '-'
  then
    (false,String.sub t 1 ((String.length t)-1))
  else
    (true,t)

let dayreg  = Str.regexp "[0-9]+D"
let timereg = Str.regexp "T"

let split_durations t =
  try
    (* try to split on day first *)
    try
      let i = Str.search_forward dayreg t 0 in
      (Str.string_before t i, Str.string_after t i)
	(* if not working, split on time *)
    with
    | _ ->
	let i = Str.search_forward timereg t 0 in
	(Str.string_before t i, Str.string_after t i)
  (* Finally, just return the duration as year month *)
  with
  | _ -> (t,"")

let duration_of_untyped t =
  try
    let (sign,t) = get_sign t in
    let (ymdt,dtdt) =
      match split_durations t with
      | ("P",t) ->
	  (None,Some ("P" ^ t))
      | (t,"") ->
	  (Some t,None)
      | (t1,t2) ->
	  (Some t1, Some ("P" ^ t2))
    in
    let ymd =
      match ymdt with
      | None -> DateTime.zero_yearMonthDuration
      | Some t -> (
	  Gmisc.wrap_lexer Datatypes_lexer.parse_yearMonthDuration t)
    in
    let dtd =
      match dtdt with
      | None -> DateTime.zero_dayTimeDuration
      | Some t -> (
	  Gmisc.wrap_lexer Datatypes_lexer.parse_dayTimeDuration t)
    in
    DateTime.mkduration sign (ymd,dtd)
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not a duration value")))

let dateTime_of_untyped t = 
  try
    let (sign,t) = get_sign t in
    let dt = Gmisc.wrap_lexer Datatypes_lexer.parse_dateTime t in
    if sign
    then dt
    else DateTime.negate_dateTime dt
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not a dateTime value")))

let time_of_untyped t =  
  try 
    Gmisc.wrap_lexer Datatypes_lexer.parse_time t
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not a time value")))

let date_of_untyped t =  
  try
    let (sign,t) = get_sign t in
    let d = Gmisc.wrap_lexer Datatypes_lexer.parse_date t in
    if sign
    then d
    else DateTime.negate_date d
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not a date value")))

let gYearMonth_of_untyped t =
  try
    let (sign,t) = get_sign t in
    let gym = Gmisc.wrap_lexer Datatypes_lexer.parse_gYearMonth t in
    if sign
    then gym
    else DateTime.negate_gYearMonth gym
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not a gYearMonth value")))

let gYear_of_untyped t =
  try
    let (sign,t) = get_sign t in
    let gy = Gmisc.wrap_lexer Datatypes_lexer.parse_gYear t in
    if sign
    then gy
    else DateTime.negate_gYear gy
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not a gYear value")))

let gMonthDay_of_untyped t =
  try
    Gmisc.wrap_lexer Datatypes_lexer.parse_gMonthDay t
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not a gMonthDay value")))

let gDay_of_untyped t =
  try
    Gmisc.wrap_lexer Datatypes_lexer.parse_gDay t
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not a Day value")))

let gMonth_of_untyped t =
  try
    Gmisc.wrap_lexer Datatypes_lexer.parse_gMonth t
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not a gMonth value")))

let hexBinary_of_untyped t = 
  try 
    Gmisc.binary_of_hexString (Whitespace.remove_whitespace(t))
  with
  | _ -> raise (Query (Validation("Text: \"" ^ t ^ "\" not a hexBinary value. ")))

(* XML Schema Datatypes 3.2.16 base64Binary

   Base64Binary  ::=  ((B64S B64S B64S B64S)*
                      ((B64S B64S B64S B64) |
                       (B64S B64S B16S '=') |
                       (B64S B04S '=' #x20? '=')))?
   B64S         ::= B64 #x20?
   B16S         ::= B16 #x20?
   B04S         ::= B04 #x20?
   B04         ::=  [AQgw]
   B16         ::=  [AEIMQUYcgkosw048]
   B64         ::=  [A-Za-z0-9+/]
*)
let base64Binary_of_untyped t = 
  try 
      (* Netcoding.Base64 accepts the canonical grammar, which doesn't require 'AQgw' before '==' *)
    let v = Netencoding.Base64.decode t ~accept_spaces:true in
    let t' = Whitespace.remove_all_whitespace t in 
    let l = String.length t' in
    if (l > 2 && String.get t' (l - 2) = '=') then
      let c' = String.get t' (l - 3) in
      if not (c' = 'A' || c' = 'Q' || c' = 'g' || c' = 'w') then 
	raise (Invalid_argument "")
      else v
    else v
  with
  | _ -> raise (Query (Validation("Text: \"" ^ t ^ "\" not a base64Binary value")))

let anyURI_of_untyped t = 
  try 
    _kinda_uri_of_string t
  with
  | _ -> raise (Query (Validation("Text: \"" ^ t ^ "\" not a URI value")))

let qname_of_untyped nse t =
  try
    let uqname = (Namespace_names.uqname_element_of_raw_string (Whitespace.remove_whitespace t)) in
    resolve_anon_qname_to_symbol nse uqname
  with
  | _ ->
      raise (Query (Validation ("Text: \"" ^ t ^ "\" not a qname value")))

let notation_of_untyped t = t

let raise_outof_bounds i dt =
  let dtname = Namespace_symbols.symbol_prefix_string dt in
  let is = _string_of_integer i in
  raise (Query (Validation ("Integer value "
			    ^ is
			    ^ " is out of bound for type "
			    ^ dtname))) 

let check_nonPositiveInteger i dt =
  if not(_integer_le i _integer_zero)
  then raise_outof_bounds i dt

let check_negativeInteger i dt =
  if not(_integer_lt i _integer_zero)
  then raise_outof_bounds i dt

let check_long i dt =
  if not((_integer_le i _integer_long_max) &&
	 (_integer_ge i _integer_long_min))
  then raise_outof_bounds i dt

let check_int i dt =
  if not((_integer_le i _integer_int_max) &&
	 (_integer_ge i _integer_int_min))
  then raise_outof_bounds i dt

let check_short i dt =
  if not((_integer_le i _integer_short_max) &&
	 (_integer_ge i _integer_short_min))
  then raise_outof_bounds i dt

let check_byte i dt =
  if not((_integer_le i _integer_byte_max) &&
	 (_integer_ge i _integer_byte_min))
  then raise_outof_bounds i dt

let check_nonNegativeInteger i dt =
  if not(_integer_ge i _integer_zero)
  then raise_outof_bounds i dt

let check_positiveInteger i dt =
  if not(_integer_gt i _integer_zero)
  then raise_outof_bounds i dt

let check_unsignedLong i dt =
  if not((_integer_le i _integer_unsigned_long_max) &&
	 (_integer_ge i _integer_zero))
  then raise_outof_bounds i dt

let check_unsignedInt i dt =
  if not((_integer_le i _integer_unsigned_int_max) &&
	 (_integer_ge i _integer_zero))
  then raise_outof_bounds i dt

let check_unsignedShort i dt =
  if not((_integer_le i _integer_unsigned_short_max) &&
	 (_integer_ge i _integer_zero))
  then raise_outof_bounds i dt

let check_unsignedByte i dt =
  if not((_integer_le i _integer_unsigned_byte_max) &&
	 (_integer_ge i _integer_zero))
  then raise_outof_bounds i dt

let check_integer_facets i dt =
  begin
    if Namespace_symbols.symbol_equal dt xs_nonPositiveInteger
    then check_nonPositiveInteger i dt;
    if Namespace_symbols.symbol_equal dt xs_negativeInteger
    then check_negativeInteger i dt;
    if Namespace_symbols.symbol_equal dt xs_long
    then check_long i dt;
    if Namespace_symbols.symbol_equal dt xs_int
    then check_int i dt;
    if Namespace_symbols.symbol_equal dt xs_short
    then check_short i dt;
    if Namespace_symbols.symbol_equal dt xs_byte
    then check_byte i dt;
    if Namespace_symbols.symbol_equal dt xs_nonNegativeInteger
    then check_nonNegativeInteger i dt;
    if Namespace_symbols.symbol_equal dt xs_positiveInteger
    then check_positiveInteger i dt;
    if Namespace_symbols.symbol_equal dt xs_unsignedLong
    then check_unsignedLong i dt;
    if Namespace_symbols.symbol_equal dt xs_unsignedInt
    then check_unsignedInt i dt;
    if Namespace_symbols.symbol_equal dt xs_unsignedShort
    then check_unsignedShort i dt;
    if Namespace_symbols.symbol_equal dt xs_unsignedByte
    then check_unsignedByte i dt
  end

let integer_of_untyped dt t =
  let i =
    try
      Gmisc.wrap_lexer Datatypes_lexer.parse_integer t
    with
    | _ ->
	raise (Query (Validation ("Text: \"" ^ t ^ "\" not an integer value")))
  in
  begin
    check_integer_facets i dt;
    i
  end

(***************)
(* Comparisons *)
(***************)

let string_equal x1 x2 =
  x1 = x2
let bool_equal x1 x2 =
  x1 = x2
let float_equal x1 x2 =
  x1 = x2
let double_equal x1 x2 =
  x1 = x2
let duration_equal x1 x2 =
  if DateTime.duration_compare x1 x2 = 0 then true else false
let yearMonthDuration_equal x1 x2 =
  if DateTime.yearMonthDuration_compare x1 x2 = 0 then true else false
let dayTimeDuration_equal x1 x2 =
  if DateTime.dayTimeDuration_compare x1 x2 = 0 then true else false
let dateTime_equal tz_local x1 x2 =
  if DateTime.dateTime_compare tz_local x1 x2 = 0 then true else false
let time_equal tz_local x1 x2 =
  if DateTime.time_compare tz_local x1 x2 = 0 then true else false
let date_equal tz_local x1 x2 =
  if DateTime.date_compare tz_local x1 x2 = 0 then true else false
let gYearMonth_equal tz_local x1 x2 =
  if DateTime.gYearMonth_compare tz_local x1 x2 = 0 then true else false
let gYear_equal tz_local x1 x2 =
  if DateTime.gYear_compare tz_local x1 x2 = 0 then true else false
let gMonthDay_equal tz_local x1 x2 =
  if DateTime.gMonthDay_compare tz_local x1 x2 = 0 then true else false
let gDay_equal tz_local x1 x2 =
  if DateTime.gDay_compare tz_local x1 x2 = 0 then true else false
let gMonth_equal tz_local x1 x2 =
  if DateTime.gMonth_compare tz_local x1 x2 = 0 then true else false
let hexBinary_equal x1 x2 = x1 = x2
let base64Binary_equal x1 x2 = x1 = x2
let anyURI_equal x1 x2 = AnyURI._uri_eq x1 x2
let qname_equal x1 x2 =
  Namespace_symbols.symbol_equal x1 x2 (* Should have both same URI and same local name - Jerome *)
let notation_equal x1 x2 =
  x1 = x2
let untyped_equal x1 x2 =
  x1 = x2

(* value less than or equal *)

let string_lteq x1 x2 =
  x1 <= x2
let bool_lteq x1 x2 =
  x1 <= x2
let float_lteq x1 x2 =
  x1 <= x2
let double_lteq x1 x2 =
  x1 <= x2
let duration_lteq x1 x2 =
  raise (Query (Internal_Error "lteq not supported in durations"))
let dateTime_lteq tz_local x1 x2 =
  if not (DateTime.dateTime_compare tz_local x1 x2 = 1) then true else false
let time_lteq tz_local x1 x2 =
  if not (DateTime.time_compare tz_local x1 x2 = 1) then true else false
let date_lteq tz_local x1 x2 =
  if not (DateTime.date_compare tz_local x1 x2 = 1) then true else false
let gYearMonth_lteq tz_local x1 x2 =
  if not (DateTime.gYearMonth_compare tz_local x1 x2 = 1) then true else false
let gYear_lteq tz_local x1 x2 =
  if not (DateTime.gYear_compare tz_local x1 x2 = 1) then true else false
let gMonthDay_lteq tz_local x1 x2 =
  if not (DateTime.gMonthDay_compare tz_local x1 x2 = 1) then true else false
let gDay_lteq tz_local x1 x2 =
  if not (DateTime.gDay_compare tz_local x1 x2 = 1) then true else false
let gMonth_lteq tz_local x1 x2 =
  if not (DateTime.gMonth_compare tz_local x1 x2 = 1) then true else false
let hexBinary_lteq x1 x2 =
  x1 <= x2
let base64Binary_lteq x1 x2 =
  x1 <= x2
(*
let anyURI_lteq x1 x2 = x1 <= x2
*)
let qname_lteq x1 x2 =
  (x1 <= x2)   (* It does not look quite right here - Jerome *)
let notation_lteq x1 x2 =
  x1 <= x2
let yearMonthDuration_lteq x1 x2 =
  if not (DateTime.yearMonthDuration_compare x1 x2 = 1) then true else false
let dayTimeDuration_lteq x1 x2 =
  if not (DateTime.dayTimeDuration_compare x1 x2 = 1) then true else false
let untyped_lteq x1 x2 =
  x1 <= x2

(* value less than *)

let string_lt x1 x2 =
  x1 < x2
let bool_lt x1 x2 =
  x1 < x2
let float_lt x1 x2 =
  x1 < x2
let double_lt x1 x2 =
  x1 < x2
let duration_lt x1 x2 =
  raise (Query (Internal_Error "lt not supported in durations"))
let dateTime_lt tz_local  x1 x2 =
  if DateTime.dateTime_compare tz_local x1 x2 = -1 then true else false
let time_lt tz_local  x1 x2 =
  if DateTime.time_compare tz_local x1 x2 = -1 then true else false
let date_lt tz_local  x1 x2 =
  if DateTime.date_compare tz_local x1 x2 = -1 then true else false
let gYearMonth_lt tz_local x1 x2 =
  if DateTime.gYearMonth_compare tz_local x1 x2 = -1 then true else false
let gYear_lt tz_local x1 x2 =
  if DateTime.gYear_compare tz_local x1 x2 = -1 then true else false
let gMonthDay_lt tz_local x1 x2 =
  if DateTime.gMonthDay_compare tz_local x1 x2 = -1 then true else false
let gDay_lt tz_local x1 x2 =
  if DateTime.gDay_compare tz_local x1 x2 = -1 then true else false
let gMonth_lt tz_local x1 x2 =
  if DateTime.gMonth_compare tz_local x1 x2 = -1 then true else false
let hexBinary_lt x1 x2 =
  x1 < x2
let base64Binary_lt x1 x2 =
  x1 < x2
let qname_lt x1 x2 =
  (x1 < x2)   (* It does not look quite right here - Jerome *)
let notation_lt x1 x2 =
  x1 < x2
let yearMonthDuration_lt x1 x2 =
  if DateTime.yearMonthDuration_compare x1 x2 = -1 then true else false
let dayTimeDuration_lt x1 x2 =
  if DateTime.dayTimeDuration_compare x1 x2 = -1 then true else false
let untyped_lt x1 x2 =
  x1 < x2

(* value greater than or equal *)

let string_gteq x1 x2 =
  x1 >= x2
let bool_gteq x1 x2 =
  x1 >= x2
let float_gteq x1 x2 =
  x1 >= x2
let double_gteq x1 x2 =
  x1 >= x2
let duration_gteq x1 x2 =
  raise (Query (Internal_Error "gteq not supported in durations"))
let dateTime_gteq tz_local  x1 x2 =
  if not (DateTime.dateTime_compare tz_local x1 x2 = -1) then true else false
let time_gteq tz_local  x1 x2 =
  if not (DateTime.time_compare tz_local x1 x2 = -1) then true else false
let date_gteq tz_local  x1 x2 =
  if not (DateTime.date_compare tz_local x1 x2 = -1) then true else false
let gYearMonth_gteq tz_local x1 x2 =
  if not (DateTime.gYearMonth_compare tz_local x1 x2 = -1) then true else false
let gYear_gteq tz_local x1 x2 =
  if not (DateTime.gYear_compare tz_local x1 x2 = -1) then true else false
let gMonthDay_gteq tz_local x1 x2 =
  if not (DateTime.gMonthDay_compare tz_local x1 x2 = -1) then true else false
let gDay_gteq tz_local x1 x2 =
  if not (DateTime.gDay_compare tz_local x1 x2 = -1) then true else false
let gMonth_gteq tz_local x1 x2 =
  if not (DateTime.gMonth_compare tz_local x1 x2 = -1) then true else false
let hexBinary_gteq x1 x2 =
  x1 >= x2
let base64Binary_gteq x1 x2 =
  x1 >= x2
let qname_gteq x1 x2 =
  (x1 >= x2)   (* It does not look quite right here - Jerome *)
let notation_gteq x1 x2 =
  x1 >= x2
let yearMonthDuration_gteq x1 x2 =
  if not (DateTime.yearMonthDuration_compare x1 x2 = -1) then true else false
let dayTimeDuration_gteq x1 x2 =
  if not (DateTime.dayTimeDuration_compare x1 x2 = -1) then true else false
let untyped_gteq x1 x2 =
  x1 >= x2

(* value greater than *)

let string_gt x1 x2 =
  x1 > x2
let bool_gt x1 x2 =
  x1 > x2
let float_gt x1 x2 =
  x1 > x2
let double_gt x1 x2 =
  x1 > x2
let duration_gt x1 x2 =
  raise (Query (Internal_Error "gt not supported in durations"))
let dateTime_gt tz_local x1 x2 =
  if DateTime.dateTime_compare tz_local x1 x2 = 1 then true else false
let time_gt tz_local x1 x2 =
  if DateTime.time_compare tz_local x1 x2 = 1 then true else false
let date_gt tz_local x1 x2 =
  if DateTime.date_compare tz_local x1 x2 = 1 then true else false
let gYearMonth_gt tz_local x1 x2 =
  if DateTime.gYearMonth_compare tz_local x1 x2 = 1 then true else false
let gYear_gt tz_local x1 x2 =
  if DateTime.gYear_compare tz_local x1 x2 = 1 then true else false
let gMonthDay_gt tz_local x1 x2 =
  if DateTime.gMonthDay_compare tz_local x1 x2 = 1 then true else false
let gDay_gt tz_local x1 x2 =
  if DateTime.gDay_compare tz_local x1 x2 = 1 then true else false
let gMonth_gt tz_local x1 x2 =
  if DateTime.gMonth_compare tz_local x1 x2 = 1 then true else false
let hexBinary_gt x1 x2 =
  x1 > x2
let base64Binary_gt x1 x2 =
  x1 > x2
let qname_gt x1 x2 =
  (x1 > x2)   (* It does not look quite right here - Jerome *)
let notation_gt x1 x2 =
  x1 > x2
let yearMonthDuration_gt x1 x2 =
  if DateTime.yearMonthDuration_compare x1 x2 = 1 then true else false
let dayTimeDuration_gt x1 x2 =
  if DateTime.dayTimeDuration_compare x1 x2 = 1 then true else false
let untyped_gt x1 x2 =
  x1 > x2


(*******************************)
(* Operations on atomic values *)
(*******************************)

(* Generic operations on values *)

let is_negative_zero f =
  (f = 0.0) && ((string_of_float f).[0] = '-')

(*
  serialize_float    

  From F&O Section 17.7:

  * If ST is xs:float or xs:double, then:

    o If SV is NaN, the resulting value is "NaN".

    o If SV has an absolute value that is greater than or equal to
    0.000001 (one millionth) and less than 1000000 (one million), then
    the value is converted to an xs:decimal and the resulting
    xs:decimal is converted to an xs:string using the rules above.

    o Otherwise, the canonical lexical representation of SV value is
      returned, as defined in [XML Schema Part 2: Datatypes].

*)

let serialize_float_precision prec f =
  if is_negative_zero f then "-0"
  else if f = 0.0 then "0"
  else if (compare f nan) = 0 then "NaN"
  else if f = infinity then "INF"
  else if f = -.infinity then "-INF"
  else
    if (abs_float f) > 0.000001 && (abs_float f) < 1000000.0 then
      let f =
	if prec
	then
	  f
	else
	  (float_of_string (Printf.sprintf "%.7f" f))
      in
      _string_of_decimal(_decimal_of_float f)
(*
      _string_of_decimal(_decimal_of_float f) (*(_decimal_of_string (Printf.sprintf "%f" f))*)
*)
    else
      let s =
	if prec
	then
	  let range = log10 (abs_float f) in
	  if (range <= -11.0) then Printf.sprintf "%.14E" f
	  else if (range < 100.0) then Printf.sprintf "%.15E" f
	  else Printf.sprintf "%.16E" f
	else
	  Printf.sprintf "%.7E" f
      in
      let (mantisse, rest) =
	Gmisc.split_left_on_char s '.'
      in
      let (decimal,sign,expo) =
	try
	  let (decimal,expo) = Gmisc.split_left_on_char rest 'E' in
	  let (sign,expo) = (String.sub expo 0 1), (String.sub expo 1 ((String.length expo)-1)) in
	  (decimal, sign, expo)
	with
	| _ ->
	    (rest,"+","0")
      in
      let new_mantisse = mantisse in
      let new_decimal = Gmisc.remove_trailing decimal '0' in
      let new_sign = if sign = "+" then "" else sign in
      let new_expo = Gmisc.remove_leading expo '0' in
      new_mantisse ^ "." ^ new_decimal ^ "E" ^ new_sign ^ new_expo

let serialize_float f  = serialize_float_precision false (_cast_double_to_float f)
let serialize_double f = serialize_float_precision true f

(* Serialize binary types *)

let serialize_base64Binary b = Netencoding.Base64.encode b
let serialize_hexBinary h = Gmisc.string_of_hexBinary h

(******************************)
(* Operations on atomic types *)
(******************************)

let atomic_is_numeric dt =
  match dt with
  | ATDecimal -> true
  | ATFloat -> true
  | ATDouble -> true
  | ATInteger -> true
  | _ -> false

let atomic_is_anyURI dt =
  match dt with
  | ATAnyURI -> true
  | _ -> false

let atomic_is_anystring dt =
  match dt with
  | ATString -> true
  | ATAnyURI -> true
  | _ -> false

let atomic_type_subsumes st st' =
  match (st,st') with
  | (ATString, ATString) -> true
  | (ATBoolean, ATBoolean) -> true
  | (ATDecimal, ATDecimal) -> true
  | (ATFloat,ATFloat) -> true
  | (ATDouble, ATDouble) -> true
  | (ATDuration, ATDuration) -> true
  | (ATDateTime, ATDateTime) -> true
  | (ATTime, ATTime) -> true
  | (ATDate, ATDate) -> true
  | (ATGYearMonth, ATGYearMonth) -> true
  | (ATGYear, ATGYear) -> true
  | (ATGMonthDay, ATGMonthDay) -> true
  | (ATGDay, ATGDay) -> true
  | (ATGMonth, ATGMonth) -> true
  | (ATHexBinary, ATHexBinary) -> true
  | (ATBase64Binary, ATBase64Binary) -> true
  | (ATAnyURI, ATAnyURI) -> true
  | (ATQName, ATQName) -> true
  | (ATNOTATION, ATNOTATION) -> true
  | (ATInteger, ATInteger) -> true
    (* Note:
         Untyped is in a separate part of the type hierarchy.
       - Jerome
     *)
  | (ATYearMonthDuration, ATYearMonthDuration) -> true
  | (ATDayTimeDuration, ATDayTimeDuration) -> true
  | (ATUntypedAtomic, ATUntypedAtomic) -> true
  | (ATAnyAtomic, _) -> true
  | _ -> false

(******************************)
(* Operations on atomic types *)
(******************************)

(* mappings for built-in atomic types qnames *)

let built_in_type_table = [
  (xs_string, ATString);
  (xs_boolean, ATBoolean);
  (xs_decimal, ATDecimal);
  (xs_float, ATFloat);
  (xs_double, ATDouble);
  (xs_duration, ATDuration);
  (xs_dateTime, ATDateTime);
  (xs_time, ATTime);
  (xs_date, ATDate);
  (xs_gYearMonth, ATGYearMonth);
  (xs_gYear, ATGYear);
  (xs_gMonthDay, ATGMonthDay);
  (xs_gDay, ATGDay);
  (xs_gMonth, ATGMonth);
  (xs_hexBinary, ATHexBinary);
  (xs_base64Binary, ATBase64Binary);
  (xs_anyURI, ATAnyURI);
  (xs_QName, ATQName);
  (xs_NOTATION, ATNOTATION);

  (xs_integer, ATInteger);
  
  (* Types in xs namespace *)
  (xs_yearMonthDuration, ATYearMonthDuration);
  (xs_dayTimeDuration, ATDayTimeDuration);
  (xs_untypedAtomic, ATUntypedAtomic);
  (xs_anyAtomicType, ATAnyAtomic);
]

let bltin_types : (atomic_type) SQNameHashtbl.t = SQNameHashtbl.create 121

let lookup_bltin_type rqname =
  try
    SQNameHashtbl.find bltin_types rqname
  with
  | _ ->
      raise (Query (Mapping_Failure ("Cannot find atomic type " ^ (symbol_prefix_string rqname))))
let add_bltin_type (rqname,t) = ignore (SQNameHashtbl.add bltin_types rqname t)

let _ = 
  try
    List.iter add_bltin_type built_in_type_table
  with
  | e ->
      begin
	eprintf_error "  " e;
	Format.fprintf (!Conf.glx_err_formatter) "@."; 
      end

(* mapping from atomic types to qnames *)

let symbol_of_primitive_type pt =
  match pt with
  | ATString ->       	   xs_string
  | ATBoolean ->      	   xs_boolean
  | ATDecimal ->      	   xs_decimal
  | ATFloat ->        	   xs_float
  | ATDouble ->       	   xs_double
  | ATDuration ->     	   xs_duration
  | ATDateTime ->     	   xs_dateTime
  | ATTime -> 	      	   xs_time
  | ATDate -> 	      	   xs_date
  | ATGYearMonth ->   	   xs_gYearMonth
  | ATGYear ->        	   xs_gYear
  | ATGMonthDay ->    	   xs_gMonthDay
  | ATGDay ->         	   xs_gDay
  | ATGMonth ->       	   xs_gMonth
  | ATHexBinary ->    	   xs_hexBinary
  | ATBase64Binary -> 	   xs_base64Binary
  | ATAnyURI ->       	   xs_anyURI
  | ATQName ->        	   xs_QName
  | ATNOTATION ->     	   xs_NOTATION
  | ATInteger ->      	   xs_integer
  | ATYearMonthDuration -> xs_yearMonthDuration
  | ATDayTimeDuration ->   xs_dayTimeDuration
  | ATUntypedAtomic ->     xs_untypedAtomic
  | ATAnyAtomic ->         xs_anyAtomicType

(* Unit tag of text node *)

let unit_symbol_of_base_type bt =
  match bt with
  | ATString ->
      stringsym
  | ATBoolean ->
      booleansym
  | ATDecimal ->
      decimalsym
  | ATFloat ->
      floatsym
  | ATDouble ->
      doublesym
  | ATDuration ->
      durationsym
  | ATDateTime ->
      dateTimesym
  | ATTime ->
      timesym
  | ATDate ->
      datesym
  | ATGYearMonth ->
      gYearMonthsym
  | ATGYear ->
      gYearsym
  | ATGMonthDay ->
      gMonthDaysym
  | ATGDay ->
      gDaysym
  | ATGMonth ->
      gMonthsym
  | ATHexBinary ->
      hexBinarysym
  | ATBase64Binary ->
      base64Binarysym
  | ATAnyURI ->
      anyURIsym
  | ATQName ->
      qnamesym
  | ATNOTATION ->
      notationsym
  | ATInteger ->
      integersym
  | ATUntypedAtomic ->
      untypedAtomicsym
  | ATYearMonthDuration ->
      yearMonthDurationsym
  | ATDayTimeDuration ->
      dayTimeDurationsym
  | ATAnyAtomic -> 
      anyAtomicTypesym

(* Replaces the above. *)
let can_be_promoted_to at = 
  match at with
    | ATString -> [ATString; ATAnyAtomic] 
    | ATBoolean -> [ATBoolean; ATAnyAtomic]
    | ATInteger -> [ATInteger; ATDecimal; ATFloat; ATDouble; ATAnyAtomic]
    | ATDecimal -> [ATDecimal; ATFloat; ATDouble; ATAnyAtomic]
    | ATFloat -> [ATFloat; ATDouble; ATAnyAtomic]
    | ATDouble -> [ATDouble; ATAnyAtomic]
    | ATDuration -> [ATDuration; ATAnyAtomic]
    | ATDateTime -> [ATDateTime; ATAnyAtomic]
    | ATTime -> [ATTime; ATAnyAtomic] 
    | ATDate -> [ATDate; ATAnyAtomic]
    | ATGYearMonth -> [ATGYearMonth; ATAnyAtomic]
    | ATGYear -> [ATGYear; ATAnyAtomic]
    | ATGMonthDay -> [ATGMonthDay; ATAnyAtomic]
    | ATGDay -> [ATGDay; ATAnyAtomic]
    | ATGMonth -> [ATGMonth; ATAnyAtomic]
    | ATHexBinary -> [ATHexBinary; ATAnyAtomic]
    | ATBase64Binary -> [ATBase64Binary; ATAnyAtomic]
    | ATAnyURI -> [ATAnyURI; ATString; ATAnyAtomic]
    | ATQName -> [ATQName; ATAnyAtomic]
    | ATNOTATION -> [ATNOTATION; ATAnyAtomic]
    | ATYearMonthDuration -> [ATYearMonthDuration; ATAnyAtomic]
    | ATDayTimeDuration -> [ATDayTimeDuration; ATAnyAtomic]
    | ATUntypedAtomic -> [ATUntypedAtomic; ATAnyAtomic]
    | ATAnyAtomic -> [ATAnyAtomic]

(* Small helper functions to remove the calls to list mem *)
let bt_can_be_promoted_to bt at =
  match (at,bt) with
    (* at = bt, this reforms an unnecessary polymorphic equality *)
  | (ATString, ATString)         | (ATBoolean, ATBoolean) | (ATDecimal, ATDecimal)
  | (ATFloat, ATFloat)           | (ATDouble, ATDouble)   | (ATDuration, ATDuration)
  | (ATDateTime, ATDateTime)     | (ATTime, ATTime)       | (ATDate, ATDate)
  | (ATGYearMonth, ATGYearMonth) | (ATGYear, ATGYear)     | (ATGMonthDay, ATGMonthDay)
  | (ATGDay, ATGDay)             | (ATGMonth, ATGMonth)
  | (ATHexBinary, ATHexBinary)   | (ATBase64Binary, ATBase64Binary)
  | (ATAnyURI, ATAnyURI)         | (ATQName, ATQName)     | (ATNOTATION, ATNOTATION)
  | (ATYearMonthDuration, ATYearMonthDuration) | (ATDayTimeDuration, ATDayTimeDuration)
  | (ATInteger, ATInteger)
  | (ATAnyAtomic, ATAnyAtomic) ->
  (* Same type and can be promoted *)
      (true, true)
  (* Derived atomic types *)
  (* Duration promotions *)
  | (ATYearMonthDuration, ATDuration) | (ATDayTimeDuration, ATDuration)
  (* Integer promotions *)
  | (ATInteger, ATDecimal) |  (ATInteger, ATFloat) | (ATInteger, ATDouble)
  (* Decimal promotions *)
  | (ATDecimal, ATFloat)   | (ATDecimal, ATDouble)
  | (ATFloat, ATDouble)
  | (ATAnyURI, ATString) ->
  (* Not the same type but can be promoted *)
      (false, true)
  | (_, ATAnyAtomic) ->
      (* Not the same type but can be promoted *)
      (false, true)
        (* not the same type and can not be promoted *)
  | _ -> (false, false)

let untyped_atomic_type = ATUntypedAtomic


(* Printing *)

(* Atomic types *)

let string_of_atomic_type at =
  match at with
  | ATString   	   	-> "string"
  | ATBoolean  	   	-> "boolean"
  | ATDecimal  	   	-> "decimal"
  | ATFloat    	   	-> "float"
  | ATDouble   	   	-> "double"
  | ATDuration 	   	-> "duration"
  | ATDateTime 	   	-> "dateTime"
  | ATTime     	   	-> "time"
  | ATDate     	   	-> "date"
  | ATGYearMonth   	-> "gYearMonth"
  | ATGYear        	-> "gYear"
  | ATGMonthDay    	-> "gMonthDay"
  | ATGDay         	-> "gDay"
  | ATGMonth       	-> "gMonth"
  | ATHexBinary    	-> "hexBinary"
  | ATBase64Binary 	-> "base64Binary"
  | ATAnyURI   	   	-> "anyURI"
  | ATQName    	   	-> "QName"
  | ATNOTATION 	   	-> "NOTATION"
  | ATInteger  	   	-> "integer"
  | ATYearMonthDuration -> "yearMonthDuration"
  | ATDayTimeDuration 	-> "dayTimeDuration"
  | ATUntypedAtomic    	-> "untypedAtomic"
  | ATAnyAtomic    	-> "anyAtomicType"

let atomic_type_to_int at =
  match at with
    | ATString -> 0
    | ATBoolean -> 1
    | ATDecimal -> 2
    | ATFloat   -> 3
    | ATDouble  -> 4
    | ATDuration -> 5
    | ATDateTime -> 6
    | ATTime     -> 7
    | ATDate     -> 8
    | ATGYearMonth -> 9
    | ATGYear    -> 10
    | ATGMonthDay -> 11
    | ATGDay      -> 12
    | ATGMonth    -> 13
    | ATHexBinary -> 14
    | ATBase64Binary -> 15
    | ATAnyURI    -> 16
    | ATQName     -> 17
    | ATNOTATION  -> 18

    (* Derived atomic types *)
    | ATInteger   -> 19

    (* xs data types *)
    | ATYearMonthDuration -> 20
    | ATDayTimeDuration   -> 21 
    | ATUntypedAtomic     -> 22
    | ATAnyAtomic         -> 23


let compare_types t t1 = 
  let t_int  = atomic_type_to_int t  in
  let t1_int = atomic_type_to_int t1 in
    t_int - t1_int

let base64_of_hex v = v 
let hex_of_base64 v = v

