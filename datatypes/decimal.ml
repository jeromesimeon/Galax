(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: decimal.ml,v 1.22 2007/02/01 22:08:46 simeon Exp $ *)

(* Module Decimal
   Description:
     This modules implement internal support for the XML Schema
     primitive simple type decimal, and the corresponding derived
     types.
 *)

open Error

open Num
open Big_int

(* Note:
     This modules implement internal support for the XML Schema
     primitive simple type decimal, and the corresponding derived types.
   - Jerome
 *)

(*************)
(* Utilities *)
(*************)

(* Setting global parameters *)

let precision = 18

(* Note:
     18 is the minimum precision required by XML Schema.
   - Jerome
 *)


(*********************************************************)
(* Internal representation of decimals and derived types *)
(*********************************************************)

type _integer = big_int  (* xs:integer is arbitrary precision *)
type _decimal = num      (* xs:decimal is arbitrary precision *)


(*************************)
(* Operations on decimal *)
(*************************)

let _decimal_zero = num_of_int 0
let _decimal_ten = num_of_int 10
let _decimal_onehalf =
  let _bi_one = Big_int.big_int_of_int 1 in
  let _bi_two = Big_int.big_int_of_int 2 in
  Num.num_of_ratio(Ratio.create_ratio _bi_one _bi_two) 
let _decimal_one = num_of_int 1
let _decimal_two = num_of_int 2

let _decimal_of_string s =
  try
    try
      let (s1, s2) = Gmisc.split_left_on_char s '.' in
      let s1 = (* Fix in case there is no leading digit - Jerome *)
	match s1 with
	|	"" -> "0"
	|	"-" -> "-0"
	|	_ -> s1
      in
      let exponent = power_int_positive_int 10 (String.length s2) in
      let string_exponent = string_of_big_int exponent in
      let new_s = s1 ^ s2 ^ "/" ^ string_exponent in
      num_of_string new_s
    with
    | Not_found ->
	num_of_string s
  with
  | _ ->
      raise (Query (Cast_Error ("Error casting \"" ^ s ^ "\" to a decimal")))

let _decimal_of_float f = 
  try
    _decimal_of_string(Printf.sprintf "%.10F" f)
  with
  | _ -> 
      _decimal_of_string(Printf.sprintf "%f" f)

let _small_decimal_of_float f = 
  try
    _decimal_of_string(Printf.sprintf "%F" f)
  with
  | _ -> 
      _decimal_of_string(Printf.sprintf "%f" f)

let _float_of_decimal d = float_of_num d

(* 
  _string_of_decimal:

  From F&O Section 17.1.2:

  * If ST is xs:decimal, then:

    o If SV is in the value space of xs:integer, that is, if there are
      no significant digits after the decimal point, then the value is
      converted from an xs:decimal to an xs:integer and the resulting
      xs:integer is converted to an xs:string using the rule above.

    o Otherwise, the canonical lexical representation of SV is
      returned, as defined in [XML Schema Part 2: Datatypes].
*)
let _string_of_decimal n =
  let s = approx_num_fix precision n in
  let (mantisse, rest) =
    Gmisc.split_left_on_char s '.'
  in
  let new_mantisse = 
    try
      if (String.get mantisse 0) = '+'
      then
	String.sub s 1 ((String.length mantisse) - 1)
      else
	mantisse
    with
    | _ ->
	mantisse
  in
  let new_rest = Gmisc.remove_trailing rest '0' in
  if (new_rest = "0") || (new_rest = "") then new_mantisse 
  else new_mantisse ^ "." ^ new_rest

let _decimal_string_of_decimal n =
  let s = approx_num_fix precision n in
  let (mantisse, rest) =
    Gmisc.split_left_on_char s '.'
  in
  let new_mantisse = 
    try
      if (String.get mantisse 0) = '+'
      then
	String.sub s 1 ((String.length mantisse) - 1)
      else
	mantisse
    with
    | _ ->
	mantisse
  in
  let new_rest = Gmisc.remove_trailing rest '0' in
  if (new_rest = "0") || (new_rest = "") then new_mantisse ^ ".0"
  else new_mantisse ^ "." ^ new_rest

let _decimal_eq d1 d2 =
  eq_num d1 d2

let _decimal_lt d1 d2 =
  lt_num d1 d2

let _decimal_le d1 d2 =
  le_num d1 d2

let _decimal_gt d1 d2 =
  gt_num d1 d2

let _decimal_ge d1 d2 =
  ge_num d1 d2

let _decimal_add d1 d2 =
  add_num d1 d2

let _decimal_sub d1 d2 =
  sub_num d1 d2

let _decimal_mult d1 d2 =
  mult_num d1 d2

let _decimal_div d1 d2 =
  div_num d1 d2

let _decimal_idiv d1 d2 =
  let d = _decimal_div d1 d2 in
  let d1 = if ((sign_num d) = -1) then ceiling_num d else floor_num d in
  big_int_of_num d1

let _float_idiv f1 f2 =
  let d = _decimal_of_float (f1 /. f2) in
  let d1 = if ((sign_num d) = -1) then ceiling_num d else floor_num d in
  big_int_of_num d1

let _float_idiv f1 f2 =
  let d = _decimal_of_float (f1 /. f2) in
  let d1 = if ((sign_num d) = -1) then ceiling_num d else floor_num d in
  big_int_of_num d1

let _float_mod f1 f2 =
  mod_float f1 f2
(*
  let idiv = float_of_big_int (_float_idiv f1 f2) in
  f1 -. (idiv *. f2)*)

let _decimal_mod d1 d2 =
  let idiv = num_of_big_int (_decimal_idiv d1 d2) in
  _decimal_sub d1 (_decimal_mult idiv d2)

let _decimal_unary_minus d1 =
  minus_num d1 

let _decimal_power d1 d2 =
  power_num d1 d2

let _decimal_floor d1 =
  floor_num d1

let _decimal_ceil d1 =
  ceiling_num d1

let _decimal_of_integer i1 =
  num_of_big_int i1

let _float_of_integer i =
  float_of_big_int i

let _decimal_of_int i = num_of_int i
let _int_of_decimal d = int_of_num d


(*************************)
(* Operations on integer *)
(*************************)

let _integer_zero = zero_big_int
let _integer_one = unit_big_int

let _integer_of_string s =
  big_int_of_string s

let _integer_long_max = _integer_of_string "9223372036854775807"
let _integer_unsigned_long_max = _integer_of_string "18446744073709551615"
let _integer_long_min = _integer_of_string "-9223372036854775808"

let _integer_int_max = _integer_of_string "2147483647"
let _integer_unsigned_int_max = _integer_of_string "4294967295"
let _integer_int_min = _integer_of_string "-2147483648"

let _integer_short_max = _integer_of_string "32767"
let _integer_unsigned_short_max = _integer_of_string "65535"
let _integer_short_min = _integer_of_string "-32768"

let _integer_byte_max = _integer_of_string "127"
let _integer_unsigned_byte_max = _integer_of_string "255"
let _integer_byte_min = _integer_of_string "-128"

(* 
  _string_of_integer

  From F&O Sec 17.1.2:

  * If ST is xs:integer, TV is the canonical lexical representation of
    SV as defined in [XML Schema Part 2: Datatypes]. There is no
    decimal point.
*)
let _string_of_integer i =
  string_of_big_int i

let _integer_eq i1 i2 =
  eq_big_int i1 i2

let _integer_lt i1 i2 =
  lt_big_int i1 i2

let _integer_le i1 i2 =
  le_big_int i1 i2

let _integer_gt i1 i2 =
  gt_big_int i1 i2

let _integer_ge i1 i2 =
  ge_big_int i1 i2

let _integer_add i1 i2 =
  add_big_int i1 i2

let _integer_sub i1 i2 =
  sub_big_int i1 i2

let _integer_mult i1 i2 =
  mult_big_int i1 i2

let _integer_div i1 i2 =
  _decimal_div (_decimal_of_integer i1) (_decimal_of_integer i2)

let _integer_idiv i1 i2 =
  _decimal_idiv (_decimal_of_integer i1) (_decimal_of_integer i2)

let _integer_mod i1 i2 =
  let idiv = _integer_idiv i1 i2 in
  _integer_sub i1 (_integer_mult idiv i2)

let _integer_unary_minus i1 =
  minus_big_int i1 

let _integer_of_decimal d1 =
  if is_integer_num d1
  then
    big_int_of_num d1
  else
    raise (Query (Cast_Error ("Cannot cast decimal : " ^ (string_of_num d1) ^ " into an int")))

let _integer_of_int i =
  big_int_of_int i

let _int_of_integer i =
  try
    int_of_big_int i
  with
  | Failure _ ->
      raise (Query (Internal_Error ("Cannot cast integer : " ^ (string_of_big_int i) ^ " into an int")))

let _cast_decimal_to_integer d =
  if sign_num d < 0
  then
    big_int_of_num (ceiling_num d)
  else
    big_int_of_num (floor_num d)
  
let _cast_float_to_integer f =
  if f < 0.0
  then
    _integer_of_decimal (_decimal_of_float (ceil f))
  else
    _integer_of_decimal (_decimal_of_float (floor f))

let is_nan f =
  (classify_float f) = FP_nan

let min_float_mantissa = -. (2.0 ** 24.0) -. 1.0
let max_float_mantissa = (2.0 ** 24.0) -. 1.0

let is_integer_float f =
  fst (modf f) = 0.0

let is_float_mantissa f =
  (f >= min_float_mantissa) && (f <= max_float_mantissa)

let is_exponent_float n =
  (n >= -149) && (n <= 104)

let frexp_int f =
  let (x,n) = frexp f in
  let x' = ref x in
  let n' = ref n in
  while (not(is_integer_float !x') && (is_float_mantissa !x')) do
    decr n';
    x' := ldexp x (n - !n')
  done;
  (snd(modf !x'),!n')

let _cast_double_to_float f =
  if is_nan f then nan
  else if f = infinity then infinity
  else if f = (neg_infinity) then neg_infinity
  else
    (* Step 1: decompose f into m * 2^e where m and e are integers *)
    let (m,e) = frexp_int f in
    if (e > 104)
    then
      if m < 0.0 then neg_infinity else infinity
    else if (e < -149)
    then
      if m < 0.0 then -0.0 else 0.0
    else 
      (m *. (2.0 ** (float_of_int e)))

let _cast_double_to_decimal f =
  let s = Printf.sprintf "%f" f in
  _decimal_of_string s

(*

   From F&O 17.1.2 Casting to xs:string and xs:untypedAtomic

   If ST is xs:float or xs:double, then:

    * TV will be an xs:string in the lexical space of xs:double or
      xs:float that when converted to an xs:double or xs:float under
      the rules of 17.1.1 Casting from xs:string and xs:untypedAtomic
      produces a value that is equal to SV, or is "NaN" if SV is
      NaN. In addition, TV must satisfy the constraints in the
      following sub-bullets.

      o If SV has an absolute value that is greater than or equal to
      0.000001 (one millionth) and less than 1000000 (one million),
      then the value is converted to an xs:decimal and the resulting
      xs:decimal is converted to an xs:string according to the rules
      above, as though using an implementation of xs:decimal that
      imposes no limits on the totalDigits or fractionDigits facets.

      o If SV has the value positive or negative zero, TV is "0" or "-0" respectively.

      o If SV is positive or negative infinity, TV is the string "INF" or "-INF" respectively.

      o In other cases, the result consists of a mantissa, which has
      the lexical form of an xs:decimal, followed by the letter "E",
      followed by an exponent which has the lexical form of an
      xs:integer. Leading zeroes and "+" signs are prohibited in the
      exponent. For the mantissa, there must be a decimal point, and
      there must be exactly one digit before the decimal point, which
      must be non-zero. The "+" sign is prohibited. There must be at
      least one digit after the decimal point. Apart from this
      mandatory digit, trailing zero digits are prohibited.

   Note:

   The above rules allow more than one representation of the same
   value. For example, the xs:float value whose exact decimal
   representation is 1.26743223E15 might be represented by any of the
   strings "1.26743223E15", "1.26743222E15" or "1.26743224E15" (inter
   alia). It is implementation-dependent which of these
   representations is chosen.

*)
let _string_of_float f = 
   if (f > 0.000001 && f < 1000000.) then 
     _string_of_decimal(_decimal_of_float f)
   else if is_nan f then "NaN"
   else if f = infinity then "INF"
   else if f = (neg_infinity) then "-INF"
   else string_of_float f

let is_neg_zero f =
  (_string_of_float f = "-0.")

let is_neg f = 
  (_string_of_float f).[0] = '-'
