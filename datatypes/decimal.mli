(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: decimal.mli,v 1.18 2007/02/01 22:08:46 simeon Exp $ *)

(* Module Decimal
   Description:
     This modules implement internal support for the XML Schema
     primitive simple type decimal, and the corresponding derived
     types.
 *)

(*********************************************************)
(* Internal representation of decimals and derived types *)
(*********************************************************)

type _decimal  = Num.num
type _integer  = Big_int.big_int



(*************************)
(* Operations on decimal *)
(*************************)

val _decimal_zero      	 : _decimal
val _decimal_one      	 : _decimal
val _decimal_two      	 : _decimal
val _decimal_ten      	 : _decimal
val _decimal_onehalf   	 : _decimal
  
val _decimal_of_float     : float -> _decimal
val _decimal_of_string 	 : string -> _decimal
val _string_of_decimal 	 : _decimal -> string
val _decimal_string_of_decimal : _decimal -> string
val _float_of_decimal 	 : _decimal -> float
val _string_of_float     : float -> string

val _decimal_eq        	 : _decimal -> _decimal -> bool
val _decimal_lt        	 : _decimal -> _decimal -> bool
val _decimal_le        	 : _decimal -> _decimal -> bool
val _decimal_gt        	 : _decimal -> _decimal -> bool
val _decimal_ge        	 : _decimal -> _decimal -> bool
val _decimal_add       	 : _decimal -> _decimal -> _decimal
val _decimal_sub       	 : _decimal -> _decimal -> _decimal
val _decimal_mult      	 : _decimal -> _decimal -> _decimal
val _decimal_div       	 : _decimal -> _decimal -> _decimal
val _decimal_idiv      	 : _decimal -> _decimal -> _integer
val _decimal_power     	 : _decimal -> _decimal -> _decimal
val _decimal_mod       	 : _decimal -> _decimal -> _decimal
val _decimal_unary_minus : _decimal -> _decimal
val _decimal_floor       : _decimal -> _decimal
val _decimal_ceil        : _decimal -> _decimal

val _float_idiv      	 : float    -> float    -> _integer
val _float_mod      	 : float    -> float    -> float

val _decimal_of_integer  : _integer -> _decimal
val _decimal_of_int      : int -> _decimal
val _int_of_decimal      : _decimal -> int

(*************************)
(* Operations on integer *)
(*************************)

val _integer_zero         : _integer
val _integer_one          : _integer

val _integer_long_max          	: _integer
val _integer_unsigned_long_max 	: _integer
val _integer_long_min          	: _integer

val _integer_int_max           	: _integer
val _integer_unsigned_int_max  	: _integer
val _integer_int_min           	: _integer

val _integer_short_max          : _integer
val _integer_unsigned_short_max : _integer
val _integer_short_min          : _integer

val _integer_byte_max           : _integer
val _integer_unsigned_byte_max  : _integer
val _integer_byte_min           : _integer

val _integer_of_string 	  : string -> _integer
val _string_of_integer 	  : _integer -> string

val _integer_eq 	  : _integer -> _integer -> bool
val _integer_lt 	  : _integer -> _integer -> bool
val _integer_le 	  : _integer -> _integer -> bool
val _integer_gt 	  : _integer -> _integer -> bool
val _integer_ge 	  : _integer -> _integer -> bool
val _integer_add  	  : _integer -> _integer -> _integer
val _integer_sub  	  : _integer -> _integer -> _integer
val _integer_mult 	  : _integer -> _integer -> _integer
val _integer_div  	  : _integer -> _integer -> _decimal
val _integer_idiv  	  : _integer -> _integer -> _integer
val _integer_mod  	  : _integer -> _integer -> _integer
val _integer_unary_minus  : _integer -> _integer

val _integer_of_decimal   : _decimal -> _integer
val _float_of_integer     : _integer -> float

val is_nan                     : float -> bool
val _cast_decimal_to_integer   : _decimal -> _integer
val _cast_float_to_integer     : float -> _integer
val _cast_double_to_float      : float -> float
val _cast_double_to_decimal    : float -> _decimal

val _integer_of_int       : int -> _integer
val _int_of_integer       : _integer -> int

