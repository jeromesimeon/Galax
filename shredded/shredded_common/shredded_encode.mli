(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* Module: Shredded_Jungle_encoder
   Description:
     This module implements the string encoding function for integers  
*)

(* Turns a Caml integer into a shredded_jungle string to be used for storage *)

(* Array based versions *)
val needed_length_for_int : int
val serialized_int_size   : int

val inplace_encode_int: char array -> int (* offset *) -> int (* value *) -> unit
val inplace_decode_int: char array -> int (* offset *) -> int

val inplace_encode_int32: char array -> int (* offset *) -> Int32.t (* value *) -> unit
val inplace_decode_int32: char array -> int (* offset *) -> Int32.t

val inplace_encode_int64: char array -> int (* offset *) -> Int64.t (* value *) -> unit
val inplace_decode_int64: char array -> int (* offset *) -> Int64.t

val length_bits_of_string : string -> int
val string_of_bits : char array -> string
val bits_of_string : string -> char array 

val inplace_decode_string : char array -> int (* offset *) -> int (* length *) -> string
val inplace_encode_string : char array -> int (* offset *) -> string (* value *) -> unit

val inplace_decode_int64_pair : char array -> int (* offset *) -> (Int64.t * Int64.t)
val inplace_encode_int64_pair : char array -> int (* offset *) -> (Int64.t * Int64.t) (* value *) -> unit

val copy_encode   : int -> char array
