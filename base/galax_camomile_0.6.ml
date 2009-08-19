(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_camomile_0.6.ml,v 1.1 2007/09/27 19:19:28 simeon Exp $ *)

(* Module: Glx_camomile
   Description:
     This module is a wrapper over Camomile operations.
*)

open Error

module TextUTF8 = Camomile.UNF.Make(Camomile.UTF8)

let nfc x = TextUTF8.nfc x
let nfd x = TextUTF8.nfd x
let nfkc x = TextUTF8.nfkc x
let nfkd x = TextUTF8.nfkd x

let utf8_string_of_code_point i =
  Encoding.character (Encoding.get_internal_encoding ()) i

let utf8_add_point_to_buffer b i =
  let c = Encoding.character (Encoding.get_internal_encoding ()) i in
  Buffer.add_string b c

let utf8_string_of_code_points c =
  let b = Buffer.create 10 in
  List.iter (utf8_add_point_to_buffer b) c;
  Buffer.contents b

let utf8_codepoint_compare s1 s2 =
  Camomile.UTF8.compare s1 s2

let utf8_code_points_of_string s =
  try
    let codept_list = ref [] in
    Camomile.UTF8.iter
      (fun uc -> let ic = Camomile.UChar.code uc in codept_list := (!codept_list) @ [ic]) s;
    !codept_list
  with 
  | Camomile.UChar.Out_of_range ->
      raise(Query(Unicode_Error("Unicode character cannot be represented by positive integer.")))

