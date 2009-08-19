(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_camomile_0.7.ml,v 1.2 2007/10/24 15:15:17 simeon Exp $ *)

(* Module: Glx_camomile
   Description:
     This module is a wrapper over Camomile operations.
*)

open Error

let makedatadir d = Filename.concat d "database"
let makelocaledir d = Filename.concat d "locales"
let makecharmapdir d = Filename.concat d "charmaps"
let makeunimapdir d = Filename.concat d "mappings"

module Camomileconfig =
struct
(* configuration for tools *)
  let datadir = 
    try makedatadir (Sys.getenv "UNICODE_MAPS") with Not_found -> 
      CamomileLibrary.CamomileDefaultConfig.datadir
  let localedir = 
    try makelocaledir (Sys.getenv "UNICODE_MAPS") with Not_found -> 
      CamomileLibrary.CamomileDefaultConfig.localedir
  let charmapdir = 
    try makecharmapdir (Sys.getenv "UNICODE_MAPS") with Not_found -> 
      CamomileLibrary.CamomileDefaultConfig.charmapdir
  let unimapdir =
    try makeunimapdir (Sys.getenv "UNICODE_MAPS") with Not_found -> 
      CamomileLibrary.CamomileDefaultConfig.unimapdir
end

module TextUTF8 = CamomileLibrary.UNF.Make(Camomileconfig)(CamomileLibrary.UTF8)

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
  CamomileLibrary.UTF8.compare s1 s2

let utf8_code_points_of_string s =
  try
    let codept_list = ref [] in
    CamomileLibrary.UTF8.iter
      (fun uc -> let ic = CamomileLibrary.UChar.code uc in codept_list := (!codept_list) @ [ic]) s;
    !codept_list
  with 
  | CamomileLibrary.UChar.Out_of_range ->
      raise(Query(Unicode_Error("Unicode character cannot be represented by positive integer.")))

