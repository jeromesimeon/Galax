(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: encoding.ml,v 1.13 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Encoding
   Description:
     Configuration of character encoding operations.
*)

open Error

let utf8_string = "UTF-8"


(* Internal stuff used to set-up Pxp resolvers *)

class warner =
  object 
    method warn w =
      print_endline ("WARNING: " ^ w)
  end

let global_warner = new warner

(**********************)
(* Character encoding *)
(**********************)

type encoding = Pxp_types.encoding
      (* All possible external encodings *)

type rep_encoding = Pxp_types.rep_encoding
      (* All possible internal encodings *)

(* Converting a string to an encoding *)

let encoding_of_string s =
  try
    let s' = if (s = "ISO-8859") then (s^"-1") else s in
    Netconversion.encoding_of_string s'
  with
  | _ ->
      raise (Query (Undefined ("Undefined character encoding: " ^ s ^". See http://www.w3.org/TR/REC-xml/#charencoding")))

(* Converting an encoding to a string *)

let string_of_encoding =
  try
    Netconversion.string_of_encoding
  with
  | _ ->
      raise (Query (Undefined "Undefined character encoding"))

(* Converting an internal encoding to an encoding *)

let encoding_of_rep_encoding enc =
  match enc with
  | `Enc_utf8 -> `Enc_utf8
  | `Enc_iso88591 -> `Enc_iso88591
  | _ ->
      raise (Query (Undefined "Internal encoding can only be UTF8 or ISO-8859-1"))

(* Converting an encoding to an internal encoding *)

let rep_encoding_of_encoding enc =
  match enc with
  | `Enc_utf8 -> `Enc_utf8
  | `Enc_iso88591 -> `Enc_iso88591
  | _ ->
      raise (Query (Undefined "Internal encoding can only be UTF8 or ISO-8859-1"))


(* Character encoding *)

let internal_encoding = ref `Enc_utf8
let output_encoding   = ref `Enc_utf8

let set_internal_encoding enc =
  internal_encoding := enc;
  output_encoding := encoding_of_rep_encoding enc

let set_output_encoding enc =
  output_encoding := enc

let set_default_output_encoding () =
  output_encoding := encoding_of_rep_encoding !internal_encoding

let get_internal_encoding () =
  !internal_encoding

let get_output_encoding () =
  !output_encoding

(**********************************************)
(* String / character conversion capabilities *)
(**********************************************)

(* Converting a string from one encoding to another *)

(* This is a FIX to PXP so that it does not encode '%' as a character
reference.  - Jerome *)

let pxp_fix_write_data_string ~(from_enc:rep_encoding) ~to_enc os content =
  (* Write the 'from_enc'-encoded string 's' as 'to_enc'-encoded string to
   * 'os'. The characters '&', '<', '>', '"', '%' and every character that
   * cannot be represented in 'to_enc' are paraphrased as entity reference
   * "&...;".
   *)
  let convert_ascii s =
    (* Convert the ASCII-encoded string 's'. Note that 'from_enc' is
     * always ASCII-compatible
     *)
    if to_enc = (from_enc :> encoding)
    then s
    else
      Netconversion.recode_string
        ~in_enc:(from_enc :> encoding)
        ~out_enc:to_enc
        ~subst:(fun n -> assert false)
	s
  in

  let write_ascii s =
    (* Write the ASCII-encoded string 's' *)
    let s' = convert_ascii s in
    Pxp_types.write os s' 0 (String.length s')
  in

  let write_part j l =
    (* Writes the substring of 'content' beginning at pos 'j' with length 'l'
     *)
    if to_enc = (from_enc :> encoding) then
      Pxp_types.write os content j l
    else begin
      let s' = Netconversion.recode_string
	         ~in_enc:(from_enc :> encoding)
	         ~out_enc:to_enc
	         ~subst:(fun n ->
			   convert_ascii ("&#" ^ string_of_int n ^ ";"))
		 (String.sub content j l)
      in
      Pxp_types.write os s' 0 (String.length s')
    end
  in
  let i = ref 0 in
  for k = 0 to String.length content - 1 do
    match content.[k] with
	('&' | '<' | '>' | '"' | '\'') as c ->
	  if !i < k then
	    write_part !i (k - !i);
	  begin match c with
	      '&' -> write_ascii "&amp;"
	    | '<' -> write_ascii "&lt;"
	    | '>' -> write_ascii "&gt;"
	    | '"' -> write_ascii "&quot;"
	    | '\'' -> write_ascii "&apos;"
	    | _   -> assert false
	  end;
	  i := k+1
      | _ -> ()
  done;
  if !i < String.length content then
    write_part !i (String.length content - !i)


(* END OF FIX *)


let write_data_string from_enc to_enc s =
  try
    let b = Buffer.create 80 in
    pxp_fix_write_data_string from_enc to_enc (`Out_buffer b) s;
    Buffer.contents b
  with
  | _ ->
      raise (Query (Undefined ("Cannot convert string \"" ^ s ^ "\" to output encoding")))


let write_markup_string from_enc to_enc s =
  try
    let b = Buffer.create 80 in
    Pxp_aux.write_markup_string from_enc to_enc (`Out_buffer b) s;
    Buffer.contents b
  with
  | _ ->
      raise (Query (Undefined ("Cannot convert markup name \"" ^ s ^ "\" to output encoding")))

let character enc k =
  try
    Pxp_aux.character enc global_warner k
  with
  | Pxp_types.WF_error msg ->
      raise (Query (Undefined msg))

