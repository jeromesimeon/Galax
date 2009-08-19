(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: qname_lexer.ml,v 1.4 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Qname_lexer
   Description:
     This module implements a parser for XML QNames.
 *)

open Error

type qname_lexer = Lexing.lexbuf -> string option * string

let qname_lexers_by_encoding =
  Hashtbl.create 5;;

let dummy_lexer x =
  raise (Query (Undefined ("No lexical support linked! You must have at least one character encoding selected before compilation, and do not forget to link Galax using the -linkall option.")))


let current_qname_xml_lexer = ref dummy_lexer

let register_qname_lexer encoding lx =
  begin
    match encoding with
    | `Enc_utf8 ->
	current_qname_xml_lexer := lx
    | `Enc_iso88591 ->
	if Hashtbl.mem qname_lexers_by_encoding `Enc_utf8
	then ()
	else
	  current_qname_xml_lexer := lx
    | _ ->
	raise (Query (Prototype "Encoding not supported yet"))
  end;
  Hashtbl.add qname_lexers_by_encoding encoding lx

let parse_qname x = !current_qname_xml_lexer x

