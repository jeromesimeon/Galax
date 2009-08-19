(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xmldecl_lexer.mll,v 1.5 2007/02/01 22:08:51 simeon Exp $ *)

(* Module Xmldecl_lexer
   Description:
     This module contains the lexer for XML declarations.
*)

{

open Lexing

open Finfo
open Error

open Parse_xquery   (* XML parser tokens *)
open Lexing_util         (* Lexical states and text handling *)

}

(*****************************)
(* Lexer regular expressions *)
(*****************************)

(* XML Version *)

let ws = [ ' ' '\t' '\r' '\n' ]

let whitespace = ws*
let whitespaceplus = ws+

let xdeclname = (['A'-'Z' 'a'-'z' '0'-'9' '.' '_' ':' '-'])+


(****************)
(* Lexing rules *)
(****************)

rule xmldecl lh = parse
  | whitespaceplus   (* Recognizes whitespace so the parser can do something about it *)
      { S }
  | "version" whitespace "=" whitespace
      { XVERSION }
  | "standalone" whitespace "=" whitespace
      { XSDDECL }
  | "'"
      { XSINGLEQUOTE }
  | "\""
      { XDOUBLEQUOTE }
  | xdeclname
      { XDECLNAME (lexeme lexbuf) } 
  | "encoding" whitespace "=" whitespace '"' xdeclname '"'
      { let xdec_text = lexeme lexbuf in
        XENCODING (get_xml_encoding_quotes xdec_text) }
  | "encoding" whitespace "=" whitespace "'" xdeclname "'"
      { let xdec_text = lexeme lexbuf in
        XENCODING (get_xml_encoding_single_quotes xdec_text) }
  | "?>"        (* Closing opening XMLDecl *)
      { pop_state lh; CLOSINGPI }
  | _
      { raise (Query (Lexing(lexing_locinfo lexbuf, ("Illegal character '" ^ (lexeme lexbuf) ^ "' in XML declaration")))) }

