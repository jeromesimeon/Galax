(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: pragma_lexer.mll,v 1.3 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Pragma_lexer
   Description:
     This module contains the part of the XQuery lexer corresponding
     to the PRAGMA lexical state.
*)

{

open Lexing

open Finfo
open Error

open Parse_xquery   (* tokens definition *)
open Lexing_util     (* lexical state handling *)

}

(*****************************)
(* Lexer regular expressions *)
(*****************************)

(* Whitespace *)

let space = ['\032' '\009' '\013' '\010']
let whitespace = space*
let whitespaceplus = space+

(* XML character sets *)

(* Note:
     Some of these Char codes have been taken from the PXP ISO
     parser. I am not sure that they are doing the right thing and
     should be double checked for conformance.
   - Jerome
 *)

let base_char    = ['A'-'Z' 'a'-'z']
let ideographics = ['\192'-'\214' '\216'-'\246' '\248'-'\255']
let extender     = '\183'
let digit        = ['0'-'9']
let ascii_digit = ['0'-'9']
let ascii_hexdigit = ['0'-'9' 'A'-'F' 'a'-'f']

let char_ref_number = ascii_digit+
let char_ref_hex_number = ascii_hexdigit+

(* XML names *)

let letter   = base_char (* | ideographics *)
let nmstart  = letter | '_'
let nmchar   = letter | extender | digit | '.' | '-' | '_'
 
let ncname   = nmstart nmchar*

let qname    = ncname (':' ncname)?

(****************)
(* Lexing rules *)
(****************)

rule token lh = parse
(* Whitespace *)
  | space                  (* for all others, skip blanks *)
      { token lh lexbuf }
  | "{"
      { pop_state lh; push_default lh; LCURLY }
  | "(#" whitespace (qname as q)
      { reset_string lh; Default_lexer.pragma lh q lexbuf }
  | "(:"
      { set_depth lh 1; Default_lexer.xquery_comment lh lexbuf; token lh lexbuf }
  | eof
      { raise (Query (Lexing(lexing_locinfo lexbuf, "Pragma not terminated"))) }
  | _
      { raise
	  (Query
	     (Lexing(lexing_locinfo lexbuf,
		     ("Illegal character '" ^ (lexeme lexbuf) ^ "' in expression")))) }
