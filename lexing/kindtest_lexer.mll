(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: kindtest_lexer.mll,v 1.8 2007/07/31 17:34:08 simeon Exp $ *)

(* Module: Kindtest_lexer
   Description:
     This module contains the part of the XQuery lexer corresponding
     to the KINDTEST lexical state.
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

(* Comments *)
  | ":)"
      { raise (Query (Lexing(lexing_locinfo lexbuf, "Unmatched end of comment"))) }
  | "(:" 
      { set_depth lh 1; Default_lexer.xquery_comment lh lexbuf; token lh lexbuf }
  | "{--" 
      { raise (Query (Lexing(lexing_locinfo lexbuf, "ATTENTION: The syntax of comments in XQuery has now changed from {-- --} to (: :)"))) }

(* Parenthesis *)
  | ")"
      { pop_state lh; set_item_type lh; RPAR }
(*
  | ")" whitespace "+"
      { pop_state lh; RPARPLUS }
  | ")" whitespace "*"
      { pop_state lh; RPARSTAR }
  | ")" whitespace "?"
      { pop_state lh; RPARQUESTION }
*)

(* Symbols *)

  | "*"
      { STAR }
  | ","
      { COMMA }

(* XML Names *)
  | "element" whitespace "("
      { push_kindtest lh; ELEMENTLPAR }
  | "schema-element" whitespace "("
      { push_kindtest lh; SCHEMAELEMENTLPAR }
  | qname
      { let var_s = (lexeme lexbuf) in
        match process_qname_string var_s with
	| NCNAME_KIND s ->
	    NCNAME s
	| QNAME_KIND qname ->
	    QNAME qname }
  | "\""
      { reset_string lh; Default_lexer.string lh lexbuf }
  | "'"
      { reset_string lh; Default_lexer.string_single lh lexbuf }


(********************)
(* Unknown caracter *)
(********************)

(* Note:
     If the lexer arrives here, this is an error.
   - Jerome
*)

  | _
      { raise (Query (Lexing(lexing_locinfo lexbuf, ("Illegal character '" ^ (lexeme lexbuf) ^ "' in expression")))) }

