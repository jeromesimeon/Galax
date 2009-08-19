(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: operator_lexer.mll,v 1.25 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Operator_lexer
   Description:
     This module contains the part of the XQuery lexer corresponding
     to the OPERATOR lexical state.
*)

{

open Lexing

open Finfo
open Error
open Parse_xquery   (* tokens definition *)
open Lexing_util    (* lexical state handling *)

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

(* Numerics *)

let sign            = '-' | '+'
let exponent        = 'e' | 'E'

let integer_numeric = digit+
let decimal_numeric = (('.' digit+) | (digit+ '.' (digit+)?))
let double_numeric  = (('.' digit+) | (digit+ ('.' (digit+)?)?)) exponent sign? digit+

(****************)
(* Lexing rules *)
(****************)

rule token lh = parse

(* Arithmetic operators *)
  | "-"
      { make_new_token (lexing_locinfo lexbuf) lh todef MINUS }
  | "+"
      { make_new_token (lexing_locinfo lexbuf) lh tokinddef PLUS }
  | "*"
      { make_new_token (lexing_locinfo lexbuf) lh tokinddef MULT }
  | "?"
      { make_new_token (lexing_locinfo lexbuf) lh tokinddef QUESTION }

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

(* Reserved words *)
  | "<<"
      { make_new_token (lexing_locinfo lexbuf) lh todef PRECEDES }
  | ">>"
      { make_new_token (lexing_locinfo lexbuf) lh todef FOLLOWS }

(* Updates *)
(* Not handled by look-ahead yet as they collide with the simple "as" keyword - Jerome *)
  | "as" whitespace "first"
      { make_new_token (lexing_locinfo lexbuf) lh tonone ASFIRST }
  | "as" whitespace "last"
      { make_new_token (lexing_locinfo lexbuf) lh tonone ASLAST }

(* Type thingies *)
  | ":"
      { make_new_token (lexing_locinfo lexbuf) lh tonone COLON }

(* Slash and SlashSlash *)
  | "/"
      { make_new_token (lexing_locinfo lexbuf) lh todef SLASH }
  | "//"
      { make_new_token (lexing_locinfo lexbuf) lh todef SLASHSLASH }

(* Comparators *)
  | "="
      { make_new_token (lexing_locinfo lexbuf) lh todef EQUALS }
  | "!="
      { make_new_token (lexing_locinfo lexbuf) lh todef NOTEQUALS }
  | "<"
      { make_new_token (lexing_locinfo lexbuf) lh todef LT }
  | ">"
      { make_new_token (lexing_locinfo lexbuf) lh todef GT }
  | "<="
      { make_new_token (lexing_locinfo lexbuf) lh todef LTEQUALS }
  | ">="
      { make_new_token (lexing_locinfo lexbuf) lh todef GTEQUALS }

(* Assignment *)
  | ":="
      { make_new_token (lexing_locinfo lexbuf) lh todef COLONEQUALS }

(* Choice *)
  | "|"
      { make_new_token (lexing_locinfo lexbuf) lh todef BAR }

(* Parenthesis *)
  | "("
      { match_paren (lexing_locinfo lexbuf) lh }

  | ")"
      { make_new_token (lexing_locinfo lexbuf) lh tonone RPAR }
  | "[" 
      { make_new_token (lexing_locinfo lexbuf) lh todef LBRACK }
  | "]" 
      { make_new_token (lexing_locinfo lexbuf) lh tonone RBRACK }
  | "{"
      { match_curly (lexing_locinfo lexbuf) lh }
  | "}"
      { try
	  pop_state lh; make_new_token (lexing_locinfo lexbuf) lh tonone RCURLY
        with
          Stack.Empty ->
	    raise (Query (Lexing(lexing_locinfo lexbuf, "Unmatched end of enclosed expression"))) }

(* Misc symbols *)
  | ","
      { make_new_token (lexing_locinfo lexbuf) lh todef COMMA }
  | ";"
      { make_new_token (lexing_locinfo lexbuf) lh todef SEMICOLON }
  | "."
      { make_new_token (lexing_locinfo lexbuf) lh tonone DOT }
  | ".."
      { make_new_token (lexing_locinfo lexbuf) lh tonone DOTDOT }

(* XML Names *)
  | qname integer_numeric
      { qnamesep_lexing_error (lexing_locinfo lexbuf) }
  | qname
      { let var_s = (lexeme lexbuf) in
        match process_qname_string var_s with
	| NCNAME_KIND s ->
	    match_operator_keyword (lexing_locinfo lexbuf) lh s
	| QNAME_KIND qname ->
	    qname_lexing_error (lexing_locinfo lexbuf) }

  | "$"
      { match_operator_keyword (lexing_locinfo lexbuf) lh "$" }

(***************)
(* End of file *)
(***************)

  | eof
      { make_new_token (lexing_locinfo lexbuf) lh tonone EOF }

(********************)
(* Unknown caracter *)
(********************)

(* Note:
     If the lexer arrives here, this is an error.
   - Jerome
*)

  | _
      { raise (Query (Lexing(lexing_locinfo lexbuf, ("Illegal character '" ^ (lexeme lexbuf) ^ "' in expression")))) }

