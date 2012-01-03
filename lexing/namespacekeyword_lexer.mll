(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespacekeyword_lexer.mll,v 1.7 2007/07/31 17:34:08 simeon Exp $ *)

(* Module: Namespacekeyword_lexer
   Description:
     This module contains the part of the XQuery lexer corresponding
     to the NAMESPACEKEYWORD lexical state.
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

(* XML names *)

let letter   = base_char (* | ideographics *)
let nmstart  = letter | '_'
let nmchar   = letter | extender | digit | '.' | '-' | '_'
 
let ncname   = nmstart nmchar*

let qname    = ncname (':' ncname)?

let ascii_digit = ['0'-'9']
let ascii_hexdigit = ['0'-'9' 'A'-'F' 'a'-'f']
let char_ref_number = ascii_digit+
let char_ref_hex_number = ascii_hexdigit+

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

(**************)
(* Whitespace *)
(**************)

  | space                  (* for all others, skip blanks *)
      { token lh lexbuf }

(************)
(* Comments *)
(************)

  | ":)"
      { raise (Query (Lexing(lexing_locinfo lexbuf, "Unmatched end of comment"))) }
  | "(:" 
      { set_depth lh 1; Default_lexer.xquery_comment lh lexbuf; token lh lexbuf }
  | "{--" 
      { raise (Query (Lexing(lexing_locinfo lexbuf, "ATTENTION: The syntax of comments in XQuery has now changed from {-- --} to (: :)"))) }

(***********)
(* Symbols *)
(***********)

  | ","
      { Some COMMA }
  | ";"
      { pop_state lh; push_default lh; Some SEMICOLON }
  | ")" 
      { Some RPAR }
  | "*"
      { Some MULT }
  | "?"
      { Some QUESTION }
  | "+"
      { Some PLUS }
  | "{"
      { push_default lh;
	Some LCURLY }

(*******************)
(* String literals *)
(*******************)

  | "\""
      { reset_string lh; Some (string lh lexbuf) }
  | "'"
      { reset_string lh; Some (string_single lh lexbuf) }

(* Variables *)
  | "$"
      { push_varname lh; Some DOLLAR }
  | (qname as q) whitespace "("
      { let actual_qname = Namespace_names.uqname_function_of_string q in
        Some (FUNCTIONNAMELPAR actual_qname) }

  (* XML Names *)
  | qname
      { let var_s = (lexeme lexbuf) in
        match process_qname_string var_s with
	| NCNAME_KIND s ->
	    match_namespace_keyword (lexing_locinfo lexbuf) lh s
	| QNAME_KIND qname ->
            qname_lexing_error (lexing_locinfo lexbuf) }

  | (qname as q) whitespace "(:"
      { begin
	  set_depth lh 1; Default_lexer.xquery_comment lh lexbuf;
          match process_qname_string q with
	  | NCNAME_KIND s ->
	      match_namespace_keyword (lexing_locinfo lexbuf) lh s
	  | QNAME_KIND qname ->
              qname_lexing_error (lexing_locinfo lexbuf)
        end }

(***************)
(* End of file *)
(***************)

  | eof
      { Some EOF }


(********************)
(* Unknown caracter *)
(********************)

(* Note:
     If the lexer arrives here, this is an error.
   - Jerome
*)

  | _
      { raise (Query (Lexing(lexing_locinfo lexbuf, ("Illegal character '" ^ (lexeme lexbuf) ^ "' in expression")))) }

and string lh = parse
  | "\"\"" { add_char_to_string lh '"'; string lh lexbuf }                         (* Escaped quote *)
  | "&#" (char_ref_number as charref) ";"
      { let charref_int = int_of_string charref in
        let s = Encoding.character (Encoding.get_internal_encoding ()) charref_int in
        add_string_to_string lh s; string lh lexbuf }
  | "&#x" (char_ref_hex_number as hexcharref) ";"
      { let hexcharref_int = int_of_string ("0x" ^ hexcharref) in
	let s = Encoding.character (Encoding.get_internal_encoding ()) hexcharref_int in
        add_string_to_string lh s; string lh lexbuf }
  | "&lt;"
      { add_char_to_string lh '<'; string lh lexbuf }
  | "&gt;"
      { add_char_to_string lh '>'; string lh lexbuf }
  | "&amp;"
      { add_char_to_string lh '&'; string lh lexbuf }
  | "&quot;"
      { add_char_to_string lh '"'; string lh lexbuf }
  | "&apos;"
      { add_char_to_string lh '\''; string lh lexbuf }
  | "&"
      { raise (Query (Lexing(lexing_locinfo lexbuf, "Malformed entity reference"))) }
  | "\013\n" { add_char_to_string lh '\n'; string lh lexbuf }
  | "\013" { add_char_to_string lh '\n'; string lh lexbuf }
  | '"'    { let s = get_string lh in STRING s }  (* End of string *)
  | eof    { raise (Query (Lexing(lexing_locinfo lexbuf, "String not terminated"))) }
  | _      { add_char_to_string lh (lexeme_char lexbuf 0); string lh lexbuf }

and string_single lh = parse
  | "''"   { add_char_to_string lh '\''; string_single lh lexbuf }                 (* Escaped ' *)
  | "&#" (char_ref_number as charref) ";"
      { let charref_int = int_of_string charref in
        let s = Encoding.character (Encoding.get_internal_encoding ()) charref_int in
        add_string_to_string lh s; string_single lh lexbuf }
  | "&#x" (char_ref_hex_number as hexcharref) ";"
      { let hexcharref_int = int_of_string ("0x" ^ hexcharref) in
	let s = Encoding.character (Encoding.get_internal_encoding ()) hexcharref_int in
        add_string_to_string lh s; string_single lh lexbuf }
  | "&lt;"
      { add_char_to_string lh '<'; string_single lh lexbuf }
  | "&gt;"
      { add_char_to_string lh '>'; string_single lh lexbuf }
  | "&amp;"
      { add_char_to_string lh '&'; string_single lh lexbuf }
  | "&quot;"
      { add_char_to_string lh '"'; string_single lh lexbuf }
  | "&apos;"
      { add_char_to_string lh '\''; string_single lh lexbuf }
  | "&"
      { raise (Query (Lexing(lexing_locinfo lexbuf, "Malformed entity reference"))) }
  | "\013\n" { add_char_to_string lh '\n'; string_single lh lexbuf }
  | "\013" { add_char_to_string lh '\n'; string_single lh lexbuf }
  | '\''   { let s = get_string lh in STRING s }  (* End of string *)
  | eof    { raise (Query (Lexing(lexing_locinfo lexbuf, "String not terminated"))) }
  | _      { add_char_to_string lh (lexeme_char lexbuf 0); string_single lh lexbuf }

