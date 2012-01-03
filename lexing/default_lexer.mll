(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: default_lexer.mll,v 1.48 2007/04/23 14:53:23 simeon Exp $ *)

(* Module: Default_lexer
   Description:
     This module contains the part of the XQuery lexer corresponding
     to the DEFAULT lexical state.
*)

{

open Lexing

open Finfo
open Error

open Parse_xquery   (* tokens definition *)
open Lexing_util     (* lexical state handling *)

let wrap_bang f =
  if Conf.is_xquerybang()
  then f ()
  else raise (Query (Toplevel_Error "XQuery! tokens found, please turn xquerybang option on"))

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

(********************)
(* Numeric literals *)
(********************)

(* Double *)
  | double_numeric
      { let v = float_of_string (lexeme lexbuf) in
        pop_state lh; push_operator lh; Some (DOUBLE v) }

(* Decimals *)
  | decimal_numeric
      { let v = Decimal._decimal_of_string (lexeme lexbuf) in
        pop_state lh; push_operator lh; Some (DECIMAL v) }

  | integer_numeric qname
      { raise (Query (Lexing(lexing_locinfo lexbuf, "Numeric literal followed by name"))) }

(* Integers *)
  | integer_numeric
      { let v = Decimal._integer_of_string (lexeme lexbuf) in
        pop_state lh; push_operator lh; Some (INT v) }

(* Arithmetic operators *)
  | "-"
      { Some MINUS }
  | "+"
      { Some PLUS }

(* Whitespace *)
  | space                  (* for all others, skip blanks *)
      { token lh lexbuf }

(* Comments *)
  | ":)"
      { raise (Query (Lexing(lexing_locinfo lexbuf, "Unmatched end of comment"))) }
  | "(:"
      { set_depth lh 1; xquery_comment lh lexbuf; token lh lexbuf }
  | "{--" 
      { raise (Query (Lexing(lexing_locinfo lexbuf, "ATTENTION: The syntax of comments in XQuery has now changed from {-- --} to (: :)"))) }

  | "*"
      { pop_state lh; push_operator lh; Some STAR }
  | "*" whitespace ":" whitespace (ncname as n)
      { pop_state lh; push_operator lh; Some (STARNCNAME n) }
  | (ncname as n) whitespace ":" "*"
      { pop_state lh; push_operator lh; Some (NCNAMESTAR n) }

(* Axis names *)
  | (ncname as n) whitespace "::"
      { Some (AXIS (get_axis n)) }

(* Declaration keywords *)
  | "declare" whitespaceplus "schema" whitespace "{"
      { push_schema_declaration lh; Some DECLARESCHEMALCURLY }

  | "declare" whitespaceplus "attribute" whitespaceplus (qname as q)
      { let sn = Namespace_names.uqname_element_of_string q in
        push_type_declaration lh;
        Some (DECLAREATTRIBUTE sn) }

  | "declare" whitespaceplus "element" whitespaceplus (qname as q)
      { let sn = Namespace_names.uqname_element_of_string q in
        push_type_declaration lh;
        Some (DECLAREELEMENT sn) }

  | "declare" whitespaceplus "simple" whitespaceplus "type" whitespaceplus (qname as q)
      { let sn = Namespace_names.uqname_element_of_string q in
        push_type_declaration lh;
        Some (DECLARESIMPLETYPE sn) }

  | "declare" whitespaceplus "complex" whitespaceplus "type" whitespaceplus (qname as q)
      { let sn = Namespace_names.uqname_element_of_string q in
        push_type_declaration lh;
        Some (DECLARECOMPLEXTYPE sn) }

  | "declare" whitespaceplus "group" whitespaceplus (qname as q)
      { let sn = Namespace_names.uqname_element_of_string q in
        push_type_declaration lh;
        Some (DECLAREGROUP sn) }

  | "declare" whitespaceplus "attrGroup" whitespaceplus (qname as q)
      { let sn = Namespace_names.uqname_element_of_string q in
        push_type_declaration lh;
        Some (DECLAREATTRGROUP sn) }

(* Slash and SlashSlash *)
  | "/"
      { Some SLASH }
  | "//"
      { Some SLASHSLASH }

(* XML tags *)
  | "<?" (ncname as n) whitespace (* Processing instruction *)
      { pop_state lh; push_operator lh; push_processing_instruction lh; Some (OPENINGPI n) }
  | "<!--"                  (* Opening comment *)
      { pop_state lh; push_operator lh; push_comment lh; Some LOPENINGCOMMENT }

  | "<"                     (* Opening XML tag *)
      { pop_state lh; push_operator lh; push_opening_tag lh; Some LOPENINGTAG }

(* Choice *)
  | "|"
      { Some BAR }

(* Parenthesis *)
  | "("
      { Some LPAR }
  | ")" 
      { pop_state lh; push_operator lh; Some RPAR }
  | "[" 
      { Some LBRACK }
  | "]" 
      { pop_state lh; push_operator lh; Some RBRACK }
  | "{"
      { pop_state lh; push_operator lh; push_default lh;
	Some LCURLY }
  | "}"
      { try
	  pop_state lh; Some RCURLY
        with
        | Stack.Empty ->
	    raise
	      (Query (Lexing(lexing_locinfo lexbuf, "Unmatched end of enclosed expression"))) }

(* Misc symbols *)
  | "@"
      { Some ATSIGN }
  | ","
      { Some COMMA }
  | ";"
      { Some SEMICOLON }
  | "."
      { pop_state lh; push_operator lh; Some DOT }
  | ".."
      { pop_state lh; push_operator lh; Some DOTDOT }

(* Variables *)
  | "$"
      { pop_state lh; push_operator lh; push_varname lh; Some DOLLAR }

(* Constructors *)

  | "attribute" whitespaceplus (qname as q) whitespace "{"
      { pop_state lh; push_operator lh; push_default lh; Some (ATTRIBUTEQNAMECURLY (Namespace_names.uqname_element_of_string q)) }

  | "namespace" whitespaceplus (ncname as n) whitespace "{"
      { pop_state lh; push_operator lh; push_default lh; Some (NAMESPACENCNAMECURLY n) }

  | "element" whitespaceplus (qname as q) whitespace "{"
      { pop_state lh; push_operator lh; push_default lh; Some (ELEMENTQNAMECURLY (Namespace_names.uqname_element_of_string q)) }

  | "processing-instruction" whitespaceplus (ncname as n) whitespace "{"
      { pop_state lh; push_operator lh; push_default lh; Some (PINCNAMECURLY n) }

  | "(#" whitespace (qname as q)
      { reset_string lh;
	pop_state lh; push_operator lh; push_pragma lh;
	Some (pragma lh q lexbuf) }

(* XML Names *)
  | qname
      { let var_s = (lexeme lexbuf) in
        pop_state lh; push_operator lh;
        match_default_keyword (lexing_locinfo lexbuf) lh var_s }

  | (qname as q) whitespace "(:"
      { begin
	  set_depth lh 1; xquery_comment lh lexbuf;
          pop_state lh; push_operator lh;
          match_default_keyword (lexing_locinfo lexbuf) lh q
        end }


(*******************)
(* String literals *)
(*******************)

  | "\""
      { reset_string lh; Some (string lh lexbuf) }
  | "'"
      { reset_string lh; Some (string_single lh lexbuf) }


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
      { raise
	  (Query
	     (Lexing(lexing_locinfo lexbuf,
		     ("Illegal character '" ^ (lexeme lexbuf) ^ "' in expression")))) }


(************)
(* Comments *)
(************)

and xquery_comment lh = parse
  | "(:"
      { set_depth lh (succ (get_depth lh)); xquery_comment lh lexbuf }
  | ":)"
      { set_depth lh (pred (get_depth lh)); if get_depth lh > 0 then xquery_comment lh lexbuf }
  | eof
      { raise (Query (Lexing(lexing_locinfo lexbuf, "Comment not terminated"))) }
  | [^ '\n']
      { xquery_comment lh lexbuf }
  | "\n"
      { xquery_comment lh lexbuf }


(*******************)
(* String literals *)
(*******************)

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
  | '"'    { let s = get_string lh in pop_state lh; push_operator lh; STRING s }  (* End of string *)
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
  | '\''   { let s = get_string lh in pop_state lh; push_operator lh; STRING s }  (* End of string *)
  | eof    { raise (Query (Lexing(lexing_locinfo lexbuf, "String not terminated"))) }
  | _      { add_char_to_string lh (lexeme_char lexbuf 0); string_single lh lexbuf }

and pragma lh q = parse
  | "#)"
      { let s = get_string lh in
        PRAGMA (Namespace_names.uqname_element_of_string q,s) }
  | eof
      { raise (Query (Lexing(lexing_locinfo lexbuf, "Pragma not terminated"))) }
  | _
      { add_char_to_string lh (lexeme_char lexbuf 0); pragma lh q lexbuf }
