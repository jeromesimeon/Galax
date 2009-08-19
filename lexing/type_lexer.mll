(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: type_lexer.mll,v 1.12 2007/02/01 22:08:51 simeon Exp $ *)

(* Module Type_lexer
   Description:
     This module contains the lexer for type declarations.
*)

{

open Lexing

open Xquery_ast
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

let qname    = ncname? (':' ncname)?

(****************)
(* Lexing rules *)
(****************)

rule type_declaration lh = parse
  (* Whitespace *)
  | whitespace                  (* for all others, skip blanks *)
      { type_declaration lh lexbuf }

  (* Comments *)
  | "(:" 
      { set_depth lh 1; Default_lexer.xquery_comment lh lexbuf; type_declaration lh lexbuf }
  | ";" { pop_state lh; SEMICOLON }

  (* Keywords *)
  | "document"
      { DOCUMENT }
  | "element"
      { ELEMENT }
  | "attribute"
      { ATTRIBUTE }
  | "type"
      { TYPE }
  | "none"
      { NONE }
  | "text"
      { TEXT }
  | "processing-instruction"
      { PROCESSINGINSTRUCTION }
  | "comment"
      { COMMENT }
  | "group"
      { GROUP }
  | "attrGroup"
      { ATTRGROUP }

  (* Modifiers *)
  | "nillable"
      { NILLABLE }
  | "mixed"
      { MIXED }
  | "substitutes" whitespaceplus "for"
      { SUBSTITUTESFOR }
  | "of" whitespaceplus "type"
      { OFTYPE }
  | "of" whitespaceplus "simple" whitespaceplus "type"
      { OFSIMPLETYPE }
  | "restricts"
      { RESTRICTS }
  | "extends"
      { EXTENDS }
  | "list of"
      { LISTOF }
  | "union of"
      { UNIONOF }

  (* Names *)
  | ":" ncname
      { QNAME (Namespace_names.NSDefaultElementPrefix, get_escaped_name (lexeme lexbuf)) }
  | qname
      { let var_s = (lexeme lexbuf) in
        QNAME (Namespace_names.uqname_element_of_string var_s) }

  (* Symbols *)
  | "{"
      { push_xtype lh ; LCURLY }
  | "|"
      { BAR }
  | "*"
      { STAR }
  | "+"
      { PLUS }
  | ","
      { COMMA }
  | "&"
      { AMPERSAND }
  | "?"
      { QUESTION }
  | "("
      { LPAR }
  | ")" 
      { RPAR }

  (* Unknown caracter *)
  | _
      { raise (Query (Lexing(lexing_locinfo lexbuf, "Illegal character in type declaration"))) }

and xtype lh = parse
  (* Whitespace *)
  | whitespace                  (* for all others, skip blanks *)
      { xtype lh lexbuf }

  (* Comments *)
  | "(:" 
      { set_depth lh 1; Default_lexer.xquery_comment lh lexbuf; xtype lh lexbuf }
  | ";" { SEMICOLON }

  (* Keywords *)
  | "document"
      { DOCUMENT }
  | "element"
      { ELEMENT }
  | "attribute"
      { ATTRIBUTE }
  | "type"
      { TYPE }
  | "none"
      { NONE }
  | "text"
      { TEXT }
  | "processing-instruction"
      { PROCESSINGINSTRUCTION }
  | "comment"
      { COMMENT }
  | "group"
      { GROUP }
  | "attrGroup"
      { ATTRGROUP }

  (* Modifiers *)
  | "nillable"
      { NILLABLE }
  | "mixed"
      { MIXED }
  | "substitutes" whitespaceplus "for"
      { SUBSTITUTESFOR }
  | "of" whitespaceplus "type"
      { OFTYPE }
  | "of" whitespaceplus "simple" whitespaceplus "type"
      { OFSIMPLETYPE }
  | "restricts"
      { RESTRICTS }
  | "extends"
      { EXTENDS }
  | "list of"
      { LISTOF }
  | "union of"
      { UNIONOF }

  (* Names *)
  | ":" ncname
      { QNAME (Namespace_names.NSDefaultElementPrefix, get_escaped_name (lexeme lexbuf)) }
  | qname
      { let var_s = (lexeme lexbuf) in
        QNAME (Namespace_names.uqname_element_of_string var_s) }

  (* Symbols *)
  | "{"
      { push_xtype lh ; LCURLY }
  | "}"
      { pop_state lh ; RCURLY }
  | "|"
      { BAR }
  | "*"
      { STAR }
  | "+"
      { PLUS }
  | ","
      { COMMA }
  | "&"
      { AMPERSAND }
  | "?"
      { QUESTION }
  | "("
      { LPAR }
  | ")" 
      { RPAR }
  | eof
      { EOF }

  (* Unknown caracter *)
  | _
      { raise (Query (Lexing(lexing_locinfo lexbuf, "Illegal character in type declaration"))) }

