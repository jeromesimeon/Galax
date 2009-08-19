(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: itemtype_lexer.mll,v 1.10 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: XQuery_lexer
   Description:
     This module contains the part of the XQuery lexer corresponding
     to the ITEMTYPE lexical state.
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

(* Variables *)
  | "$" whitespace (qname as q)
      { VARNAME (Namespace_names.uqname_element_of_string q) }
  | "as"
      { AS }

(* Reserved words *)
  | "element" whitespace "("
      { pop_state lh; push_kindtest lh; ELEMENTLPAR }
  | "attribute" whitespace "("
      { pop_state lh; push_kindtest lh; ATTRIBUTELPAR }
  | "schema-element" whitespace "("
      { pop_state lh; push_kindtest lh; SCHEMAELEMENTLPAR }
  | "schema-attribute" whitespace "("
      { pop_state lh; push_kindtest lh; SCHEMAATTRIBUTELPAR }
  | "document-node" whitespace "("
      { pop_state lh; push_kindtest lh; DOCUMENTNODELPAR }
  | "type" whitespace "("
      { pop_state lh; push_kindtest lh; TYPELPAR }
  | "node" whitespace "("
      { pop_state lh; push_kindtest lh; NODELPAR }
  | "item" whitespace "("
      { pop_state lh; push_kindtest lh; ITEMLPAR }
  | "numeric" whitespace "("
      { pop_state lh; push_kindtest lh; NUMERICLPAR }
  | "anystring" whitespace "("
      { pop_state lh; push_kindtest lh; ANYSTRINGLPAR }
  | "text" whitespace "("
      { pop_state lh; push_kindtest lh; TEXTLPAR }
  | "comment" whitespace "("
      { pop_state lh; push_kindtest lh; COMMENTLPAR }
  | "processing-instruction" whitespace "("
      { pop_state lh; push_kindtest lh; PROCESSINGINSTRUCTIONLPAR }
  | "empty-sequence" whitespace "("
      { pop_state lh; push_kindtest lh; EMPTYSEQUENCELPAR }

(* XML Names *)
  | qname
      { let q = (lexeme lexbuf) in
        pop_state lh; (* This is an atomic type here - Jerome *)
        QNAME (Namespace_names.uqname_element_of_string q) }

  | (qname as q) whitespace "?"
      { pop_state lh; (* This is an atomic type here - Jerome *)
        QNAMEQUESTION (Namespace_names.uqname_element_of_string q) }

  | (qname as q) whitespace "*"
      { pop_state lh; (* This is an atomic type here - Jerome *)
        QNAMESTAR (Namespace_names.uqname_element_of_string q) }

  | (qname as q) whitespace "+"
      { pop_state lh; (* This is an atomic type here - Jerome *)
        QNAMEPLUS (Namespace_names.uqname_element_of_string q) }

(********************)
(* Unknown caracter *)
(********************)

(* Note:
     If the lexer arrives here, this is an error.
   - Jerome
*)

  | _
      { raise (Query (Lexing(lexing_locinfo lexbuf, ("Illegal character '" ^ (lexeme lexbuf) ^ "' in expression")))) }

