(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: copynamespaces_lexer.mll,v 1.3 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Copynamespaces_lexer
   Description:
     This module contains the part of the XQuery lexer corresponding
     to the COPYANAMESPACES lexical state.
*)

{

open Lexing

open Finfo
open Error
open Parse_xquery    (* tokens definition *)
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


(************************)
(* Declaration keywords *)
(************************)

  | ","
      { COMMA }

  | "preserve"
      { PRESERVE }

  | "no-preserve"
      { NOPRESERVE }

  | "inherit"
      { INHERIT }

  | "no-inherit"
      { NOINHERIT }

  | ";"
      { pop_state lh; push_default lh; SEMICOLON } 

(***************)
(* End of file *)
(***************)

  | eof
      { EOF }


(********************)
(* Unknown caracter *)
(********************)

(* Note:
     If the lexer arrives here, this is an error.
   - Jerome
*)

  | _
      { raise (Query (Lexing(lexing_locinfo lexbuf, ("Illegal character '" ^ (lexeme lexbuf) ^ "' in expression")))) }


