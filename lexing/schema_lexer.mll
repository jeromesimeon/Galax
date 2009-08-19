(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_lexer.mll,v 1.4 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Schema_lexer
   Description:
     This module contains the part of the XQuery lexer corresponding
     to the SCHEMADECL lexical state.
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

(* Declaration keywords *)

  | "declare" whitespaceplus "namespace"
      { pop_state lh; push_namespacedecl lh; DECLARENAMESPACE }

  | "declare" whitespace "default" whitespaceplus "element"
      { pop_state lh; push_namespacekeyword lh; DECLAREDEFAULTELEMENT }

  | "declare" whitespaceplus "schema" whitespace "{"
      { push_schema_declaration lh; DECLARESCHEMALCURLY }

  | "declare" whitespaceplus "attribute" whitespaceplus (qname as q)
      { let sn = Namespace_names.uqname_element_of_string q in
        push_type_declaration lh;
        DECLAREATTRIBUTE (sn) }

  | "declare" whitespaceplus "element" whitespaceplus (qname as q)
      { let sn = Namespace_names.uqname_element_of_string q in
        push_type_declaration lh;
        DECLAREELEMENT (sn) }

  | "declare" whitespaceplus "simple" whitespaceplus "type" whitespaceplus (qname as q)
      { let sn = Namespace_names.uqname_element_of_string q in
        push_type_declaration lh;
        DECLARESIMPLETYPE (sn) }

  | "declare" whitespaceplus "complex" whitespaceplus "type" whitespaceplus (qname as q)
      { let sn = Namespace_names.uqname_element_of_string q in
        push_type_declaration lh;
        DECLARECOMPLEXTYPE (sn) }

  | "declare" whitespaceplus "group" whitespaceplus (qname as q)
      { let sn = Namespace_names.uqname_element_of_string q in
        push_type_declaration lh;
        DECLAREGROUP (sn) }

  | "declare" whitespaceplus "attrGroup" whitespaceplus (qname as q)
      { let sn = Namespace_names.uqname_element_of_string q in
        push_type_declaration lh;
        DECLAREATTRGROUP (sn) }

  | "}"
      { try
	  pop_state lh; RCURLY
        with
          Stack.Empty ->
	    raise (Query (Lexing(lexing_locinfo lexbuf, "Unmatched end of enclosed expression"))) }

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

