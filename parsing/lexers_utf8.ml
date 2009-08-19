(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lexers_utf8.ml,v 1.10 2007/02/01 22:08:51 simeon Exp $ *)

open Datatypes_lexer
open Lexing_util
open Parse_util

(* UTF8 lexers *)

let my_utf8_lexers =
  { encoding = `Enc_utf8;
    opening_tag_state  	 = Opening_tag_lexer_utf8.opening_tag;
    closing_tag_state  	 = Closing_tag_lexer_utf8.closing_tag;
    text_state         	 = Text_lexer_utf8.text;
    attribute_text_state = Opening_tag_lexer_utf8.attribute_text;
    pi_state             = Text_lexer_utf8.pi;
    cdata_state          = Text_lexer_utf8.cdata;
    comment_state        = Text_lexer_utf8.xml_comment }

let my_utf8_qname_lexers = Qname_lexer_utf8.parse_qname

let _ =
  Qname_lexer.register_qname_lexer `Enc_utf8 my_utf8_qname_lexers;
  Parse_util.register_lexer my_utf8_lexers


