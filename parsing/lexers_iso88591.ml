(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lexers_iso88591.ml,v 1.11 2007/02/01 22:08:51 simeon Exp $ *)

open Datatypes_lexer
open Lexing_util
open Parse_util

(* ISO88591 lexers *)

let my_iso88591_lexers =
  { encoding = `Enc_iso88591;
    opening_tag_state  	 = Opening_tag_lexer_iso88591.opening_tag;
    closing_tag_state  	 = Closing_tag_lexer_iso88591.closing_tag;
    text_state         	 = Text_lexer_iso88591.text;
    attribute_text_state = Opening_tag_lexer_iso88591.attribute_text;
    pi_state             = Text_lexer_iso88591.pi;
    cdata_state          = Text_lexer_iso88591.cdata;
    comment_state        = Text_lexer_iso88591.xml_comment }

let my_iso88591_qname_lexers = Qname_lexer_iso88591.parse_qname

let _ =
  Qname_lexer.register_qname_lexer `Enc_iso88591 my_iso88591_qname_lexers;
  Parse_util.register_lexer my_iso88591_lexers

