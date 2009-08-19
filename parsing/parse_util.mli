(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parse_util.mli,v 1.11 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Parse_util
   Description:
     This module implements some internal utilities functions used
     during parsing.
 *)

open Lexing_util
open Parse_xquery

(*******************)
(* Lexers handling *)
(*******************)

type xml_lexers =
    { encoding             : Encoding.encoding;
      opening_tag_state    : lexing_handler -> Lexing.lexbuf -> token;
      closing_tag_state    : lexing_handler -> Lexing.lexbuf -> token;
      text_state           : lexing_handler -> Lexing.lexbuf -> token;
      attribute_text_state : lexing_handler -> Lexing.lexbuf -> token;
      pi_state             : lexing_handler -> Lexing.lexbuf -> token;
      cdata_state          : lexing_handler -> Lexing.lexbuf -> token;
      comment_state        : lexing_handler -> Lexing.lexbuf -> token }


type language_lexers =
    { language             	: language_kind;
      schema_declaration_state  : lexing_handler -> Lexing.lexbuf -> token;
      type_declaration_state   	: lexing_handler -> Lexing.lexbuf -> token;
      xtype_state         	: lexing_handler -> Lexing.lexbuf -> token;
      default_state        	: lexing_handler -> Lexing.lexbuf -> token option;
      operator_state         	: lexing_handler -> Lexing.lexbuf -> token option;
      namespacedecl_state    	: lexing_handler -> Lexing.lexbuf -> token;
      namespacekeyword_state 	: lexing_handler -> Lexing.lexbuf -> token option;
      copynamespaces_state 	: lexing_handler -> Lexing.lexbuf -> token;
      xmlspacedecl_state     	: lexing_handler -> Lexing.lexbuf -> token;
      itemtype_state         	: lexing_handler -> Lexing.lexbuf -> token;
      kindtest_state         	: lexing_handler -> Lexing.lexbuf -> token;
      kindtestforpi_state    	: lexing_handler -> Lexing.lexbuf -> token;
      schemacontextstep_state   : lexing_handler -> Lexing.lexbuf -> token;
      varname_state             : lexing_handler -> Lexing.lexbuf -> token;
      vardecl_state             : lexing_handler -> Lexing.lexbuf -> token;
      pragma_state              : lexing_handler -> Lexing.lexbuf -> token }

val register_lexer         :  xml_lexers -> unit
val set_lexer              :  Encoding.rep_encoding -> unit


(*******************)
(* Lexing wrappers *)
(*******************)

(* XQuery lexer *)

val xquery_lexfun : Lexing_util.lexing_handler -> Lexing.lexbuf -> Parse_xquery.token

