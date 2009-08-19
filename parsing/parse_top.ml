(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parse_top.ml,v 1.24 2007/08/09 20:21:22 simeon Exp $ *)

(* Module: Parse_top
   Description:
     This module implements top-level parsing functions.
 *)

open Finfo
open Error
open Encoding

open Xquery_common_ast
open Xquery_ast

open Lexing_util
open Parse_util


(*******************************)
(* Top-level parsing functions *)
(*******************************)

(* Statement *)

let parse_statement_from_io parse_ctxt gio =
  let proc_ctxt = Parse_context.get_processing_context parse_ctxt in
  let lh = init_xquery_lexing () in
  let gin = Parse_io.galax_utf8_input_from_input_spec proc_ctxt gio in
  try
    begin
      let lexbuf = Parse_io.lexbuf_from_galax_input gin in
      try
	let ast = Parse_xquery.statement (xquery_lexfun lh) lexbuf in
	begin
	  Parse_io.close_galax_input gin;
	  ast
	end
      with
      | Parsing.Parse_error ->
	  let finfo = (make_finfo (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf)) in
	  let msg = Parse_io.parse_error_msg gio in
	  raise (Query(Parsing (finfo, msg)))
    end
  with e ->
    Parse_io.close_galax_input gin;
    raise e

(* Prolog *)

let parse_prolog_from_io parse_ctxt gio =
  let proc_ctxt = Parse_context.get_processing_context parse_ctxt in
  let lh = init_xquery_lexing () in
  let gin = Parse_io.galax_utf8_input_from_input_spec proc_ctxt gio in
  try
    let lexbuf = Parse_io.lexbuf_from_galax_input gin in
    try
      let ast = Parse_xquery.prolog (xquery_lexfun lh) lexbuf in
      begin
	Parse_io.close_galax_input gin;
	(parse_ctxt,ast)
      end
    with Parsing.Parse_error ->
      let finfo = (make_finfo (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf)) in
      let msg = Parse_io.parse_error_msg gio in
      raise (Query(Parsing (finfo, msg)))
  with e ->
    Parse_io.close_galax_input gin;
    raise e

(* Library module *)

let parse_library_module_from_io parse_ctxt gio =
  let proc_ctxt = Parse_context.get_processing_context parse_ctxt in
  let lh = init_xquery_lexing () in
  let gin = Parse_io.galax_utf8_input_from_input_spec proc_ctxt gio in
  try
    let lexbuf = Parse_io.lexbuf_from_galax_input gin in
    try
      let ast = Parse_xquery.librarymodule (xquery_lexfun lh) lexbuf in
      begin
	Parse_io.close_galax_input gin;
	(parse_ctxt,ast)
      end
    with Parsing.Parse_error ->
      let finfo = (make_finfo (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf)) in
      let msg = Parse_io.parse_error_msg gio in
      raise (Query(Parsing (finfo, msg)))
  with e ->
    Parse_io.close_galax_input gin;
    raise e

(* Main module *)

let parse_main_module_from_io parse_ctxt gio =
  let proc_ctxt = Parse_context.get_processing_context parse_ctxt in
  let lh = init_xquery_lexing () in
  let gin = Parse_io.galax_utf8_input_from_input_spec proc_ctxt gio in
  try
    let lexbuf = Parse_io.lexbuf_from_galax_input gin in
    try
      let ast = Parse_xquery.mainmodule (xquery_lexfun lh) lexbuf in
      begin
	Parse_io.close_galax_input gin;
	(parse_ctxt,ast)
      end
    with Parsing.Parse_error -> 
      let finfo =
	(make_finfo (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf))
      in
      let msg = Parse_io.parse_error_msg gio in
      raise (Query(Parsing (finfo, msg)))
  with e ->
    Parse_io.close_galax_input gin;
    raise e

(* Main module *)

let parse_interface_from_io parse_ctxt gio =
  let proc_ctxt = Parse_context.get_processing_context parse_ctxt in
  let lh = init_xquery_lexing () in
  let gin = Parse_io.galax_utf8_input_from_input_spec proc_ctxt gio in
  try
    let lexbuf = Parse_io.lexbuf_from_galax_input gin in
    try
      let ast = Parse_xquery.interface (xquery_lexfun lh) lexbuf in
      begin
	Parse_io.close_galax_input gin;
	(parse_ctxt,ast)
      end
    with Parsing.Parse_error -> 
      let finfo =
	(make_finfo (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf))
      in
      let msg = Parse_io.parse_error_msg gio in
      raise (Query(Parsing (finfo, msg)))
  with e ->
    Parse_io.close_galax_input gin;
    raise e

(* Content models *)

let parse_type_from_io parse_ctxt gio =
  let proc_ctxt = Parse_context.get_processing_context parse_ctxt in
  let lh = init_type_lexing () in
  let gin = Parse_io.galax_utf8_input_from_input_spec proc_ctxt gio in
  try
    let lexbuf = Parse_io.lexbuf_from_galax_input gin in
    try
      let ast = Parse_xquery.extype (xquery_lexfun lh) lexbuf in
      begin
	Parse_io.close_galax_input gin;
	ast
      end
    with Parsing.Parse_error -> 
      let finfo = (make_finfo (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf)) in
      let msg = Parse_io.parse_error_msg gio in
      raise (Query(Parsing (finfo, msg)))
  with e ->
    Parse_io.close_galax_input gin;
    raise e

(* Debug lexer *)

let tokens_from_gio gio =
  let proc_ctxt = Processing_context.default_processing_context() in
  let lh = init_xquery_lexing () in
  let gin = Parse_io.galax_utf8_input_from_input_spec proc_ctxt gio in
  let lexbuf = Parse_io.lexbuf_from_galax_input gin in
  let res = ref [] in
  try
    while (true) do
      let next = xquery_lexfun lh lexbuf in
      res := (next,get_whole_lex_stack lh,default_token lh) :: !res;
      if next = Parse_xquery.EOF then raise Not_found else ()
    done;
    List.rev !res
  with
  | _ ->
      List.rev !res

let tokens_from_file file =
  tokens_from_gio (Galax_io.File_Input file)
let tokens_from_string str =
  tokens_from_gio (Galax_io.String_Input str)

(* let _ = print_string("Parse_top\n") *)

let parse_main_module_from_string s =
  let proc_ctxt = Processing_context.default_processing_context() in
  let pac = Parse_context.build_xquery_parse_context proc_ctxt in
  (parse_main_module_from_io pac (Galax_io.String_Input s))
let parse_library_module_from_string s =
  let proc_ctxt = Processing_context.default_processing_context() in
  let pac = Parse_context.build_xquery_parse_context proc_ctxt in
  (parse_library_module_from_io pac (Galax_io.String_Input s))
let parse_interface_from_string s =
  let proc_ctxt = Processing_context.default_processing_context() in
  let pac = Parse_context.build_xquery_parse_context proc_ctxt in
  (parse_interface_from_io pac (Galax_io.String_Input s))
let parse_statement_from_string s =
  let proc_ctxt = Processing_context.default_processing_context() in
  let pac = Parse_context.build_xquery_parse_context proc_ctxt in
  (parse_statement_from_io pac (Galax_io.String_Input s))

let ast_from_string s = snd(parse_main_module_from_string s)

let ast_from_file s =
  let proc_ctxt = Processing_context.default_processing_context() in
  let pac = Parse_context.build_xquery_parse_context proc_ctxt in
  snd (parse_main_module_from_io pac (Galax_io.File_Input s))

