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
      res := (next,get_whole_lex_stack lh,get_item_type lh) :: !res;
      if next = Parse_xquery.EOF then raise Not_found else ()
    done;
    List.rev !res
  with
  | Not_found ->
      List.rev !res
  | _ ->
      List.rev ((Parse_xquery.LEXERROR,get_whole_lex_stack lh,get_item_type lh) :: !res)

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

open Parse_xquery

let string_of_tok tok =
  match tok with
  | XQNAME uqname -> "XQNAME<" ^ (Namespace_names.string_of_uqname uqname) ^ ">"
  | XNCNAME ncname -> "XNCNAME<" ^ ncname ^ ">"
  | XSTRING str -> "XSTRING<" ^ str ^ ">"
  | XMLPI -> "XMLPI"
  | XVERSION -> "XVERSION"
  | XSDDECL -> "XSDDECL"
  | XSINGLEQUOTE -> "XSINGLEQUOTE"
  | XDOUBLEQUOTE -> "XDOUBLEQUOTE"
  | XDECLNAME str -> "XDECLNAME<" ^ str ^ ">"
  | XENCODING str -> "XENCODING<" ^ str ^ ">"
  | DOCTYPE -> "DOCTYPE"
  | DOCTYPECLOSE -> "DOCTYPECLOSE"
  | DTDDECLOPEN -> "DTDDECLOPEN"
  | DTDDECLCLOSE -> "DTDDECLCLOSE"
  | S -> "S"
  | LOPENINGTAG -> "LOPENINGTAG"
  | LCLOSINGTAG -> "LCLOSINGTAG"
  | ROPENINGTAG -> "ROPENINGTAG"
  | RCLOSINGTAG -> "RCLOSINGTAG"
  | REMPTYELEMENT -> "REMPTYELEMENT"
  | TEXTLCLOSINGTAG str -> "TEXTLCLOSINGTAG<" ^ str ^ ">"
  | TEXTLOPENINGTAG str -> "TEXTLOPENINGTAG<" ^ str ^ ">"
  | TEXTLCURLY str -> "TEXTLCURLY<" ^ str ^ ">"
  | OPENINGPI ncname -> "OPENINGPI<" ^ ncname ^ ">"
  | LOPENINGCOMMENT -> "LOPENINGCOMMENT"
  | TEXTCLOSINGPI str -> "TEXTCLOSINGPI<" ^ str ^ ">"
  | TEXTRCLOSINGCOMMENT str -> "TEXTRCLOSINGCOMMENT<" ^ str ^ ">"
  | TEXTOPENINGPI (str,ncname) -> "TEXTOPENINGPI<" ^ str ^ "," ^ ncname ^ ">"
  | TEXTLOPENINGCOMMENT str -> "TEXTLOPENINGCOMMENT<" ^ str ^ ">"
  | ATTRIBUTETEXT str -> "ATTRIBUTETEXT<" ^ str ^ ">"
  | ATTRIBUTETEXTLCURLY str -> "ATTRIBUTETEXTLCURLY<" ^ str ^ ">"
  | TEXTCHARREF (str,i) -> "TEXTCHARREF<" ^ str ^ "," ^ (string_of_int i) ^ ">"
  | TEXTENTITYREF (str1,str2) -> "TEXTENTITYREF<" ^ str1 ^ "," ^ str2 ^ ">"
  | PEREF ncname -> "PEREF<" ^ ncname ^ ">"
  | PERCENT -> "PERCENT"
  | NMTOKEN ncname -> "NMTOKEN<" ^ ncname ^ ">"
  | REQUIRED -> "REQUIRED"
  | IMPLIED -> "IMPLIED"
  | FIXED -> "FIXED"
  | PCDATA -> "PCDATA"
  | ELEMENTDECL -> "ELEMENTDECL"
  | ATTLISTDECL -> "ATTLISTDECL"
  | ENTITYDECL -> "ENTITYDECL"
  | NOTATIONDECL -> "NOTATIONDECL"
  | BEGINDECL -> "BEGINDECL"
  | ENDDECL -> "ENDDECL"
  | BODYDECL -> "BODYDECL"
  | DECLAREDEFAULTCOLLATION -> "DECLAREDEFAULTCOLLATION"
  | DECLARENAMESPACE -> "DECLARENAMESPACE"
  | DECLAREBASEURI -> "DECLAREBASEURI"
  | MODULENAMESPACE -> "MODULENAMESPACE"
  | INTERFACENAMESPACE -> "INTERFACENAMESPACE"
  | DECLAREDEFAULTELEMENT -> "DECLAREDEFAULTELEMENT"
  | DECLAREDEFAULTFUNCTION -> "DECLAREDEFAULTFUNCTION"
  | IMPORTSCHEMA -> "IMPORTSCHEMA"
  | IMPORTMODULE -> "IMPORTMODULE"
  | IMPORTINTERFACE -> "IMPORTINTERFACE"
  | IMPORTSERVICE -> "IMPORTSERVICE"
  | NAMESPACE -> "NAMESPACE"
  | DEFAULTELEMENT -> "DEFAULTELEMENT"
  | DECLAREVALUEINDEX -> "DECLAREVALUEINDEX"
  | DECLARENAMEINDEX -> "DECLARENAMEINDEX"
  | DECLAREFUNCTION -> "DECLAREFUNCTION"
  | EXTERNAL -> "EXTERNAL"
  | DECLAREUPDATINGFUNCTION -> "DECLAREUPDATINGFUNCTION"
  | DECLAREVARIABLE -> "DECLAREVARIABLE"
  | XQUERYVERSION -> "XQUERYVERSION"
  | ENCODING -> "ENCODING"
  | PRESERVE -> "PRESERVE"
  | NOPRESERVE -> "NOPRESERVE"
  | INHERIT -> "INHERIT"
  | NOINHERIT -> "NOINHERIT"
  | STRIP -> "STRIP"
  | ORDERED -> "ORDERED"
  | UNORDERED -> "UNORDERED"
  | DECLARECONSTRUCTION -> "DECLARECONSTRUCTION"
  | DECLAREBOUNDARYSPACE -> "DECLAREBOUNDARYSPACE"
  | DECLAREORDERING -> "DECLAREORDERING"
  | DECLAREOPTION -> "DECLAREOPTION"
  | DECLARESERVER -> "DECLARESERVER"
  | DECLAREDEFAULTORDER -> "DECLAREDEFAULTORDER"
  | DECLARECOPYNAMESPACES -> "DECLARECOPYNAMESPACES"
  | DECLARESCHEMALCURLY -> "DECLARESCHEMALCURLY"
  | DECLAREATTRIBUTE uqname -> "DECLAREATTRIBUTE<" ^ (Namespace_names.string_of_uqname uqname) ^ ">"
  | DECLAREELEMENT uqname -> "DECLAREELEMENT<" ^ (Namespace_names.string_of_uqname uqname) ^ ">"
  | DECLARESIMPLETYPE uqname -> "DECLARESIMPLETYPE<" ^ (Namespace_names.string_of_uqname uqname) ^ ">"
  | DECLARECOMPLEXTYPE uqname -> "DECLARECOMPLEXTYPE<" ^ (Namespace_names.string_of_uqname uqname) ^ ">"
  | DECLAREGROUP uqname -> "DECLAREGROUP<" ^ (Namespace_names.string_of_uqname uqname) ^ ">"
  | DECLAREATTRGROUP uqname -> "DECLAREATTRGROUP<" ^ (Namespace_names.string_of_uqname uqname) ^ ">"
  | GROUP -> "GROUP"
  | ATTRGROUP -> "ATTRGROUP"
  | OFTYPE -> "OFTYPE"
  | OFSIMPLETYPE -> "OFSIMPLETYPE"
  | NILLABLE -> "NILLABLE"
  | MIXED -> "MIXED"
  | SUBSTITUTESFOR -> "SUBSTITUTESFOR"
  | RESTRICTS -> "RESTRICTS"
  | EXTENDS -> "EXTENDS"
  | LISTOF -> "LISTOF"
  | UNIONOF -> "UNIONOF"
  | ATTRIBUTEQNAMECURLY uqname -> "ATTRIBUTEQNAMECURLY<" ^ (Namespace_names.string_of_uqname uqname) ^ ">"
  | ELEMENTQNAMECURLY uqname -> "ELEMENTQNAMECURLY<" ^ (Namespace_names.string_of_uqname uqname) ^ ">"
  | NAMESPACENCNAMECURLY ncname -> "NAMESPACENCNAMECURLY<" ^ ncname ^ ">"
  | PINCNAMECURLY ncname -> "PINCNAMECURLY<" ^ ncname ^ ">"
  | DOCUMENTCURLY -> "DOCUMENTCURLY"
  | TEXTCURLY -> "TEXTCURLY"
  | PICURLY -> "PICURLY"
  | COMMENTCURLY -> "COMMENTCURLY"
  | ATTRIBUTECURLY -> "ATTRIBUTECURLY"
  | ELEMENTCURLY -> "ELEMENTCURLY"
  | PRAGMA (uqname,str) -> "PRAGMA<" ^ (Namespace_names.string_of_uqname uqname) ^ "," ^ str ^ ">"
  | DOCUMENTNODELPAR -> "DOCUMENTNODELPAR"
  | ELEMENTLPAR -> "ELEMENTLPAR"
  | ATTRIBUTELPAR -> "ATTRIBUTELPAR"
  | TEXTLPAR -> "TEXTLPAR"
  | SCHEMAELEMENTLPAR -> "SCHEMAELEMENTLPAR"
  | SCHEMAATTRIBUTELPAR -> "SCHEMAATTRIBUTELPAR"
  | EMPTYSEQUENCELPAR -> "EMPTYSEQUENCELPAR"
  | TYPELPAR -> "TYPELPAR"
  | ITEMLPAR -> "ITEMLPAR"
  | NUMERICLPAR -> "NUMERICLPAR"
  | ANYSTRINGLPAR -> "ANYSTRINGLPAR"
  | ELEMENT -> "ELEMENT"
  | ATTRIBUTE -> "ATTRIBUTE"
  | DOCUMENT -> "DOCUMENT"
  | TYPE -> "TYPE"
  | NONE -> "NONE"
  | QUESTION -> "QUESTION"
  | NODE -> "NODE"
  | ITEM -> "ITEM"
  | TEXT -> "TEXT"
  | PROCESSINGINSTRUCTION -> "PROCESSINGINSTRUCTION"
  | COMMENT -> "COMMENT"
  | ASCENDING -> "ASCENDING"
  | DESCENDING -> "DESCENDING"
  | EMPTY -> "EMPTY"
  | GREATEST -> "GREATEST"
  | LEAST -> "LEAST"
  | OR -> "OR"
  | AND -> "AND"
  | STAR -> "STAR"
  | MULT -> "MULT"
  | DIV -> "DIV"
  | IDIV -> "IDIV"
  | MOD -> "MOD"
  | MINUS -> "MINUS"
  | PLUS -> "PLUS"
  | IPLUS -> "IPLUS"
  | ISTAR -> "ISTAR"
  | STARNCNAME ncname -> "STARNCNAME<" ^ ncname ^ ">"
  | NCNAMESTAR ncname -> "NCNAMESTAR<" ^ ncname ^ ">"
  | IN -> "IN"
  | SATISFIES -> "SATISFIES"
  | RETURN -> "RETURN"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | TO -> "TO"
  | WHERE -> "WHERE"
  | INTERSECT -> "INTERSECT"
  | UNION -> "UNION"
  | EXCEPT -> "EXCEPT"
  | PRECEDES -> "PRECEDES"
  | FOLLOWS -> "FOLLOWS"
  | CASE -> "CASE"
  | INSTANCEOF -> "INSTANCEOF"
  | DEFAULT -> "DEFAULT"
  | IFLPAR -> "IFLPAR"
  | TYPESWITCHLPAR -> "TYPESWITCHLPAR"
  | STABLEORDERBY -> "STABLEORDERBY"
  | ORDERBY -> "ORDERBY"
  | ASSERTAS -> "ASSERTAS"
  | CASTAS -> "CASTAS"
  | CASTABLEAS -> "CASTABLEAS"
  | TREATAS -> "TREATAS"
  | VALIDATELCURLY -> "VALIDATELCURLY"
  | VALIDATESTRICTLCURLY -> "VALIDATESTRICTLCURLY"
  | VALIDATELAXLCURLY -> "VALIDATELAXLCURLY"
  | ORDEREDCURLY -> "ORDEREDCURLY"
  | UNORDEREDCURLY -> "UNORDEREDCURLY"
  | SOMEDOLLAR -> "SOMEDOLLAR"
  | EVERYDOLLAR -> "EVERYDOLLAR"
  | FORDOLLAR -> "FORDOLLAR"
  | LETDOLLAR -> "LETDOLLAR"
  | AS -> "AS"
  | AT -> "AT"
  | DELETENODE -> "DELETENODE"
  | INSERTNODE -> "INSERTNODE"
  | RENAMENODE -> "RENAMENODE"
  | REPLACENODE -> "REPLACENODE"
  | REPLACEVALUEOFNODE -> "REPLACEVALUEOFNODE"
  | ASFIRST -> "ASFIRST"
  | ASLAST -> "ASLAST"
  | INTO -> "INTO"
  | BEFORE -> "BEFORE"
  | AFTER -> "AFTER"
  | WITH -> "WITH"
  | COPYDOLLAR -> "COPYDOLLAR"
  | MODIFY -> "MODIFY"
  | COPYLCURLY -> "COPYLCURLY"
  | SNAPLCURLY -> "SNAPLCURLY"
  | SNAPLCURLYORDERED -> "SNAPLCURLYORDERED"
  | SNAPDELETENODE -> "SNAPDELETENODE"
  | SNAPINSERTNODE -> "SNAPINSERTNODE"
  | SNAPRENAMENODE -> "SNAPRENAMENODE"
  | SNAPREPLACENODE -> "SNAPREPLACENODE"
  | SNAPREPLACEVALUEOFNODE -> "SNAPREPLACEVALUEOFNODE"
  | WHILELPAR -> "WHILELPAR"
  | LETVARDOLLAR -> "LETVARDOLLAR"
  | SETDOLLAR -> "SETDOLLAR"
  | DECLAREDOLLAR -> "DECLAREDOLLAR"
  | NCNAME ncname -> "NCNAME<" ^ ncname ^ ">"
  | QNAME uqname -> "QNAME<" ^ (Namespace_names.string_of_uqname uqname) ^ ">"
  | VARNAME uqname -> "VARNAME<" ^ (Namespace_names.string_of_uqname uqname) ^ ">"
  | QNAMEPLUS uqname -> "QNAMEPLUS<" ^ (Namespace_names.string_of_uqname uqname) ^ ">"
  | QNAMESTAR uqname -> "QNAMESTAR<" ^ (Namespace_names.string_of_uqname uqname) ^ ">"
  | QNAMEQUESTION uqname -> "QNAMEQUESTION<" ^ (Namespace_names.string_of_uqname uqname) ^ ">"
  | AXIS axis -> "AXIS<" ^ (Xquery_common_ast_util.string_of_axis axis) ^ ">"
  | PROCESSINGINSTRUCTIONLPAR -> "PROCESSINGINSTRUCTIONLPAR"
  | COMMENTLPAR -> "COMMENTLPAR"
  | NODELPAR -> "NODELPAR"
  | DECIMAL dec -> "DECIMAL<" ^ (Decimal._string_of_decimal dec) ^ ">"
  | INT i -> "INT<" ^ (Decimal._string_of_integer i) ^ ">"
  | DOUBLE f -> "DOUBLE<" ^ (string_of_float f) ^ ">"
  | STRING str -> "STRING<" ^ str ^ ">"
  | COLON -> "COLON"
  | SLASH -> "SLASH"
  | SLASHSLASH -> "SLASHSLASH"
  | DOT -> "DOT"
  | DOTDOT -> "DOTDOT"
  | EQ -> "EQ"
  | EQUALS -> "EQUALS"
  | NE -> "NE"
  | NOTEQUALS -> "NOTEQUALS"
  | IS -> "IS"
  | LTE -> "LTE"
  | GTE -> "GTE"
  | LTEQUALS -> "LTEQUALS"
  | GTEQUALS -> "GTEQUALS"
  | COLONEQUALS -> "COLONEQUALS"
  | LTOP -> "LTOP"
  | GTOP -> "GTOP"
  | LT -> "LT"
  | GT -> "GT"
  | BAR -> "BAR"
  | ATSIGN -> "ATSIGN"
  | COMMA -> "COMMA"
  | AMPERSAND -> "AMPERSAND"
  | SEMICOLON -> "SEMICOLON"
  | LPAR -> "LPAR"
  | RPAR -> "RPAR"
  | IRPAR -> "IRPAR"
  | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK"
  | LCURLY -> "LCURLY"
  | RCURLY -> "RCURLY"
  | DOLLAR -> "DOLLAR"
  | FUNCTIONNAMELPAR uqname -> "FUNCTIONNAMELPAR<" ^ (Namespace_names.string_of_uqname uqname) ^ ">"
  | LETSERVER -> "LETSERVER"
  | IMPLEMENT -> "IMPLEMENT"
  | FROMSERVER -> "FROMSERVER"
  | ATSERVER -> "ATSERVER"
  | FORSERVER -> "FORSERVER"
  | BOX -> "BOX"
  | EVALCLOSURE -> "EVALCLOSURE"
  | DO -> "DO"
  | EOF -> "EOF"
  | LEXERROR -> "LEXERROR"

let string_of_state state =
  match state with
  | DefaultState -> "DefaultState"
  | OperatorState -> "OperatorState"
  | RenameOperatorState -> "RenameOperatorState"
  | NamespaceDeclState -> "NamespaceDeclState"
  | NamespaceKeywordState -> "NamespaceKeywordState"
  | CopyNamespacesState -> "CopyNamespacesState"
  | XMLSpaceDeclState -> "XMLSpaceDeclState"
  | ItemTypeState -> "ItemTypeState"
  | KindTestState -> "KindTestState"
  | KindTestForPIState -> "KindTestForPIState"
  | SchemaContextStepState -> "SchemaContextStepState"
  | VarNameState -> "VarNameState"
  | VarDeclState -> "VarDeclState"
  | PragmaState -> "PragmaState"
  | StartTagState -> "StartTagState"
  | ElementContentState -> "ElementContentState"
  | EndTagState -> "EndTagState"
  | XMLCommentState -> "XMLCommentState"
  | PIState -> "PIState"
  | CDATASectionState -> "CDATASectionState"
  | QuoteAttributeContentState -> "QuoteAttributeContentState"
  | AposAttributeContentState -> "AposAttributeContentState"
  | EntityIncludedInLiteralState -> "EntityIncludedInLiteralState"
  | SchemaDeclarationState -> "SchemaDeclarationState"
  | TypeDeclarationState -> "TypeDeclarationState"
  | XTypeState -> "XTypeState"

let print_state ff state =
  Format.fprintf ff "%s;" (string_of_state state)

let rec print_states_aux ff states =
  match states with
  | [] -> ()
  | x :: [] -> print_state ff x
  | x :: l -> Format.fprintf ff "%a@,%a" print_state x print_states_aux l

let print_states psf ff states =
  Format.fprintf ff "@[<hv 2>stack {@,%a@;<0 -2>}@]" print_states_aux states

let print_token psf ff (tok,states,flag) =
  Format.pp_print_tab ff ();
  Format.fprintf ff "%s" (string_of_tok tok);
  if psf then
    begin
      Format.pp_print_tab ff ();
      Format.fprintf ff "%a" (print_states psf) states;
      Format.pp_print_tab ff ();
      Format.fprintf ff "%b" flag
    end

let print_tokens_aux psf ff tl =
  List.iter (print_token psf ff) tl

let print_tokens psf ff tl =
  Format.pp_open_tbox ff ();
  Format.pp_set_tab ff ();
  Format.fprintf ff "Tokens                                     ";
  if psf then
    begin
      Format.pp_set_tab ff ();
      Format.fprintf ff "Stack                                     ";
      Format.pp_set_tab ff ();
      Format.fprintf ff "ItemKind Flag";
    end;
  Format.pp_print_tab ff ();
  Format.fprintf ff "------";
  if psf then
    begin
      Format.pp_print_tab ff ();
      Format.fprintf ff "-----                                     ";
      Format.pp_print_tab ff ();
      Format.fprintf ff "-------------";
    end;
  print_tokens_aux psf ff tl;
  Format.pp_close_tbox ff ();
  Format.fprintf ff "@."
