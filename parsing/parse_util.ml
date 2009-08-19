(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parse_util.ml,v 1.16 2007/07/31 17:34:08 simeon Exp $ *)


(* Module: Parse_util
   Description:
     This module implements some internal utilities functions used
     during parsing.
 *)

open Finfo
open Error
open Conf
open Encoding

open Xquery_common_ast
open Xquery_ast

open Lexing
open Lexing_util


(*******************)
(* Lexers handling *)
(*******************)

type xml_lexers =
    { encoding             : Encoding.encoding;
      opening_tag_state    : lexing_handler -> lexbuf -> Parse_xquery.token;
      closing_tag_state    : lexing_handler -> lexbuf -> Parse_xquery.token;
      text_state           : lexing_handler -> lexbuf -> Parse_xquery.token;
      attribute_text_state : lexing_handler -> lexbuf -> Parse_xquery.token;
      pi_state             : lexing_handler -> lexbuf -> Parse_xquery.token;
      cdata_state          : lexing_handler -> lexbuf -> Parse_xquery.token;
      comment_state        : lexing_handler -> lexbuf -> Parse_xquery.token }


type language_lexers =
    { language             	: language_kind;
      schema_declaration_state  : lexing_handler -> lexbuf -> Parse_xquery.token;
      type_declaration_state   	: lexing_handler -> lexbuf -> Parse_xquery.token;
      xtype_state         	: lexing_handler -> lexbuf -> Parse_xquery.token;
      default_state        	: lexing_handler -> lexbuf -> Parse_xquery.token option;
      operator_state         	: lexing_handler -> lexbuf -> Parse_xquery.token option;
      namespacedecl_state    	: lexing_handler -> lexbuf -> Parse_xquery.token;
      namespacekeyword_state 	: lexing_handler -> lexbuf -> Parse_xquery.token option;
      copynamespaces_state 	: lexing_handler -> lexbuf -> Parse_xquery.token;
      xmlspacedecl_state     	: lexing_handler -> lexbuf -> Parse_xquery.token;
      itemtype_state         	: lexing_handler -> lexbuf -> Parse_xquery.token;
      kindtest_state         	: lexing_handler -> lexbuf -> Parse_xquery.token;
      kindtestforpi_state    	: lexing_handler -> lexbuf -> Parse_xquery.token;
      schemacontextstep_state   : lexing_handler -> lexbuf -> Parse_xquery.token;
      varname_state             : lexing_handler -> lexbuf -> Parse_xquery.token;
      vardecl_state             : lexing_handler -> lexbuf -> Parse_xquery.token;
      pragma_state              : lexing_handler -> lexbuf -> Parse_xquery.token }

let lexers_by_encoding = Hashtbl.create 5;;

let dummy_lexer x y =
  raise (Query (Undefined ("No lexical support compiled! You must have at least one character encoding selected before compilation!!")))

let current_xml_lexer = ref
    { encoding = `Enc_utf8;
      opening_tag_state    = dummy_lexer;
      closing_tag_state    = dummy_lexer;
      text_state           = dummy_lexer;
      attribute_text_state = dummy_lexer;
      pi_state             = dummy_lexer;
      cdata_state          = dummy_lexer;
      comment_state        = dummy_lexer }

let current_xquery_lexer = ref
    { language               	= XQuery_1_0;
      schema_declaration_state 	= Schema_lexer.token;
      type_declaration_state   	= Type_lexer.type_declaration;
      xtype_state        	= Type_lexer.xtype;
      default_state          	= Default_lexer.token;
      operator_state         	= Operator_lexer.token;
      namespacedecl_state    	= Namespacedecl_lexer.token;
      namespacekeyword_state 	= Namespacekeyword_lexer.token;
      copynamespaces_state 	= Copynamespaces_lexer.token;
      xmlspacedecl_state     	= dummy_lexer;
      itemtype_state         	= Itemtype_lexer.token;
      kindtest_state         	= Kindtest_lexer.token;
      kindtestforpi_state    	= dummy_lexer;
      schemacontextstep_state   = dummy_lexer;
      varname_state             = Varname_lexer.token;
      vardecl_state             = Varname_lexer.vardecl;
      pragma_state              = Pragma_lexer.token }

let register_lexer lx =
  let encoding = lx.encoding in
  begin
    match encoding with
    | `Enc_utf8 ->
	begin
	  set_internal_encoding `Enc_utf8;
	  current_xml_lexer := lx
	end
    | `Enc_iso88591 ->
	if Hashtbl.mem lexers_by_encoding `Enc_utf8
	then ()
	else
	  begin
	    set_internal_encoding `Enc_iso88591;
	    current_xml_lexer := lx
	  end
    | _ ->
	raise (Query (Undefined "Internal encoding should be either UTF8 or ISO-8859-1"))
  end;
  Hashtbl.add lexers_by_encoding lx.encoding lx

let set_lexer enc =
  match enc with
  | `Enc_utf8 ->
      let lx =
	try
	  Hashtbl.find lexers_by_encoding `Enc_utf8
	with
	| _ ->
	    raise (Query (Undefined ("Support for UTF8 internal encoding node compiled! You must select UTF8 at configuration time.")))
      in
      set_internal_encoding `Enc_utf8;
      current_xml_lexer := lx
  | `Enc_iso88591 ->
      let lx =
	try
	  Hashtbl.find lexers_by_encoding `Enc_iso88591
	with
	| _ ->
	    raise (Query (Undefined ("Support for ISO-8859-1 internal encoding node compiled! You must select ISO-8859-1 at configuration time.")))
      in
      set_internal_encoding `Enc_iso88591;
      current_xml_lexer := lx
  | _ ->
      raise (Query (Undefined "Internal encoding can only be UTF8 or ISO-8859-1"))


(*******************)
(* Lexing wrappers *)
(*******************)

(* Note:
     These lexing functions switch according to the lexical states.
   - Jerome
 *)

(* XQuery lexer *)

let op_wrap lh x = unset_item_type lh; x

let xquery_lexfun_apply lh x =
  match get_current_lex_state lh with
    (* XQuery lexical states *)
  | DefaultState           -> !current_xquery_lexer.default_state lh x
  | OperatorState          -> op_wrap lh (!current_xquery_lexer.operator_state lh x)
  | RenameOperatorState    -> raise (Query (Internal_Error "found rename state! ouch!"))
  | NamespaceDeclState     -> Some (!current_xquery_lexer.namespacedecl_state lh x)
  | NamespaceKeywordState  -> (!current_xquery_lexer.namespacekeyword_state lh x)
  | CopyNamespacesState    -> Some (!current_xquery_lexer.copynamespaces_state lh x)
  | XMLSpaceDeclState      -> Some (!current_xquery_lexer.xmlspacedecl_state lh x)
  | ItemTypeState          -> Some (!current_xquery_lexer.itemtype_state lh x)
  | KindTestState          -> Some (!current_xquery_lexer.kindtest_state lh x)
  | KindTestForPIState     -> Some (!current_xquery_lexer.kindtestforpi_state lh x)
  | SchemaContextStepState -> Some (!current_xquery_lexer.schemacontextstep_state lh x)
  | VarNameState           -> Some (!current_xquery_lexer.varname_state lh x)
  | VarDeclState           -> Some (!current_xquery_lexer.vardecl_state lh x)
  | PragmaState            -> Some (!current_xquery_lexer.pragma_state lh x)
	  
     (* Both XML & XQuery states *)
  | StartTagState                -> Some (!current_xml_lexer.opening_tag_state lh x)
  | ElementContentState          -> Some (!current_xml_lexer.text_state lh x)
  | EndTagState                  -> Some (!current_xml_lexer.closing_tag_state lh x)
  | XMLCommentState              -> Some (!current_xml_lexer.comment_state lh x)
  | PIState                      -> Some (!current_xml_lexer.pi_state lh x)
  | CDATASectionState            -> Some (!current_xml_lexer.cdata_state lh x)
  | QuoteAttributeContentState   -> Some (!current_xml_lexer.attribute_text_state lh x)
  | AposAttributeContentState    -> Some (!current_xml_lexer.attribute_text_state lh x)
  | EntityIncludedInLiteralState -> Some (!current_xml_lexer.attribute_text_state lh x)

     (* Galax extensions states *)
  | SchemaDeclarationState       -> Some (!current_xquery_lexer.schema_declaration_state lh x)
  | TypeDeclarationState         -> Some (!current_xquery_lexer.type_declaration_state lh x)
  | XTypeState                   -> Some (!current_xquery_lexer.xtype_state lh x)

let flush_buffers lh x =
  (* 1. fill the buffer if need be *)
  match get_buffered lh with
  | [] -> x
  | s :: [] ->
      set_buffered lh [];
      if default_token lh
      then
	begin
	  set_buffered_tokens lh ((get_buffered_tokens lh) @ [x]);
	  Parse_xquery.NCNAME s
	end
      else
	raise (Query (Internal_Error "Non-empty string buffer in parser!!"))
  | _ ->
      set_buffered lh [];
      raise (Query (Internal_Error "Non-empty string buffer in parser!!"))

let rec xquery_lexfun lh x =
  match get_buffered_tokens lh with
  | [] ->
      begin
	match xquery_lexfun_apply lh x with
	| None -> xquery_lexfun lh x
	| Some t -> flush_buffers lh t
      end
  | t :: others ->
      begin
	set_buffered_tokens lh others;
	t
      end

