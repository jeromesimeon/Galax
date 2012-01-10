(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lexing_util.mli,v 1.15 2007/07/31 17:34:08 simeon Exp $ *)

(* Module: Lexutil
   Description:
     This module contains some basic functions used during lexing.
*)

open Parse_xquery

(****************************)
(* Lexing global parameters *)
(****************************)

(* Language currently parsed *)

type language_kind =
  | XPath_2_0
  | XQuery_1_0


(*****************)
(* Lexical state *)
(*****************)

(* Note:
     Galax uses lexical states to implement both XML and XQuery
     parsing. See XQuery 1.0 specifiction [A.2.2 Lexical Rules] for
     more details on those lexical states.
   - Jerome
 *)


type lex_state =
      (* XQuery states *)
  | DefaultState
  | OperatorState
  | RenameOperatorState
  | NamespaceDeclState
  | NamespaceKeywordState
  | CopyNamespacesState
  | XMLSpaceDeclState
  | ItemTypeState
  | KindTestState
  | KindTestForPIState
  | SchemaContextStepState
  | VarNameState
  | VarDeclState
  | PragmaState
      (* XML & XQuery states *)
  | StartTagState
  | ElementContentState
  | EndTagState
  | XMLCommentState
  | PIState
  | CDATASectionState
  | QuoteAttributeContentState      (* Note: There are three kinds of attribute value lexing. - Jerome *)
  | AposAttributeContentState
  | EntityIncludedInLiteralState
      (* Galax extensions states *)
  | SchemaDeclarationState
  | TypeDeclarationState
  | XTypeState


(******************)
(* Lexical states *)
(******************)

(* A lexical state handler ... *)

type lexing_handler

(* Initialization of the lexical state *)

val init_xquery_lexing : unit -> lexing_handler
val init_type_lexing   : unit -> lexing_handler

(* Accesses the current lexical state *)

val get_current_lex_state : lexing_handler -> lex_state
val get_whole_lex_stack   : lexing_handler -> lex_state list

(* Access the comments depth *)

val get_depth : lexing_handler -> int
val set_depth : lexing_handler -> int -> unit

(* String and text handling *)

val reset_string           : lexing_handler -> unit
val add_char_to_string     : lexing_handler -> char -> unit
val add_string_to_string   : lexing_handler -> string -> unit
val get_string             : lexing_handler -> string


(* Accesses the current quote kind *)

type attribute_quote_kind =
  | SingleQuoteKind
  | DoubleQuoteKind
  | EntityIncludedInLiteralKind

val attribute_quote_kind : lexing_handler -> attribute_quote_kind

(* Stack discipline for the lexical state *)

val push_state                  : lex_state -> lexing_handler -> unit
val push_none                   : lexing_handler -> unit
val push_default                : lexing_handler -> unit  (* XQuery states *)
val push_operator               : lexing_handler -> unit
val push_rename_op              : lexing_handler -> unit
val push_namespacedecl          : lexing_handler -> unit
val push_namespacekeyword       : lexing_handler -> unit
val push_copynamespaces         : lexing_handler -> unit
val push_xmlspacedecl           : lexing_handler -> unit
val push_itemtype               : lexing_handler -> unit
val push_kindtest               : lexing_handler -> unit
val push_kindtestforpi          : lexing_handler -> unit
val push_schemacontextstep      : lexing_handler -> unit
val push_varname                : lexing_handler -> unit
val push_vardecl                : lexing_handler -> unit

val push_opening_tag            : lexing_handler -> unit (* XML & XQuery states *)
val push_text                   : lexing_handler -> unit
val push_closing_tag            : lexing_handler -> unit
val push_comment                : lexing_handler -> unit
val push_processing_instruction : lexing_handler -> unit
val push_pragma                 : lexing_handler -> unit
val push_cdata                  : lexing_handler -> unit
val push_attribute_text_double  : lexing_handler -> unit
val push_attribute_text_single  : lexing_handler -> unit

val push_entity_included            : lexing_handler -> unit (* entity states -- used from the parser itself! *)
val push_entity_included_in_literal : lexing_handler -> unit

(* Galax extensions states *)
val push_schema_declaration         : lexing_handler -> unit
val push_type_declaration           : lexing_handler -> unit
val push_xtype                      : lexing_handler -> unit

(* Poping the lexical state *)

val pop_state                   : lexing_handler -> unit
val pop_state_keep_buffer       : lexing_handler -> unit


(**************)
(* Parse kind *)
(**************)

val get_parsing_kind : lexing_handler -> language_kind


(********************)
(* Token processing *)
(********************)

type qname_kind =
  | NCNAME_KIND of string
  | QNAME_KIND of Namespace_names.uqname

(* Processes a qname *)

val process_qname_string : string -> qname_kind

(* Extracts an axis name *)

val get_axis : string -> Xquery_common_ast.axis

(* Extracts escaped name *)

val get_escaped_name : string -> string

(* Check the validity of a PI target name *)

val get_target_pi  : string -> string

(* Extracts the character encoding *)

val get_xml_encoding_quotes        : string -> string
val get_xml_encoding_single_quotes : string -> string

(* Extracts character and entity references *)

val get_char_ref       : string -> string
val get_hexchar_ref    : string -> string
val get_entity_ref     : string -> string

(* val inside_rename_stmt : bool ref  *)

val qname_lexing_error    : Finfo.finfo -> 'a
val qnamesep_lexing_error : Finfo.finfo -> 'a

val match_operator_keyword  : Finfo.finfo -> lexing_handler -> string -> token option
val match_default_keyword   : Finfo.finfo -> lexing_handler -> string -> token option
val match_namespace_keyword : Finfo.finfo -> lexing_handler -> string -> token option
val match_curly             : Finfo.finfo -> lexing_handler -> token option
val match_paren             : Finfo.finfo -> lexing_handler -> token option

val get_buffered_tokens : lexing_handler -> Parse_xquery.token list
val set_buffered_tokens : lexing_handler -> Parse_xquery.token list -> unit

val get_buffered : lexing_handler -> string list
val set_buffered : lexing_handler -> string list -> unit

val default_token : lexing_handler -> bool

val make_new_token : Finfo.finfo -> lexing_handler -> (lexing_handler -> unit) -> token -> token option

val toop_pushdef  : lexing_handler -> unit
val todef         : lexing_handler -> unit
val toren_pushdef : lexing_handler -> unit
val toop_pushitem : lexing_handler -> unit
val toop_pushvar  : lexing_handler -> unit
val tonamespace   : lexing_handler -> unit
val todecl        : lexing_handler -> unit
val tonamespace   : lexing_handler -> unit
val tocopy        : lexing_handler -> unit
val pushitem      : lexing_handler -> unit
val pushdef       : lexing_handler -> unit
val tokinddef     : lexing_handler -> unit
val tonone        : lexing_handler -> unit

val set_item_type : lexing_handler -> unit
val unset_item_type : lexing_handler -> unit

val check_item_type : lexing_handler -> bool
val get_item_type : lexing_handler -> bool
