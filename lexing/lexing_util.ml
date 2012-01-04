(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lexing_util.ml,v 1.29 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Lexing_util
   Description:
     This module contains some basic functions used during
     lexing. Notably this support lexical state handling during XML
     and XQuery parsing.
*)

open Error

open Xquery_common_ast
open Xquery_ast
open Xquery_core_ast

open Parse_xquery

(****************************)
(* Lexing global parameters *)
(****************************)

(* Character encoding *)

type encoding_kind =
  | Utf8
  | Iso88591


(* Language currently parsed *)

type language_kind =
  | XPath_2_0
  | XQuery_1_0


(* Lexical state *)

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


(**************************)
(* Lexical state handling *)
(**************************)

type lexing_handler =
    { current_lex_state        : lex_state Stack.t;
      current_parsing_kind     : language_kind;
      mutable flag_item_type   : bool;
      mutable depth            : int;
      mutable string_buffer    : string;
      mutable string_end       : int;
      mutable default_token    : bool;
      mutable buffered         : string array;
      mutable buffered_tokens  : token list }

let initial_string_buffer () = String.create 256

let get_current_lex_state lh =
  Stack.top lh.current_lex_state

let get_whole_lex_stack lh =
  let sl = ref [] in
  let st = lh.current_lex_state in
  let f t = sl := t :: !sl in
  begin
    Stack.iter f st;
    List.rev !sl
  end

(* Item type flag *)

let set_item_type lh =
  lh.flag_item_type <- true

let unset_item_type lh =
  lh.flag_item_type <- false

let check_item_type lh =
  let x = lh.flag_item_type in
  begin
    unset_item_type lh;
    x
  end

(****************************)
(* String and text handling *)
(****************************)

(* Manipulation of the string buffer *)

let reset_string lh =
  lh.string_buffer <- initial_string_buffer ();
  lh.string_end <- 0

let add_char_to_string lh ch =
  let len = String.length (lh.string_buffer) in
  if lh.string_end >= len
  then
    begin
      let newBuffer = String.create (len * 2) in
      String.blit (lh.string_buffer) 0 newBuffer 0 len;
      lh.string_buffer <- newBuffer
    end;
  String.unsafe_set lh.string_buffer lh.string_end ch;
  lh.string_end <- succ lh.string_end

let add_string_to_string lh str =
  String.iter (add_char_to_string lh) str

let get_string lh =
  let s = String.sub lh.string_buffer 0 lh.string_end in
  lh.string_buffer <- initial_string_buffer ();
  s


(****************************)
(* Push a new lexical state *)
(****************************)

(* XQuery states *)

let push_state st lh = 
  reset_string lh;
  Stack.push st lh.current_lex_state

let push_none lh = ()

let push_default lh =
  reset_string lh;
  Stack.push DefaultState lh.current_lex_state

let push_operator lh =
  reset_string lh;
  Stack.push OperatorState lh.current_lex_state

let push_rename_op lh = 
  reset_string lh;
  Stack.push RenameOperatorState lh.current_lex_state

let push_namespacedecl lh =
  Stack.push NamespaceDeclState lh.current_lex_state

let push_namespacekeyword lh =
  Stack.push NamespaceKeywordState lh.current_lex_state

let push_copynamespaces lh =
  Stack.push CopyNamespacesState lh.current_lex_state

let push_xmlspacedecl lh =
  Stack.push XMLSpaceDeclState lh.current_lex_state

let push_itemtype lh =
  Stack.push ItemTypeState lh.current_lex_state

let push_kindtest lh =
  Stack.push KindTestState lh.current_lex_state

let push_kindtestforpi lh =
  Stack.push KindTestForPIState lh.current_lex_state

let push_schemacontextstep lh =
  Stack.push SchemaContextStepState lh.current_lex_state

let push_varname lh =
  Stack.push VarNameState lh.current_lex_state

let push_vardecl lh =
  Stack.push VarDeclState lh.current_lex_state

let push_pragma lh =
  reset_string lh;
  Stack.push PragmaState lh.current_lex_state

(* XML & XQuery states *)

let push_opening_tag lh =
  Stack.push StartTagState lh.current_lex_state

let push_text lh =
  reset_string lh;
  Stack.push ElementContentState lh.current_lex_state

let push_closing_tag lh =
  Stack.push EndTagState lh.current_lex_state

let push_comment lh =
  reset_string lh;
  Stack.push XMLCommentState lh.current_lex_state

let push_processing_instruction lh =
  reset_string lh;
  Stack.push PIState lh.current_lex_state

let push_cdata lh =
  Stack.push CDATASectionState lh.current_lex_state

let push_attribute_text_double lh =
  reset_string lh;
  Stack.push QuoteAttributeContentState lh.current_lex_state

let push_attribute_text_single lh =
  reset_string lh;
  Stack.push AposAttributeContentState lh.current_lex_state

let push_entity_included lh =
  reset_string lh;
  Stack.push ElementContentState lh.current_lex_state

let push_entity_included_in_literal lh =
  reset_string lh;
  Stack.push EntityIncludedInLiteralState lh.current_lex_state

(* Galax extensions states *)

let push_schema_declaration lh =
  Stack.push SchemaDeclarationState lh.current_lex_state

let push_type_declaration lh =
  Stack.push TypeDeclarationState lh.current_lex_state

let push_xtype lh =
  Stack.push XTypeState lh.current_lex_state

let new_lexing_handler language_kind =
  { current_lex_state    = Stack.create ();
    current_parsing_kind = language_kind;
    flag_item_type       = false;
    depth                = 0;
    string_buffer        = initial_string_buffer ();
    string_end           = 0;
    default_token        = false;
    buffered             = [||];
    buffered_tokens      = [] }


(****************************)
(* Poping the lexical state *)
(****************************)

let pop_state lh =
  reset_string lh;
  ignore(Stack.pop lh.current_lex_state)

let pop_state_keep_buffer lh =
  ignore(Stack.pop lh.current_lex_state)


(* Access the comments depth *)

let get_depth lh   = lh.depth
let set_depth lh i = lh.depth <- i


(****************)
(* Parsing kind *)
(****************)

let get_parsing_kind lh =
  lh.current_parsing_kind

type attribute_quote_kind =
  | SingleQuoteKind
  | DoubleQuoteKind
  | EntityIncludedInLiteralKind

let attribute_quote_kind lh =
  match get_current_lex_state lh with
  | AposAttributeContentState -> SingleQuoteKind
  | QuoteAttributeContentState -> DoubleQuoteKind
  | EntityIncludedInLiteralState -> EntityIncludedInLiteralKind
  | _ ->
      raise (Query (Internal_Error "Requiring quote kind outside of an attribute"))

let init_xquery_lexing () =
  let lh = new_lexing_handler XQuery_1_0 in
  Stack.push DefaultState lh.current_lex_state;
  lh

let init_type_lexing () =
  let lh = new_lexing_handler XQuery_1_0 in
  Stack.push XTypeState lh.current_lex_state;
  lh

(********************)
(* Token processing *)
(********************)

(* Extracts an axis name *)

let resolve_axis =
  function
    | "attribute" -> Attribute
    | "child" -> Child
    | "descendant" -> Descendant
    | "descendant-or-self" -> Descendant_or_self
    | "parent" -> Parent
    | "self" -> Self
    | "ancestor" -> Ancestor
    | "ancestor-or-self" -> Ancestor_or_self
    | "following-sibling" -> Following_sibling
    | "preceding-sibling" -> Preceding_sibling
    (* full axis feature -- Philippe *)
    | "following" -> Following
    | "preceding" -> Preceding
    | s -> raise (Query (Parsing (Finfo.parsing_locinfo (),"Unknown XQuery axis " ^ s)))

(* The following axis are not in XQuery - Jerome
    | "namespace" -> Namespace
*)

let get_axis axis_name =
  resolve_axis axis_name

(* Extracts escaped name *)

let get_escaped_name var_s =
  let len = (String.length var_s) - 1 in
  (String.sub var_s 1 len)

type qname_kind =
  | NCNAME_KIND of string
  | QNAME_KIND of Namespace_names.uqname

let process_qname_string s =
  try
    let i = String.index s ':' in
    if i = 0
    then
      NCNAME_KIND (get_escaped_name s)
    else
      QNAME_KIND (Namespace_names.uqname_element_of_string s)
  with
  | _ ->
      NCNAME_KIND s

(* Check the validity of a PI target name *)

let get_target_pi pi_target =
  if (String.lowercase pi_target = "xml")
  then
    raise (Query (Parsing (Finfo.parsing_locinfo (),"Process instruction target cannot match ['X' 'x']['M' 'm']['L' 'l']")))
  else
    pi_target

(* Extracts the character encoding *)

let get_xml_encoding_quotes var_s =
  let mid = String.index var_s '\"' in
  let sta = mid+1 in
  let len = (String.length var_s) - sta - 1 in
  String.sub var_s sta len

let get_xml_encoding_single_quotes var_s =
  let mid = String.index var_s '\'' in
  let sta = mid+1 in
  let len = (String.length var_s) - sta - 1 in
  String.sub var_s sta len

(* Extracts character and entity references *)

let get_char_ref       l =
  let len = String.length l in
  String.sub l 2 (len-3)

let get_hexchar_ref    l =
  let len = String.length l in
  String.sub l 3 (len-4)

let get_entity_ref l =
  let len = String.length l in
  String.sub l 1 (len-2)


(* Error messages for the new lexing approach *)

let qname_lexing_error1 li =
  raise (Query (Lexing(li, "1 Found QName while in operator state")))
let qname_lexing_error2 li =
  raise (Query (Lexing(li, "2 Found QName while in operator state")))
let qname_lexing_error3 li =
  raise (Query (Lexing(li, "3 Found QName while in operator state")))
let qname_lexing_error4 li =
  raise (Query (Lexing(li, "4 Found QName while in operator state")))
let qname_lexing_error5 li =
  raise (Query (Lexing(li, "5 Found QName while in operator state")))
let qname_lexing_error li =
  raise (Query (Lexing(li, "0 Found QName while in operator state")))
let qnamesep_lexing_error li =
  raise (Query (Lexing(li, "Found QName without proper separator")))

(* Wrapping functions for keywords per language *)

let wrap_bang f =
  if Conf.is_xquerybang() 
  then f ()
  else raise (Query (Toplevel_Error "XQuery! tokens found, please turn xquerybang option on"))

let wrap_bang_or_p f =
  if Conf.is_xquerybang() || Conf.is_xqueryp() || Conf.is_dxq()
  then f ()
  else raise (Query (Toplevel_Error "XQuery! or XQueryP tokens found, please turn xquerybang or xqueryp option on"))

let wrap_p f =
  if Conf.is_xqueryp() || Conf.is_dxq() 
  then f ()
  else raise (Query (Toplevel_Error "XQueryP tokens found, please turn xqueryp option on"))

let wrap_bang_or_p_or_u f =
  if Conf.is_xquerybang() || Conf.is_xqueryp() || Conf.is_ultf() || Conf.is_dxq() 
  then f ()
  else raise (Query (Toplevel_Error "Updates or XQuery! or XQueryP tokens found, please turn ultf or xquerybang or xqueryp option on"))

(* Lexing transition functions *)

let toop_pushdef lh  = pop_state lh; push_operator lh; push_default lh
let todef lh         = pop_state lh; push_default lh
let toren_pushdef lh = pop_state lh; push_rename_op lh; push_default lh
let toren_todef lh   = pop_state lh; push_rename_op lh; push_default lh
let toop_pushitem lh = pop_state lh; push_operator lh; push_itemtype lh
let toop_pushvar lh  = pop_state lh; push_operator lh; push_varname lh
let tonamespace lh   = pop_state lh; push_namespacedecl lh
let todecl lh        = pop_state lh; push_namespacekeyword lh
let tonamespace lh   = pop_state lh; push_namespacedecl lh
let tocopy lh        = pop_state lh; push_copynamespaces lh
let pushitem lh      = push_itemtype lh
let pushdef lh       = push_default lh
let tonone lh        = ()
let tokinddef lh     =
  if (check_item_type lh)
  then ()
  else
    begin
      pop_state lh;
      if Stack.is_empty lh.current_lex_state then begin pushdef lh end;
      if get_current_lex_state lh = KindTestState
      then begin pop_state lh end
      else begin pushdef lh end
    end
let todefreturn lh   =
  begin
    pop_state lh;
    if Stack.is_empty lh.current_lex_state
    then begin pushdef lh end
    else
      if get_current_lex_state lh = KindTestState
      then begin pop_state lh;pushdef lh end
      else pushdef lh
  end

let update_toop_pushdef lh t  = wrap_bang_or_p_or_u(fun () -> toop_pushdef lh; t)
let update_todef lh t         = wrap_bang_or_p_or_u(fun () -> todef lh; t)
let update_toren_pushdef lh t = wrap_bang_or_p_or_u(fun () -> toren_pushdef lh; t)
let update_toren_todef lh t = wrap_bang_or_p_or_u(fun () -> toren_todef lh; t)
let xquery_todefreturn lh t   = todefreturn lh; t
let xquery_todef lh t         = todefreturn lh; t
let xquery_toop_pushitem lh t = toop_pushitem lh; t
let xquery_toop_pushdef lh t  = toop_pushdef lh; t
let bang_toop_pushdef lh t    = wrap_bang(fun () -> toop_pushdef lh; t)
let bang_todef lh t           = wrap_bang(fun () -> todef lh; t)
let bangp_toop_pushdef lh t   = wrap_bang_or_p(fun () -> toop_pushdef lh; t)
let bangp_todef lh t          = wrap_bang_or_p(fun () -> todef lh; t)
let p_toop_pushdef lh t       = wrap_p(fun () -> toop_pushdef lh; t)
let p_todef lh t              = wrap_p(fun () -> todef lh; t)
let xquery_toop_pushvar lh t  = toop_pushvar lh; t
let xquery_tonamespace lh t   = tonamespace lh; t
let xquery_todecl lh t        = todecl lh; t
let xquery_tonamespace lh t   = tonamespace lh; t
let xquery_tocopy lh t        = tocopy lh; t
let xquery_pushitem lh t      = pushitem lh; t
let xquery_pushdef lh t       = pushdef lh; t
let xquery_none lh t          = t
(* Hack because of conflicts in transitions between rename and type assertions *)
let xquery_as lh t =
  begin
    (* let oldstate = get_current_lex_state lh in *)
    pop_state lh; 
    begin
      try
	if get_current_lex_state lh = RenameOperatorState then
	  begin
	    pop_state lh; 
	    push_default lh
	  end
	else
	  begin
	    push_operator lh;
	    push_itemtype lh
	  end
      with Stack.Empty ->
	begin
	  push_operator lh;
	  push_itemtype lh
	end
    end;
    t
  end

(* Some utilities *)

let reset_buffered lh bt =
  lh.buffered_tokens <- bt;
  lh.buffered <- [||];
  lh.default_token <- false

let cleanup_lex_handler li lh =
  let buffered_string = Array.to_list lh.buffered in
  let new_tokens =
    match buffered_string with
    | [] -> []
    | [x] ->
	let tok =
          match process_qname_string x with
	  | NCNAME_KIND ncname ->
	      NCNAME ncname
	  | QNAME_KIND qname ->
	      QNAME qname
	in
	[tok]
    | _ -> qname_lexing_error2 li
  in
  let new_buffered_tokens = lh.buffered_tokens @ new_tokens in
  reset_buffered lh new_buffered_tokens

let make_new_token li lh move_state tok =
  cleanup_lex_handler li lh;
  move_state lh;
  match lh.buffered_tokens with
  | [] -> Some tok
  | _ -> lh.buffered_tokens <- lh.buffered_tokens @ [tok]; None

(* Keyword tables *)

let operator_keyword_table = [
  (* One word keywords *)
  [|"or"|],(xquery_todef,OR);
  [|"and"|],(xquery_todef,AND);
  [|"in"|],(xquery_todef,IN);
  [|"satisfies"|],(xquery_todef,SATISFIES);
  [|"return"|],(xquery_todef,RETURN);
  [|"then"|],(xquery_todef,THEN);
  [|"else"|],(xquery_todef,ELSE);
  [|"to"|],(xquery_todef,TO);
  [|"where"|],(xquery_todef,WHERE);
  [|"intersect"|],(xquery_todef,INTERSECT);
  [|"union"|],(xquery_todef,UNION);
  [|"except"|],(xquery_todef,EXCEPT);
  [|"case"|],(xquery_toop_pushitem,CASE);
  [|"ascending"|],(xquery_none,ASCENDING);
  [|"descending"|],(xquery_none,DESCENDING);
  [|"empty"|],(xquery_none,EMPTY);
  [|"least"|],(xquery_none,LEAST);
  [|"greatest"|],(xquery_none,GREATEST);
  [|"default"|],(xquery_toop_pushitem,DEFAULT);
  [|"into"|],(xquery_todef,INTO);
  [|"as"|],(xquery_as,AS);
  [|"before"|],(xquery_todef,BEFORE);
  [|"after"|],(xquery_todef,AFTER);
  [|"with"|],(xquery_todef,WITH);
  [|"modify"|],(xquery_todef,MODIFY);
  [|"external"|],(xquery_none,EXTERNAL);
  [|"at"|],(xquery_todef,AT);
  [|"eq"|],(xquery_todef,EQ);
  [|"ne"|],(xquery_todef,NE);
  [|"lt"|],(xquery_todef,LTOP);
  [|"gt"|],(xquery_todef,GTOP);
  [|"le"|],(xquery_todef,LTE);
  [|"ge"|],(xquery_todef,GTE);
  [|"is"|],(xquery_todef,IS);
  [|"div"|],(xquery_todef,DIV);
  [|"implements"|],(xquery_todef,IMPLEMENT); 
  [|"implement"|],(xquery_todef,IMPLEMENT); 
  [|"do"|],(xquery_todef,DO); 
  [|"box"|],(xquery_todef,BOX); 
  [|"idiv"|],(xquery_todef,IDIV);
  [|"mod"|],(xquery_todef,MOD);

  (* long keywords *)
  [|"treat";"as"|],(xquery_toop_pushitem,TREATAS);
  [|"assert";"as"|],(xquery_toop_pushitem,ASSERTAS);
  [|"cast";"as"|],(xquery_toop_pushitem,CASTAS);
  [|"castable";"as"|],(xquery_toop_pushitem,CASTABLEAS);
  [|"instance";"of"|],(xquery_toop_pushitem,INSTANCEOF);
  [|"order";"by"|],(xquery_todef,ORDERBY);
  [|"stable";"order";"by"|],(xquery_todef,STABLEORDERBY);
  [|"for";"$"|],(xquery_toop_pushvar,FORDOLLAR);
  [|"let";"$"|],(xquery_toop_pushvar,LETDOLLAR);
]

let default_keyword_table = [
  (* long keywords *)
  [|"declare";"default";"collation"|],(xquery_tonamespace,DECLAREDEFAULTCOLLATION);
  [|"declare";"namespace"|],(xquery_tonamespace,DECLARENAMESPACE);
  [|"declare";"base-uri"|],(xquery_tonamespace,DECLAREBASEURI);
  [|"module";"namespace"|],(xquery_tonamespace,MODULENAMESPACE);
  [|"interface";"namespace"|],(xquery_tonamespace,INTERFACENAMESPACE);
  [|"declare";"default";"element"|],(xquery_todecl,DECLAREDEFAULTELEMENT);
  [|"declare";"default";"function"|],(xquery_todecl,DECLAREDEFAULTFUNCTION);
  [|"import";"schema"|],(xquery_todecl,IMPORTSCHEMA);
  [|"import";"service"|],(xquery_todecl,IMPORTSERVICE);
  [|"import";"module"|],(xquery_todecl,IMPORTMODULE);
  [|"import";"interface"|],(xquery_todecl,IMPORTINTERFACE);
  [|"declare";"value";"index"|],(xquery_todef,DECLAREVALUEINDEX);
  [|"declare";"name";"index"|],(xquery_todef,DECLARENAMEINDEX);
  [|"declare";"boundary-space"|],(xquery_todecl,DECLAREBOUNDARYSPACE);
  [|"declare";"construction"|],(xquery_todecl,DECLARECONSTRUCTION);
  [|"declare";"ordering"|],(xquery_todecl,DECLAREORDERING);
  [|"declare";"option"|],(xquery_tonamespace,DECLAREOPTION);
  [|"declare";"default";"order"|],(xquery_todecl,DECLAREDEFAULTORDER);
  [|"declare";"copy-namespaces"|],(xquery_tocopy,DECLARECOPYNAMESPACES);
  [|"declare";"function"|],(xquery_todecl,DECLAREFUNCTION);
  [|"declare";"event"|],(xquery_tonamespace,DECLAREFUNCTION);
  [|"declare";"updating";"function"|],(xquery_todecl,DECLAREUPDATINGFUNCTION);
  [|"xquery";"version"|],(xquery_todecl,XQUERYVERSION);
  [|"some";"$"|],(xquery_toop_pushvar,SOMEDOLLAR);
  [|"every";"$"|],(xquery_toop_pushvar,EVERYDOLLAR);
  [|"for";"$"|],(xquery_toop_pushvar,FORDOLLAR);
  [|"let";"$"|],(xquery_toop_pushvar,LETDOLLAR);
  [|"declare";"variable";"$"|],(xquery_toop_pushvar,DECLAREVARIABLE);
  [|"document";"{"|],(xquery_toop_pushdef,DOCUMENTCURLY);
  [|"text";"{"|],(xquery_toop_pushdef,TEXTCURLY);
  [|"processing-instruction";"{"|],(xquery_toop_pushdef,PICURLY);
  [|"comment";"{"|],(xquery_toop_pushdef,COMMENTCURLY);
  [|"element";"{"|],(xquery_toop_pushdef,ELEMENTCURLY);
  [|"attribute";"{"|],(xquery_toop_pushdef,ATTRIBUTECURLY);
  [|"ordered";"{"|],(xquery_toop_pushdef,ORDEREDCURLY);
  [|"unordered";"{"|],(xquery_toop_pushdef,UNORDEREDCURLY);
  [|"validate";"{"|],(xquery_toop_pushdef,VALIDATELCURLY);
  [|"validate";"lax";"{"|],(xquery_toop_pushdef,VALIDATELAXLCURLY);
  [|"validate";"strict";"{"|],(xquery_toop_pushdef,VALIDATESTRICTLCURLY);
  (* update keywords *)
  [|"delete";"node"|],(update_todef,DELETENODE);
  [|"delete";"nodes"|],(update_todef,DELETENODE);
  [|"insert";"node"|],(update_todef,INSERTNODE);
  [|"insert";"nodes"|],(update_todef,INSERTNODE);
  [|"rename";"node"|],(update_toren_todef,RENAMENODE);
  [|"replace";"node"|],(update_todef,REPLACENODE);
  [|"replace";"value";"of";"node"|],(update_todef,REPLACEVALUEOFNODE);
  [|"copy";"$"|],(update_todef,COPYDOLLAR);
  [|"snap";"ordered";"{"|],(bang_toop_pushdef,SNAPLCURLYORDERED);
  [|"copy";"{"|],(bang_toop_pushdef,COPYLCURLY);
  [|"snap";"{"|],(bang_toop_pushdef,SNAPLCURLY);
  (* [|"while"|],(bangp_todef,WHILE); *)
  [|"letvar";"$"|],(bang_todef,LETVARDOLLAR);
  [|"set";"$"|],(bangp_todef,SETDOLLAR);
  [|"declare";"$"|],(p_todef,DECLAREDOLLAR);
  [|"snap";"delete";"node"|],(bang_todef,SNAPDELETENODE);
  [|"snap";"delete";"nodes"|],(bang_todef,SNAPDELETENODE);
  [|"snap";"insert";"node"|],(bang_todef,SNAPINSERTNODE);
  [|"snap";"insert";"nodes"|],(bang_todef,SNAPINSERTNODE);
  [|"snap";"rename";"node"|],(bang_todef,SNAPRENAMENODE);
  [|"snap";"replace";"node"|],(bang_todef,SNAPREPLACENODE);
  [|"snap";"replace";"value";"of";"node"|],(bang_todef,SNAPREPLACEVALUEOFNODE);
  (* DXQ keywords *)
  [|"declare";"server"|],(bangp_todef,DECLARESERVER); 
  [|"let";"server"|],(bangp_todef,LETSERVER); 
  [|"from";"server"|],(bangp_todef,FROMSERVER);
  [|"for";"server"|],(bangp_todef,FORSERVER);
  [|"at";"server"|],(bangp_todef,ATSERVER);
  [|"eval";"box"|],(xquery_todef,EVALCLOSURE); 
]

let namespace_keyword_table = [
  (* One word keywords *)
  [|"namespace"|],(xquery_tonamespace,NAMESPACE);
  [|"encoding"|],(xquery_none,ENCODING);
  [|"empty"|],(xquery_none,EMPTY);
  [|"greatest"|],(xquery_pushdef,GREATEST);
  [|"least"|],(xquery_pushdef,LEAST);
  [|"preserve"|],(xquery_pushdef,PRESERVE);
  [|"strip"|],(xquery_pushdef,STRIP);
  [|"ordered"|],(xquery_pushdef,ORDERED);
  [|"unordered"|],(xquery_pushdef,UNORDERED);
  [|"external"|],(xquery_pushdef,EXTERNAL);
  [|"as"|],(xquery_pushitem,AS);
  [|"at"|],(xquery_none,AT);
  (* long keywords *)
  [|"default";"element"|],(xquery_none,DEFAULTELEMENT);
]

type keyword_match =
  | NoMatch
  | PartialMatch
  | Match of ((lexing_handler -> token -> token) * token)

let hash_of_table table =
  let ht = Hashtbl.create 379 in
  let add_to_hash (keywords,entry) =
    let len = Array.length keywords in
    for i = 0 to len-1 do
      let sub = Array.sub keywords 0 (i+1) in
      let matres =
	if (i = len-1)
	then Match entry
	else PartialMatch
      in
      Hashtbl.add ht sub matres
    done;
  in
  begin
    List.iter add_to_hash table;
    ht
  end

let namespace_keyword_hash = hash_of_table namespace_keyword_table
let default_keyword_hash   = hash_of_table default_keyword_table
let operator_keyword_hash  = hash_of_table operator_keyword_table

let match_in_hash tl sl1 =
  try
    Hashtbl.find tl sl1
  with
  | _ ->
      NoMatch

let find_match_keyword tl sl1 =
  match_in_hash tl sl1

let print_buffered sl =
  Printf.printf "Lexing: ";
  Array.iter (fun x -> Printf.printf "%s;" x) sl;
  Printf.printf "\n"; flush stdout

let rec match_operator_keyword li lh s =
  let sl = Array.append lh.buffered [|s|] in
  (* print_buffered sl; *)
  let actual_table =
    if lh.default_token
    then
      begin
	(* Printf.printf "Using default table\n"; flush stdout; *)
	default_keyword_hash
      end
    else
      begin
	(* Printf.printf "Using operator table\n"; flush stdout; *)
	operator_keyword_hash
      end
  in
  match find_match_keyword actual_table sl with
  | NoMatch ->
      if not(lh.default_token)
      then
	qname_lexing_error1 li
      else
	lh.default_token <- false;
      let buffered_string = Array.to_list lh.buffered in
      begin
	match buffered_string with
	| [] -> qname_lexing_error2 li
	| first :: rest ->
	    let tok =
              match process_qname_string first with
	      | NCNAME_KIND ncname ->
		  NCNAME ncname
	      | QNAME_KIND qname ->
		  QNAME qname
	    in
	    begin
	      lh.buffered_tokens <- (lh.buffered_tokens @ [tok]);
	      lh.buffered <- Array.of_list rest;
	      match match_operator_keyword li lh s with
	      | None -> None
	      | Some x ->
		  begin
		    lh.buffered_tokens <- (lh.buffered_tokens @ [x]);
		    None
		  end
	    end
      end
  | PartialMatch ->
      lh.buffered <- sl;
      None
  | Match (pp,t) ->
      lh.buffered <- [||];
      lh.default_token <- false;
      let tok = pp lh t in
      lh.buffered_tokens <- (lh.buffered_tokens @ [tok]);
      None

let rec match_curly li lh =
  if not(lh.default_token)
  then begin pop_state lh; push_operator lh; push_default lh; Some LCURLY end
  else
    let sl = Array.append lh.buffered [|"{"|] in
    let actual_table = default_keyword_hash in
    match find_match_keyword actual_table sl with
    | NoMatch 
	(* NoMatch is permissible when the "{" follows the "then" in an if-then-else *)
    | PartialMatch ->
	qname_lexing_error3 li
    | Match (pp,t) ->
	lh.buffered <- [||];
	lh.default_token <- false;
	Some (pp lh t)

(* matching a parenthesis in the operator state *)
let rec match_paren li lh =
  if not(lh.default_token)
  then begin todef lh; Some LPAR end
  else
    begin
      let q =
	begin
	  let bf = Array.to_list lh.buffered in
	  lh.buffered <- [||];
	  lh.default_token <- false;
	  match bf with
	  | [q] -> q
	  | _ -> qname_lexing_error4 li
	end
      in
      (* Function calls *)
      (* From the XQuery Spec - Jerome
	 
	 A.3 Reserved Function Names
	 
	 The following names are not recognized as function names in an
	 unprefixed form because expression syntax takes precedence.
	 
       * attribute
       * comment
       * document-node
       * element
       * item
       * if
       * item
       * node
       * processing-instruction
       * schema-attribute
       * schema-element
       * text
       * typeswitch
       * empty-sequence
       *)
      todef lh;
      let actual_qname = Namespace_names.uqname_function_of_string q in
      match actual_qname with
      | (Namespace_names.NSDefaultFunctionPrefix, "attribute") -> Some ATTRIBUTELPAR
      | (Namespace_names.NSDefaultFunctionPrefix, "comment") -> Some COMMENTLPAR
      | (Namespace_names.NSDefaultFunctionPrefix, "document-node") -> Some DOCUMENTNODELPAR
      | (Namespace_names.NSDefaultFunctionPrefix, "element") -> Some ELEMENTLPAR
      | (Namespace_names.NSDefaultFunctionPrefix, "item") -> Some ITEMLPAR
      | (Namespace_names.NSDefaultFunctionPrefix, "if") -> Some IFLPAR
      | (Namespace_names.NSDefaultFunctionPrefix, "while") -> Some WHILELPAR
      | (Namespace_names.NSDefaultFunctionPrefix, "node") -> Some NODELPAR
      | (Namespace_names.NSDefaultFunctionPrefix, "processing-instruction") -> Some PROCESSINGINSTRUCTIONLPAR
      | (Namespace_names.NSDefaultFunctionPrefix, "schema-attribute") -> Some SCHEMAATTRIBUTELPAR
      | (Namespace_names.NSDefaultFunctionPrefix, "schema-element") -> Some SCHEMAELEMENTLPAR
      | (Namespace_names.NSDefaultFunctionPrefix, "text") -> Some TEXTLPAR
      | (Namespace_names.NSDefaultFunctionPrefix, "typeswitch") -> Some TYPESWITCHLPAR
      | (Namespace_names.NSDefaultFunctionPrefix, "empty-sequence") -> Some EMPTYSEQUENCELPAR
      | _ -> Some (FUNCTIONNAMELPAR actual_qname)
    end


let match_default_keyword li lh s =
  let sl = Array.append lh.buffered [|s|] in
  match find_match_keyword default_keyword_hash sl with
  | NoMatch
  | PartialMatch ->
      begin
	lh.default_token <- true;
	lh.buffered <- sl;
	None
      end
  | Match (pp,t) ->
      begin
	lh.default_token <- false;
	lh.buffered <- [||];
	Some (pp lh t)
      end

let match_namespace_keyword li lh s =
  let sl = Array.append lh.buffered [|s|] in
  match find_match_keyword namespace_keyword_hash sl with
  | NoMatch ->
      lh.default_token <- false;
      lh.buffered <- [||];
      qname_lexing_error5 li
  | PartialMatch ->
      lh.default_token <- false;
      lh.buffered <- sl;
      None
  | Match (pp,t) ->
      lh.default_token <- false;
      lh.buffered <- [||];
      Some (pp lh t)

let get_buffered_tokens lh =
  lh.buffered_tokens

let set_buffered_tokens lh ts =
  lh.buffered_tokens <- ts

let default_token lh =
  lh.default_token

let get_buffered lh =
  Array.to_list lh.buffered

let set_buffered lh sl =
  lh.buffered <- (Array.of_list sl)

