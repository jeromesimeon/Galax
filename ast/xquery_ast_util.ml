(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_ast_util.ml,v 1.47 2007/09/25 15:12:42 mff Exp $ *)

(* Module: Xquery_astutil
   Description:
     This module implements some useful operations the XQuery AST.
*)

open Finfo

open Namespace_names
open Namespace_builtin

open Xquery_common_ast
open Xquery_common_ast_util

open Xquery_type_ast
open Xquery_ast

open Error


(**************************)
(* AST creation functions *)
(**************************)

(* Sequence type AST creation *)

let mksequencetype d =
  { psequencetype_desc = d;
    psequencetype_loc = parsing_locinfo () }

let fmksequencetype d l =
  { psequencetype_desc = d;
    psequencetype_loc = l }


(* Common data structures AST creation *)

let mkpattern d =
  { ppattern_desc = d;
    ppattern_loc = parsing_locinfo () }

let fmkpattern d l =
  { ppattern_desc = d;
    ppattern_loc = l }

(* XQuery AST creation *)

let mkexpr d =
  { pexpr_desc = d;
    pexpr_loc = parsing_locinfo () }

let mkstep_qualifier d =
  { pstep_qualifier_desc = d;
    pstep_qualifier_loc = parsing_locinfo () }

let mkfl_expr d =
  { pfl_desc = d;
    pfl_loc = parsing_locinfo () }

let mkcopyvar_expr d =
  { pcopyvar_desc =  d;
    pcopyvar_loc = parsing_locinfo() }

let mkfunction_def d =
  { pfunction_def_desc = d;
    pfunction_loc = parsing_locinfo () }

let mkvar_decl d =
  { pvar_decl_desc = d;
    pvar_decl_loc = parsing_locinfo () }

let mkserver_decl d =
  { pserver_decl_desc = d;
    pserver_decl_loc = parsing_locinfo () }

let mkindex_def d =
  { pindex_def_desc = d;
    pindex_def_loc = parsing_locinfo () }

let mkcontext_decl d =
  { pcontext_decl_desc = d;
    pcontext_decl_loc = parsing_locinfo () }

let mkblock_decl_expr d =
  { bl_decl_desc = d;
    bl_decl_loc = parsing_locinfo () }


let fmkexpr d l =
  { pexpr_desc = d;
    pexpr_loc = l }

let fmkstep_qualifier d l =
  { pstep_qualifier_desc = d;
    pstep_qualifier_loc = l }

let fmkfl_expr d l =
  { pfl_desc = d;
    pfl_loc = l }

let fmkcopyvar_expr d l =
  { pcopyvar_desc = d;
    pcopyvar_loc = l }

let fmkfunction_def d l =
  { pfunction_def_desc = d;
    pfunction_loc = l }

let fmkvar_decl d l =
  { pvar_decl_desc = d;
    pvar_decl_loc = l }

let fmkserver_decl d l =
  { pserver_decl_desc = d;
    pserver_decl_loc = l }

let fmkindex_def d l =
  { pindex_def_desc = d;
    pindex_def_loc = l }

let fmkcontext_decl d l =
  { pcontext_decl_desc = d;
    pcontext_decl_loc = l }


let fmkblock_decl_expr d l =
  { bl_decl_desc = d;
    bl_decl_loc = l }

(**************************)
(* AST accessor functions *)
(**************************)

(* Note:
     Takes an attribute symbol and expression computing that attribute
     and returns the corresponding new prefix/namespace binding or fails
   - Jerome
 *)

let extract_text_from_expression_list el =
  match el with
  | [] -> ""
  | [e1] ->
      begin
	match e1.pexpr_desc with
	| EText t -> t
	| _ ->
	    raise (Query (Malformed_Expr "complex expression in attribute"))
      end
  | _ ->
      raise (Query (Malformed_Expr "many expressions in attribute"))

(* XQuery Section 3.7.1.2 Namespace Declaration Attributes

   [Definition: A namespace declaration attribute is used inside a
   direct element constructor. Its purpose is to bind a namespace
   prefix or to set the default element/type namespace for the
   constructed element node, including its attributes.] Syntactically,
   a namespace declaration attribute has the form of an attribute with
   namespace prefix xmlns, or with name xmlns and no namespace
   prefix. The value of a namespace declaration attribute must be a
   URILiteral; otherwise a static error is raised [err:XQST0022]. All
   the namespace declaration attributes of a given element must have
   distinct names [err:XQST0071]. Each namespace declaration attribute
   is processed as follows:

    * The local part of the attribute name is interpreted as a
      namespace prefix and the value of the attribute is interpreted
      as a namespace URI. This prefix and URI are added to the
      statically known namespaces of the constructor expression
      (overriding any existing binding of the given prefix), and are
      also added as a namespace binding to the in-scope namespaces of
      the constructed element. If the namespace URI is a zero-length
      string and the implementation supports [XML Names 1.1], any
      existing namespace binding for the given prefix is removed from
      the in-scope namespaces of the constructed element and from the
      statically known namespaces of the constructor expression. If
      the namespace URI is a zero-length string and the implementation
      does not support [XML Names 1.1], a static error is raised
      [err:XQST0085]. It is implementation-defined whether an
      implementation supports [XML Names] or [XML Names 1.1].

    * It is a static error [err:XQST0070] if a namespace declaration
      attribute binds a namespace URI to the predefined prefix xml or
      xmlns, or binds a prefix other than xml to the namespace URI
      http://www.w3.org/XML/1998/namespace.
*)

let filter_xml_ns att =
  match 

let extract_ns_att s el =
  match s with
  | (prefix, localname) ->
      let ns_prefix =
	match prefix with
	| (NSPrefix "xmlns") ->
	    let ln = String.lowercase localname in
	    if (ln = "xml" || ln = "xmlns") then 
	      raise (Query (Namespace_Error ("Cannot redefine namespace prefix 'xml' or 'xmlns'")))
	    else (NSPrefix localname)
	| NSDefaultElementPrefix ->
	    if localname = "xmlns"
	    then NSDefaultElementPrefix
	    else raise Not_found
	| _ ->
	    raise Not_found
      in
      let t = extract_text_from_expression_list el in
      let uri =
	try
	  (* Hack for now! - Jerome *)
	  ignore(Datatypes_util.anyURI_of_untyped t);
	  if (t = Conf.xmlns) && ((Namespace_names.string_of_prefix ns_prefix) != "xml") then 
	    raise (Query (Namespace_Error ("Reserved URI "^Conf.xmlns^" must be bound to prefix 'xml'")))
	  else NSUri t
	with
	| _ ->
	    raise (Query (Mapping_Failure ("Namespace attribute 'xmlns' does not contain URI value")))
      in
      (ns_prefix, uri)

(* Extract all the attribute namespaces *)

let rec get_ns_attributes_aux att_el =
  match att_el with
  | [] -> ([],[])
  | att_e :: att_el' ->
      let (new_nss', other_att') = get_ns_attributes_aux att_el' in
      match att_e.pexpr_desc with
      | EAttrFixed (s, elist) ->
	  begin
	    try
	      ((extract_ns_att s elist) :: new_nss', other_att')
	    with
	    | Not_found ->
		(new_nss', (att_e :: other_att'))
	  end
      |	_ ->
	  raise (Query (Malformed_Expr("Invalid attribute in element")))

let get_ns_attributes att_el =
  let (ns_list, other_atts) = get_ns_attributes_aux att_el in
  let sorted_prefixes = 
    List.sort 
      (fun (p1,_) (p2,_) -> compare (Namespace_names.string_of_prefix p1) (Namespace_names.string_of_prefix p2)) 
      ns_list 
  in
  let rec dup_prefixes prefix_list = 
    match prefix_list with 
    | (p1, u1) :: (p2, u2) :: rest -> 
	if (Namespace_names.string_of_prefix p1) = (Namespace_names.string_of_prefix p2) then
	  raise (Query (Namespace_Error ("Duplicate namespace attribute declaration for '"^(Namespace_names.string_of_prefix p1)^"'")))
	else 
	  (p1, u1) :: (dup_prefixes ((p2, u2) :: rest))
    | rest -> rest
  in (dup_prefixes sorted_prefixes, other_atts)

(*****************************)
(* Module related operations *)
(*****************************)

(* The empty module *)

let empty_prolog () = 
  { pprolog_xschemas = [];
    pprolog_contexts = [];
    pprolog_funcvars = [];
    pprolog_indices = [] }

let empty_interface ncn uri = 
  { pinterface_decl = (ncn, uri);
    pinterface_prolog = 
    { iprolog_xschemas = [];
      iprolog_contexts = [];
      iprolog_funcvars = [] }
  } 

let empty_library_module ncn uri = 
  { plibrary_module_decl = (ncn, uri, None);
    plibrary_module_prolog = empty_prolog (); 
  } 

let merge_prologs p1 p2 =
  { pprolog_xschemas  = p1.pprolog_xschemas @ p2.pprolog_xschemas;
    pprolog_contexts  = p1.pprolog_contexts @ p2.pprolog_contexts;
    pprolog_funcvars  = p1.pprolog_funcvars @ p2.pprolog_funcvars;
    pprolog_indices   = p1.pprolog_indices @ p2.pprolog_indices }

let merge_interface_prologs p1 p2 =
  { iprolog_xschemas  = p1.iprolog_xschemas @ p2.iprolog_xschemas;
    iprolog_contexts  = p1.iprolog_contexts @ p2.iprolog_contexts;
    iprolog_funcvars  = p1.iprolog_funcvars @ p2.iprolog_funcvars }

let merge_library_module_in_prolog uri prolog libmod =
  let (ncname, uri', _) = libmod.plibrary_module_decl in
  if not(NSUri uri' = uri)
  then
    raise (Query (Module_Import ("Target namespace of imported module does not corresponds to target namespace of the module resource")))
  else
    merge_prologs prolog libmod.plibrary_module_prolog

let merge_library_modules libmod1 libmod2 =
  let (_, uri1, _) = libmod1.plibrary_module_decl in
  let (_, uri2, _) = libmod2.plibrary_module_decl in
  if not(uri1 = uri2)
  then
    raise (Query (Module_Import ("Target namespace of imported module does not corresponds to target namespace of the module resource")))
  else
    { plibrary_module_decl = libmod2.plibrary_module_decl;
      plibrary_module_prolog =  merge_prologs libmod1.plibrary_module_prolog libmod2.plibrary_module_prolog;
    }

let merge_interfaces interface1 interface2 =
  if not(snd(interface1.pinterface_decl) = snd(interface2.pinterface_decl))
  then
    raise (Query (Module_Import ("Target namespace of interfaces do not match")));
  { pinterface_decl = interface2.pinterface_decl; 
    pinterface_prolog = merge_interface_prologs interface1.pinterface_prolog interface2.pinterface_prolog;
  } 

(* Merging prologs just concatenates declarations --- could lead to conflicts  *)
let merge_prologs_with_decls decls p1 p2 =
  { pprolog_xschemas  = p1.pprolog_xschemas @ p2.pprolog_xschemas;
    pprolog_contexts  = p1.pprolog_contexts @ decls;  
    pprolog_funcvars  = p1.pprolog_funcvars @ p2.pprolog_funcvars;
    pprolog_indices   = p1.pprolog_indices @ p2.pprolog_indices }

let split_main_module xmod =
  (xmod.pmain_module_prolog,xmod.pmain_module_statements)

(* Remove boundary whitespace, in case XQuery whitespace handling is
   set to 'strip' *)

let is_boundary_stuff oce =
  match oce with
  | None -> true
  | Some ce ->
      begin
	match ce.pexpr_desc with
	| EEnclosed _ | EElemFixed _ -> true
	| _ -> false
      end

let remove_boundary_whitespace ce1 ce2 ce3 =
  if (is_boundary_stuff ce1) && (is_boundary_stuff ce3)
  then
    match ce2.pexpr_desc with
    | (EText s) ->
	if (Whitespace.whitespace_only s)
	then fmkexpr (EText "") ce2.pexpr_loc
	else ce2
    | _ -> ce2
  else ce2

let rec remove_boundary_whitespace_from_children_aux prev celist =
  match celist with
  | [] -> []
  | ce1 :: [] ->
      remove_boundary_whitespace prev ce1 None :: []
  | ce1 :: ce2 :: celist' ->
      (remove_boundary_whitespace prev ce1 (Some ce2)) :: (remove_boundary_whitespace_from_children_aux (Some ce1) (ce2 :: celist'))

let remove_boundary_whitespace_from_children elist =
  remove_boundary_whitespace_from_children_aux None elist

let rec get_functions x =
  match x with
  | [] -> []
  | FunDef x :: x' -> (x :: (get_functions x'))
  | OptionDecl _ :: x'
  | VarDef _ :: x' -> get_functions x'
  | ServerDef _ :: x' -> get_functions x'

let rec get_vars x =
  match x with
  | [] -> []
  | OptionDecl _ :: x'
  | FunDef _ :: x' -> (get_vars x')
  | VarDef x :: x' -> (x :: (get_vars x'))
  | ServerDef _ :: x' -> (get_vars x')

