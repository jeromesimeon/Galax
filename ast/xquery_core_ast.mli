(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_core_ast.mli,v 1.78 2007/11/16 21:16:52 mff Exp $ *)

(* Module Xquery_core_ast
   Description:
     This *interface* contains type declarations for the core
     abstract syntax tree.
*)

open Namespace_names

open Xquery_common_ast
open Xquery_core_ast_annotation
open Xquery_type_core_ast

(**************)
(* Node kinds *)
(**************)

type celement_test =
  | CSchemaElementTest of cename                        (* A globally declared schema element *)
  | CElementTest of (cename * ctname option) option     (* Any element w/ given name and type *)

type cattribute_test =
  | CSchemaAttributeTest of caname
  | CAttributeTest of (caname * ctname option) option

type ckind_test =
  | CDocumentKind of celement_test option    (* ./document-node() *)
  | CElementKind of celement_test            (* ./element() *)
  | CAttributeKind of cattribute_test        (* ./attribute() *)
  | CPIKind of string option  	 	     (* ./pi("xxx") *)
  | CCommentKind              	 	     (* ./comment () *)
  | CTextKind                 	 	     (* ./text() *)
  | CAnyKind                  	 	     (* ./node() *)


(******************)
(* Sequence types *)
(******************)

type citemtype = 
  | CITKindTest of ckind_test
  | CITTypeRef of ctname                          (* type(TName) *)
  | CITItem
  | CITNumeric
  | CITAnyString
  | CITAtomic of ctname
  | CITEmpty

type csequencetype_desc = citemtype * (Occurrence.occurrence_indicator) option

type csequencetype = 
    { pcsequencetype_desc: csequencetype_desc;
      pcsequencetype_loc: Finfo.finfo }

type cfunction_signature = (csequencetype * cxtype) list * (csequencetype * cxtype)

(*************************)
(* Expression components *)
(*************************)

(* Used in typeswitch *)

type cpattern =
    { pcpattern_desc: cpattern_desc;
      pcpattern_loc: Finfo.finfo }

and cpattern_desc =
  | CCase of (csequencetype * cxtype)
  | CDefault


(* Used in path expressions *)

type cnode_test =
  | CPNameTest of rqname
	(* As with SequenceTypes, we pair the kind test with its normalized core type *)
  | CPNodeKindTest of (ckind_test * cxtype)


(********************) (* Core Expressions *) (********************)

(* Core AST annotations are mutable records.  Currently, they contain
   type annotations and XPath annotations. *)

type overloaded_signature_table = (cfname * cfunction_signature * acfunction_body_kind * updating_modifier) list

and acexpr =
    { pcexpr_desc  	    : acexpr_desc;            (* expression *)
      pcexpr_annot 	    : ast_annot;              (* Annotation *)
      mutable pcexpr_origin : Xquery_ast.expr_handle; (* Handle to the original expr *)
      pcexpr_loc            : Finfo.finfo }           (* File location *)

and acinsert_location =
  | CUAsLastInto of acexpr
  | CUAsFirstInto of acexpr
  | CUInto of acexpr
  | CUAfter of acexpr
  | CUBefore of acexpr

and acexpr_desc =
  | CEUnordered of acexpr
  | CEOrdered of acexpr
  | CEFLWOR of acfl_expr list * acexpr option * acorder_by option * acexpr
  | CEIf of acexpr * acexpr * acexpr
  | CETypeswitch of acexpr * (cpattern * cvname option * acexpr) list
  | CEVar of cvname
    (* 
       A function call includes a list of the argument types that must
       be matched dynamically.  For now, they include (csequencetype *
       cxtype)s, because the dynamic type matching is on values and
       sequencetypes.  When type matching is implemented, they should
       be changed to cmodels only.
    *)
  | CECall of 
      cfname * (acexpr list) * ((csequencetype * cxtype) option list * (csequencetype * cxtype)) 
	* updating_modifier * (* self-recursive *) bool
  | CEOverloadedCall of cfname * (acexpr list) * overloaded_signature_table
  | CEScalar of literal
  | CESeq of acexpr * acexpr
  | CEEmpty
  | CEDocument of acexpr
  | CEPI of ncname * string
  | CEPIComputed of acexpr * acexpr
  | CEComment of string
  | CECommentComputed of acexpr
  | CEText of string
  | CECharRef of int
  | CETextComputed of acexpr
    (* The content of direct elements and attributes must be handled
       differently than an arbitrary sequence, so we preserve every
       input expression *)
  | CEElem of cename * Namespace_context.nsenv * (acexpr list)
  | CEAnyElem of acexpr * Namespace_context.nsenv * Namespace_context.nsenv * acexpr (* static/in-scope namespaces *)
  | CEAttr of caname * (acexpr list)
  | CEAnyAttr of acexpr * Namespace_context.nsenv * acexpr
  | CEError of acexpr list
  | CETreat of acexpr * (csequencetype * cxtype)
  | CEValidate of (validation_mode * acexpr)
  | CECast of acexpr * Namespace_context.nsenv * (csequencetype * cxtype)
  | CECastable of acexpr * Namespace_context.nsenv * (csequencetype * cxtype)
  | CEForwardAxis of cvname * axis * cnode_test
  | CEReverseAxis of cvname * axis * cnode_test
  | CESome of (csequencetype * cxtype) option * cvname * acexpr * acexpr   (* Existential quantification *)
  | CEEvery of (csequencetype * cxtype) option * cvname * acexpr * acexpr  (* Universal quantification *)
  (* DXQ expressions *)
  | CELetServerImplement of ncname * string * acexpr * acexpr (* let server NCName implement URI at Expr return Expr *)
  | CEExecute of bool * ncname * string * acexpr * acexpr (* Remote execution *)
  | CEForServerClose of ncname * string * acexpr  (* for server NCName implement URI box Expr *)
  | CEEvalClosure of acexpr
  (* Update-related expressions *)
  | CECopy of acexpr
  (* Update expressions *)
  | CEDelete of acexpr
  | CEInsert of acexpr * acinsert_location
  | CERename of Namespace_context.nsenv * acexpr * acexpr
  | CEReplace of value_of_flag * acexpr * acexpr
  | CESnap of snap_modifier * acexpr 
  | CEProtoValue of Datatypes.atomic_type
  | CELetvar of (csequencetype * cxtype) option * cvname * acexpr * acexpr
  | CESet of cvname * acexpr
  | CEImperativeSeq of acexpr * acexpr
  | CEWhile of acexpr * acexpr

and acfl_expr =
  | CELET of (csequencetype * cxtype) option * cvname * acexpr
  | CEFOR of (csequencetype * cxtype) option * cvname * cvname option * acexpr

and acorder_by =
    stablekind * acorder_spec list * overloaded_signature_table

and acorder_spec = acexpr * sortkind * emptysortkind


(******************)
(* Core toplevels *)
(******************)

(* Functions definition *)

and acfunction_body =
  (* Mary: We need to distinguish between functions that are External and BltIn *)
  | CEFunctionBltIn
  | CEFunctionInterface
  | CEFunctionImported
  | CEFunctionUser of acexpr

and acfunction_body_kind =
  | CEFunctionBltInKind
  | CEFunctionInterfaceKind
  | CEFunctionImportedKind
  | CEFunctionUserKind

type acfunction_def =
    { pcfunction_def_desc : acfunction_def_desc;
      pcfunction_def_loc  : Finfo.finfo }

and acfunction_def_desc = (cfname * int) * (cvname list) * cfunction_signature * acfunction_body * updating_modifier

(* Global variable declaration *)

type acvar_body =
  | CEVarExternal       (* Variable defined in external environment *)
  | CEVarInterface      (* Interface of a variable *)
  | CEVarImported       (* Imported variable *)
  | CEVarUser of acexpr (* Variable defined in module *)

type acvar_decl =
    { pcvar_decl_desc : acvar_decl_desc;
      pcvar_decl_loc  : Finfo.finfo }

and acvar_decl_desc = cvname * (csequencetype * cxtype) option * (acvar_body)

(* Global server declaration *)

type acserver_decl =
    { pcserver_decl_desc : acserver_decl_desc;
      pcserver_decl_loc  : Finfo.finfo }

and acserver_decl_desc = ncname * string * acexpr

(* Key definitions *)

type acindex_def =
    { pcindex_def_desc : acindex_def_desc;
      pcindex_def_loc  : Finfo.finfo }

and acindex_def_desc =
  | CValueIndex of string * acexpr * acexpr
  | CNameIndex of cename

(* Statements *)

(* NOTE: As of 10/19/2004, there is no distinction between expressions
   and updates in the core. - Jerome *)

type acstatement = acexpr

(* Core Query module *)

type acprolog =
    { pcprolog_functions : acfunction_def list;
      pcprolog_vars      : acvar_decl list;
      pcprolog_servers   : acserver_decl list;
      pcprolog_indices   : acindex_def list }

type acxmodule =
    { pcmodule_prolog      : acprolog;
      pcmodule_statements  : acstatement list }

type acinterface = acprolog

