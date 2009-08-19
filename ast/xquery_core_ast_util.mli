(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_core_ast_util.mli,v 1.30 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Xquery_core_ast_util
   Description:
     This module implements some useful operations the XQuery core
     AST.
*)

open Xquery_common_ast
open Xquery_core_ast
open Xquery_core_ast_annotation

(*********************)
(* Core AST creation *)
(*********************)

(* Core typeswitch patterns *)

val mkcpattern : cpattern_desc -> cpattern
val fmkcpattern : cpattern_desc -> Finfo.finfo -> cpattern

(* Core sequence types *)

val mkcsequencetype : csequencetype_desc -> csequencetype
val fmkcsequencetype : csequencetype_desc -> Finfo.finfo -> csequencetype

(* Core expressions *)

val fmkcexpr : acexpr_desc -> Xquery_ast.expr_handle -> Finfo.finfo -> acexpr
val fmkacexpr : acexpr_desc -> ast_annot -> Xquery_ast.expr_handle -> Finfo.finfo -> acexpr

val fmkcfunction_def : acfunction_def_desc -> Finfo.finfo -> acfunction_def
val fmkcvar_decl     : acvar_decl_desc     -> Finfo.finfo -> acvar_decl
(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_core_ast_util.mli,v 1.30 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Xquery_core_ast_util
   Description:
     This module implements some useful operations the XQuery core
     AST.
*)

open Xquery_common_ast
open Xquery_core_ast
open Xquery_core_ast_annotation

(*********************)
(* Core AST creation *)
(*********************)

(* Core typeswitch patterns *)

val mkcpattern : cpattern_desc -> cpattern
val fmkcpattern : cpattern_desc -> Finfo.finfo -> cpattern

(* Core sequence types *)

val mkcsequencetype : csequencetype_desc -> csequencetype
val fmkcsequencetype : csequencetype_desc -> Finfo.finfo -> csequencetype

(* Core expressions *)

val fmkcexpr : acexpr_desc -> Xquery_ast.expr_handle -> Finfo.finfo -> acexpr
val fmkacexpr : acexpr_desc -> ast_annot -> Xquery_ast.expr_handle -> Finfo.finfo -> acexpr

val fmkcfunction_def : acfunction_def_desc -> Finfo.finfo -> acfunction_def
val fmkcvar_decl     : acvar_decl_desc     -> Finfo.finfo -> acvar_decl
val fmkcserver_decl  : acserver_decl_desc  -> Finfo.finfo -> acserver_decl
val fmkcindex_def    : acindex_def_desc    -> Finfo.finfo -> acindex_def

(* Core prolog AST creation *)

val fmkcprolog :
    acfunction_def list ->
      acvar_decl list ->
	acindex_def list -> acprolog

(* Core module AST creation *)

val fmkcmodule_from_library_module :
    acxmodule -> acstatement list -> acxmodule


(************************************)
(* Returns the core expression kind *)
(************************************)

(* Note: This is used for monitoring
   - Jerome *)

type cexpr_kind =
  | CEKUnordered
  | CEKOrdered
  | CEKFLWOR
  | CEKOrderBy (* "Temporary" OrderBy core expression -- Never exposed between processing phases *)
  | CEKIf
  | CEKWhile
  | CEKTypeswitch
  | CEKVar
  | CEKScalar
  | CEKProtoValue
  | CEKDocument
  | CEKPI
  | CEKPIComputed
  | CEKComment
  | CEKCommentComputed
  | CEKText
  | CEKCharRef
  | CEKTextComputed
  | CEKCall
  | CEKOverloadedCall
  | CEKSeq
  | CEKEmpty
  | CEKElem
  | CEKAnyElem
  | CEKAttr
  | CEKAnyAttr
  | CEKError
  | CEKTreat
  | CEKValidate
  | CEKCast
  | CEKCastable
  | CEKAxis
  | CEKSome
  | CEKEvery
  | CEKLetServerImplement
  | CEKExecute
  | CEKForServerClose
  | CEKEvalClosure
  | CEKCopy
  | CEKDelete
  | CEKDetach
  | CEKInsert
  | CEKRename
  | CEKReplace
  | CEKSnap
  | CEKLetvar
  | CEKSet
  | CEKImperativeSeq

val get_cexpr_kind : acexpr -> cexpr_kind


(*****************************)
(* Module related operations *)
(*****************************)

val merge_cmodules           : acxmodule -> acxmodule -> acxmodule


(*****************************************)
(* Some macros used during normalization *)
(*****************************************)

(* Builds a variable expression from its name *)

val mkcvar : cvname -> Xquery_ast.expr_handle -> Finfo.finfo -> acexpr

(* Generate a fresh variable name and a corresponding variable
   expression in the 'fs:' namespace *)

val cexpr_fs_dot      : Xquery_ast.expr_handle -> Finfo.finfo -> acexpr
val cexpr_fs_sequence : Xquery_ast.expr_handle -> Finfo.finfo -> acexpr
val cexpr_fs_position : Xquery_ast.expr_handle -> Finfo.finfo -> acexpr
val cexpr_fs_last     : Xquery_ast.expr_handle -> Finfo.finfo -> acexpr

(* Core expression for () *)

val cexpr_empty : Xquery_ast.expr_handle -> Finfo.finfo -> acexpr

(* Adding 'parenthesises right associatively: l = [e1; e2; e3 ...; en]
   -> (e1 (e2 (e3 (... en)...) *)

val map_to_sequence : Xquery_ast.expr_handle -> Finfo.finfo -> acexpr list -> acexpr
val map_to_imperative_sequence : Xquery_ast.expr_handle -> Finfo.finfo -> acexpr list -> acexpr

(* Extract all variable bindings from a FLWR expressions *)
(* Note: This is used to build the tuple in a FLWR with order by clause.
   - Jerome *)

val all_cflwr_bindings : acfl_expr list -> cvname list

(* Creates a FLWOR expression only with let bindings.  This is used at
   different places in the code, notably when doing rewriting and
   factorization. If the list of input let bindings is empty, returns
   the return expression 'as is' - Jerome *)
val make_let_flwor : acfl_expr list
  -> acexpr
    -> ast_annot
      -> Xquery_ast.expr_handle
	-> Finfo.finfo
	  -> acexpr

val make_for_flwor : acfl_expr list
  -> acexpr
    -> ast_annot
      -> Xquery_ast.expr_handle
	-> Finfo.finfo
	  -> acexpr

(* Remove empty text nodes at head and tail of expression list *)

val remove_empty_textnodes : acexpr list -> acexpr list


(* Some operations on core FLWOR expressions *)

val get_first_fl_clause : acexpr -> acfl_expr option * acexpr

val add_first_fl_clause : acfl_expr -> acexpr -> acexpr

val build_flwor_from_fl_clauses : acfl_expr list -> acexpr -> acexpr


(*************************)
(* Access to annotations *)
(*************************)

val set_annotation_for_cexpr : acexpr -> ast_annot -> unit

val get_type_annotation_from_cexpr : acexpr -> Xquery_type_core_ast.cxtype
val set_type_annotation_for_cexpr :  acexpr -> Xquery_type_core_ast.cxtype -> unit


val get_expr_from_insert_location : acinsert_location -> acexpr

(* added by Philippe for determining max_one property *)
val has_max_one : acexpr -> bool
