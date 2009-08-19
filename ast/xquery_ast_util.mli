(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_ast_util.mli,v 1.29 2007/09/19 20:01:50 mff Exp $ *)

(* Module: Xquery_astutil
   Description:
     This module implements some useful operations the XQuery AST.
*)

open Xquery_common_ast
open Xquery_type_ast
open Xquery_ast
open Xquery_algebra_ast


(**************************)
(* AST creation functions *)
(**************************)

(* Sequence type AST creation *)

val mksequencetype : sequencetype_desc -> sequencetype
val fmksequencetype : sequencetype_desc -> Finfo.finfo -> sequencetype

(* Typeswitch pattern AST creation *)

val mkpattern : pattern_desc -> pattern
val fmkpattern : pattern_desc -> Finfo.finfo -> pattern

(* XQuery AST creation *)

val mkexpr : expr_desc -> expr
val fmkexpr : expr_desc -> Finfo.finfo -> expr

val mkstep_qualifier : step_qualifier_desc -> step_qualifier
val fmkstep_qualifier : step_qualifier_desc -> Finfo.finfo -> step_qualifier

val mkfl_expr : fl_expr_desc -> fl_expr
val fmkfl_expr : fl_expr_desc -> Finfo.finfo -> fl_expr

val mkcopyvar_expr : copyvar_expr_desc -> copyvar_expr
val fmkcopyvar_expr : copyvar_expr_desc -> Finfo.finfo -> copyvar_expr

(* XQuery prolog AST creation *)

val mkfunction_def : function_def_desc -> function_def
val fmkfunction_def : function_def_desc -> Finfo.finfo -> function_def

val mkvar_decl  : var_decl_desc -> var_decl
val fmkvar_decl : var_decl_desc -> Finfo.finfo -> var_decl

val mkserver_decl  : server_decl_desc -> server_decl
val fmkserver_decl : server_decl_desc -> Finfo.finfo -> server_decl

val mkindex_def  : index_def_desc -> index_def
val fmkindex_def : index_def_desc -> Finfo.finfo -> index_def

val mkcontext_decl : context_decl_desc -> context_decl
val fmkcontext_decl : context_decl_desc -> Finfo.finfo -> context_decl

val mkblock_decl_expr: block_decl_desc -> block_decl_expr
val fmkblock_decl_expr: block_decl_desc -> Finfo.finfo -> block_decl_expr 

(**************************)
(* AST accessor functions *)
(**************************)

(* Processes NS attributes of an element *)

val get_ns_attributes               : expr list -> ((Namespace_names.prefix * Namespace_names.uri) list * expr list)


(*****************************)
(* Module related operations *)
(*****************************)

(* Empty program components *)

val empty_prolog : unit -> prolog
val empty_interface : Namespace_names.ncname -> string -> interface
val empty_library_module : Namespace_names.ncname -> string -> library_module

(* Merging program components *)

val merge_library_module_in_prolog : Namespace_names.uri -> prolog -> library_module -> prolog
val merge_library_modules :  library_module -> library_module -> library_module
val merge_prologs_with_decls : context_decl list -> prolog -> prolog -> prolog
val merge_interfaces :  interface -> interface -> interface

(* Extracting the statements from a given module *)

val split_main_module : main_module -> prolog * statement list

(* Remove boundary whitespace, in case XQuery whitespace handling is
   set to 'strip' *)

val remove_boundary_whitespace_from_children : expr list -> expr list

val get_functions : funcvar_def list -> function_def list
val get_vars      : funcvar_def list -> var_decl list

