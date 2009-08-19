(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: norm_ident_top.mli,v 1.4 2007/05/16 15:32:11 mff Exp $ *)

(* Module: Norm_ident_top
   Description:
     This module implements an identity normalization for XQuery
     modules which are already in the core.
*)

open Xquery_ast
open Xquery_core_ast

open Norm_context
open Processing_context

val preprocess_ident_prolog         : processing_context -> prolog ->  
  (norm_interface_table * Xquery_ast.library_module list)
val preprocess_ident_library_module : processing_context -> library_module -> 
  (norm_interface_table * Xquery_ast.library_module list)
val preprocess_ident_main_module    : processing_context -> main_module -> 
  (norm_interface_table * Xquery_ast.library_module list)

(* Normalization functions for:
     o individual statements
     o library module
     o main module 
*)

val normalize_ident_statement      :
    norm_context -> statement -> acstatement
val normalize_ident_prolog         :
    (norm_interface_table * norm_context) -> prolog -> (norm_context * acprolog)
val normalize_ident_library_module :
    (norm_interface_table * norm_context) -> library_module -> (norm_context * acxmodule)
val normalize_ident_main_module    :
    (norm_interface_table * norm_context) -> main_module -> (norm_context * acxmodule)

val normalize_ident_module         : (norm_interface_table * norm_context) -> xmodule -> (norm_context * acxmodule)

