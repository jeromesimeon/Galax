(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: norm_top.mli,v 1.14 2007/09/19 20:01:50 mff Exp $ *)

(* Module: Norm_top
   Description:
     This module implements the normalization phase for XQuery
     modules.
*)

open Xquery_ast
open Xquery_core_ast

open Norm_context
open Processing_context

(* Module pre-processing

   Return transitive closure of imported modules: 

   A table of (URI, module-location, Core interface) tuples, and 
   a list of module-definitions.

   Raise an error if no location is specified for a module, e.g., as
   an explicit hint or in the external location hints.

   Raise an error if a circularity exists in a chain of imported modules. 
*)

val aux_preprocess_prolog : processing_context -> string list -> prolog -> 
  ((Namespace_names.prefix * string) * string list option * Xquery_ast.interface) list * Xquery_ast.library_module list

val preprocess_prolog         : processing_context -> prolog ->  
  (norm_interface_table * Xquery_ast.library_module list)
val preprocess_library_module : processing_context -> library_module -> 
  (norm_interface_table * Xquery_ast.library_module list)
val preprocess_main_module    : processing_context -> main_module -> 
  (norm_interface_table * Xquery_ast.library_module list)

val nopreprocess_prolog         : processing_context -> prolog ->  
  (norm_interface_table * Xquery_ast.library_module list)
val nopreprocess_library_module : processing_context -> library_module -> 
  (norm_interface_table * Xquery_ast.library_module list)
val nopreprocess_main_module    : processing_context -> main_module -> 
  (norm_interface_table * Xquery_ast.library_module list)

(* Normalization functions for:
     o individual statements
     o library module
     o main module 
*)
val normalize_statement      :
    norm_context -> statement -> acstatement
val normalize_interface    :
    (norm_interface_table * norm_context) -> interface -> (norm_context * acinterface)
val normalize_prolog         :
    (norm_interface_table * norm_context) -> prolog -> (norm_context * acprolog)
val normalize_library_module :
    (norm_interface_table * norm_context) -> library_module -> (norm_context * acxmodule)
val normalize_main_module    :
    (norm_interface_table * norm_context) -> main_module -> (norm_context * acxmodule)

val normalize_module         : 
    (norm_interface_table * norm_context) -> xmodule -> (norm_context * acxmodule)

(* Dummy functions used when normalization is disabled *)

val nonorm_statement      :  norm_context -> statement -> acstatement
val nonorm_prolog         :  (norm_interface_table * norm_context) -> prolog -> (norm_context * acprolog)
val nonorm_library_module :  (norm_interface_table * norm_context) -> library_module -> (norm_context * acxmodule)
val nonorm_main_module    :  (norm_interface_table * norm_context) -> main_module -> (norm_context * acxmodule)

