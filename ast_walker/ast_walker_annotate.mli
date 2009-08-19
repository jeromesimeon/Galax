(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ast_walker_annotate.mli,v 1.2 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Ast_walker_annotate
   Description:
     This module implements a generic tree walker, which recursively
     applies rules to annotate nodes in an AST.  The annotation rules
     are imperative, i.e., the annotations on the AST nodes are
     modified in place.
*)

open Ast_walker_annotate_context

open Xquery_core_ast

val annotate_cexpr   	: 'a annotation_context -> acexpr      -> unit
val annotate_cstatement : 'a annotation_context -> acstatement -> unit
val annotate_cprolog 	: 'a annotation_context -> acprolog    -> unit
val annotate_cxmodule 	: 'a annotation_context -> acxmodule   -> unit

