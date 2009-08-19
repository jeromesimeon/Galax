(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: print_xquery.mli,v 1.4 2007/08/01 17:06:17 mff Exp $ *)

(* Module Print_xquery
   Description:
     This module implements pretty-printing for the XQuery AST.
*)

(******************)
(* Sequence types *)
(******************)

val print_sequencetype : Format.formatter -> Xquery_ast.sequencetype -> unit

(**********************)
(* XQuery expressions *)
(**********************)

val print_expr : Format.formatter -> Xquery_ast.expr -> unit

(**************)
(* Statements *)
(**************)

val print_statement : Format.formatter -> Xquery_ast.statement -> unit

(**********)
(* Module *)
(**********)

val print_interface : Format.formatter -> Xquery_ast.interface -> unit
val print_library_module : Format.formatter -> Xquery_ast.library_module -> unit
val print_main_module : Format.formatter -> Xquery_ast.main_module -> unit
val print_module : Format.formatter -> Xquery_ast.xmodule -> unit
val print_prolog : Format.formatter -> Xquery_ast.prolog -> unit

