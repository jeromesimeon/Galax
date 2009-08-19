(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: wsdl_import.mli,v 1.8 2007/02/01 22:08:55 simeon Exp $ *)


(** 
  @(#)wsdl_import.mli

  Interface for the Wsdl_import module which builds an XQuery AST from
  a wsdl AST.

  Service namespace
  WSDL AST 
  Chosen service 
  Chosen port

  @author Nicola Onose
*)

val wsdl_ast_to_xquery_ast_import : 
  string -> Wsdl_ast.wsdl_module -> 
  string option -> string option ->
  Xquery_ast.library_module

(**
  * Dumps directly to a file
  * The first string is the name of the output file.
  * The second string is the name of the namespace of the functions.
  *
*)
val wsdl_to_xqfile_import : 
  string -> string -> 
  Wsdl_ast.wsdl_module -> 
  string option -> string option -> unit

