(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: wsdl_export.mli,v 1.5 2007/02/01 22:08:55 simeon Exp $ *)


(** 
  @(#)wsdl_export.mli

  @author Nicola Onose
*)

val wsdl_ast_to_xquery_ast_export : 
  string -> Wsdl_ast.wsdl_module -> 
  string option -> string option -> string -> string -> Xquery_ast.xmodule
  (* service namespace prefix * WSDL module * chosen service in WSDL * port * installation-dir * server-implementation filename *)

(**
  * Dumps directly to a file
  * The first string is the name of the output file.
  * The second string is the name of the namespace of the functions.
  *
*)
val wsdl_to_xqfile_export : 
  string -> string -> 
  Wsdl_ast.wsdl_module -> 
  string option -> string option -> string -> string option -> unit
  (* Service filename * service namespace prefix * WSDL module * chosen service in WSDL * port * 
     installation-dir * server-implementation filename *)
  
