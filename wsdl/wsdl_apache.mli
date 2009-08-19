(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: wsdl_apache.mli,v 1.4 2007/02/01 22:08:55 simeon Exp $ *)

(** 
  @(#)wsdl_apache.mli
  
  @author Nicola Onose
*)



(**
  * Dumps directly to a file
  * The arguments are:
  * - the name of the exported module (.xq) file.
  * - the name of the server stub.
  * - the prefix of the namespace of the functions.
  * - the WSDL AST
  * - the name of the WSDL service 
  * - the name of the WSDL port  
  *
*)

val wsdl2xq_server_source : 
    string option ->
    string ->
    Namespace_names.ncname ->
    Wsdl_ast.wsdl_module ->
    string option -> Namespace_names.ncname option -> unit
