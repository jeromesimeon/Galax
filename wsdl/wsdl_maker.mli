(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: wsdl_maker.mli,v 1.2 2007/02/01 22:08:55 simeon Exp $ *)

(** 
  @(#)wsdl_maker.mli

  Builds a wsdl description from an XQuery module. 

  @author Nicola Onose
*)

val build_wsdl : Xquery_ast.library_module -> string -> 
  string option -> string option -> Wsdl_ast.soap_address ->
  Wsdl_ast.wsdl_module

