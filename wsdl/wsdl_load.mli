(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: wsdl_load.mli,v 1.7 2007/02/01 22:08:55 simeon Exp $ *)


(** 
  @(#)load_wsdl.mli

  Takes the name of the service and a wsdl stream
  and builds an instance of a wsdl AST.

  @author Nicola Onose
*)

val xml_to_wsdl_ast : Processing_context.processing_context -> string -> Streaming_types.xml_stream -> Wsdl_ast.wsdl_module

val test_parser : string -> Wsdl_ast.wsdl_module
