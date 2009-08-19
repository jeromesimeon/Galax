(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: wsdl_printer.mli,v 1.2 2007/02/01 22:08:55 simeon Exp $ *)

(** 
  @(#)wsdl_printer.mli

  Prints a wsdl description file from a WSDL AST. 

  @author Nicola Onose
*)


val print_wsdl : Format.formatter -> Wsdl_ast.wsdl_module -> unit

