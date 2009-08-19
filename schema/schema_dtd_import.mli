(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_dtd_import.mli,v 1.2 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_dtd_import
   Description:
     This module imports a DTD as a set of XML Schema declarations.
*)

val import_dtd : Pxp_dtd.dtd -> Xquery_type_ast.xschema

