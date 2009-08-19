(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_validation.mli,v 1.4 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_validation
   Description:
     This module implements XML Schema validation directly on a SAX
     stream.
*)

(**************)
(* Validation *)
(**************)

(* Note:

     The validate operation operates on a SAX stream on which
     namespace resolution has already been performed.

   - Jerome
 *)

val validate :
    Xquery_type_core_ast.cxschema -> Streaming_types.resolved_xml_stream -> Streaming_types.typed_xml_stream

