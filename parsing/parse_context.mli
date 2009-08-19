(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parse_context.mli,v 1.13 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Parse_context
   Description:
     This module implements the parsing context.
 *)

type parse_context

val build_xquery_parse_context :
    Processing_context.processing_context -> parse_context

val add_general_entity_to_parse_context :
    parse_context -> string -> string -> unit

val get_processing_context :
    parse_context -> Processing_context.processing_context

val get_general_entity :
    parse_context -> Finfo.finfo -> string -> string

