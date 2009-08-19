(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_pxp.mli,v 1.2 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Galax_pxp
   Description:
     This module contains useful hooks to the PXP parser.
*)

val glx_default_config : Pxp_types.config
val glx_default_entry  : Pxp_types.entry


val empty_dtd           : unit -> Pxp_dtd.dtd

val default_dtd      : Pxp_dtd.dtd
val default_lfactory : Pxp_lexer_types.lexer_factory
val default_lexobj   : Pxp_lexer_types.lexer_obj

