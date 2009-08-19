(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_namer.mli,v 1.3 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_namer
   Description:
     This module gives names to anonymous names.
*)

type t

val create : unit -> t

val fresh_name : t -> Namespace_symbols.symbol


