(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: demo_conf.mli,v 1.5 2007/02/01 22:08:55 simeon Exp $ *)

(* config parameters *)

val document_dir : string ref

val cgibin_prefix : string ref

val demo_init : string -> unit

