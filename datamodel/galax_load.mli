(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_load.mli,v 1.10 2007/08/15 18:53:48 mgreenberg Exp $ *)

(* Module: Galax_load
   Description:
     This module implements loading of a main-memory data model
     instance from a (typed) XML stream.
*)

open Streaming_types


(* This module operates by side effects, actually registering the
loading functions through Physical_load!! - Jerome and Philippe *)

