(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm_atomic_util.mli,v 1.4 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Datatypes_atomic_util
   Description:
     This module implements a number of short-cut functions to
     construct and access atomic values in the XQuery 1.0 and XPath
     2.0 data model.
*)


(***********)
(* Erasure *)
(***********)

(* Erasure of simple values *)

val erase_simple_value   : Dm_atomic.atomicValue list -> Datatypes.xs_untyped
    (* Turns a simple value back into its text representation *)


(* Normalization stuff *)

val integer_one : Dm_atomic.atomicValue

(* Total order over atomic types *)

val total_order_compare :
    Dm_atomic.atomicValue -> Dm_atomic.atomicValue -> int

val uri_dm_of_uri : AnyURI._uri option -> Dm_atomic.atomicValue option ref

val default_no_uri_dm : Dm_atomic.atomicValue option ref

