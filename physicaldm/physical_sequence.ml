(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_sequence.ml,v 1.9 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_sequence
   Description:
     This module implements the XQuery 1.0 and XPath 2.0 data model.
*)

open Error
open Namespace_names

(* Note:

     In this module, none of the operations, except forced
     materialization are destructive of the stream.

     This means that to use the cursor operations, one must first cast
     the sequence to a cursor and use the Cursor module. This makes
     sure that we can easily track the destructive operations in the
     rest of the code.

     The "forced" materialization functions are:
	 materialized_of_cursor
	 list_of_sequence
	 materialize_sequence

 - Jerome 03/19/2004
*)

open Physical_value

(*************************)
(* Sequence Constructors *)
(*************************)

(* GENERAL *)

let materialized_of_list x = LSeq x
let streamed_of_list x     = CSeq (Cursor.cursor_of_list x)

let materialized_of_cursor c = LSeq (Cursor.list_of_cursor "Physical_sequence.materialized_of_cursor" c)
let streamed_of_cursor c     = CSeq c

let list_of_sequence s =
  match s with
  | LSeq l -> l
  | CSeq c -> Cursor.list_of_cursor "Physical_sequence.list_of_sequence" c

let cursor_of_sequence s =
  match s with
  | LSeq l -> Cursor.cursor_of_list l
  | CSeq c -> c

(* BASIC *)

let sequence_empty ()       = LSeq []
let sequence_of_singleton x = LSeq [x]
let sequence_of_option oo   =
  match oo with
  | None -> sequence_empty ()
  | Some i -> sequence_of_singleton i

(* Switched from/to materialized and streamed sequences *)

let materialize_sequence s =
  match s with
  | LSeq l -> s
  | CSeq c ->
      LSeq (Cursor.list_of_cursor "Physical_sequence.materialize_sequence" c)

let stream_sequence s =
  match s with
  | LSeq l -> CSeq (Cursor.cursor_of_list l)
  | CSeq c     -> s


(**********************)
(* Sequence Accessors *)
(**********************)

(* GENERAL *)

let sequence_peek seq =
  match seq with
  | LSeq [] -> None
  | LSeq (x :: _) -> Some x
  | CSeq c -> Cursor.cursor_peek c

let sequence_npeek n seq =
  match seq with
  | LSeq list -> Gmisc.list_npeek n list
  | CSeq c        -> Cursor.cursor_npeek n c


(* BASIC / NON DESTRUCTIVE *)

let sequence_is_empty seq =
  match sequence_peek seq with
  | None -> true
  | _ -> false

let sequence_get_singleton seq =
  match sequence_npeek 2 seq with
  | [x] ->
      x
  | [] -> 
      raise (Query (Parameter_Mismatch("Was expecting an singleton sequence, found an empty sequence")))
  | _ ->
      raise (Query (Parameter_Mismatch("Was expecting an singleton sequence, found many")))

let sequence_is_singleton seq =
  match sequence_npeek 2 seq with
  | [x] ->
      true
  | _ ->
      false

let sequence_get_optional seq =
  match sequence_npeek 2 seq with
  | [] ->
      None
  | [x] ->
      Some x
  | _ ->
      raise (Query (Parameter_Mismatch("Was expecting a singleton sequence or empty")))

let sequence_is_optional seq =
  match sequence_npeek 2 seq with
  | [] ->
      true
  | [x] ->
      true
  | _ ->
      false


