(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_list.ml,v 1.3 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_list
   Description:
     This module implements sequences in the XQuery 1.0 and XPath 2.0
     data model as Caml lists.
*)

open Error


(*************************)
(* Sequence Constructors *)
(*************************)

(* BASIC *)

let list_empty ()       = []
let list_of_singleton x = [x]
let list_of_option oo   =
  match oo with
  | None -> list_empty ()
  | Some i -> list_of_singleton i


(**********************)
(* Sequence Accessors *)
(**********************)

(* GENERAL *)

let list_peek list =
  match list with
  | [] -> None
  | (x :: _) -> Some x

let list_npeek n list =
  Gmisc.list_npeek n list


(* BASIC / NON DESTRUCTIVE *)

let list_is_empty seq =
  match list_peek seq with
  | None -> true
  | _ -> false

let list_is_singleton seq =
  match list_npeek 2 seq with
  | [x] ->
      true
  | _ ->
      false

let list_is_optional seq =
  match list_npeek 2 seq with
  | [] ->
      true
  | [x] ->
      true
  | _ ->
      false


