(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_list.mli,v 1.3 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_list
   Description:
     This module implements sequences in the XQuery 1.0 and XPath 2.0
     data model as Caml lists.
*)


(*********************)
(* List Constructors *)
(*********************)

(* BASIC *)

val list_empty         : unit -> 'a list
val list_of_singleton  : 'a -> 'a list
val list_of_option     : 'a option -> 'a list


(**********************)
(* Sequence Accessors *)
(**********************)

(* GENERAL *)

val list_peek          : 'a list -> 'a option
val list_npeek         : int -> 'a list -> 'a list


(* BASIC / NON DESTRUCTIVE *)

val list_is_empty      : 'a list -> bool

val list_is_singleton  : 'a list -> bool

val list_is_optional   : 'a list -> bool


