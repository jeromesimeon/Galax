(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_symbols_util.ml,v 1.2 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Namespace_symbols_util
   Description:
     This module implements some basic utilities over name symbols.
*)

open Error

open Namespace_symbols


(*********************************)
(* Hashtables over symbol QNames *)
(*********************************)

(* These hash functions are not good
   but better ones that involve 
   concatenation or casting are too slow. *)
module SQName =
  struct
    type t = symbol
    let equal = symbol_equal
    let hash = symbol_hash
  end

module SQNameHashtbl = Hashtbl.Make(SQName)


(******************************************)
(* Association lists over resolved QNames *)
(******************************************)

type 'a symbol_assoc_list = (symbol * 'a) list

let rec remove_symbol_assoc symbol0 assoc_list =
  match assoc_list with
  | [] -> []
  | (symbol, x as pair) :: assoc_list' ->
      if symbol_equal symbol symbol0
      then assoc_list'
      else pair :: remove_symbol_assoc symbol0 assoc_list'

let rec symbol_assoc symbol0 assoc_list =
  match assoc_list with
  | [] -> raise Not_found
  | (symbol,x) :: assoc_list' ->
      if symbol_equal symbol symbol0
      then x
      else symbol_assoc symbol0 assoc_list'

let rec mem_symbol_assoc symbol0 assoc_list =
  match assoc_list with
  | [] -> false
  | (symbol,x) :: assoc_list' ->
      if symbol_equal symbol symbol0
      then true
      else mem_symbol_assoc symbol0 assoc_list'


