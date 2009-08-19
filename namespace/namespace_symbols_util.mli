(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_symbols_util.mli,v 1.2 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Namespace_symbols_util
   Description:
     This module implements some basic utilities over name symbols.
*)

open Namespace_symbols


(*********************************)
(* Hashtables over symbol QNames *)
(*********************************)

(* Note:

     The following are Hashtbl modules that operate properly on
     symbol QNames, and are defined for convenience.

     The key difference with the normal Hashtbl is that it properly
     ignore the prefix when comparing the names.

     The second kind of Hashtbl operates on a pair of QName and
     integer and can be used notably to build Hashtbl's for function
     names/arity.

   - Jerome *)

module SQNameHashtbl : Hashtbl.S with type key = symbol


(******************************************)
(* Association lists over resolved QNames *)
(******************************************)

type 'a symbol_assoc_list = (symbol * 'a) list

val remove_symbol_assoc : symbol -> 'a symbol_assoc_list -> 'a symbol_assoc_list
val symbol_assoc        : symbol -> 'a symbol_assoc_list -> 'a
val mem_symbol_assoc    : symbol -> 'a symbol_assoc_list -> bool


