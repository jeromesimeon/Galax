(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_util.mli,v 1.2 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Namespace_util
   Description:
     This module implements some basic utilities over XML names.
*)

open Namespace_names


(***********************************)
(* Hashtables over resolved QNames *)
(***********************************)

(* Note:

     The following are Hashtbl modules that operate properly on
     resolved QNames, and are defined for convenience.

     The key difference with the normal Hashtbl is that it properly
     ignore the prefix when comparing the names.

     The second kind of Hashtbl operates on a pair of QName and
     integer and can be used notably to build Hashtbl's for function
     names/arity.

   - Jerome *)

module RQNameHashtbl : Hashtbl.S with type key = rqname

module RQNameIntHashtbl : Hashtbl.S with type key = (rqname * int)


(******************************************)
(* Association lists over resolved QNames *)
(******************************************)

type 'a rqname_assoc_list = (rqname * 'a) list

val remove_rqname_assoc : rqname -> 'a rqname_assoc_list -> 'a rqname_assoc_list
val rqname_assoc        : rqname -> 'a rqname_assoc_list -> 'a
val mem_rqname_assoc    : rqname -> 'a rqname_assoc_list -> bool


