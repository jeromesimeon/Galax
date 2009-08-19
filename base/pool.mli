(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: pool.mli,v 1.7 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Pool
   Description:
     This module implements string pools. Those are bidirectional
     mappings from strings to integers, used to save space in the
     representation of XML documents.
*)


(* Signature of the NamePool modules *)

module type NamePool =
  sig
    type name
    type symbol = int

    type namepool

    val create_pool : unit -> namepool
    val init_pool : namepool -> unit

    val get_name  : namepool -> symbol -> name
    val add_name  : namepool -> name -> symbol

    val exists_name : namepool -> name -> bool
    val symbol_equals : namepool -> symbol -> symbol -> bool

    val pool_size : namepool -> (int * int * int)
  end

(* Functor to create new name pool modules for an given hashed type *)

module MakeNamePool (H: Hashtbl.HashedType) : (NamePool with type name = H.t)

