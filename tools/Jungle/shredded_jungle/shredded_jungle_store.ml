(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_jungle_store.ml,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)
open Shredded_common
open Shredded_jungle_btree
open Shredded_store

module Jungle_Store_Module= Shredded_store.Shredded_Store_Functor 
    (Shredded_jungle_basetypes) (Shredded_jungle_record)
    (Shredded_jungle_recno.Jungle_Shredded_Recno_Functor)
    (Shredded_jungle_btree.Shredded_Btree)
    (Shredded_jungle_hash.Shredded_Hash)
