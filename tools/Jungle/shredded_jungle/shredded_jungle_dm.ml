(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_jungle_dm.ml,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)
(* These have signatures and actually register the jungleshred:// uri *)
module Jungle_Shredded_Datamodel =
  Shredded_dm.Shredded_Datamodel 
    (Shredded_store.Shredded_Store_Functor)
    (Shredded_jungle_basetypes)
    (Shredded_jungle_record)
    (Shredded_jungle_recno.Jungle_Shredded_Recno_Functor)
    (Shredded_jungle_btree.Shredded_Btree)
    (Shredded_jungle_hash.Shredded_Hash)


(* This module has side-effects! *) 
module REGISTER_JUNGLE = 
  Shredded_register.Registration_Module
    (Jungle_Shredded_Datamodel)

