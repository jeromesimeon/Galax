(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_main_memory_dm.ml,v 1.3 2007/02/01 22:08:54 simeon Exp $ *)

(* These have signatures and actually register the jungleshred:// uri *)
module Main_Memory_Shredded_Datamodel =
  Shredded_dm.Shredded_Datamodel 
    (Shredded_store.Shredded_Store_Functor)
    (Shredded_main_memory_basetypes.Main_Memory_Basetypes)
    (Shredded_main_memory_record.Main_Memory_Record)
    (Shredded_main_memory_recno.Main_Memory_Recno_Functor)
    (Shredded_main_memory_btree.Main_Memory_Btree_Functor)
    (Shredded_main_memory_hash.Shredded_Main_Memory_Hash_Functor)



(* This module has side-effects! *) 
module REGISTER_JUNGLE = 
  Shredded_register.Registration_Module
    (Main_Memory_Shredded_Datamodel)

