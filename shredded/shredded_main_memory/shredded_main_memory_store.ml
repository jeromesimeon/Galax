(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_main_memory_store.ml,v 1.3 2007/02/01 22:08:54 simeon Exp $ *)

module Main_Memory_Store = Shredded_store.Shredded_Store_Functor 
  (Shredded_main_memory_basetypes.Main_Memory_Basetypes) 
  (Shredded_main_memory_record.Main_Memory_Record)
  (Shredded_main_memory_recno.Main_Memory_Recno_Functor)		      
  (Shredded_main_memory_btree.Main_Memory_Btree_Functor)    
  (Shredded_main_memory_hash.Shredded_Main_Memory_Hash_Functor)

