(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_load_update.mli,v 1.3 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Shred_load_update
   Description:
     This module is the implementation of loading the content
	 of inserts and replace in Shred store
*)
(*
module type Shredded_Load_Update_Functor_Sig = 
  functor (Shredded_Store : Shredded_load_sigs.Shredded_Load_Store) ->
    functor (Shredded_load_context_functor : Shredded_load_context.Shredded_Load_Context_Functor_Sig) ->
      sig

	val shredded_load_insert_content :
	  Dm.node Cursor.cursor ->
	  Shredded_Store.nodeid -> 
	  Shredded_Store.nodeid -> unit 		(* Not sure if there would be a return value for this function : Avinash *) 

end

module Shredded_Load_Update_Functor : Shredded_Load_Update_Functor_Sig
*)
