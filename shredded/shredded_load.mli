(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_load.mli,v 1.5 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Shredded_load
   Description:
   This module implements loading of a set of physical Shred
   indexes from a (typed) XML stream. It is a functor parameterized by 
   store.
*)

(* Load a data model instance from an XML stream *)

open Shredded_load_context
module type Shredded_Load_Functor_Sig = 
  functor (Shredded_store : Shredded_load_sigs.Shredded_Load_Store) ->
sig

  val load_shredded_store_from_resolved_stream : 
    Nodeid_context.nodeid_context -> Streaming_types.xml_stream -> 
    string -> string -> int -> Shredded_store.shredded_store
    (** [load_shred_store_from_resolved_stream xml_stream dir
	name] builds a new Shred store in directory [dir], with
	logical name [name], populated from the stream
	[xml_stream] *)


  val load_an_update_from_resolved_stream : 
    Shredded_store.shredded_store -> Streaming_types.xml_stream -> Nodeid.nodeid -> Nodeid.nodeid Cursor.cursor (* cursor? *)


  val close_shredded_store : Shredded_store.shredded_store -> unit

  val load_shredded_store_from_ordered_typed_stream : Nodeid_context.nodeid_context -> Streaming_types.ordered_xml_stream -> 
    string -> string -> int -> Shredded_store.shredded_store  

end

module Shredded_Load_Functor : Shredded_Load_Functor_Sig
