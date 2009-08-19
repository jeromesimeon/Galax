(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: register_handlers.ml,v 1.2 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Register_handlers 

   Description: 

   This module handles registration of close handlers for Galax. These
   handlers allow persistant stores to do any final cleanup before
   exiting. 

   We could also allow things like failure handlers, sync handlers but
   right now we just have proper close handlers. 
*)

(*****************************************)
(* A list of close handlers to be called *)
(* Order is the reverse of registration  *)
(*****************************************)

let (close_handlers: (unit -> unit) list ref) = ref []
let register_close_handler (h: unit -> unit) =
  close_handlers := h :: !close_handlers

let call_close_handlers () = List.iter (fun x -> x ()) !close_handlers
