(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_register.ml,v 1.7 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Shredded_register
   Description:
     This module is the registers Shred with the rest of the Galax
     engine.
*)

open Error

(* Note:
     This is the function that should be called when the doc()
     function is called with method 'shred:'.
   - Jerome
 *)

module type Shredded_Registration = sig
  val open_and_get_root : string -> string -> Physical_value.item list
  val close       : unit -> unit
  val sync        : unit -> unit
  val implem_name : string
end

(* Creating a module has side-effects! *)
module Registration_Module
  (Reg : Shredded_Registration) = struct
    let extract_dir_name local =
      try
	Gmisc.split_left_on_char local '#'
      with
	| _ ->
	    raise (Query (Shredded_Error ("Could not extract directory and local name from Shred URL: " ^ local)))

    let check_parameters params =
      match params with
	| ("",None,local) ->
	    extract_dir_name local
	| ("",_,_) ->
	    raise (Query (Shredded_Error ("Shred URL should not have port")))
	| (_,_,_) ->
	    raise (Query (Shredded_Error ("Shred URL should not have a host name")))

    let shredded_doc_call proc_ctxt params =
      let (directory,name) = check_parameters params in
	Reg.open_and_get_root directory name 

    (* Registration *)
    (* Register the uri for the implementation and also its close handler *)
    let () =
      try
	Fn_doc.register_back_end Reg.implem_name shredded_doc_call
      with
	| e ->
	    begin
	      eprintf_error "  " e		
	    end

    let () = Register_handlers.register_close_handler Reg.close
    (* let () = Conf.register_sync_hanlder Reg.sync *)    
end
