(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_jungle_recno.ml,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)
(* This matches the abstract module signatures necessary
   to implement the data model *)

module Jungle_Shredded_Recno_Functor
  (Basetypes : Shredded_store_sigs.Shredded_Basetypes)
  (Record    : Shredded_store_sigs.Record_Type)
  = struct
    (* Conversion Fucntions *)
    open Basetypes
    type record      = Record.record
    let fixed_length = Record.is_fixed_length ()
    let record_size  = Record.get_record_size ()
    (*****************************************)
    (* These are the actual recno structures *)
    (* We fill in the default values         *)
    (*****************************************)
    type recno_handle = Jungle.jungle_recno
    type cursor_direction = Next | Prev

    (* Takes the name of the file open flags and returns a handle on a
       new btree *)
    let recno_open filename buffersize = 
      let handle = Jungle.jungle_recno_open filename 
	([],[]) buffersize fixed_length record_size in
	handle


    let recno_put h recno data = 
      Jungle.jungle_recno_put h recno (Record.encode data) None

    let recno_get_unsafe h recno =    
      Record.decode (Jungle.jungle_recno_get_unsafe h recno None [])

    let recno_get h recno = 
      match Jungle.jungle_recno_get h recno None [] with
	| None -> None
	| Some r -> Some (Record.decode r)
      
    let recno_delete = Jungle.jungle_recno_delete 
    let recno_close  = Jungle.jungle_recno_close 
    let recno_close_no_sync  = Jungle.jungle_recno_close_no_sync
    let recno_sync   = Jungle.jungle_recno_sync

    (******************)
    (* Cursor Section *)
    (******************)
    type recno_cursor = Jungle.jungle_recno_cursor
	
	
    let recno_cursor_open h = 
      Jungle.jungle_recno_cursor_open h []

    let recno_cursor_put c k v = 
      Jungle.jungle_recno_cursor_put  c k (Record.encode v) None

    let recno_cursor_get_next c = 
      match Jungle.jungle_recno_cursor_get_next c [] with
	| Some str ->   Some (Record.decode str)
	| None -> None

    let recno_cursor_get_prev c  =  
      match Jungle.jungle_recno_cursor_get_prev c [] with
	| Some str ->   Some (Record.decode str)
	| None -> None

    let recno_cursor_get_first c = 
      match Jungle.jungle_recno_cursor_get_first c [] with
	| Some str ->   Some (Record.decode str)
	| None -> None

    let recno_cursor_get_last c = 
      match Jungle.jungle_recno_cursor_get_last c [] with
	| Some str -> Some (Record.decode str)
	| None -> None

    let recno_cursor_get_next_dup c = 
      match Jungle.jungle_recno_cursor_get_next_dup c [] with
	| Some str ->   Some (Record.decode str)
	| None -> None

    (* This is special as we need to pass the key as well *)
    let recno_cursor_get_set c recno  = 
      match Jungle.jungle_recno_cursor_get_set c recno [] with
	| Some str -> Some (Record.decode str)
	| None -> None

    let recno_cursor_del   = Jungle.jungle_recno_cursor_del
    let recno_cursor_close = Jungle.jungle_recno_cursor_close  

    let recno_cursor_to_cursor c d =
      Cursor.cursor_of_function 
	(match d with
	  | Next -> (fun () -> recno_cursor_get_next c)
	  | Prev -> (fun () -> recno_cursor_get_prev c))

  end;;

