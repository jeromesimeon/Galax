(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_main_memory_recno.ml,v 1.5 2007/02/01 22:08:54 simeon Exp $ *)

(* This matches the abstract module signatures necessary to implement
   the data model *)

module Main_Memory_Recno_Functor
  (Basetypes : Shredded_store_sigs.Shredded_Basetypes)
  (Record    : Shredded_store_sigs.Record_Type)
  = struct
    (* Conversion Functions *)
    open Basetypes
    type record      = Record.record
    let fixed_length = Record.is_fixed_length ()
    (* let record_size  = Record.get_record_size () *)

    (*****************************************)
    (* These are the actual recno structures *)
    (* We fill in the default values         *)
    (*****************************************)
    type recno_handle = {
      mutable contents : Record.record option array
    }
    let mk_handle oa = { contents = oa; } 

    type cursor_direction = Next | Prev
    
    let initial_size = 1024
    

    let is_index_safe h index = 
      (index >= 0) && (index < (Array.length h.contents)) 

    let assert_index_unsafe h index = 
      if is_index_safe h index then
	raise (Error.Query 
		 (Error.Shredded_Error 
			      ("Unsafe access on Main Memory Recno [invalid index]: " ^ (string_of_int index))))
	  
    

    (* May not want to grow this way *)
    let calc_next_size current_size = 
      current_size lsl 1    

    let grow_contents h = 
      let old_size  = Array.length h.contents in 
      let new_size  = calc_next_size old_size in
      let new_array = Array.make new_size None in
	Array.blit h.contents 0 new_array 0 old_size;
	h.contents <- new_array

    (* Takes the name of the file open flags and returns a handle on a
       new btree *)
    let (recno_handles: (string, recno_handle) Hashtbl.t) = Hashtbl.create 1
    let recno_open filename buffersize = 
      if Hashtbl.mem recno_handles filename
      then Hashtbl.find recno_handles filename
      else begin
	let handle = mk_handle (Array.make initial_size None) in 
	  Hashtbl.add recno_handles filename handle;
	  handle
      end

    let recno_put h recno data = 
      while (Array.length h.contents) <= recno do
	grow_contents h
      done
      ; (* should be ok to store now *)
      h.contents.(recno) <- (Some data)

    let recno_get_unsafe h recno =    
      if is_index_safe h recno then
	match h.contents.(recno) with
	  | None -> 
	      raise (Error.Query 
		       (Error.Shredded_Error 
			  ("unsafe_get of invalid record " ^ (string_of_int recno))))
	  | Some v -> v
      else 
        raise (Error.Query 
		 (Error.Shredded_Error 
		    ("unsafe_get of invalid record - out of bounds " ^ (string_of_int recno))))

    let recno_get h recno = 
      if is_index_safe h recno then
	h.contents.(recno)
      else None
	    
    let recno_delete h recno = 
      if is_index_safe h recno then 
	h.contents.(recno) <- None
      else (* Should this be an error? *)
	raise (Error.Query 
		 (Error.Shredded_Error 
		    ("unsafe_delete of invalid record - out of bounds " ^ (string_of_int recno))))

    let recno_close h          =  () (* h.contents <- [||] *)
    let recno_close_no_sync h  =  () (* h.contents <- [||] *) 
    let recno_sync h           =  ()

    (******************)
    (* Cursor Section *)
    (******************)
    type recno_cursor = { handle : recno_handle;
			  mutable current_index:  int }
	
    let mk_cursor h ci = { handle = h; current_index = ci }
      
    let recno_cursor_open h = { handle = h; current_index = 0 } 

    (* Does this advance us? - this is really akward! *)
    let recno_cursor_put c k v = recno_put c.handle k v

    let rec recno_cursor_get_next c =       
      c.current_index <- c.current_index + 1;
      if is_index_safe c.handle c.current_index then
	match recno_get c.handle c.current_index with
	  | None -> None
	  | (Some v) as ret ->  ret
      else None

    let rec recno_cursor_get_prev c  =  
      c.current_index <- c.current_index - 1;
      if is_index_safe c.handle c.current_index then
	match recno_get c.handle c.current_index with
	  | None -> recno_cursor_get_prev c
	  | (Some v) as ret ->  ret
      else None

    let recno_cursor_get_first c = 
      c.current_index <- 0;
      if is_index_safe c.handle c.current_index then
	match recno_get c.handle c.current_index with
	  | None -> recno_cursor_get_next c 
	  | (Some v) as ret ->  ret
      else None

    let recno_cursor_get_last c = 
      (* Set it past the end, and then walk to the previous entry *)
      c.current_index <- (Array.length c.handle.contents) + 1;
      recno_cursor_get_prev c 

    (* Dup? *)
    let recno_cursor_get_next_dup = recno_cursor_get_next
      
    (* use the same trick as last *)
    let recno_cursor_get_set c recno  = 
      c.current_index <- recno - 1;
      recno_cursor_get_next c

    let recno_cursor_del c = 
      recno_delete c.handle c.current_index

	(* set to a very invalid index *)
    let recno_cursor_close c = c.current_index <- (-1000)

    let recno_cursor_to_cursor c d =
      Cursor.cursor_of_function 
	(match d with
	  | Next -> (fun () -> recno_cursor_get_next c)
	  | Prev -> (fun () -> recno_cursor_get_prev c))

  end;;
