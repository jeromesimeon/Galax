(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_main_memory_hash.ml,v 1.3 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Shredded_main_memory_hash
   Description:
     This module describes a functorial interface for in memory
     hash tables.
*)

exception Cursor_Error of string

module Shredded_Main_Memory_Hash_Functor
  (Key   : Shredded_common.Shred_Type)
  (Value : Shredded_common.Shred_Type)
  =
struct
  type hash  = (Key.t, Value.t) Hashtbl.t
  type hash_key   = Key.t
  type hash_value = Value.t


      
  let (hash_handles:((string ,hash) Hashtbl.t)) = Hashtbl.create 1


  let hash_open filename buffersize l = 
    if Hashtbl.mem hash_handles filename 
    then Hashtbl.find hash_handles filename
    else 
      begin 
	let ht = 
	  (* This is problematic for the semantic *)
	  (*	  
		  if not l then 
		  raise (Error.Query (Error.Shredded_Error ("Main memory hash only supports duplicate operations")))
	  else *)
	    Hashtbl.create buffersize
	in
	  Hashtbl.add hash_handles filename ht;
	  ht
      end

  let hash_put sh k v = Hashtbl.add sh k v

  let hash_get sh k = 
    if Hashtbl.mem sh k 
    then Some (Hashtbl.find sh k)
    else None

  let hash_get_all sh k = Cursor.cursor_of_list (Hashtbl.find_all sh k)

  type cursor_direction = Next | Prev

  let hash_delete sh k = Hashtbl.remove sh k

  let hash_close sh = ()
  let hash_sync  sh = ()

  (* HASH CURSORS ARE HORRILBY INEFFICIENT *)
  type hash_cursor  = 
      { ht : hash;
	mutable l  : (Key.t * Value.t) array;
	cur : int ref;
      }

  let hash_cursor_open sh =    
    let list = Hashtbl.fold (fun k v l -> (k,v) :: l) sh [] in
      { ht = sh; 
	l = Array.of_list list; 
	cur = ref 0 } 


  (* Where do we add this in? Order is not defined ? *)
  let hash_cursor_put c k v =     
    hash_put c.ht k v 

  let hash_cursor_get_next {ht=ht;l=l;cur=cur} =
    let len = Array.length l in 
      incr cur;
      if !cur >= 0 && !cur < len then
	begin 
	  let v =  l.(!cur) in
	    Some v
	end
      else
	None

  let hash_cursor_get_prev {ht=ht;l=l;cur=cur} = 
    let len = Array.length l in 
      decr cur; 
      if !cur >= 0 && !cur < len then
	begin 
	  let v =  l.(!cur) in
	    Some v
	end
      else
	None
	  
  let hash_cursor_get_first {ht=ht;l=l;cur=cur} = 
    if Array.length l > 0 then 
      begin
	cur := 0;
	Some l.(0)
      end
    else None

  let hash_cursor_get_last {ht=ht;l=l;cur=cur} = 
    if Array.length l > 0 then 
      begin
	cur := (Array.length l) - 1;
	Some l.(!cur)
      end
    else None

  let hash_cursor_to_cursor c cursor_direction = 
    let cursor_function = 
      match cursor_direction with
	| Next -> (fun () -> hash_cursor_get_next c)
	| Prev -> (fun () -> hash_cursor_get_prev c)
    in
      Cursor.cursor_of_function cursor_function
	

  (* Both of these need key as additional parameter *)
  let hash_cursor_get_set c       = raise (Cursor_Error ("Hash get_set_range main memory not working"))
  let hash_cursor_get_both c       = raise (Cursor_Error ("Hash get_both main memory not working"))

  let hash_cursor_del  ({ht=ht;l=l;cur=cur} as e) = 
    let len  = Array.length l in 
      if !cur >= 0 && !cur < len then 
	begin
	  let left = Array.sub l 0 !cur in	
	  let right = Array.sub l (!cur+1) (len - 1 - !cur) in 
	    e.l <- Array.append left right;
	    (* Cur remains the same *)
	end
      else raise (Cursor_Error "Invalid delete call")

  (* Two things need to happen, the item disappears from the cursor *)
  let hash_cursor_close  sh = ()
end
