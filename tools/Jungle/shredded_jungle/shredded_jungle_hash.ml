(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_jungle_hash.ml,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)
module Shredded_Hash 
  (Key   : Shredded_common.Shred_Type)
  (Value : Shredded_common.Shred_Type)
  =
struct
  type hash  = Jungle.jungle_hash
  type hash_key   = Key.t
  type hash_value = Value.t
  type cursor_direction = Next | Prev

  let hash_open filename buffersize bduplicates = 
    let duplicates = if bduplicates then [Jungle.JDB_HASH_DUP] else [] in 
      Jungle.jungle_hash_open filename ([],duplicates) buffersize

  let hash_put sh k v =
    Jungle.jungle_hash_put sh 
      (Key.encode k)
      (Value.encode v) None

  let hash_get sh k =
    match Jungle.jungle_hash_get sh 
      (Key.encode k) None []with
      | None -> None
      | Some v -> 
	  Some (Value.decode v)
  
  let hash_get_all h k =     
    let jungle_cursor_function = 
      Jungle.jungle_hash_getall h 
	(Key.encode k)
    in
    let cursor_wrapper () =
      match jungle_cursor_function () with
	| None -> None
	| Some v ->
	    Some (Value.decode v)
    in
      Cursor.cursor_of_function cursor_wrapper


  let hash_delete sh k =
    Jungle.jungle_hash_delete sh 
      (Key.encode k)

  let hash_close = Jungle.jungle_hash_close
  let hash_sync  = Jungle.jungle_hash_sync 

  type hash_cursor  = Jungle.jungle_hash_cursor
  let decode_option_pair p = 
    match p with
      | None -> None
      | Some (k,v) -> Some ((Key.decode k), 
			    (Value.decode v))
  let hash_cursor_open sh =
    Jungle.jungle_hash_cursor_open sh [] 

  let hash_cursor_put c k v =
    Jungle.jungle_hash_cursor_put c
      (Key.encode k)
      (Value.encode v) None

  let hash_cursor_get_next c =
    decode_option_pair (Jungle.jungle_hash_cursor_get_next c [])

  let hash_cursor_get_prev c = 
    decode_option_pair (Jungle.jungle_hash_cursor_get_prev c [])

  let hash_cursor_get_first c = 
    decode_option_pair (Jungle.jungle_hash_cursor_get_first c [])

  let hash_cursor_get_last c = 
    decode_option_pair (Jungle.jungle_hash_cursor_get_last c [])


  (* Both of these need key as additional parameter *)
  let hash_cursor_get_set c k = 
    decode_option_pair (Jungle.jungle_hash_cursor_get_set c (Key.encode k) [])

  let hash_cursor_get_both c (k,v) = 
    decode_option_pair (Jungle.jungle_hash_cursor_get_both c ((Key.encode k),(Value.encode v)) [])


  let hash_cursor_del    = Jungle.jungle_hash_cursor_del
  let hash_cursor_close = Jungle.jungle_hash_cursor_close
    
  let hash_cursor_to_cursor btc d = 
    Cursor.cursor_of_function 
      (
	match d with
	  | Next -> (fun () -> hash_cursor_get_next btc)
	  | Prev -> (fun () -> hash_cursor_get_prev btc)
      )
end
