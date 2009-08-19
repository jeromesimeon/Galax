(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_jungle_btree.ml,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)
(* This module implements the wrapper for btrees stored in jungle,
   notably the encoding of bit orders and duplicates *)
exception Not_implemented of string

(************************)
(* Btree Module Functor *)
(************************)
(* Functor over the types of key and value *)
module  Shredded_Btree
  (Key       : Shredded_common.Shred_Type)
  (Value     : Shredded_common.Shred_Type)
  = struct
  type btree_handle  = Jungle.jungle_btree
  type btree_key     = Key.t
  type btree_value   = Value.t
  type cursor_direction = Next | Prev

  let btree_open filename bufsize bduplicates =
    let duplicates = if bduplicates then [Jungle.JDB_BTREE_DUPSORT] else [] in 
    Jungle.jungle_btree_open filename ([],duplicates) bufsize
      
  let btree_put h k v =
    Jungle.jungle_btree_put h
      (Key.encode k)
      (Value.encode v) None
      
  let btree_get h k   = 
    match Jungle.jungle_btree_get h (Key.encode k) None [] with
      | None -> None
      | Some v -> Some (Value.decode v)
      

  (* Need to first change idl to 
	 allow actual access to cursors. 
     The interface is dirty right now. *)
  let btree_get_all h k =     
    let jungle_cursor_function = 
      Jungle.jungle_btree_getall h 
	(Key.encode k)
    in
    let cursor_wrapper () =
      match jungle_cursor_function () with
	| None -> None
	| Some v ->
	    Some (Value.decode v)
    in
      Cursor.cursor_of_function cursor_wrapper


  let btree_delete_all h k = 
    Jungle.jungle_btree_delete_all h 
      (Key.encode k) 

  let btree_delete h k v  = 
    Jungle.jungle_btree_delete h 
      (Key.encode k)
      (Value.encode v)

  let btree_close = Jungle.jungle_btree_close
  let btree_sync  = Jungle.jungle_btree_sync
    
  type btree_cursor = Jungle.jungle_btree_cursor

  let decode_option_pair p = 
    match p with
      | None -> None
      | Some (k,v) -> Some ((Key.decode k), 
			    (Value.decode v))

  let btree_cursor_open c =
    Jungle.jungle_btree_cursor_open c []
      
  let btree_cursor_put c k v = 
    Jungle.jungle_btree_cursor_put c
      (Key.encode k)
      (Value.encode v) None

  let btree_cursor_get_next c = 
    decode_option_pair (Jungle.jungle_btree_cursor_get_next c [])

  let btree_cursor_get_prev c = 
    decode_option_pair (Jungle.jungle_btree_cursor_get_prev c [])


  let btree_cursor_get_first c = 
    decode_option_pair (Jungle.jungle_btree_cursor_get_first c [])

  let btree_cursor_get_last c =
    decode_option_pair (Jungle.jungle_btree_cursor_get_last c [])

  (* Both of these need key as additional parameter *)
  let btree_cursor_get_set c k = 
    decode_option_pair (Jungle.jungle_btree_cursor_get_set c (Key.encode k) [])
      
  let btree_cursor_get_both c (k,v) =
    decode_option_pair (Jungle.jungle_btree_cursor_get_both c ((Key.encode k),(Value.encode v)) [])
      
  let btree_cursor_get_set_range c k = 
    decode_option_pair (Jungle.jungle_btree_cursor_get_set_range c (Key.encode k) [])
      
  let btree_cursor_get_both_range c (k,v) =
    decode_option_pair (Jungle.jungle_btree_cursor_get_both_range c ((Key.encode k),(Value.encode v)) [])


  let btree_cursor_del   = Jungle.jungle_btree_cursor_del

  let btree_cursor_close = Jungle.jungle_btree_cursor_close

  let btree_cursor_to_cursor btc d = 
    Cursor.cursor_of_function 
      (
	match d with
	  | Next -> (fun () -> btree_cursor_get_next btc)
	  | Prev -> (fun () -> btree_cursor_get_prev btc)
      )
end

(* All the signatures to let it go.. *)
