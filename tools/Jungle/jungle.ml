(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: jungle.ml,v 1.11 2007/02/01 22:08:54 simeon Exp $ *)

(* NOTE: We need to change the name of SmallDB *)
(*****************************************************************************************************)
(* 4.1 BDB Version                                                                                   *)
(* TAKEN FROM db.h #define	DB_NOTFOUND		(-30991)/* Key/data pair not found (EOF). */ *)
(* let const_DB_NOTFOUND = -30991                                                                    *)
(*****************************************************************************************************)

(*************************************************************************************)
(* 4.3 changes the DB_NOTFOUND value                                                 *)
(* #define	DB_NOTFOUND		(-30989)/* Key/data pair not found (EOF). */ *)
(* let const_DB_NOTFOUND = -30989                                                    *)
(*************************************************************************************)

open Smalldb
let const_DB_NOTFOUND = get_const_DB_NOTFOUND ()

(* Profiling Counter *)
let put_count = ref 0
and get_count = ref 0

let get_put_count() = !put_count

let get_get_count() = !get_count

(* Misc functions *)

let increment num = 
  num := !num + 1

let get_func_name x = 
  increment x;
  "function" ^ (string_of_int !x)

(* A generic exception for now *)

exception Jungle_Error of string

(* A counter for generating names for functions
   computing the secondary index key *)

let count = ref 0

(************************************************************************)
(* 						Jungle Flag Declaration 						*)
(************************************************************************)

(* Generic open flags for all kinds of index *)

type jungle_open_flags =
  | JDB_CREATE
  | JDB_EXCL
  | JDB_DIRTY_READ
  | JDB_RDONLY
  | JDB_THREAD

(* Btree Specific Flags *)
type jungle_open_btree_comp_flags =
  | JDB_BTREE_DUP
  | JDB_BTREE_DUPSORT
  | JDB_BTREE_RECNUM
  | JDB_BTREE_REVSPLITOFF

type jungle_put_btree_excl_flags = 
  | JDB_BTREE_NOOVERWRITE	
  | JDB_BTREE_NODUPDATA

type jungle_get_btree_excl_flags = 
   | JDB_BTREE_GET_BOTH
   | JDB_BTREE_SET_RECNO  

type jungle_get_btree_comp_flags = 
   | JDB_BTREE_MULTIPLE
   | JDB_BTREE_DIRTY_READ
   | JDB_BTREE_RMW

(* Recno Specific Flags *)  
type jungle_open_recno_comp_flags = 
  | JDB_RECNO_RENUMBER
  | JDB_RECNO_SNAPSHOT

type jungle_put_recno_excl_flags = 
  | JDB_RECNO_APPEND
  | JDB_RECNO_NOOVERWRITE	

type jungle_get_recno_excl_flags = 
  | JDB_RECNO_GET_BOTH

type jungle_get_recno_comp_flags = 
  | JDB_RECNO_MULTIPLE
  | JDB_RECNO_DIRTY_READ
  | JDB_RECNO_RMW

(* Hashtable Specific Flags *)
type jungle_open_hash_comp_flags =
  | JDB_HASH_DUP
  | JDB_HASH_DUPSORT

type jungle_put_hash_excl_flags = 
  | JDB_HASH_NOOVERWRITE	
  | JDB_HASH_NODUPDATA

type jungle_get_hash_excl_flags = 
   | JDB_HASH_GET_BOTH
   | JDB_HASH_SET_RECNO 

type jungle_get_hash_comp_flags = 
   | JDB_HASH_MULTIPLE
   | JDB_HASH_DIRTY_READ
   | JDB_HASH_RMW

(* Cursor Flags *)

(* Common flags for cursor_open *)
type jungle_curopen_comp_flags = 
   | JDB_CUR_DIRTY_READ
   | JDB_CUR_WRITECURSOR

(* Common flags for cursor_get *)
type jungle_cur_get_comp_flags = 
   | JDB_CURGET_DIRTY_READ
   | JDB_CURGET_MULTIPLE
   | JDB_CURGET_MULTIPLE_KEY
   | JDB_CURGET_RMW

(* BTREE Specific Cursor Flags *)
type jungle_cur_put_btree_excl_flags =
   | JDB_BTREE_CURPUT_AFTER
   | JDB_BTREE_CURPUT_BEFORE
   | JDB_BTREE_CURPUT_CURRENT
   | JDB_BTREE_CURPUT_KEYFIRST
   | JDB_BTREE_CURPUT_KEYLAST
   | JDB_BTREE_CURPUT_NODUPDATA

(* Recno Specific Cursor Flags *)
type jungle_cur_put_recno_excl_flags =
   | JDB_RECNO_CURPUT_AFTER		(*Check DB_RENUMBER_FLAG in create*)
   | JDB_RECNO_CURPUT_BEFORE 	(*Check DB_RENUMBER_FLAG in create*)
   | JDB_RECNO_CURPUT_CURRENT


type jungle_cur_put_hash_excl_flags = 
   | JDB_HASH_CURPUT_AFTER
   | JDB_HASH_CURPUT_BEFORE
   | JDB_HASH_CURPUT_CURRENT
   | JDB_HASH_CURPUT_KEYFIRST
   | JDB_HASH_CURPUT_KEYLAST
   | JDB_HASH_CURPUT_NODUPDATA


(************************************************************************)
(* 				Jungle to BDB Flag conversion operations	 			*)
(************************************************************************)

(* Flag conversion operations *)

(* Generic Open Flag Conversion *)
let bdb_flag_of_jungle_open_flag f =
  match f with
  | JDB_CREATE 		-> G_DB_CREATE
  | JDB_EXCL 		-> G_DB_EXCL
  | JDB_DIRTY_READ 	-> G_DB_DIRTY_READ
  | JDB_RDONLY 		-> G_DB_RDONLY
  | JDB_THREAD		-> G_DB_THREAD

(* Btree flag Conversions *)
let bdb_flag_of_jungle_open_btree_comp_flag f =
  match f with
  | JDB_BTREE_DUP -> G_DB_DUP
  | JDB_BTREE_DUPSORT -> G_DB_DUPSORT
  | JDB_BTREE_RECNUM -> G_DB_RECNUM
  | JDB_BTREE_REVSPLITOFF -> G_DB_REVSPLITOFF

let bdb_flag_of_jungle_put_btree_excl_flag f = 
  match f with
    | None -> []
    | Some v -> 
	begin 
	  match v with 
	    | JDB_BTREE_NOOVERWRITE	->  G_DB_NOOVERWRITE		
	    | JDB_BTREE_NODUPDATA	->  G_DB_NODUPDATA	
	end :: []

let bdb_flag_of_jungle_get_btree_excl_flag f = 
  match f with 
    | None -> []
    | Some f -> 
	begin
	  match f with
	    | JDB_BTREE_GET_BOTH         -> G_DB_GET_BOTH
	    | JDB_BTREE_SET_RECNO        -> G_DB_SET_RECNO 
	end :: []

let bdb_flag_of_jungle_get_btree_comp_flag f = 
  match f with
   | JDB_BTREE_MULTIPLE         -> G_DB_MULTIPLE
   | JDB_BTREE_DIRTY_READ       -> G_DB_DIRTY_READ
   | JDB_BTREE_RMW              -> G_DB_RMW
  
(* Recno flag Conversions *)
let bdb_flag_of_jungle_open_recno_comp_flag f =
  match f with
  | JDB_RECNO_RENUMBER -> G_DB_RENUMBER
  | JDB_RECNO_SNAPSHOT -> G_DB_SNAPSHOT
  
let bdb_flag_of_jungle_put_recno_excl_flag f = 
  match f with 
    | None -> []
    | Some f -> 
	begin
	  match f with
	    | JDB_RECNO_APPEND    ->      G_DB_APPEND
	    | JDB_RECNO_NOOVERWRITE ->	G_DB_NOOVERWRITE	
	end :: []

let bdb_flag_of_jungle_get_recno_excl_flag f = 
  match f with 
    | None -> []
    | Some f -> begin
	match f with
	  | JDB_RECNO_GET_BOTH   -> G_DB_GET_BOTH
      end :: []
	      
let bdb_flag_of_jungle_get_recno_comp_flag f = 
  match f with
  | JDB_RECNO_MULTIPLE   -> G_DB_MULTIPLE
  | JDB_RECNO_DIRTY_READ -> G_DB_DIRTY_READ
  | JDB_RECNO_RMW 		 -> G_DB_RMW


(* Hash Flag Conversions *)
let bdb_flag_of_jungle_open_hash_comp_flag f = 
  match f with 
  | JDB_HASH_DUP		-> G_DB_DUP
  | JDB_HASH_DUPSORT	-> G_DB_DUPSORT

let bdb_flag_of_jungle_put_hash_excl_flag f = 
  match f with 
    | None -> []
    | Some f -> 
	begin
	  match f with 
	    | JDB_HASH_NOOVERWRITE -> G_DB_NOOVERWRITE	
	    | JDB_HASH_NODUPDATA	 -> G_DB_NODUPDATA
	end :: []

let bdb_flag_of_jungle_get_hash_excl_flag f = 
  match f with
    | None -> []
    | Some f -> begin
	match f with 
	  | JDB_HASH_GET_BOTH	-> G_DB_GET_BOTH
	  | JDB_HASH_SET_RECNO -> G_DB_SET_RECNO 
      end :: []

let bdb_flag_of_jungle_get_hash_comp_flag f = 
  match f with 
   | JDB_HASH_MULTIPLE 	 -> G_DB_MULTIPLE
   | JDB_HASH_DIRTY_READ -> G_DB_DIRTY_READ
   | JDB_HASH_RMW 		 -> G_DB_RMW

	
(* Common/Generic Cursor Flag *)
let bdb_flag_of_jungle_curopen_comp_flag f = 
  match f with
   | JDB_CUR_DIRTY_READ 	-> G_DB_DIRTY_READ 
   | JDB_CUR_WRITECURSOR 	-> G_DB_WRITECURSOR 

let bdb_flag_of_jungle_cur_get_comp_flag f = 
  match f with
   | JDB_CURGET_DIRTY_READ  	-> G_DB_DIRTY_READ
   | JDB_CURGET_MULTIPLE 		-> G_DB_MULTIPLE
   | JDB_CURGET_MULTIPLE_KEY 	-> G_DB_MULTIPLE_KEY
   | JDB_CURGET_RMW 			-> G_DB_RMW

(* Btree Cursor Flag *)
let bdb_flag_of_jungle_cur_put_btree_excl_flag f = 
  match f with 
    | None -> []
    | Some f -> 
	begin
	  match f with
	    | JDB_BTREE_CURPUT_AFTER     -> G_DB_AFTER
	    | JDB_BTREE_CURPUT_BEFORE 	-> G_DB_BEFORE
	    | JDB_BTREE_CURPUT_CURRENT 	-> G_DB_CURRENT
	    | JDB_BTREE_CURPUT_KEYFIRST 	-> G_DB_KEYFIRST
	    | JDB_BTREE_CURPUT_KEYLAST 	-> G_DB_KEYLAST
	    | JDB_BTREE_CURPUT_NODUPDATA -> G_DB_NODUPDATA
	end :: []

(* Recno Cursor Flag *)
let bdb_flag_of_jungle_cur_put_recno_excl_flag f =
  match f with 
    | None -> []
    | Some f ->
	begin
	  match f with
	    | JDB_RECNO_CURPUT_AFTER		->	G_DB_AFTER		
	    | JDB_RECNO_CURPUT_BEFORE 	->	G_DB_BEFORE 	
	    | JDB_RECNO_CURPUT_CURRENT 	->  G_DB_CURRENT
	end :: []


(* Hash Cursor Flag *)
let bdb_flag_of_jungle_cur_put_hash_excl_flag f = 
  match f with 
    | None -> []
    | Some f -> 
	begin
	  match f with
	    | JDB_HASH_CURPUT_AFTER      -> G_DB_AFTER
	    | JDB_HASH_CURPUT_BEFORE 	-> G_DB_BEFORE
	    | JDB_HASH_CURPUT_CURRENT 	-> G_DB_CURRENT
	    | JDB_HASH_CURPUT_KEYFIRST 	-> G_DB_KEYFIRST
	    | JDB_HASH_CURPUT_KEYLAST 	-> G_DB_KEYLAST
	    | JDB_HASH_CURPUT_NODUPDATA  -> G_DB_NODUPDATA
	end :: []


(************************************************************************)
(* 					Database Handle Definitions		 					*)
(************************************************************************)

type jungle_btree =
    { btree_handle : dbpointer;
      mutable btree_open_flag : bool }

type jungle_btree_key   = char array
type jungle_btree_value = char array
type jungle_btree_cursor = 
	{ btree_cursor_handle : cursor_pointer;
	  mutable btree_cursor_open_flag : bool }
 
type jungle_recno = 
    { recno_handle : recpointer;
      mutable recno_open_flag : bool }

type jungle_recno_key = int
type jungle_recno_value = char array

type jungle_recno_cursor = 
    { recno_cursor_handle : reccursor_pointer; 
      mutable recno_cursor_open_flag : bool }

type jungle_hash = 
	{ hash_handle : dbpointer;
	  mutable hash_open_flag : bool }
type jungle_hash_key = char array
type jungle_hash_value = char array

type jungle_hash_cursor = 
	{ hash_cursor_handle : cursor_pointer;
	  mutable hash_cursor_open_flag : bool }
 




(************************************************************************)
(* 					Functions on BTREE Database		 					*)
(************************************************************************)

let jungle_btree_open filename (gflags,bflags) bufsize =
  (* Converting the flags *)  
  let open_flags = List.map bdb_flag_of_jungle_open_flag gflags in
  let set_flags  = List.map bdb_flag_of_jungle_open_btree_comp_flag bflags in
  (* Ignoring the open_flags for now, not processed by the stub yet *)
  let (error,dbpointer) = galaxDBopen filename DB_BTREE open_flags set_flags bufsize in
  if error = 0
  then
    { btree_handle = dbpointer;
      btree_open_flag = true }
  else
    raise (Jungle_Error "Could not open Btree")

let jungle_btree_put btree key data put_btree_excl_flag = 
  if(btree.btree_open_flag) 
  then
    begin
      let put_flag = bdb_flag_of_jungle_put_btree_excl_flag put_btree_excl_flag in 
      (* Converting the flags *)
      let error = galaxDBput btree.btree_handle key data put_flag in 
  	if error = 0
  	then  ()
	else raise (Jungle_Error "Could not store in Btree")
    end
  else 
    raise (Jungle_Error "Btree not open")


let jungle_btree_getall btree key = 
  if (btree.btree_open_flag) 
  then
    (* We are opening a new cursor *each* time *)
    let error, cursor       = galaxDBCursor btree.btree_handle [] in
      if error = 0 then 
	begin
	  let (error, value) = galaxDBgetall_first cursor key in
	    (* Setup cursor state *)
	  let last_key   = ref key in
	  let last_value = ref value in
	  let last_error = ref error in 
	  let cursor_function () =    
	    if !last_error = 0 then 
	      begin
		let v = !last_value in
		let (new_error,new_key,new_value) = galaxDBgetall_next cursor in
		  last_error := new_error; last_key := new_key; last_value := new_value; 
		  (* Some (key,value) *)
		  Some v 
	      end
	    else if !last_error = const_DB_NOTFOUND 
	    then begin 
	      (* Close the cursor and leave *)
	      if galaxDBCurClose cursor = 0 then None 
	      else raise (Jungle_Error "Could not close Btree Cursor [getall]: Jungle.ml") 
	    end
	    else raise (Jungle_Error "Could not read from Btree Cursor [getall]: Jungle.ml") 
	  in
	    cursor_function 	   
	end
      else raise (Jungle_Error "Could not create a btree!")
  else
    raise (Jungle_Error "[btree_getall] Btree not open")

let jungle_btree_get btree key get_btree_excl_flag get_btree_comp_flags = 
  if(btree.btree_open_flag)
  then
    begin
      (* Converting the flags *)
      let comp_get_flags = List.map bdb_flag_of_jungle_get_btree_comp_flag get_btree_comp_flags in 
      let get_flags = (bdb_flag_of_jungle_get_btree_excl_flag get_btree_excl_flag) @ comp_get_flags  in 
      let (error, data) = galaxDBget btree.btree_handle key get_flags in 
  	if error = 0
  	then data 
	else if error = const_DB_NOTFOUND then 
	  begin	    
	    None
	  end
	else 
    	  raise (Jungle_Error ("Could not retreive data from Btree " ^ (string_of_int error)))
    end
  else   
    raise (Jungle_Error "Btree not open")

let jungle_btree_delete btree key value = 
  if(btree.btree_open_flag) 
  then
    begin
      let error = galaxDBdelete btree.btree_handle key value in
	if error = 0
	then
	  ()
	else 
    	  raise (Jungle_Error "Could not delete data from Btree")
    end
  else
    raise (Jungle_Error "Btree not open!")

let jungle_btree_delete_all btree key = 
  if(btree.btree_open_flag) 
  then
    begin
      let error = galaxDBdelete_all btree.btree_handle key in
	if error = 0
	then
	  ()
	else 
    	  raise (Jungle_Error "Could not delete data from Btree")
    end
  else
    raise (Jungle_Error "Btree not open!")

let jungle_btree_close btree =
  if (btree.btree_open_flag)
  then
    begin
    	let error = galaxDBclose btree.btree_handle in
        if error = 0
        then
        	btree.btree_open_flag <- false
	   	else 
    		raise (Jungle_Error "Could not close Btree")
    end
  else
    raise (Jungle_Error "Btree already closed!")

let jungle_btree_sync btree =
  if (btree.btree_open_flag)
  then
    begin
    	let error = galaxDBsync btree.btree_handle in
        if error = 0
        then
			()
	   	else 
    		raise (Jungle_Error "Could not sync Btree to harddisk")
    end
  else
    raise (Jungle_Error "Btree already closed!")



(***************************)
(* Btree cursor operations *)
(***************************)


let jungle_btree_cursor_open btree curbflags = 
   if(btree.btree_open_flag)
   then 
     begin
       (* Convert the flags *) 
        let cur_open_flags = List.map bdb_flag_of_jungle_curopen_comp_flag curbflags in 
        let (error, curpointer) = galaxDBCursor btree.btree_handle cur_open_flags in
        if error = 0
        then 
                { btree_cursor_handle = curpointer;
                  btree_cursor_open_flag = true }
        else 
                raise (Jungle_Error "Could not open Btree Cursor") 
     end
   else 
        raise (Jungle_Error "Cannot open cursor on closed Btree") 

(* This does not take array of flags *)
let jungle_btree_cursor_put btree_cursor key data curpflag = 
  if(btree_cursor.btree_cursor_open_flag) 
  then
    begin
      let curput_flag = bdb_flag_of_jungle_cur_put_btree_excl_flag curpflag in
      let error = galaxDBCurput btree_cursor.btree_cursor_handle key data curput_flag in 
	if error = 0
	then 
	  ()
	else 
          raise (Jungle_Error "Could not write in Btree Cursor") 
    end
  else 
    raise (Jungle_Error "Btree Cursor closed") 

let jungle_btree_cursor_get btree_cursor cur_get_excl_flag cur_get_comp_flags = 
  if(btree_cursor.btree_cursor_open_flag) 
  then
    begin
      let comp_cur_get_flags = List.map bdb_flag_of_jungle_cur_get_comp_flag cur_get_comp_flags in
      let cur_get_flags = cur_get_excl_flag :: comp_cur_get_flags   in  
      let (error, key, data) = galaxDBCurget btree_cursor.btree_cursor_handle cur_get_flags in
	
	if error = 0
	then
	  Some (key, data)
	else if error = const_DB_NOTFOUND then
	    None
	  else
            raise (Jungle_Error "Could not read from Btree Cursor: Jungle.ml") 
    end
  else
    raise (Jungle_Error "Btree Cursor closed") 

let jungle_btree_cursor_get_next btree_cursor cur_get_comp_flags = 
	jungle_btree_cursor_get btree_cursor G_DB_NEXT  cur_get_comp_flags

let jungle_btree_cursor_get_prev btree_cursor cur_get_comp_flags = 
	jungle_btree_cursor_get btree_cursor G_DB_PREV  cur_get_comp_flags

let jungle_btree_cursor_get_first btree_cursor cur_get_comp_flags =
	jungle_btree_cursor_get btree_cursor G_DB_FIRST cur_get_comp_flags

let jungle_btree_cursor_get_last btree_cursor cur_get_comp_flags = 
	jungle_btree_cursor_get btree_cursor G_DB_LAST cur_get_comp_flags


let internal_jungle_btree_cursor_get btree_cursor get_function search_key flag cur_get_comp_flags = 
 if(btree_cursor.btree_cursor_open_flag) 
  then
    begin
      let comp_cur_get_flags = List.map bdb_flag_of_jungle_cur_get_comp_flag cur_get_comp_flags in
      let cur_get_flags      = flag :: comp_cur_get_flags   in  
      let (error, key, data) = get_function btree_cursor.btree_cursor_handle search_key cur_get_flags in
	
	if error = 0
	then
	  Some (key, data)
	else if error = const_DB_NOTFOUND then
	    None
	  else
            raise (Jungle_Error "Could not read from Btree Cursor: Jungle.ml") 
    end
  else
    raise (Jungle_Error "Btree Cursor closed") 

let jungle_btree_cursor_get_set btree_cursor k cur_get_comp_flags = 
  internal_jungle_btree_cursor_get btree_cursor galaxDBCurget_set k G_DB_SET cur_get_comp_flags


let jungle_btree_cursor_get_set_range btree_cursor k cur_get_comp_flags = 
  internal_jungle_btree_cursor_get btree_cursor galaxDBCurget_set k G_DB_SET_RANGE cur_get_comp_flags

let galaxDBCurget_both_wrapper h (k,v) f = galaxDBCurget_both h k v f

let jungle_btree_cursor_get_both btree_cursor (k,v) cur_get_comp_flags = 
  internal_jungle_btree_cursor_get btree_cursor galaxDBCurget_both_wrapper (k,v) G_DB_GET_BOTH cur_get_comp_flags

let jungle_btree_cursor_get_both_range btree_cursor (k,v) cur_get_comp_flags = 
  internal_jungle_btree_cursor_get btree_cursor galaxDBCurget_both_wrapper (k,v) G_DB_GET_BOTH_RANGE cur_get_comp_flags

let jungle_btree_cursor_del btree_cursor = 
	if(btree_cursor.btree_cursor_open_flag) 
	then
	  begin
		let error = galaxDBCurdel btree_cursor.btree_cursor_handle in 
		if error = 0
		then
			()
		else 
			raise (Jungle_Error "Could not delete from Btree Cursor")
	  end
	else
        raise (Jungle_Error "Btree Cursor closed") 


let jungle_btree_cursor_close btree_cursor = 
	if(btree_cursor.btree_cursor_open_flag)
	then
	  begin
		let error = galaxDBCurClose btree_cursor.btree_cursor_handle in 
	    if error = 0 
		then
			btree_cursor.btree_cursor_open_flag <- false
		else 
			raise (Jungle_Error "Cannot Close Btree cursor, cursor already Close")
	  end
	else
        raise (Jungle_Error "Btree Cursor closed") 



(************************************************************************)
(* 					Functions on RECORD Database	 					*)
(************************************************************************)


let jungle_recno_open filename (gflags,rflags) bufsize bfixed length =
  (* Converting the flags *)
  let open_flags = List.map bdb_flag_of_jungle_open_flag gflags in
  let set_flags = List.map bdb_flag_of_jungle_open_recno_comp_flag rflags in
  let (error,recpointer) = galaxDBopen_rec filename open_flags set_flags bufsize bfixed length in
  if error = 0
  then
    { recno_handle = recpointer;
      recno_open_flag = true }
  else
    raise (Jungle_Error "Could not open Recno")

let jungle_recno_put record recno data put_recno_excl_flags = 
  if(record.recno_open_flag) 
  then
        begin
  		(* Converting the flags *)
  	  let put_flag = bdb_flag_of_jungle_put_recno_excl_flag put_recno_excl_flags in 
  	  let error = galaxDBput_rec record.recno_handle recno data put_flag in 
  	    if error = 0
  	    then
	      (* 
		 begin
		 put_count := !put_count + 1;
		 end
	      *)
			 ()
		else 
    		raise (Jungle_Error "Could not store in recno")
        end
  else 
    raise (Jungle_Error "Recno not open")

let jungle_recno_get record recno get_recno_excl_flag get_recno_comp_flags = 
  if(record.recno_open_flag)
  then
    begin
      (* Converting the flags *)
      let comp_get_flags = List.map bdb_flag_of_jungle_get_recno_comp_flag get_recno_comp_flags in 
      let get_flags      = comp_get_flags @ (bdb_flag_of_jungle_get_recno_excl_flag get_recno_excl_flag) in        
      let (error, data)  = 
	galaxDBget_rec record.recno_handle recno get_flags in 
  	if error = 0
  	then (* Some data *) data
	else if error = const_DB_NOTFOUND then None
	else raise (Jungle_Error "Could not retreive record")
    end
  else   
    raise (Jungle_Error "Recno not open")


let jungle_recno_get_unsafe record recno get_recno_excl_flags get_recno_comp_flags = 
  if(record.recno_open_flag)
  then
    begin
      (* Converting the flags *)
      (* let comp_get_flags = bdb_flag_of_jungle_get_recno_comp_flags_array get_recno_comp_flags in 
      let get_flags = Array.append comp_get_flags (bdb_flag_of_jungle_get_recno_excl_flags_array get_recno_excl_flags) in  *)
      let (error, data) =
	galaxDBget_rec record.recno_handle recno (List.map bdb_flag_of_jungle_get_recno_comp_flag get_recno_comp_flags) in 
  	if error = 0
  	then match data with None -> raise (Jungle_Error "Could not retreive record") | Some v -> v
	else raise (Jungle_Error "Could not retreive record")
    end
  else   
    raise (Jungle_Error "Recno not open")

let jungle_recno_delete record recno = 
  if(record.recno_open_flag) 
  then
    begin
      let error = galaxDBdel_rec record.recno_handle recno in
	if error = 0
	then
	  ()
	else 
    	  raise (Jungle_Error "Could not delete record from Recno")
    end
  else
    raise (Jungle_Error "Recno not open!")

let jungle_recno_close record =
  if (record.recno_open_flag)
  then
    begin
      let error = galaxDBclose_rec record.recno_handle in
        if error = 0
        then
          record.recno_open_flag <- false
	else 
    	  raise (Jungle_Error "Could not close Recno")
    end
  else
    raise (Jungle_Error "Recno already closed!")


let jungle_recno_close_no_sync record =
  if (record.recno_open_flag)
  then
    begin
      let error = galaxDBclose_rec record.recno_handle in
        if error = 0
        then
          record.recno_open_flag <- false
	else 
    	  raise (Jungle_Error "Could not close Recno")
    end
  else
    raise (Jungle_Error "Recno already closed!")


let jungle_recno_sync record =
  if (record.recno_open_flag)
  then
    begin
      let error = galaxDBsync_rec record.recno_handle in
        if error = 0
        then
	  ()
	else 
    	  raise (Jungle_Error "Could not sync Recno to disk")
    end
  else
    raise (Jungle_Error "Recno already closed!")


(***************************)
(* Recno cursor operations *)
(***************************)


let jungle_recno_cursor_open record currflags = 
   if(record.recno_open_flag)
   then 
     begin
       (* Convert the flags *) 
        let cur_open_flags = List.map bdb_flag_of_jungle_curopen_comp_flag currflags in 
        let (error, reccur_pointer) = galaxDBCursor_rec record.recno_handle cur_open_flags in
        if error = 0
        then 
                { recno_cursor_handle = reccur_pointer;
                  recno_cursor_open_flag = true }
        else 
                raise (Jungle_Error "Could not open Record Cursor") 
     end
   else 
        raise (Jungle_Error "Cannot open cursor on closed Recno") 


let jungle_recno_cursor_put recno_cursor key data curpflag = 
	if(recno_cursor.recno_cursor_open_flag) 
	then
	  begin

		let curput_flag = bdb_flag_of_jungle_cur_put_recno_excl_flag curpflag in
		let error = galaxDBCurput_rec recno_cursor.recno_cursor_handle data curput_flag in 
		if error = 0
		then 
			()
		else 
                raise (Jungle_Error "Could not write in Record Cursor") 
	  end
	else 
        raise (Jungle_Error "Record Cursor closed") 


let jungle_recno_cursor_get recno_cursor cur_get_recno_excl_flags cur_get_comp_flags = 
  if(recno_cursor.recno_cursor_open_flag) 
  then
    begin
      let comp_cur_get_flags = List.map bdb_flag_of_jungle_cur_get_comp_flag cur_get_comp_flags in
      let cur_get_flags = comp_cur_get_flags @ cur_get_recno_excl_flags in  
      let (error, data) = galaxDBCurget_rec recno_cursor.recno_cursor_handle cur_get_flags in	
	if error = 0
	then
	  Some data	
	else if error = const_DB_NOTFOUND then
	  None
	else	   
          raise (Jungle_Error "Could not read from Recno Cursor") 
    end
  else
    raise (Jungle_Error "Recno Cursor closed") 

let jungle_recno_cursor_get_next recno_cursor cur_get_comp_flags = 
	jungle_recno_cursor_get recno_cursor [ G_DB_NEXT ] cur_get_comp_flags

let jungle_recno_cursor_get_prev recno_cursor cur_get_comp_flags = 
	jungle_recno_cursor_get recno_cursor [ G_DB_PREV ] cur_get_comp_flags

let jungle_recno_cursor_get_first recno_cursor cur_get_comp_flags = 
	jungle_recno_cursor_get recno_cursor [ G_DB_FIRST ] cur_get_comp_flags

let jungle_recno_cursor_get_last recno_cursor cur_get_comp_flags = 
	jungle_recno_cursor_get recno_cursor [ G_DB_LAST ] cur_get_comp_flags

let jungle_recno_cursor_get_next_dup recno_cursor cur_get_comp_flags = 
	jungle_recno_cursor_get recno_cursor [ G_DB_NEXT_DUP ] cur_get_comp_flags

let jungle_recno_cursor_get_set recno_cursor recno cur_get_comp_flags = 
  if(recno_cursor.recno_cursor_open_flag) then
    begin
      let comp_cur_get_flags = List.map bdb_flag_of_jungle_cur_get_comp_flag cur_get_comp_flags in
      let cur_get_flags = G_DB_SET :: comp_cur_get_flags  in  
      let (error, data) = galaxDBCurget_set_rec recno_cursor.recno_cursor_handle recno cur_get_flags in	
	if error = 0
	then
	  data	
	else if error = const_DB_NOTFOUND then
	  None
	else	   
          raise (Jungle_Error "Could not read from Recno Cursor") 
    end
  else
    raise (Jungle_Error "Recno Cursor closed") 

let jungle_recno_cursor_del recno_cursor = 
	if(recno_cursor.recno_cursor_open_flag) 
	then
	  begin
		let error = galaxDBCurdel_rec recno_cursor.recno_cursor_handle in 
		if error = 0
		then
			()
		else 
			raise (Jungle_Error "Could not delete from Recno Cursor")
	  end
	else
        raise (Jungle_Error "Recno Cursor closed") 

let jungle_recno_cursor_close recno_cursor =
	if(recno_cursor.recno_cursor_open_flag)
	then
	  begin
		let error = galaxDBCurClose_rec recno_cursor.recno_cursor_handle in 
	    if error = 0 
		then
			recno_cursor.recno_cursor_open_flag <- false
		else 
			raise (Jungle_Error "Cannot Close Recno cursor")
	  end
	else
        raise (Jungle_Error "Recno Cursor already closed") 





(************************************************************************)
(* 					Functions on Hashtable Database 					*)
(************************************************************************)

let jungle_hash_open filename (gflags,hflags) bufsize =
  (* Converting the flags *)
  let open_flag = List.map bdb_flag_of_jungle_open_flag gflags in
  let set_flags = List.map bdb_flag_of_jungle_open_hash_comp_flag hflags in
  (* Ignoring the open_flags for now, not processed by the stub yet *)
  let (error,dbpointer) = galaxDBopen filename DB_HASH open_flag set_flags bufsize in
  if error = 0
  then
    { hash_handle = dbpointer;
      hash_open_flag = true }
  else
    raise (Jungle_Error "Could not open hash table")

let jungle_hash_put hashtbl key data put_hash_excl_flags = 
  if(hashtbl.hash_open_flag) 
  then
    begin
      (* Converting the flags *)
      let put_flags = bdb_flag_of_jungle_put_hash_excl_flag put_hash_excl_flags in
  	  let error = galaxDBput hashtbl.hash_handle key data put_flags in 
  	    if error = 0
  	    then  ()
	    else 
    	      raise (Jungle_Error "Could not store in Hashtable")
    end
  else 
    raise (Jungle_Error "Hash table not open")

let jungle_hash_get hashtbl key get_hash_excl_flags get_hash_comp_flags = 
  if(hashtbl.hash_open_flag)
  then
        begin
  		(* Converting the flags *)
  		let comp_get_flags = List.map bdb_flag_of_jungle_get_hash_comp_flag get_hash_comp_flags in 
  		let get_flags = comp_get_flags @ (bdb_flag_of_jungle_get_hash_excl_flag get_hash_excl_flags) in 
  		let (error, data) = galaxDBget hashtbl.hash_handle key get_flags in 
  		if error = 0 then 
		  data (* is an option type *)
		else if error = const_DB_NOTFOUND then None
		    else 
    		      raise (Jungle_Error "Could not retreive data from Hash Table [safe]")
        end
  else   
    raise (Jungle_Error "Hash Table not open")


let jungle_hash_getall hash key = 
  if (hash.hash_open_flag) 
  then
    (* We are opening a new cursor *each* time *)
    let error, cursor       = galaxDBCursor hash.hash_handle [] in
      if error = 0 then 
	begin
	  let (error, value) = galaxDBgetall_first cursor key in
	    (* Setup cursor state *)
	  let last_key   = ref key in
	  let last_value = ref value in
	  let last_error = ref error in 
	  let cursor_function () =    
	    if !last_error = 0 then 
	      begin
		let v = !last_value in
		let (new_error,new_key,new_value) = galaxDBgetall_next cursor in
		  last_error := new_error; last_key := new_key; last_value := new_value; 
		  (* Some (key,value) *)
		  Some v 
	      end
	    else if !last_error = const_DB_NOTFOUND 
	    then begin 
	      (* Close the cursor and leave *)
	      if galaxDBCurClose cursor = 0 then None 
	      else raise (Jungle_Error "Could not close Hash Cursor [getall]: Jungle.ml") 
	    end
	    else raise (Jungle_Error "Could not read from Hash Cursor [getall]: Jungle.ml") 
	  in
	    cursor_function 	   
	end
      else raise (Jungle_Error "Could not create a btree!")
  else
    raise (Jungle_Error "[btree_getall] Btree not open")


let jungle_hash_get_unsafe hashtbl key get_hash_excl_flags get_hash_comp_flags = 
  if(hashtbl.hash_open_flag)
  then
        begin
  		(* Converting the flags *)
  		let comp_get_flags = List.map bdb_flag_of_jungle_get_hash_comp_flag get_hash_comp_flags in 
  		let get_flags = comp_get_flags @ (bdb_flag_of_jungle_get_hash_excl_flag get_hash_excl_flags) in 
  		let (error, data) = galaxDBget hashtbl.hash_handle key get_flags in 
  		if error = 0
  		then 
			match data with 
			  | None -> raise (Jungle_Error "Could not retreive data from Hash Table ")
			  | Some v -> v 
		else 
    		raise (Jungle_Error "Could not retreive data from Hash Table ")
        end
  else   
    raise (Jungle_Error "Hash Table not open")

let jungle_hash_delete hashtbl key = 
  if(hashtbl.hash_open_flag) 
  then
	begin
		let error = galaxDBdel hashtbl.hash_handle key in
		if error = 0
		then
			()
		else 
    		raise (Jungle_Error "Could not delete data from Hashtable")
	end
  else
    raise (Jungle_Error "Hash Table not open!")

let jungle_hash_close hashtbl =
  if (hashtbl.hash_open_flag)
  then
    begin
    	let error = galaxDBclose hashtbl.hash_handle in
        if error = 0
        then
        	hashtbl.hash_open_flag <- false
	   	else 
    		raise (Jungle_Error "Could not close Hashtable")
    end
  else
    raise (Jungle_Error "Hashtable already closed!")

let jungle_hash_sync hashtbl =
  if (hashtbl.hash_open_flag)
  then
    begin
    	let error = galaxDBsync hashtbl.hash_handle in
        if error = 0
        then
			()
	   	else 
    		raise (Jungle_Error "Could not sync Hashtable to disk")
    end
  else
    raise (Jungle_Error "Hashtable already closed!")



(***************************)
(* Hash cursor operations *)
(***************************)


let jungle_hash_cursor_open hash curbflags = 
   if(hash.hash_open_flag)
   then 
     begin
       (* Convert the flags *) 
        let cur_open_flags = List.map bdb_flag_of_jungle_curopen_comp_flag curbflags in 
        let (error, curpointer) = galaxDBCursor hash.hash_handle cur_open_flags in
        if error = 0
        then 
                { hash_cursor_handle = curpointer;
                  hash_cursor_open_flag = true }
        else 
                raise (Jungle_Error "Could not open Btree Cursor") 
     end
   else 
        raise (Jungle_Error "Cannot open cursor on closed Btree") 

(* This does not take array of flags *)
let jungle_hash_cursor_put hash_cursor key data curpflag = 
	if(hash_cursor.hash_cursor_open_flag) 
	then
	  begin
		let curput_flag = bdb_flag_of_jungle_cur_put_hash_excl_flag curpflag in
		let error = galaxDBCurput hash_cursor.hash_cursor_handle key data curput_flag in 
		if error = 0
		then 
			()
		else 
                raise (Jungle_Error "Could not write in Btree Cursor") 
	  end
	else 
        raise (Jungle_Error "Btree Cursor closed") 

let jungle_hash_cursor_get hash_cursor cur_get_excl_flag cur_get_comp_flags = 
  if(hash_cursor.hash_cursor_open_flag) 
  then
    begin
      let comp_cur_get_flags = List.map bdb_flag_of_jungle_cur_get_comp_flag cur_get_comp_flags in
      let cur_get_flags = cur_get_excl_flag :: comp_cur_get_flags in  
      let (error, key, data) = galaxDBCurget hash_cursor.hash_cursor_handle cur_get_flags in
	if error = 0 then Some (key, data)
	else if error = const_DB_NOTFOUND then None
	else raise (Jungle_Error "Could not read from Hash Cursor") 
    end
  else
    raise (Jungle_Error "Hash Cursor closed") 

let jungle_hash_cursor_get_next hash_cursor cur_get_comp_flags = 
	jungle_hash_cursor_get hash_cursor  G_DB_NEXT  cur_get_comp_flags

let jungle_hash_cursor_get_prev hash_cursor cur_get_comp_flags = 
	jungle_hash_cursor_get hash_cursor  G_DB_PREV  cur_get_comp_flags

let jungle_hash_cursor_get_first hash_cursor cur_get_comp_flags =
	jungle_hash_cursor_get hash_cursor  G_DB_FIRST  cur_get_comp_flags

let jungle_hash_cursor_get_last hash_cursor cur_get_comp_flags = 
	jungle_hash_cursor_get hash_cursor  G_DB_LAST  cur_get_comp_flags

let jungle_hash_cursor_get_set hash_cursor k cur_get_comp_flags = 
  if(hash_cursor.hash_cursor_open_flag) 
  then
    begin
      let comp_cur_get_flags = List.map bdb_flag_of_jungle_cur_get_comp_flag cur_get_comp_flags in
      let cur_get_flags = G_DB_SET :: comp_cur_get_flags in  
      let (error, key, data) = galaxDBCurget_set hash_cursor.hash_cursor_handle k cur_get_flags in
	if error = 0 then Some (key, data)
	else if error = const_DB_NOTFOUND then None
	else raise (Jungle_Error "Could not read from Hash Cursor") 
    end
  else
    raise (Jungle_Error "Hash Cursor closed") 

let jungle_hash_cursor_get_both hash_cursor (k,v) cur_get_comp_flags = 
  if(hash_cursor.hash_cursor_open_flag) then
    begin
      let comp_cur_get_flags = List.map bdb_flag_of_jungle_cur_get_comp_flag cur_get_comp_flags in
      let cur_get_flags = G_DB_GET_BOTH :: comp_cur_get_flags in  
      let (error, key, data) = galaxDBCurget_both hash_cursor.hash_cursor_handle k v cur_get_flags in
	if error = 0 then Some (key, data)
	else if error = const_DB_NOTFOUND then None
	else raise (Jungle_Error "Could not read from Hash Cursor") 
    end
  else
    raise (Jungle_Error "Hash Cursor closed") 


let jungle_hash_cursor_del hash_cursor = 
  if(hash_cursor.hash_cursor_open_flag) 
  then
    begin
      let error = galaxDBCurdel hash_cursor.hash_cursor_handle in 
	if error = 0
	then
	  ()
	else 
	  raise (Jungle_Error "Could not delete from Btree Cursor")
    end
  else
    raise (Jungle_Error "Btree Cursor closed") 


let jungle_hash_cursor_close hash_cursor = 
	if(hash_cursor.hash_cursor_open_flag)
	then
	  begin
		let error = galaxDBCurClose hash_cursor.hash_cursor_handle in 
	    if error = 0 
		then
			hash_cursor.hash_cursor_open_flag <- false
		else 
			raise (Jungle_Error "Cannot Close Btree cursor, cursor already Close")
	  end
	else
        raise (Jungle_Error "Btree Cursor closed") 


(************************************************************************)
(* 					Functions to Create Secondary Index					*)
(************************************************************************)

(********************************)
(* Secondary Index Type = BTREE *)
(********************************)

(* Primary Database Type = Btree *)

let jungle_btree_primary_btree_sec_index_open btree secfilename (callback_func:string->string->string) (gflags,bflags) bsize = 
  let open_flags = List.map bdb_flag_of_jungle_open_flag gflags in
  let set_flags  = List.map bdb_flag_of_jungle_open_btree_comp_flag bflags in
  let (error1, sec_dbpointer) = galaxDBopen secfilename DB_BTREE open_flags set_flags bsize in 
    if error1 = 0
    then
      begin
	let callback_func_name = get_func_name count in 
	  Callback.register callback_func_name callback_func;
	  let error2 = galaxDBassociate btree.btree_handle sec_dbpointer callback_func_name [ G_DB_CREATE ] in 
	    if error2 = 0 
	    then 
    	      { btree_handle = sec_dbpointer;
      		btree_open_flag = true }
	    else 
	      raise (Jungle_Error "Could not create Btree Secondary Index for the Btree Database")
      end
    else 
      raise (Jungle_Error "Could not open Btree Secondary Index file")

(* Primary Database Type = Recno *)

let jungle_recno_primary_btree_sec_index_open record secfilename callback_func (gflags,bflags) bsize = 
  let open_flags = List.map bdb_flag_of_jungle_open_flag gflags in
  let set_flags  = List.map bdb_flag_of_jungle_open_btree_comp_flag bflags in
  let (error1, sec_dbpointer) = galaxDBopen secfilename DB_BTREE open_flags set_flags bsize in 
  if error1 = 0
  then
	begin
		let callback_func_name = get_func_name count in 
		Callback.register callback_func_name callback_func;
		let error2 = galaxDBassociate_with_rec record.recno_handle sec_dbpointer callback_func_name [ G_DB_CREATE ] in 
		if error2 = 0 
		then 
    	{ btree_handle = sec_dbpointer;
      	  btree_open_flag = true }
		else 
		  raise (Jungle_Error "Could not create Btree Secondary Index for the Recno Database")
	end
  else 
	raise (Jungle_Error "Could not open Btree Secondary Index file")

(* Primary Database Type = Hash *)

let jungle_hash_primary_btree_sec_index_open hashtbl secfilename callback_func (gflags,bflags) bsize = 
  let open_flags = List.map bdb_flag_of_jungle_open_flag gflags in
  let set_flags  = List.map bdb_flag_of_jungle_open_btree_comp_flag bflags in
  let (error1, sec_dbpointer) = galaxDBopen secfilename DB_BTREE open_flags set_flags bsize in 
  if error1 = 0
  then
	begin
		let callback_func_name = get_func_name count in 
		Callback.register callback_func_name callback_func;
		let error2 = galaxDBassociate hashtbl.hash_handle sec_dbpointer callback_func_name [ G_DB_CREATE ] in 
		if error2 = 0 
		then 
    	{ btree_handle = sec_dbpointer;
      	  btree_open_flag = true }
		else 
		  raise (Jungle_Error "Could not create Btree Secondary Index for the Hash Database")
	end
  else 
	raise (Jungle_Error "Could not open Btree Secondary Index file")

(********************************)
(* Secondary Index Type = Hash  *)
(********************************)

(* Primary Database Type = Btree *)

let jungle_btree_primary_hash_sec_index_open btree secfilename callback_func (gflags,hflags) bsize = 
  let open_flags = List.map bdb_flag_of_jungle_open_flag gflags in
  let set_flags  = List.map bdb_flag_of_jungle_open_hash_comp_flag hflags in
  let (error1, sec_dbpointer) = galaxDBopen secfilename DB_HASH open_flags set_flags bsize in 
  if error1 = 0
  then
	begin
		let callback_func_name = get_func_name count in 
		Callback.register callback_func_name callback_func;
		let error2 = galaxDBassociate btree.btree_handle sec_dbpointer callback_func_name [ G_DB_CREATE ] in 
		if error2 = 0 
		then 
    	{ hash_handle = sec_dbpointer;
      	  hash_open_flag = true }
		else 
		  raise (Jungle_Error "Could not create Hash Secondary Index for the Btree Database")
	end
  else 
	raise (Jungle_Error "Could not open Hash Secondary Index file")

(* Primary Database Type = Recno *)

let jungle_recno_primary_hash_sec_index_open record secfilename callback_func (gflags,hflags) bsize = 
  let open_flags = List.map bdb_flag_of_jungle_open_flag gflags in
  let set_flags  = List.map bdb_flag_of_jungle_open_hash_comp_flag hflags in
  let (error1, sec_dbpointer) = galaxDBopen secfilename DB_HASH open_flags set_flags bsize in 
  if error1 = 0
  then
	begin
		let callback_func_name = get_func_name count in 
		Callback.register callback_func_name callback_func;
		let error2 = galaxDBassociate_with_rec record.recno_handle sec_dbpointer callback_func_name [ G_DB_CREATE ] in 
		if error2 = 0 
		then 
    	{ hash_handle = sec_dbpointer;
      	  hash_open_flag = true }
		else 
		  raise (Jungle_Error "Could not create Hash Secondary Index for the Recno Database")
	end
  else 
	raise (Jungle_Error "Could not open Hash Secondary Index file")

(* Primary Database Type = Hash *)

let jungle_hash_primary_hash_sec_index_open hashtbl secfilename callback_func (gflags,hflags) bsize = 
  let open_flags = List.map bdb_flag_of_jungle_open_flag gflags in
  let set_flags  = List.map bdb_flag_of_jungle_open_hash_comp_flag hflags in
  let (error1, sec_dbpointer) = galaxDBopen secfilename DB_HASH open_flags set_flags bsize in 
  if error1 = 0
  then
	begin
		let callback_func_name = get_func_name count in 
		Callback.register callback_func_name callback_func;
		let error2 = galaxDBassociate hashtbl.hash_handle sec_dbpointer callback_func_name [ G_DB_CREATE ] in 
		if error2 = 0 
		then 
    	{ hash_handle = sec_dbpointer;
      	  hash_open_flag = true }
		else 
		  raise (Jungle_Error "Could not create Hash Secondary Index for the Hash Database")
	end
  else 
	raise (Jungle_Error "Could not open Hash Secondary Index file")


(********************************)
(* Secondary Index Type = Recno *)
(********************************)

(* Primary Database Type = Btree *)

let jungle_btree_primary_recno_sec_index_open btree secfilename callback_func (gflags,rflags) bsize = 
  let open_flags = List.map bdb_flag_of_jungle_open_flag gflags in
  let set_flags  = List.map bdb_flag_of_jungle_open_recno_comp_flag rflags in
  let (error1, sec_dbpointer) = galaxDBopen_rec secfilename open_flags set_flags bsize false (-1) in 
  if error1 = 0
  then
	begin
		let callback_func_name = get_func_name count in 
		Callback.register callback_func_name callback_func;
		let error2 = galaxDBassociate_rec btree.btree_handle sec_dbpointer callback_func_name [ G_DB_CREATE ] in 
		if error2 = 0 
		then 
    	{ recno_handle = sec_dbpointer;
      	  recno_open_flag = true }
		else 
		  raise (Jungle_Error "Could not create Recno Secondary Index for the Btree Database")
	end
  else 
	raise (Jungle_Error "Could not open Recno Secondary Index file")

(* Primary Database Type = Recno *)

let jungle_recno_primary_recno_sec_index_open record secfilename callback_func (gflags,rflags) bsize = 
  let open_flags = List.map bdb_flag_of_jungle_open_flag gflags in
  let set_flags  = List.map bdb_flag_of_jungle_open_recno_comp_flag rflags in
  let (error1, sec_dbpointer) = galaxDBopen_rec secfilename open_flags set_flags bsize false (-1) in 
  if error1 = 0
  then
	begin
		let callback_func_name = get_func_name count in 
		Callback.register callback_func_name callback_func;
		let error2 = galaxDBassociate_rec_with_rec record.recno_handle sec_dbpointer callback_func_name [ G_DB_CREATE ] in 
		if error2 = 0 
		then 
    	{ recno_handle = sec_dbpointer;
      	  recno_open_flag = true }
		else 
		  raise (Jungle_Error "Could not create Recno Secondary Index for the Recno Database")
	end
  else 
	raise (Jungle_Error "Could not open Recno Secondary Index file")

(* Primary Database Type = Hash *)

let jungle_hash_primary_recno_sec_index_open hashtbl secfilename callback_func (gflags,rflags) bsize = 
  let open_flags = List.map bdb_flag_of_jungle_open_flag gflags in
  let set_flags  = List.map bdb_flag_of_jungle_open_recno_comp_flag rflags in
  let (error1, sec_dbpointer) = galaxDBopen_rec secfilename open_flags set_flags bsize false (-1) in 
  if error1 = 0
  then
	begin
		let callback_func_name = get_func_name count in 
		Callback.register callback_func_name callback_func;
		let error2 = galaxDBassociate_rec hashtbl.hash_handle sec_dbpointer callback_func_name [ G_DB_CREATE ] in 
		if error2 = 0 
		then 
    	{ recno_handle = sec_dbpointer;
      	  recno_open_flag = true }
		else 
		  raise (Jungle_Error "Could not create Recno Secondary Index for the Hash Database")
	end
  else 
	raise (Jungle_Error "Could not open Recno Secondary Index file")

