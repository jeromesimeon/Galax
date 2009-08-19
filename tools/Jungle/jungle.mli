(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* A generic exception for now *)

exception Jungle_Error of string

(* Methods for retrieving profilling information *)

val get_put_count : unit -> int

val get_get_count : unit -> int

(* Generic open flags for all kinds of index *)

type jungle_open_flags =
  | JDB_CREATE
  | JDB_EXCL
  | JDB_DIRTY_READ
  | JDB_RDONLY
  | JDB_THREAD

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

(* BTREE Cursor Flags *)
type jungle_curopen_comp_flags = 
   | JDB_CUR_DIRTY_READ
   | JDB_CUR_WRITECURSOR

type jungle_cur_put_btree_excl_flags =
   | JDB_BTREE_CURPUT_AFTER
   | JDB_BTREE_CURPUT_BEFORE
   | JDB_BTREE_CURPUT_CURRENT
   | JDB_BTREE_CURPUT_KEYFIRST
   | JDB_BTREE_CURPUT_KEYLAST
   | JDB_BTREE_CURPUT_NODUPDATA

(* Generic curget flags *)
type jungle_cur_get_comp_flags = 
   | JDB_CURGET_DIRTY_READ
   | JDB_CURGET_MULTIPLE
   | JDB_CURGET_MULTIPLE_KEY
   | JDB_CURGET_RMW

type jungle_cur_put_hash_excl_flags = 
   | JDB_HASH_CURPUT_AFTER
   | JDB_HASH_CURPUT_BEFORE
   | JDB_HASH_CURPUT_CURRENT
   | JDB_HASH_CURPUT_KEYFIRST
   | JDB_HASH_CURPUT_KEYLAST
   | JDB_HASH_CURPUT_NODUPDATA

  
(* Operations on BTrees *)

type jungle_btree
type jungle_btree_key   = char array
type jungle_btree_value = char array

val jungle_btree_open :
  string -> jungle_open_flags list * jungle_open_btree_comp_flags list -> int -> jungle_btree
	(* Takes the name of the file open flags and returns a handle
	   on a new btree *)

val jungle_btree_put : 
  jungle_btree -> jungle_btree_key -> jungle_btree_value -> jungle_put_btree_excl_flags option -> unit 

val jungle_btree_get :
  jungle_btree -> jungle_btree_key -> jungle_get_btree_excl_flags option -> 
  jungle_get_btree_comp_flags list -> jungle_btree_value option

(* Return a function suitable to be used in a cursor *)
val jungle_btree_getall :
  jungle_btree -> jungle_btree_key -> (unit -> jungle_btree_value option)
  
val jungle_btree_delete     : jungle_btree -> jungle_btree_key -> jungle_btree_value -> unit
val jungle_btree_delete_all : jungle_btree -> jungle_btree_key -> unit

val jungle_btree_close :
        jungle_btree -> unit 

val jungle_btree_sync :
        jungle_btree -> unit 

type jungle_btree_cursor  

val jungle_btree_cursor_open :
	jungle_btree -> jungle_curopen_comp_flags list -> jungle_btree_cursor

val jungle_btree_cursor_put :
	jungle_btree_cursor -> jungle_btree_key -> jungle_btree_value -> jungle_cur_put_btree_excl_flags option -> unit

val jungle_btree_cursor_get_next :
	jungle_btree_cursor -> jungle_cur_get_comp_flags list -> (jungle_btree_key * jungle_btree_value) option

val jungle_btree_cursor_get_prev :
	jungle_btree_cursor -> jungle_cur_get_comp_flags list -> (jungle_btree_key * jungle_btree_value) option

val jungle_btree_cursor_get_first :
	jungle_btree_cursor -> jungle_cur_get_comp_flags list -> (jungle_btree_key * jungle_btree_value) option

val jungle_btree_cursor_get_last :
	jungle_btree_cursor -> jungle_cur_get_comp_flags list -> (jungle_btree_key * jungle_btree_value) option

(**************************************)
(* These are the actual get functions *)
(**************************************)
val jungle_btree_cursor_get_set :
	jungle_btree_cursor -> jungle_btree_key -> jungle_cur_get_comp_flags list -> (jungle_btree_key * jungle_btree_value) option

val jungle_btree_cursor_get_set_range :
  jungle_btree_cursor -> jungle_btree_key  -> jungle_cur_get_comp_flags list ->  (jungle_btree_key * jungle_btree_value) option

val jungle_btree_cursor_get_both : 
  jungle_btree_cursor -> jungle_btree_key * jungle_btree_value  -> jungle_cur_get_comp_flags list ->  (jungle_btree_key * jungle_btree_value) option

val jungle_btree_cursor_get_both_range : 
  jungle_btree_cursor -> jungle_btree_key * jungle_btree_value  -> jungle_cur_get_comp_flags list ->  (jungle_btree_key * jungle_btree_value) option

val jungle_btree_cursor_del :
	jungle_btree_cursor -> unit

val jungle_btree_cursor_close :
	jungle_btree_cursor -> unit

(* Operations on Recno *)

type jungle_recno
type jungle_recno_key = int
type jungle_recno_value = char array
type jungle_open_recno_comp_flags = 
  | JDB_RECNO_RENUMBER
  | JDB_RECNO_SNAPSHOT

type jungle_put_recno_excl_flags = 
  | JDB_RECNO_APPEND
  | JDB_RECNO_NOOVERWRITE	
   (* Cannot specify both of above at the same time *)

type jungle_get_recno_excl_flags = 
  | JDB_RECNO_GET_BOTH

type jungle_get_recno_comp_flags = 
  | JDB_RECNO_MULTIPLE
  | JDB_RECNO_DIRTY_READ
  | JDB_RECNO_RMW

type jungle_cur_put_recno_excl_flags =
   | JDB_RECNO_CURPUT_AFTER		(*Check DB_RENUMBER_FLAG in create*)
   | JDB_RECNO_CURPUT_BEFORE 	(*Check DB_RENUMBER_FLAG in create*)
   | JDB_RECNO_CURPUT_CURRENT

val jungle_recno_open :
    string -> jungle_open_flags list * jungle_open_recno_comp_flags list -> int -> bool -> int -> jungle_recno
	(* Takes the name of the file open flags and returns a handle
	   on a new btree *)

val jungle_recno_put : 
	jungle_recno -> jungle_recno_key -> jungle_recno_value -> jungle_put_recno_excl_flags option -> unit 

val jungle_recno_get :
  jungle_recno -> jungle_recno_key -> jungle_get_recno_excl_flags option -> 
  jungle_get_recno_comp_flags list -> jungle_recno_value option

val jungle_recno_get_unsafe :
  jungle_recno -> jungle_recno_key -> jungle_get_recno_excl_flags option -> 
  jungle_get_recno_comp_flags list -> jungle_recno_value 

val jungle_recno_delete :
	jungle_recno -> int -> unit

val jungle_recno_close : jungle_recno -> unit
val jungle_recno_close_no_sync : jungle_recno -> unit

val jungle_recno_sync :
	jungle_recno -> unit

type jungle_recno_cursor

val jungle_recno_cursor_open :
	jungle_recno -> jungle_curopen_comp_flags list -> jungle_recno_cursor

val jungle_recno_cursor_put :
	jungle_recno_cursor -> jungle_recno_key -> jungle_recno_value -> jungle_cur_put_recno_excl_flags option -> unit

val jungle_recno_cursor_get_next :
	jungle_recno_cursor -> jungle_cur_get_comp_flags list -> jungle_recno_value option

val jungle_recno_cursor_get_prev :
	jungle_recno_cursor -> jungle_cur_get_comp_flags list -> jungle_recno_value option

val jungle_recno_cursor_get_first:
	jungle_recno_cursor -> jungle_cur_get_comp_flags list -> jungle_recno_value option

val jungle_recno_cursor_get_last :
	jungle_recno_cursor -> jungle_cur_get_comp_flags list -> jungle_recno_value option

val jungle_recno_cursor_get_next_dup :
	jungle_recno_cursor -> jungle_cur_get_comp_flags list -> jungle_recno_value option

 
val jungle_recno_cursor_get_set :
	jungle_recno_cursor  -> jungle_recno_key -> jungle_cur_get_comp_flags list -> jungle_recno_value option

val jungle_recno_cursor_del :
	jungle_recno_cursor -> unit

val jungle_recno_cursor_close :
	jungle_recno_cursor -> unit


(* Operations on Hash *)

type jungle_hash
type jungle_hash_key = char array
type jungle_hash_value = char array

(* Hashtable Specific Flags *)

type jungle_open_hash_comp_flags =
  | JDB_HASH_DUP
  | JDB_HASH_DUPSORT

type jungle_put_hash_excl_flags = 
  | JDB_HASH_NOOVERWRITE	
  | JDB_HASH_NODUPDATA
   (* Cannot specify both of above at the same time *)

type jungle_get_hash_excl_flags = 
   | JDB_HASH_GET_BOTH
   | JDB_HASH_SET_RECNO 
   (* Cannot specify both of above at the same time *)

type jungle_get_hash_comp_flags = 
   | JDB_HASH_MULTIPLE
   | JDB_HASH_DIRTY_READ
   | JDB_HASH_RMW

val jungle_hash_open : 
    string -> jungle_open_flags list * jungle_open_hash_comp_flags list -> int -> jungle_hash

val jungle_hash_put : 
	jungle_hash -> jungle_hash_key -> jungle_hash_value -> jungle_put_hash_excl_flags option -> unit 

val jungle_hash_get : 
	jungle_hash -> jungle_hash_key -> jungle_get_hash_excl_flags option -> jungle_get_hash_comp_flags list -> jungle_hash_value option

(* Return a function suitable to be used in a cursor *)
val jungle_hash_getall : 
	jungle_hash -> jungle_hash_key -> (unit -> jungle_btree_value option)

val jungle_hash_get_unsafe : 
	jungle_hash -> jungle_hash_key -> jungle_get_hash_excl_flags option -> jungle_get_hash_comp_flags list -> jungle_hash_value 

val jungle_hash_delete :
	jungle_hash -> jungle_hash_key -> unit
 
val jungle_hash_close :
	jungle_hash -> unit 

val jungle_hash_sync :
	jungle_hash -> unit 

type jungle_hash_cursor  

val jungle_hash_cursor_open :
	jungle_hash -> jungle_curopen_comp_flags list -> jungle_hash_cursor

val jungle_hash_cursor_put :
	jungle_hash_cursor -> char array -> char array -> jungle_cur_put_hash_excl_flags option -> unit

val jungle_hash_cursor_get_next :
	jungle_hash_cursor -> jungle_cur_get_comp_flags list -> (jungle_hash_key * jungle_hash_value) option

val jungle_hash_cursor_get_prev :
	jungle_hash_cursor -> jungle_cur_get_comp_flags list -> (jungle_hash_key * jungle_hash_value) option

val jungle_hash_cursor_get_first :
	jungle_hash_cursor -> jungle_cur_get_comp_flags list -> (jungle_hash_key * jungle_hash_value) option

val jungle_hash_cursor_get_last :
	jungle_hash_cursor -> jungle_cur_get_comp_flags list -> (jungle_hash_key * jungle_hash_value) option

(* Both of these need key as additional parameter *)
val jungle_hash_cursor_get_set :
	jungle_hash_cursor -> jungle_hash_key -> jungle_cur_get_comp_flags list -> (jungle_hash_key * jungle_hash_value) option

val jungle_hash_cursor_get_both :
	jungle_hash_cursor -> (jungle_hash_key * jungle_hash_value) -> jungle_cur_get_comp_flags list -> (jungle_hash_key * jungle_hash_value) option

val jungle_hash_cursor_del :
	jungle_hash_cursor -> unit

val jungle_hash_cursor_close :
	jungle_hash_cursor -> unit

(* Create Secondary Index of type BTREE *)
val jungle_btree_primary_btree_sec_index_open :
    jungle_btree-> string -> (string -> string -> string) -> jungle_open_flags list * jungle_open_btree_comp_flags list -> int -> jungle_btree
	(* Takes the primary btree database pointer, secondary index filename, 
	   name of the call-back function, open flags and return a handle to 
       new btree secondary index *)

val jungle_recno_primary_btree_sec_index_open :
    jungle_recno -> string -> (string -> string) -> jungle_open_flags list * jungle_open_btree_comp_flags list -> int -> jungle_btree
	(* Same as the previous but the primary database pointer is  
       recno instead of btree *)

val jungle_hash_primary_btree_sec_index_open :
    jungle_hash-> string -> (string -> string -> string) -> jungle_open_flags list * jungle_open_btree_comp_flags list -> int -> jungle_btree
	(* Same as the previous but the primary database pointer is  
       recno instead of btree *)


(* Create Secondary Index of type Hash *)
val jungle_btree_primary_hash_sec_index_open :
    jungle_btree-> string -> (string -> string -> string) -> jungle_open_flags list * jungle_open_hash_comp_flags list -> int -> jungle_hash
val jungle_recno_primary_hash_sec_index_open :
    jungle_recno -> string -> (string -> string) -> jungle_open_flags list * jungle_open_hash_comp_flags list -> int -> jungle_hash
val jungle_hash_primary_hash_sec_index_open :
    jungle_hash-> string -> (string -> string -> string) -> jungle_open_flags list * jungle_open_hash_comp_flags list -> int -> jungle_hash

(* Create Secondary Index of type Recno *)
val jungle_btree_primary_recno_sec_index_open :
    jungle_btree-> string -> (string -> string -> string) -> jungle_open_flags list * jungle_open_recno_comp_flags list -> int -> jungle_recno
val jungle_recno_primary_recno_sec_index_open :
    jungle_recno -> string -> (string-> string) -> jungle_open_flags list * jungle_open_recno_comp_flags list -> int -> jungle_recno
val jungle_hash_primary_recno_sec_index_open :
    jungle_hash-> string -> (string -> string -> string) -> jungle_open_flags list * jungle_open_recno_comp_flags list -> int -> jungle_recno
