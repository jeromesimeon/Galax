(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cursor.mli,v 1.3 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Cursor
   Description:
     This module implements XQuery 1.0 and XPath 2.0 data model
     sequences as lazy, but destructive cursors.
*)

(***********)
(* Cursors *)
(***********)

type 'a cursor


(***********************)
(* Cursor Constructors *)
(***********************)

(* GENERAL *)

val cursor_of_list     : 'a list -> 'a cursor
val cursor_of_function : (unit -> 'a option) -> 'a cursor
val cursor_of_stream   : 'a Stream.t -> 'a cursor

(* BASIC *)

val cursor_empty         : unit -> 'a cursor
val cursor_of_singleton  : 'a -> 'a cursor
val cursor_of_option     : 'a option -> 'a cursor


(********************)
(* Cursor Accessors *)
(********************)

(* GENERAL *)

val cursor_peek          : 'a cursor -> 'a option
val cursor_npeek         : int -> 'a cursor -> 'a list
    (* Preserve the cursor *)

val cursor_next          : 'a cursor -> 'a
    (* Consume the stream, and raise Stream.Failure at the end *)

val cursor_junk          : 'a cursor -> unit

(* BASIC / NON DESTRUCTIVE *)

val cursor_is_empty      : 'a cursor -> bool

val cursor_get_singleton : 'a cursor -> 'a
val cursor_is_singleton  : 'a cursor -> bool

val cursor_get_optional  : 'a cursor -> 'a option
val cursor_is_optional   : 'a cursor -> bool


(* ITERATORS / DESTRUCTIVE *)

val cursor_cons            : 'a -> 'a cursor -> 'a cursor
val cursor_append          : 'a cursor -> 'a cursor -> 'a cursor
val cursor_map       	   : ('a -> 'b) -> 'a cursor -> 'b cursor
val cursor_iter      	   : ('a -> unit) -> 'a cursor -> unit
val cursor_map_concat      : ('a -> 'b cursor) -> 'a cursor -> 'b cursor
val cursor_filter    	   : ('a -> bool) -> 'a cursor -> 'a cursor
val cursor_fold_left       : ('a -> 'b -> 'a) -> 'a -> 'b cursor -> 'a
val cursor_exists          : ('a -> bool) -> 'a cursor -> bool
val cursor_find            : ('a -> bool) -> 'a cursor -> 'a

val cursor_list_fold       : 'a cursor list -> 'a cursor
val cursor_array_fold      : 'a cursor array -> 'a cursor

val cursor_for_all         : ('a -> bool) -> 'a cursor -> bool
    (* Used only in has_element_content(), and in of_type *)

val cursor_for_all2        : ('a -> 'b -> bool) -> 'a cursor -> 'b cursor -> bool
    (* Used only in fn:deep-equal in ./algebra/algebra_fn *)

val cursor_length          : 'a cursor -> int
    (* Used only in fn:count in ./algebra/algebra_fn *)

val cursor_first           : 'a cursor -> 'a cursor
val cursor_last            : 'a cursor -> 'a cursor

val cursor_subsequence2    : 'a cursor -> int -> 'a cursor
val cursor_subsequence3    : 'a cursor -> int -> int -> 'a cursor

(* EXPENSIVE ! Materializes
 the cursor into a list *)

val list_of_cursor         : string -> 'a cursor -> 'a list
val rev_list_of_cursor     : 'a cursor -> 'a list

(* Caml stream of a cursor *)

val stream_of_cursor       : 'a cursor -> 'a Stream.t

