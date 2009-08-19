(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: gmisc.mli,v 1.36 2007/02/28 18:48:06 mff Exp $ *)

(* Module: Gmisc
   Description:
     This module implements some additions to the Caml standard
     library that appeared to be useful in the process of developing
     Galax.
*)

(* Note:
     All these functions raise standard Caml exceptions, rather than
     Galax exceptions in the rest of the system.
   - Jerome
 *)


(*********************************)
(* I/O and File System functions *)
(*********************************)

(* Load the content of a file into a string *)

val load_file_in_buffer   : Netbuffer.t -> string -> unit
val load_string_in_buffer : Netbuffer.t -> string -> unit
val load_file             : string -> string
(* Converts Win95 carriage returns and newlines *)
val string_of_file        : string -> string
val get_files_in_directory: string -> string list
val ls                    : string -> string -> string list
(* Convert a shell-style regular expression, using the special
   characters, ?*[], to a Caml-style regular expression. *)
val convert_regexp        : string -> string 

(******************)
(* List functions *)
(******************)

(* an additional function to partition list,
   but only getting the first element that satisfy
   the given predicate *)

val partition_first : ('a -> bool * 'b) -> 'a list -> 'b * 'a list

(* Partitions a list based on a predicate over the index of the
element in the list *)

val partition_index : (int -> bool) -> 'a list -> 'a list * 'a list

(* Partitions a list by pairs of elements, raises Invalid_argument in
   case the list contains an odd number of elements *)

val partition_pairs : 'a list -> ('a * 'a) list

(* Filter non existing elements from a list *)

val filter_non_exists : 'a list -> 'a list -> 'a list

(* Map concat *)

val map_concat : ('a -> 'b list) -> 'a list -> 'b list

(* N first items *)

val list_npeek : int -> 'a list -> 'a list

(* Triple split *)

val triple_split : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list

(* Remove duplicates *)

val remove_duplicates : 'a list -> 'a list

(* Sort and remove duplicates : takes a _reverse_ comparator that
returns -1 if a1 > a2 *)

val sort_and_remove_duplicates_revcompare : ('a -> 'a -> int) -> 'a list -> 'a list

(* Unwrap a list of optional values *)

val unwrap_option_list : 'a option list -> 'a list
val some_list : 'a option list -> 'a list
val is_some : 'a option -> bool

val is_subset       : 'a list -> 'a list -> bool     (* X subset Y *)
val intersect_list  : 'a list -> 'a list -> 'a list  (* X intersect Y *)
val difference_list : 'a list -> 'a list -> 'a list  (* X - Y *)


(*********************)
(* Hashtbl functions *)
(*********************)

val create_hashtable : int -> ('a * 'b) list -> ('a, 'b) Hashtbl.t
    (* Create a hashtable of the given size and fills it with the
       given bindings. *)

val all_of_hashtable : (('a, 'b) Hashtbl.t) -> ('a * 'b) list
    (* Returns all entries in hash table *)

val keys_of_hashtable : (('a, 'b) Hashtbl.t) -> 'a list
    (* Returns all keys in hash table *)

val cond_add : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit

val merge_hashtable : ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t

(********************)
(* String functions *)
(********************)

val split_right_on_char  : string -> char -> string * string
val split_left_on_char   : string -> char -> string * string
val split_on_char        : string -> char -> string list
val remove_leading       : string -> char -> string
val remove_trailing      : string -> char -> string
val quote_quotes         : string -> string

(*********************)
(* Parsing functions *)
(*********************)

val wrap_lexer : (Lexing.lexbuf -> 'a) -> string -> 'a
    (* [wrap_lexer f s] applies the lexing function [f] on the
       string [s].
       Raises [Failure] in case of failure. *)


(**********************)
(* Printing functions *)
(**********************)

(* Print to stdout *)

val printf_stub : string -> (Format.formatter -> 'a -> unit) -> 'a -> unit

(* Print to stderr *)

val eprintf_stub : string -> (Format.formatter -> 'a -> unit) -> 'a -> unit

(* Print to output channel *)

val fprintf_stub : Format.formatter -> string -> (Format.formatter -> 'a -> unit) -> 'a -> unit

(* Print to a string buffer *)

val bprintf_stub : string -> (Format.formatter -> 'a -> unit) -> 'a -> string


(**********************)
(* Filename functions *)
(**********************)

(* Rename a DOS dir to a UNIX dir *)

val rename_dir : string -> string

val string_hash : string -> int


(*********************)
(* Integer functions *)
(*********************)

(* Some missing conversions *)

val big_int_of_int32 : int32 -> Big_int.big_int
val int32_of_big_int : Big_int.big_int -> int32

val big_int_of_int64 : int64 -> Big_int.big_int
val int64_of_big_int : Big_int.big_int -> int64

val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b 

val binary_of_hexString : string -> string
val string_of_hexBinary : string -> string

(*
val comment_blit : string -> unit

*)
