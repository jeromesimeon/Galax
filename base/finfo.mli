(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: finfo.mli,v 1.9 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Finfo
   Description:
     This module is used to keep track of the original location of
     expressions in files during query processing. This is essentially
     used to provide accurate error messages.
*)

type finfo
    (* A file location *)

val get_file_id : string -> int

val set_current_file : string -> unit
    (* The file currently being parsed *)

val make_finfo : int -> int -> finfo
    (* Creates a file location *)

val make_finfo_line_col : string -> int -> int -> finfo
    (* Creates a file location (line, column) in a particular file *)

val make_finfo_line_col_id : int -> int -> int -> finfo
    (* Creates a file location (line, column) in a particular file *)

val bogus : finfo
    (* An empty file location *)

val finfo_to_string : finfo -> string
    (* Prints a location within a string *)

val parsing_locinfo : unit -> finfo
    (* Generates a file location during parsing *)

val lexing_locinfo : Lexing.lexbuf -> finfo
    (* Generates a file location during lexing *)

val extract : string -> finfo -> string
    (* Extracts the part of a string corresponding to a given file
       location *)

