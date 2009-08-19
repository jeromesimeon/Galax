(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: finfo.ml,v 1.11 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Finfo
   Description:
     This module is used to keep track of the original location of
     expressions in files during query processing. This is essentially
     used to provide accurate error messages.
*)

open Format
open Pool

(******************************************************)
(* This module implements location within source file *)
(******************************************************)

(***************************************)
(* Handle on the files being processed *)
(***************************************)

type file_name = string

module FileName =
  struct
    type t = file_name
    let equal = (=)
    let hash = Hashtbl.hash
  end

module FilesPool = MakeNamePool (FileName)

type file_handle = FilesPool.symbol

let input_files = FilesPool.create_pool ()

(* Name of the file currently being processed *)

let bogus_file = ""
let bogus_file_handle =
  FilesPool.add_name input_files bogus_file

let current_file = ref bogus_file
let current_file_id = ref bogus_file_handle

(* file info *)
type start_end_pos = 
    { file_handle : file_handle; (* which file it comes from *)
      start_pos : int;           (* offset for the begining of a parsed token *)
      end_pos : int }            (* offset for the end of a parsed token *)

type line_col_pos = 
    { fh : file_handle; (* which file it comes from *)
      line : int;             (* line for the begining of a parsed token *)
      col  : int }            (* column for the begining of a parsed token *)

type finfo = 
  | StartEndPos of start_end_pos  
  | LineColPos of line_col_pos

(* creates a new location info *)

let make_finfo start_pos end_pos =
  StartEndPos
    ({ file_handle = !current_file_id;
       start_pos = start_pos; 
       end_pos = end_pos })

let make_finfo_line_col file line col =
  LineColPos
    ({ fh = FilesPool.add_name input_files file;
       line = line;
       col = col })

let make_finfo_line_col_id id line col =
  LineColPos
    ({ fh = id;
       line = line; 
       col = col })

let get_file_id file =
  FilesPool.add_name input_files file

let get_current_file () =
  !current_file

let get_current_file_id () =
  !current_file_id

let set_current_file file =
  current_file := file;
  current_file_id := FilesPool.add_name input_files file

(* Bogus location *)

let bogus =
  StartEndPos
    { file_handle = bogus_file_handle;
      start_pos = -1; 
      end_pos = -1 }

(* Prints the location within a file *)

let loc_in_file file_name pos =
  let inch = open_in file_name in
  let lineno = ref 1 in
  let linepos = ref 0 in
  for i=0 to pos do
    try
      let ch =
	input_char inch
      in
      if ch = '\012' or ch = '\n' then begin
	incr lineno;
	linepos := 0
      end
      else incr linepos
    with
    | End_of_file ->
	incr linepos
  done;
  close_in inch;
  !lineno,(!linepos-1)

(* Prints the location within a string *)

let finfo_to_string finfo = 
  match finfo with
  | StartEndPos { file_handle = hand ; start_pos = start_pos ; end_pos = end_pos } ->
    let fname = FilesPool.get_name input_files hand in
    if fname = "" then
      sprintf "characters %d-%d" start_pos end_pos
    else 
      let lineno,linepos = loc_in_file fname start_pos in
      sprintf "File \"%s\", line %d, characters %d-%d" 
	fname lineno linepos (linepos + end_pos - start_pos)
  | LineColPos { fh = hand ; line = line ; col = col } ->
    let fname = FilesPool.get_name input_files hand in
      sprintf "File \"%s\", line %d, col %d" 
	fname line col

(* Create a location from parsing token *)

let parsing_locinfo () =
  try
    make_finfo (Parsing.symbol_start()) (Parsing.symbol_end())
  with
  | _ ->
      bogus

(* Create a location from lexing token *)

let lexing_locinfo lexbuf =
  make_finfo (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf)

(* Extract the part of a string corresponding to a given finfo *)

let extract s fi =
  match fi with
  | StartEndPos fi ->
      let starting = fi.start_pos in
      let len = fi.end_pos - starting in
      String.sub s starting len
  | LineColPos fi -> 
      raise(Invalid_argument("Finfo: Cannot extract substring using line/col position"))


