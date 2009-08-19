(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parse_io.mli,v 1.7 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Io
   Description:
     This module contains some basic operations to manipulate I/O's
     from Galax.
*)

(**************************)
(* Galax Input Operations *)
(**************************)

(* Note:
     galax_input's include a ready-to use lexing buffer, and must be
     closed after use.  
*)

type galax_input

(* Creates an internal I/O from an external, Galax I/O *)

val galax_input_from_input_spec : Processing_context.processing_context -> Galax_io.input_spec -> galax_input

val galax_utf8_input_from_input_spec : Processing_context.processing_context -> Galax_io.input_spec -> galax_input

(* Closes the I/O *)

val close_galax_input : galax_input -> unit

(* Return the lexbuf component from that I/O *)

val lexbuf_from_galax_input : galax_input -> Lexing.lexbuf

(* Parsing error on input *)

val parse_error_msg : Galax_io.input_spec -> string

val name_of_input_spec : Galax_io.input_spec -> string


(***************************)
(* Galax Output Operations *)
(***************************)

(* Note:
     galax_output's include a ready-to use lexing buffer, and must be
     closed after use.
*)

type galax_output

(* Creates an internal I/O from an external, Galax I/O *)

val galax_output_from_output_spec : Galax_io.output_spec -> galax_output

(* Creates an output formatter out of a galax output *)

val formatter_of_galax_output : galax_output -> Format.formatter

(* Closes a Galax output *)

val close_galax_output : galax_output -> unit


