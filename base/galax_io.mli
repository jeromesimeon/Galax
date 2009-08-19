(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_io.mli,v 1.12 2007/08/01 18:06:30 simeon Exp $ *)

(* Module: Galax_io
   Description:
     This module is used to represent input and output methods for the
     Galax engine.
*)


(****************************)
(* Galax I/O specifications *)
(****************************)

(* What kind of input ? *)

type input_spec =
  | File_Input of string
  | String_Input of string
  | Buffer_Input of Netbuffer.t
  | Http_Input of string
  | Channel_Input of in_channel

(* What kind of output ? *)

type output_spec =
  | File_Output of string
  | Buffer_Output of Buffer.t
  | Channel_Output of out_channel
  | Formatter_Output of Format.formatter


(*********************)
(* PXP Parsing hooks *)
(*********************)

type pull_handle

type entity_kind = Document_entity | Document_fragment

type pxp_stream = (unit -> Pxp_types.event option) * pull_handle

val pull_parser_from_input_spec : input_spec -> entity_kind -> pxp_stream

val close_pull_parser : pull_handle -> unit

val dtd_from_input_spec : input_spec -> Pxp_dtd.dtd 

val uri_string_of_gio : input_spec -> string option
val string_of_gio : input_spec -> string 

