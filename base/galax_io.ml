(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_io.ml,v 1.12 2007/09/07 19:03:40 mff Exp $ *)

(* Module: Galax_io
   Description:
     This module contains some basic I/O structures and operations.
*)

open Error

open Pxp_ev_parser
open Pxp_types


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

let source_from_input_spec gio =
  match gio with
  | File_Input s ->
      from_file s
  | String_Input s ->
      from_string s
  | Buffer_Input nb ->
      let (in_channel,close) = Netchannels.create_input_netbuffer nb in
      close ();
      from_obj_channel in_channel
  | Http_Input uri ->
      begin
	match Galax_url.glx_decode_url uri with
	| Galax_url.Http (host,port,local) ->
	    from_string (Http.HTTP.get uri) 
	| Galax_url.File f ->
	    from_file f
	| Galax_url.ExternalSource _ ->
	    raise (Query (Prototype "Cannot parse external source"))
      end
  | Channel_Input ic ->
      from_channel ic

let close_source s =
  let resolver =
    match s with
    | Entity (_,resolver)
    | ExtID (_,resolver)
    | XExtID (_,_,resolver) -> resolver
  in
  resolver#close_in

type pull_handle = source
type pxp_stream = (unit -> Pxp_types.event option) * pull_handle

type entity_kind = Document_entity | Document_fragment

(* Mary : The default entity manager requires that the entity read be
   a complete XML document.  See Pxp_ev_parser for how to create an
   entity manager for document fragments. 
*)
let pull_parser_from_input_spec gio entity_kind =
  let config = Galax_pxp.glx_default_config in
  let entry = Galax_pxp.glx_default_entry in
  let source = source_from_input_spec gio in
  let entity_manager = 
    match entity_kind with
    | Document_entity -> create_entity_manager config source 
    (* Mary: The following line needs to be changed, somehow, to create an entity
       manager that can parse individual XML entities instead of complete documents. *)
    | Document_fragment -> create_entity_manager ~is_document:false config source 
  in
  (create_pull_parser config entry entity_manager,source)

let close_pull_parser source =
  close_source source

let dtd_from_input_spec gio =
  let config = Galax_pxp.glx_default_config in
  let source = source_from_input_spec gio in
  Pxp_dtd_parser.parse_dtd_entity config source

let ff = "file://"

let uri_string_of_gio gio =
  match gio with
  | File_Input s ->
      if Filename.is_relative s
      then
	Some
	  (Gmisc.rename_dir (Filename.concat ff (Filename.concat (Sys.getcwd ()) s)))
      else
	Some (ff ^ s)
  | String_Input s -> None
  | Buffer_Input _ -> None
  | Http_Input uri -> Some uri
  | Channel_Input _ -> None

let string_of_gio gio =
  match gio with
  | File_Input s ->
      if Filename.is_relative s
      then
	(Gmisc.rename_dir (Filename.concat ff (Filename.concat (Sys.getcwd ()) s)))
      else
	(ff ^ s)
  | String_Input s -> "[StringInput]"
  | Buffer_Input _ -> "[BufferInput]"
  | Http_Input uri -> uri
  | Channel_Input _ -> "[ChannelInput]"

