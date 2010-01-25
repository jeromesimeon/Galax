(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: top_config.mli,v 1.35 2007/08/17 18:28:54 simeon Exp $ *)

(* Module: Top_wsdl2xquery
   Description:
      Command-line for importing/exporting a wsdl description to an
      XQuery stub.
 *)

open Format
open Error
open Xquery_ast

open Top_options

let process_args proc_ctxt gargs =
  let args =
    make_options_argv
      proc_ctxt
      (usage_wsdl ())
      [ WSDL_Options;Misc_Options ]
      gargs
  in
  match args with
  | [] -> failwith "Input file(s) not specified"
  | [fname] -> fname
  | _ ->  failwith ("Too many input files")

(**
  True if the string str ends with subs
*)
let ends_with str subs = 
  let rec compeq s1 s2 start len i = 
    if i >= len then true
    else      
      if s1.[i+start] != s2.[i] then
	false
      else
	compeq s1 s2 start len (i+1) 
  in
  let len2 = String.length subs in
  let len1 = String.length str in
    if len1 >= len2 then
      compeq str subs (len1-len2) len2 0
    else
      false
	

(**
  Sets the output file name and the namespace if the user has not yet done it.
*)  
let set_parameters fname = 
  let base_name fname = 
    try 
      let beg = String.rindex fname '/' + 1 in
      let len = String.length fname - beg in
	if len > 0 then
	  String.sub fname beg len
	else
	  fname
    with Not_found -> fname
  in
  let cut_suffix fname = 
    if ends_with fname ".wsdl" then
      let nlen = String.length fname in
	String.sub fname 0 (nlen-5)
    else
      fname
  in
  let base = cut_suffix (base_name fname) in
    if !Conf.generate_client && !Conf.client_filename = "" then
      Conf.client_filename := base ^ ".xq";
    if !Conf.generate_server && !Conf.server_filename = "" then
      Conf.server_filename := base ^ "server.xq";
    if !Conf.service_namespace = "" then
      Conf.service_namespace := base
	

(**
  The function that does the work by calling the others
*)
let process_wsdl fname = 
  set_parameters fname

let sub_main proc_ctxt wsdl_file =
  process_wsdl wsdl_file;
  let (_, wsdl_stream) = Streaming_parse.open_xml_stream_from_io (Galax_io.File_Input wsdl_file) in      
  let wsdl_ast = Wsdl_load.xml_to_wsdl_ast proc_ctxt wsdl_file wsdl_stream  in
  if !Conf.generate_client then
    Wsdl_import.wsdl_to_xqfile_import
      (!Conf.installdir ^ "/" ^ !Conf.client_filename) (!Conf.service_namespace) 
      wsdl_ast
      !Conf.chosen_service !Conf.chosen_port;
  if !Conf.generate_server then
    Wsdl_export.wsdl_to_xqfile_export 
      !Conf.server_filename !Conf.service_namespace 
      wsdl_ast	  
      !Conf.chosen_service !Conf.chosen_port !Conf.installdir !Conf.impl_filename


let main proc_ctxt gargs =
  Conf.print_prolog := true;
  let wsdl_file = process_args proc_ctxt gargs in
  sub_main proc_ctxt wsdl_file

(*************)
(* Let's go! *)
(*************)

let go gargs =
  (* 1. First get the default processing context for galax-compile *)
  let proc_ctxt = Processing_context.default_processing_context() in

  (* 2. Compile the input queries *)
  Top_util.exec main proc_ctxt gargs

