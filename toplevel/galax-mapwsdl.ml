(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(** 
  @(#)galax-mapwsdl.ml
  

  Top level tool for importing/exporting a wsdl description to an XQuery stub.

  @author: Nicola Onose
  
*)

open Format
open Error
open Xquery_ast

let service_namespace = ref ""
let client_filename = ref ""
let server_filename = ref ""
let impl_filename = ref None

let generate_client = ref false
let generate_server = ref false
let chosen_port = ref None
let chosen_service = ref None
let installdir = ref "."


let usage_msg = 
  sprintf "Usage: %s file [-c client_xqfile] [-s server_xqfile] [-server] [-client] [-namespace service_namespace] [-impl filename]\n" Sys.argv.(0)
    
let process_args () =
  let args = ref [] in
  Arg.parse 
    [ "-c", Arg.String (fun xq -> generate_client := true; client_filename := xq), 
      "sets the name of the output .xq server stub";
      "-client", Arg.Unit (fun () -> generate_client := true), 
      "generate an .xq client file";
      "-s", Arg.String (fun xq -> generate_server := true; server_filename := xq),
      "sets the name of the output .xq server stub";
      "-impl", Arg.String (fun xq -> impl_filename := Some xq),
      "sets the name of the server implementation module";
      "-server", Arg.Unit (fun () -> generate_server := true),
      "generate an .xq server stub";
      "-namespace", Arg.String (fun nms -> service_namespace := nms),
      "sets the namespace prefix in the XQuery source to be generated";
      "-service", Arg.String (fun s -> chosen_service := Some s),
      "chooses a service from those defined in the file; " ^
      "by default the first service is chosen";
      "-port", Arg.String (fun p -> chosen_port := Some p),
      "chooses a port from the list contained in the service element; " ^
      "by default the first port that can be parsed is chosen" ]
    (fun arg -> args := arg :: !args) usage_msg;    
  match !args with
  | [] -> failwith ("No input WSDL file specified\n" ^ usage_msg)
  | [fname] -> fname
  | _ ->  failwith ("Too many input files\n" ^ usage_msg) 	  	  	  

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
    if !generate_client && !client_filename = "" then
      client_filename := base ^ ".xq";
    if !generate_server && !server_filename = "" then
      server_filename := base ^ "server.xq";
    if !service_namespace = "" then
      service_namespace := base
	

(**
  The function that does the work by calling the others
*)
let process_wsdl fname = 
  set_parameters fname
    

let main proc_ctxt () =
  Conf.print_prolog := true;
  let wsdl_file = process_args () in
  process_wsdl wsdl_file;
  let (_, wsdl_stream) = Streaming_parse.open_xml_stream_from_io (Galax_io.File_Input wsdl_file) in      
  let wsdl_ast = Wsdl_load.xml_to_wsdl_ast proc_ctxt wsdl_file wsdl_stream  in
  if !generate_client then
    Wsdl_import.wsdl_to_xqfile_import
      (!installdir ^ "/" ^ !client_filename) (!service_namespace) 
      wsdl_ast
      !chosen_service !chosen_port;
  if !generate_server then
    Wsdl_export.wsdl_to_xqfile_export 
      (!server_filename) (!service_namespace) 
      wsdl_ast	  
      !chosen_service !chosen_port !installdir (!impl_filename)


let _ =
  let proc_ctxt = Processing_context.default_processing_context() in
  Top_util.exec main proc_ctxt ()

