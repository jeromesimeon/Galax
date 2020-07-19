(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* Module: Top_xquery2soap
   Description:
   Top level tool for deploying an XQuery module as a SOAP server.
 *)

open Format
open Error
open Xquery_ast
open Top_options

open Unix  (* -- for file copying *)

let usage_msg = 
  sprintf "Usage: %s XQueryModule [-wsdl WSDL] [-port WSDLPort] [-binding WSDLBinding] [-installdir Directory] [-interfacedir Directory] [-address URI] [-nms module-namespace]\n" Sys.argv.(0)
    
let process_args proc_ctxt gargs =
  let args =
    make_options_argv
      proc_ctxt
      (usage_soap ())
      [ SOAP_Options;Misc_Options ]
      gargs
  in
  match args with
  | [] -> failwith ("No input XQuery file specified\n" ^ usage_msg)
  | [fname] -> fname
  | _ ->  failwith ("Too many input files\n" ^ usage_msg) 	  	  	  

(**
  True if the string str ends with subs
*)
let ends_with str subs = 
  let rec compeq s1 s2 start len i = 
    if i >= len then true
    else      
      if s1.[i+start] <> s2.[i] then
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

	
let set_extension xq_module = 
  if (ends_with xq_module ".xq") then xq_module
  else xq_module ^ ".xq"
    
let strip_extension xq_module =
  if (ends_with xq_module ".xq") then 
    String.sub xq_module 0 (String.length xq_module - 3)
  else xq_module
    

let process_xq () = 
  if !Conf.installdir = "" then
    Conf.installdir := ".";
  if !Conf.wsdl_url = "" then
    raise (Query (Toplevel_Error "No WSDL specified: module export with no WSDL interface is not currently implemented"))
  else if !Conf.nms_uri = "" then
    raise (Query (Toplevel_Error "Error in service export: no namespace specified"))

let get_local_name path = 
  try 
    let pos_slash = String.rindex path '/' in
      String.sub path (pos_slash + 1) (String.length path - pos_slash - 1)
  with Not_found -> path

(* determine the absolute path + filename of the .xqs file *)
let xqs_file_name xqfile soap_uri = 
  let localfilename = 
    match soap_uri with
	Some adr -> 
	  begin
	    match Galax_url.glx_decode_url adr with
		Galax_url.File s -> s
	      | Galax_url.Http (_,_,local) -> local
	      | _ -> raise (Query (Toplevel_Error ("Bad address " ^ adr ^
						   "; only http and file methods are supported")))
	  end
      | None -> (get_local_name xqfile) ^ "s"
  in
    if !Conf.installdir <> "." then
      !Conf.installdir ^ "/" ^ localfilename
    else
      localfilename


(* file_copy() -- from Didier Remy's OS course *)
let file_copy in_name out_name =
  let buffer_size = 8192 in
  let buffer = Bytes.create buffer_size in
  let fin = openfile in_name [O_RDONLY] 0 in
  let fout = openfile out_name [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
  let rec copy_loop () = 
    match read fin buffer 0 buffer_size with
	0 -> ()
      | r -> ignore (write fout buffer 0 r); copy_loop() in
    try 
      copy_loop();
      close fin; close fout
    with Unix_error (e,f,s) -> raise (Query (Error (error_message e)))


let are_the_same_file f1 f2 = 
  try 
    let stats1 = lstat f1 
    and stats2 = lstat f2 in
      stats1.st_dev = stats2.st_dev && stats1.st_ino = stats2.st_ino
  with Unix_error _ -> false
		 

let main proc_ctxt gargs =
  let xq_module = process_args proc_ctxt gargs in
  let xq_file = set_extension xq_module in
    process_xq ();
    let (_, wsdl_stream) = 
      Streaming_parse.open_xml_stream_from_io (Galax_io.Http_Input !Conf.wsdl_url) in
    let server_filename = xqs_file_name xq_file !Conf.address_uri in
    let new_xq_filename = get_local_name xq_file in
    let new_path = !Conf.installdir ^ "/" ^ new_xq_filename in
      if not(are_the_same_file new_path xq_file) then	
	begin
	  print_endline ("copying " ^ xq_file ^ "->" ^ new_path);
	  file_copy xq_file new_path;      
	end;
      let wsdl_ast = Wsdl_load.xml_to_wsdl_ast proc_ctxt !Conf.wsdl_url wsdl_stream  in
	Wsdl_apache.wsdl2xq_server_source
	  (Some new_xq_filename) server_filename !Conf.nms_uri
	  wsdl_ast
	  None !Conf.chosen_port  
	  

let go gargs =
  let proc_ctxt = Processing_context.default_processing_context() in
  Top_util.exec main proc_ctxt gargs

