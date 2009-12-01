(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

open Debug
open Physical_value

open Error

open Format
open Print_top

open Monitoring_context
open Processing_context

open Top_util
open InternalQuery
open Top_config
open Top_options

open Xquery_algebra_ast

open Galax_server_util
open Galax_server

open Http.HTTP

open Planio_physical_value
  
type enctype = XML_Encoding | URL_Encoding | Html_Encoding
    
let galaxd_host = ref "localhost"
let galaxd_port = ref (!dxq_default_port)

let pid_counter = ref 0

let m_pid = Mutex.create()
let m_enfuns = Mutex.create()

let zero_ns_uri = "http://www.galaxquery.org/zero/2007"

let get_new_pid () = 
  Mutex.lock m_pid;
  let p = !pid_counter in
    pid_counter := p + 1;
    Mutex.unlock m_pid;
    p
      

let zerohost = ref "localhost"
let zeroport = ref (string_of_int (!Galax_server_util.zerod_default_port))

(* let string_of_http_method = function *)
(*     Get s -> "GET "^s *)
(*   | Post s -> "POST "^s  *)

let strip_backslash s = 
  let slen = String.length s in
    if slen > 1 && String.get s 0 = '/' then
      String.sub s 1 (slen-1)
    else s

let resource_of_http_method =  function
    Get s -> strip_backslash s
  | Post s -> strip_backslash s 

let method_type  = function 
    Get _ -> "GET"
  | Post _ -> "POST"

let replace_resource m r = match m with
    Get s -> Get r
  | Post s -> Post r


(* copied from galaxd.ml *)

let tcp_server f port =
  let rec accept_non_intr s =
    try Unix.accept s
    with Unix.Unix_error (Unix.EINTR, _, _) -> accept_non_intr s in
  let addr = Unix.ADDR_INET(Unix.inet_addr_any,port) in
  let sock =
    Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.bind sock addr;
    Unix.listen sock 5;
    
    while true do
      let (s, _) = accept_non_intr sock in
	ignore (
	Thread.create
	  (fun () ->
         let inchan = Unix.in_channel_of_descr s in
         let outchan = Unix.out_channel_of_descr s in
           (try f inchan outchan with e ->
		      Format.eprintf "Error: %s\n%!"
                (Printexc.to_string e);
		      ());
           (try close_out outchan with _ -> ());
	       Thread.exit() )
	  () )
    done


(* In case we are running a simulation, we will be creating a bunch of
   processes.  Set things up so we can kill them all if there is any
   problem. *)
let mainpid = Unix.getpid() (* executed once in main process *) 
  
let _ = 
  Sys.set_signal Sys.sigterm
    (Sys.Signal_handle 
       (fun _ ->
	  Printf.eprintf "Received SIGTERM signal\n%!";	   
	  if Unix.getpid() = mainpid then
	    Unix.kill 0 Sys.sigterm;
	  exit 1))
    
let terminate_mainpid() =
  begin
    Printf.eprintf "In terminate_mainpid\n%!";	   
    Unix.kill mainpid Sys.sigterm; 
    (* Never reached *)
    Unix.sleep 1000
  end

let symbolicName = ref ""

(* Returns the process id of the business process to be called or
   created and the URL of the resource. *)
let compute_processid_and_resurl meth = 
  let s = resource_of_http_method meth in
  let slen = String.length s in
    if slen < 11 || not(String.sub s 0 9 = "processid") then
      (string_of_int (get_new_pid()), s)
    else
      try 
	    let p2 = String.index_from s 9 '/' in
	      (String.sub s 9 (p2-9), String.sub s (p2+1) (slen-p2-1))
      with
	      Not_found -> failwith "Invalid zerod URL"

let make_location_header s_bpid = 
  "Location: http://" ^ (!zerohost) ^ ":" ^ (!zeroport) ^ "/processid" ^ s_bpid ^ "\r\n"

let parse_resolved_xml_stream_from_io gio =
  (* 1. Open a SAX cursor on the input document *)
  let (dtd_opt, xml_stream) = Streaming_parse.open_xml_stream_from_io gio  in
	  (* 2. Resolve namespaces *)
  let resolved_xml_stream = Streaming_ops.resolve_xml_stream xml_stream in
  resolved_xml_stream

let get_items_from_ans pc ans_string = 
  let xstream = parse_resolved_xml_stream_from_io (Galax_io.String_Input ans_string) in
  let c = Physical_xml_value.item_cursor_of_xml_value (unbox_tree_result pc xstream) in
    Serialization.bserialize_datamodel pc c

let is_zero_uri = function
    Namespace_names.NSWildcardUri -> false
  | Namespace_names.NSUri s -> s = zero_ns_uri


let content_type_of_headers h = 
  try 
    match 
      List.find (fun line -> match line with
                     ContentLength _ | Other _ -> false
                   | Header (n,v) -> String.lowercase n = "content-type") h
    with
        Header(_,"application/xml+xhtml") -> XML_Encoding
      | Header(_,"application/x-www-form-urlencoded") -> URL_Encoding
      | Header(ct,_) -> failwith ("Unsupported encoding type "^ct)
      | Other _ -> assert false
      | ContentLength _ -> assert false
  with Not_found -> URL_Encoding

let xml_params_of_html_params buf =
  let params = 
    Netencoding.Url.dest_url_encoded_parameters buf
  in
  let b2 = Buffer.create (String.length buf) in
    Buffer.add_string b2 "<parameters>\n";    
    List.iter (fun (n,v) -> Buffer.add_string b2 ("<"^n^">"); 
                 Buffer.add_string b2 (Netencoding.Html.decode 
                                         ~in_enc:`Enc_usascii ~out_enc:`Enc_usascii 
                                         ~entity_base:`Html 
                                         ()  v); 
                 Buffer.add_string b2 ("</"^n^">\n")) 
      params;
    Buffer.add_string b2 "</parameters>";
    Buffer.contents b2
  
let treat_request pc dispatch_prefix i o =
  let response_enctype = ref XML_Encoding in
  let (meth,headers,payload) = get_http_request i in
  let p,r = compute_processid_and_resurl meth in
    try 
      let content = match payload with
          None -> 
            begin
              match content_type_of_headers headers with
                  XML_Encoding -> "()"
                | URL_Encoding -> (response_enctype := Html_Encoding; "()")
                | Html_Encoding -> failwith "Unsupported encoding: html"
            end
        | Some buf -> 
            match content_type_of_headers headers with
                XML_Encoding -> buf
              | URL_Encoding -> (response_enctype := Html_Encoding; xml_params_of_html_params buf)
              | Html_Encoding -> failwith "Unsupported encoding: html"
      in
      let meth_type = method_type meth in
	  let q = dispatch_prefix^":dispatch("^p^",\""^meth_type^"\", \""^r^"\", "^content^")" in
	  let response = http_query_response (!symbolicName) (!galaxd_host) (!galaxd_port) "/query" "POST" q in
        if !Conf.verbose then
	      Printf.eprintf "Received response from galaxd:%s%!\n" response;
        let unboxed = get_items_from_ans pc response in
          match (!response_enctype) with
              XML_Encoding ->
	            send_xml_http_response o (make_location_header p) unboxed 
            | Html_Encoding -> 
                send_html_http_response o (make_location_header p) unboxed 
            | URL_Encoding -> assert false
    with (Failure m) -> send_html_http_response o "" ("Error: "^m^"\n")  
	  | e ->  send_html_http_response o "" ("Error: "^ (Printexc.to_string e)^"\n")      
	      
	  
let server_body hostopt portopt termination proc_ctxt dispatch_prefix =
  let port = match portopt with None -> !Galax_server_util.zerod_default_port | Some port -> port in    
    begin
      match hostopt with
	    | None -> 
	        Printf.eprintf "\nzerod: starting server on port %d%!\n" port
	    | Some host -> 
	        Printf.eprintf "\nzerod: starting server for %s on port %d%!\n" host port; 
	        zerohost := host
    end;
    zeroport := string_of_int port;
    symbolicName := (!zerohost) ^ ":" ^ (string_of_int port); 
    tcp_server (treat_request proc_ctxt dispatch_prefix) port
      

let start_server proc_ctxt dispatch_prefix =
  (* Note! Server must fork once! *)
  let pid = Unix.fork() in
    if pid <> 0 then 
	  server_body (Some "localhost") (proc_ctxt.zerod_port) terminate_mainpid proc_ctxt dispatch_prefix

(* TO BE CHANGED: The URL should end with the name of the current Zero app. *)

let process_args proc_ctxt =
  let args =
    make_options
      proc_ctxt
      (usage_zerod ())
      [Zerod_Options]
  in 
    match args with
      | [] -> failwith "Prefix of dispatch function not specified"
      | [prefix] -> prefix
      | _ -> failwith "Too many arguments"


      
let _ = 
  try
    let proc_ctxt = galax_run_proc_ctxt () in
      begin
	match proc_ctxt.dxq_host with
	    Some h -> galaxd_host := h
	  | None -> ()
      end;
      begin
	match proc_ctxt.dxq_port with
	    Some p -> galaxd_port := p
	  | None -> ()
      end;      	    
      let dispatch_prefix = process_args proc_ctxt in
	start_server proc_ctxt dispatch_prefix
  with
    | e -> 
	begin
	  (* If we end up here, something has gone very, very wrong *)
	  Printf.eprintf "Fatal Error: %s\n%!" (Printexc.to_string e);
	  eprintf_error "  " e;
	  Format.fprintf (!Conf.glx_err_formatter) "@.";
	  Format.pp_print_flush !Conf.glx_err_formatter ();
	end
	  
