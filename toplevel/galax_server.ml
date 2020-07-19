(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_server.ml,v 1.67 2008/01/17 19:18:03 mff Exp $ *)

open Debug
open Error
open Galax_server_util
open Http
open Namespace_builtin
open Streaming_constructors
open Processing_context

(******************* DXQ BUILT-IN FUNCTIONS ********************)

module DXQBuiltins = struct
  (* Utility functions for internal DXQ stuff *)
  let dxq_prefix = Namespace_names.NSPrefix "dxq"
  let dxq_uri = Namespace_names.NSUri (Conf.glxns ^ "/dxq")

  let add_symbol name arity f =
    let symb = (dxq_prefix, dxq_uri, name) in
    Code_fn.add_bltin_fctn ((symb, arity), f)

  let farg0 name f =
    let xqf = fun comp_ctxt alg_ctxt n -> f comp_ctxt alg_ctxt in
    add_symbol name 0 xqf

  let farg1 name f =
    let xqf = fun comp_ctxt alg_ctxt n ->
      f comp_ctxt alg_ctxt (Args.get_array_param1 n) in
    add_symbol name 1 xqf

  let farg2 name f =
    let xqf = fun comp_ctxt alg_ctxt n ->
      f comp_ctxt alg_ctxt (Args.get_array_param2 n) in
    add_symbol name 2 xqf

end

(* For the dns example *)
(*
   Domain name ordering.
   Should be case insensitive

   . <= anything
   anything < anythingelse.anything

*)
module DNS = struct
  open DXQBuiltins
  let dns_lt s t =
    if s = "." then true else
    let slen = String.length s in
    let tlen = String.length t in
    if slen >= tlen then false else
    let tsuffix = String.lowercase_ascii(String.sub t (tlen-slen) slen) in
    if String.compare tsuffix (String.lowercase_ascii s) <> 0 then false else
    String.get t (tlen-slen-1) = '.'

  let dns_le s t =
    if s = "." then true else
    let slen = String.length s in
    let tlen = String.length t in
    if slen > tlen then false else
    if slen = tlen then String.lowercase_ascii s = String.lowercase_ascii t else
    let tsuffix = String.lowercase_ascii(String.sub t (tlen-slen) slen) in
    if String.compare tsuffix (String.lowercase_ascii s) <> 0 then false else
    String.get t (tlen-slen-1) = '.'

  let _ = farg2 "dns-lt"(fun comp_ctxt alg_ctxt (p1,p2) ->
    let s1 = Physical_util.get_string p1 in
    let s2 = Physical_util.get_string p2 in
    let b = dns_lt s1 s2 in
    Cursor.cursor_of_singleton (Physical_item_util._boolean b))

  let _ = farg2 "dns-le"(fun comp_ctxt alg_ctxt (p1,p2) ->
    let s1 = Physical_util.get_string p1 in
    let s2 = Physical_util.get_string p2 in
    let b = dns_le s1 s2 in
    Cursor.cursor_of_singleton (Physical_item_util._boolean b))

end

module BitFunctions = struct
  open DXQBuiltins
  let _ = begin
    farg2 "shift_right_logical" (fun comp_ctxt alg_ctxt (p1, p2) ->
      let x = Int32.of_int(Decimal._int_of_integer (Physical_util.get_integer p1)) in
      let y = Decimal._int_of_integer (Physical_util.get_integer p2) in
      Cursor.cursor_of_singleton (Physical_item_util._integer (Decimal._integer_of_int(Int32.to_int(Int32.shift_right_logical x y))))
	);

    farg2 "shift_left_logical" (fun comp_ctxt alg_ctxt (p1, p2) ->
      let x = Int32.of_int(Decimal._int_of_integer (Physical_util.get_integer p1)) in
      let y = Decimal._int_of_integer (Physical_util.get_integer p2) in
      Cursor.cursor_of_singleton (Physical_item_util._integer (Decimal._integer_of_int(Int32.to_int(Int32.shift_left x y))))
	);

    farg2 "logical_xor" (fun comp_ctxt alg_ctxt (p1, p2) ->
      let x = Int32.of_int(Decimal._int_of_integer (Physical_util.get_integer p1)) in
      let y = Int32.of_int(Decimal._int_of_integer (Physical_util.get_integer p2)) in
      Cursor.cursor_of_singleton (Physical_item_util._integer (Decimal._integer_of_int(Int32.to_int(Int32.logxor x y))))
	);

    farg2 "logical_and" (fun comp_ctxt alg_ctxt (p1, p2) ->
      let x = Int32.of_int(Decimal._int_of_integer (Physical_util.get_integer p1)) in
      let y = Int32.of_int(Decimal._int_of_integer (Physical_util.get_integer p2)) in
      Cursor.cursor_of_singleton (Physical_item_util._integer (Decimal._integer_of_int(Int32.to_int(Int32.logand x y))))
	);

    farg2 "logical_or" (fun comp_ctxt alg_ctxt (p1, p2) ->
      let x = Int32.of_int(Decimal._int_of_integer (Physical_util.get_integer p1)) in
      let y = Int32.of_int(Decimal._int_of_integer (Physical_util.get_integer p2)) in
      Cursor.cursor_of_singleton (Physical_item_util._integer (Decimal._integer_of_int(Int32.to_int(Int32.logor x y))))
	);

    (* Number of leading zeros *)
    farg1 "nlz" (fun comp_ctxt alg_ctxt p1 ->
      let x = ref(Int32.of_int(Decimal._int_of_integer (Physical_util.get_integer p1))) in
      let res = begin
        (* We need unsigned comparison, Int32.compare is signed *)
        let ucompare a b =
          let a_neg = Int32.compare a Int32.zero < 0 in
          let b_neg = Int32.compare b Int32.zero < 0 in
          match a_neg,b_neg with
            true,false -> 1  (* a > b *)
          | false,true -> -1 (* a < b *)
          | true,true -> Int32.compare b a
          | false,false -> Int32.compare a b
        in
        if (Int32.compare !x Int32.zero = 0) then 32 else
        (* x has at least one 1-bit *)
        let n = ref 0 in
        if not(ucompare !x (Int32.of_int 0xffff) > 0) then
          (n := !n + 16; x := Int32.shift_left !x 16);
        if not(ucompare !x (Int32.of_int 0xffffff) > 0) then
          (n := !n + 8; x := Int32.shift_left !x 8);
        if not(ucompare !x (Int32.of_int 0xfffffff) > 0) then
          (n := !n + 4; x := Int32.shift_left !x 4);
        if not(ucompare !x (Int32.of_int 0x3fffffff) > 0) then
          (n := !n + 2; x := Int32.shift_left !x 2);
        (* Can't use Int32.of_int below because the 31-bit integer 0x7fffffff is negative *)
        if not(ucompare !x (Int32.of_string "0x7fffffff") > 0) then
          n := !n + 1;
        !n end
      in
      Cursor.cursor_of_singleton (Physical_item_util._integer (Decimal._integer_of_int res)));
  end
end

(**********************************************************************)
(* TODO                                                               *)
(* Needs to detect and handle recursion.                              *)
(* See other TODOs commented below.                                   *)
(**********************************************************************)

(* This is a hack... We should construct a real element here. *)
let server_error_msg e =
  try
    match e with 
    | Fn_error.Xquery_error e -> Fn_error.downgrade_error (Processing_context.default_processing_context()) e
    | _ -> raise e
  with
  | e -> 
      let errmsg = bprintf_error "" e in
      "<a:Error xmlns:a=\"http://www.galaxquery.org/alg/2007\">"^errmsg^"</a:Error>"


let livemem = ref 0 
let recmem msg = 
  let st = (Gc.full_major(); Gc.stat ()) in
(*  Printf.eprintf "%s %d\n%!" msg st.Gc.live_words *)
  Printf.eprintf "%s\t\t%d\t%d\n%!" msg (st.Gc.live_words) (st.Gc.live_words - !livemem);
  livemem := st.Gc.live_words

(************** SERVERKIND START ************************************)
module type SERVERKIND =
  sig
    val async_eval : async_eval_sig
    val delay : float -> unit 
    type http_request = (in_channel * out_channel * Http.HTTP.http_method * Http.HTTP.header list * string option)
    val http_tcp_server : (* Expected to loop infinitely *)
        bool -> (exn -> unit) -> (http_request -> unit) -> Unix.sockaddr -> unit
    val udp_server :      (* Expected to return immediately *)
        bool -> (exn -> unit) -> (bytes -> unit) -> Unix.sockaddr -> unit
  end

(************** SERVERKIND END ************************************)

(************** Functor GALAXSERVER START ************************************)
module type GALAXSERVER =
  functor (ServerKind : SERVERKIND) ->
  sig
    val full_hostname  : string
    val short_hostname : string

    val log_debug_name : unit -> string
    val portmap : Galax_server_util.Sim.portmap 

    (* Initialize server with optional virtual hostname, optional physical port,
       and inter-server latencies, if known. 
    *)
    val init : bool -> string option -> int option -> Top_util.Graph.graph_edge list -> unit

    val async_eval : async_eval_ext_sig

    (* Map host-port-string to virtual-host, physical-host, physical-port triple *)
    val interpret_hostport_string : interpret_hostport_sig
    val evaluate_closure : evaluate_closure_sig
    val evaluate_remote_query : evaluate_remote_query_sig
    val start_server      : string -> int -> 
      (* Namespace prefix and URI of exported module *)
      (Namespace_names.ncname * string * Galax.prepared_program) -> string -> unit
  end

(************** Functor GALAXSERVER END ************************************)
(************** Module Server START ************************************)
(* Parameterized by SERVER ThreadServer or DummyServer *)
module Server =
  functor (ServerKind : SERVERKIND) ->
  struct
  (* Much of this is taken from QCM *)
    open Galax_server_util
    open Physical_value

    let portmap = Hashtbl.create 11    (* port map for server simulation *)
    let latencymap = Hashtbl.create 11 (* latency map *)

    let interpret_hostport_string hostport = Sim.lookup_host_port_in_portmap portmap hostport 

  (**********************************************************************)
  (* Various names for server                                           *)
  (**********************************************************************)
    let full_hostname  = try Sim.full_hostname() with e -> (eprintf_error "  " e; "")

    let short_hostname = try Sim.short_hostname full_hostname with e -> (eprintf_error "  " e; "")
    (* symbolic name is virtual host name or short-host-name for real server *)
    let sym_name = ref None
    let symbolic_name() = 
      match !sym_name with
      |	None -> raise(Query(DXQ_Error("Galax_server.Server not initialized")))
      |	Some hpn -> hpn
    let log_debug_name() = Printf.sprintf "debug-%s" (symbolic_name())

  (**********************************************************************)
  (* INITIALIZATION                                                     *)
  (**********************************************************************)

    let drop_flag = ref false

    let init drop vhostopt portopt latencies =
      begin
	drop_flag := drop;
	let vhost = 
	  match vhostopt with 
	    None -> short_hostname 
	  | Some vhost -> vhost 
	in 
	sym_name := Some (
 	  match (vhostopt, portopt) with
             (None, None) -> 
               (Printf.sprintf "%s" short_hostname)
           | (None, Some port) ->
               (Printf.sprintf "%s_%d" short_hostname port)
           | (Some vhost, _) ->
               (Printf.sprintf "%s" vhost)); 
	let file = log_debug_name() in
	if Sys.file_exists file then Sys.remove file else ();

	(* Initialize the latency map *)
        List.iter 
           (* We're assuming that latencies are symmetric for now *)
        (fun (n1, n2, _, lats) -> 
	  let lat = match lats with [] -> 0.0 | lat::_ -> lat in
  	   Debug.print_dxq_debug("latency from "^n1^" to "^n2^"="^(string_of_float lat)^"\n");
           if (n1 = vhost) then Hashtbl.add latencymap n2 lat
           else if (n2 = vhost) then Hashtbl.add latencymap n1 lat)
        latencies 
      end

  (**********************************************************)
  (* GALAX DEBUGGING                                        *)
  (**********************************************************)
    let get_log_debug() =
      let file = log_debug_name() in
      if not(Sys.file_exists file)
      then Printf.sprintf "(%s does not exist)" file
      else begin
        try
          let fd = Unix.openfile file [Unix.O_RDONLY] 0o644 in
          Unix.lockf fd Unix.F_RLOCK 0; (* can block *)
          let inchan = Unix.in_channel_of_descr fd in
          let len = in_channel_length inchan in
          let buf = Bytes.create len in
          really_input inchan buf 0 len;
          Unix.lockf fd Unix.F_ULOCK 0;
          Unix.close fd;
          Bytes.to_string buf
        with _ -> Printf.sprintf "(exception reading file %s)" file
      end

    open Http.HTTP

    (******************************************************)
    (* DIAGNOSTIC MESSAGES & ERROR REPORTING              *)
    (******************************************************)
    let message s =
      begin
	(Format.fprintf (!Conf.glx_err_formatter) "[%s] %s\n%!" (symbolic_name()) s);
	(Format.eprintf "[%s] %s\n%!" (symbolic_name()) s);
      end

    let http_protect s =
      let s = Pcre.replace ~pat:"&" ~templ:"&amp;" s in
      let s = Pcre.replace ~pat:"<" ~templ:"&lt;" s in
      let s = Pcre.replace ~pat:">" ~templ:"&gt;" s in
      s

    (* NB: Unix.PF_INET implies IPv4 *)
    let udp_sock = Unix.handle_unix_error(fun proto -> Unix.socket Unix.PF_INET Unix.SOCK_DGRAM proto) 0

    let send_gui_report = Gui.gui_report udp_sock 

    let report_query_error kind qs errmsg = 
      print_dxq_debug(Printf.sprintf "Error @ [%s]: %s\n" (symbolic_name()) errmsg);
      message ("Error in evaluation: "^errmsg);
      message (Printf.sprintf "%s was: '%s'" kind qs);
      send_gui_report (Debug.dxq_debug()) (Gui.GuiNode(symbolic_name(),"x",errmsg))
	
    let async_exn_handler e = 
      (* Redirect debugging output to a buffer *)
      let errmsg = server_error_msg e in
      send_gui_report (Debug.dxq_debug()) (Gui.GuiNode(symbolic_name(),"x",errmsg));
      Debug.print_dxq_debug(Printf.sprintf "Error @ [%s]: %s\n" (symbolic_name()) errmsg)

    (**********************************************************************)
    (* QUERY EVALUATION                                                   *)
    (* Evaluate a query at a remote peer                                  *)
    (**********************************************************************)

    let async_eval = ServerKind.async_eval (Debug.dxq_debug()) async_exn_handler 

    (******* SEND QUERY TO PEER ************)
    let evaluate_remote_query(execute_locally,server_loc,xquery_kind,query,plan_name) =
      let (vhost,phost,port) = server_loc in
      Debug.print_dxq_debug("In evaluate_remote_query vhost "^vhost^" host "^phost^" port "^(string_of_int port)^"\n");
      try
	match xquery_kind with
        (***************************** SEND ASYNC QUERY *************************************)
	| XQueryPlanAsync ->
	    let hdr = (symbolic_name())^"\000" in
	    let query_size = String.length query + (String.length hdr) in 
	    (* Local execution always requires TCP *)
            if execute_locally || query_size > Galax_server_util.query_udp_buffer_length then  
	      begin
		if (Debug.dxq_debug()) then Format.eprintf 
		    "Warning: large async plan (%d), falling back to tcp\n@?" query_size;
		http_query_async (symbolic_name()) phost port "/plan-async" "POST" query;
		Debug.print_dxq_debug("After http_query_async\n");
              end
            else 
	      begin
		Debug.print_dxq_debug ("UDP http_query_async symbolic_name:"^
				       (symbolic_name())^" phost="^phost^" port="^(string_of_int port)^"\n");
		let sockaddr = Galax_server_util.get_inet_socket_address phost port in
		let _ = Unix.sendto udp_sock (Bytes.of_string (hdr^query)) 0 query_size [] sockaddr in
		()
              end;
            send_gui_report (Debug.dxq_debug()) (Gui.GuiMessage(symbolic_name(), vhost, "*", plan_name, query));
            None
        (***************************** SEND SYNC QUERY *************************************)
	| XQueryPlan 
	| XQueryString ->
            send_gui_report (Debug.dxq_debug()) (Gui.GuiMessage(symbolic_name(), vhost, "?", plan_name, query));
            let command = 
	      if xquery_kind = XQueryString then "/query" else "/plan" in
	    Debug.print_dxq_debug("http_query_response  symbolic_name:"^(symbolic_name())^" host="^phost^" port="^(string_of_int port)^"\n");
	    let result = http_query_response (symbolic_name()) phost port command "POST" query in
	    Debug.print_dxq_debug("After http_query_response\n");
	    Some result
      with e -> 
	let errmsg = (server_error_msg e) in
	(report_query_error "Server-side query" query errmsg; Some errmsg)

  (***********************************************************)
  (* We use the Connected-Host header to tell the server our *)
  (* name so that it can report it to the GUI on the reply   *)
  (***********************************************************)

  (**************************** SERVER STATE ********************************)
    let get_connected_host headers =
      let rec loop x =
	match x with
          Header(x,y)::tl -> if x = "Connected-Host" then y else loop tl
	| _::tl -> loop tl
	| [] -> ""
      in
      let ch = loop headers in
      Debug.print_dxq_debug("Connected host is "^ch^"\n"); 
      ch

    let inject_latency(server) = 
      if (Debug.dxq_debug()) then message("inject latency from "^short_hostname^" to "^server^"\n");
      try 
	let lat = Hashtbl.find latencymap server in
	if (Debug.dxq_debug()) then message("="^(string_of_float lat)^"\n"); 
	ServerKind.delay(lat) 
      with
      |	Not_found -> ()

  (******************* ASYNC QUERY VIA UDP ***********************)
  (* Receive and execute an asynchronous plan *)
    let udp_handler eval =
      fun input -> (* TODO: perhaps eval in a new thread? *)
	try
	  let (connected_host, qs) = Gmisc.split_right_on_char (Bytes.to_string input) '\000' in
	  inject_latency(connected_host);
	  (try eval XQueryPlan qs (* NB: not XQueryPlanAsync WHY NOT???? *)
 	  with e -> report_query_error "UDP query" qs (server_error_msg e))
	with e -> report_query_error "UDP message" "Malformed" (server_error_msg e)

  (**********************************************************************)
  (* Browser HTML headers                                               *)
  (**********************************************************************)

  (**********************************************************************)
  (* HTTP request handler                                               *)
  (**********************************************************************)
    let http_server eval2 xq_program (inchan, outchan, meth, headers, payload) =

      (******************  BROWSER UI **************************************)
      (* These should be initialized after portmap is initialized *)
      let hosts =
	if (Hashtbl.length portmap <> 0) then
          "<strong>Hosts</strong>: "^
	  String.concat " " 
	    (Hashtbl.fold (fun host (port,xq_file,xq_program) strs -> 
              (Printf.sprintf "<a href='http://localhost:%d/'>%s</a>" port host)::strs) portmap [])
	else "" 
      in
      let browser_header =
	"<strong>Galaxd "^
	(symbolic_name())^
	"</strong>:\n"^
	"<a href='/'>Home</a> <a href='/program'>Program</a> <a href='/debug'>Debug log</a>"^
	"<br />"^
	hosts^
	"<hr />\n" 
      in
      (******** EVALUATE QUERY FROM PEER **********)
      let evaluate_query_from_peer kind payload =
	let kind_string = 
	  match kind with
	  | XQueryString    -> "Query"
	  | XQueryPlan      -> "Synchronous plan"
	  | XQueryPlanAsync -> "Asynchronous plan"
	in
        begin 
	  match payload with
            None -> message ("Error: received an empty "^kind_string)
	  | Some qs ->
	      if (Debug.dxq_debug()) then message (kind_string^" is:\n"^qs);
	      if (kind = XQueryPlanAsync) then 
              (* Don't close outchan, b/c it will be closed at thread exit. *)
              (* If closed now, some other thread could reuse the file      *)
              (* descriptor and then our thread's exit would close it.  So  *)
              (* just flush it.                                             *)
		(send_html_http_response outchan "" ""; flush outchan;)
	      else ();
	      try 
		let connected_host = get_connected_host headers in
		inject_latency(connected_host); 
		let result = eval2 kind qs in 
		if (kind != XQueryPlanAsync) then 
		begin
		  if (connected_host <> "") then
		    send_gui_report (Debug.dxq_debug()) 
		      (Gui.GuiMessage(symbolic_name(), connected_host,"!","Result",result));
		  send_html_http_response outchan "" result;
		end
		else ()
	    (* Errors always reported to GUI *)
	      with e -> report_query_error kind_string qs (server_error_msg e)
	end
      in
      (******** EVALUATE QUERY FROM GUI/BROWSER **********)
      let evaluate_query_from_gui kind qs = 
	try 
	  if (kind = "job") then (send_html_http_response outchan "" ""; flush outchan)
	  else ();
	  send_gui_report (Debug.dxq_debug()) (Gui.GuiNode(symbolic_name(),"?",qs));
	  let result = eval2 XQueryString qs in
	  let formatted_result = 
	    if (kind = "job") then Galax_server_util.pretty_print_html result
	    else result
	  in
	  send_gui_report (Debug.dxq_debug()) (Gui.GuiNode(symbolic_name(),"!",formatted_result));
	  if (kind = "form") then send_xml_http_response outchan "" result
	  else ()
	with e -> report_query_error kind qs (server_error_msg e)
      in
      
   (***** BEGIN HTTP SERVER HANDLER *****)  
      match meth with
      (************ QUERIES FROM PEERS ************************)
	Post("/query") ->      evaluate_query_from_peer XQueryString payload
      | Post("/plan") ->       evaluate_query_from_peer XQueryPlan payload 
      | Post("/plan-async") -> evaluate_query_from_peer XQueryPlanAsync payload 

      (************ QUERY FROM GUI ************************)
      | Post("/job") -> (* A job submitted by GUI *)
	  begin match payload with
            None -> message ("Error: GUI submitted an empty job request")
          | Some qs -> evaluate_query_from_gui "job" qs
	  end
      (************ QUERY FROM BROWSER ************************)
      | Post("/submit") -> (* A job submitted by browser *)
	  begin match payload with
	    None ->
	      let response =
		"<html>\n<head>\n"^
		"<title>Error: empty query</title></head>\n<body>\n" ^
		"<h1>Error</h1>\n" ^
		"<p>You submitted an empty query</p>\n" ^
		"</body>\n</html>\n" in
	      send_html_http_response outchan "" response
	  | Some formsubmit ->
            (* expect Content-Type: application/x-www-form-urlencoded *)
	      let params = Netencoding.Url.dest_url_encoded_parameters formsubmit in
	      try
		let qs = List.assoc "qs" params in
		evaluate_query_from_gui "form" qs 
	      with _ ->
		message "Error: received a garbled form submission"
	  end
      (************ CONTROL OPTIONS FROM BROWSER ************************)
      | Get("/") -> (* Produce a form for browser submit *)
	  let response =
	    "<html>\n<head><title>Submit Xquery</title></head>\n<body>\n" ^
	    browser_header ^
	    "<form method='POST' action='/submit'>\n" ^
	    "<p>Query:</p>\n" ^
	    "<p>\n"^
	    "<textarea name='qs' rows='5' cols='60'></textarea>\n"^
	    "</p>\n" ^
	    "<p><input type='submit' value='Evaluate'/></p>\n" ^
	    "</form>\n" ^
	    "</body>\n</html>\n" in
	  send_html_http_response outchan "" response;
      | Get("/program-only") -> (* Show the program only  *)
	  send_html_http_response outchan "" xq_program
      | Get("/program") -> (* Show the context *)
	  let xq_program = http_protect xq_program in
	  let response =
	    "<html>\n<head><title>"^
	    (symbolic_name())^
	    ": program</title></head>\n<body>\n" ^
	    browser_header ^
	    "<pre><code>\n"^
	    xq_program^
	    "</code></pre>\n"^
	    "</body>\n</html>\n" in
	  send_html_http_response outchan "" response
      | Get("/debug") -> (* Show the debugging log *)
	  let log = (get_log_debug()) in
	  let log = if String.length log = 0 then "(EMPTY)" else http_protect log in
	  let response =
	    "<html>\n<head><title>"^
	    (symbolic_name())^
	    ": debug log</title></head>\n<body>\n" ^
	    browser_header ^
	    "<pre><code>\n"^
	    log^
	    "</code></pre>\n"^
	    "</body>\n</html>\n" in
	  send_html_http_response outchan "" response
      (************ CONTROL OPTIONS FOR GUI ************************)
      | Post("/guistart") -> (* Start reporting to the gui *)
	  begin match payload with
	    None ->
	      message "Error: empty gui start request"
	  | Some formsubmit ->
	      begin
		try
		  let params =
		    Netencoding.Url.dest_url_encoded_parameters formsubmit in
		  let host = List.assoc "host" params in
		  let port = List.assoc "port" params in
		  Gui.gui_init (!drop_flag) host (int_of_string port);
		  let payload =
		    Gui.string_of_guievent(Gui.GuiNewNode(symbolic_name(),xq_program)) in
		  send_html_http_response outchan "" payload;
		  message (Printf.sprintf "Starting GUI reporting to %s:%s\n" host port)
		with _ ->
		  message "Error starting GUI reporting"
	      end
	  end
      | Get("/favicon.ico") -> () (* common, ignore it *)
      (************ Garbled Requests ************************)
      | Post(s) -> message (Printf.sprintf "Error: received a garbled request (POST %s)" s)
      | Get(s) ->  message (Printf.sprintf "Error: received a garbled request (GET %s)" s)
    (***** END HTTP SERVER HANDLER *****)  

    (*********** GENERIC QUERY EVALUATION START *******)
    let prepared_program_opt = ref None 
    let evaluate_closure module_uri closure_string = 
      let prepared_program = 
	match !prepared_program_opt with None -> raise(Query(DXQ_Error "Server program not initialized"))
	| Some prepared_program -> prepared_program 
      in
      let compiled_program = Galax.compiled_program_of_prepared_program prepared_program  in
      print_dxq_debug("In generic_evaluate_closure\n");
      print_dxq_debug("Before Galax.compile_serialized_closure\n");
      let comp_mod = Galax.module_of_compiled_program compiled_program module_uri in
      let (prepared_program', stmt) =
	Galax.compile_serialized_closure_in_module compiled_program comp_mod (Galax_io.String_Input closure_string) in
      print_dxq_debug("Before Galax.eval_compiled_closure_statement\n");
      Galax.eval_compiled_closure_statement prepared_program' stmt

    let generic_evaluate_query (prefix, uri, prepared_program) library_module xquery_kind query =
      (* Redirect debugging output to a buffer *)
      let debug_buffer = Buffer.create 512 in
      Galax_server_util.set_server_output debug_buffer;
      try
	let proc_ctxt = Galax.procctxt_of_prepared_program prepared_program in
	Monitor.start_monitor_external_call proc_ctxt "Galaxd.main";

      (* Mary: seems like this buffer is only written to AFTER
         evaluation, when we serialize below.  Can we move this code down? *)
	let buff = Buffer.create 512 in
	Conf.xml_formatter := Format.formatter_of_buffer buff;

	let (prepared_program, pv, pt) =
	  match xquery_kind with
	  | XQueryString ->
	      begin
		print_dxq_debug("Before Galax.eval_statement\n");
	     (* eval_statement returns a materialized item list *)
		(prepared_program,
		 Physical_value_util.physical_value_of_item_list(
	         Galax.eval_statement prepared_program (Galax_io.String_Input query)),
		 Xquery_physical_type_ast_util.dom_list_xml_type)
	      end
	  | XQueryPlan
	  | XQueryPlanAsync ->
	      begin
		let compiled_program = Galax.compiled_program_of_prepared_program prepared_program  in
		print_dxq_debug("Before Galax.compile_serialized_closure\n");
		let (prepared_program', stmt) =
		  Galax.compile_serialized_closure compiled_program (Galax_io.String_Input query) in
		let (pt, pv) = Galax.eval_compiled_closure_statement prepared_program' stmt in
		print_dxq_debug("Before Galax.eval_compiled_statement\n");
		(prepared_program', pv, pt)
	      end
	in

      (* xml_header and xml_footer end up in the output, if Conf.verbose is on, so get rid of the defaults *)
	Conf.xml_header := "";
	Conf.xml_footer := "";

      (* NB!! The physical value returned by Galax.eval_compiled_closure_statement is already materialized! *)
      (* Export physical value v as a typed xml stream *)
	let boxed_stream =
	  begin
	    match pv with
	    | PXMLValue xv ->
		let xt = Xquery_physical_type_ast_util.assert_non_discarded_xml_type pt in
		Planio_physical_value.box_tree_result xt xv
	    | PTable tu ->
		let tt = Xquery_physical_type_ast_util.assert_tuple_type pt in
		let compiled_program = Galax.compiled_program_of_prepared_program prepared_program in
		let code_context = Galax.code_selection_context_of_main_module compiled_program in
		let tc = Code_util_materialize.export_input_tuple_as_tuple_cursor code_context tt tu  in
	      (* TODO!  Getting actual tuples from input table! *)
		Planio_physical_value.box_table_result tt tc
	  end
	in
      (* Serialize it to the appropriate output buffer *)
	print_dxq_debug("Before fserialize_typed_xml_stream\n");
	Serialization.fserialize_typed_xml_stream proc_ctxt (Format.formatter_of_buffer buff) boxed_stream;

	Monitor.end_monitor_external_call proc_ctxt;
	Monitor.serialize_monitor (get_external_nsenv proc_ctxt) proc_ctxt;

	Galax_server_util.log_debug (log_debug_name()) (debug_buffer);
	let result = Buffer.contents buff in
	Buffer.reset buff;
	Register_handlers.call_close_handlers ();
	result
      with e ->
	begin
	  log_debug (log_debug_name()) (debug_buffer);   (* Flush buffer right away. *)
	  Register_handlers.call_close_handlers ();
	  raise e  (* Caught and handled in evaluate_query_from_{peer,gui} *)
	end
    (*********** GENERIC QUERY EVALUATION START *******)

    (*****************  SERVER START-UP ****************************)
    (* Never returns *)
    let start_server host portnum (prefix, uri, prepared_program) xq_program = 
      try
	(* This is a gross hack, but it gets prepared_program to evaluate_closure *)
	prepared_program_opt := Some prepared_program;
	let query_eval_fun = generic_evaluate_query (prefix, uri, prepared_program) xq_program in
(*	Sim.check_network_up message;  *)

        (* UDP server creates thread to listen on UDP socket *)
        Printf.eprintf "galaxd: starting UDP server for %s on port %d\n%!" host portnum;
	ServerKind.udp_server (Debug.dxq_debug()) 
	  async_exn_handler
	  (udp_handler (fun kind qs -> ignore(query_eval_fun kind qs)))
	  (Unix.ADDR_INET(Unix.inet_addr_any,portnum));

        (* HTTP/TCP server starts-up and never returns *)
        Printf.eprintf "galaxd: starting TCP server for %s on port %d\n%!" host portnum;
	ServerKind.http_tcp_server (Debug.dxq_debug())
	  async_exn_handler 
	  (http_server query_eval_fun xq_program)
	  (Unix.ADDR_INET(Unix.inet_addr_any,portnum))
      with
      | e ->
	  begin
	    Format.eprintf "ERROR In GalaxdServer.go\n%!";
	    eprintf_error "  " e;
	    Format.fprintf (!Conf.glx_err_formatter) "%!";
	    Format.pp_print_flush !Conf.glx_err_formatter ();
	  end

  (*********** Built-in GUI functions in DXQ namespace *****************)
    let _ = DXQBuiltins.farg1 "gui-report" (fun comp_ctxt alg_ctxt p ->
      let s = Physical_util.get_string p in
      send_gui_report false (Gui.GuiNode(symbolic_name(),"!",s));
      Cursor.cursor_empty())

    let _ = DXQBuiltins.farg2 "gui-status" (fun code_ctxt alg_ctxt (p1, p2) ->
      let comp_ctxt = Code_selection_context.compile_context_from_code_selection_context code_ctxt in
      let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
      let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
      let kind = Physical_util.get_string p1 in
      let s = Serialization.bserialize_datamodel proc_ctxt p2 in
      send_gui_report false (Gui.GuiNode(symbolic_name(),kind,s));
      Cursor.cursor_empty())

end (* module Server *)
(************** Module Server END ************************************)

