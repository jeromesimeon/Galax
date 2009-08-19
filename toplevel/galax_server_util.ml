(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

open Error
open Namespace_builtin
open Streaming_constructors
open Top_util
open InternalQuery

let dxq_default_port = ref 3000
let gui_udp_buffer_length = 4096
let query_udp_buffer_length = 16384
let zerod_default_port = ref 2020

let get_inet_socket_address host port = 
  try
    Unix.ADDR_INET((Unix.gethostbyname host).Unix.h_addr_list.(0), port)
  with Not_found ->
    raise (Query(DXQ_Error
             (Printf.sprintf
                "Can't find the IP address of the server (%s)" host)))

(* Run HTML tidy on input string *)
let pretty_print_html s =
  let fn = Filename.temp_file "galax-pp" ".xml" in
  let f = open_out fn in
  output_string f s;
  close_out f;
  ignore(Sys.command (Printf.sprintf "tidy -q -i -xml -m %s" fn));
  let result = Gmisc.string_of_file fn in
  Sys.remove fn;
  result

let parse_host_port_string hostport = 
  try
    begin
      let i = String.index hostport ':' in
      let host = String.sub hostport 0 i in 
      let port_string = (String.sub hostport (i+1) (String.length hostport - (i+1))) in 
      let port = 
	try int_of_string port_string 
	with | Failure _ -> raise(Query(DXQ_Error(Printf.sprintf "invalid port '%s'\n" port_string)))
      in
      (host, Some port, port_string)
    end
  with
  | Not_found -> (hostport, None, "")

(************ GALAXD INIT ************)
let set_server_output buff =
  begin
    let fmt = Format.formatter_of_buffer buff in
    Conf.expr_formatter                         := fmt;
    Conf.type_formatter                         := fmt;
    Conf.core_expr_formatter                    := fmt;
    Conf.optimized_expr_formatter               := fmt;
    Conf.factorized_expr_formatter              := fmt;
    Conf.projection_formatter                   := fmt;
    Conf.projected_file_formatter               := fmt;
    Conf.algebra_optimization_rewrite_formatter := fmt;
    Conf.logical_algebra_formatter              := fmt;
    Conf.optimized_algebra_formatter            := fmt;
    Conf.physical_algebra_formatter             := fmt;
    Conf.dfgraph_formatter                      := fmt;
    Conf.glx_err_formatter                      := fmt;
  end
(************ GALAXD INIT ************)
(************ GALAXD DEBUGGING ************)
let log_debug file buff =
  let s = Buffer.contents buff in
  Buffer.reset buff;
  if String.length s = 0 then ()
  else
    let fd = Unix.openfile file [Unix.O_RDWR; Unix.O_CREAT] 0o644 in
    Unix.lockf fd Unix.F_TLOCK 0;
    let outchan = Unix.out_channel_of_descr fd in
    seek_out outchan (out_channel_length outchan);
    Printf.fprintf outchan "**********************************\n%s\n%!" s;
    flush outchan;
    Unix.lockf fd Unix.F_ULOCK 0;
    Unix.close fd

(************ GALAXD DEBUGGING ************)

module Gui = struct
  type nodeName = string

  type guiEvent =
      GuiMessage of nodeName * nodeName * string * string * string
    | GuiNode of nodeName * string * string 
    | GuiNewNode of nodeName * string 
    | GuiNone

  let gui_host_port = ref None
  let drop_flag = ref false

  let gui_init drop host port = 
    gui_host_port := Some(host, port);
    drop_flag := drop

  let guievent_of_string s =
    let rec parts s i =
      try
        let j = String.index_from s i '\000' in
        let part = String.sub s i (j-i) in
        part::(parts s (j+1))
      with Not_found -> [] in
    let p = parts s 0 in
    match p with
    | ["NewNode";node;s] -> GuiNewNode(node,s)
    | ["Node";node;s1;s2] -> GuiNode(node,s1,s2)
    | ["Message";src;dest;s1;s2;msg] -> GuiMessage(src,dest,s1,s2,msg)
    | _ -> GuiNone

  let string_of_guievent g =
    match g with
    | GuiNewNode(node,s) ->
        Printf.sprintf "NewNode\000%s\000%s\000" node s
    | GuiNode(node,s1,s2) ->
        Printf.sprintf "Node\000%s\000%s\000%s\000" node s1 s2
    | GuiMessage(src,dest,s1,s2,msg) ->
        Printf.sprintf "Message\000%s\000%s\000%s\000%s\000%s\000" src dest s1 s2 msg
    | GuiNone -> Printf.sprintf "None\000"

  (**********************************************************************)
  (* Sending reports to the gui                                         *)
  (**********************************************************************)
  let gui_report_tcp debug s =
    let slen = String.length s in
    if (debug) then 
      Printf.eprintf "Warning: large gui report (%d), falling back to tcp\n%!" slen; 
    match (!gui_host_port) with 
    | Some (host, port) ->
	begin
	  try ignore (Http.HTTP.gen_request host port "/report" "POST" (Some("",s)))
	  with _ -> ()
	end
    | None -> ()

  let gui_sockaddr_ref = ref None
  let gui_sockaddr () =
    match !gui_sockaddr_ref with
    | None -> 
	begin
	  match (!gui_host_port) with
	  | Some (host, port) ->
	      begin
		gui_sockaddr_ref := Some(Http.get_inet_socket_address host port);
		!gui_sockaddr_ref
	      end
	  | None -> None
	end
    | _ -> !gui_sockaddr_ref

  let gui_report udp_sock debug ge =
(*    if (debug) then 
    match ge with
    | GuiMessage _ -> ()
    | _ ->  
*)
(*    Printf.eprintf "Gui report: %s\n%!" (string_of_guievent ge);   *)
    match gui_sockaddr() with
      None -> Printf.eprintf "UDP socket addr None"
    | Some sa -> 
	begin
	  let send_message = 
	    (match ge with
	    | GuiNewNode(name,info) -> true
	    | GuiNode (_, kind, _) ->
		(kind = "l" || kind = "g" || kind = "!" || kind = "x" || not(!drop_flag))
	    | _ -> (not(!drop_flag)))
	  in
	  if (send_message) then 
            let s = string_of_guievent ge in
            let slen = String.length s in
            if slen > gui_udp_buffer_length then  
	      ((* Printf.eprintf "TCP!\n%!";   *)
	      gui_report_tcp debug s)
            else
              ((* Printf.eprintf "UDP!\n%!";   *)
	      let rlen = Unix.sendto udp_sock s 0 slen [] sa in 
	      if (rlen = -1) then Printf.eprintf "sendto returned -1\n%!"
	      else if (rlen != slen) then Printf.eprintf "sendto sent %d expected %d\n%!" (rlen) (slen)
	      else ())
	  else ()
	end
	  
end (* module Gui *)

                       (* Virtual host name, physical host name, physical port *)
type server_location = (Gui.nodeName * string * int)

type xquery_kind =
    XQueryString
  | XQueryPlan
  | XQueryPlanAsync

type evaluate_remote_query_sig = 
    (bool * server_location * xquery_kind * string * string -> string option )

type evaluate_closure_sig = 
    string -> string -> (Xquery_physical_type_ast.physical_type * Physical_value.physical_value)

(* Executed once, in the main process *)
type async_eval_sig = bool -> (exn -> unit) -> (unit -> unit) -> unit
type async_eval_ext_sig = (unit -> unit) -> unit

type interpret_hostport_sig = string -> server_location

module Sim = struct
  type portmap = (string, (int * string * string)) Hashtbl.t

  let full_hostname() = 
    let h = Unix.gethostname() in
    if String.contains h '.' then h else "localhost"

  (* e.g., saul.cis.upenn.edu *)
  let short_hostname full_hostname =       (* e.g., saul *)
    try
      let i = String.index full_hostname '.' in
      String.sub full_hostname 0 i
    with Not_found -> full_hostname

  (* For simulations.  Look in a directory with .xq files and
     deterministically assign each one a port.

     For example, if the starting port is 3000 and there are files

     a.xq b.xq bar.xq foo.xq

     in the directory, then we would have the port assignments

     a->3000
     b->3001
     bar->3002
     foo->3003

     and a, b, bar, and foo would be considered virtual hosts operating
     at those ports.
  *)

  let populate_portmap portmap dir starting_port_opt =
    let starting_port = match starting_port_opt with None -> !dxq_default_port | Some port -> port in 
    let xqfiles = Gmisc.ls dir "*.xq" in
    let iface_file =
      let r = Str.regexp(Gmisc.convert_regexp "*-int.xq") in
      fun s -> Str.string_match r s 0 in
    let xqfiles = List.filter
        (fun s -> not (iface_file s)) xqfiles in
    let curr_port = ref starting_port in
    List.iter
      (fun xqf ->
        let port = !curr_port in
        curr_port := !curr_port + 1;
        let vhost = Filename.chop_extension xqf in
        let xq_program = Gmisc.string_of_file (dir ^ "/" ^ xqf) in
	Hashtbl.add portmap vhost (port, xqf, xq_program))
      xqfiles

  (* Portmap should be a hash table *)
  let lookup_host_port_in_portmap portmap hostport =
    let (host, portopt, portstr) = parse_host_port_string hostport in
    match (portopt) with 
      (* We can only simulate on a virtual host.  If a port is
         specified in hostport string, it denotes a real host. *)
    | None -> 
	begin
	  try 
	    let (port, filename, program) = Hashtbl.find portmap hostport 
	    in (hostport, "localhost", port)
	  with Not_found ->
	    begin
	      match (portopt) with
	      | (None) -> (host, host, !dxq_default_port)
	      | (Some port) -> (hostport, host, port)
	    end
	end
    | Some port -> (hostport, host, port)

    (* A quick check to see if the network is up, useful in Windows 95 *)
  let check_network_up message = 
    try 
      let name = Unix.gethostname() in
      if (name = "localhost.localdomain") then ()
      else ignore (Unix.gethostbyname(name))
    with Not_found ->
      message "Error: the network is not up!\n";
      flush stderr;
      Pervasives.exit(33);

end (* module Sim *)

