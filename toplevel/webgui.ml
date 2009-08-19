(****************************************************************************)
(*                                                                          *)
(* Web interface for controlling galaxd and visualizing distributed         *)
(* queries.                                                                 *)
(*                                                                          *)
(* Trevor Jim.                                                              *)
(*                                                                          *)
(* Requires HTML tidy in path                                               *)
(* Requires inkscape in path                                                *)
(* Requires gui.swf, gui2.swf in current directory         *)
(*                                                                          *)
(* Compile like this:                                                       *)
(*                                                                          *)
(* ocamlc -o webgui -I +../pkg-lib/pcre -I +../pkg-lib/netstring            *)
(*    pcre.cma unix.cma netstring.cma str.cma webgui.ml                     *)
(*                                                                          *)
(* (on one line)                                                            *)
(*                                                                          *)
(****************************************************************************)
open Galax_server_util
open Top_util
open InternalQuery
open Physical_value

(****************************************************************************)
let portmap    = Hashtbl.create 11
let gui_port   = ref 4000 
let flash_port = 4040
let first_msg_sent = ref false

let fatal_error exit_code msg = 
  Printf.eprintf "%s\n%!" msg;
  exit exit_code
(****************************************************************************)
let short_name name =
  let suffix = Printf.sprintf ":%d$" !dxq_default_port in
  Pcre.replace ~pat:suffix ~templ:"" name

(***** TIME FUNCTIONS   *****************************************************)
(* Milliseconds since beginning of run *)
let start_time = ref None

let delta_msecs() = 
  match !start_time with
  | None -> (start_time := Some (Unix.gettimeofday()); 0)
  | Some start -> int_of_float((Unix.gettimeofday() -. start) *. 1000.0)

let sync_gui_clocks() = 
  if (not(!first_msg_sent)) then
    begin
      first_msg_sent := true;
      start_time := Some (Unix.gettimeofday())
    end
  else ()

(****** MESSAGE MANAGEMENT ***************************************************)
let load_message gio = 
  try load_document_item gio
  with
  | e -> 
      begin
	let errmsg = 
	  match gio with
	  | Galax_io.String_Input s -> "string '"^s^"'"
	  | Galax_io.File_Input f -> "file "^f
	  | Galax_io.Http_Input s -> "http '"^s^"'"
	  | _ -> "Channel or Buffer input"
	in
	raise (Error.Query(Error.Parsing(Finfo.bogus, "Invalid message in "^errmsg)))
      end
(*****  GUI SERVER STATE *****************************************************)
module State = struct
  module StringSet =  Set.Make(struct type t = string let compare = String.compare end)

  (*****  FLASHPLAYER INTERFACE  ***********************************************)
  (* Flashplayer variables *)
  let flash_mutex = Mutex.create()
  let flash_channel_live = ref None      
  let flash_channel_receive_ready = ref false
  let flash_out_buffer = ref ""

  let set_flash_channel outchan =
    Mutex.lock flash_mutex;
    Printf.eprintf "Setting flash channel\n%!";
    (match !flash_channel_live with None -> ()
    | Some c -> (try 
        Printf.eprintf "set_flash_channel close\n%!";
        close_out c
    with _ -> ()));
    flash_channel_live := Some outchan;
    flash_channel_receive_ready := false;
    Mutex.unlock flash_mutex

  let flash_channel_ready() =
    Mutex.lock flash_mutex;
    flash_channel_receive_ready := true;
    Mutex.unlock flash_mutex

  (****** SEND EVENT TO FLASH *************************************************)
  let policy_file =
    "<?xml version=\"1.0\"?><!DOCTYPE cross-domain-policy SYSTEM \"http://www.adobe.com/xml/dtds/cross-domain-policy.dtd\"><cross-domain-policy><allow-access-from domain=\"*\" to-ports=\"4040\" /></cross-domain-policy>\000"

  let policy_file_request = "<policy-file-request/>"

  let send_policy_file() =
    Mutex.lock flash_mutex;
    begin
      match !flash_channel_live with
	Some c -> 
          begin
            Printf.eprintf "Sending policy file\n%!";
            Printf.fprintf c "%s%!" policy_file;
            Printf.eprintf "Closing flash channel\n%!";
            (try close_out c with _ -> ());
            flash_channel_live := None;
            flash_channel_receive_ready := false
          end
      | None -> () (* Should be impossible *)
    end;
    Mutex.unlock flash_mutex

  let report_to_flash xml =
    Mutex.lock flash_mutex;
    (match !flash_channel_live,!flash_channel_receive_ready with
      _,false -> 
	  (* Queue up messages for Flashplayer *)
	(Printf.eprintf "Queuing %s\n%!" xml;
	 flash_out_buffer :=  (!flash_out_buffer) ^ xml ^ "\000")
    | None, true ->
        (* Should be impossible *)
	(Printf.eprintf "Queuing %s\n%!" xml;
	 flash_out_buffer :=  (!flash_out_buffer) ^ xml ^ "\000")
    | Some c,true ->
        (try 
	  if not((!flash_out_buffer) = "") then 
	    begin
	      Printf.eprintf ("Queued messages:*%s*\n%!") (!flash_out_buffer); 
	      sync_gui_clocks();
	      Printf.fprintf c "%s%!" (!flash_out_buffer);
	      flash_out_buffer := ""
	    end
	  else ();
          if (not(xml = "")) then 
	    begin
(*	      if (!verbose_flag) then *)
 	      Printf.eprintf ("Sending: *%s*\n%!") xml;    
	      sync_gui_clocks();
	      Printf.fprintf c "%s\000%!" xml 
	    end
	  else ()
        with e ->
          (* A common exn here is EPIPE where the client Flash movie has
             been closed, hence the file descriptor is closed.  We have to
             ignore the SIGPIPE signal to get the EPIPE error, because the
             default is termination. 
	  *)
          (flash_channel_live := None;
           flash_channel_receive_ready := false;
	   Printf.eprintf "Error on flashplayer channel: %s\n%!"
             (Printexc.to_string e);
           Printf.eprintf "report_to_flash close\n%!";
           (try close_out c with _ -> ()))));
    Mutex.unlock flash_mutex

  (******* LOG DIRECTORY AND FILE MANAGEMENT **********************************)
  let log_flag = ref false
  let set_log b = log_flag := b
  let log_mutex = Mutex.create()

  let log_directory = Printf.sprintf "%s/webgui-%d" Filename.temp_dir_name (Unix.getpid())
  let log_file = Printf.sprintf "%s/webgui.log" log_directory 

  (* Uniquely named file for each cached message body *)
  let msg_file clock_tick src tgt = Printf.sprintf "%s/%d-%s-%s.msg" log_directory clock_tick src tgt

  let _ = 
    try Unix.mkdir log_directory 0o740
    with e -> fatal_error 1 (Printf.sprintf "Error creating %s: %s" log_directory (Error.bprintf_error "" e))
  let log_out_channel = open_out log_file

  let open_log() = 
    Mutex.lock log_mutex;
    if (!log_flag) then Printf.fprintf log_out_channel ""
    else ();
    Mutex.unlock log_mutex

  let close_log() = 
    Mutex.lock log_mutex;
    log_flag := false;
    Printf.fprintf log_out_channel "<end c='%d'/>" (delta_msecs()); 
    close_out log_out_channel;
    Mutex.unlock log_mutex

  let log_xml_comment s = 
    Mutex.lock log_mutex;
    Printf.fprintf log_out_channel "<!-- %s -->\n" s;
    Mutex.unlock log_mutex

  let log_message xml_msg = 
    Mutex.lock log_mutex;
    if (!log_flag) then
      Printf.fprintf log_out_channel "%s\000%!" xml_msg
    else ();
    Mutex.unlock log_mutex;
    report_to_flash xml_msg  (* Report to Flash *)

  let log_event report_msg_body_flag (msg_type, src, tgt, msg, attrs, elems) = 
    if not(msg_type = "") then 
      begin
      (* NB: all attributes must be printed with double quotes and not
         single quotes because otherwise the haXe "XML" parser chokes *)
	let clock_tick = delta_msecs() in 
 	let event = Printf.sprintf
             "<%s c=\"%d\" %s>%s</%s>" msg_type clock_tick attrs elems msg_type in

	log_message event;

        (* Cache message body on disk if not sent to Flashplayer *)
        (* Don't need to lock file, because each message is uniquely 
           identified by clock-tick, src, target  *)
	if (not(report_msg_body_flag)) then 
	  let msg_log_file = msg_file clock_tick src tgt in
	  let msg_outc = open_out msg_log_file in
	  Printf.fprintf msg_outc "%s%!" msg;
	  close_out msg_outc
	else ()
      end
    else ()
  (******* LOG DIRECTORY AND FILE MANAGEMENT **********************************)

  (***** GRAPH COLORS ***********)
  let color = ref 0
  let colors_reported = ref false
  let color_mutex = Mutex.create()
  let color_table = Hashtbl.create 11
  (* Default color table *)
  let default_colors = [("x",0);("!",1);("*",2);("?",3)] 
  let _ = 
    List.iter (fun (msg, clr) -> Hashtbl.add color_table msg clr) default_colors;
    color := 4

  let get_color msg = 
    Mutex.lock color_mutex;
    (* Report colors once *)
    if (not(!colors_reported)) then 
      begin
	List.iter 
	  (fun (msg, color) -> 
	    log_event false ("l", "", "", "", (Printf.sprintf "clr=\"%d\" name=\"%s\"" color msg), ""))
	  default_colors;
	colors_reported := true
      end
    else ();
    let c = 
      try Hashtbl.find color_table msg
      with Not_found ->
	begin
          let c = !color in
	  Hashtbl.add color_table msg (c);
          log_event false ("l", "", "", "", (Printf.sprintf "clr=\"%d\" name=\"%s\"" (!color) msg), "");
	  color := c + 1;
	  c
	end
    in
    Mutex.unlock color_mutex;
    c
  (***** GRAPH COLORS ***********)

  (******* EVENT STATE TABLE ***********)
  (* The event state table is largely used to give each event a unique state number.
     The state numbers are no longer used but could be useful in the future *)
  let state_mutex = Mutex.create()
  type show = { query:bool; response:bool; async:bool; start:bool; finish:bool;
                regexp:string; fnodes:StringSet.t }

  let state_default =
    { query=true; response=true; async=true; start=true; finish=true;
      regexp="";
      fnodes=StringSet.empty } (* filtered nodes: show node if NOT in set *)
  let state_table = Hashtbl.create 11

  let state_num = ref 0

  let new_state() =
    Mutex.lock state_mutex;
    state_num := !state_num + 1;
    let st = !state_num in
    Hashtbl.add state_table st state_default;
    Mutex.unlock state_mutex;
    st

  let is_state st = true
  let title = ref "Galax"
  let node_ct = ref 0
  let nodes = Hashtbl.create 11 
  let node_names() = Hashtbl.fold (fun s p c -> s::c) nodes []
  let new_node s prog = 
    begin
      Mutex.lock state_mutex;
      (try 
	ignore(Hashtbl.find nodes s) 
      with
      | Not_found -> Hashtbl.add nodes s (!node_ct,prog); incr node_ct);
      Mutex.unlock state_mutex
    end

  (****************************************************************************)

  (******  GRAPH LAYOUT MANAGEMENT ***********************************)
  (* Graph Layout : maps node name to incident "neighbor" nodes. *)
  let neighbor_graph_mutex = Mutex.create()
  let neighbor_graph = Hashtbl.create 11

  let new_neighbor s (t,l) = 
    begin
      Mutex.lock neighbor_graph_mutex;
      (try 
	let nbs = (Hashtbl.find neighbor_graph s) in
	let is_neighbor = List.exists (fun (t',l) -> t'=t) nbs in
	if not(is_neighbor) then Hashtbl.replace neighbor_graph s ((t,l)::nbs)
	else ()
      with
      | Not_found -> Hashtbl.add neighbor_graph s [(t,l)]);
      Mutex.unlock neighbor_graph_mutex
    end

  let replace_neighbor_edges s tgts = 
    begin
      Mutex.lock neighbor_graph_mutex;
      Hashtbl.replace neighbor_graph s tgts;
      Mutex.unlock neighbor_graph_mutex
    end

  let neighbor_event src tgts = 
    let tgt_strs = String.concat "" (List.map (fun (t, l) -> "<nd name=\""^(t)^"\" l=\""^l^"\"/>") tgts) in
    ("n", src, "", "", ("s=\""^src^"\""), tgt_strs)

  let neighbor_node_and_edges src = 
    Mutex.lock neighbor_graph_mutex;
    let tgts = 
      try Hashtbl.find neighbor_graph src with | Not_found -> []
    in 
    Mutex.unlock neighbor_graph_mutex;
    neighbor_event src tgts
end

(*****  HTML PAGE LAYOUT **************************************)
module HTML = struct

  let html_protect s =
    let s =
      Pcre.replace ~pat:"&" ~templ:"&amp;" s in
    let s =
      Pcre.replace ~pat:"<" ~templ:"&lt;" s in
    let s =
      Pcre.replace ~pat:">" ~templ:"&gt;" s in
    s

  (* for protecting attributes in double quotes.
     NB: not sure how to do in single quotes, there is no
     html entity for single quote? *)
  let attribute_protect s =
    let s =
      Pcre.replace ~pat:"&" ~templ:"&amp;" s in
    let s =
      Pcre.replace ~pat:"\"" ~templ:"&quot;" s in
    s

  (* Useful for printing javascript:alert("...") *)
  let js_protect s =
    let s = html_protect s in
    let s =
      Pcre.replace ~pat:"\\\\" ~templ:"\\\\" s in
  let s =
    Pcre.replace ~pat:"\"" ~templ:"\\\"" s in
  let s =
    Pcre.replace ~pat:"\'" ~templ:"\\\'" s in
  let s =
    Pcre.replace ~pat:"\n" ~templ:"\\n" s in
  s

  let draw_header b =
    (* TODO: ought to escape title *)
    Printf.bprintf b "%s"
      "<html xmlns='http://www.w3.org/1999/xhtml'>\n<head>\n";
    Printf.bprintf b
      "  <title>%s</title>\n" !State.title;
    Printf.bprintf b
      "  <style type='text/css'>\n    a.error { color: red; font-weight: bold; }\n  </style>";
    Printf.bprintf b
      "</head>\n<body>\n";
    Printf.bprintf b
      "  <h3>%s</h3>\n" !State.title;
    Printf.bprintf b "<div style='float: left; margin-right: 10px'>\n"

  let draw_programs b export =
    Printf.bprintf b "</div>\n";
    Printf.bprintf b "<p><b>Program:</b>\n";
    let node_names = List.sort compare(State.node_names()) in
    List.iter
      (fun n ->
        if export then
          Printf.bprintf b "<a href='prog-%s.html'>%s</a>\n"
            (short_name n)
            (short_name n)
        else
          let (vhost,phost,port) = Sim.lookup_host_port_in_portmap portmap n  in
          Printf.bprintf b "<a href='http://%s:%d/program'>%s</a>\n" phost port (short_name n))
      node_names;
    Printf.bprintf b "</p>\n"

  let draw_controls b =
    let node_names = List.sort compare(State.node_names()) in
    Printf.bprintf b "<p><b>Debug log:</b>\n";
    List.iter
      (fun n ->
        let (vhost,phost,port) = Sim.lookup_host_port_in_portmap portmap n  in
        Printf.bprintf b "<a href='http://%s:%d/debug'>%s</a>\n" phost port (short_name n))
      node_names;
    Printf.bprintf b "</p>\n";
    Printf.bprintf b
      "<form method='POST' action='/submit'>\n<p><b>Query:</b> <input name='qs' size='60'/></p>\n<p><b>Submit to:</b>\n";
    List.iter
      (fun n ->
        Printf.bprintf b
          "<input type='submit' name='node' value='%s'/>\n" (short_name n))
      node_names;
    Printf.bprintf b
      "<input type='submit' name='node' value='ALL'/>";
    Printf.bprintf b "</p></form>\n"

  let draw_footer b =
    Printf.bprintf b "%s" "</body></html>\n"

end (* module HTML *)

(***** THREADED SERVER *************************************************)
module ThreadServer = struct

  (***** UDP SERVER ***************************************************)
  (* Create a thread that loops, handling UDP GUI reports *)
  let udp_server event_handler port =
    let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any,port) in
    let sock = Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_DGRAM 0 in
    (try Unix.bind sock sockaddr;
    with _ -> fatal_error 1 (Printf.sprintf "Error: port %d on localhost already in use" port));
    ignore(
       Thread.create (fun () ->
	 try
           let buflen = Galax_server_util.gui_udp_buffer_length in
           let buf = String.create buflen in
           Printf.eprintf "Starting UDP loop\n%!";
           while true do
             let n = Unix.recv sock buf 0 buflen [] in
             event_handler (String.sub buf 0 n); (* FIX: no copy *)
             ()
           done
	 with e ->
	   begin
             Printf.eprintf "Error: %s\n%!" (Error.bprintf_error "" e);
	     raise e
	   end) 
	 ())

  let tcp_server server_fun port =
    let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any,port) in
    let rec accept_non_intr s =
      try Unix.accept s
      with
	Unix.Unix_error (Unix.EINTR, _, _) -> accept_non_intr s
      | e -> fatal_error 1 (Printf.sprintf "Error: %s" (Error.bprintf_error "" e))
    in
    let sock =
      Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    (try Unix.bind sock sockaddr;
    with _ -> fatal_error 1 (Printf.sprintf "Error: port %d on localhost already in use" port));
    Unix.listen sock 15;
    Printf.fprintf stderr "Listening on localhost:%d\n%!" port;

    while true do
      let (s, _) = accept_non_intr sock in
      ignore
        (Thread.create
	   (fun () ->
             let inchan = Unix.in_channel_of_descr s in
             let outchan = Unix.out_channel_of_descr s in
             (try server_fun inchan outchan with e ->
               Printf.eprintf "Error: %s\n%!"
                 (Error.bprintf_error "" e);
               ());
               (* Close outchan to flush output, but not inchan b/c it *)
               (* uses the same file descriptor as outchan             *)
             Printexc.print close_out outchan)
	   ())
    done
end (* module ThreadServer *)

(**********************************************************************)
(* Parse command-line arguments                                       *)
(**********************************************************************)
let query_files = ref []
let graph_topology : Top_util.Graph.graph_edge list ref = ref []
let servers = ref []
let control_file_contents = ref ""
;;

module Server = struct

  open Http.HTTP

  (**********************************************************************)
  (* Figure out local host name                                         *)
  (**********************************************************************)
  let my_host_port = ref ""

  (**********************************************************************)
  (* Diagnostic log messages                                            *)
  (**********************************************************************)
  let verbose_flag = ref false
  let set_verbose b = verbose_flag := b

  let report_msg_body_flag = ref false
  let set_report_msg b = report_msg_body_flag := b

  let drop_flag = ref false
  let set_drop b = drop_flag := b

  let message s =
    if !verbose_flag then
      (Printf.eprintf "[%s] %s\n" !my_host_port s; flush stderr)

  let redirect outchan target =
    let o = output_string outchan in
    o "HTTP/1.0 302 Moved Temporarily\r\n";
    o "Location: "; o target; o "\r\n";
    o "Connection: close\r\n";
    o "\r\n"

  (**********************************************************************)
  (* Request handling                                                   *)
  (**********************************************************************)
  let gui_error_message f e = 
    Printf.eprintf "Error in WebGUI %s: %s\n%!" f (Error.bprintf_error "" e)

  let gui_event_handler report =
    message (Printf.sprintf "Report received: %s\n" report);
    try 
      begin
	let ge = Gui.guievent_of_string report in
	let log_entry =
	  begin
	    match ge with
	    | Gui.GuiNone -> ("", "", "", "", "", "")
	    | Gui.GuiNewNode(name,info) ->
		(State.new_node name info; ("n", name, "", "", "s=\""^name^"\"", ""))
	    | Gui.GuiNode(src,"l",msg) ->
	  (* Neighbor/connectivity graph l: <graph name=''><edge s='' t='' l='' w=''>*</graph> *)
		begin
		  try
		    let input_doc = load_message (Galax_io.String_Input msg) in
		    let rootnode = eval_to_item eval_expr "./graph" input_doc in 
		    let edges = eval_to_item_list eval_expr "./edge" rootnode in
(*		    let s = (State.name2node src) in  *)
		    let nodes_and_labels = List.map (fun e ->
		      (let optlbl = eval_to_item_list eval_expr "./@l" e in 
(*		      State.name2node(eval_to_string eval_expr "./@t/string()" e),*)
		      (eval_to_string eval_expr "./@t/string()" e),
		      if (List.length optlbl > 0) then eval_to_string eval_expr "./string()" (List.hd optlbl) else "")) edges 
		    in
		    State.replace_neighbor_edges src nodes_and_labels;
		    State.neighbor_node_and_edges src 
		  with
		  | Error.Query(Error.Parsing(_)) -> ("", "", "", "", "", "")
		end
	  (* Overlay graph g: <graph name=''>(<edge s='' t='' l='' w=''>|<tree s=''/>|<node s=''/>)*</graph> *)
	    | Gui.GuiNode (src, "g", msg) ->
		begin
		  try
		    let input_doc = load_message (Galax_io.String_Input msg) in
		    let rootnode = eval_to_item eval_expr "./graph" input_doc in 
		    let name = eval_to_string eval_expr "./@name/string()" rootnode in
		    let edges = eval_to_item_list eval_expr "./edge" rootnode in
		    let trees = eval_to_item_list eval_expr "./tree" rootnode in
		    let nodes = eval_to_item_list eval_expr "./node" rootnode in
		    let elems = String.concat "" 
			((List.map (fun e ->
			  let s = eval_to_string eval_expr "./@s/string()" e in
			  let t = eval_to_string eval_expr "./@t/string()" e in
			  let l = eval_to_string eval_expr "./@l/string()" e in
			  let ws = eval_to_string eval_expr "./@w/string()" e in 
			  let clr = eval_to_string eval_expr "./@c/string()" e in 
			  let w = (if ws = "" then "1" else ws) in 
			  "<e s=\""^s^"\" t=\""^t^"\" l=\""^l^"\" w=\""^w^"\" c=\""^clr^"\"/>") edges)@
		      (List.map (fun e ->
			  let s = eval_to_string eval_expr "./@s/string()" e in
			  "<nd s=\""^s^"\"/>") nodes)@
		      (List.map (fun e ->
			  let s = eval_to_string eval_expr "./@s/string()" e in
			  "<t s=\""^s^"\"/>") trees))
		    in
		    ("g", src, "", "", "name =\""^name^"\" s=\""^src^"\"", elems)
		  with
		  | Error.Query(Error.Parsing _) -> ("", "", "", "", "", "")
		end
          (* Event at a node *)
	    | Gui.GuiNode (src, kind, msg) ->
		begin
		  if (kind = "!" && msg = "DEAD!") then 
		    begin
		      State.replace_neighbor_edges (src) [];
		    end
		  else ();
		  let msg_body = 
		    if !report_msg_body_flag then
		      "<![CDATA["^msg^"]]>"
		    else ""
		  in
		  ("m", src, "", msg, Printf.sprintf "s=\"%s\" clr=\"%d\" l=\"%s\"" src (State.get_color kind) kind, msg_body)
		end
          (* Message from source node to destination node *)
	    | Gui.GuiMessage(src,dest,kind,msg_name,msg) -> (* Message kinds: x, *, ?, ! *)
		begin
		  let msg_body = if !report_msg_body_flag then "<![CDATA["^msg^"]]>" else "" in
		  ("m", src, dest, msg, Printf.sprintf "s=\"%s\" t=\"%s\" clr=\"%d\"" src dest (State.get_color msg_name), msg_body)
		end
	  end
	in
	State.log_event (!report_msg_body_flag) log_entry
      end
    with 
    | e -> gui_error_message "gui_event_handler" e

  (* Request character data from a host node, e.g., a program *)
  let gui_content_request_to_host name request (open_elem, close_elem) =
    let (vhost,phost,port) = Sim.lookup_host_port_in_portmap portmap name in
    ignore(Thread.create
       (fun () ->
         (try
           let response = http_query_response vhost phost port request "GET" "" in
	   let msg = open_elem^"<![CDATA["^response^"]]>"^close_elem in
	   State.report_to_flash msg
         with e -> gui_error_message  "gui_content_request_to_host" e);
         Thread.exit())
       ())

  let get_connection_state headers =
    let c = get_header headers "Cookie" in
    try
      Scanf.sscanf c "session=%d" (fun st ->
        if State.is_state st then (st,"") else raise Not_found)
    with _ ->
      let st = State.new_state() in
      (st,header_set_session st)

  (* Submit a query to a host *)
  (* Threaded because query/response is synchronous *)
  let submit_to_host name qs =
    let (vhost,phost,port) = Sim.lookup_host_port_in_portmap portmap name in
    ignore
      (Thread.create
         (fun () ->
           (try
             (* expected response is "" so we can ignore it *)
             ignore(Http.HTTP.http_query_response vhost phost port "/job" "POST" qs);
           with e -> gui_error_message "submit_to_host" e);
           Thread.exit())
         ())

  let broadcast_submit qs =
    List.iter (fun name -> submit_to_host name qs) (State.node_names())


  (****************************************************************************)
  (***** FLASH MESSAGE HANDLER ***************************************************)
  let flash_handler message from_flash_chan = 
    try
      let b = Buffer.create 128 in
      while true do
	Buffer.clear b;
	try
	  while true do
            let c = input_char from_flash_chan in
            if c = '\000' then raise Not_found; 
            Buffer.add_char b c
	  done
	with Not_found ->
	  begin
	    let input = (Buffer.contents b) in
	    if (String.length input = 0) then ()(* Empty message *)
	    else
	      try
		Printf.eprintf ("From flashplayer:\n*%s*\n%!") input; 
		if input = State.policy_file_request then 
                  State.send_policy_file() 
		else if input = "<startSending/>" then
                  begin
                    State.flash_channel_ready();
                    State.report_to_flash "" (* Flush output buffer *)
                  end 
		else
                  let input_doc = load_message (Galax_io.String_Input (Buffer.contents b)) in

       (* Evaluate query : <q broadcast="">query-text</q> or  <q s="node">query-text</q> *)
		let query_list = eval_to_item_list eval_expr "./q" input_doc in 
		if (List.length query_list > 0) then 
		  let query_node = List.hd query_list in 
		  let query = eval_to_string eval_expr "./string()" query_node in
		  let broadcast_attr = eval_to_item_list eval_expr "./@broadcast" query_node in
		  if (List.length broadcast_attr > 0) then 
		    (message ("Broadcast:"^query^"\n");
		     broadcast_submit query)
		  else
		    (let src = eval_to_string eval_expr "./@s/string()" query_node in 
		    message ("Query "^src^":"^query^"\n");
		    submit_to_host src query)
        (* Get program content : <p s="node"/> *)
		else
		  let program_list = eval_to_item_list eval_expr "./p" input_doc in
		  if (List.length program_list > 0) then 
		    (let program_node = List.hd program_list in
		    let src = eval_to_string eval_expr "./@s/string()" program_node in 
		    message ("Get program of "^src^"\n");
		    gui_content_request_to_host src "/program-only" (Printf.sprintf "<p s=\"%s\">" src, "</p>"))
		  else 
        (* Get message content : <c c="tick" s="node" t="optional-target-node"/> *)
		    let content_list = eval_to_item_list eval_expr "./c" input_doc in
		    if (List.length content_list > 0) then 
		      let content_node = List.hd content_list in 
		      let clock = eval_to_string eval_expr "./@c/string()" content_node in 
		      let src = eval_to_string eval_expr "./@s/string()" content_node in 
		      message ("Get content of "^src^" at "^clock^"\n");
		      let tgts = eval_to_item_list eval_expr "./@t" content_node in 
                      let (tgt, tgt_attr) = 
			if (List.length tgts > 0) then 
			  let tgt = eval_to_string eval_expr "./string()" (List.hd tgts)  in
			  (tgt, "t=\""^tgt^"\"")
			else ("","")
                      in
                  (* Open file that contains message content *)
                      let log_file = State.msg_file (int_of_string clock) src tgt in
		      let msg_inc = open_in log_file in
                      let msg_buf = Buffer.create 128 in 
                      begin
			try 
			  while true do Buffer.add_char msg_buf (input_char msg_inc) done
			with
			| End_of_file -> 
			    let msg = "<c c=\""^clock^"\" s=\""^src^"\" "^tgt_attr^"><![CDATA["^(Buffer.contents msg_buf)^"]]></c>" in
			    (message("Content is "^msg^"\n");
 			     State.report_to_flash (msg))
                      end;
                      close_in msg_inc
   		    else
		      Printf.eprintf "Unknown message from Flashplayer \n%!" 
	      with
	      | Error.Query(Error.Parsing _) as e -> 
		  (Printf.eprintf "Error parsing message from Flashplayer %s\n%!" (Error.bprintf_error "" e); ())
	  end
      done
    with e ->
      Printf.eprintf "Error: %s\n%!" (Error.bprintf_error "" e)

  let flash_server from_flash_channel to_flash_channel =
    (* Store the to_flash_channel for later use by other threads.  Must
       use dup because tcp_server will close the file descriptor
       on exit from this function, the dup prevents it. 
    *)
    let fd = Unix.descr_of_out_channel to_flash_channel in
    State.set_flash_channel (Unix.out_channel_of_descr (Unix.dup fd));
    let from_flash_chan =
      Unix.in_channel_of_descr
        (Unix.dup (Unix.descr_of_in_channel from_flash_channel)) 
    in

    (* Report query files to Flash *)
    List.iter
      (fun (f,contents) ->
        let msg =
          Printf.sprintf "<query name=\"%s\"><![CDATA[%s]]></query>"
            f contents in
        State.log_message msg)
      !query_files;
    
    (* Create new thread for each UDP message Flashplayer *)
    ignore
      (Thread.create
         (fun () -> flash_handler message from_flash_chan)
	 ())

  (***** HTTP MESSAGE HANDLER ***************************************************)
  let http_handler inchan outchan =
    let (meth,headers,payload) = get_http_request inchan in
    message "Got a connection";
    (* Echo.echo (meth,headers,payload); *)
    let (state, header) = get_connection_state headers in
    match meth with
      Post("/report") -> (* A GUI event *)
        message "Report event";
        begin match payload with
          None -> message "Error: received an empty report"
        | Some report ->
            gui_event_handler report;
            send_html_http_response outchan header "";
        end
    | Post("/submit") -> (* A query submitted by browser *)
        begin match payload with
          None ->
            let response =
              "<html>\n<head>\n"^
              "<title>Error: empty query</title></head>\n<body>\n" ^
              "<h1>Error</h1>\n" ^
              "<p>You submitted an empty query</p>\n" ^
              "</body>\n</html>\n" in
            send_html_http_response outchan header response
        | Some formsubmit ->
            (* expect Content-Type: application/x-www-form-urlencoded *)
            let params =
              Netencoding.Url.dest_url_encoded_parameters formsubmit in
            try
              let qs = List.assoc "qs" params in
              let node = List.assoc "node" params in
              if node = "ALL" then broadcast_submit qs
              else submit_to_host node qs;
              redirect outchan "/" 
            with _ ->
              message "Error: received a garbled form submission"
        end
    | Get("/") ->
        (* TODO: this is duplicated in the next case below, refactor *)
        let b = Buffer.create 10 in
        HTML.draw_header b;
        HTML.draw_programs b false;
        HTML.draw_controls b;
        HTML.draw_footer b;
        message "Sending response";
        send_xml_http_response outchan header (Buffer.contents b);
        message "Response sent"
    | Get("/favicon.ico") ->
        message "Wants favicon";
        () (* common, ignore it *)
    | Get(s) ->
	begin
        message (Printf.sprintf "GET %s" s);
        let r_xhtml = Str.regexp "/\\(.*\\)\\.xhtml$" in
        let r_swf = Str.regexp "/\\(.*\\.swf\\)$" in
        try
          if (Str.string_match r_xhtml s 0) then begin
	    message "Matched .xhtml";
            let b = Buffer.create 10 in
            HTML.draw_header b;
            HTML.draw_programs b false;
            HTML.draw_controls b;
            HTML.draw_footer b;
            message "Sending response";
            send_xml_http_response outchan header (Buffer.contents b);
            message "Response sent"
          end
	  (* Startup the Flashplayer *)
          else if (Str.string_match r_swf s 0) then begin
	    message "Matched .swf";
            let file = Str.matched_group 1 s in
            let st = Unix.stat file in
            let len = st.Unix.st_size in
            let inch = open_in file in
            let b = Buffer.create len in
            Buffer.add_channel b inch len;
            send_http_response outchan
              ((header_content_type "application/x-shockwave-flash")^header)
              (Buffer.contents b);
            message "Response sent"
          end
        with _ -> redirect outchan "/" 
	end
      (************* GARBLED REQUESTS *************)
    | Post(s) ->
        message (Printf.sprintf "Error: received a garbled request (POST %s)" s);
        redirect outchan "/" 

  (***** GUI SERVER STARTUP ********************************************)
  let start_server portnum = (* never returns *)
    my_host_port := Printf.sprintf "%s:%d" (Sim.full_hostname()) portnum;

    Sim.check_network_up message; 
    (* Start Flashplayer handler *)
    ignore
      (Thread.create
	 (fun () ->ThreadServer.tcp_server flash_server flash_port)
	 ());

    (* Start UDP server, which handles GUI reports: returns immediately *)
    ThreadServer.udp_server gui_event_handler portnum;

    (* Populate neighbor graph and return rank ordered nodes *)
    let rank_ordered_nodes = 
      List.fold_left (fun nodes (s,t,lbls,wt) -> 
	State.new_node s "";
	let lbl = match lbls with
	| [] -> "" | lbl::_ -> lbl in
	State.new_neighbor s (t,lbl);
	if (List.mem s nodes) then nodes else nodes@[s]) [] !graph_topology
    in
    State.log_message !control_file_contents;
    (* Report neighbor graph, in ranked-node order, to Flashplayer *)
    List.iter (fun s -> 
      State.log_event (!report_msg_body_flag) (State.neighbor_node_and_edges s)) rank_ordered_nodes;
(*
    Hashtbl.iter (fun s nbs -> State.log_event (!report_msg_body_flag) (State.neighbor_event s nbs)) State.neighbor_graph; *)

    (* Send start-up message to each Galaxd Server *)
    List.iter
      (fun server ->
        (* Change host_port into host:port *)
        let server = Str.global_replace (Str.regexp "_") ":" server in

        let (vhost,phost,port) = Sim.lookup_host_port_in_portmap (portmap) server  in
        let my_host_port = Printf.sprintf "host=%s&port=%d" (Sim.full_hostname()) portnum in
        Printf.fprintf stderr "Asking %s to report (%s:%d)\n" server phost port;
        flush stderr;
        try
          let report = http_query_response vhost phost port "/guistart" "POST" my_host_port in
          (* report should be a GuiNewNode *)
          gui_event_handler report;
        with _ -> Printf.fprintf stderr "Error contacting %s:%d\n" phost port)
      (!servers);
    flush stderr;

    (* Start HTTP server: never returns *)
    ThreadServer.tcp_server http_handler portnum

end (* module Server *)
;;

(**********************************************************************)
(* MAIN PROGRAM                                                       *)
(**********************************************************************)
let parse_arguments() = Arg.parse
    [ ("-log",
       Arg.Bool State.set_log,
       "      Log all messages to /tmp/webgui-<processid>. ");
      ("-drop",
       Arg.Bool Server.set_drop,
       "      Ignore/drop inter-node messages; Only show state changes in HTML view. ");
      ("-msg",
       Arg.Bool Server.set_report_msg,
       "      Report message content to Flashplayer. ");
      ("-t",
       Arg.String (fun top_file -> 
	 let proc_ctxt = Processing_context.default_processing_context() in 
	 let input_doc = 
	   Physical_value.Item_Node
	     (List.nth (Galax.load_document proc_ctxt (Galax_io.File_Input top_file)) 0) in
	 graph_topology := Graph.load_graph input_doc "Neighbors" 
	 ),
       "      Load initial neighbor topology. ");
      ("-q",
       Arg.String
         (fun d ->
           let files = Gmisc.ls d "*.xq" in
           query_files :=
             (List.map (fun f ->
               let f_chopped = Filename.chop_suffix f ".xq" in
               let contents =
                 Gmisc.string_of_file(Filename.concat d f) in
               (f_chopped,contents)) files)
             @(!query_files)),
       " <d>  Look in directory <d> for query (.xq) files");
      ("-port",
       Arg.Int (fun i -> dxq_default_port := i),
       " <n>  Use <n> as the default port for servers");
      ("-guiport",
       Arg.Int (fun i -> gui_port := i),
       (Printf.sprintf
          " <n>  Use port <n> on localhost for the gui (default %d)" !gui_port));
      ("-d",
       Arg.String
         (fun d ->
           State.title := "Galax: " ^ (Filename.basename d);
           let server_files = Gmisc.ls d "*.xq" in
           servers := List.map
               (fun f -> Filename.chop_suffix f ".xq")
               server_files),
       " <d>  Contact servers defined in directory <d>");
      ("-s",
       Arg.String
         (fun d ->
           State.title := "Galax: " ^ (Filename.basename d);
           Sim.populate_portmap portmap d None (* Start on dxq_default_port *);
           if (List.length (Gmisc.ls d "control.xml") > 0) then
             control_file_contents :=
               Gmisc.string_of_file(Filename.concat d "control.xml");
           try
             let d_query = Filename.concat d "queries" in
             let files = Gmisc.ls d_query "*.xq" in
             query_files :=
               (List.map (fun f ->
                 let f_chopped = Filename.chop_suffix f ".xq" in
                 let contents =
                   Gmisc.string_of_file(Filename.concat d_query f) in
                 (f_chopped,contents)) files)
               @(!query_files)
           with _ -> (* if directory doesn't exist, will get error *) ()),
       " <d>  Simulate directory <d> on localhost");
      ("-v",
       Arg.Bool Server.set_verbose,
       "      Verbose operation");
    ]
    (fun s -> servers := s :: !servers)
    "Usage: webgui <options> host:port ...\nOptions are:"

(***********************)
(* Initialize port map *)
(***********************)
let init_portmap () = 
  begin
    let portmap_keys = Gmisc.keys_of_hashtable portmap in
    if (portmap_keys <> []) then 
      if (!servers <> []) then
	fatal_error 2 "Can't simulate a directory and specify other servers as well"
      else 
	servers := portmap_keys
    else 
      if (!servers = []) then
	fatal_error 2 "Must specify directory to simulate or the servers to execute"
      else ()
  end

let _ = 
  try 
    parse_arguments();
    init_portmap();
    (* Ignore SIGPIPE so that if a client closes a socket on us we do not
       terminate on write but instead return error EPIPE *)
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

    State.open_log();
    Server.start_server !gui_port
  with e ->
    begin
      Printf.eprintf "Error: %s\n%!" (Error.bprintf_error "" e);
      raise e
    end
      ;;
