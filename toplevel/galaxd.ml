(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galaxd.ml,v 1.85 2008/02/01 02:45:26 mff Exp $ *)

(**********************************************************************)
(* Trevor Jim                                                         *)
(*                                                                    *)
(* Galaxd: stand-alone Galax server that communicates over HTTP.      *)
(**********************************************************************)

(* Typically, you will invoke galaxd

       galaxd a.xq b.xq ...

   This starts up a server listening on the default port, 3000.  The
   files a.xq, b.xq, ... are the context for evaluation, and they are
   optional.

   You can submit a query to the server by visiting
   http://localhost:3000/ with your web browser.  This will give you a
   form to submit an xquery expression for evaluation.

   The galaxd server can also be queried by a galax process running
   over the network.  For example, if you run galaxd on host
   alice.example.com, you can use the following xquery expression on
   another host bob.example.com:

       fn:doc("dxq://alice.example.com/")

   This will cause bob.example.com to evaluate the query

       local:main()

   using the galaxd server on alice.example.com.  In this case, the
   context files a.xq, b.xq, ... must define the function
   local:main().

   You may use the -port flag to start the server on a non-default
   port, e.g.,

       galaxd -port 8000

   will start the server on port 8000 instead of port 3000.

   It is sometimes useful to simulate a virtual network of galaxd
   servers.  Use the -d flag to do this:

       galaxd -s dir

   Here, dir must be a directory containing xquery files, on per host
   that you want to simulate.  For example, if dir contains files

       a.xq, b.xq, c.xq, d.xq, e.xq

   then galaxd will simulate a network of five hosts named a, b, c, d,
   and e on localhost.  Within the programs a.xq, b.xq, ...,

       fn:doc("dxq://a/")

   will evaluate local:main() on the server for a, and similarly

       fn:doc("dxq://b/")

   will evaluate local:main() on the server for b, etc.

   The server for a will actually be listening to port 3000 on
   localhost, b will be on port 3001, etc.  These ports can be used to
   submit queries to the servers using a web browser.

*)

open Debug

open Error

open Format
open Print_top
open Galax_server_util
open Galax_server

open Monitoring_context
open Processing_context

open Top_util
open Top_config
open Top_options

open Xquery_algebra_ast

(******************* Module THREADED SERVER START ****************)
(*
  A module that implements a threaded process server
*)
module ThreadServer : SERVERKIND =
  struct
    let num_t = ref 1
    let num_m = Mutex.create()
    let tcreate debug f x =
      let _ = Thread.create (fun x -> Random.init (Thread.id (Thread.self())); f x) x in
      if debug then
	begin
	  Mutex.lock num_m;
	  num_t := !num_t + 1;
	  Printf.eprintf "Thread.create (%d total)\n%!" !num_t;
	  Mutex.unlock num_m
	end
    let texit debug =
      begin
	if (debug) then
	  begin
	    Mutex.lock num_m;
	    num_t := !num_t - 1;
	    Printf.eprintf "Thread.exit (%d left)\n%!" !num_t;
	    Mutex.unlock num_m;
	  end;
	Thread.exit()
      end
    let delay = Thread.delay
    let async_eval debug exn_handler f =
      tcreate debug
        (fun () ->
          (try f () with e ->
	    begin
              Format.eprintf
		"Uncaught exception in async_eval %s\n%!" (Error.bprintf_error "" e);
	      exn_handler e;
	    end);
          texit debug) ()

    (* Never returns *)
    type http_request = (in_channel * out_channel * Http.HTTP.http_method * Http.HTTP.header list * string option)
    let http_tcp_server debug exn_handler http_request_handler sockaddr =
      let rec accept_non_intr s =
	try Unix.accept s
	with Unix.Unix_error (Unix.EINTR, _, _) -> accept_non_intr s in
      let sock =
	Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
      Unix.setsockopt sock Unix.SO_REUSEADDR true;
      Unix.bind sock sockaddr;
      Unix.listen sock 15;

      while true do
(* Format.eprintf "Before accept\n%!"; *)
	let (s, client_addr) = accept_non_intr sock in
(* Format.eprintf "After accept\n%!"; *)
        let inchan = Unix.in_channel_of_descr s in
        let outchan = Unix.out_channel_of_descr s in
(* Format.eprintf "Before HTTP get request\n%!"; *)
	let (meth,headers,payload) = Http.HTTP.get_http_request inchan in
	if (Debug.dxq_debug()) then
	  Http.HTTP.Echo.echo (meth,headers,payload);
	let _ = Thread.create (*tcreate debug*)
	  (fun http_request ->
(* Format.eprintf "In TCP thread create\n%!"; *)
            (try http_request_handler http_request
	    with e ->
		 begin
                   Format.eprintf "Error: uncaught exception %s in tcp loop\n%!"
                     (Printexc.to_string e);
		   exn_handler e;
                   ()
		 end;
	      );
            (try close_out outchan with _ -> ());
            (* Don't close inchan b/c it uses the same file descriptor *)
            (* as outchan                                              *)
            texit debug)
	  (inchan, outchan, meth, headers, payload)
	in ()
      done
    let udp_server debug exn_handler f sockaddr =
      let sock = Unix.socket (Unix.domain_of_sockaddr sockaddr)
          Unix.SOCK_DGRAM 0 in
      Printexc.print (Unix.bind sock) sockaddr;
      ignore (* Don't return thread handle b/c would force toplevel
                programs that don't need threads to link in the
                threads library *)
        (Thread.create
           (fun () ->
(* Format.eprintf "In UDP thread create\n%!"; *)
	     Random.init (Thread.id (Thread.self()));
             let buflen = Galax_server_util.query_udp_buffer_length in
             let buf = String.create buflen in
             while true do
               try
                 let n = Unix.recv sock buf 0 buflen [] in
                 f(String.sub buf 0 n); (* TODO: eliminate copy *)
               with e ->
		 begin
                   Format.eprintf "Error: uncaught exception %s in udp loop\n%!"
                     (Printexc.to_string e);
		   exn_handler e;
                   ()
		 end
             done)
           ())
  end

module GalaxdServer = Server(ThreadServer)
(******************* Module THREADED SERVER END ****************)

(******************* DXQ BUILT-IN FUNCTIONS *******************
   Thread Primitives that are exposed as builtin functions in DXQ.
   They are included in this module, so that only one Galax module 
   depends on Threads library
*)
module Sync = struct
  open DXQBuiltins
  (* Mutexes.  Note, they will never be collected *)
  let mmutex = Mutex.create()
  let mtable = Hashtbl.create 11
  let mindex = ref 0
  let mget p =
    Mutex.lock mmutex;
    let m = Hashtbl.find mtable (Top_util.InternalQuery.get_int (Cursor.list_of_cursor "Galaxd.Sync.mget" p)) in
    Mutex.unlock mmutex;
    m

  (* Condition variables.  Note, they will never be collected *)
  let cvmutex = Mutex.create()
  let cvtable = Hashtbl.create 11
  let cvindex = ref 0
  let cvget p =
    Mutex.lock cvmutex;
    let cv = Hashtbl.find cvtable (Top_util.InternalQuery.get_int (Cursor.list_of_cursor "Galaxd.Sync.cvget" p)) in
    Mutex.unlock cvmutex;
    cv

  (* Install symbols *)
  let _ = begin
    farg1 "delay" (fun comp_ctxt alg_ctxt p ->
      let i = Physical_util.get_integer p in
      Thread.delay (Decimal._float_of_integer i);
      Cursor.cursor_empty());

    farg0 "mcreate" (fun comp_ctxt alg_ctxt  ->
      Mutex.lock mmutex;
      let i = !mindex in
      mindex := i+1;
      let m = Mutex.create() in
      Hashtbl.add mtable i m;
      Mutex.unlock mmutex;
      Cursor.cursor_of_singleton (Physical_item_util._integer (Decimal._integer_of_int i)));

    farg1 "lock" (fun comp_ctxt alg_ctxt p ->
      let m = mget p in
      Mutex.lock m;
      Cursor.cursor_empty());

    farg1 "try_lock" (fun comp_ctxt alg_ctxt p ->
      let m = mget p in
      let b = Mutex.try_lock m in
      Cursor.cursor_of_singleton (Physical_item_util._boolean b));

    farg1 "unlock" (fun comp_ctxt alg_ctxt p ->
      let m = mget p in
      Mutex.unlock m;
      Cursor.cursor_empty());

    farg0 "cvcreate" (fun comp_ctxt alg_ctxt  ->
      Printf.printf "Creating CV\n"; flush stdout;
      Mutex.lock cvmutex;
      let i = !cvindex in
      cvindex := i+1;
      let cv = Condition.create() in
      Hashtbl.add cvtable i cv;
      Mutex.unlock cvmutex;
      Printf.printf "Creating DONE\n"; flush stdout;
      Cursor.cursor_of_singleton (Physical_item_util._integer (Decimal._integer_of_int i)));

    farg1 "signal" (fun comp_ctxt alg_ctxt p ->
      let cv = cvget p in
      Condition.signal cv;
      Cursor.cursor_empty());

    farg1 "broadcast" (fun comp_ctxt alg_ctxt p ->
      Printf.printf "Broadcasting CV\n"; flush stdout;
      let cv = cvget p in
      Condition.broadcast cv;
      Printf.printf "Broadcasting DONE\n"; flush stdout;
      Cursor.cursor_empty());

    farg2 "wait" (fun comp_ctxt alg_ctxt (p1,p2) ->
      Printf.printf "Waiting on CV\n"; flush stdout;
      let cv = cvget p1 in
      let m = mget p2 in
      Condition.wait cv m;
      Printf.printf "Waiting DONE\n"; flush stdout;
      Cursor.cursor_empty());

    add_symbol "my_address" 0
      (fun code_ctxt alg_ctxt n ->
	let comp_ctxt = Code_selection_context.compile_context_from_code_selection_context code_ctxt in
	let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
	let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
        let a =
          match (proc_ctxt.dxq_host, proc_ctxt.dxq_port) with
          | (Some h, Some 0) -> h
          | (Some h, Some p) -> h ^ ":" ^ (string_of_int p)
          | (Some h,_) -> h
          | (_,Some p) -> "localhost:" ^ (string_of_int p)
	  | (None,None) -> raise (Query(DXQ_Error("In dxq:my_address(): host/port undefined in processing context")))
        in
        Cursor.cursor_of_singleton (Physical_item_util._string a));
  end
end
(******************* DXQ BUILT-IN FUNCTIONS *******************)

(********** PROCESS MANAGEMENT ********)
(* In case we are running a simulation, we will be forking a bunch
   of processes.  Set things up so we can kill them all if there is
   any problem. *)
let mainpid = Unix.getpid() (* executed once in main process *)
;;
Sys.set_signal Sys.sigterm
    (Sys.Signal_handle
       (fun _ ->
	 Printf.eprintf "Received SIGTERM signal\n%!";
	 if Unix.getpid() = mainpid then
	   Unix.kill 0 Sys.sigterm;
	 exit 1))
;;
let terminate_mainpid proc_ctxt () =
  begin
    Monitor.serialize_monitor proc_ctxt;
    Printf.eprintf "In terminate_mainpid\n%!";
    Unix.kill mainpid Sys.sigterm;
         (* Never reached *)
    Unix.sleep 1000
  end
;;
(********** PROCESS MANAGEMENT END ********)

(********** GALAXD START-UP ********)

let load_latencies proc_ctxt =
  match proc_ctxt.dxq_topology with
  | None -> []
  | Some latency_file ->
      try
	let input_doc = 
	  Physical_value.Item_Node (List.nth (Galax.load_document proc_ctxt (Galax_io.File_Input latency_file)) 0) in
	Graph.load_graph input_doc "Latencies" 
      with
      |	exn -> raise (Query(Internal_Error("In Galax.load_latencies "^(Error.bprintf_error "" exn))))
(*
      let rootnode = eval_to_item eval_expr "./config" input_doc in
      let latency_root = eval_to_item eval_expr "./graph[name='Latencies']" rootnode in
      let latencies = eval_to_item_list eval_expr "./edge" latency_root in
      List.map (fun l ->
	let src = eval_to_string eval_expr "./@s/string()" l in
	let tgt = eval_to_string eval_expr "./@t/string()" l in
	let lat = eval_to_float eval_expr "xs:float(./@w)" l in
        (src, tgt, lat)
	) latencies 
*)

let compile_server_module proc_ctxt xq_module =
  let mod_ctxt = init_all proc_ctxt in
  Galax.export_server_module mod_ctxt (Galax_io.String_Input xq_module)

(*********** GALAXD SERVER BODY *********)
let server_body proc_ctxt latencies hostopt portopt xq_module termination =
  begin
    let port =
      match portopt with None -> !Galax_server_util.dxq_default_port | Some port -> port in
    let host =
      match hostopt with None -> "localhost" | Some host -> host in

    Printf.eprintf "galaxd: initializing server %s\n%!" host;
    let buff_debug = Buffer.create 512 in
    try

       (* set_output redirects all formatters, so after this point, ALL DEBUG
          & ERROR OUTPUT is written to buff_debug.  *)

      Galax_server_util.set_server_output buff_debug;

      (* init() initializes server's error output   *)
      (* Could we just pass all this info in proc_ctxt? *)
      GalaxdServer.init (proc_ctxt.dxq_drop_msgs) hostopt portopt latencies;

      (* Not sure what physical host name should be : fully qualified name or short name *)
      Processing_context.set_dxq_host_port
	proc_ctxt (Some GalaxdServer.short_hostname) (Some port);

      let (prefix, uri, compiled_program) = compile_server_module proc_ctxt xq_module in
      (* Can we just make the exported library the main module? *)
      let prepared_program = Galax.prepare_program compiled_program None in
      log_debug (GalaxdServer.log_debug_name()) (buff_debug);

      Printf.eprintf "galaxd: Before start_server %s\n%!" host;
      GalaxdServer.start_server host port (prefix, uri, prepared_program) xq_module

    with e ->
      let s = Error.bprintf_error "  " e in
      Printf.eprintf "%s\n%!" s;
      Printf.eprintf "galaxd: all servers will exit\n%!";
      log_debug (GalaxdServer.log_debug_name()) (buff_debug);
         (* Exception in the main process.  Kill the main process,
            so that all children terminate. *)
      match hostopt with
      | None -> Printf.eprintf "galaxd: exception in server on port %d\n%!" port;
      | Some host -> Printf.eprintf "galaxd: exception in server for %s on port %d\n%!" host port;
      termination()
  end
;;
(*********** GALAXD SERVER BODY *********)

(*********** SIMULATION START-UP LOOP *********)
let rec sim_loop proc_ctxt  latencies l =
  match l with
    [] -> ()
  | [(vhost,(port,filename,xq_module))] ->
      server_body proc_ctxt  latencies (Some vhost) (Some port) xq_module (terminate_mainpid proc_ctxt)
  | (vhost,(port,filename,xq_module))::tl ->
      let pid = Unix.fork() in
      Random.self_init ();
      if pid <> 0 then sim_loop proc_ctxt latencies tl 
      else
	begin
	  if (Monitoring_context.get_monitor_mem proc_ctxt.monitor_context ||
   	      Monitoring_context.get_monitor_time proc_ctxt.monitor_context) then 
	    Top_options.arg_output_monitor proc_ctxt ("monitor-"^vhost^".xml");
	  let termination() =
	    begin
	      Monitor.serialize_monitor proc_ctxt;
	      Printf.eprintf "In termination: Received SIGTERM signal\n%!";
              Unix.kill (Unix.getppid()) Sys.sigterm;
              exit 1
	    end
	  in
	  server_body proc_ctxt latencies (Some vhost) (Some port) xq_module termination
	end
;;
(*********** SIMULATION START-UP LOOP *********)

let start_server proc_ctxt latencies xq_module =
  (* Note! Server must fork once! *)
  let pid = Unix.fork() in
  if pid <> 0 then
  server_body proc_ctxt latencies (Some GalaxdServer.short_hostname) (proc_ctxt.dxq_port) xq_module (terminate_mainpid proc_ctxt)
;;

(************************)
(* Command-line options *)
(************************)

let process_args proc_ctxt =
  let args =
    make_options
      proc_ctxt
      usage_galax_daemon
      [Daemon_Options;Misc_Options;Monitoring_Options;ProcessingPhases_Options;Behavior_Options;Printing_Options;Optimization_Options;CodeSelection_Options]
  in
  List.rev args
;;

let _ =
  try
    begin
      (* Configuration HACKS, HACKS, and MORE HACKS *)
      Conf.allow_streamed_tuple_fields := false;
      Conf.set_language Conf.Language_DXQ;
      (* Changed Conf.materialize_tables_kind from Analysis (i.e., always
	 do analysis) to Always, due to quadratic memory usage bug somewhere
	 in Alg_path_structutil.path_sequences_with_disjoint_roots 
      *)
      Conf.set_materialize_tables Conf.Always;

   (* Mary: I think each server should have its own copy of the processing context. *)
      let proc_ctxt = galax_run_proc_ctxt () in

  (* specify xml output*)
      Processing_context.set_serialization_kind proc_ctxt
	Processing_context.Serialize_As_Well_Formed ;

  (* 2. Parse command-line arguments *)
      let files = process_args proc_ctxt in

   (* Set the server in the processing context *)
      Processing_context.set_dxq_server proc_ctxt
	(GalaxdServer.evaluate_closure, GalaxdServer.evaluate_remote_query, GalaxdServer.async_eval, GalaxdServer.interpret_hostport_string);

      match (proc_ctxt.dxq_source) with
      | (None) ->
      (* xq_module : XQuery Server Module that is exported by the Galax daemon *)
	  let xq_module = String.concat "\n" (List.map Gmisc.string_of_file files) in
	  let latencies = load_latencies proc_ctxt in
	  start_server proc_ctxt latencies xq_module
      | (Some (LocalSimulation d)) ->
      (* d : Directory containing set of XQuery Server Modules that
         are exported by the Galax daemon *)
	  Sim.populate_portmap GalaxdServer.portmap d proc_ctxt.dxq_port;
	  Sys.chdir d; (* chdir because the files in the directory may contain relative  *)
                   (* references to other files in the directory; so we need a       *)
                   (* standard place to start                                        *)
	  (* The latencies are loaded from the sim directory *)
	  let latencies = load_latencies proc_ctxt in
	  sim_loop proc_ctxt latencies (Gmisc.all_of_hashtable GalaxdServer.portmap)
      | (Some(RemoteExecution d)) ->
 	  Sys.chdir d; (* chdir because the files in the directory may contain relative  *)
                    (* references to other files in the directory; so we need a       *)
                    (* standard place to start                                        *)
 	  (* The latencies are loaded from the sim directory *)
 	  let latencies = load_latencies proc_ctxt in
 	  let file1 = (GalaxdServer.full_hostname) ^ ".xq" in
 	  let file2 = (GalaxdServer.short_hostname) ^ ".xq" in
 	  let xq_module =
 	    if Sys.file_exists file1 then Gmisc.string_of_file file1 else
 	    if Sys.file_exists file2 then Gmisc.string_of_file file2 else
 	    (Printf.eprintf "Error: cannot find program for host %s in %s\n%!" (GalaxdServer.short_hostname) d;
 	     exit 1) in
 	  start_server proc_ctxt latencies xq_module
    end
  with
  | e ->
      begin
 (* If we end up here, something has gone very, very wrong *)
 	Printf.eprintf "Fatal Error: %s\n%!" (Error.bprintf_error_safe "" e);
 	eprintf_error_safe "  " e;
 	Format.fprintf (!Conf.glx_err_formatter) "@.";
 	Format.pp_print_flush !Conf.glx_err_formatter ();
      end
	;;
	
	
