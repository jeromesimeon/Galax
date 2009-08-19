(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(*

  Galax_server_util: Functionality shared by the Galaxd daemon,
  Galax_server and Webgui.

      * Server-name management 
      * Shared GUI utilities

*)

(* Server defaults: *)
val dxq_default_port : int ref       (* 3000 *)
val gui_udp_buffer_length : int      (* 4096 *)
val query_udp_buffer_length : int    (* 16384 *)

val zerod_default_port : int ref     (* 2020 *)

(* Pretty prints HTML using "tidy" *)
val pretty_print_html : string -> string

(* Translate hostname, port into IP-address socket address.
   Raises DXQ_Error if IP address not found.
*)
val get_inet_socket_address : string -> int -> Unix.sockaddr

(* Parse "host[:port]" string into (host, port-int option, port-string) *)
val parse_host_port_string : string -> string * int option * string

(* Redirect all of Galax server's output to buffer *)
val set_server_output: Buffer.t -> unit

(* Log debugging output contained in buffer in file *)
val log_debug : string -> Buffer.t -> unit

(*
   Gui interface
*)
module Gui :
  sig
    type nodeName = string
    type guiEvent =
                      (* src  * target * msg-kind * short-name * full-msg *)
        GuiMessage of nodeName * nodeName * string * string * string 
                      (* src  * msg-kind * full-msg *)
      | GuiNode of nodeName * string * string 
      | GuiNewNode of nodeName * string
      | GuiNone
    (* Initialize GUI state with "drop message" flag and host and port of 
       Webgui process. *)
    val gui_init : bool -> string -> int -> unit
    val guievent_of_string : string -> guiEvent
    val string_of_guievent : guiEvent -> string
    val gui_report : Unix.file_descr -> bool ->  guiEvent -> unit
  end

                       (* Virtual host name, physical host name, physical port *)
type server_location = (Gui.nodeName * string * int)

type xquery_kind =
    XQueryString
  | XQueryPlan
  | XQueryPlanAsync

type evaluate_remote_query_sig = 
    (bool * server_location * xquery_kind * string * string -> string option)

type evaluate_closure_sig = 
    string -> string -> (Xquery_physical_type_ast.physical_type * Physical_value.physical_value)

type async_eval_sig = bool -> (exn -> unit) -> (unit -> unit) -> unit
type async_eval_ext_sig = (unit -> unit) -> unit

type interpret_hostport_sig = string -> server_location

(*
   Simulation values and types
*)
module Sim :
  sig
    (* Maps virtual port names to port, xquery-file, xquery-program tuples *)
    type portmap = (string, (int * string * string)) Hashtbl.t

    (* full_hostname = e.g., saul.cis.upenn.edu or localhost, if not fully qualified *)
    val full_hostname : unit -> string 
    (* short_hostname = e.g., saul *)
    val short_hostname : string -> string 

    (* Map files in named directory to (host,port,file-contents) *)
    (*            directory -> starting-port -> [(virtual-host,port,filename,file-contents)] *)
    val populate_portmap : portmap -> string -> int option -> unit
    val lookup_host_port_in_portmap : portmap -> string -> server_location
    val check_network_up : (string -> unit) -> unit
  end

