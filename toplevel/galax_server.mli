(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_server.mli,v 1.26 2007/11/06 16:13:09 mff Exp $ *)
open Galax_server_util

val server_error_msg : exn -> string

module type SERVERKIND =
  sig
    val async_eval : Processing_context.async_eval_sig
    val delay : float -> unit
    type http_request = 
	(in_channel * out_channel * Http.HTTP.http_method * Http.HTTP.header list * string option)
    val http_tcp_server : (* Expected to be an infinite loop *)
        bool -> (exn -> unit) -> (http_request -> unit) -> Unix.sockaddr -> unit
    val udp_server :      (* Expected to return immediately *)
        bool -> (exn -> unit) -> (bytes -> unit) -> Unix.sockaddr -> unit
  end

module type GALAXSERVER =
  functor (ServerKind : SERVERKIND) ->
  sig
    val full_hostname  : string
    val short_hostname : string

    val log_debug_name : unit -> string

    val portmap : Galax_server_util.Sim.portmap

    (* Initialize server with:
       drop-msgs flag,
       optional virtual hostname, optional physical port,
       and inter-server latencies, if known. 
    *)
    val init : bool -> string option -> int option -> Top_util.Graph.graph_edge list -> unit
    val async_eval : Processing_context.async_eval_ext_sig

    (* Map host-port-string to virtual-host, physical-host, physical-port triple *)
    val interpret_hostport_string : Processing_context.interpret_hostport_sig
    val evaluate_closure : Processing_context.evaluate_closure_sig
    val evaluate_remote_query : Processing_context.evaluate_remote_query_sig
    val start_server      : string -> int -> 
      (* Namespace prefix and URI of exported module *)
      (Namespace_names.ncname * string * Galax.prepared_program) -> string -> unit
  end

module Server : GALAXSERVER

module DXQBuiltins :
  sig
    val add_symbol :
      Namespace_names.ncname ->
      int ->
      (Code_selection_context.code_selection_context -> Code_fn.bltin) ->
      unit
    val farg0 :
      Namespace_names.ncname ->
      (Code_selection_context.code_selection_context ->
       Execution_context.algebra_context -> Physical_value.item Cursor.cursor) ->
      unit
    val farg1 :
      Namespace_names.ncname ->
      (Code_selection_context.code_selection_context ->
       Execution_context.algebra_context ->
       Physical_value.item Cursor.cursor -> Physical_value.item Cursor.cursor) ->
      unit
    val farg2 :
      Namespace_names.ncname ->
      (Code_selection_context.code_selection_context ->
       Execution_context.algebra_context ->
       Physical_value.item Cursor.cursor * Physical_value.item Cursor.cursor ->
       Physical_value.item Cursor.cursor) ->
      unit
  end

