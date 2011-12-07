(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: resolve_stream_context.mli,v 1.7 2007/08/17 18:28:54 simeon Exp $ *)

(* Module: Type_stream_context
   Description:
     This module implements a the loading context.
*)


(*****************************)
(* A type for the ts context *)
(*****************************)

type ts_context


(****************************)
(* Creates a new ts context *)
(****************************)

val build_ts_context : unit -> ts_context

(***************************)
(* Accesses the ts context *)
(***************************)

val get_nsenv : ts_context -> Namespace_context.nsenv
val pop_nsenv : ts_context -> unit


(*********************************************)
(* Adds namespace bindings to the ts context *)
(*********************************************)

val push_ns_bindings : ts_context -> Namespace_context.binding_table -> unit

val resolve_element_name : ts_context -> Namespace_context.nsenv -> Namespace_names.uqname -> Namespace_symbols.symbol * bool

val resolve_attribute_name : ts_context -> Namespace_context.nsenv -> Namespace_names.uqname -> Namespace_symbols.symbol

