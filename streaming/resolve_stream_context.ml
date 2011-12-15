(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: resolve_stream_context.ml,v 1.11 2007/08/30 22:39:53 simeon Exp $ *)

(* Module: Type_stream_context
   Description:
     This module implements a the context used when adding types to a
     stream.
*)

open Error

(*****************************)
(* A type for the ts context *)
(*****************************)

module HashedEnv =
  struct
    type t = Namespace_context.nsenv * Namespace_names.uqname
    let equal (e1,k1) (e2,k2) =
      (e1 = e2) && (k1 = k2)
    let hash (e1,k1) =
      Hashtbl.hash k1
  end

module HashtblEnv = Hashtbl.Make(HashedEnv)

type ts_context =
    { ts_nsenv          : Namespace_context.nsenv Stack.t;
      ts_attr_names 	: Namespace_symbols.symbol HashtblEnv.t;
      ts_elem_names 	: Namespace_symbols.symbol HashtblEnv.t }


(****************************)
(* Creates a new ts context *)
(****************************)

let build_ts_context () =
  let init_stack = Stack.create () in
  begin
    Stack.push (Namespace_context.default_xml_out_nsenv ()) init_stack;
    { ts_nsenv = init_stack;
      ts_attr_names = HashtblEnv.create 1439;
      ts_elem_names = HashtblEnv.create 1439; }
  end

(* Accesses the loading context *)

let get_nsenv ts_context =
  Stack.top ts_context.ts_nsenv

let pop_nsenv ts_context =
  try
    ignore(Stack.pop ts_context.ts_nsenv)
  with
  | _ ->
      raise (Query (Stream_Error "Empty stack during namespace resolution over a SAX stream"))


(*********************************************)
(* Adds namespace bindings to the ts context *)
(*********************************************)

let push_ns_bindings ts_context bindings =
  match bindings with
  | [] ->
      let in_scope_nsenv = get_nsenv ts_context in
      Stack.push in_scope_nsenv ts_context.ts_nsenv
  | _ ->
      let in_scope_nsenv = get_nsenv ts_context in
      let in_scope_nsenv' = Namespace_context.add_all_ns in_scope_nsenv bindings in
      Stack.push in_scope_nsenv' ts_context.ts_nsenv

let resolve_element_name ts_context nsenv uqname =
  try
    HashtblEnv.find ts_context.ts_elem_names (nsenv,uqname),false
  with
  | _ ->
      let rqname,default = Namespace_resolve.resolve_element_qname_default nsenv uqname in
      let s = (Namespace_symbols.relem_symbol rqname) in
      begin
	HashtblEnv.add ts_context.ts_elem_names (nsenv,uqname) s;
	s,default
      end

let attr_lookup_count = ref 0
let attr_missed_lookup_count = ref 0

let resolve_attribute_name ts_context nsenv uqname =
  try
    incr attr_lookup_count;
    HashtblEnv.find ts_context.ts_attr_names (nsenv,uqname)
  with
  | _ ->
      incr attr_missed_lookup_count;
      let rqname = Namespace_resolve.resolve_attribute_qname nsenv uqname in
      let s = (Namespace_symbols.rattr_symbol rqname) in
      begin
	HashtblEnv.add ts_context.ts_attr_names (nsenv,uqname) s;
	s
      end

