(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: execution_context.ml,v 1.11 2007/05/16 15:32:09 mff Exp $ *)

(* Module: Execution_context
   Description:
     This module implements the XQuery algebra context.
*)

open Error

open Xquery_common_ast

open Dm_atomic

open Physical_value

open Code_selection_context


(*******************************************************
 PROGRAM's run-time, DYNAMIC  context: 

     The context item +
     The context tuple +
     Current dateTime + 
     Snap semantic + 
     Update cache + 
     Local timezone +
     Index tables for the declared keys + 
     Alive documents

  Shared by all program components
*******************************************************)
type key_table =
    (string * string, item list) Hashtbl.t

type algebra_context =
    { mutable context_item    : item option;
      mutable cur_datetime    : atomicDateTime;
      mutable local_timezone  : atomicDayTimeDuration; 
      current_snap_semantic   : Xquery_common_ast.snap_modifier; (* Not mutable *)
      update_holder           : Update_ordering.update_holder;   (* Ref list *)
      keys            	      : key_table;  (* Mutable table *)
      alive_documents         : Fn_doc.alive_documents  (* Mutable table *) }

let mk_algebra_context ci cd lt css uh k = 
  { context_item          = ci;
    cur_datetime          = cd;
    local_timezone        = lt;
    current_snap_semantic = css;
    update_holder         = uh;
    keys                  = k;
    alive_documents      = Fn_doc.build_alive_documents_table () } 

(******************)
(* Initialization *)
(******************)

(* Set the initial timezone to UTC if can't get the system timezone *)

let initial_timezone () =
  try
    DateTime.local_timezone()
  with _ ->
    DateTime.default_UTC()

(* Set initial dateTime to January 1, 1970 at midnight UTC if there is
   a system error *)

let initial_datetime () =
  try 
    DateTime.current_dateTime()
  with _ ->
    DateTime.default_dateTime()

(* Creates a new algebra context *)


let build_algebra_context () =
  mk_algebra_context 
    None 
    (new atomicDateTime(initial_datetime()))
    (new atomicDayTimeDuration(initial_timezone()))
    Xquery_common_ast.Snap_Unordered_Deterministic
    (Update_ordering.make_fresh_place_holder ())
    (Hashtbl.create 167)

let copy_algebra_context alg_ctxt = 
  { context_item          = alg_ctxt.context_item;
    cur_datetime          = alg_ctxt.cur_datetime;
    local_timezone        = alg_ctxt.local_timezone;
    current_snap_semantic = alg_ctxt.current_snap_semantic;
    update_holder         = alg_ctxt.update_holder;
    keys                  = alg_ctxt.keys;
    alive_documents       = alg_ctxt.alive_documents}

let default_algebra_context () =
  build_algebra_context ()


(*************************************)
(* Support for current date and time *)
(*************************************)

let get_current_datetime alg_ctxt = alg_ctxt.cur_datetime
let get_timezone alg_ctxt = alg_ctxt.local_timezone

let set_timezone alg_ctxt tz  =
  alg_ctxt.local_timezone <- tz


(*****************************)
(* Support for declared keys *)
(*****************************)

(* Add a key and its value to algebra context *)

let add_key_to_algebra_context alg_ctxt (kn,key) node =
  let ht = alg_ctxt.keys in
  begin
    if (Hashtbl.mem ht (kn,key))
    then
      raise (Query (Key_Error (key,"Key definition binding node for value " ^ key ^ " twice")))
    else
      Hashtbl.add ht (kn,key) node
  end;
  alg_ctxt

(* Lookup a key's value in algebra context *)

let key_from_algebra_context alg_ctxt (kn,key) =
  let ht = alg_ctxt.keys in
  if (Hashtbl.mem ht (kn,key))
  then
    Hashtbl.find ht (kn,key)
  else
    raise (Query (KeyRef_Error (key,"Binding for key value " ^ key ^ " not found.")))


(********************************)
(* Support for update semantics *)
(********************************)
let get_current_snap_semantic alg_ctxt = alg_ctxt.current_snap_semantic
let set_current_snap_semantic alg_ctxt sm = 
  mk_algebra_context 
    alg_ctxt.context_item
    alg_ctxt.cur_datetime
    alg_ctxt.local_timezone
    sm
    alg_ctxt.update_holder
    alg_ctxt.keys

let get_ordering_structure alg_ctxt =
  match alg_ctxt.current_snap_semantic with
    | Xquery_common_ast.Snap_Ordered_Deterministic -> 
	alg_ctxt.update_holder
    | _ -> 
	raise (Query (Update_Error "Retrieving ordering structure in unordered semantic"))

let set_ordering_structure alg_ctxt uh = 
  { context_item          = alg_ctxt.context_item;
    cur_datetime          = alg_ctxt.cur_datetime;
    local_timezone        = alg_ctxt.local_timezone;
    current_snap_semantic = alg_ctxt.current_snap_semantic;
    update_holder         = uh;
    keys                  = alg_ctxt.keys;
    alive_documents       = alg_ctxt.alive_documents}

let allocate_update_holder alg_ctxt = 
  let uh = Update_ordering.allocate_update_holder alg_ctxt.update_holder in
    set_ordering_structure alg_ctxt uh

let enter_snap alg_ctxt sm = 
  { context_item          = alg_ctxt.context_item;
    cur_datetime          = alg_ctxt.cur_datetime;
    local_timezone        = alg_ctxt.local_timezone;
    current_snap_semantic = sm;
    update_holder         = (Update_ordering.make_fresh_place_holder ());
    keys                  = alg_ctxt.keys;
    alive_documents       = alg_ctxt.alive_documents}

(* Alive documents *)
let alive_documents_from_algebra_context alg_ctxt =
  alg_ctxt.alive_documents

(* let _ = print_string("Execution_context\n") *)
