(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: tuple_context_manager.ml,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Tuple_context_manager
     This module eals with variable context during runtime.  It also
     holds the build functions to access and store variables
 *)

open Error
open Xquery_common_ast

open Namespace_names
open Physical_value
open Array

(********************************)
(*** Variable Context Manager ***)
(********************************)


(* Consider using polymorphic types to close off after instantiated objects *)

type tuple_context_manager = {
  tuple_stack               : xml_value array Stack.t;
  mutable current_tuple     : xml_value array;
  last_slot                 : int ref; 
  (* Debugging parameters *)
  id                        : int;  
}
type tuple_ref = tuple_context_manager * int
let empty_physical_xml = (DomValue (Physical_sequence.sequence_empty ()))

let string_of_tuple_ref v = 
  let tcm, vr = v in
    "t"^(string_of_int tcm.id) ^ ":" ^ (string_of_int vr)

let get_new_tuple_slot tcm = 
  let ls = !(tcm.last_slot) in
    incr (tcm.last_slot);
    tcm,ls

let debug_id = ref 0;;
(* We don't know the size at the outset *)
let build_context_manager () = 
  incr debug_id;
  { tuple_stack   = Stack.create ();
    current_tuple = Array.make 1 empty_physical_xml;
    last_slot     = ref 0;
    id            = !debug_id
  }


let build_tuple_enter_context cm = 
  (fun () ->
     Stack.push (Array.copy cm.current_tuple) cm.tuple_stack;
  )

let build_tuple_exit_context cm = 
  (fun () ->
     try       
       let old_context = Stack.pop cm.tuple_stack in
	 Array.blit old_context 0 cm.current_tuple 0 !(cm.last_slot) 
     with Stack.Empty ->
       raise (Query (Code_Selection ("Tried to exit an empty context")))
  )



let instantiate_tuple_context_manager cm  = 
  cm.current_tuple <- Array.make !(cm.last_slot) empty_physical_xml

let build_tuple_store_code tr = 
  let tcm, tl = tr in
  (fun pv -> tcm.current_tuple.(tl) <- pv)

let build_tuple_retrieve_code tr = 
  let tcm, tl = tr in
    (fun () -> tcm.current_tuple.(tl) )
