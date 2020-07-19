(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: variable_context_manager.ml,v 1.13 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Update_ordering 
     This module deals with variable context during runtime.  It also
     holds the build functions to access and store variables
*)

open Error
open Xquery_common_ast

open Namespace_names
open Physical_xml_value
open Physical_value
open Physical_value_util
open Array

(********************************)
(*** Variable Context Manager ***)
(********************************)


let empty_xml_value () = xml_value_of_item_list []
(* Consider using polymorphic types to close off after instantiated objects *)

type variable_context_manager = {
  variable_stack            : xml_value array Stack.t;
  mutable current_variables : xml_value array ;
  (* If we want to evaluate global variables lazily, we need
     to cache the plan associated with the global variable
     (algop) so that it can be executed on demand. Most
     likely, that plan should go here. -Mary & Kristi
   *)
  last_slot                 : int ref; 
  (* Debugging parameters *)
  id                        : int;
  
}
type variable_ref = variable_context_manager * int

let string_of_variable_ref v = 
  let cm, vr = v in
    (string_of_int cm.id) ^ ":" ^ (string_of_int vr)

let get_new_variable_slot cm = 
  let ls = !(cm.last_slot) in
    incr (cm.last_slot);
    cm,ls

let debug_id = ref 0;;
(* We don't know the size at the outset *)
let build_context_manager () = 
  incr debug_id;
  { variable_stack = Stack.create ();
    current_variables = Array.make 1 (empty_xml_value());
    last_slot = ref 0;
    id = !debug_id }

let instantiate_variable_context_manager cm = 
  Debug.print_dxq_debug ("Instantiating  context " ^ (string_of_int cm.id) ^ " with " ^ (string_of_int !(cm.last_slot)));  
  cm.current_variables <- Array.make !(cm.last_slot) (empty_xml_value())

let build_variable_enter_context cm = 
  (fun () ->
     Stack.push (Array.copy cm.current_variables) cm.variable_stack;
  )

let build_variable_exit_context cm = 
  (fun () ->
     try       
       let old_context = Stack.pop cm.variable_stack in
	 Array.blit old_context 0 cm.current_variables 0 !(cm.last_slot) 
     with Stack.Empty ->
       raise (Query (Code_Selection ("Tried to exit an empty context")))
     | Invalid_argument msg ->
	 raise (Query (Code_Selection ("Array.blit fails in build_variable_exit_context")))
  )


(* Insert and retrieve and code *)
let build_parameter_insert_code cm vr =
  (fun pv -> 
    try 
      cm.current_variables.(vr) <- pv
    with _ ->
      raise (Query(Code_Selection("Invalid variable offset "^(string_of_int vr)^" in build_parameter_list"))))

let build_variable_store_code (var_ref:variable_ref) = 
  let cm, vr = var_ref in
  (fun pv -> 
    try
      cm.current_variables.(vr) <- pv
    with _ ->
      raise (Query(Code_Selection("Invalid variable offset "^(string_of_int vr)^" in build_variable_store_code"))))


let build_variable_retrieve_code (var_ref:variable_ref) =
  let cm, vr = var_ref in
  (fun () -> 
    try
      cm.current_variables.(vr) 
    with _ ->
      raise (Query(Code_Selection("Invalid variable offset "^(string_of_int vr)^" in build_variable_retrieve_code"))))

let build_variable_assign_code (var_ref:variable_ref) rhs =
  let cm, vr = var_ref in
  (fun () -> 
    let pval = rhs() in
      try
        cm.current_variables.(vr) <- pval;
        empty_xml_value () 
      with _ ->
        raise (Query(Code_Selection("Invalid variable offset "^(string_of_int vr)^" in build_variable_retrieve_code"))) )
