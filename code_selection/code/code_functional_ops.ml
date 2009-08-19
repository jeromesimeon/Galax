(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_functional_ops.ml,v 1.19 2007/08/27 18:49:03 simeon Exp $ *)

(* Module: Code_functional_ops
   Description:
     This module contains code building for operators that implement
     "functional" code (if..then..else, let bindings, etc.).
*)

open Code_selection_context

open Cs_util_coercion
open Cs_util

open Xquery_common_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_physical_algebra_ast
open Xquery_physical_type_ast

open Error


(* AOIf *)
let build_default_if_code code_ctxt =
  (fun (d1, d2) eval alg_ctxt pv  ->
     if Physical_util.get_boolean pv then
       eval alg_ctxt d1
     else eval alg_ctxt d2)

let build_if_code code_ctxt algop = 
  let fn = build_default_if_code code_ctxt in
  let dep = access_twosub algop.pdep_sub_expression in 
  (coerce_twodep fn dep coerce_unary_item_cursor_to_physical_value), code_ctxt

(* AOWhile *)

let build_default_while_code code_ctxt =
  (fun (d1, d2) eval alg_ctxt () ->
    let result = ref [] in
    while Physical_util.get_boolean (Physical_value_util.item_cursor_of_physical_value (eval alg_ctxt d1)) do
      let next = Physical_value_util.item_list_of_physical_value (eval alg_ctxt d2) in
      result := !result @ next
    done;
    !result)

let build_while_code code_ctxt algop = 
  let fn = build_default_while_code code_ctxt in
  let dep = access_twosub algop.pdep_sub_expression in 
  (coerce_twodep fn dep coerce_unit_to_item_list), code_ctxt

(* AOLet *)
let build_default_let_code bind_fun =
  (fun e2 eval alg_ctxt input ->	   
     bind_fun input;
     eval alg_ctxt e2
  )

let build_default_let_sax_value_code code_ctxt v =
  build_default_let_code (Code_binding.build_bind_sax_value_to_variable_code code_ctxt v)

let build_default_let_sax_discard_code code_ctxt v =
  build_default_let_code (fun s -> Streaming_ops.discard_typed_xml_stream s)

let build_default_let_item_cursor_code code_ctxt v =
  build_default_let_code (Code_binding.build_bind_item_cursor_to_variable_code code_ctxt v)

let build_default_let_type_checked_item_cursor_code code_ctxt dt v =
  build_default_let_code (Code_binding.build_bind_type_checked_item_cursor_to_variable_code code_ctxt dt v)

let build_default_let_item_list_code code_ctxt v =
  build_default_let_code (Code_binding.build_bind_item_list_to_variable_code code_ctxt v)

let build_default_let_type_checked_item_list_code code_ctxt dt v =
  build_default_let_code (Code_binding.build_bind_type_checked_item_list_to_variable_code code_ctxt dt v)

let build_let_code code_ctxt algop (odt, vn) = 
  let dep = access_onesub algop.pdep_sub_expression in 
  let csc = add_variable_to_current_context code_ctxt vn in
  let physop = Cs_util.get_physical_opname algop in 
  match physop with
  | POLetvar (_, PT_Dom PT_CursorSeq) ->
      begin
	match odt with
	| Some dt ->
	    let fn  = build_default_let_type_checked_item_cursor_code csc dt vn in 
	    (coerce_onedep fn dep coerce_unary_item_cursor_to_physical_value), csc
	| _ ->
	    let fn  = build_default_let_item_cursor_code csc vn in 
	    (coerce_onedep fn dep coerce_unary_item_cursor_to_physical_value), csc
      end
  | POLetvar (_, PT_Dom PT_ListSeq) ->
      begin
	match odt with
	| Some dt ->
	    let fn  = build_default_let_type_checked_item_list_code csc dt vn in
	    (coerce_onedep fn dep coerce_unary_item_cursor_to_physical_value), csc
	| _ ->
	    let fn  = build_default_let_item_list_code csc vn in
	    (coerce_onedep fn dep coerce_unary_item_list_to_physical_value), csc
      end
  | POLetvar (_, PT_Sax PT_Stream) ->
      let fn  = build_default_let_sax_value_code csc vn in 
      (coerce_onedep fn dep coerce_unary_sax_to_physical_value), csc
  | POLetvar (_, PT_Sax PT_Discarded) ->
      let fn  = build_default_let_sax_discard_code csc vn in 
      (coerce_onedep fn dep coerce_unary_sax_to_physical_value), csc
  | _ -> raise(Query(Code_Selection("Invalid physical operator in build_let_code")))

(* CEVar *)
let build_default_cevar_code code_ctxt vn (* defn *) =
  let retrieve_code = build_var_xml_value_retrieve code_ctxt vn in
  (* If we want to evaluate global variables lazily, we need
     to cache the plan associated with the global variable
     (algop) so that it can be executed on demand. Most
     likely, that plan should go here. -Mary & Kristi
  *)
  (fun alg_ctxt () -> 
    (* If this is a reference to a global variable, just return the
       plan for computing that variable else ... *)
    retrieve_code())
  
let build_var_code code_ctxt algop vn = 
  (*
     Assume that we can recover the plan that corresponds to the definition of this variable:
     call that defn
  *)
  let fn = build_default_cevar_code code_ctxt vn (* defn *) in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_unit_to_xml), code_ctxt

(* CESet *)
let build_default_set_code code_ctxt vn =
  (fun alg_ctxt inp ->
    let f' = fun () -> Physical_xml_value.xml_value_of_item_list inp in
      (build_current_assign_code code_ctxt vn f') ()
  )
      
let build_set_code code_ctxt algop vn = 
  let _ = access_onesub algop.psub_expression in 
  let fn = build_default_set_code code_ctxt vn in
    (coerce_nodep fn coerce_unary_item_list_to_xml), code_ctxt

