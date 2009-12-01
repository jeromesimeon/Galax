(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: top_util.ml,v 1.24 2008/02/01 02:45:26 mff Exp $ *)

(* Module: Top_util
   Description:
     This module implements some support functions for Galax
     command-line tools.
 *)

open Format

open Conf
open Print_top
open Error

open Xquery_common_ast
open Xquery_ast
open Xquery_core_ast

open Processing_context

open Top_config


(****************)
(* I/O handling *)
(****************)

let init_output_refs f f_out f_formatter = 
  f_out := open_out(f); 
  f_formatter := Format.formatter_of_out_channel (!f_out)

let close_channel_ref chan =
  if (!chan != stdout) then
    begin
      close_out (!chan);
      chan := stdout;
    end


(***********************)
(* Top-level execution *)
(***********************)

(* A catch-all exception handler *)

(* Both these functions evaluate "exit 1" on exceptions *)
let exec func proc_ctxt arg =
  try
    begin
      try
	func proc_ctxt arg;
      with
      | Fn_error.Xquery_error msg -> Fn_error.downgrade_error proc_ctxt msg
    end; 
    Monitor.serialize_monitor proc_ctxt;
  with
  | e ->
      begin
	Monitor.serialize_monitor proc_ctxt;
	eprintf_error "  " e;
	fprintf (!Conf.glx_err_formatter) "@.";
	exit 1;
      end

(* Both these functions evaluate "exit 1" on exceptions *)
let low_exec func () =
  try
    func ()
  with
  | e ->
      begin
	eprintf_error "  " e;
	fprintf (!Conf.glx_err_formatter) "@.";
	exit 1;
      end

(************)
(* Printing *)
(************)

let separator =
  ref "\n\n------------------------------------------------------------------------------\n\n"

let print_processing_file file =
  if (!Conf.print_global && !Conf.verbose) then
    begin
      printf "---------------------------------------\n";
      printf "Processing file: %s\n" file;
      printf "---------------------------------------\n";
      printf "%s" !separator;
      flush stdout;
    end


(******************)
(* Initialization *)
(******************)

let galax_run_proc_ctxt () =
  let proc_ctxt = default_processing_context () in
  begin
    set_serialization_kind proc_ctxt Serialize_As_XQuery;
    Conf.print_xml := true;
    proc_ctxt
  end

let galax_compile_proc_ctxt () =
  let proc_ctxt = default_processing_context () in
  begin
    set_evaluation_phase_arg proc_ctxt "off";
    Conf.print_optimized_algebra := true;
    proc_ctxt
  end

let init_all proc_ctxt =
  begin
    (* Check the given user's configuration *)
    Processing_context.check_config proc_ctxt;
    Galax.load_standard_library proc_ctxt 
  end


(*******************************)
(* Set up the external context *)
(*******************************)

(* Set up external context *)
(* Load documents bound to global variables *)

let set_up_external_context proc_ctxt =
  let (ext_ctxt_item,opt_ctxt_item) =
    match !context_item with
    | None -> (false,None)
    | Some f ->
	let actual_item =
	  List.hd (Galax.load_document proc_ctxt (Galax_io.File_Input f))
	in (true,Some (Physical_value.Item_Node actual_item))
  in
  let var_vals =
    List.map
      (fun (v, f) -> (v, (Physical_value.Item_Atomic (new Dm_atomic.atomicUntyped(f))) :: []))
      !global_vars
  in
  let var_docs =
    List.map
      (fun (v, f) -> (v, List.map (fun n -> (Physical_value.Item_Node n)) (Galax.load_document proc_ctxt (Galax_io.File_Input f)))) 
      !global_docs
  in
  (* Build external context of global variables and values *)
  (* Note:
       What about the types of external variables?  Where are they
       defined?
   - Jerome *)
  (ext_ctxt_item,
   Galax.build_external_context
     proc_ctxt
     opt_ctxt_item
     (Some (new Dm_atomic.atomicDayTimeDuration(DateTime.local_timezone())))
     (var_vals @ var_docs))


(***********************)
(* Compilation helpers *)
(***********************)

let compile_main_module_helper opt_ext_ctxt mod_ctxt input =
  if !(Conf.execute_logical_plan)
  then
    Galax.compile_serialized_logical_main_module mod_ctxt input
  else if !(Conf.execute_optimized_plan)
  then
    Galax.compile_serialized_optimized_main_module mod_ctxt input
  else
    Galax.import_main_module opt_ext_ctxt mod_ctxt input

(********************************************************************************)
(* Module to evaluate internal queries over materialized (in-memory) data models *)
(********************************************************************************)

module InternalQuery = struct

  open Datatypes
  open Dm_atomic
  open Dm
  open Physical_value
  open Physical_item
  open Processing_context

(* We don't statically type check internal queries . *)
  let default_proc_ctxt() =
    begin
      let pc = default_processing_context() in
      pc.serialization_kind <- Serialize_As_Standard;
      pc.typing_kind <- Typing_None; 
      pc
    end

  let dpc = default_proc_ctxt()
  let defaultcp () = Galax.load_standard_library (dpc) 

  let load_prolog config_file = 
    try
      Galax.import_prolog_only (defaultcp()) true config_file
    with
    | e ->
      begin
	eprintf_error "" e;
	Format.fprintf (!Conf.glx_err_formatter) "@.";
	exit 1;
      end
  let ccp = load_prolog (Galax_io.String_Input "")

  (*
     All this boilerplate should be consolidated and abstracted! -Mary
  *)
  let get_string (il : item list) : xs_string =
    try
      let x = List.nth il 0 in
      (getAtomicValue x)#getAtomicString()
    with
    | e -> raise(Query(Internal_Error("In InternalQuery.get_string"^(bprintf_error "  " e)^"\n")))

  let get_string_list (il : item list) : xs_string list =
    try
      List.map (fun x -> (getAtomicValue x)#getAtomicString()) il
    with
    | e -> raise(Query(Internal_Error("In InternalQuery.get_string_list"^(bprintf_error "  " e)^"\n")))
	
  let get_int (il : item list) : int =
    try
      let x = List.nth il 0 in
      Decimal._int_of_integer ((getAtomicValue x)#getAtomicInteger())
    with
    | e -> raise(Query(Internal_Error("In InternalQuery.get_int"^(bprintf_error "  " e)^"\n")))
 
  let get_float (il : item list) : float =
    try
      let x = List.nth il 0 in
      (getAtomicValue x)#getAtomicFloat()
    with
    | e -> raise(Query(Internal_Error("In InternalQuery.get_float"^(bprintf_error "  " e)^"\n")))

  let get_float_list (il : item list) : xs_float list =
    try
      List.map (fun x -> (getAtomicValue x)#getAtomicFloat()) il
    with
    | e -> raise(Query(Internal_Error("In InternalQuery.get_float_list"^(bprintf_error "  " e)^"\n")))
	
  let get_item (il : item list) : item =
    try
      List.nth il 0
    with
    | e -> raise(Query(Internal_Error("In InternalQuery.get_item"^(bprintf_error "  " e)^"\n")))

  let get_item_list (il : item list) : item list = il

 let make_from_string (s : string) : item list =
   [(Item_Atomic (new atomicString s))]
 
 let make_from_int (i : int) : item list =
   [(Item_Atomic (new atomicInteger (Decimal._integer_of_int i)))]
 

  let load_document_item gio = 
    Item_Node (List.nth (Galax.load_document dpc gio) 0) 

  let eval_to_string eval query dot =
    try
      get_string (eval query dot)
    with 
    | e -> raise(Query(Internal_Error("In eval_to_string: Query '"^query^"' failed with :"^(bprintf_error "  " e)^"\n")))

  let eval_to_string_list eval query dot =
    get_string_list (eval query dot)
 
  let eval_to_int eval query dot =
    try
      get_int (eval query dot)
    with 
    | e -> raise(Query(Internal_Error("In eval_to_int: Query '"^query^"' failed with :"^(bprintf_error "  " e)^"\n")))

  let eval_to_float eval query dot =
    get_float (eval query dot)

  let eval_to_float_list eval query dot =
    get_float_list (eval query dot)
 
  let eval_to_item eval query dot =
    get_item (eval query dot)

  let eval_to_item_list eval query dot =
    get_item_list (eval query dot)

  let eval_expr query dot = 
    let ec = Galax.build_external_context (default_processing_context()) (Some dot) None [] in
    let pp = Galax.prepare_program ccp (Some ec) in
    Galax.eval_statement pp (Galax_io.String_Input query)

end

module Graph =  struct
  open InternalQuery
  type graph_edge = (string * string * string list * float list)

  (* Load a GUI graph *)
  let load_graph input_doc graph_name =
    try
      let rootnode = eval_to_item eval_expr "./config" input_doc in
      let graph_roots = eval_to_item_list eval_expr ("./graph[@name='"^graph_name^"']") rootnode in
      match graph_roots with 
      |	[] -> raise (Query(Internal_Error("In Graph.load_graph: No graph with @name='"^graph_name^"'")))
      |	[graph_root] -> 
	  let edges = eval_to_item_list eval_expr "./edge" graph_root in
	  List.map (fun l ->
	    let src = eval_to_string eval_expr "./@s/string()" l in
	    let tgt = eval_to_string eval_expr "./@t/string()" l in
	    let lbls = eval_to_string_list eval_expr "./@l/string()" l in
	    let wts = eval_to_float_list eval_expr "./xs:float(@w)" l in
	    (src, tgt, lbls, wts)
	      ) edges
      |	_ -> raise (Query(Internal_Error("In Graph.load_graph: Multiple graphs with @name='"^graph_name^"'")))
    with
    |	exn -> raise (Query(Internal_Error("In Graph.load_graph "^(Error.bprintf_error "" exn))))
end

