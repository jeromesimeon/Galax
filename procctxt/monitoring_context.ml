(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: monitoring_context.ml,v 1.11 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Monitoring_context
   Description:
     This module contains the context for monitoring functions.
*)

(* Type that represents various processing phases within the system *)

type query_phase =
  | Parsing_Phase
      (* [Query Parsing] This is the phase during which Galax is
         parsing the query. *)
  | Normalization_Phase
      (* [Normalization] This is the phase during which Galax is
         normalizing the query to the core. *)
  | Rewriting_Phase
      (* [Rewriting] This is a phase during which Galax is
         applies XQuery rewriting simplifications. *)
  | Factorization_Phase
      (* [Factorization] This is a phase during which Galax performs
         code factorization and query canonicalization to prepare
         query optimization. *)
  | Compile_Phase
      (* [Compile Phase] This phase takes a Core AST and produces
         a naive "logical" query plan. *)
  | Optimization_Phase
      (* [Optimization Phase] This is a phase during which Galax
         performs algebraic query optimization. *)
  | Selection_Phase
      (* [Selection Phase] This phase takes a "logical" query plan and
         selects evaluation code for each algebra operation in the
         plan. *)
  | Evaluation_Phase
      (* [Evaluation] This is a phase during which Galax is doing
         query evaluation. *)

type phase = 
  | PQuery of query_phase
  | Document_Serialization_Phase
      (* [Serialization] This is a phase during which Galax is
         serializing the result. *)
  | Document_Toplevel_ParsingLoading_Phase of string
  | Document_ParsingLoading_Phase of string
      (* [Document parsing&loading] In the case where SAX is used, it
         is not so easy to separate parsing from loading, which is
         done in a streaming fashion. This is a phase during which
         Galax is both parsing and loading simultaneously. *)
  | External_Phase 
      (* Used to monitor external functions *)

let string_of_phase phase =
  match phase with
  | PQuery Parsing_Phase -> "parsing" 
  | PQuery Normalization_Phase -> "normalization" 
  | PQuery Rewriting_Phase -> "rewriting"
  | PQuery Factorization_Phase -> "factorization"
  | PQuery Compile_Phase -> "compile"
  | PQuery Optimization_Phase -> "optimization"
  | PQuery Selection_Phase -> "selection"
  | PQuery Evaluation_Phase -> "evaluation"
  | Document_Serialization_Phase -> "serialization"
  | Document_Toplevel_ParsingLoading_Phase _ -> "toplevel-load-document"
  | Document_ParsingLoading_Phase _ -> "load-document"
  | External_Phase -> "external"

(*************************)
(* Data model statistics *)
(*************************)

type dm_stats = { mutable node_count : int }
let current_stats =  { node_count = 0 }

let increment_node_count () = current_stats.node_count <- current_stats.node_count + 1 
let get_node_count () = current_stats.node_count 

(***********************)
(* Activity monitoring *)
(***********************)

(* Monitoring output is partitioned between prolog processing and
   statement/query processing *)

type call_kind = | Prolog | Statement | Serialization

let string_of_call_kind ck = 
  match ck with
  | Prolog -> "prolog"
  | Statement -> "statement"
  | Serialization -> "serialization"

(**************)
(* Statistics *)
(**************)

type stats_item =
    { glx_total_memory : float;
      glx_live_memory : float;
      glx_time : float ; 
      glx_node_count : int }

type stats_point =
    { mutable relative_stats : stats_item;
      mutable absolute_stats : stats_item }

type call_stat_summary =
    { mutable call_name : string ; 
      mutable call_kind : call_kind ; 
      mutable current_phase : phase option; 
      query_toplevel_phase : stats_point;
      query_parsing_phase : stats_point;
      query_normalization_phase : stats_point;
      query_rewriting_phase : stats_point;
      query_factorization_phase : stats_point;
      query_compile_phase : stats_point;
      query_optimization_phase : stats_point;
      query_selection_phase : stats_point;
      query_evaluation_phase : stats_point;
      query_serialization_phase : stats_point;
      mutable document_toplevel_parsingloading_phase : (string, stats_point) Hashtbl.t ;
      mutable document_parsingloading_phase : (string, stats_point) Hashtbl.t ;
      external_phase : stats_point; }

(*******************)
(* Monitor context *)
(*******************)

type monitor_context = 
  { (* Monitor info *)

    mutable monitor_mem : bool ; 
    mutable monitor_time : bool ; 
    mutable monitor_output : Galax_io.output_spec ; 
    mutable call_stack : call_stat_summary list ;
    mutable resume_stack : call_stat_summary Stack.t ;
    mutable current_call : call_stat_summary option ;
  }  
      

(*****************************)
(* Monitor context functions *)   
(*****************************)

let default_monitor_context () = 
 {
    monitor_mem = false ; 
    monitor_time = false ; 
    monitor_output = Galax_io.Channel_Output stdout; 
    call_stack = [] ;
    resume_stack = Stack.create(); 
    current_call = None; 
 } 
let set_monitor_mem mc b = mc.monitor_mem <- b 
let get_monitor_mem mc = mc.monitor_mem
let set_monitor_time mc b = mc.monitor_time <- b 
let get_monitor_time mc = mc.monitor_time

let set_monitor_output mc oc = mc.monitor_output <- oc
let get_monitor_output mc = mc.monitor_output 

let push_resume_call mc call = Stack.push call mc.resume_stack
let pop_resume_call mc = 
  try 
    Some(Stack.pop mc.resume_stack)
  with
  | Stack.Empty -> None

let add_completed_call mc call = mc.call_stack <- call :: mc.call_stack 
let set_current_call mc callopt = mc.current_call <- callopt
let get_current_call mc = mc.current_call

let get_last_call mc = 
  try
    Some(List.hd mc.call_stack)
  with
  | Failure _ -> None

let get_all_calls mc = mc.call_stack 


