(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: monitoring_context.mli,v 1.10 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Monitoring_context
   Description:
     This module contains the context for monitoring functions
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
         applying XQuery rewriting simplifications. *)
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

(*************************)
(* Data model statistics *)
(*************************)

val increment_node_count : unit -> unit
val get_node_count : unit -> int

(***********************)
(* Activity monitoring *)
(***********************)

(* Monitoring output is partitioned between prolog processing and
   statement/query processing and serialization *)

type call_kind = | Prolog | Statement | Serialization 

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
      mutable document_toplevel_parsingloading_phase : (string, stats_point) Hashtbl.t;
      mutable document_parsingloading_phase : (string, stats_point) Hashtbl.t;
      external_phase : stats_point; }

(***********************)
(* Monitor context     *)
(***********************)

type monitor_context 

val  default_monitor_context : unit -> monitor_context

val  set_monitor_mem    : monitor_context -> bool -> unit
val  set_monitor_time   : monitor_context -> bool -> unit
val  get_monitor_mem    : monitor_context -> bool 
val  get_monitor_time   : monitor_context -> bool 

val  set_monitor_output : monitor_context -> Galax_io.output_spec -> unit
val  get_monitor_output : monitor_context -> Galax_io.output_spec

val  set_current_call : monitor_context -> call_stat_summary option -> unit
val  get_current_call : monitor_context -> call_stat_summary option

val  push_resume_call : monitor_context -> call_stat_summary -> unit
val  pop_resume_call  : monitor_context -> call_stat_summary option

val  add_completed_call : monitor_context -> call_stat_summary -> unit
val  get_last_call : monitor_context -> call_stat_summary option
val  get_all_calls : monitor_context -> call_stat_summary list

val  string_of_call_kind : call_kind -> string
val  string_of_phase : phase -> string

