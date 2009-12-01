(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: top_options.mli,v 1.42 2007/08/17 18:28:54 simeon Exp $ *)

(* Module: Top_options
   Description:
     This module contains code and messages for command-line options.
 *)

open Processing_context

(*******************)
(* Version message *)
(*******************)

val print_version : unit -> unit


(***********)
(* Options *)
(***********)

type option_classes =
  | GalaxParse_Options
  | Misc_Options
  | Monitoring_Options
  | Encoding_Options
  | Context_Options
  | DataModel_Options
  | Serialization_Options
  | Behavior_Options
  | ProcessingPhases_Options
  | Printing_Options
  | Optimization_Options
  | CodeSelection_Options
  | Runtime_Options
  | PrintParse_Options
  | Daemon_Options
  | Zerod_Options
  | Testing_Options
  | XQueryX_Options

(* Misc options *)

val opt_version : string
val arg_version : processing_context -> unit -> unit
val msg_version : string

val opt_verbose : string
val arg_verbose : processing_context -> string -> unit
val msg_verbose : string

val opt_debug : string
val arg_debug : processing_context -> string -> unit
val msg_debug : string

val opt_streaming_shebang : string
val arg_streaming_shebang : processing_context -> string -> unit
val msg_streaming_shebang : string

val opt_scjoin_shebang : string
val arg_scjoin_shebang : processing_context -> string -> unit
val msg_scjoin_shebang : string

val opt_twigjoin_shebang : string
val arg_twigjoin_shebang : processing_context -> string -> unit
val msg_twigjoin_shebang : string

val opt_print_plan_shebang : string
val arg_print_plan_shebang : processing_context -> string -> unit
val msg_print_plan_shebang : string


(* Monitoring options *)

val opt_monitor : string
val arg_monitor : processing_context -> string -> unit
val msg_monitor : string

val opt_monitor_mem : string
val arg_monitor_mem : processing_context -> string -> unit
val msg_monitor_mem : string

val opt_monitor_time : string
val arg_monitor_time : processing_context -> string -> unit
val msg_monitor_time : string

val opt_output_monitor : string
val arg_output_monitor : processing_context -> string -> unit
val msg_output_monitor : string

(* Character encoding options *)

val opt_internal_encoding : string
val arg_internal_encoding : processing_context -> string -> unit
val msg_internal_encoding : string

val opt_output_encoding : string
val arg_output_encoding : processing_context -> string -> unit
val msg_output_encoding : string

(* Input context options *)

val opt_var : string
val arg_var : processing_context -> string -> unit
val msg_var : string

val opt_doc : string
val arg_doc : processing_context -> string -> unit
val msg_doc : string

val opt_context_item : string
val arg_context_item : processing_context -> string -> unit
val msg_context_item : string

val opt_context : string
val arg_context : processing_context -> string -> unit
val msg_context : string

(* Data model options *)

val opt_xml_whitespace : string
val arg_xml_whitespace : processing_context -> unit -> unit
val msg_xml_whitespace : string

val opt_xml_pic : string
val arg_xml_pic : processing_context -> unit -> unit
val msg_xml_pic : string

(* Serialization options *)

val opt_serialize : string
val arg_serialize : processing_context -> string -> unit
val msg_serialize : string

(* Behavior options *)

val opt_typing : string
val arg_typing : processing_context -> string -> unit
val msg_typing : string

(* All of these features are set on a per-module basis. 
   Put in the module's prolog, if you really want them.

val opt_boundary_space : string
val arg_boundary_space : processing_context -> string -> unit
val msg_boundary_space : string

val opt_construction : string
val arg_construction : processing_context -> string -> unit
val msg_construction : string

val opt_ordering : string
val arg_ordering : processing_context -> string -> unit
val msg_ordering : string

val opt_default_order : string
val arg_default_order : processing_context -> string -> unit
val msg_default_order : string

val opt_ns_preserve : string
val arg_ns_preserve : processing_context -> string -> unit
val msg_ns_preserve : string

val opt_ns_inherit : string
val arg_ns_inherit : processing_context -> string -> unit
val msg_ns_inherit : string

val opt_base_uri : string
val arg_base_uri : processing_context -> string -> unit
val msg_base_uri : string

*)

val opt_language : string
val arg_language : processing_context -> string -> unit 
val msg_language : string

val opt_syntax : string
val arg_syntax : processing_context -> string -> unit 
val msg_syntax : string

(* Processing phases options *)

val opt_normalize : string
val arg_normalize : processing_context -> string -> unit
val msg_normalize : string

val opt_static_typing : string
val arg_static_typing : processing_context -> string -> unit
val msg_static_typing : string

val opt_rewriting : string
val arg_rewriting : processing_context -> string -> unit
val msg_rewriting : string

val opt_factorization : string
val arg_factorization : processing_context -> string -> unit
val msg_factorization : string

val opt_optimization : string
val arg_optimization : processing_context -> string -> unit
val msg_optimization : string

val opt_code_selection : string
val arg_code_selection : processing_context -> string -> unit
val msg_code_selection : string

val opt_dynamic : string
val arg_dynamic : processing_context -> string -> unit
val msg_dynamic : string

val opt_execute : string
val arg_execute : processing_context -> string -> unit
val msg_execute : string


(* Printing options *)

val opt_print_error : string
val arg_print_error : processing_context -> string -> unit
val msg_print_error : string

val opt_print_warnings : string
val arg_print_warnings : processing_context -> string -> unit
val msg_print_warnings : string

val opt_print_plan : string
val arg_print_plan : processing_context -> string -> unit
val msg_print_plan : string

val opt_output_err : string
val arg_output_err : processing_context -> string -> unit
val msg_output_err : string

val opt_print_xml : string
val arg_print_xml : processing_context -> string -> unit
val msg_print_xml : string

val opt_output_xml : string
val arg_output_xml : processing_context -> string -> unit
val msg_output_xml : string

val opt_print_type : string
val arg_print_type : processing_context -> string -> unit
val msg_print_type : string

val opt_output_type : string
val arg_output_type : processing_context -> string -> unit
val msg_output_type : string

val opt_print_expr : string
val arg_print_expr : processing_context -> string -> unit
val msg_print_expr : string

val opt_output_expr : string
val arg_output_expr : processing_context -> string -> unit
val msg_output_expr : string

val opt_print_norm_expr : string
val arg_print_norm_expr : processing_context -> string -> unit
val msg_print_norm_expr : string

val opt_output_norm_expr : string
val arg_output_norm_expr : processing_context -> string -> unit
val msg_output_norm_expr : string

val opt_print_rewr_expr : string
val arg_print_rewr_expr : processing_context -> string -> unit
val msg_print_rewr_expr : string

val opt_output_rewr_expr : string
val arg_output_rewr_expr : processing_context -> string -> unit
val msg_output_rewr_expr : string

val opt_print_fact_expr : string
val arg_print_fact_expr : processing_context -> string -> unit
val msg_print_fact_expr : string

val opt_output_fact_expr : string
val arg_output_fact_expr : processing_context -> string -> unit
val msg_output_fact_expr : string

val opt_print_global : string
val arg_print_global : processing_context -> string -> unit
val msg_print_global : string

val opt_print_annotations : string
val arg_print_annotations : processing_context -> string -> unit
val msg_print_annotations : string

val opt_print_prolog : string
val arg_print_prolog : processing_context -> string -> unit
val msg_print_prolog : string

val opt_output_all : string
val arg_output_all : processing_context -> string -> unit
val msg_output_all : string

val opt_print_log_plan : string
val arg_print_log_plan : processing_context -> string -> unit
val msg_print_log_plan : string

val opt_output_log_plan : string
val arg_output_log_plan : processing_context -> string -> unit
val msg_output_log_plan : string

val opt_print_log_rewrites : string
val arg_print_log_rewrites : processing_context -> string -> unit
val msg_print_log_rewrites : string

val opt_output_log_rewrites : string
val arg_output_log_rewrites : processing_context -> string -> unit
val msg_output_log_rewrites : string


val opt_print_opt_plan : string
val arg_print_opt_plan : processing_context -> string -> unit
val msg_print_opt_plan : string

val opt_output_opt_plan : string
val arg_output_opt_plan : processing_context -> string -> unit
val msg_output_opt_plan : string

val opt_print_phys_plan : string
val arg_print_phys_plan : processing_context -> string -> unit
val msg_print_phys_plan : string

val opt_output_phys_plan : string
val arg_output_phys_plan : processing_context -> string -> unit
val msg_output_phys_plan : string

val opt_print_comp_annot : string
val arg_print_comp_annot : processing_context -> string -> unit
val msg_print_comp_annot : string

val opt_print_materialize : string
val arg_print_materialize : processing_context -> string -> unit
val msg_print_materialize : string

val opt_output_dfgraph : string
val arg_output_dfgraph : processing_context -> string -> unit
val msg_output_dfgraph : string


(* Testing options *)

val opt_genresults : string
val arg_genresults : processing_context -> unit -> unit
val msg_genresults : string

(* Optimization options *)

val opt_aggressive : string
val arg_aggressive : processing_context -> string -> unit
val msg_aggressive : string

val opt_projection : string
val arg_projection : processing_context -> string -> unit
val msg_projection : string

val opt_treejoin_log : string
val arg_treejoin_log : processing_context -> string -> unit
val msg_treejoin_log : string

val opt_treejoin_phys : string
val arg_treejoin_phys : processing_context -> string -> unit
val msg_treejoin_phys : string

val opt_streaming : string
val arg_streaming : processing_context -> string -> unit
val msg_streaming : string

val opt_infer_independence : string
val arg_infer_indepedence : processing_context -> string -> unit
val msg_infer_indepedence : string

val opt_dxq : string
val arg_dxq : processing_context -> string -> unit
val msg_dxq : string

val opt_embed_xqueryx : string
val arg_embed_xqueryx : processing_context -> string -> unit
val msg_embed_xqueryx : string

val opt_inline : string
val arg_inline : processing_context -> string -> unit
val msg_inline : string

val opt_inline_var : string
val arg_inline_var : processing_context -> string -> unit
val msg_inline_var : string

val opt_buffer_chunks : string
val arg_buffer_chunks : processing_context -> string -> unit
val msg_buffer_chunks : string

val opt_buffer_csize : string
val arg_buffer_csize : processing_context -> string -> unit
val msg_buffer_csize : string

val opt_buffer_inc : string
val arg_buffer_inc : processing_context -> string -> unit
val msg_buffer_inc : string

val opt_nested_loop_join : string
val arg_nested_loop_join : processing_context -> string -> unit
val msg_nested_loop_join : string

val opt_sbdo : string
val arg_sbdo : processing_context -> string -> unit
val msg_sbdo : string

val opt_force_materialized : string
val arg_force_materialized : processing_context -> string -> unit
val msg_force_materialized : string

val opt_allow_streamed_tuple_fields : string
val arg_allow_streamed_tuple_fields : processing_context -> string -> unit
val msg_allow_streamed_tuple_fields : string

val opt_descendant_hack : string
val arg_descendant_hack : processing_context -> string -> unit
val msg_descendant_hack : string

val opt_materialize_tables : string
val arg_materialize_tables : processing_context -> string -> unit 
val msg_materialize_tables : string


(******************************)
(* Titles for option clusters *)
(******************************)

val title_main : string

val title_misc_options               : string
val title_monitor_options            : string
val title_character_encoding_options : string
val title_input_context_options      : string
val title_data_model_options         : string
val title_serialization_options      : string
val title_behavior_options           : string
val title_processing_phases_options  : string
val title_printing_options           : string
val title_prototype_options          : string
val title_optimization_options       : string
val title_code_selection_options     : string
val title_runtime_options            : string

val title_galax_parse_options        : string

(*****************)
(* Usage message *)
(*****************)

val usage_galax_run     : unit -> string
val usage_galax_schema  : unit -> string
val usage_galax_daemon  : unit -> string
val usage_galax_parse   : unit -> string
val usage_galax_compile : unit -> string
val usage_zerod         : unit -> string 

(* Build option operations *)

val make_options : processing_context -> string -> option_classes list -> string list
val make_options_argv :
    processing_context -> string -> option_classes list -> string array -> string list

