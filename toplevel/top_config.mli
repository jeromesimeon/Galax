(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: top_config.mli,v 1.35 2007/08/17 18:28:54 simeon Exp $ *)

(* Module: Top_config
   Description:
     This module implements support for handling configuration of
     Galax from the top-level.
 *)


(**********************)
(* Context parameters *)
(**********************)

val context_file : (string option) ref
val context_item : (string option) ref
val global_vars  : ((string * string) list) ref
val global_docs  : ((string * string) list) ref


(**************************)
(* Printing configuration *)
(**************************)

val output_all : bool ref


(*****************************)
(* galax-parse configuration *)
(*****************************)

val validation 	  : bool ref
val dtd	          : bool ref
val diff          : bool ref
val schemafile    : string option ref
val pxp  	  : bool ref
val stream  	  : bool ref
val resolve       : bool ref
val annotate      : bool ref
val erase         : bool ref
val prefix        : bool ref
val load       	  : bool ref
val export     	  : bool ref


(*********************************)
(* Argument Processing functions *)
(*********************************)

val bool_of_onoff             : string -> bool

val sbdo_kind_of_arg   	      : string -> Processing_context.sbdo_kind
val typing_kind_of_arg 	      : string -> Processing_context.typing_kind
val serialization_kind_of_arg : string -> Processing_context.serialization_kind
val projection_kind_of_arg    : string -> Processing_context.projection_kind
val treejoin_log_of_arg       : string -> Processing_context.treejoin_logical_kind
val treejoin_phys_of_arg      : string -> Processing_context.treejoin_physical_kind
val buffer_chunks_of_arg      : string -> int
val buffer_csize_of_arg       : string -> int
val buffer_inc_of_arg         : string -> int


(***************************)
(* Command-line parameters *)
(***************************)


(* Set specific parameters in the processing context *)

val set_debug_kind_arg           : Processing_context.processing_context -> string -> unit
val set_normalization_phase_arg  : Processing_context.processing_context -> string -> unit
val set_normalization_ident_arg  : Processing_context.processing_context -> string -> unit
val set_typing_phase_arg         : Processing_context.processing_context -> string -> unit
val set_rewriting_phase_arg      : Processing_context.processing_context -> string -> unit
val set_factorization_phase_arg  : Processing_context.processing_context -> string -> unit
val set_optimization_phase_arg   : Processing_context.processing_context -> string -> unit
val set_code_selection_phase_arg : Processing_context.processing_context -> string -> unit
val set_evaluation_phase_arg     : Processing_context.processing_context -> string -> unit
val set_execute_kind_arg         : Processing_context.processing_context -> string -> unit

val set_xml_whitespace_arg         : Processing_context.processing_context -> unit -> unit
val set_xml_pis_and_comments_arg   : Processing_context.processing_context -> unit -> unit
val unset_xml_whitespace_arg       : Processing_context.processing_context -> unit -> unit
val unset_xml_pis_and_comments_arg : Processing_context.processing_context -> unit -> unit

val set_inline_functions_arg     : Processing_context.processing_context -> string -> unit
val set_inline_variables_arg     : Processing_context.processing_context -> string -> unit
val set_sbdo_kind_arg            : Processing_context.processing_context -> string -> unit
val set_language_kind_arg        : Processing_context.processing_context -> string -> unit
val set_syntax_kind_arg          : Processing_context.processing_context -> string -> unit
val set_typing_kind_arg          : Processing_context.processing_context -> string -> unit
val set_serialization_kind_arg   : Processing_context.processing_context -> string -> unit
(*
val set_boundary_space_kind_arg  : Processing_context.processing_context -> string -> unit
val set_construction_kind_arg    : Processing_context.processing_context -> string -> unit
val set_ordering_kind_arg        : Processing_context.processing_context -> string -> unit
val set_default_order_kind_arg   : Processing_context.processing_context -> string -> unit
val set_ns_preserve_kind_arg     : Processing_context.processing_context -> string -> unit
val set_ns_inherit_kind_arg      : Processing_context.processing_context -> string -> unit
val set_base_uri_arg             : Processing_context.processing_context -> string -> unit
*)
val set_projection_kind_arg      : Processing_context.processing_context -> string -> unit

val set_treejoin_log_arg         : Processing_context.processing_context -> string -> unit
val set_treejoin_phys_arg        : Processing_context.processing_context -> string -> unit
val set_streaming_arg            : Processing_context.processing_context -> string -> unit
val set_infer_indepedence_arg    : Processing_context.processing_context -> string -> unit
val set_dxq_arg                  : Processing_context.processing_context -> string -> unit
val set_embed_xqueryx_arg        : Processing_context.processing_context -> string -> unit
val set_error_arg                : Processing_context.processing_context -> string -> unit
val set_plan_arg                 : Processing_context.processing_context -> string -> unit



val set_streaming_shebang : Processing_context.processing_context -> string -> unit
val set_scjoin_shebang : Processing_context.processing_context -> string -> unit
val set_twigjoin_shebang : Processing_context.processing_context -> string -> unit
val set_print_plan_shebang : Processing_context.processing_context -> string -> unit

val set_materialize_tables_kind_arg : Processing_context.processing_context -> string -> unit 
