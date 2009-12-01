(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: processing_context.mli,v 1.51 2007/10/16 01:25:35 mff Exp $ *)

(* Module: Processing_context
   Description:

   The processing context contains the state that is shared across
   multiple Galax API calls and between the processing-model phases
   of the Galax engine.

   The processing context contains : 

   1. basic configuration parameters for Galax, notably set up of
   the processing phases.
   2. monitoring data, which includes the call stack of all Galax
   API calls
   3. cached documents, i.e., all documents loaded by Galax API
   calls
*)

open Monitoring_context

(***********)
(* Options *)
(***********)

type sbdo_kind =
  | SBDO_Remove
  | SBDO_Preserve
  | SBDO_AdHoc
  | SBDO_Tidy
  | SBDO_DupTidy
  | SBDO_Sloppy

type typing_kind =
  | Typing_None
  | Typing_Weak
  | Typing_Strong

type serialization_kind =
  | Serialize_As_Standard
  | Serialize_As_Well_Formed
  | Serialize_As_XQuery
  | Serialize_As_Canonical

type projection_kind =
  | Projection_None
  | Projection_Standard
  | Projection_Optimized

type treejoin_logical_kind =
  | Default       (* no rewritings for bulk *)
  | TreeJoin      (* apply bulk TreeJoin rewrites 
                      NOTES 
		        - these include TupleTreeJoin rewrites 
			- Use this for streaming *)
  | Twig
  
type treejoin_physical_kind =
  | NestedLoop
  | SCJoin
  | IndexSortJoin
  | TwigJoin
  | Streaming

type dxq_execution_kind = 
  | LocalSimulation of string
  | RemoteExecution of string

(******************************************************)
(* MODULE processing context : unique to each module  *)
(******************************************************)

type module_processing_context =
    { 
      mutable boundary_space_kind : Xquery_common_ast.strip_or_preserve;
      mutable construction_kind   : Xquery_common_ast.strip_or_preserve;
      mutable ordering_kind       : Xquery_common_ast.ordered_or_unordered;
      mutable default_order_kind  : Xquery_common_ast.emptysortkind;
      mutable ns_preserve_kind    : Xquery_common_ast.preserve_or_no_preserve;
      mutable ns_inherit_kind     : Xquery_common_ast.inherit_or_no_inherit;

      (* Base URI *)
      mutable base_uri           : AnyURI._uri option;

      (* Default collation *)
      mutable default_collation  : string;

      mutable system             : bool;
      mutable name_generators     : Namespace_generate.name_gen ref list ;
    }
(* Default module processing context *)
val default_module_processing_context : unit -> module_processing_context

val set_boundary_space_kind  : module_processing_context -> Xquery_common_ast.strip_or_preserve -> unit
val set_construction_kind    : module_processing_context -> Xquery_common_ast.strip_or_preserve -> unit
val set_ordering_kind        : module_processing_context -> Xquery_common_ast.ordered_or_unordered -> unit
val set_default_order_kind   : module_processing_context -> Xquery_common_ast.emptysortkind -> unit
val set_ns_preserve_kind     : module_processing_context -> Xquery_common_ast.preserve_or_no_preserve -> unit
val set_ns_inherit_kind      : module_processing_context -> Xquery_common_ast.inherit_or_no_inherit -> unit

(* Returns the base URI *)
val get_base_uri             : module_processing_context -> AnyURI._uri option
val get_default_collation    : module_processing_context -> string (* AnyURI._uri *)
val set_base_uri             : module_processing_context -> AnyURI._uri option -> unit
val set_default_collation    : module_processing_context -> string -> unit

(*****************************)
(* Name generator operations *)
(*****************************)

val get_name_generator : module_processing_context -> Namespace_names.prefix -> Namespace_names.uri -> string -> Namespace_generate.name_gen ref


(**************************)
(* DXQ-related signatures *)
(**************************)

(* Moved there due to circular module depdendencies. - JS *)

                       (* Virtual host name, physical host name, physical port *)
type server_location = (string * string * int)

type xquery_kind =
    XQueryString
  | XQueryPlan
  | XQueryPlanAsync

type evaluate_remote_query_sig = 
    (bool * server_location * xquery_kind * string * string -> string option )

type evaluate_closure_sig = 
    string -> string -> (Xquery_physical_type_ast.physical_type * Physical_value.physical_value)

(* Executed once, in the main process *)
type async_eval_sig = bool -> (exn -> unit) -> (unit -> unit) -> unit
type async_eval_ext_sig = (unit -> unit) -> unit

type interpret_hostport_sig = string -> server_location

(*****************************************************************)
(* PROGRAM processing context : shared by all program components *)
(*****************************************************************)
type processing_context =
{ 
  (* Processing module options *)
      mutable normalization  : bool; (* true when normalization is on [default: on] *)
      mutable normalization_ident : bool; (* true when normalization is identity [default: off] *)
      mutable typing         : bool; (* true when static typing is on [default: off] *)
      mutable rewriting      : bool; (* true when rewriting of the normalized core on [default: on] *)
      mutable factorization  : bool; (* true when factorization is on [default: on] *)
      mutable optimization   : bool; (* true when algebraic optimization is on [default: on] *)
      mutable code_selection : bool; (* true when code selection is on [default: on] *)
      mutable evaluation     : bool; (* true when evaluation is on [default: on] *)

 (* Data model options *)
      mutable xml_whitespace       : bool;
      mutable xml_pis_and_comments : bool;

 (* Normalization options *)
      (* Location hints *)
      mutable schema_location_hints : (string, string) Hashtbl.t ;
      mutable module_location_hints : (string, string) Hashtbl.t ;
      mutable interface_location_hints : (string, string) Hashtbl.t ;
      mutable merge_module_locations : bool; 

 (* Compilation options/variables *)
      mutable typing_kind 	  : typing_kind;       (* Static analysis kind *)

 (* Optimization options *)
      mutable sbdo_kind        	 : sbdo_kind;         (* SBDO optim kind *)
      mutable inline_functions 	 : bool;              (* Function inline *)
      mutable inline_variables 	 : bool;              (* Variable inline *)
      mutable projection_kind    : projection_kind;   (* Document projection *) 
      mutable treejoin_phys      : treejoin_physical_kind;
      mutable treejoin_log       : treejoin_logical_kind;
      mutable streaming          : bool;
      mutable infer_independence : bool; (* path analysis for algebraic optimizations *)


 (* DXQ configuration options *)
      mutable dxq_server : 
      (evaluate_closure_sig *
	 evaluate_remote_query_sig * 
	 async_eval_ext_sig * 
	 interpret_hostport_sig) option; 
      mutable dxq_optimization   : bool; 
      mutable dxq_host  : string option;  
      mutable dxq_port  : int option; 
      mutable dxq_source : dxq_execution_kind option; 
      mutable dxq_topology : string option;  
      mutable dxq_drop_msgs : bool;  

      mutable zerod_host  : string option;  
      mutable zerod_port  : int option; 


 (* Execution options *)
      mutable serialization_kind  : serialization_kind;(* Serialization kind *)
      monitor_context            : monitor_context;   (* Function monitoring context *)

      (* External namespace bindings *)
      mutable external_nsenv     : Namespace_context.nsenv;
}

(* Returns the default processing context *)
val default_processing_context : unit -> processing_context

(* Returns the external namespace environment *)

val get_external_nsenv       : processing_context -> Namespace_context.nsenv

(* Set specific parameters in the processing context *)

val set_normalization_phase  : processing_context -> bool -> unit
val set_normalization_ident  : processing_context -> bool -> unit
val set_typing_phase         : processing_context -> bool -> unit
val set_rewriting_phase      : processing_context -> bool -> unit
val set_factorization_phase  : processing_context -> bool -> unit
val set_optimization_phase   : processing_context -> bool -> unit
val set_code_selection_phase : processing_context -> bool -> unit
val set_evaluation_phase     : processing_context -> bool -> unit

val set_xml_whitespace       : processing_context -> bool -> unit
val set_xml_pis_and_comments : processing_context -> bool -> unit

val set_inline_functions     : processing_context -> bool -> unit
val set_inline_variables     : processing_context -> bool -> unit
val set_sbdo_kind            : processing_context -> sbdo_kind -> unit
val set_typing_kind          : processing_context -> typing_kind -> unit
val is_weak_typing           : processing_context -> bool

val set_serialization_kind   : processing_context -> serialization_kind -> unit
val set_projection_kind      : processing_context -> projection_kind -> unit

val set_treejoin_log         : processing_context -> treejoin_logical_kind  -> unit
val set_treejoin_phys        : processing_context -> treejoin_physical_kind -> unit
val set_streaming            : processing_context -> bool -> unit
val set_infer_independence   : processing_context -> bool -> unit

(* DXQ options *)
val set_dxq_server           : processing_context -> (evaluate_closure_sig *
							evaluate_remote_query_sig * 
							async_eval_ext_sig * 
							interpret_hostport_sig) -> unit
val set_dxq                  : processing_context -> bool -> unit
val set_dxq_host_port        : processing_context -> string option -> int option -> unit
val set_dxq_source           : processing_context -> dxq_execution_kind -> unit
val set_dxq_topology         : processing_context -> string -> unit
val set_dxq_drop_msgs        : processing_context -> bool -> unit

val set_zerod_host_port        : processing_context -> string option -> int option -> unit

val set_namespace_env        : processing_context -> Namespace_context.nsenv -> unit

(******************)
(* Location hints *)
(******************)
val add_schema_location_hints       : processing_context -> (string * string) list -> unit
val add_module_location_hints       : processing_context -> (string * string) list -> unit
val add_interface_location_hints    : processing_context -> (string * string) list -> unit
val get_schema_location_hints       : processing_context -> (string, string) Hashtbl.t
val get_module_location_hints       : processing_context -> (string, string) Hashtbl.t
val get_interface_location_hints    : processing_context -> (string, string) Hashtbl.t
val print_location_hints            : (string, string) Hashtbl.t -> unit
(* A location hint may resolve to multiple resource names *)
val resolve_schema_location_hint : processing_context -> string option -> string -> string list
val resolve_module_location_hint : processing_context -> string option -> string -> string list
val resolve_interface_location_hint : processing_context -> string option -> string -> string list

(***************************)
(* Check the configuration *)
(***************************)

val check_config : processing_context -> unit
val copy_processing_context : processing_context -> processing_context

