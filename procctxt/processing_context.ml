(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: processing_context.ml,v 1.68 2008/02/01 02:45:26 mff Exp $ *)

(* Module: Processing_context
   Description:
     This module contains basic configuration parameters for Galax,
     notably set up of the processing phases.
*)

open Error

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
  | Serialize_As_Text

type projection_kind =
  | Projection_None
  | Projection_Standard
  | Projection_Optimized

type treejoin_logical_kind =
  | Default       (* no rewritings for bulk *)
  | TreeJoin      (* apply bulk TreeJoin rewrites 
                      NOTES 
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

(*********************************************************)
(* MODULE processing context : per-module STATIC context *)
(*********************************************************)

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

let default_module_processing_context() = 
  {
  boundary_space_kind 	     = Xquery_common_ast.Strip;
  construction_kind   	     = Xquery_common_ast.Preserve;
  ordering_kind             = Xquery_common_ast.Ordered;
  default_order_kind        = Xquery_common_ast.EmptyGreatest;
  ns_preserve_kind          = Xquery_common_ast.NSPreserve;
  ns_inherit_kind           = Xquery_common_ast.NSInherit;

   (* Base URI *)
  base_uri                  = AnyURI.default_base_uri();
   (* Default collation *)
  default_collation         = AnyURI._string_of_uri(AnyURI.default_collation_uri());

  system                    = false;
  name_generators           = [];
} 

(* Returns the base URI *)

let get_base_uri proc_ctxt = proc_ctxt.base_uri
let get_default_collation proc_ctxt = proc_ctxt.default_collation
let set_boundary_space_kind  proc_ctxt boundary_space_kind = proc_ctxt.boundary_space_kind <- boundary_space_kind
let set_construction_kind    proc_ctxt construction_kind = proc_ctxt.construction_kind <- construction_kind
let set_ordering_kind        proc_ctxt ordering_kind = proc_ctxt.ordering_kind <- ordering_kind
let set_default_order_kind   proc_ctxt defaultorder = proc_ctxt.default_order_kind <- defaultorder
let set_ns_preserve_kind     proc_ctxt nspreserve_kind = proc_ctxt.ns_preserve_kind <- nspreserve_kind
let set_ns_inherit_kind      proc_ctxt nsinherit_kind = proc_ctxt.ns_inherit_kind <- nsinherit_kind

let set_mod_proc pc dpc =
  pc.boundary_space_kind <- dpc.boundary_space_kind;
  pc.construction_kind <- dpc.construction_kind;
  pc.ordering_kind <- dpc.ordering_kind;
  pc.default_order_kind <- dpc.default_order_kind;
  pc.ns_preserve_kind <- dpc.ns_preserve_kind;
  pc.ns_inherit_kind <- dpc.ns_inherit_kind;
  pc.base_uri <- dpc.base_uri;
  pc.default_collation <- dpc.default_collation

let register_name_gen mod_proc_ctxt ngr =
  mod_proc_ctxt.name_generators <- ngr :: mod_proc_ctxt.name_generators

let get_name_generator mod_proc_ctxt prefix uri localprefix =
  let ngr = ref (Namespace_generate.create_name_generator prefix uri localprefix) in
  begin
    register_name_gen mod_proc_ctxt ngr;
    ngr
  end

(*
let reset_name_generator pc =
  List.iter (fun x -> Namespace_generate.reset_name_generator !x) pc.name_generators
*)

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
(* PROGRAM processing context : per-program STATIC context       *)
(* Shared by all program components                              *)
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
      mutable merge_module_locations  : bool;

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

      (* Zero proxy configuration *)
      mutable zerod_host  : string option;  
      mutable zerod_port : int option;

 (* Execution options *)
      mutable serialization_kind  : serialization_kind;(* Serialization kind *)
      monitor_context            : monitor_context;   (* Function monitoring context *)
      (* External namespace bindings *)
      mutable external_nsenv     : Namespace_context.nsenv;
}

(* Default processing context *)
let default_processing_context () = 
  {

  (* Processing model options *)
   normalization 	     = true;  (* true when normalization is on [default: on] *)
   normalization_ident 	     = false; (* true when normalization is identity [default: off] *)
   typing        	     = true;  (* true when static typing is on [default: on] *)
   rewriting   	 	     = true;  (* true when rewriting of the normalized core on [default: on] *)
   factorization             = false; (* true when factorization is on [default: on] *)
   optimization              = true;  (* true when algebraic optimization is on [default: on] *)
   code_selection            = true;  (* true when code selection is on [default: on] *)
   evaluation 	 	     = true;  (* true when evaluation is on [default: on] *)

 (* Data model options *)
   xml_whitespace  = true; (* true when preserving boundary whitespace [default: on] *)
   xml_pis_and_comments = true; (* true when preserving pis and comments [default: on ] *)

 (* Normalization options *)
   (* Location hints *)
   schema_location_hints      = Hashtbl.create 7;
   module_location_hints      = Hashtbl.create 7;
   interface_location_hints   = Hashtbl.create 7;
   (* Flag to interpret module locations *)
   merge_module_locations        = false; 
  
 (* Compilation options/variables *)
   typing_kind        	     = Typing_Weak;   (* What kind of static analysis?   [default: weak] *)

 (* Optimization options *)
   sbdo_kind          	     = SBDO_Preserve; (* What kind of SBDO optimization? [default: duptidy] *)
   inline_functions    	     = false;
   inline_variables    	     = true;
   projection_kind    	     = Projection_None ;
   (* Optimization and Algebra options *)
   treejoin_log              = Default;  
   treejoin_phys             = NestedLoop;
   streaming                 = false;
   infer_independence        = false;

 (* DXQ configuration options *)
   dxq_server        = None; 
   dxq_optimization  = false;
   dxq_host          = None;
   dxq_port          = None;
   dxq_source        = None;
   dxq_topology      = None;
   dxq_drop_msgs     = false;

   zerod_host        = None;
   zerod_port        = None;

 (* Execution options *)
   serialization_kind 	     = Serialize_As_Standard;
   (* API Function Monitoring Context *)
   monitor_context           = default_monitor_context() ;
   (* External namespace bindings *)
   external_nsenv            = Namespace_context.default_xquery_nsenv;

 }

(* Returns the external namespace environment *)

let get_external_nsenv proc_ctxt = proc_ctxt.external_nsenv

(* Set specific parameters in the processing context *)

let set_normalization_phase  proc_ctxt b = proc_ctxt.normalization <- b
let set_normalization_ident  proc_ctxt b = proc_ctxt.normalization_ident <- b
let set_typing_phase         proc_ctxt b = proc_ctxt.typing <- b
let set_rewriting_phase      proc_ctxt b = proc_ctxt.rewriting <- b
let set_factorization_phase  proc_ctxt b = proc_ctxt.factorization <- b
let set_optimization_phase   proc_ctxt b = proc_ctxt.optimization <- b
let set_code_selection_phase proc_ctxt b = proc_ctxt.code_selection <- b
let set_evaluation_phase     proc_ctxt b = proc_ctxt.evaluation <- b

let set_xml_whitespace       proc_ctxt b = proc_ctxt.xml_whitespace <- b
let set_xml_pis_and_comments proc_ctxt b = proc_ctxt.xml_pis_and_comments <- b

let set_inline_variables     proc_ctxt b = proc_ctxt.inline_variables <- b
let set_inline_functions     proc_ctxt b = proc_ctxt.inline_functions <- b
let set_sbdo_kind            proc_ctxt sbdo_kind             = proc_ctxt.sbdo_kind <- sbdo_kind
let set_typing_kind          proc_ctxt typing_kind           = proc_ctxt.typing_kind <- typing_kind
let set_serialization_kind   proc_ctxt serialization_kind    = proc_ctxt.serialization_kind <- serialization_kind

let set_projection_kind      proc_ctxt projection_kind       = proc_ctxt.projection_kind <- projection_kind

let set_treejoin_log         proc_ctxt treejoin_log        = proc_ctxt.treejoin_log  <- treejoin_log
let set_treejoin_phys        proc_ctxt treejoin_phys       = proc_ctxt.treejoin_phys <- treejoin_phys
let set_streaming            proc_ctxt streaming           = proc_ctxt.streaming <- streaming
let set_infer_independence   proc_ctxt infer_independence  = proc_ctxt.infer_independence <- infer_independence
let set_dxq_server           proc_ctxt establish_srvr      = proc_ctxt.dxq_server <- Some establish_srvr
let set_dxq                  proc_ctxt dxq                 = proc_ctxt.dxq_optimization <- dxq
let set_dxq_host_port        proc_ctxt hostopt portopt     = 
  (proc_ctxt.dxq_host <- hostopt; proc_ctxt.dxq_port <- portopt)
let set_dxq_source           proc_ctxt source              =  proc_ctxt.dxq_source <- (Some source)
let set_dxq_topology         proc_ctxt source              =  proc_ctxt.dxq_topology <- (Some source)
let set_dxq_drop_msgs        proc_ctxt drop                =  proc_ctxt.dxq_drop_msgs <- drop

let set_zerod_host_port      proc_ctxt hostopt portopt     =
  (proc_ctxt.zerod_host <- hostopt; proc_ctxt.zerod_port <- portopt)

let set_namespace_env        proc_ctxt nsenv =
  proc_ctxt.external_nsenv <- nsenv

(******************************)
(* Location Hints             *)
(* Schema, Module, Interface location hints: URI => locations *)
(******************************)
let add_schema_location_hints proc_ctxt sl =
  List.iter (fun (uri, loc) -> Hashtbl.add proc_ctxt.schema_location_hints uri loc) sl
let add_module_location_hints proc_ctxt sl =
  List.iter (fun (uri, loc) -> Hashtbl.add proc_ctxt.module_location_hints uri loc) sl
let add_interface_location_hints proc_ctxt sl =
  List.iter (fun (uri, loc) -> Hashtbl.add proc_ctxt.interface_location_hints uri loc) sl
let get_schema_location_hints  proc_ctxt =  proc_ctxt.schema_location_hints
let get_module_location_hints  proc_ctxt =  proc_ctxt.module_location_hints
let get_interface_location_hints  proc_ctxt =  proc_ctxt.interface_location_hints
let print_location_hints lh =
  Printf.printf "\t------------------\n";
  Hashtbl.iter (fun x y -> Printf.printf "\turi: %s; loc: %s\n" x y) lh;
  Printf.printf "\t------------------\n"

(* Always using the location hint, if it is present - Jerome *)
let resolve_location_hint hints loc_hint_option target_uri = 
  match loc_hint_option with
  | Some uri -> [uri]
  | None ->
      begin
	try
	  Hashtbl.find_all hints target_uri 
	with
	| Not_found ->
	  [target_uri] 
      end
let resolve_schema_location_hint proc_ctxt loc_hint_option target_uri = 
  resolve_location_hint proc_ctxt.schema_location_hints loc_hint_option target_uri 

let resolve_module_location_hint proc_ctxt loc_hint_option target_uri = 
  resolve_location_hint proc_ctxt.module_location_hints loc_hint_option target_uri 

let resolve_interface_location_hint proc_ctxt loc_hint_option target_uri = 
  resolve_location_hint proc_ctxt.interface_location_hints loc_hint_option target_uri 

let set_base_uri proc_ctxt base_uri =  proc_ctxt.base_uri <- base_uri

(* Only default collation is known for now - Jerome *)
let known_collation uri =
  uri = Conf.collns

let check_collation_aux proc_ctxt uri =
  let uri' = 
    match  proc_ctxt.base_uri with
    | Some base_uri -> 
	(AnyURI._string_of_uri(AnyURI._uri_resolve base_uri (AnyURI._kinda_uri_of_string uri)))
    | None -> uri
  in
  if known_collation uri' then uri'
  else
    raise
      (Query(Wrong_Args("[err:XQST0038] Unknown collation:" ^ uri')))

let check_collation proc_ctxt uri =
  ignore(check_collation_aux proc_ctxt uri)

let set_default_collation proc_ctxt uri =
  let uri' = check_collation_aux proc_ctxt uri in
  proc_ctxt.default_collation <- uri'


(***************************)
(* Check the configuration *)
(***************************)

let check_config proc_ctxt =
  begin
    if proc_ctxt.treejoin_phys = Streaming && not(proc_ctxt.treejoin_log = TreeJoin) then
      raise (Query(Wrong_Args("XPath streaming requires logical bulk treejoins (use -treejoin-log treejoin)")));
    if proc_ctxt.typing && (not(proc_ctxt.normalization) && not(proc_ctxt.normalization_ident)) then
      raise (Query(Wrong_Args("Static typing requires normalization")));
    if proc_ctxt.rewriting && (not(proc_ctxt.normalization) && not(proc_ctxt.normalization_ident)) then
      raise (Query(Wrong_Args("Optimization requires normalization")));
    if proc_ctxt.evaluation && (not(proc_ctxt.normalization) && not(proc_ctxt.normalization_ident)) then
      raise (Query(Wrong_Args("Dynamic evaluation requires normalization")));
    if !Conf.print_type && not(proc_ctxt.typing) then
      raise (Query(Wrong_Args("Cannot print type without static typing on")));
    if !Conf.print_core_expr && (not(proc_ctxt.normalization)) then
      raise (Query(Wrong_Args("Cannot print normalized expression without normalization on")));
    if !Conf.print_optimized_expr && not(proc_ctxt.rewriting) then
      raise (Query(Wrong_Args("Cannot print optimized expression without optimization on")));
    if !Conf.print_xml && not(proc_ctxt.evaluation) then
      raise (Query(Wrong_Args("Cannot print xml result without dynamic evaluation on")));
    if proc_ctxt.evaluation && not(proc_ctxt.code_selection) then
      raise (Query(Wrong_Args("Cannot perform dynamic evaluation without code selection on")));
  end

let copy_processing_context pc =
  { 
  (* Processing module options *)
  normalization       = pc.normalization;
  normalization_ident = pc.normalization_ident;
  typing              = pc.typing;
  rewriting           = pc.rewriting;
  factorization       = pc.factorization;
  optimization        = pc.optimization;
  code_selection      = pc.code_selection;
  evaluation          = pc.evaluation;

 (* Data model options *)
  xml_whitespace       = pc.xml_whitespace;
  xml_pis_and_comments = pc.xml_pis_and_comments;

 (* Normalization options *)
      (* Location hints *)
  schema_location_hints = Hashtbl.copy pc.schema_location_hints;
  module_location_hints = Hashtbl.copy pc.module_location_hints;
  interface_location_hints = Hashtbl.copy pc.interface_location_hints;
  merge_module_locations  = pc.merge_module_locations;

 (* Compilation options/variables *)
  typing_kind 	  = pc.typing_kind;

 (* Optimization options *)
  sbdo_kind        	 = pc.sbdo_kind;
  inline_functions 	 = pc.inline_functions;
  inline_variables 	 = pc.inline_variables;
  projection_kind    = pc.projection_kind;
  treejoin_phys      = pc.treejoin_phys;
  treejoin_log       = pc.treejoin_log;
  streaming          = pc.streaming;
  infer_independence = pc.infer_independence;

 (* DXQ configuration options *)
  dxq_server = pc.dxq_server;
  dxq_optimization   = pc.dxq_optimization;
  dxq_host  = pc.dxq_host;
  dxq_port  = pc.dxq_port;
  dxq_source = pc.dxq_source;
  dxq_topology = pc.dxq_topology;
  dxq_drop_msgs = pc.dxq_drop_msgs;

 (* Execution options *)
  serialization_kind  = pc.serialization_kind;
  monitor_context     = pc.monitor_context;   
      (* External namespace bindings *)
  external_nsenv = pc.external_nsenv;

  zerod_host = pc.zerod_host;
  zerod_port = pc.zerod_port;
}

let is_weak_typing proc_ctxt =
  match proc_ctxt.typing_kind with
  | Typing_None -> false
  | Typing_Weak -> true
  | Typing_Strong -> false


