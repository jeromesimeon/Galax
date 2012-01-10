(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: conf.mli,v 1.93 2007/10/24 15:15:17 simeon Exp $ *)

(* Module: Conf
   Description:
     This module contains global variables and parameters changing the
     general behavior of the Galax interpretor.
*)

(***********************)
(* Version information *)
(***********************)

val system : string
    (* System name *)

val version : string
    (* Current version number *)

val copyright : string
    (* Copyright notice *)

val status : string
    (* This release's status *)

val build : string
    (* Build information *)

val motd : string
    (* Any additional notice *)

val xquery_version : string
val xquery_conformance : bool ref

val galax_web : string

(**************************)
(* Galax library location *)
(**************************)

(* Where is the Galax library installed *)

val galax_library : string

(* Where is the standard library file *)

val pervasive_content : string ref

(* Where are Camomile Unicode maps *)

val unicode_maps : string option ref


(***********************)
(* Behavior parameters *)
(***********************)

(* Output behavior *)

val print_global        : bool ref
val print_prolog        : bool ref

val verbose : bool ref
    (* true if verbose output emitted  *)

val verbose_error : bool ref
    (* true is full error message is emitted *)

val warning       : bool ref

val xml_charescape_fn : (string -> string) ref
    (* function for escaping characters in XML output and expressions *)

val print_xml : bool ref
    (* true if core expression emitted *)
val xml_output    : out_channel ref
val xml_formatter : Format.formatter ref 
val xml_header 	  : string ref
val xml_footer 	  : string ref

val print_expr : bool ref
    (* true if expression emitted *)
val expr_output : out_channel ref
val expr_formatter : Format.formatter ref 
val expr_header : string ref 
val expr_footer : string ref 

val print_type : bool ref
    (* true if type emitted *)
val type_output : out_channel ref
val type_formatter : Format.formatter ref
val type_header : string ref 
val type_footer : string ref 

val print_core_expr : bool ref
    (* true if core expression emitted *)
val core_expr_output : out_channel ref
val core_expr_formatter : Format.formatter ref
val core_expr_header : string ref
val core_expr_footer : string ref

val print_annotations : bool ref
    (* true if expression annotations are printed *)

val print_optimized_expr : bool ref
    (* true if optimized core expression emitted *)
val optimized_expr_output : out_channel ref
val optimized_expr_formatter : Format.formatter ref
val optimized_expr_header : string ref 
val optimized_expr_footer : string ref 

val print_factorized_expr : bool ref
    (* true if factorized core expression emitted *)
val factorized_expr_output : out_channel ref
val factorized_expr_formatter : Format.formatter ref
val factorized_expr_header : string ref 
val factorized_expr_footer : string ref 

val print_projection : bool ref
val projection_output : out_channel ref
val projection_formatter : Format.formatter ref
    (* true when printing projection of document is on.
       off by default *)

val print_projected_file : bool ref
val projected_file_output : out_channel ref
val projected_file_formatter : Format.formatter ref

val projection_suffix : string

val glx_stderr : out_channel ref
val glx_err_formatter : Format.formatter ref


val print_algebra_optimization_rewrite     : bool ref
val algebra_optimization_rewrite_output    : out_channel ref
val algebra_optimization_rewrite_formatter : Format.formatter ref
val algebra_optimization_rewrite_header    : string ref
val algebra_optimization_rewrite_footer    : string ref

val print_logical_algebra  : bool ref
val logical_algebra_output : out_channel ref
val logical_algebra_formatter : Format.formatter ref
val logical_algebra_header : string ref
val logical_algebra_footer : string ref

val serialize_logical_algebra  : bool ref

val print_optimized_algebra  : bool ref
val optimized_algebra_output : out_channel ref
val optimized_algebra_formatter : Format.formatter ref
val optimized_algebra_header : string ref
val optimized_algebra_footer : string ref

val print_physical_algebra  : bool ref
val physical_algebra_output : out_channel ref
val physical_algebra_formatter : Format.formatter ref
val physical_algebra_header : string ref
val physical_algebra_footer : string ref

val print_dfgraph     : bool ref
val dfgraph_output    : out_channel ref
val dfgraph_formatter : Format.formatter ref

val genresults : bool ref

val print_lex_states : bool ref

(********************)
(* XML Plan Loading *)
(********************)

val load_xml_plans          : bool ref
val execute_logical_plan    : bool ref
val execute_optimized_plan  : bool ref

(*********************)
(* Global parameters *)
(*********************)

(* XML & XML Schema namespaces *)

val emptyns   : string
 
val xmlns     : string
val xmlnsns   : string
val xsns      : string
val xsdns     : string
val xsins     : string
val fnns      : string
val xqxns     : string
val opns      : string
val fsns      : string
val collns    : string
val errns     : string
val localns   : string
val glxns     : string

val bPrinting_comp_annotations : bool ref

(* Materialization flag *)

val print_materialize : bool ref

(********************************************)
(* Experimental parameters for optimization *)
(********************************************)
val new_descendant_style : bool ref

(* Aggressive sbdo remove *)

val aggressive_sbdo_remove     : bool ref

(* Physical optimization flags *)

val nested_loop_join           : bool ref

(* Variable materialization flag *)

val force_materialized_variables : bool ref
val allow_streamed_tuple_fields  : bool ref

(* Jungle flags *)

val old_children_method : bool ref
val jungle_buffsize : int option ref


(* SAX materialization buffers *)

val buffer_chunks : int ref
val buffer_csize  : int ref
val buffer_inc    : int ref

(* Statistics *)

val countload : int ref
val countnext : int ref
val countexpo : int ref

(* Language *)

type language_kind =
  | Language_XQuery10
  | Language_XQueryUpdates
  | Language_XQueryBang
  | Language_XQueryP
  | Language_DXQ

val language : language_kind ref

val set_language : language_kind -> unit

val is_xquery     : unit -> bool
val is_ultf       : unit -> bool
val is_xquerybang : unit -> bool
val is_xqueryp    : unit -> bool
val is_dxq        : unit -> bool

type syntax_kind =
  | XQuery_Syntax
  | XQueryX_Syntax

val syntax : syntax_kind ref
val set_syntax : syntax_kind -> unit
val is_xquery_syntax : unit -> bool
val is_xqueryx_syntax : unit -> bool

val batch_xqueryx : bool ref
val embed_xqueryx : bool ref

type materialize_tables_kind = 
    | Always
    | Analysis
    | Never


val set_materialize_tables : materialize_tables_kind -> unit
val get_materialize_tables : unit -> materialize_tables_kind

(* WSDL parameters *)

val service_namespace : string ref
val client_filename : string ref
val server_filename : string ref
val impl_filename : string option ref

val generate_client : bool ref
val generate_server : bool ref
val chosen_port : string option ref
val chosen_service : string option ref
val installdir : string ref

(* SOAP parameters *)

val wsdl_url : string ref
val chosen_port : string option ref
val chosen_binding : string option ref
val installdir : string ref
val interfacedir : string ref
val address_uri : string option ref
val nms_uri : string ref

