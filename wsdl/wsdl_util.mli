(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: wsdl_util.mli,v 1.15 2007/02/01 22:08:55 simeon Exp $ *)

open Xquery_common_ast
open Xquery_ast
open Xquery_ast_util
open Wsdl_ast
open Wsdl_astutil


val default_soap_encoding_uris : string list
val encodingstyle_default : string

val default_soapenv_string : string 
val default_soapenv_nms : Datatypes.xs_anyURI
val default_soapenv_uri : Namespace_names.uri
val default_soapenv_prefix_name : Namespace_names.ncname
val default_xsi_uri : Namespace_names.uri
val default_xsi_prefix_name : Namespace_names.ncname
val default_xsd_uri : Namespace_names.uri
val default_xsd_prefix_name : Namespace_names.ncname



val wsdl_prefix_string : string
val wsdl_prefix : Namespace_names.prefix
val wsdl_uri_string : string
val wsdl_uri : Namespace_names.uri
val soap_prefix_string : string
val soap_prefix : Namespace_names.prefix
val soap_uri_string : string
val soap_uri : Namespace_names.uri
val http_prefix_string : string
val http_prefix : Namespace_names.prefix
val http_uri_string : string
val http_uri : Namespace_names.uri

 
val get_port_type :
  Wsdl_ast.service_binding -> Wsdl_ast.wsdl_module -> Wsdl_ast.port_type 

val get_operation_in_port_type :
  Wsdl_ast.port_type ->
  Wsdl_ast.operation_in_binding -> Wsdl_ast.operation_in_port_type 

val get_output_message :
  Wsdl_ast.operation_in_port_type -> Wsdl_ast.wsdl_module -> Wsdl_ast.message

val get_input_message :
  Wsdl_ast.operation_in_port_type -> Wsdl_ast.wsdl_module -> Wsdl_ast.message 

val get_fault_messages :
  Wsdl_ast.operation_in_port_type ->
  Wsdl_ast.wsdl_module -> Wsdl_ast.message list

val map_wsdl_type : service_schema option -> part_type_decl -> Xquery_ast.sequencetype
  
val make_import_args :
  Wsdl_ast.wsdl_module ->
  Wsdl_ast.port_type ->
  Wsdl_ast.operation_in_binding -> 
  Namespace_names.ncname list * Xquery_ast.function_signature

val build_params_elems :
  Wsdl_ast.wsdl_module -> Wsdl_ast.message ->
  Wsdl_ast.operation_in_binding ->
  Namespace_names.ncname list * (Xquery_ast.sequencetype option list) * Xquery_ast.expr list

val impex_params_elems :
  (Wsdl_ast.operation_in_port_type -> Wsdl_ast.wsdl_module -> Wsdl_ast.message) ->
  Wsdl_ast.wsdl_module ->
  Wsdl_ast.port_type ->
  Wsdl_ast.operation_in_binding ->
  Namespace_names.ncname list * Xquery_ast.function_signature * Xquery_ast.expr list

val build_attribute :
  ('a -> Xquery_ast.expr) ->
  Namespace_names.prefix -> Namespace_names.ncname -> 'a -> Xquery_ast.expr

val make_untyped_expression : Datatypes.xs_untyped -> Xquery_ast.expr 

val build_untyped_attribute :
  Namespace_names.prefix ->
  Namespace_names.ncname -> Datatypes.xs_untyped -> Xquery_ast.expr

val make_uri_expression : Datatypes.xs_anyURI -> Xquery_ast.expr

val build_uri_attribute :
  Namespace_names.prefix ->
  Namespace_names.ncname -> Datatypes.xs_anyURI -> Xquery_ast.expr

val build_ns_attribute :
  Namespace_names.ncname -> Datatypes.xs_anyURI -> Xquery_ast.expr

val make_generic_fault :
  Wsdl_ast.wsdl_module ->
  Datatypes.xs_untyped ->
  Datatypes.xs_untyped ->
  Datatypes.xs_untyped -> Datatypes.xs_untyped -> Xquery_ast.expr 

val make_generic_envelope : Namespace_names.prefix -> Xquery_ast.expr list -> Xquery_ast.expr 

val make_envelope_and_body :
  Wsdl_astutil.message_type ->
  (Namespace_names.ncname list) * Xquery_ast.function_signature * Xquery_ast.expr list ->
  Wsdl_ast.wsdl_module ->
  Wsdl_ast.service_binding ->
  Wsdl_ast.operation_in_binding -> 
  (Namespace_names.ncname list) * Xquery_ast.function_signature * Xquery_ast.expr
  
type impex_fun =   
    Wsdl_ast.wsdl_module ->  Wsdl_ast.service_binding -> Wsdl_ast.port_type -> 
  Wsdl_ast.operation_in_binding -> 
  (Namespace_names.ncname list) * Xquery_ast.function_signature * (Xquery_ast.expr list)
  
val make_envelope_and_body_impex :
  Wsdl_astutil.message_type ->
  impex_fun ->
  Wsdl_ast.wsdl_module ->
  Wsdl_ast.service_binding ->
  Wsdl_ast.port_type ->
  Wsdl_ast.operation_in_binding -> 
  (Namespace_names.ncname list) * Xquery_ast.function_signature * Xquery_ast.expr
  
val make_module_context_decls :
  Namespace_names.ncname -> Wsdl_ast.wsdl_module -> Xquery_ast.context_decl list

val is_the_right_port :
  Namespace_names.ncname option -> Wsdl_ast.service_port -> bool

val is_the_right_service : string option -> Wsdl_ast.service -> bool

val get_binding_for_a_port :
  Wsdl_ast.service_port -> Wsdl_ast.wsdl_module -> Wsdl_ast.service_binding

val string_of_stringoption : string option -> string

val make_type_declarations :
  Wsdl_ast.wsdl_module -> Xquery_type_ast.xtype_declaration list

val make_schema :
  Wsdl_ast.wsdl_module -> Xquery_type_ast.xschema



val is_encoded_input: Wsdl_ast.operation_in_binding -> bool
val is_encoded_output: Wsdl_ast.operation_in_binding -> bool


val get_input_parts : Wsdl_ast.port_type -> Wsdl_ast.operation_in_binding -> Wsdl_ast.wsdl_module ->
  Wsdl_ast.part list


val get_output_part : Wsdl_ast.port_type -> Wsdl_ast.operation_in_binding -> Wsdl_ast.wsdl_module ->
  Wsdl_ast.part


val output_part_test : Wsdl_ast.port_type -> Wsdl_ast.operation_in_binding -> Wsdl_ast.wsdl_module ->
  Xquery_ast.expr


val part_type_annot : Wsdl_ast.part -> Wsdl_ast.wsdl_module -> Xquery_ast.expr
