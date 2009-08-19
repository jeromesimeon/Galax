(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: wsdl_ast.mli,v 1.18 2007/02/01 22:08:55 simeon Exp $ *)

(** 
  @(#)wsdl_ast.mli
  
  Syntax tree representation of a wsdl description.

  NOTE: For the time being, it is only a partial representation of 
  the WSDL specifications. The "import" element is not supported.
  All the HTTP and SOAP bindings are not fully implemented.

  
  @author Nicola Onose
  @see wsdl_load.mlp

*)


(************************)
(* Abstract Syntax Tree *)
(************************)

open Datatypes 

open Namespace_names
open Namespace_symbols

type soapStyleChoice = Soap_RPC | Soap_Document

type soap_binding = 
    { nms_prefix: prefix;
      transport: xs_anyURI option;
      soap_binding_style: soapStyleChoice option; (* or "styleDefault" in wsdl 1.2 *)
      encodingStyleDefault: xs_anyURI option }

type soapUseChoice = Literal | Encoded

type soap_body = 
    { soap_body_encodingStyle: xs_anyURI option;
      use: soapUseChoice option;
      soap_body_nms: xs_anyURI option }
    
type soap_header = 
    { soap_header_element: uqname option;
      soap_header_type: uqname option;
      soap_header_localname: string option;
      soap_header_nms: xs_anyURI option;      
      soap_header_encodingStyle: xs_anyURI option;
      soap_header_role: xs_anyURI option }
    
type soap_headerfault = 
    { soap_headerfault_message: uqname;
      soap_headerfault_part: string;
      soap_headerfault_nms: xs_anyURI option;
      soap_headerfault_encodingStyle: xs_anyURI option }
    
type soap_inout = soap_body option * soap_header list * soap_headerfault list
    
type soap_fault = 
    { soap_fault_prefix: prefix; (* the prefix used for the namespace *)
      soap_fault_name: string;
      soap_fault_nms: xs_anyURI option;
      soap_fault_encodingStyle: xs_anyURI option }

type soap_operation = 
    { nms_soap_operation: prefix;
      soapAction: xs_anyURI option;
      soap_operation_style: soapStyleChoice option }

type soap_address =
    { nms_soap_address: prefix;
      location: xs_anyURI }      
    
type part_type_decl = PartType of (uri * uqname) | PartElement of (uri * uqname)
  (* ``uri'' is the URI corresponding to the prefix from the uqname *)
  
type part = 
    { part_name: ncname;
      part_type: part_type_decl }
    
type message = 
    { message_name: ncname;
      mutable message_parts: part list }
        
type operation_in_port_type =
    { operation_name: ncname;
      input: rqname option;
      output: rqname option;
      faults: rqname list }
    (*
      | OneWayOp of ncname * inputType (* the string is the name of the operation *)
      | RequestResponseOp of ncname * inputType * outputType * faultType list
      | SolicitResponseOp of ncname * inputType * outputType * faultType list
      | NotificationOp of ncname * outputType
    *)

type port_type = 
    { port_type_name: ncname;
      mutable operations: operation_in_port_type list;
      mutable extends: string option (* uqname list*) }

type operation_in_binding = 
    { operation_in_binding_name: ncname;
      binding_input:  soap_inout option;
      binding_output: soap_inout option;
      binding_faults: soap_fault list;
      binding_soap_operation: soap_operation option;
      http_relative_uri: string option }
    
type service_binding =
    { binding_name: string;
      binding_type: rqname; (*port_type*)
      mutable bind_soap: soap_binding option;
      mutable binding_operations: operation_in_binding list;
      http_method : string }

type service_port = 
    { service_port_name: ncname;
      service_port_binding: rqname;
      port_soap_address: soap_address option;
      http_base_uri: string option }    

type service = 
    { service_name: string;
      mutable ports: service_port list }
    
type service_schema = Xquery_type_ast.xschema
    (*
      InternalSchema of Xquery_type_ast_named.xschema  (* from the "types" element *)
      | ExternalSchema of string                         (* XMLSchema file name *)
    *)

    
(**
  wsdl_module contains all the information in the definitions
  element that is considered significant
*)
type wsdl_module =
    { wsdl_name: string;
      mutable global_xmlns: Namespace_context.binding_table; 
      mutable targetNamespace: xs_anyURI;      
      mutable wsdl_services: service list;
      mutable wsdl_types: service_schema option;
      mutable wsdl_bindings: service_binding list;
      mutable wsdl_port_types: port_type list;
      mutable wsdl_messages: message list;
      mutable soapenv_prefix_name : ncname;
      mutable xsi_prefix_name : ncname;
      mutable xsd_prefix_name : ncname}
    
