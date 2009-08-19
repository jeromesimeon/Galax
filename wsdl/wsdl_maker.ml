(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: wsdl_maker.ml,v 1.10 2007/09/25 15:12:43 mff Exp $ *)

(** 
  @(#)wsdl_maker.ml

  Builds a wsdl description from an XQuery module. 

  @author Nicola Onose
*)

open Namespace_names
open Xquery_ast


open Wsdl_ast
open Wsdl_util
open Xquery_type_ast_map


let collect_nms ctx = 
  let rec collect xmls = function
    | [] -> xmls
    | d::rest -> 
	match d.pcontext_decl_desc with 
	  | ENamespaceDecl (p,u) -> collect ((NSPrefix p, u)::xmls)  rest
	  | _ -> collect xmls rest
  in
    collect [] ctx
      

let collect_schema xslist = 
  if List.length xslist > 0 then Some (Xquery_type_ast_util.fmkschema xslist [] [])
  else None


let nms_for_prefix pref env = 
  let res = 
    List.find 
      (fun (p,_) -> p=pref)
      env
  in
    snd res
  
let is_in_nms prefix nms env =
  try 
    match nms_for_prefix prefix env with
	NSUri u -> u = nms
      | _ -> false
  with Not_found -> false


let mk_response fdef wmod out_name = match fdef.pfunction_def_desc with
    _,_,(_,res_type),_,_ -> [ {part_name = out_name; part_type = get_wsdl_type_for_xquery_type res_type wmod} ]
  
let mk_request fdef wmod = match fdef.pfunction_def_desc with
    _,_,(arg_types,_),_,_ ->     
      let rec build_args i args = function
	  [] -> args
	| t::rest -> 
	    build_args (i+1) args rest
      in List.rev (build_args 1 [] arg_types)
	   

let mk_msg_pair wmod pt_op fdef out_name =
  let mresp = { message_name = pt_op.operation_name ^ "Response"; 
	        message_parts = mk_response fdef wmod out_name }
  and mreq =  { message_name = pt_op.operation_name ^ "Request"; 
	        message_parts = mk_request fdef wmod }
  in
    wmod.wsdl_messages <- mresp::(mreq::wmod.wsdl_messages)


(** only functions declared in the target namespace are exported*)
let mk_one_op wmod pt b soap_body target_prefix targetnms fdef = 
  let (fpref, funname), vars, fsig, fbody, _ = fdef.pfunction_def_desc in
    (* check that fpref is the targetNamespace !! *)    
    if is_in_nms fpref targetnms wmod.global_xmlns then 
      let in_name = funname ^ "Request" in
      let out_name = funname ^ "Response" in	
      let pt_op = { operation_name = funname;  
		    input = Some (target_prefix, NSUri targetnms, in_name);
		    output = Some (target_prefix, NSUri targetnms, out_name);
		    faults = [] } 
      in
      let soap_op =  { nms_soap_operation = target_prefix;
		       soapAction = None;
		       soap_operation_style = Some Soap_RPC }	
      in
      let b_op =  { operation_in_binding_name = funname;
		    binding_input = Some (Some soap_body, [], []);
		    binding_output = Some (Some soap_body, [], []);
		    binding_faults = [];
		    binding_soap_operation = Some soap_op;
		    http_relative_uri = None }
      in
	pt.operations <- pt_op::pt.operations;
	b.binding_operations <- b_op::b.binding_operations;
	mk_msg_pair wmod pt_op fdef out_name
	  

let mk_ops wmod funs target_prefix targetnms = 
  let b =  List.hd (wmod.wsdl_bindings) in
  let pt = List.hd (wmod.wsdl_port_types) in
  let soap_body = 
    { soap_body_encodingStyle = Some (AnyURI._kinda_uri_of_string "http://schemas.xmlsoap.org/soap/encoding/");
      use = Some Encoded;
      soap_body_nms = Some (AnyURI._kinda_uri_of_string targetnms) }
  in
  let _ = 
    List.iter (mk_one_op wmod pt b soap_body target_prefix targetnms) funs
  in  
    pt.operations <- List.rev (pt.operations);
    b.binding_operations <- List.rev (b.binding_operations);
    wmod.wsdl_messages <- List.rev wmod.wsdl_messages;
    wmod;;


let fix_name suffix wname =  function 
    None -> wname ^ suffix
  | Some s -> s

let set_binding_name = fix_name "Binding"

let set_port_name = fix_name "Port"


let build_wsdl xmod name port_name bind_name address = 
  let (target_pref, target, _) = xmod.plibrary_module_decl in
  let prolog = xmod.plibrary_module_prolog in
  let xmls = (NSPrefix target_pref, NSUri target)::(collect_nms prolog.pprolog_contexts) in
  let real_binding_name = set_binding_name name bind_name in
  let pTypeName = real_binding_name ^ "Type"
  in
  let pt = {port_type_name = pTypeName; operations = []; extends = None}
  in
  let b = { binding_name = real_binding_name;
	    binding_type = (NSPrefix target_pref, NSUri target, pTypeName); 
	    bind_soap = None;
	    binding_operations = [];
	    http_method = "POST" }
  in
  let real_port_name = set_port_name name port_name in
  let port =  { service_port_name = real_port_name;
		service_port_binding = NSPrefix target_pref, NSUri target, real_binding_name;
		port_soap_address = Some address;
		http_base_uri = None }
  in        
  let create_new_pref xmls default_name = create_new_var (List.map (fun (p,_) -> string_of_prefix p) xmls) default_name
  in
  let stub = 
    { 
      wsdl_name = name;
      targetNamespace = AnyURI._kinda_uri_of_string target;
      global_xmlns = xmls;
      wsdl_services = [ {service_name = name; ports = [port]} ];
      wsdl_types = collect_schema prolog.pprolog_xschemas;
      wsdl_bindings = [b];
      wsdl_port_types = [pt];
      wsdl_messages = [];
      soapenv_prefix_name = create_new_pref xmls default_soapenv_prefix_name;
      xsi_prefix_name =  create_new_pref ((NSPrefix default_xsi_prefix_name, default_xsi_uri)::xmls) default_xsi_prefix_name;
      xsd_prefix_name =  create_new_pref ((NSPrefix default_xsd_prefix_name, default_xsd_uri)::xmls) default_xsd_prefix_name
    }
  in
  mk_ops stub
    (Xquery_ast_util.get_functions prolog.pprolog_funcvars) (NSPrefix target_pref) target

