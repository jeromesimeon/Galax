(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: wsdl_astutil.ml,v 1.10 2007/02/01 22:08:55 simeon Exp $ *)

(** 
  @(#)wsdl_astutil.mli
  
  Useful operations for manipulating wsdl syntax as defined in 
  wsdl_ast.mli

  @author Nicola Onose
  @see ast/wsdl_ast

*)  

open Wsdl_ast
   
type message_type = Input_msg | Output_msg | Fault_msg of string
			
let get_wsdl_types defs = defs.wsdl_types

let is_an_rpc_binding binding = 
    match binding.bind_soap with
      | Some bs ->
	  bs.soap_binding_style = Some Soap_RPC
      | None -> false


let is_an_rpc_operation op_in_binding = 
    match op_in_binding.binding_soap_operation with
      | Some bso ->
	  bso.soap_operation_style = Some Soap_RPC
      | None -> false

	  
let get_soap_body op_in_binding = 
  match op_in_binding.binding_input with
    | Some (Some sb as b, _, _) -> b
    | Some (None, _, _) -> None
    | _ ->  
	match op_in_binding.binding_output with
	  | Some (Some sb as b, _, _) -> b
	  | _ -> None	      


(**
  retrieves the xmlns attribute that has the same URI as
  the targetNamespace
*)
  let get_target_namespace_prefix wsdl_mod = 
    try 
      let tnms = AnyURI._string_of_uri wsdl_mod.targetNamespace 
      in
      let pref,_ = 
	List.find 
	  (fun (_,x) -> (Namespace_names.quoted_string_of_uri x) = tnms) 
	  wsdl_mod.global_xmlns
      in
	Some (Namespace_names.string_of_prefix pref)
    with Not_found -> None   

(**
  get the prefix of the namespace for the 
  parts inside the SOAP body
*)
let get_soap_body_nms_prefix wsdl_mod op_in_binding = 
  match op_in_binding.binding_input with
    | Some (Some sb, _, _) -> 
	begin
	  match sb.soap_body_nms with
	    | Some u -> None, u
	    | None -> 
		get_target_namespace_prefix wsdl_mod, 
		wsdl_mod.targetNamespace
	end
    | _ -> 
	get_target_namespace_prefix wsdl_mod, 
	wsdl_mod.targetNamespace

	

  
let get_encoding_style op_in_binding serv_binding msgtype = 
  let default_enc s_binding = 
    match s_binding.bind_soap with
      | None -> None
      | Some bs -> bs.encodingStyleDefault
  in
    match msgtype with
      | Fault_msg name ->
	  begin
	    try 
	      let f = List.find
			(fun x -> x.soap_fault_name = name) 
			op_in_binding.binding_faults
	      in
		f.soap_fault_encodingStyle
	    with Not_found -> default_enc serv_binding
	  end
      | _ ->
	  let inout_body = 
	    if msgtype = Input_msg then op_in_binding.binding_input
	    else op_in_binding.binding_output
	  in
	  match inout_body with
	  | Some (Some bi, _, _) -> 
	      begin
		match bi.soap_body_encodingStyle with
		| Some u as enc -> enc
		| None -> default_enc serv_binding
	      end
	  | _ -> default_enc serv_binding
		  

