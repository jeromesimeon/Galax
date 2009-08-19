(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: wsdl_export.ml,v 1.27 2007/09/25 15:12:43 mff Exp $ *)


(** 
  @(#)wsdl_export.ml

  Builds an XQuery AST from a wsdl AST -- export capabilities.
  
  It should not be used any more, since the we plan to
  export modules and not to generate mere skeletons. 
  @see wsdl_apache.ml 
  

  @author Nicola Onose
*)

open Error

open Namespace_names

open Xquery_common_ast
open Xquery_ast
open Xquery_ast_util
open Wsdl_ast
open Wsdl_util


(** 
  For services defining a SoapAction HTTP header,
  a function in the server XQuery file should be 
  called in a context containing a variable
  $SoapAction having the value given by the client.
*)


exception SoapServerException of error

let unknown_bind_exn = SoapServerException 
			 (Protocol_Error "Don't know how to handle non Soap/RPC service")



let one_output (v_out,t,e) fun_localname implfun_localname service_nms wsdl_mod binding port_type op =
  (* input parameters are the arguments of the input message *)
  (* maybe we should also check the soapAction header *)

  if not(Wsdl_astutil.is_an_rpc_binding binding)
    && not(Wsdl_astutil.is_an_rpc_operation op) then
      raise unknown_bind_exn
  else
    let v,funsig_input = make_import_args wsdl_mod port_type op in
    let vars_in = List.map 
		    (fun s -> NSDefaultElementPrefix,s) 
		    v 
    in      
    let vars_out = List.map 
		     (fun s -> NSDefaultElementPrefix,s) 
		     v_out
    in 
      (* till we have name typing, we use a 'heuristics' *)
    let fun_nms = NSPrefix service_nms in
    let fun_fullname = (fun_nms,fun_localname) in 
    let impl_fullname = (fun_nms, implfun_localname) in 
    let arg_exprs = List.map (fun v -> mkexpr (EVar v)) vars_in in
    let let_exprs = 
      mkexpr (EFLWOR 
		((List.map 		 
		    (fun x -> 
		       mkfl_expr (ELet 
				    (None,
				     x,
				     (mkexpr (EApp (impl_fullname, arg_exprs))))))
		    vars_out),
		 None,
		 None,
		 e))
    in
      mkfunction_def 
	(fun_fullname,
	 vars_in,
	 funsig_input,
	 (EFunctionUser let_exprs),
	NonUpdating)
	
	
let make_one_function_output service_nms wsdl_mod binding port_type op =
  let msg = 
    get_output_message (get_operation_in_port_type port_type op)  wsdl_mod
  in
  let v,t,e = build_params_elems wsdl_mod msg op
  in
(*  let fun_localname = op.operation_in_binding_name ^ ".server" *)
  let fun_localname = op.operation_in_binding_name
  in
  let implfun_localname = op.operation_in_binding_name 
  in
    one_output (v,t,List.hd e) fun_localname implfun_localname service_nms wsdl_mod binding port_type op 
  

let add_fault_functions service_nms wsdl_mod binding port_type op_in_binding other_funs =
  let pt_op = get_operation_in_port_type port_type op_in_binding in
  let msgs = get_fault_messages pt_op wsdl_mod in  
  let rec process_messages funs i = function
    | [] -> funs
    | m::rest ->
	let pt_op = get_operation_in_port_type port_type op_in_binding in
	let msgname = (get_input_message pt_op wsdl_mod).message_name
	in
	let vars_envelope = make_envelope_and_body
			      (Wsdl_astutil.Fault_msg msgname)
			      ([],([],None),[])  (* bogus *)
			      wsdl_mod binding 
			      op_in_binding
	in
	let fun_localname = op_in_binding.operation_in_binding_name ^
			    ".fault_" ^ (string_of_int i)
	in
	let f = one_output 
		  vars_envelope
		  fun_localname
                  (* Mary: Not sure what the implementation function should be called here: *)
	          op_in_binding.operation_in_binding_name  
		  service_nms
		  wsdl_mod
		  binding 
		  port_type
		  op_in_binding 
	in
	  process_messages (f::funs) (i+1) rest
  in
    process_messages other_funs 1 msgs
      
(** add the output and the fault functions for the operation op to the list otherfuns *)
let add_operation_export service_nms wsdl_mod binding port_type op otherfuns =
  let outfun = make_one_function_output service_nms wsdl_mod binding port_type op in
    add_fault_functions service_nms wsdl_mod binding port_type op (outfun::otherfuns)
      
(** 
  create all the export functions (output/faults) needed for the given binding 
  and add them to the list otherfuns
*)
let make_functions_for_a_binding_export service_nms wsdl_mod binding otherfuns = 
  let port_type = get_port_type binding wsdl_mod in
    List.fold_left
      (fun extrafuns opbind -> add_operation_export service_nms wsdl_mod binding port_type opbind extrafuns)
      otherfuns
      binding.binding_operations
      
let make_all_export_functions service_nms wsdl_mod chosen_service chosen_port = 
  let good_service = 
    if chosen_service = None then 
      if wsdl_mod.wsdl_services = [] then None
      else Some (List.hd wsdl_mod.wsdl_services)
    else
      try 
	Some 
	  (List.find (is_the_right_service chosen_service) wsdl_mod.wsdl_services)
      with Not_found -> 
	raise (Query 
		 (Load_Error
		    ("Service " ^ (string_of_stringoption chosen_service) ^" not found")))

  in    
    match good_service with
      | None -> []
      | Some srv -> 
	  let good_port = 
	    if chosen_port = None then 
	      if srv.ports = [] then None
	      else Some (List.hd srv.ports)
	    else
	      try 
		Some
		  (List.find (is_the_right_port chosen_port) srv.ports)
	      with Not_found -> 
		raise (Query 
			 (Load_Error
			    ("Port " ^ (string_of_stringoption chosen_port) ^" not found")))
	  in	    	  
	    match good_port with
	      | None -> []
	      | Some port ->	
		  let b = Wsdl_util.get_binding_for_a_port port wsdl_mod in
		    make_functions_for_a_binding_export service_nms wsdl_mod b []

    
let wsdl_ast_to_xquery_ast_export service_nms wsdl_mod chosen_service chosen_port installdir impl_filename = 
  let uri = AnyURI._string_of_uri wsdl_mod.targetNamespace in
  let import_module_stmt = mkcontext_decl(EImportModuleDecl(service_nms, uri, Some impl_filename)) in 
  let prolog =
    { pprolog_xschemas = [];
      pprolog_contexts = import_module_stmt :: (make_module_context_decls service_nms wsdl_mod);
      pprolog_funcvars = List.map (fun x ->FunDef x) (make_all_export_functions 
	service_nms
	wsdl_mod
	chosen_service
	chosen_port);
      pprolog_indices = [] }
  in
  let library_module =
    { plibrary_module_decl = (service_nms,uri,None);
      plibrary_module_prolog = prolog }
  in
  ELibraryModule library_module

let export_to_out_channel chan service_nms wsdl_mod chosen_service chosen_port installdir impl_filename = 
  let ff = Format.formatter_of_out_channel chan in
  let xqm = wsdl_ast_to_xquery_ast_export service_nms 
	      wsdl_mod chosen_service chosen_port installdir impl_filename in
    Print_xquery.print_module ff xqm
      
let wsdl_to_xqfile_export filename nms wmod serv port installdir optimpl_filename = 
  let can_out = open_out filename in
  let impl_filename = 
    match optimpl_filename with
    | None -> 
	begin
	  try
	    (Filename.chop_extension filename)^"-impl.xq" 
	  with
	  | _ -> filename^"-impl.xq"
	end
    | Some impl_filename -> impl_filename 
  in
    export_to_out_channel can_out nms wmod serv port installdir impl_filename;
    close_out can_out
      
