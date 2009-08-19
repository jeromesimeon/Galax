(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: wsdl_import.ml,v 1.40 2007/09/25 15:12:43 mff Exp $ *)


(** 
  @(#)wsdl_import.ml

  Builds an XQuery AST from a wsdl AST -- import capabilities.

  @author Nicola Onose
*)


open Error
open Namespace_names
open Namespace_builtin

open Datatypes

open Xquery_common_ast
open Xquery_ast
open Xquery_ast_util

open Wsdl_ast
open Wsdl_util
open Xquery_type_ast_map

   (* This suffix is added to the WSDL function name in order to 
      create a port independent version of a function *)
let generic_fun_suffix = ".1"


let mk_string_expr s = 
  mkexpr(EScalar (StringLiteral s))
			   
let import_params_elems wsdl_mod binding port_type op_in_binding =
  impex_params_elems get_input_message wsdl_mod port_type op_in_binding 

let make_envelope_and_body_import = make_envelope_and_body_impex
				      Wsdl_astutil.Input_msg
				      import_params_elems 
				      

let ret_expr output_var node_test soapenv_body_test op binding port_type wsdl_mod service_nms = 
  let ret_cond = mkexpr 
		   (EPath
		      (PSlashSlash 
			 (mkexpr (EVar output_var), 
			  (mkexpr (EPath (PAxis (Child, PNameTest (NSPrefix wsdl_mod.soapenv_prefix_name, "Fault"))))))))
  in
  let ret_ifthen_expr = mkexpr (EApp ((NSPrefix "fn", "error"), 
				      [mkexpr (EPath (PSlash (mkexpr (EVar output_var), node_test)))]))
  in
  let response_expr = mkexpr (EPath (PSlash (mkexpr (EVar output_var), soapenv_body_test)))
  in
  let ret_else_expr =
    if Wsdl_astutil.is_an_rpc_binding binding then
      let wrapper_op = 
	mkexpr (EPath (PAxis (Child, PNameTest (NSPrefix service_nms, op.operation_in_binding_name)))) 
      in
      let unwrapped_msg = 
	mkexpr (EPath (PSlash (response_expr, wrapper_op)))
      in
	if not (is_encoded_output op) then (* literal *)
	  unwrapped_msg
	else (* encoded *)
	  let unwrap_part = output_part_test port_type op wsdl_mod in
	    mkexpr (EPath (PSlash (unwrapped_msg, unwrap_part)))
    else  (* no wrapper element *)
      mkexpr (EPath (PSlash (response_expr, node_test)))
  in
    mkexpr (EIf (ret_cond, ret_ifthen_expr, ret_else_expr))
      


let let_expr input_var output_var soap_funcall ret_expr elems = 
  mkexpr (EFLWOR 
	    ([mkfl_expr 
		  (ELet (None,
			 input_var,
			 elems));
	      mkfl_expr 
		(ELet (None,
		       output_var,
		       soap_funcall))],
	     None,
	     None,
	     ret_expr))
        
    
(* Create a function independent of a service port URL
   The function dedicated to a specific port will call this function
   using the appropriate URL argument *)
let make_one_function  service_nms wsdl_mod binding port_type op =
  let v',(d,r),e = make_envelope_and_body_import 
		     wsdl_mod
		     binding
		     port_type
		     op
  in
  let url_var_name = create_new_var v' "u"  in    
  let v = v' @ [url_var_name] in
  let vars = List.map 
	       (fun s -> NSDefaultElementPrefix,s) 
	       v 
  in
  let fun_nms = NSPrefix service_nms in 
  let fun_localname = op.operation_in_binding_name  ^ generic_fun_suffix in    
  let meth = make_untyped_expression binding.http_method in
  let action = match op.binding_soap_operation with
    | None -> make_untyped_expression ""
    | Some so -> 
	let s = 
	  match so.soapAction with 
	    | None -> ""
	    | Some action -> AnyURI._string_of_uri action
	in
	  make_untyped_expression ("SoapAction: " ^ s) 
  in
  let input_var_name = create_new_var v "input"
  in
  let input_var = (NSDefaultElementPrefix,
		   input_var_name) 
  in    
  let soap_funcall = 
    mkexpr (EApp 
	      ((NSPrefix "glx",
		"soap-call"),
	       [mkexpr(EVar (NSDefaultElementPrefix,url_var_name)); 
		meth; action; mkexpr(EVar input_var)]))
  in
  let output_var_name = create_new_var (input_var_name::v) "output"
  in
  let output_var = (NSDefaultElementPrefix,
		    output_var_name) 
  in    
  let node_test = 
    mkexpr (EPath (PAxis (Child, PNodeKindTest AnyKind)))
  in
  let soapenv_body_test = 
    mkexpr (EPath (PAxis (Child, PNameTest (NSPrefix wsdl_mod.soapenv_prefix_name, "Body"))))
  in
  let return_expr = ret_expr output_var node_test soapenv_body_test op binding port_type wsdl_mod service_nms
  in
  let fun_fullname = (fun_nms,fun_localname) in 
  mkfunction_def 
    (fun_fullname,
     vars,
     (d@[(Some (make_sequencetype (ITAtomic (NSPrefix "xsd", "anyURI"))))],r),
     (EFunctionUser (let_expr input_var output_var soap_funcall return_expr e)),
    NonUpdating)


let make_functions_for_a_binding_import service_nms wsdl_mod binding = 
  let port_type = get_port_type binding wsdl_mod in
    List.map 
      (fun opbind -> make_one_function service_nms wsdl_mod binding port_type opbind)
      binding.binding_operations
      
      
(** create all functions corresponding to all bindings in the given WSDL *)  
(*
  let make_binding_functions service_nms wsdl_mod =   
  let rec make_functions funlist = function
  | [] -> funlist
  | binding::rest -> 
  begin
  let one_binding = make_functions_for_a_binding_import service_nms wsdl_mod binding in
  make_functions (List.rev_append one_binding funlist) rest
  end
  in
  make_functions [] wsdl_mod.wsdl_bindings 
*)
(** create all functions corresponding to selected bindings in the given WSDL *)  
let make_binding_functions service_nms bindings wsdl_mod =   
  let rec make_functions funlist = function
    | [] -> funlist
    | binding::rest -> 
	begin
	  let one_binding = make_functions_for_a_binding_import service_nms wsdl_mod binding in
	    make_functions (List.rev_append one_binding funlist) rest
	end
  in
    make_functions [] bindings


(* create a function dedicated to a specific service call *)
let make_one_service_call_function service_nms wsdl_mod service port binding port_type op =
  let v,funsig = make_import_args wsdl_mod port_type op in    
  let fun_nms = NSPrefix service_nms in 
  let fun_localname = (*service.service_name ^ "_" ^ *)
    op.operation_in_binding_name in 
  let vars = List.map 
	       (fun s -> NSDefaultElementPrefix,s) 
	       v 
  in
  let host_string = match port.port_soap_address with
    | None ->  
	begin
	  match port.http_base_uri with
	    | None ->
		raise (Query 
			 (Load_Error
			    ("No SOAP server or HTTP address in port " ^ 
			     port.service_port_name)))	
	    | Some base_uri_string ->
		begin
		  match op.http_relative_uri with
		    | None -> base_uri_string
		    | Some rel -> base_uri_string ^ rel
		end
	end
	(*make_untyped_expression http_string*)
    | Some sa ->
	AnyURI._string_of_uri sa.location
  in
  let host = 
    mkexpr (EApp 
	      ((NSPrefix "xsd",
		"anyURI"),
	       [mk_string_expr host_string]))
  in
  let called_vars = List.map (fun w -> mkexpr(EVar w)) vars in
  let call_fun_expr =  (* call the function defined in the binding *)
    mkexpr (EApp 
	      ((NSPrefix service_nms, op.operation_in_binding_name ^ 
		  generic_fun_suffix),
	       called_vars @ [host]))
  in
  let fun_fullname = (fun_nms,fun_localname) in 
    mkfunction_def 
      (fun_fullname,
       vars,
       funsig,
       (EFunctionUser call_fun_expr),
      NonUpdating)
      
      

let make_functions_for_a_binding_and_a_port service_nms wsdl_mod service port binding =
  let port_type = get_port_type binding wsdl_mod in
    List.map
      (make_one_service_call_function service_nms wsdl_mod service port binding port_type)
      binding.binding_operations      

let make_functions_for_a_port service_nms wsdl_mod service port =
  let b = Wsdl_util.get_binding_for_a_port port wsdl_mod
  in
    (make_functions_for_a_binding_and_a_port
       service_nms
       wsdl_mod
       service
       port
       b,
       b)
    
    
let make_functions_for_a_service service_nms chosen_port wsdl_mod service = 
  let rec pick_a_port ps = (* try all ports, to see which one works *)
    match ps with
      | [] -> 
	  begin
	    match chosen_port with
	      | None -> None
	      | Some cp -> raise (Query 
				    (Load_Error
				       ("Port " ^ cp ^ " not found\n")))
	  end
      | p::portlist -> 
	  if is_the_right_port chosen_port p  then
	    Some (make_functions_for_a_port service_nms wsdl_mod service p)
	  else
	    pick_a_port portlist
	      (*
		with Query (Load_Error s) -> 
		prerr_endline s;
		pick_a_port portlist
	      *)
  in
    pick_a_port service.ports      
      
(** create all functions corresponding to a given service *)  
let make_service_functions service_nms chosen_service chosen_port wsdl_mod =   
  let rec make_functions = function
    | [] -> 
	begin
	  match chosen_service with
	    | None -> None
	    | Some cs -> raise (Query 
				  (Load_Error
				     ("Service " ^ cs ^ " not found\n")))
	end
    | serv::rest -> 
	if is_the_right_service chosen_service serv  then 
	  make_functions_for_a_service service_nms chosen_port wsdl_mod serv
	else
	  make_functions rest
  in
    make_functions wsdl_mod.wsdl_services 
      
let make_all_import_functions service_nms chosen_service chosen_port wsdl_mod = 
  (* first, build functions which call a specific URI *)
  let serv_funs, bindings = match
    make_service_functions service_nms 
      chosen_service chosen_port wsdl_mod with
	| None -> [],[]
	| Some (funs, b) -> funs, [b]
  in
    (* build `generic' functions with a URI as a parameter *)
  let binding_funs = make_binding_functions service_nms bindings wsdl_mod in
  List.rev_append binding_funs serv_funs
      
let wsdl_ast_to_xquery_ast_import service_nms wsdl_mod chosen_service chosen_port = 
  let prolog =
    { pprolog_xschemas = [make_schema wsdl_mod];
      pprolog_contexts = make_module_context_decls service_nms wsdl_mod;
      pprolog_funcvars = List.map (fun x->FunDef x) (make_all_import_functions
						       service_nms chosen_service chosen_port wsdl_mod);
      pprolog_indices = [] }
  in
  let library_module =
    { plibrary_module_decl = (service_nms, AnyURI._string_of_uri wsdl_mod.targetNamespace,None);
      plibrary_module_prolog = prolog }
  in
  library_module

let import_to_out_channel chan service_nms wsdl_mod chosen_service chosen_port = 
  let ff = Format.formatter_of_out_channel chan in
  let xqm = wsdl_ast_to_xquery_ast_import service_nms 
	      wsdl_mod chosen_service chosen_port 
  in
    Print_xquery.print_library_module ff xqm
(*    Print_common.print_literal Format.std_formatter (URILiteral (AnyURI._actual_uri_of_string "http://toto.com"))   *)

let wsdl_to_stdout = import_to_out_channel Pervasives.stdout

let xquery_import_from_wsdl_file wsdl_file nms = 
  let w_ast = Wsdl_load.test_parser wsdl_file in
    wsdl_to_stdout nms w_ast 


let wsdl_to_xqfile_import filename nms wmod serv port = 
  let can_out = open_out filename in
    import_to_out_channel can_out nms wmod serv port;
    close_out can_out
