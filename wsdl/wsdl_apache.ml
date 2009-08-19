(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: wsdl_apache.ml,v 1.25 2007/09/25 15:12:43 mff Exp $ *)

(** 
  @(#)wsdl_apache.ml
  
  Build a small server stub that is used by the Apache server
  to call the exported XQuery module.

  The interface of the exported module is a WSDL description.
  This had either been given or it had been generated and then passed to 
  this module.
  
  @author Nicola Onose
*)

open Error

open Namespace_names
open Namespace_builtin

open Datatypes

open Xquery_common_ast
open Xquery_ast
open Xquery_ast_util
open Xquery_type_ast_map
open Wsdl_ast
open Wsdl_util



let envelope_fun_name = "make_envelope"

(*
  let soapenv_element = fmkdatatype 
  (DTElemRef (Some (soapenv_prefix, "Envelope")), None) 
  Finfo.bogus
*)
		
let stringtype = fmksequencetype  
		   (ITAtomic (xs_prefix, "string"), 
		    None)
		   Finfo.bogus
		   
(* parameters used by the make_envelope function *)
let funname_param = "funname"
let nmsname_param = "nmsname"
let param_param = "param"
let funname_var = mkexpr (EVar (NSDefaultElementPrefix, funname_param))
let nmsname_var = mkexpr (EVar (NSDefaultElementPrefix, nmsname_param))
let param_var = mkexpr (EVar (NSDefaultElementPrefix, param_param))

  
(* number of parameters for a function *)
let input_parts_number pt_op wsdl_mod = 
  try 
    let m = get_input_message pt_op wsdl_mod in
      List.length m.message_parts
  with (Query(Load_Error _)) -> 0
    
    
let one_funcall fun_localname service wsdl_mod binding port_type op =
  (* input parameters are the arguments of the input message *)
  (* maybe we should also check the soapAction header *)
  let pt_op = get_operation_in_port_type port_type op in
  let nb_params = input_parts_number pt_op wsdl_mod in
  let params = 
    if nb_params = 0 then []
    else
      let param_arg = 
	mkexpr (EVar (NSDefaultElementPrefix, "param"))
      in
      let build_param i = 
	let p_i_arg = 
	  mkexpr (EPath (PAxis (Child, PNameTest (NSDefaultElementPrefix, "p"^(string_of_int i)))))
	in
	  mkexpr  (* $param/pi/node()/node() -- extract the content of the argument *)
	    (EPath 
	       (PSlash (
		mkexpr  
  		  (EPath 
		     (PSlash (mkexpr (EPath (PSlash (param_arg, p_i_arg))),
			      mkexpr (EPath (PAxis (Child, PNodeKindTest AnyKind)))))),
		mkexpr (EPath (PAxis (Child, PNodeKindTest AnyKind))))))
      in
      let rec build_p_list n plist = 
	if n=0 then plist 
	else build_p_list (n-1) ((build_param n)::plist)
      in  
	build_p_list nb_params []
  in
  let fun_nms = NSPrefix service.service_name in
  let fun_fullname = (fun_nms, fun_localname) in   
    mkexpr (EApp (fun_fullname, params))
      	  

let make_ifthenelse fun_localname service wsdl_mod binding port_type op otherfuns = 
  let cond = 
    mkexpr (EBinaryOp
	      (mkexpr (EBinaryOp( nmsname_var, BEEqual, mkexpr(EScalar (StringLiteral (AnyURI._string_of_uri wsdl_mod.targetNamespace))))),
	       BEAnd,    
	       mkexpr (EBinaryOp( funname_var, BEEqual, mkexpr(EScalar (StringLiteral fun_localname))))))
  in
  let funcall_expr = 
    mkexpr (EEnclosed
	      (one_funcall fun_localname service wsdl_mod binding port_type op))
  in
  let content = 
    if not (is_encoded_output op) then (* literal *)
      funcall_expr 
    else (* encoded *)
      let this_part = get_output_part port_type op wsdl_mod in
      let part_name = this_part.part_name in
      let type_annot = part_type_annot this_part wsdl_mod in
	mkexpr (EElemFixed ((NSDefaultElementPrefix, part_name), [type_annot], [funcall_expr]))
  in
    match  make_envelope_and_body
      (Wsdl_astutil.Output_msg)
      ([] , ([],None) , [content])
      wsdl_mod
      binding
      op
    with _,_,if_envelope ->
      mkexpr (EIf (cond, if_envelope, otherfuns))
        
      
let make_funcalls_for_a_binding service wsdl_mod binding = 
  let fault_message = 
    make_generic_fault wsdl_mod "Server" "Wrong function name" wsdl_mod.wsdl_name ""
  in
  let wrong_funname = make_generic_envelope (NSPrefix wsdl_mod.soapenv_prefix_name) [fault_message]
  in
  let port_type = get_port_type binding wsdl_mod in
    List.fold_left
      (fun otherfuns opbind -> make_ifthenelse
	 opbind.operation_in_binding_name service wsdl_mod binding port_type opbind otherfuns)
      wrong_funname
      (List.rev binding.binding_operations)
      

(**
  Generate the make_envelope function by using the functions defined above.
*)
let make_all_funcalls service_nms wsdl_mod service port =   
  let b = Wsdl_util.get_binding_for_a_port port wsdl_mod in		    
  let entire_body = 
    make_funcalls_for_a_binding service wsdl_mod b
  in
  let entire_function = 
    mkfunction_def 
      ((NSPrefix "local", envelope_fun_name),
       ([NSDefaultElementPrefix, nmsname_param;
	 NSDefaultElementPrefix, funname_param;			    
	 NSDefaultElementPrefix, param_param]),
       ([Some stringtype; Some stringtype; Some anytype], Some anytype),
       (EFunctionUser entire_body), NonUpdating)
  in 
    [entire_function]
		      


let get_service_and_port wsdl_mod chosen_service chosen_port =
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
      | None -> None
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
	      | None -> None
	      | Some port -> Some (srv, port)

let export_to_out_channel chan service_nms wsdl_mod chosen_service chosen_port xq_file_name xqs_file_name  = 
  match get_service_and_port wsdl_mod chosen_service chosen_port with
      None -> raise (Query (Load_Error "xquery2soap: Could not find a service or port. Giving up\n"))
    | Some (service,port) ->
	let ff = Format.formatter_of_out_channel chan in
	  (*  let _ = make_module_context_decls service_nms wsdl_mod ff in *)
	let server_uri = xqs_file_name ^ "#" ^ port.service_port_name in
	let module_uri = AnyURI._string_of_uri wsdl_mod.targetNamespace in
	let prolog =
	  { pprolog_xschemas = [];
	    pprolog_contexts = 
	      mkcontext_decl(EImportModuleDecl(service.service_name, module_uri, xq_file_name)) ::
		make_module_context_decls service_nms wsdl_mod;
	    pprolog_funcvars = 
	      List.map (fun x->FunDef x) (make_all_funcalls service_nms wsdl_mod service port);
	    pprolog_indices = [] }
	in
	let library_module =
	  { plibrary_module_decl = (service.service_name ^ "-server", server_uri, None);
	    plibrary_module_prolog = prolog }
	in
	let xmodule = ELibraryModule library_module in
	  Print_xquery.print_module ff xmodule
	    
(*
let xquery_export_from_wsdl_file wsdl_file nms = 
  let w_ast = Wsdl_load.test_parser wsdl_file in
    export_to_out_channel Pervasives.stdout nms w_ast
*)    

      
let wsdl2xq_server_source xq_file_name xqs_file_name nms wmod service port = 
(*  file_copy xq_file xqs_file; let can_out = open_out_gen [Open_append] 0o644 xqs_file in *)
  let can_out = open_out xqs_file_name in
    export_to_out_channel can_out nms wmod service port xq_file_name xqs_file_name;
    close_out can_out

