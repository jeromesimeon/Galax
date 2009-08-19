(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: wsdl_util.ml,v 1.30 2007/07/18 15:03:11 ndonose Exp $ *)


(** 
  @(#)wsdl_util.ml

  Module used by Wsdl_import, Wsdl_export and Wsdl_apache
  @author Nicola Onose
*)


(**

NOTE:
  Support for SOAP:Header elements can be added only after named typing
  and schema import are integrated. 
  --> see commented code of make_soap_headers
*)


open Error

open Namespace_names

open Datatypes

open Xquery_common_ast

open Xquery_ast
open Xquery_ast_util

open Xquery_type_ast
open Xquery_type_ast_map
open Xquery_type_core_ast

open Wsdl_ast
open Wsdl_astutil


type impex_fun =   
    Wsdl_ast.wsdl_module ->  
      Wsdl_ast.service_binding -> 
	Wsdl_ast.port_type -> 
	  Wsdl_ast.operation_in_binding -> 
	    (ncname list) * Xquery_ast.function_signature * (Xquery_ast.expr list)	
	

(* The first one is the one defined in the SOAP 1.1 specs
   The second is only for compatibility with WSDL 1.1 *)
let default_soap_encoding_uris =
  ["http://schemas.xmlsoap.org/soap/encoding/"; "encodingStyleDefault"]
				   
let encodingstyle_default = List.hd default_soap_encoding_uris

let default_soapenv_string = "http://schemas.xmlsoap.org/soap/envelope/" 
let default_soapenv_nms = Datatypes_util.anyURI_of_untyped 
			     default_soapenv_string			     

let default_soapenv_uri = Namespace_names.NSUri default_soapenv_string

let default_soapenv_prefix_name = "soapenv"    
(*	

	let soapenv_prefix = NSPrefix soapenv_prefix_name 
*)
		   
let default_xsi_string = "http://www.w3.org/2001/XMLSchema-instance"
let default_xsi_uri = Namespace_names.NSUri default_xsi_string
let default_xsi_prefix_name = "xsi"    


let default_xsd_string = "http://www.w3.org/2001/XMLSchema"
let default_xsd_uri = Namespace_names.NSUri default_xsd_string
let default_xsd_prefix_name = "xsd"    
		       
let wsdl_prefix_string = "wsdl"
let wsdl_prefix = NSPrefix wsdl_prefix_string
let wsdl_uri_string = "http://schemas.xmlsoap.org/wsdl/"
let wsdl_uri = NSUri wsdl_uri_string

let soap_prefix_string = "soap"
let soap_prefix = NSPrefix soap_prefix_string
let soap_uri_string = "http://schemas.xmlsoap.org/wsdl/soap/"
let soap_uri = NSUri soap_uri_string

let http_prefix_string = "http"
let http_prefix = NSPrefix http_prefix_string
let http_uri_string = "http://schemas.xmlsoap.org/wsdl/http/"
let http_uri = NSUri http_uri_string
		 

(*
  let is_in_targetNamespace uq wsdl_mod = 
  match uq with 
  | NSPrefix p, _ -> 
  (try 
  List.assoc p wsdl_mod.global_xmlns =  wsdl_mod.targetNamespace
  with Not_found -> false)
  | _ -> false
*)
      
(**
  checks whether the prefix of the given uqname b_uqname is the
  prefix of the targetNamespace of the document
  @return the local name from the uqname
  @throws Query(Load_Error _) if the namespace is different
  from the targetNamespace prefix
*)


let check_if_targetNamespace b_rqname wsdl_mod = 
  match b_rqname with	p, u, b_name -> 
    match u with
      | NSWildcardUri -> b_name
      | NSUri u_str ->
	  if u_str = AnyURI._string_of_uri wsdl_mod.targetNamespace
	  then b_name
	  else          
	    begin
	      print_endline u_str;
	      print_endline (AnyURI._string_of_uri wsdl_mod.targetNamespace);
	      raise (Query (Load_Error ("Qualified name " ^
					(prefixed_string_of_rqname b_rqname)
					^ " not in targetNamespace "
					  ^(AnyURI._string_of_uri wsdl_mod.targetNamespace)^"\n")))      
	    end
	    
	    
(**
  get the portType for a given binding
*)
      
let get_port_type binding wsdl_mod = 
  let type_name = check_if_targetNamespace 
		    binding.binding_type	    
		    wsdl_mod 
  in
    try 
      List.find 
	(fun x -> x.port_type_name = type_name) 
	wsdl_mod.wsdl_port_types
    with Not_found ->
      raise (Query 
	       (Load_Error
		  ("PortType " ^ type_name ^" not found\n")))



(**
  get the operation in a portType corresponding to 
  the description of an operation in the binding
*)  

let get_operation_in_port_type port_type bind_operation = 
  let opname = bind_operation.operation_in_binding_name in    
    try 
      List.find 
	(fun x -> x.operation_name = opname) 
	port_type.operations
    with Not_found ->
      raise (Query 
	       (Load_Error
		  ("Operation " ^ opname ^" not found")))
      


(** get a certain type of message(s) (input|output|list of faults)
  corresponding to a defined operation *)

let get_output_message pt_op wsdl_mod =
  match pt_op.output with
    | None ->  (* should we also allow operations with no return value (procedures) ? *)
	raise (Query (* we should get a fault message here, if there is a specific fault message defined *)
		 (Load_Error
		    ("Operation " ^ pt_op.operation_name ^
		     " has no output message defined")))
    | Some op_msg ->
	let output_name = check_if_targetNamespace 
			   op_msg
			   wsdl_mod 
	in
	  try 
	    List.find 
	      (fun x -> x.message_name = output_name)
	      wsdl_mod.wsdl_messages 
	  with Not_found ->
	    raise (Query 
		     (Load_Error
			("Output message " ^ output_name ^ " defined in operation " ^
			 pt_op.operation_name ^ " not found")))
	    
	    
let get_input_message pt_op wsdl_mod =
  match pt_op.input with
    | None -> 
	raise (Query 
		 (Load_Error
		    ("Operation " ^ pt_op.operation_name ^
		     " has no input message defined")))
    | Some op_msg ->
	let input_name = check_if_targetNamespace 
			   op_msg
			   wsdl_mod 
	in
	  try 
	    List.find 
	      (fun x -> x.message_name = input_name)
	      wsdl_mod.wsdl_messages 
	  with Not_found ->
	    raise (Query 
		     (Load_Error
			("Input message " ^ input_name ^ " defined in operation " ^
			 pt_op.operation_name ^ " not found")))
	    
	    
(** 
  One portType can have several fault messages, 
  not just one, like for input or output messages. 
*)
let get_fault_messages pt_op wsdl_mod =
  let rec fault_list msgs = function
    | [] -> msgs
    | op_msg::rest ->
	let fault_name = check_if_targetNamespace 
			   op_msg
			   wsdl_mod 
	in
	  try 
	    let m = 
	      List.find 
		(fun x -> x.message_name = fault_name)
		wsdl_mod.wsdl_messages
	    in
	      fault_list (m::msgs) rest
	  with Not_found ->
	    raise (Query 
		     (Load_Error
			("Fault message " ^ fault_name ^ " defined in operation " ^
			 pt_op.operation_name ^ " not found")))
  in
    fault_list [] pt_op.faults
      


let consider_body_as_encoded sb = 
  match sb.use with
    | Some Encoded -> true
    | Some Literal -> false
    | None -> true


let is_encoded_output opbind =   
    match opbind.binding_output with
      | Some (Some sb, _, _) -> consider_body_as_encoded sb
      | _ -> true


let is_encoded_input opbind =   
    match opbind.binding_input with
      | Some (Some sb, _, _) -> consider_body_as_encoded sb
      | _ -> true


let get_parts get_message port_type op wsdl_mod = 	  
  let op1 = get_operation_in_port_type port_type op in
  let msg = get_message op1 wsdl_mod in	
    msg.message_parts
      
      
let get_input_parts = get_parts get_input_message 

let get_output_part port_type op wsdl_mod = 
  let parts = get_parts get_output_message port_type op wsdl_mod
  in List.hd parts


let output_part_test port_type op wsdl_mod = 
  let this_part = get_output_part port_type op wsdl_mod  in
    mkexpr (EPath (PAxis (Child, PNameTest (NSDefaultElementPrefix, this_part.part_name))))    



let part_type_annot this_part wsdl_mod = match this_part.part_type with
    PartType (_, ptype) ->
      mkexpr (EAttrFixed ((NSPrefix wsdl_mod.xsi_prefix_name, "type"),
			  [mkexpr (EScalar (StringLiteral (string_of_uqname ptype)))]))
  | PartElement (_, pelem) ->
      mkexpr (EAttrFixed ((NSPrefix wsdl_mod.xsi_prefix_name, "element"),
			  [mkexpr (EScalar (StringLiteral (string_of_uqname pelem)))]))
      
(** Map the given type or element according to the schema, if there is one.
  If there is no schema, look to see if it is a builtin type. *)
      
let map_wsdl_type schema =  
  let noschema ((pref,u,name) as rq) =
    try 
(*      let _ = Datatypes_util.lookup_bltin_type (Namespace_symbols.rtype_symbol (pref,u,name)) *)
      let rsymbol = Namespace_symbols.rtype_symbol rq in
      let _ = List.find (fun d -> Namespace_symbols.rtype_equal rsymbol d.ctypedecl_name) 
      in  Xquery_ast_util.mksequencetype (ITAtomic (pref,name), None)
(*    with Query (Mapping_Failure s) -> *)
    with Not_found ->
      raise (Query (Mapping_Failure 
		      ("Failure while trying to validate type " 
		       ^ quoted_uri_string_of_rqname rq)))
      
  in  
    function
      | PartElement (u, (pref,name)) -> 
	  begin
	    match schema with
	      | None -> noschema (pref,u,name)
	      | Some sch -> 
		  match Xquery_type_ast_map.xquery_element_from_wsdl_element (pref,u,name) sch with
		    | None -> raise (Query (Mapping_Failure 
					      ("No schema mapping for the element " 
					       ^ prefixed_string_of_rqname (pref,u,name))))
		    | Some seqtype -> seqtype
	  end
      | PartType (u, (pref,name)) ->
	  begin
	    match schema with
	      | None ->   noschema (pref,u,name)
	      | Some sch -> 
		  match Xquery_type_ast_map.xquery_type_from_wsdl_type (pref,u,name) sch with
		    | None -> raise (Query (Mapping_Failure 
					      ("No schema mapping for the type " 
					       ^ prefixed_string_of_rqname (pref,u,name))))
		    | Some seqtype -> seqtype
	  end;;


let make_args get_message wsdl_mod port_type op_in_binding = 
  let make_var_name part = part.part_name in
  let pt_op = get_operation_in_port_type port_type op_in_binding in
  let inp = get_message pt_op wsdl_mod in
  let msgout = get_output_message pt_op wsdl_mod in
  let pout = List.hd msgout.message_parts in
  let ret = map_wsdl_type wsdl_mod.wsdl_types pout.part_type in 		  
  let rec make_rec vars types = function
      | [] -> vars, (types, Some ret)
      | part::rest ->
	  let v = make_var_name part in
	  let t = map_wsdl_type wsdl_mod.wsdl_types part.part_type in
	    make_rec (v::vars) ((Some t)::types) rest
  in
    make_rec [] [] inp.message_parts 

      
let make_import_args wsdl_mod port_type op_in_binding =
  make_args get_input_message wsdl_mod port_type op_in_binding 



(**
  Creates the list of elements containing the formal 
  parameters of the function.

  @return a tuple formed by the message corresponding 
  to the input/output of a given operation pt_op and a list
  of strings containing the names of the variables that
  should be the parameters of the function
*)      
let build_params_elems wsdl_mod msg op_in_binding = 
  let make_var_name part = part.part_name in
  let consider_body_as_encoded sb = 
    match sb.use with
      | Some Encoded -> true
      | Some Literal -> false
      | None -> true
  in
  let should_build_var_wrappers =     
    match get_soap_body op_in_binding with
      | Some soapb -> 
	  if consider_body_as_encoded soapb then	    
	    begin
	      match soapb.soap_body_encodingStyle with
		| None -> true
		| Some style -> 
		    try 
		      let _ = List.find
				(fun x -> x = AnyURI._string_of_uri style) 
				default_soap_encoding_uris
		      in true
		    with Not_found -> false
	    end	    
	  else false
      | None -> true
  in
  let rec build_args vars types elems = function
    | [] -> vars, types, elems
    | part::rest ->
	let v = make_var_name part in
	let t = map_wsdl_type wsdl_mod.wsdl_types part.part_type in
	let e = 	
	  let vexpr = mkexpr (EVar (NSDefaultElementPrefix, v))
	  in
	  let varexpr = 
	    mkexpr (EEnclosed vexpr)
	  in
	    if should_build_var_wrappers then (* usually, a RPC approach *)
	      let attr = 
		if is_encoded_input op_in_binding then
		  [part_type_annot part wsdl_mod]
		else []
	      in
		mkexpr
		  (EElemFixed ((NSDefaultElementPrefix, part.part_name),
			     attr, [varexpr]))
	    else (* usually, when use=literal inside the soap:body *)
	      varexpr
	in
	  build_args (v::vars) ((Some t)::types) (e::elems) rest
  in
    build_args [] [] [] msg.message_parts


(** apply the previous function to different types of messages *)

let impex_params_elems get_message wsdl_mod port_type op_in_binding =
  let pt_op = get_operation_in_port_type port_type op_in_binding in
  let msg = get_message pt_op wsdl_mod in  
  let v,t,e = build_params_elems wsdl_mod msg op_in_binding in
  let msgout = get_output_message pt_op wsdl_mod in
    if 1 <> (List.length msgout.message_parts) then
      raise (Query 
	       (Load_Error
		  (pt_op.operation_name ^
		   " PortType operation: Multi-part output messages not supported")))
    else
      let pout = List.hd msgout.message_parts 
      in
      let ret = map_wsdl_type wsdl_mod.wsdl_types pout.part_type 
      in v,(t,Some ret),e
	  

(** utilities for building attributes *)
let build_attribute constructor prefix localname att_value = 
  mkexpr (EAttrFixed 
	    ((prefix, localname),
	     [constructor att_value]))  
    

let make_untyped_expression str_value = 
  mkexpr (EText str_value)
(*    (EScalar (StringLiteral str_value)) *)

let build_untyped_attribute = 
  build_attribute make_untyped_expression


let make_uri_expression uri_value = 
  mkexpr
    (EScalar (URILiteral uri_value)) 

let build_uri_attribute =
  build_attribute make_uri_expression



let build_ns_attribute prefix uri_value =
  build_untyped_attribute 
    (NSPrefix "xmlns") 
    prefix
    (AnyURI._string_of_uri uri_value)
    


(*
let make_soap_headers op_in_binding msgtype soapenv_prefix = 
  let rec make_one_by_one hlist = function
    | [] -> hlist
    | h, rest ->
	mkexpr
	(EElemFixed ( (NSPrefix soapenv_prefix,"Header"),
		      ??,
		      ??
	::
	(make_one_by_one hlist rest)
  in
  let hds = 
    match msgtype with
      | Input_msg -> 
	  match op_in_binding.binding_input with
	      _, sh, _ -> sh
      | Output_msg ->
	  match op_in_binding.binding_output with
	      _, sh, _ -> sh
      | Fault_msg _ -> []
  in
    make_one_by_one [] hds
*)
  

let make_generic_fault wsdl_mod faultcode faultstring faultactor detail = 
  let code_elem = 
    mkexpr
      (EElemFixed ((NSDefaultElementPrefix, "faultcode"),
		   [], [mkexpr (EText faultcode)]))
  in    
  let string_elem = 
    mkexpr
      (EElemFixed ((NSDefaultElementPrefix, "faultstring"),
		   [], [mkexpr (EText faultstring)]))
  in    
  let actor_elem = 
    mkexpr
      (EElemFixed ((NSDefaultElementPrefix, "faultactor"),
		   [], [mkexpr (EText faultactor)]))
  in    
  let detail_elem = 
    mkexpr
      (EElemFixed ((NSDefaultElementPrefix, "detail"),
		   [], [mkexpr (EText detail)]))
  in    
    mkexpr
      (EElemFixed ((NSPrefix wsdl_mod.soapenv_prefix_name,"Fault"), 
		   [], 
		   [code_elem; string_elem; actor_elem; detail_elem]))
  


let make_generic_envelope soapenv_pref content = 
  let soap_body_elem = EElemFixed ((soapenv_pref, "Body"),
				   [], 
				   content) in    
    mkexpr
      (EElemFixed ( (soapenv_pref, "Envelope"),
		    [build_ns_attribute (string_of_prefix soapenv_pref) default_soapenv_nms],
		    [mkexpr soap_body_elem])) 
      

let get_prefix_strings d_bindings =
  List.fold_left (fun string_list nms -> match nms with 
		    | NSPrefix s,_ -> s::string_list
		    | _ -> string_list)
    [] d_bindings
    
(**
  create the `Envelope' element with a descendant called 
  `Body' which contains the elements associated to the parameters
*)

let make_envelope_and_body 
  msgtype
  (vars,funsig,params_elems) 
  wsdl_mod binding op_in_binding =

  let operation_elems = 
    if Wsdl_astutil.is_an_rpc_binding binding
      || Wsdl_astutil.is_an_rpc_operation op_in_binding 
    then (* 'rpc' soap:binding *)
      let funbody_pref_string, nms = Wsdl_astutil.get_soap_body_nms_prefix 
				       wsdl_mod
				       op_in_binding 
      in
      let funbody_pref, ns_attr = 
	let pref_str = match funbody_pref_string with
	  | Some s -> s
	  | None -> create_new_var (get_prefix_strings wsdl_mod.global_xmlns) "xns"
	in
	  NSPrefix pref_str,
	  build_ns_attribute pref_str nms
      in
      let attribs = 
	match Wsdl_astutil.get_encoding_style op_in_binding binding msgtype with
	  | None -> [ns_attr]
	  | Some enc -> 
	      let e_attr =
		build_untyped_attribute
		  (NSPrefix wsdl_mod.soapenv_prefix_name)
		  "encodingStyle"
		  (AnyURI._string_of_uri enc)
	      in
		[ns_attr; e_attr]
      in	
      let op_element = 
	mkexpr
	  (EElemFixed ( ( funbody_pref,
			  op_in_binding.operation_in_binding_name),
			attribs,
			params_elems))
      in
	[op_element]
    else     (* 'document' or undefined soap:binding *)
      params_elems
  in
    vars,  
    funsig,
    make_generic_envelope (NSPrefix wsdl_mod.soapenv_prefix_name) operation_elems
      
      
      
let make_envelope_and_body_impex msgtype (get_params:impex_fun) wsdl_mod binding port_type op_in_binding =  
  let vars_params = get_params
		      wsdl_mod
		      binding
		      port_type
		      op_in_binding			      
  in
    make_envelope_and_body msgtype vars_params wsdl_mod binding op_in_binding 
      

let make_xschema_name wsdl_mod = 
  (wsdl_mod.wsdl_name)^".schema"
  
  
let nms_string_of_uri = function
    NSWildcardUri -> "*"
  | NSUri s -> s


let get_schema_target_nms wsdl_mod schm = 
  let rec search_target = function
    | x::decls -> 
	(match x with
	   | ("targetNamespace", u) -> u
	   | _ -> search_target decls)
    | [] ->  raise (Query 
		      (Load_Error
			 ("WSDL types in the " ^ wsdl_mod.wsdl_name ^
			  " module contains a schema with no targetNamespace")))
  in search_target schm.xschema_namespace_declarations 
	    

let get_schema_nms_decls wsdl_mod =
  match wsdl_mod.wsdl_types with
    | None -> []
    | Some sch -> 
	List.map (fun x -> mkcontext_decl (ENamespaceDecl x)) sch.xschema_namespace_declarations


(**
  * Retrieve the list of declarations for the prolog of the XQuery AST module.
  *
  *
**)

let make_module_context_decls service_nms wsdl_mod = 
(*
  let prefixes = 
    List.filter 
      (fun (x,y) -> not (string_of_prefix x =""))
      wsdl_mod.global_xmlns
  in *)
    mkcontext_decl (ENamespaceDecl (wsdl_mod.soapenv_prefix_name, default_soapenv_uri))  ::
      (mkcontext_decl (ENamespaceDecl (wsdl_mod.xsi_prefix_name, default_xsi_uri))) :: 
      (mkcontext_decl (ENamespaceDecl (wsdl_mod.xsd_prefix_name, default_xsd_uri)) :: get_schema_nms_decls wsdl_mod)
      (*
	mkcontext_decl (ENamespaceDecl (service_nms,
	(Namespace.NSUri (AnyURI._string_of_uri wsdl_mod.targetNamespace)))) ::
	(List.map 
	(fun (x,y) -> mkcontext_decl (ENamespaceDecl (Namespace.string_of_prefix x, Namespace.NSUri (nms_string_of_uri y)))) 
	prefixes) @
      *)


	  
let is_the_right_port p_name p1  = 
  match p_name with
    | None -> true
    | Some s -> p1.service_port_name = s


let is_the_right_service s_name s1  = 
  match s_name with
    | None -> true
    | Some s -> s1.service_name = s


let get_binding_for_a_port port wsdl_mod = 
  let b_uqname = port.service_port_binding in
  let b_name = check_if_targetNamespace b_uqname wsdl_mod in
    try 
      List.find 
	(fun x -> x.binding_name = b_name) 
	wsdl_mod.wsdl_bindings
    with Not_found -> 
      raise (Query 
	       (Load_Error
		    ("Binding " ^ b_name ^" not found")))
      

let string_of_stringoption = function
  | None -> ""
  | Some s -> s


let rec get_all_type_declarations wsdl_mod schm =
  List.iter 
    (fun (name,uri) -> wsdl_mod.global_xmlns <- (NSPrefix name, uri)::wsdl_mod.global_xmlns)
    schm.xschema_namespace_declarations;
  List.fold_left (fun decls schema -> decls @ (get_all_type_declarations wsdl_mod schema))
    schm.xschema_type_declarations 
    schm.xschema_imported_schemas						      
    
(**
  retrieves the schema(s) declarations and has as side effect to add
  the namespace declarations from the schema(s) to wsdl_mod
*)
let make_type_declarations wsdl_mod =
  match wsdl_mod.wsdl_types with
    | None -> []
    | Some schm -> 
	get_all_type_declarations wsdl_mod schm
	
	
let make_schema wsdl_mod =
  Xquery_type_ast_util.fmkschema [] [] (make_type_declarations wsdl_mod)

