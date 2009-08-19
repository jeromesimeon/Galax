(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: norm_ident_top.ml,v 1.37 2008/03/21 19:02:32 simeon Exp $ *)

(* Module: Norm_ident_top
   Description:
     This module implements an identity normalization for XQuery
     modules which are already in the core.
*)

open Format

open Error

open Namespace_names
open Namespace_builtin
open Namespace_symbols
open Namespace_resolve

open Datatypes
open Datatypes_util

open Xquery_common_ast
open Xquery_common_ast_util

open Xquery_ast
open Xquery_ast_util

open Xquery_type_ast
open Xquery_type_ast_util

open Xquery_core_ast
open Xquery_core_ast_annotation
open Xquery_core_ast_util

open Norm_context
open Norm_util
open Norm_ident_expr


(****************************************************)
(* functions to map each components in query_module *)
(****************************************************)

(* Global variable declaration *)

let normalize_ident_variable norm_context v =
  let nsenv = nsenv_from_norm_context norm_context in 
  let var_desc =
    match v.pvar_decl_desc with
    | (vname,dt,EVarUser e) ->
	let cvname = resolve_variable_qname nsenv vname in
	let (_, ce) = normalize_ident_expr norm_context e in
	let cdt = normalize_optional_sequencetype norm_context dt in
	(cvname, cdt, CEVarUser ce)
    | (vname,dt,var_body) ->
	let cvname = resolve_variable_qname nsenv vname in
	let cdt = normalize_optional_sequencetype norm_context dt in
	let cevar_body =
	  if (var_body = EVarExternal) then CEVarExternal
	  else CEVarInterface
	in
	(cvname, cdt, cevar_body)
  in
  fmkcvar_decl var_desc v.pvar_decl_loc

(* Index definition *)

let normalize_ident_index norm_context v =
  let nsenv = nsenv_from_norm_context norm_context in
  let index_def =
    match v.pindex_def_desc with
    | ValueIndex (kn,e1,e2) ->
	let (_, ce1) = normalize_ident_expr norm_context e1 in
	let (_, ce2) = normalize_ident_expr norm_context e2 in
	CValueIndex (kn, ce1, ce2)
    | NameIndex ename ->
	let cename = resolve_element_qname nsenv ename in
	CNameIndex cename
  in
  fmkcindex_def index_def v.pindex_def_loc

(* Function Body *)

let normalize_ident_body norm_context b output_dt =
  match b with
  | EFunctionInterface ->
      CEFunctionInterface
  | EFunctionImported ->
      CEFunctionImported
  | EFunctionBltIn ->
      CEFunctionBltIn
  | EFunctionUser expr ->
      let (_, ce) = (normalize_ident_expr norm_context expr) in
      let fi = (expr.pexpr_loc) in
      let (v1, var1) = gen_new_cvar norm_context (Some expr) fi in
      (* Apply rules for convert output value of function to simple value, if necessary *)
      let ce' = convert_function_input_output norm_context ((v1, var1), (ce, output_dt)) var1 in 
      (* Type match checks that result of evaluating function body matches declared return type *)
      (CEFunctionUser ce')

(* Function Definition *)

let normalize_ident_function_signature norm_context f = 
  let nsenv = nsenv_from_norm_context norm_context in 
  let (fname, vars, (input_types, return_type), fun_body, upd) = f.pfunction_def_desc in
  let rfname = resolve_function_qname nsenv fname in
  let csign =
    (List.map
       (fun dt ->
	 (normalize_optional_sequencetype_strong norm_context dt))
       input_types,
     (normalize_optional_sequencetype_strong norm_context return_type)) in
  let arity = List.length (fst csign) in
  ((rfname, arity), (rfname, csign, Norm_util.map_fun_kind fun_body, upd))

let normalize_ident_function_signatures norm_context fs =
  let fs = Xquery_ast_util.get_functions fs in
  List.map (normalize_ident_function_signature norm_context) fs

let normalize_ident_function_body norm_context f = 
  let nsenv = nsenv_from_norm_context norm_context in
  let (fname, vnames, sign, body, is_upd) = f.pfunction_def_desc in
  let rfname = resolve_function_qname nsenv fname in
  let (prefix,uri,_) = rfname in
  let fi = f.pfunction_loc in
  (* It is a static error to define a user-defined function without a
     namespace prefix or in a predefined namespace other than local:
     *)
  let _ = 
    match body with 
    | EFunctionUser _ -> 
	begin
	  match prefix with
	  | NSPrefix ncname ->
	      if (is_predefined_namespace uri && not(uri = local_uri)) then
		raise (error_with_file_location fi (Query(Static_Error("Cannot declare function "^(string_of_uqname fname)^" in predefined namespace "^(quoted_string_of_uri uri)^". Use 'local:'."))))
	  | _ -> raise (error_with_file_location fi (Query(Static_Error("Prefix of function name"^(string_of_uqname fname)^" must not be empty.  Use 'local:'."))))
	end
    | _ -> () in
  let arity = List.length (fst sign) in
  let (input_types, output_type), fun_kind, upd = one_sig_from_norm_context norm_context (rfname, arity) in
  let cvnames = List.map (fun vname -> resolve_variable_qname nsenv vname) vnames in
  let cbody = normalize_ident_body norm_context body output_type in
  fmkcfunction_def ((rfname,arity), cvnames, (input_types, output_type), cbody, upd) f.pfunction_loc 


(**************)
(* Statements *)
(**************)

let normalize_ident_statement norm_context statement =
  let (_, ce) = normalize_ident_expr norm_context statement
  in ce

let normalize_ident_statement_list norm_context ss = 
  List.map (normalize_ident_statement norm_context) ss


(*******************************)
(* Mapping context of a module *)
(*******************************)

(* Note:
     normalize_ident_context takes a context and an expression map the expression
     and return the updated context
   - Jerome *)

(* HACK! -- See comment to Vladimir *)
let rec filter1_ns_decls ns_decls =
  match ns_decls with
  | [] -> []
  | (pref,uri) :: ns_decls -> (NSPrefix pref,uri) :: (filter1_ns_decls ns_decls)

let rec filter2_ns_decls ns_decls =
  match ns_decls with
  | [] -> []
  | (NSPrefix pref,uri) :: ns_decls -> (pref,uri) :: (filter2_ns_decls ns_decls)
  | _ :: ns_decls -> (filter2_ns_decls ns_decls)

let normalize_ident_context_decl interface_table norm_context (ns_decls, schemas, cinterfaces) c =
  let proc_ctxt = (processing_context_from_norm_context norm_context) in
  let mod_proc_ctxt = (module_context_from_norm_context norm_context) in
  let fi = c.pcontext_decl_loc in
  try
    match c.pcontext_decl_desc with
    | EBaseURIDecl base_uri_string ->
	let base_uri = AnyURI._kinda_uri_of_string base_uri_string in
	begin
	  Processing_context.set_base_uri mod_proc_ctxt (Some base_uri);
	  (ns_decls, schemas, cinterfaces)
	end
    | ENamespaceDecl (ncn, uri) ->
	if (uri = (NSUri ("http://www.w3.org/XML/1998/namespace"))) || (ncn = "xml") || (ncn = "xmlns")
	then raise (Query (Namespace_Error "[err:XQST0070] Namespace declaration redefines the xml or xmlns namespace"));
	let new_binding = (NSPrefix ncn,uri) in
	(ns_decls@[new_binding], schemas, cinterfaces)
    | EDefaultElementNamespaceDecl uri ->
	let new_binding = (NSDefaultElementPrefix,uri) in
	(ns_decls@[new_binding], schemas, cinterfaces)
    | EDefaultFunctionNamespaceDecl uri ->
	let new_binding = (NSDefaultFunctionPrefix,uri) in
	(ns_decls@[new_binding], schemas, cinterfaces)
    | ESchemaDecl (prefix_option, target_uri, loc_hint_option) ->
	let xschema = Schema_import.import_schema proc_ctxt prefix_option target_uri loc_hint_option in
	let new_schemas = schemas@[xschema] in
	begin
	  match prefix_option with
	  | None ->
	      (ns_decls, new_schemas, cinterfaces)
	  | Some (NSPrefix ncn) -> 
	      let new_binding = ((NSPrefix ncn),NSUri target_uri) in
	      (ns_decls@[new_binding], new_schemas, cinterfaces)
	  | Some NSDefaultElementPrefix ->
	      let new_binding = (NSDefaultElementPrefix,NSUri target_uri) in
	      (ns_decls@[new_binding], new_schemas, cinterfaces)
	  | Some _ ->
	      raise (Query (Namespace_Error "Bogus namespace prefix in schema import AST"))
	end
    | EImportServiceDecl (ncn, wsdl_uri, wsdl_file) ->
	raise (Query(Prototype("Import WSDL Service not implemented in Norm_top.preprocess_prolog")))

	  (* This code does not align with norm_top! *)
    | EImportInterfaceDecl (ncn, uri, file)  ->
	begin
	(* Interface and Module imports both return Core interfaces *)
	  try 
	    let (_,_, _) = Hashtbl.find interface_table (NSInterfacePrefix "", uri) in 
	    let new_binding = (NSInterfacePrefix ncn, NSUri uri) in
	    (ns_decls@[new_binding], schemas, cinterfaces)
	  with
	  | Not_found -> raise (Query(Internal_Error("Module Interface '"^uri^"' not defined")))
	end
    | EImportModuleDecl (ncn, uri, file) ->
	begin
	(* Interface and Module imports both return Core interfaces *)
	  try 
	    let (modloc, norm_ctxt', cinterface) = Hashtbl.find interface_table (NSPrefix "", uri) in 
	    let new_binding = (NSPrefix ncn, NSUri uri) in
	    (ns_decls@[new_binding], schemas, cinterfaces @ [(norm_ctxt', cinterface)])
	  with
	  | Not_found -> raise (Query(Internal_Error("Module Interface '"^uri^"' not defined")))
	end
    (* New declarations are ignored for now - Jerome *)
    | EXmlSpaceDecl strip_or_preserve ->
	Processing_context.set_boundary_space_kind mod_proc_ctxt strip_or_preserve;
	(ns_decls, schemas, cinterfaces)
    | EDefaultCollationDecl uri ->
	Processing_context.set_default_collation mod_proc_ctxt uri;
	(ns_decls, schemas, cinterfaces)
    | EConstructionDecl strip_or_preserve ->
	Processing_context.set_construction_kind mod_proc_ctxt strip_or_preserve;
	(ns_decls, schemas, cinterfaces)
    | EOrderingDecl ordered_or_unordered ->
	Processing_context.set_ordering_kind mod_proc_ctxt ordered_or_unordered;
	(ns_decls, schemas, cinterfaces)
    | EDefaultEmptyOrderDecl emptysortkind ->
	Processing_context.set_default_order_kind mod_proc_ctxt emptysortkind;
	(ns_decls, schemas, cinterfaces)
    | ECopyNamespacesDecl (preserve_or_no_preserve,inherit_or_no_inherit) ->
	Processing_context.set_ns_preserve_kind mod_proc_ctxt preserve_or_no_preserve;
	Processing_context.set_ns_inherit_kind mod_proc_ctxt inherit_or_no_inherit;
	(ns_decls, schemas, cinterfaces)
  with
  | exn -> raise (error_with_file_location fi exn)

let normalize_ident_context_with_env interface_table norm_context cdl =
  List.fold_left (normalize_ident_context_decl interface_table norm_context) ([], [], []) cdl

let normalize_ident_funvar norm_context (fs,vs) fv =
  match fv with
  | OptionDecl _ ->
      raise (Query (Malformed_Core_Expr "Expression is not normalized (found 'option declaration')"))
  | FunDef f -> ((normalize_ident_function_body norm_context f) :: fs,vs)
  | VarDef v -> (fs,(normalize_ident_variable norm_context v) :: vs)
  | ServerDef s -> raise (Query(Prototype "Global server declarations not handled in Norm_ident_top"))

let normalize_ident_funvars norm_context fvs =
  List.fold_left (fun x -> normalize_ident_funvar norm_context x) ([],[]) fvs

(****************************************************************)
(* Toplevel functions to map the Full XQuery to the core XQuery *)
(****************************************************************)

(* Prolog *)

let normalize_ident_prolog (interface_table, norm_context) prolog =
  try
    let nsenv = nsenv_from_norm_context norm_context in
    begin
      (* 1. Normalize the context *)
      let { pprolog_xschemas = inlined_xschemas;
	    pprolog_contexts = ns;
	    pprolog_funcvars = fvs;
	    pprolog_indices = ks } = prolog
      in
      let (ns_decls, imported_xschemas, imported_interfaces) = 
	normalize_ident_context_with_env interface_table norm_context ns in
      (* Any redeclarations of namespaces that are inconsistent should raise an error *)
      let nsenv2 = Namespace_context.add_all_ns_xquery nsenv ns_decls in
      (* 2. Processing ISSDS *)
      (* Note:
         The namespace environment is not quite correct here!
         We will need to revisit this.
	 - Jerome *)
      let imported_cxschemas =
	List.map (Schema_norm.normalize Namespace_context.default_xml_nsenv) imported_xschemas
      in
      let inlined_cxschemas = List.map (Schema_norm.normalize nsenv2) inlined_xschemas in
      let all_cxschemas = imported_cxschemas @ inlined_cxschemas in
      let complete_cxschema = Xquery_type_core_ast_util.merge_cxschemas all_cxschemas in
      (* 4. Builds the new normalization context *)
      let norm_context' = copy_norm_context_with_sigs norm_context nsenv2 complete_cxschema in
      let sigs' = normalize_ident_function_signatures norm_context' fvs in
      let norm_context'' = add_sigs_to_norm_context norm_context' sigs' in
      (* 5. map all function bodies: *)
      let ((functions,vars)) = normalize_ident_funvars norm_context'' fvs in
      let (indices) = List.map (normalize_ident_index norm_context'') ks in
      (* 6. Add all the interfaces' functions and variables *)
      List.iter (fun (nc, ci) -> merge_imported_norm_context norm_context'' nc) imported_interfaces;
      let imported_functions = (List.concat(List.map (fun (nc, ci) -> ci.pcprolog_functions) imported_interfaces)) in
      let imported_vars = (List.concat(List.map (fun (nc, ci) -> ci.pcprolog_vars) imported_interfaces)) in
      let cprolog =
	{ pcprolog_functions = imported_functions @ functions;
	  pcprolog_vars      = imported_vars @ vars;
	  pcprolog_servers   = [];
	  pcprolog_indices   = indices }
      in
      (norm_context'', cprolog)
    end
  with
  | exn -> raise (Query(Static_Internal(bprintf_error "In normalize_ident_prolog " exn)))


(* Library module *)

let normalize_ident_library_module (interface_table, norm_context) q =
  try
    (* 1. Get the original query prolog *)
    let prolog = q.plibrary_module_prolog in
    (* 2. Get the module target namespace *)
    let (pref,uri,optint) = q.plibrary_module_decl in
    (* 3. Extend the prolog with the corresponding namespace declaration *)
    let new_decl = fmkcontext_decl (ENamespaceDecl (pref,NSUri uri)) Finfo.bogus in
    let patched_prolog =
      { pprolog_xschemas 	= prolog.pprolog_xschemas;
	pprolog_contexts 	= new_decl :: prolog.pprolog_contexts;
	pprolog_funcvars        = prolog.pprolog_funcvars;
	pprolog_indices  	= prolog.pprolog_indices }
    in
    (* 4. Normalize the prolog *)
    let (norm_context',cprolog) = normalize_ident_prolog (interface_table, norm_context) patched_prolog in
    (* 6. Build the resulting core module *)
    let cmodule =
      { pcmodule_prolog = cprolog;
	pcmodule_statements = [] }
    in
    (norm_context', cmodule)
  with
  | exn -> raise (Query(Static_Internal(bprintf_error "In normalize_ident_library_module " exn)))

(* Interface *)
let normalize_ident_interface interface_table proc_ctxt interface =
  try
    let norm_context = default_norm_context proc_ctxt in 
    let { pinterface_decl = decl ;
	  pinterface_prolog = iprolog; } = interface
    in
    let prolog = 
    { pprolog_xschemas 	= iprolog.iprolog_xschemas;
      pprolog_contexts 	= iprolog.iprolog_contexts;
      pprolog_funcvars  =  iprolog.iprolog_funcvars;
      pprolog_indices  	= [] }
    in
    normalize_ident_prolog (interface_table, norm_context) prolog 
  with
  | exn -> raise (Query(Static_Internal(bprintf_error "In normalize_ident_interface " exn)))

(* Main module *)

let normalize_ident_main_module (interface_table, norm_context) q = 
  try
    (* 1. Get the original query prolog *)
    let prolog = q.pmain_module_prolog in
    (* 2. Normalize the prolog *)
    let (norm_context',cprolog) = normalize_ident_prolog (interface_table, norm_context) prolog in
    if (Debug.default_debug()) then 
      Print_top.printf_cxschema "Input Schema :" (cxschema_from_norm_context norm_context') else (); 
    (* 3. Normalize the statements *)
    let es = q.pmain_module_statements in
    let (ces) = normalize_ident_statement_list norm_context' es in
    (* 4. Extend prolog with generated functions: Not necessary here *)  
    (* let cprolog' = {cprolog with pcprolog_functions = List.rev_append fwhile_funs cprolog.pcprolog_functions} in *)
    (* 5. Build the main module *)  
    let cmodule =
      { pcmodule_prolog = cprolog;
        pcmodule_statements = ces }
    in
    (norm_context', cmodule)
  with
  | exn -> raise (Query(Static_Internal(bprintf_error "In normalize_ident_main_module " exn)))
        

(* Module *)

let normalize_ident_module (interface_table, norm_context) qm =
  match qm with
  | ELibraryModule q ->
      normalize_ident_library_module (interface_table, norm_context) q
  | EMainModule q ->
      normalize_ident_main_module (interface_table, norm_context) q

let preprocess_ident_prolog proc_ctxt prolog = 
  let imported_interfaces, imported_modules = Norm_top.aux_preprocess_prolog proc_ctxt [] prolog in
  let cinterface_table = Hashtbl.create 11 in 
  List.iter (fun ((ncn,uri), modloc, interface) ->
    let norm_ctxt, cinterface = normalize_ident_interface cinterface_table proc_ctxt interface in 
    Hashtbl.add cinterface_table (ncn,uri) (modloc, norm_ctxt, cinterface)) imported_interfaces;
  (cinterface_table, imported_modules)

(* Library module *)

let preprocess_ident_library_module proc_ctxt q =
  try
    (* 1. Get the original query prolog *)
    let prolog = q.plibrary_module_prolog in
    preprocess_ident_prolog proc_ctxt prolog
  with
  | exn -> raise (Query(Static_Internal(bprintf_error "In preprocess_ident_library_module " exn)))

(* Main module *)

let preprocess_ident_main_module proc_ctxt q = 
  try
    (* 1. Get the original query prolog *)
    let prolog = q.pmain_module_prolog in
    preprocess_ident_prolog proc_ctxt prolog
  with
  | exn -> raise (Query(Static_Internal(bprintf_error "In preprocess_ident_main_module " exn)))
        
