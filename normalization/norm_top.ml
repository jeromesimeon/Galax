(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: norm_top.ml,v 1.119 2007/11/16 21:16:52 mff Exp $ *)

(* Module: Norm_top
   Description:
     This module implements the normalization phase for XQuery
     modules.
*)

open Format
open Debug
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
open Norm_expr

(****************************************************)
(* functions to map each components in query_module *)
(****************************************************)

(* Global variable declaration *)

let check_dup_vars ht cvname =
  if Namespace_util.RQNameHashtbl.mem ht cvname
  then
    raise (Query (Malformed_Core_Expr "[err:XQST0049] Two global variables with the same name"))
  else
    Namespace_util.RQNameHashtbl.add ht cvname ()

let normalize_option_decl norm_context oname ocontent =
    begin
      match oname with
      | (NSPrefix _,_) -> ()
      | _ ->
	  raise (Query (Malformed_Core_Expr "[err:XPST0081] option declaration cannot be resolved (empty prefix)"))
    end;
  begin
    try
      let nsenv = nsenv_from_norm_context norm_context in 
      let _ = resolve_element_qname nsenv oname in
      norm_context
    with
    | _ ->
	raise (Query (Malformed_Core_Expr "[err:XPST0081] option declaration cannot be resolved (unbound prefix)"))
  end

let normalize_variable_decl moddecl_opt norm_context ht v =
  let check_global_var_ns (vpref, vuri, vncname) = 
    match moddecl_opt with
    | None -> ()
    | Some (pref, uri) -> 
	if (vuri = NSUri uri || vpref = Namespace_builtin.local_prefix) then ()
	else
	  raise (Query (Malformed_Core_Expr "[err:XPSTXXXX Global variable defined in non-module namespace)"))
  in
  let fi = v.pvar_decl_loc in
  try
    let nsenv = nsenv_from_norm_context norm_context in
    let (norm_context,(fdefs, var_desc)) =
      match v.pvar_decl_desc with
      | (vname,dt,EVarUser e) ->
	  let cvname = Namespace_resolve.resolve_variable_qname nsenv vname in
	  set_top_var_decl norm_context cvname;
	  let (fdefs, ce) = normalize_expr norm_context e in
	  let (norm_context,cvname) = resolve_global_qname_register norm_context vname (CEVarUser ce) in
	  check_global_var_ns cvname;
	  check_dup_vars ht cvname;
	  (* Dependencies *)
	  let cdt = normalize_optional_sequencetype norm_context dt in
	  let r = (norm_context,(fdefs, (cvname, cdt, CEVarUser ce))) in
	  (unset_top_decl norm_context; r)
      | (vname,dt,var_body) ->
	  begin
	    let cevar_body =
	      match var_body with 
		EVarInterface -> CEVarInterface 
	      |	EVarImported -> CEVarImported
	      | EVarExternal -> CEVarExternal
	      |	_ -> raise (Query(Internal_Error("Unmatched variable body kind in normalize_variable_decl")))
	    in
	    let (norm_context,cvname) = resolve_global_qname_register norm_context vname (cevar_body) in
	    check_global_var_ns cvname;
	    check_dup_vars ht cvname;
	    let cdt = normalize_optional_sequencetype norm_context dt in
	    (norm_context,([], (cvname, cdt, cevar_body)))
	  end
    in
    (norm_context, (fdefs, fmkcvar_decl var_desc v.pvar_decl_loc))
  with
  | exn -> raise (error_with_file_location fi exn)

(* Global server declaration 
   declare server NC1 implements NC2 at E1;
*)
let normalize_server_decl norm_context ht s =
  let fi = s.pserver_decl_loc in
  let (nc1, nc2, e1) = s.pserver_decl_desc in
  try

    let (fdefs, ce1) = normalize_expr norm_context e1 in
    let (norm_context, uri, cserver_interface, ce1) = 
      extend_server_environment true norm_context nc1 nc2 (e1,ce1) in
    (* Temporary hack until dynamic server environment is implemented *)
    (* declare variable $fs_nc1 := ce1; declare server nc1 implements URI at $fs_nc1 *)
    let norm_context, nc1v = resolve_global_qname_register norm_context (fs_prefix, nc1) (CEVarUser ce1) in
    let temp_var_decl = fmkcvar_decl (nc1v, None, CEVarUser ce1) s.pserver_decl_loc in

    (norm_context, 
     (fdefs @ cserver_interface.pcprolog_functions, 
      temp_var_decl :: cserver_interface.pcprolog_vars, 
      [fmkcserver_decl (nc1, uri, ce1) s.pserver_decl_loc]))
  with
  | Query(Namespace_Internal _ ) -> raise (Query (Namespace_Internal ("Interface namespace prefix "^nc2^ " not found.")))
  | exn -> raise (error_with_file_location fi exn)

(* Key definition *)

let normalize_index norm_context v =
  let fi = v.pindex_def_loc in 
  try
    let nsenv = nsenv_from_norm_context norm_context in 
    let (index_def) =
      match v.pindex_def_desc with
      | ValueIndex (kn,e1,e2) ->
	  let (fdefs1, ce1) = normalize_expr norm_context e1 in
	  let (fdefs2, ce2) = normalize_expr norm_context e2 in
	  if (List.length (fdefs1@fdefs2) != 0) then  
	    raise (Query (Malformed_Expr "Index expressions may not contain side effects"));
	  (CValueIndex (kn, ce1, ce2))
      | NameIndex ename ->
	  let cename = resolve_element_qname nsenv ename in
	  (CNameIndex cename)
    in
    (fmkcindex_def index_def v.pindex_def_loc)
  with
  | exn -> raise (error_with_file_location fi exn)

(* Function Body *)

let normalize_body norm_context rfname arity fun_body output_dt =
  match fun_body with
  | EFunctionBltIn ->
      ([],norm_context,CEFunctionBltIn)
  | EFunctionInterface ->
      ([],norm_context,CEFunctionInterface)
  | EFunctionImported ->
      ([],norm_context,CEFunctionImported)
  | EFunctionUser expr ->
      let (fdefs, ce) = (normalize_expr norm_context expr) in
      let fi = (expr.pexpr_loc) in
      let (v1, var1) = gen_new_cvar norm_context (Some expr) fi in
      (* Apply rules for convert output value of function to simple
	 value, if necessary *)
      let ce' = convert_function_input_output norm_context ((v1, var1), (ce, output_dt)) var1 in 
      (* Type match checks that result of evaluating function body
	 matches declared return type *)
      (fdefs,norm_context,CEFunctionUser ce')

(* Function Definition *)

let new_namespace_binding (prefix, uri) = 
  (match prefix with 
  | NSPrefix ncn
  | NSInterfacePrefix ncn
  | NSServerPrefix ncn -> 
      if (uri = (NSUri ("http://www.w3.org/XML/1998/namespace"))) || (ncn = "xml") || (ncn = "xmlns")
      then raise (Query (Namespace_Error "[err:XQST0070] Namespace declaration redefines the xml or xmlns namespace"))
      else ()
  | _ -> ());
  (prefix, uri)

let check_proper_sigs system ht (cfname,arity) =
  let (_,uri,_) = cfname in
  if not(system) then
    if (uri = xml_uri) || (uri = xmlns_uri) || (uri = xs_uri) || (uri = xsi_uri) || (uri = fn_uri)
    then
      raise (Query (Malformed_Core_Expr "[err:XQST0045] Function declared in a reserved URI"));
  if Namespace_util.RQNameIntHashtbl.mem ht (cfname,arity)
  then
    raise (Query (Malformed_Core_Expr "[err:XQST0034] Two function declarations with the same name and same arity"))
  else
    Namespace_util.RQNameIntHashtbl.add ht (cfname,arity) ()

let normalize_function_signature ht norm_context f = 
  let nsenv = nsenv_from_norm_context norm_context in
  let mod_procctxt = module_context_from_norm_context norm_context in
  let (fname, vars, (input_types, return_type), fun_body, upd) = f.pfunction_def_desc in
  let rfname = resolve_function_qname nsenv fname in
  let csign =
    (List.map
       (fun dt ->
	 (normalize_optional_sequencetype_strong norm_context dt))
       input_types,
     (normalize_optional_sequencetype_strong norm_context return_type)) in
  let arity = List.length (fst csign) in
  check_proper_sigs (mod_procctxt.Processing_context.system) ht (rfname,arity);
  ((rfname, arity), (rfname, csign, map_fun_kind fun_body, upd))

let normalize_function_signatures norm_context fs =
  let ht = Namespace_util.RQNameIntHashtbl.create 17 in
  let fs = Xquery_ast_util.get_functions fs in
  List.map (normalize_function_signature ht norm_context) fs

let check_dup_params ht cvname =
  if Namespace_util.RQNameHashtbl.mem ht cvname
  then
    raise (Query (Malformed_Core_Expr "[err:XQST0039] Two parameters with the same name in function declaration"))
  else
    Namespace_util.RQNameHashtbl.add ht cvname ()

let check_distinct_parameters cvnames =
  let ht = Namespace_util.RQNameHashtbl.create 17 in
  List.iter (check_dup_params ht) cvnames

let normalize_function_body norm_context f = 
  let fi = f.pfunction_loc in 
  try
    let nsenv = nsenv_from_norm_context norm_context in
    let (fname, vnames, sign, body, is_upd) = f.pfunction_def_desc in
    let rfname = resolve_function_qname nsenv fname in
    (* Dependencies *)
    set_top_fun_decl norm_context rfname;
    let arity = List.length (fst sign) in
    let (input_types, output_type), opt_fun_kind, upd_flag_in_ctxt =
      one_sig_from_norm_context norm_context (rfname, arity)
    in
    let (norm_context,cvnames) =
      let f (norm_context,cvns) vname =
	let (norm_context,cvname) = resolve_variable_qname_register norm_context vname in
	(norm_context,cvname::cvns)
      in
      let norm_context,cvnames = List.fold_left f (norm_context,[]) vnames in
      norm_context, (List.rev cvnames)
    in
    check_distinct_parameters cvnames;
    let (fdefs,norm_context,cbody) = normalize_body norm_context rfname arity body output_type in
    if Conf.is_xquerybang() || Conf.is_xqueryp() then
      begin
	match cbody with 
	| CEFunctionInterface
	| CEFunctionImported
	| CEFunctionBltIn -> ()
	| CEFunctionUser exp ->
	    if (is_upd = NonUpdating) && (expr_may_generate_updates exp)
	    then
	      raise (Query
		       (Static_Error
			  ("Function "^(string_of_uqname fname)^
			   " may generate updates, but it was not declared as updating.")))
	    else ();
      end;
    let r = 
    (norm_context,fdefs@[fmkcfunction_def ((rfname,arity), cvnames, (input_types, output_type), cbody, is_upd) f.pfunction_loc])
    in (unset_top_decl norm_context; r)
  with
  | exn -> raise (error_with_file_location fi exn)

(**************)
(* Statements *)
(**************)

(* Top-level snap normalization: 
   Ensure every query is wrapped by a top-level snap 
*)
let normalize_toplevel_snap ce = 
  match ce.pcexpr_desc with
    | CESnap _ -> ce
	(* Default to unordered, unless there is a snap *)
    | _ -> fmkcexpr (CESnap (Snap_Unordered_Deterministic, ce)) None ce.pcexpr_loc
	
let normalize_statement norm_context statement =
(*
print_string ("Before normalize_statement"^(Print_top.bprintf_statement "" statement)^"\n");
Norm_context.dump_norm_context norm_context;
*)
  let (fdefs, ce) = (normalize_expr norm_context statement) in
  if (List.length (fdefs) != 0) then  
    raise (Query (Malformed_Expr "Top-level expression may not contain while-loops"));
  let cs = normalize_toplevel_snap ce in
(*
print_string ("After normalize_statement\n");
Norm_context.dump_norm_context norm_context;
*)
  cs

let normalize_statement_list norm_context ss = 
  List.fold_left 
    (fun (fdefs,csl) s -> 
      let (fdefs',cs) = normalize_expr norm_context s
      in (fdefs@fdefs',csl@[normalize_toplevel_snap cs])) ([],[]) ss

let normalize_context_decl norm_context (ns_decls, schemas, cinterfaces) c =
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
	let new_binding = new_namespace_binding (NSPrefix ncn, uri) in
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
	      let new_binding = new_namespace_binding (NSPrefix ncn, NSUri target_uri) in
	      (ns_decls@[new_binding], new_schemas, cinterfaces)
	  | Some NSDefaultElementPrefix ->
	      let new_binding = (NSDefaultElementPrefix,NSUri target_uri) in
	      (ns_decls@[new_binding], new_schemas, cinterfaces)
	  | Some _ ->
	      raise (Query (Namespace_Error "Bogus namespace prefix in schema import AST"))
	end
    | EImportServiceDecl (ncn, wsdl_uri, wsdl_file) ->
	raise (Query(Prototype("Import WSDL Service not implemented in Norm_top.preprocess_prolog")))

    | EImportInterfaceDecl (ncn, uri, file) ->
	let new_binding = new_namespace_binding (NSInterfacePrefix ncn, NSUri uri) in
	(ns_decls@[new_binding], schemas, cinterfaces) 
    | EImportModuleDecl (ncn, uri, file) ->
	let new_binding = new_namespace_binding (NSPrefix ncn, NSUri uri) in
	let new_interface = lookup_and_map_interface norm_context (NSPrefix ncn) (NSPrefix "",uri)
	in (ns_decls@[new_binding], schemas, cinterfaces @ [new_interface])
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

(* Checking constraints on the prolog setters *)

let multiple_boundary_space cdl =
  List.length (List.find_all (fun x -> match x.pcontext_decl_desc with EXmlSpaceDecl _ -> true | _ -> false) cdl) > 1

let multiple_base_uri cdl =
  List.length (List.find_all (fun x -> match x.pcontext_decl_desc with EBaseURIDecl _ -> true | _ -> false) cdl) > 1

let multiple_default_element_namespace cdl =
  List.length (List.find_all (fun x -> match x.pcontext_decl_desc with EDefaultElementNamespaceDecl _ -> true | _ -> false) cdl) > 1

let multiple_default_function_namespace cdl =
  List.length (List.find_all (fun x -> match x.pcontext_decl_desc with EDefaultFunctionNamespaceDecl _ -> true | _ -> false) cdl) > 1

let multiple_namespace_prefixes cdl =
  let bef = ref [] in
  let rec find_aux cdl =
    match cdl with
    | [] -> ()
    | x :: cdl ->
	begin
	  match x.pcontext_decl_desc with
	  | ENamespaceDecl (pref,_) ->
	      let r = (List.exists (fun x -> x = pref) !bef) in
	      begin
		bef := pref::!bef;
		if r then raise Not_found
		else ()
	      end
	  | _ -> ()
	end; find_aux cdl
  in
  try
    find_aux cdl;
    false
  with
  | _ ->
      true

let multiple_construction_decl cdl =
  List.length (List.find_all (fun x -> match x.pcontext_decl_desc with EConstructionDecl _ -> true | _ -> false) cdl) > 1

let multiple_ordering_decl cdl =
  List.length (List.find_all (fun x -> match x.pcontext_decl_desc with EOrderingDecl _ -> true | _ -> false) cdl) > 1

let multiple_empty_order_decl cdl =
  List.length (List.find_all (fun x -> match x.pcontext_decl_desc with EDefaultEmptyOrderDecl _ -> true | _ -> false) cdl) > 1

let multiple_default_collation_decl cdl =
  List.length (List.find_all (fun x -> match x.pcontext_decl_desc with EDefaultCollationDecl _ -> true | _ -> false) cdl) > 1

let multiple_copy_namespaces_decl cdl =
  List.length (List.find_all (fun x -> match x.pcontext_decl_desc with ECopyNamespacesDecl _ -> true | _ -> false) cdl) > 1

let check_prolog_consistent cdl =
  begin
(*    if incorrect_schema_imports cdl
    then
    else
      ; *)
    if multiple_boundary_space cdl
    then
      raise (Query (Malformed_Core_Expr "[err:XQST0068] Multiple boundary-space declarations"));
    if multiple_base_uri cdl
    then
      raise (Query (Malformed_Core_Expr "[err:XQST0032] Multiple base-uri declarations"));
    if multiple_default_element_namespace cdl
    then
      raise (Query (Malformed_Core_Expr "[err:XQST0066] Multiple default element namespace declarations"));
    if multiple_default_function_namespace cdl
    then
      raise (Query (Malformed_Core_Expr "[err:XQST0066] Multiple default function namespace declarations"));
    if multiple_namespace_prefixes cdl
    then
      raise (Query (Malformed_Core_Expr "[err:XQST0033] Multiple namespace declarations for the same prefix"));
    if multiple_construction_decl cdl
    then
      raise (Query (Malformed_Core_Expr "[err:XQST0067] Multiple construction declarations"));
    if multiple_ordering_decl cdl
    then
      raise (Query (Malformed_Core_Expr "[err:XQST0065] Multiple ordering declarations"));
    if multiple_empty_order_decl cdl
    then
      raise (Query (Malformed_Core_Expr "[err:XQST0069] Multiple empty order declarations"));
    if multiple_default_collation_decl cdl
    then
      raise (Query (Malformed_Core_Expr "[err:XQST0068] Multiple default collation declarations"));
    if multiple_copy_namespaces_decl cdl
    then
      raise (Query (Malformed_Core_Expr "[err:XQST0055] Multiple copy namespaces declarations"));
  end

let normalize_context_with_env norm_context cdl =
  check_prolog_consistent cdl;
  List.fold_left (normalize_context_decl norm_context) ([], [], []) cdl

let normalize_decls moddecl_opt norm_context fvs =
  let ht = Namespace_util.RQNameHashtbl.create 17 in
  let rec normalize_decls_aux norm_context fvs =
    match fvs with
    | [] -> (norm_context,([],[],[]))
    | (OptionDecl (oname,ocontent)) :: fvs' ->
	let norm_context = normalize_option_decl norm_context oname ocontent in
	let (norm_context,(fs',vs',ss')) = normalize_decls_aux norm_context fvs' in
	(norm_context,(fs',vs',ss'))
    | (FunDef f) :: fvs' ->
	let (norm_context,fdefs) = normalize_function_body norm_context f in
	let (norm_context,(fs',vs',ss')) = normalize_decls_aux norm_context fvs' in
	(norm_context,(fdefs@fs',vs',ss'))
    | (VarDef v) :: fvs' ->
	let (norm_context,(fs,first)) = normalize_variable_decl moddecl_opt norm_context ht v in
	let (norm_context,(fs',vs',ss')) = normalize_decls_aux norm_context fvs' in
	(norm_context,(fs@fs', first::vs',ss'))
    | (ServerDef s) :: fvs' ->
	let (norm_context, (fs, vs, ss)) = normalize_server_decl norm_context ht s in 
	let (norm_context, (fs',vs',ss')) = normalize_decls_aux norm_context fvs' in
	(norm_context,(fs@fs', vs@vs', ss@ss'))
  in
  normalize_decls_aux norm_context fvs

(****************************************************************)
(* Toplevel functions to map the Full XQuery to the core XQuery *)
(****************************************************************)

(* Prolog *)
let aux_normalize_prolog moddecl_opt (interface_table, norm_context) prolog =
  set_interface_table norm_context interface_table; 
  let new_binding = 
    match moddecl_opt with 
    | None -> []
    | Some (pref,uri) -> [new_namespace_binding(pref, NSUri uri)]
  in
  try
    begin
      let nsenv = nsenv_from_norm_context norm_context in
      (* 1. Normalize the context *)
      let { pprolog_xschemas = inlined_xschemas;
	    pprolog_contexts = ns;
	    pprolog_funcvars = fvs;
	    pprolog_indices = ks } = prolog
      in
      let (ns_decls, imported_xschemas, imported_interfaces) = 
	normalize_context_with_env norm_context (ns) in
      (* Any redeclarations of namespaces that are inconsistent should raise an error *)
      let nsenv2 = Namespace_context.add_all_ns_xquery nsenv (new_binding@ns_decls) in
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
      let sigs' = normalize_function_signatures norm_context' fvs in
      let norm_context'' = add_sigs_to_norm_context norm_context' sigs' in

      (* 5. Add the imported interfaces' normalization contexts (functions and variables)
            to the local module's normalization context: IN PLACE
      *)
      List.iter (fun (nc, ci) -> merge_imported_norm_context norm_context'' nc) imported_interfaces;

      (* 6. Map all function bodies, variable and server declarations: *)
      let (norm_context'',(functions, vars, servers)) = normalize_decls moddecl_opt norm_context'' fvs in
      let (indices) = List.map (normalize_index norm_context'') ks in

      (* 7. Include imported functions and variables in this module's context. *)
      let imported_functions = (List.concat(List.map (fun (nc, ci) -> ci.pcprolog_functions) imported_interfaces)) in
      Debug.print_default_debug ("Imported functions : "^(string_of_int(List.length(imported_functions))));
      let imported_vars = (List.concat(List.map (fun (nc, ci) -> ci.pcprolog_vars) imported_interfaces)) in

      (* Checking for duplicate variables happens in too many places ... *)
      let ht = Namespace_util.RQNameHashtbl.create 7 in 
      List.iter (fun vecl -> let (v, _,_) = vecl.pcvar_decl_desc in check_dup_vars ht v) (imported_vars @ vars);
      let cprolog =
	{ pcprolog_functions = imported_functions @ functions;
	  pcprolog_vars      = imported_vars @ vars;
          pcprolog_servers   = servers; (* Should global servers be imported from other modules? *)
	  pcprolog_indices   = indices }
      in
      check_cycles norm_context'';
      (norm_context'', cprolog)
    end
  with
  | exn -> raise (Query(Static_Internal(bprintf_error "In normalize_prolog " exn)))

let normalize_prolog (interface_table, norm_context) prolog =
  aux_normalize_prolog None (interface_table, norm_context) prolog

(* Interface *)
let normalize_interface (interface_table, norm_context) interface =
  try
    let { pinterface_decl = decl ;
	  pinterface_prolog = iprolog; } = interface
    in
    (* 2. Get the module target namespace *)
    let (pref,uri) = decl in
    (* 3. Extend the interface's prolog with the namespace declaration *)
Debug.print_default_debug ("\nNormalize Interface "^pref^" = "^uri^"\n");
(*    let new_decl = fmkcontext_decl (ENamespaceDecl (pref,NSUri uri)) Finfo.bogus in *)
    let prolog = 
    { pprolog_xschemas 	= iprolog.iprolog_xschemas;
      pprolog_contexts 	= (* new_decl:: *) iprolog.iprolog_contexts;
      pprolog_funcvars  = iprolog.iprolog_funcvars;
      pprolog_indices  	= [] }
    in
Debug.print_default_debug ("\nFuncvars : "^(string_of_int(List.length prolog.pprolog_funcvars))^"\n");
    let (n,p)  = aux_normalize_prolog (Some (NSInterfacePrefix pref, uri)) (interface_table, norm_context) prolog  in
Debug.print_default_debug (Print_top.bprintf_acprolog "\nInterface: " p);
Debug.print_default_debug ("\nCE Funcvars : "^(string_of_int(List.length p.pcprolog_functions))^"\n");
    (n,p)

  with
  | exn -> raise (Query(Static_Internal(bprintf_error "In normalize_interface " exn)))

(* Library module *)

let normalize_library_module (interface_table, norm_context) q =
  try
    (* 1. Get the original query prolog *)
    let prolog = q.plibrary_module_prolog in
    (* 2. Get the module target namespace *)
    let (pref,uri,optint) = q.plibrary_module_decl in
(*
    (print_string (pref^"="^uri^"\n");
     match optint with
    | None -> print_string ("No interface location\n")
    | Some (uri, None) -> print_string ("implements "^uri^"\n")
    | Some (uri, Some loc) -> print_string ("implements "^uri^" at "^loc^"\n"));
*)
    
(*    let _ = new_namespace_binding (NSPrefix pref, NSUri uri) in *)
    (* 3. Extend the module's prolog with the corresponding namespace declaration *)
(*    let new_decl = fmkcontext_decl (ENamespaceDecl (pref, NSUri uri)) Finfo.bogus in *)
    let patched_prolog =
      { pprolog_xschemas 	= prolog.pprolog_xschemas;
	pprolog_contexts 	= (* new_decl :: *) prolog.pprolog_contexts;
	pprolog_funcvars        = prolog.pprolog_funcvars;
	pprolog_indices  	= prolog.pprolog_indices }
    in
    (* 4. Normalize the prolog *)
    let (norm_context',cprolog) = aux_normalize_prolog (Some(NSPrefix pref,uri)) (interface_table, norm_context) patched_prolog in
    (* 6. Build the resulting core module *)
    let cmodule =
      { pcmodule_prolog = cprolog;
	pcmodule_statements = [] }
    in
    (norm_context', cmodule)
  with
  | exn -> raise (Query(Static_Internal(bprintf_error "In normalize_library_module " exn)))

(* Main module *)

let normalize_main_module (interface_table, norm_context) q = 
  try
    (* 1. Get the original query prolog *)
    let prolog = q.pmain_module_prolog in
    (* 2. Normalize the prolog *)
    let (norm_context',cprolog) = aux_normalize_prolog None (interface_table, norm_context) prolog in
    if (Debug.default_debug()) then 
      begin
	Print_top.printf_cxschema "\nInput Schema :" (cxschema_from_norm_context norm_context');
	Print_top.printf_acprolog "\nProlog :" (cprolog)
      end
    else (); 
    (* 3. Normalize the statements *)
    let es = q.pmain_module_statements in
    let (fwhile_funs, ces) = normalize_statement_list norm_context' es in
    (* 4. Extend prolog with generated functions *)  
    let cprolog' = {cprolog with pcprolog_functions = List.rev_append fwhile_funs cprolog.pcprolog_functions} in
    (* 5. Build the main module *)  
    let cmodule =
      { pcmodule_prolog = cprolog';
        pcmodule_statements = ces }
    in
    (norm_context', cmodule)
  with
  | exn -> raise (Query(Static_Internal(bprintf_error "In normalize_main_module " exn)))
        
(* Module *)

let normalize_module (interface_table, norm_context) qm =
  set_interface_table norm_context interface_table; 
  match qm with
  | ELibraryModule q ->
      normalize_library_module (interface_table, norm_context) q
  | EMainModule q ->
      normalize_main_module (interface_table, norm_context) q

(* No normalization -- just return dummy core expressions *)
let nonorm_statement norm_context statement =
  fmkcexpr CEEmpty None Finfo.bogus

let nonorm_prolog (interface_table, norm_context) prolog =
  set_interface_table norm_context interface_table; 
  let cprolog =
    { pcprolog_functions = [];
      pcprolog_vars = [];
      pcprolog_servers = [];
      pcprolog_indices = [] }
  in
  (norm_context, cprolog)

let nonorm_library_module (interface_table, norm_context) q =
  (* 1. Get the original query prolog *)
  set_interface_table norm_context interface_table; 
  let prolog = q.plibrary_module_prolog in
  let (_, cprolog) = nonorm_prolog (interface_table, norm_context) prolog in
  let cmodule =
    { pcmodule_prolog = cprolog;
      pcmodule_statements = [] }
  in
  (norm_context, cmodule)

let nonorm_main_module (interface_table, norm_context) q = 
  (* 1. Get the original query prolog *)
  let prolog = q.pmain_module_prolog in
  let (_, cprolog) = nonorm_prolog (interface_table, norm_context) prolog in
  let cmodule =
    { pcmodule_prolog = cprolog; 
      pcmodule_statements = [] }
  in
  (norm_context, cmodule)

(****************************************************)
(* Toplevel functions to pre-process XQuery modules *)
(****************************************************)

let get_nsprefix qname = 
  let (prefix, lcname) = qname in 
  match prefix with
  | NSPrefix p -> (p,lcname)
  | _ -> raise (Query(Static_Internal("Expected generic prefix. Found: "^(string_of_prefix prefix))))

(* Prolog *)
let interface_signature_of_funcvar fvlist fv =
  match fv with 
  | OptionDecl (n,s) -> fvlist      (* Option declarations are never imported *)
	(* A function or variable signature excludes a definition *)
  | FunDef function_def ->
      let (fname, arglist, func_sig, _, update_modifier) = function_def.pfunction_def_desc in
      let (prefix, lcname) = get_nsprefix fname in 
      fvlist @ [ FunDef                                    
	{ pfunction_def_desc = 
	  ((NSInterfacePrefix prefix, lcname), arglist, func_sig, EFunctionInterface, update_modifier);
	  pfunction_loc = function_def.pfunction_loc 
	} ]
  | VarDef var_decl ->
      let (vname, typeopt, _) = var_decl.pvar_decl_desc in 
      let (prefix, lcname) = get_nsprefix vname in 
      fvlist @ [ VarDef
	{ pvar_decl_desc = ((NSInterfacePrefix prefix, lcname), typeopt, EVarInterface); 
	  pvar_decl_loc = var_decl.pvar_decl_loc;
	} ]
  | ServerDef server_decl -> fvlist  (* Server declarations are never imported *)

(* Generate or parse the interface associated with a module                     *)
(* For now, we require that both module and interface are in the same namespace *)

(* Pre-process context declarations to identify imported interfaces and modules *)
let rec preprocess_context_decl proc_ctxt modules_visited (imported_interfaces, imported_modules) c =
  let fi = c.pcontext_decl_loc in
  try 
    match c.pcontext_decl_desc with 
    (* Don't do anything for most declarations *)
    | EBaseURIDecl _
    | ENamespaceDecl _
    | EDefaultElementNamespaceDecl _
    | EDefaultFunctionNamespaceDecl _
    | EXmlSpaceDecl _
    | EDefaultCollationDecl _
    | EConstructionDecl _
    | EOrderingDecl _
    | EDefaultEmptyOrderDecl _
    | ECopyNamespacesDecl _
    | ESchemaDecl _ ->
	(imported_interfaces, imported_modules)

    | EImportServiceDecl (ncn, wsdl_uri, loc_hint_option) ->
	raise (Query(Prototype("Import WSDL Service not implemented in Norm_top.preprocess_prolog")))
(*	(modules_visited, imported_interfaces, imported_modules) *)

    | EImportInterfaceDecl (ncn, uri, loc_hint_option) ->
	ignore(new_namespace_binding(NSInterfacePrefix ncn, NSUri uri));
	(* For imported interfaces, just return the imported interface *)
	Debug.print_default_debug ("Import interface: "^uri^"\n");

	(* An interface can have several location hints, which may
	   correspond to alternative locations or to multiple
	   fragments of an interface that should be merged.
	   (Strange, but true)
	   This behavior is implementation-defined, but we need it to
	   implement the XQuery Test Suite.  
	*)
	let interface_locs = Processing_context.resolve_interface_location_hint proc_ctxt loc_hint_option uri in 
	let try_hints (good_hint, exn, interface) interface_loc =
	  try
	    let parse_ctxt = Parse_context.build_xquery_parse_context proc_ctxt in
	    let (_,interface') =
	      Parse_top.parse_interface_from_io parse_ctxt (Galax_io.Http_Input interface_loc)
	    in
	    (true,
	     None,
	     if (proc_ctxt.Processing_context.merge_module_locations) then 
	       (Debug.print_default_debug("Merging interface at "^interface_loc);
		Xquery_ast_util.merge_interfaces interface interface')
	     else interface')
	  with exn -> 
	    (Debug.print_default_debug ("In import-module:"^(Error.bprintf_error "" exn)); (good_hint, Some exn, interface))
        in 
        let (good_hint, exno, interface) = List.fold_left try_hints (false, None, empty_interface ncn uri) interface_locs in
	if good_hint then 
	  (imported_interfaces @ [((NSInterfacePrefix "", uri), None, interface)], imported_modules)
	else
	  begin
	    match exno with
	    | Some exn -> raise exn
	    | None -> raise (Query(Module_Import("No valid location hint for interface: "^(uri))))
	  end
    | EImportModuleDecl (ncn, uri, loc_hint_option) ->
	Debug.print_default_debug ("Import module: "^uri^"\n");
	ignore(new_namespace_binding(NSPrefix ncn, NSUri uri));
	(* For imported modules, create an interface for the module and return that. *)
	if (List.mem uri modules_visited) then
	  raise (Query(Module_Import("Circularity in import of module: "^(uri))))
	else
	  let mod_locs = Processing_context.resolve_module_location_hint proc_ctxt loc_hint_option uri in 
          let try_hints (good_hint, exn, mod_locs, nested_interfaces, nested_modules, interface, m) mod_loc =
	    try
	(* A module can have several location hints, which may
	   correspond to alternative locations to try, or to multiple
	   fragments of a module that should be concatenated.
	   This behavior is implementation defined, but we need it to
	   implement the XQuery Test Suite.  
	*)
	      let parse_ctxt = Parse_context.build_xquery_parse_context proc_ctxt in
	      let (_, m') = Parse_top.parse_library_module_from_io parse_ctxt (Galax_io.Http_Input mod_loc) in
	      let nested_interfaces', nested_modules' = 
		aux_preprocess_prolog proc_ctxt (uri:: modules_visited) m'.plibrary_module_prolog in
	      let interface' = interface_from_module proc_ctxt m' in
	      if (proc_ctxt.Processing_context.merge_module_locations) then 
		(Debug.print_default_debug("Merging module at "^mod_loc);
		 (true, 
		  None, mod_loc::mod_locs, 
		  nested_interfaces'@nested_interfaces, 
		  nested_modules'@nested_modules,
		  merge_interfaces interface interface', 
		  Xquery_ast_util.merge_library_modules m m'))
	      else
		(true, None, [mod_loc], nested_interfaces', nested_modules', interface', m')
	    with
	      exn -> 
		(Debug.print_default_debug ("In import-module:"^(Error.bprintf_error "" exn)); 
		 (good_hint, Some exn, mod_locs, nested_interfaces, nested_modules, interface, m))
	  in
	  let (good_hint, exno, mod_locs, nested_interfaces, nested_modules, interface, m) = 
	    List.fold_left try_hints (false, None, [], [], [], empty_interface ncn uri, empty_library_module ncn uri) mod_locs in
	  if good_hint then 
	    begin
(*	      Print_top.fprintf_interface Format.std_formatter "Generated interface\n" interface;
   Print_top.fprintf_library_module  Format.std_formatter "Imported module\n" m; *)
	      (imported_interfaces @ nested_interfaces @ [((NSPrefix "", uri), Some (mod_locs), interface)], 
	       imported_modules @ nested_modules @ [m])
	    end
	  else
	    begin
	      match exno with
	      | Some exn -> raise exn
	      | None -> raise (Query(Module_Import("No valid location hint for interface: "^(uri))))
	    end

  with
  | exn -> raise (error_with_file_location fi exn)

and interface_from_module proc_ctxt m = 
  match m.plibrary_module_decl with 
  | (ncname, uri1, None) -> 
      let funcvars = List.fold_left interface_signature_of_funcvar [] m.plibrary_module_prolog.pprolog_funcvars in 
      let interface_prolog = 
	{ iprolog_xschemas = [];  (* Strangely, schemas are _not_ imported *)
	  iprolog_contexts = m.plibrary_module_prolog.pprolog_contexts;  
	  iprolog_funcvars = funcvars;
	} 
      in
      { pinterface_decl = (ncname, uri1); 
	pinterface_prolog = interface_prolog;  } 
  | (ncname, uri1, Some (uri2, loc_hint_option)) when uri1 = uri2 -> 
      let import_interface_decl = 
	fmkcontext_decl (EImportInterfaceDecl (ncname, uri1, loc_hint_option)) Finfo.bogus in
      begin
	match preprocess_context_decl proc_ctxt [] ([], []) import_interface_decl with 
	| ([(_, None, interface)], []) -> interface
	| _ -> raise (Query(Internal_Error ("Preprocess_context_decl returned invalid result")))
      end
  | (ncname, uri1, Some (uri2, loc_hint_option)) -> 
      raise (Query(Module_Import("Module namespace '"^uri1^"' and interface namespace '"^uri2^"'do not match")))

and aux_preprocess_prolog proc_ctxt modules_visited prolog =
  try
    List.fold_left (preprocess_context_decl proc_ctxt modules_visited) ([], []) prolog.pprolog_contexts 
  with
  | exn -> raise (Query(Static_Internal(bprintf_error "In preprocess_prolog " exn)))

let preprocess_prolog proc_ctxt prolog = 
  let norm_ctxt = default_norm_context proc_ctxt in 
  let imported_interfaces, imported_modules = aux_preprocess_prolog proc_ctxt [] prolog in
  let cinterface_table = Hashtbl.create 11 in 
  List.iter (fun (uri, modlocs, interface) ->
    let norm_ctxt, cinterface = normalize_interface (cinterface_table, norm_ctxt) interface in 
    Hashtbl.add cinterface_table (uri) (modlocs, norm_ctxt, cinterface)) imported_interfaces;
  (cinterface_table, imported_modules)

(* Library module *)

let preprocess_library_module proc_ctxt q =
  try
    (* 1. Get the original query prolog *)
    let prolog = q.plibrary_module_prolog in
    preprocess_prolog proc_ctxt prolog
  with
  | exn -> raise (Query(Static_Internal(bprintf_error "In preprocess_library_module " exn)))

(* Main module *)

let preprocess_main_module proc_ctxt q = 
  try
    (* 1. Get the original query prolog *)
    let prolog = q.pmain_module_prolog in
    preprocess_prolog proc_ctxt prolog
  with
  | exn -> raise (Query(Static_Internal(bprintf_error "In preprocess_main_module " exn)))
        
let nopreprocess_prolog procctxt prolog = (Hashtbl.create 0, [])

let nopreprocess_library_module procctxt lm = (Hashtbl.create 0, [])

let nopreprocess_main_module procctxt m = (Hashtbl.create 0, [])
