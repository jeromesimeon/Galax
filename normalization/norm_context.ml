(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: norm_context.ml,v 1.37 2007/10/25 00:08:42 mff Exp $ *)

(* Module: Norm_context
   Description:
     This module implements the part of the context which is required
     during the normalization phase.
*)

open Error

open Namespace_names
open Namespace_util
open Namespace_context

open Xquery_common_ast
open Xquery_core_ast

open Processing_context


(* Type for the normalization context *)

type function_signature = 
    ((cfname * int) * (cfname * cfunction_signature * acfunction_body_kind * updating_modifier))

(* Target URI => Locations, Norm context, Interface *)
type norm_interface_table = ((Namespace_names.prefix * string), (string list option * norm_context * acinterface)) Hashtbl.t
and  norm_context =
    { norm_proc_context   : processing_context;
      norm_module_context : module_processing_context;
      norm_nsenv          : nsenv;
      norm_in_scope_nsenv : nsenv;
      norm_vars           : (acvar_body option list) RQNameHashtbl.t;
      norm_next_variable  : Namespace_generate.name_gen ref;
      norm_cxschema 	  : Xquery_type_core_ast.cxschema;
      norm_fun_sigs : (cfname * cfunction_signature * acfunction_body_kind * updating_modifier) RQNameIntHashtbl.t;
      norm_var_graph      : Norm_var_graph.var_graph;
      mutable norm_interface_table : norm_interface_table;
      mutable norm_top_decl    : Norm_var_graph.var_or_fun option;
      mutable norm_nested_exec : bool }

(* Builds a new normalization context *)

let string_of_cfun_kind cfun_kind = 
  match cfun_kind with 
  | CEFunctionBltInKind -> "Builtin"
  | CEFunctionInterfaceKind -> "Interface"
  | CEFunctionImportedKind -> "Imported"
  | CEFunctionUserKind -> "User"

let dump_norm_context nc = 
(* Debug.set_debug [Debug.DefaultDebug];*)
  Debug.print_compile_debug("Norm context");
  dump_nsenv nc.norm_nsenv;
  RQNameHashtbl.iter (fun c vb -> Debug.print_compile_debug("Var "^(prefixed_string_of_rqname c)^"\n")) nc.norm_vars;
  RQNameIntHashtbl.iter (fun (c,i) (c',sign,b,upd) -> 
  Debug.print_compile_debug("Function "^(prefixed_string_of_rqname c)^"/"^(prefixed_string_of_rqname c')^":"^(string_of_cfun_kind b)^"\n")) nc.norm_fun_sigs;
(* Debug.set_debug []; *)
  ()

let build_norm_context proc_ctxt nsenv cxschema siglist =
  let mod_ctxt   = Processing_context.default_module_processing_context() in 
  let ng =
    Processing_context.get_name_generator
      mod_ctxt Namespace_builtin.fs_prefix Namespace_builtin.fs_uri "v"
  in
  let norm_context =
    { norm_proc_context       = proc_ctxt;
      norm_module_context     = mod_ctxt;
      norm_nsenv    	      = nsenv;
      norm_in_scope_nsenv     = Namespace_context.default_xml_out_nsenv();
      norm_vars 	      = RQNameHashtbl.create 167;
      norm_next_variable      = ng;
      norm_cxschema           = cxschema;
      norm_fun_sigs 	      = RQNameIntHashtbl.create 167;
      norm_var_graph          = Norm_var_graph.build_var_graph ();
      norm_interface_table    = Hashtbl.create 1;
      norm_top_decl           = None;
      norm_nested_exec        = false}
  in
  let _ =
    List.iter
      (fun ((cfname, arity), (tfname, cfunc_signature, fun_body, upd)) ->
	let arity = List.length (fst cfunc_signature) in
	RQNameIntHashtbl.add
	  norm_context.norm_fun_sigs (cfname,arity) (tfname,cfunc_signature,fun_body,upd)) siglist
  in
  norm_context

let get_in_scope_nsenv norm_context = norm_context.norm_in_scope_nsenv

(* Default normalization context *)

let default_norm_context proc_ctxt =
  let default_nsenv = default_xquery_nsenv in
  let default_schema = Schema_builtin.built_in_cxschema in
  let default_signatures = [] in
  build_norm_context
    proc_ctxt
    default_nsenv
    default_schema
    default_signatures

(* Replace the namespace environment  *)

let replace_namespace_env_in_norm_context nsenv norm_ctxt =
  { norm_proc_context = norm_ctxt.norm_proc_context;
    norm_module_context = norm_ctxt.norm_module_context;
    norm_nsenv    	    = nsenv;
    norm_in_scope_nsenv	    = norm_ctxt.norm_in_scope_nsenv;
    norm_vars     	    = norm_ctxt.norm_vars;
    norm_next_variable 	    = norm_ctxt.norm_next_variable;
    norm_cxschema      	    = norm_ctxt.norm_cxschema;
    norm_fun_sigs      	    = norm_ctxt.norm_fun_sigs;
    norm_var_graph          = norm_ctxt.norm_var_graph;
    norm_interface_table    = norm_ctxt.norm_interface_table;
    norm_top_decl           = norm_ctxt.norm_top_decl;
    norm_nested_exec        = norm_ctxt.norm_nested_exec
  }

(* We need to keep the hashtable of function signatures and bodies in tact. *)

let merge_imported_norm_context dest src = 
(*  RQNameIntHashtbl.iter (fun (f,i) sign -> print_string("Dest "^(Namespace_names.prefixed_string_of_rqname f)^"\n")) dest.norm_fun_sigs; *)

  RQNameHashtbl.iter (fun v dep -> 
    if Namespace_util.RQNameHashtbl.mem dest.norm_vars v then
      raise (Query (Malformed_Core_Expr ("[err:XQST0049] Two global variables with same name: "^
					 (Namespace_names.prefixed_string_of_rqname v))))
    else RQNameHashtbl.add dest.norm_vars v dep) src.norm_vars ; 
  RQNameIntHashtbl.iter (fun (f,i) sign -> Debug.print_dxq_debug("Merge "^(Namespace_names.prefixed_string_of_rqname f)^"\n"); RQNameIntHashtbl.add dest.norm_fun_sigs (f,i) sign) src.norm_fun_sigs 

let copy_norm_context_with_sigs ctxt nsenv cxschema = 
  let ctxt = 
  { norm_proc_context = ctxt.norm_proc_context;
    norm_module_context = ctxt.norm_module_context;
    norm_nsenv          = nsenv;
    norm_in_scope_nsenv = ctxt.norm_in_scope_nsenv;
    norm_vars          	= RQNameHashtbl.copy ctxt.norm_vars;
    norm_next_variable 	= ctxt.norm_next_variable;
    norm_cxschema      	= Xquery_type_core_ast_util.merge_cxschema ctxt.norm_cxschema cxschema;
    norm_fun_sigs      	= RQNameIntHashtbl.copy ctxt.norm_fun_sigs;
    norm_var_graph     	= ctxt.norm_var_graph;
    norm_interface_table     	= ctxt.norm_interface_table;
    norm_top_decl      	= ctxt.norm_top_decl;
    norm_nested_exec   	= ctxt.norm_nested_exec
  }
  in
  ctxt

let copy_norm_context_with_nsenv ctxt nsenv1 nsenv2 = 
  let ctxt = 
  { norm_proc_context = ctxt.norm_proc_context;
    norm_module_context = ctxt.norm_module_context;
    norm_nsenv          = nsenv1;
    norm_in_scope_nsenv = nsenv2;
    norm_vars          	= RQNameHashtbl.copy ctxt.norm_vars;
    norm_next_variable 	= ctxt.norm_next_variable;
    norm_cxschema      	= ctxt.norm_cxschema;
    norm_fun_sigs      	= RQNameIntHashtbl.copy ctxt.norm_fun_sigs;
    norm_var_graph     	= ctxt.norm_var_graph;
    norm_interface_table    = ctxt.norm_interface_table;
    norm_top_decl      	= ctxt.norm_top_decl;
    norm_nested_exec   	= ctxt.norm_nested_exec
  }
  in
  ctxt

let add_ns_bindings_to_norm_context norm_context new_nss =
  begin
    let nsenv1 = norm_context.norm_nsenv in 
    let nsenv1' = Namespace_context.add_all_ns nsenv1 new_nss in
    let nsenv2' =
      Namespace_context.add_all_ns norm_context.norm_in_scope_nsenv new_nss
	(* Namespace_context.add_force_all_ns norm_context.norm_in_scope_nsenv new_nss *)
    in
    let norm_context' = copy_norm_context_with_nsenv norm_context nsenv1' nsenv2' in
    (norm_context',nsenv1')
  end


let copy_norm_context ctxt =
  { norm_proc_context   = ctxt.norm_proc_context;
    norm_module_context = ctxt.norm_module_context;
    norm_nsenv          = ctxt.norm_nsenv;
    norm_in_scope_nsenv = ctxt.norm_in_scope_nsenv;
    norm_vars           = RQNameHashtbl.copy ctxt.norm_vars;
    norm_next_variable  = ctxt.norm_next_variable;
    norm_cxschema      	= ctxt.norm_cxschema;
    norm_fun_sigs      	= RQNameIntHashtbl.copy ctxt.norm_fun_sigs;
    norm_var_graph     	= ctxt.norm_var_graph;
    norm_interface_table = ctxt.norm_interface_table;
    norm_top_decl      	= ctxt.norm_top_decl;
    norm_nested_exec   	= ctxt.norm_nested_exec
  }

let add_sigs_to_norm_context context siglist =
  let norm_context = copy_norm_context context in 
  let _ =
    List.iter 
      (fun ((cfname, arity), (tfname, cfunc_signature, fun_body_kind, upd)) ->
	RQNameIntHashtbl.add norm_context.norm_fun_sigs (cfname,arity) (tfname, cfunc_signature, fun_body_kind, upd)) siglist
  in
  norm_context

let add_sig_to_norm_context_in_place context sig1 =
  match sig1 with
  | ((cfname, arity), (tfname, cfunc_signature, fun_body_kind, upd)) ->
      RQNameIntHashtbl.add context.norm_fun_sigs (cfname,arity) (tfname, cfunc_signature, fun_body_kind, upd)

(* Extract components from the normalization context *)
let processing_context_from_norm_context norm_ctxt = norm_ctxt.norm_proc_context
let module_context_from_norm_context norm_ctxt = norm_ctxt.norm_module_context
let nsenv_from_norm_context norm_ctxt  	       = norm_ctxt.norm_nsenv
let cxschema_from_norm_context norm_ctxt       = norm_ctxt.norm_cxschema
let interface_table_from_norm_context norm_ctxt = norm_ctxt.norm_interface_table

let one_sig_from_norm_context context (cfname,arity) =
  let l = RQNameIntHashtbl.find_all context.norm_fun_sigs (cfname,arity) in
  match l with
  | [] ->
      raise (Query(Undefined(
		   " Signature of function " ^ (prefixed_string_of_rqname cfname) ^
		   " with arity " ^ (string_of_int arity) ^
		   " undefined")))
  | [(f,s,kind,u)] -> (s,kind,u)
  | (f,s,kind,u) :: sigs -> (s, kind, u)

let all_sigs_from_norm_context context (cfname,arity) =
  let l = RQNameIntHashtbl.find_all context.norm_fun_sigs (cfname,arity) in
  match l with
  | [] ->
      raise (Query(Undefined(
		   " Signature of function " ^ (prefixed_string_of_rqname cfname) ^
		   " with arity " ^ (string_of_int arity) ^
		   " undefined")))
  | l -> l

(* Variable scope *)

let register_var norm_context1 cvname =
  let norm_context2 = copy_norm_context norm_context1 in
  begin
    try
      let prev = RQNameHashtbl.find norm_context2.norm_vars cvname in
      RQNameHashtbl.replace norm_context2.norm_vars cvname (None :: prev)
    with
    | Not_found ->
	RQNameHashtbl.add norm_context2.norm_vars cvname (None :: [])
  end;
  norm_context2

let register_global_var norm_context1 cvname ce =
Debug.print_compile_debug ("Register_global_var "^(Namespace_names.prefixed_string_of_rqname cvname)^"\n");
  let norm_context2 = copy_norm_context norm_context1 in
  begin
    try
      let prev = RQNameHashtbl.find norm_context2.norm_vars cvname in
      RQNameHashtbl.replace norm_context2.norm_vars cvname ((Some ce) :: prev)
    with
    | Not_found ->
	RQNameHashtbl.add norm_context2.norm_vars cvname ((Some ce) :: [])
  end;
  norm_context2

let check_var norm_ctxt fi cvname =
  try
    match RQNameHashtbl.find norm_ctxt.norm_vars cvname with
    | [] -> raise Not_found
    | v1 :: _ -> v1
  with
  | Not_found -> dump_norm_context norm_ctxt; 
      raise
	(Query (Undefined_Variable
		  (fi, "Variable " , (prefixed_string_of_rqname cvname) ^ " is not in scope")))

let gen_new_cvar norm_ctxt h fi =
  let v = Namespace_generate.generate_name !(norm_ctxt.norm_next_variable) in
  (v, Xquery_core_ast_util.fmkcexpr (CEVar v) None fi)

let gen_new_cvar_typed norm_ctxt a eh fi =
  let v = Namespace_generate.generate_name !(norm_ctxt.norm_next_variable) in
  (v, Xquery_core_ast_util.fmkacexpr (CEVar v) a eh fi)

let set_in_execute_expr norm_context b = 
  norm_context.norm_nested_exec <- b

let get_in_execute_expr norm_context = norm_context.norm_nested_exec 

(* Var graph *)

let set_top_var_decl norm_ctxt v1 =
  norm_ctxt.norm_top_decl <- (Some (Norm_var_graph.GlobalVariable v1))

let set_top_fun_decl norm_ctxt f1 =
  norm_ctxt.norm_top_decl <- (Some (Norm_var_graph.FunctionDeclaration f1))

let unset_top_decl norm_ctxt =
  norm_ctxt.norm_top_decl <- None

let set_interface_table norm_context it = 
  norm_context.norm_interface_table <- it

let add_var_dependency norm_ctxt v2 =
  match norm_ctxt.norm_top_decl with
  | None -> ()
  | Some d1 -> Norm_var_graph.add_dependency norm_ctxt.norm_var_graph d1 (Norm_var_graph.GlobalVariable v2)
let add_fun_dependency norm_ctxt f2 =
  match norm_ctxt.norm_top_decl with
  | None -> ()
  | Some d1 -> Norm_var_graph.add_dependency norm_ctxt.norm_var_graph d1 (Norm_var_graph.FunctionDeclaration f2)

let check_cycles norm_ctxt =
  Norm_var_graph.check_cyclic_variables norm_ctxt.norm_var_graph

