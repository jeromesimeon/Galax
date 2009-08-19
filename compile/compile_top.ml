(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: compile_top.ml,v 1.28 2007/10/25 00:08:41 mff Exp $ *)

(* Module: Compile_top
   Description:
     This module compiles an XQuery toplevel statement or module into
     the ALGEBRA.
*)

open Xquery_core_ast
open Xquery_core_ast_util

open Xquery_algebra_ast
open Xquery_algebra_ast_util

open Compile_context

open Compile_util
open Compile_annotate
open Compile_expr

open Error

(*************)
(* Statement *)
(*************)

let compile_cexpr_annotate compile_ctxt cexpr = 
  let aop = compile_cexpr compile_ctxt cexpr in
    annotate_algebraic_expression aop;
    aop

let compile_statement compile_ctxt cstatement =
Debug.print_compile_debug ("In compile_statement\n");
Norm_context.dump_norm_context (norm_context_from_compile_context compile_ctxt);
  compile_cexpr_annotate compile_ctxt cstatement

(***********************)
(* Prolog declarations *)
(***********************)

(* Function definition *)

let compile_cfunction_body compile_ctxt cfunction_body =

  (* Function is either defined locally, is built-in, is an interface, or is imported *)
  match cfunction_body with
  | CEFunctionBltIn ->
      raise (Query (Compilation "Built-in function declarations should have been filtered before compilation"))
  | CEFunctionInterface -> 
      raise (Query (Compilation "Interface function declarations should have been filtered before compilation"))
  | CEFunctionImported -> AOEFunctionImported
  | CEFunctionUser cexpr1 ->
      let ao1 = AOEFunctionUser (compile_cexpr_annotate compile_ctxt cexpr1) in
      ao1

(* In here we add the function definition to the compilation context*)

let compile_cfunction_signature compile_ctxt (input_types,output_type) =
  List.map (compile_ctype compile_ctxt) input_types,
  compile_ctype compile_ctxt output_type

let compile_cfunction_def_desc compile_ctxt cfunction_def_desc =
  begin
    let cfname =
      match cfunction_def_desc with
      | ((cfname,arity),vname_list,cfunction_signature,cfunction_body,upd) ->
	  cfname
    in
    Debug.print_compile_debug ("\t\tCompiling function " ^ (Namespace_names.prefixed_string_of_rqname cfname));
  end;
  match cfunction_def_desc with
  | ((cfname,arity),vname_list,cfunction_signature,cfunction_body,upd) ->
      let vname_array = Array.of_list vname_list in
      let (input_types,output_type) = compile_cfunction_signature compile_ctxt cfunction_signature in
      let compiled_body = fmkalgop_function_body vname_array 
	  (compile_cfunction_body compile_ctxt cfunction_body) None (Some output_type) in
      add_function_to_compile_context compile_ctxt (cfname,arity) compiled_body;
      ((cfname,arity),(input_types,output_type), compiled_body, upd)

let compile_cfunction_def compile_context cfunction_def =
  let cfunction_def_desc = cfunction_def.pcfunction_def_desc in
  let fi                 = cfunction_def.pcfunction_def_loc in
  let fn_desc            =
    compile_cfunction_def_desc compile_context cfunction_def_desc
  in
  fmkalgop_function_decl fn_desc fi

(* Global variable declaration *)

let compile_cvar_decl compile_ctxt cvar_decl =
  let cvar_decl_desc = cvar_decl.pcvar_decl_desc in
  let fi             = cvar_decl.pcvar_decl_loc in
    match cvar_decl_desc with
      (* Variable is either defined locally, externally, or is imported *)
      | (vname,ocdt,CEVarUser cexpr1) ->
	  let ao1 = compile_cexpr_annotate compile_ctxt cexpr1 in
	  let oadt = compile_opt_ctype compile_ctxt ocdt in
	  let op_name = AOEVarDecl (oadt, vname) in
	  let indep = OneSub ao1 in
	  let dep = NoSub in
	    logical_aalgop_decl_mkop op_name indep dep fi 

      | (vname,ocdt,CEVarExternal) ->
	  let oadt = compile_opt_ctype compile_ctxt ocdt in
	  let op_name = AOEVarDeclExternal (oadt, vname) in
	  let indep = NoSub in
	  let dep = NoSub in
	    logical_aalgop_decl_mkop op_name indep dep fi
	    
      | (vname,ocdt,CEVarImported)  -> 
	  let oadt = compile_opt_ctype compile_ctxt ocdt in
	  let op_name = AOEVarDeclImported (oadt, vname) in
	  let indep = NoSub in
	  let dep = NoSub in
	    logical_aalgop_decl_mkop op_name indep dep fi

      | (vname,ocdt,CEVarInterface) ->
	  raise (Query (Compilation "Interface variable declarations should have been filtered before compilation"))
(* Key definitions *)

let compile_cindex_def compile_ctxt cindex_def =
  let fi = cindex_def.pcindex_def_loc in
    match cindex_def.pcindex_def_desc with
      | CValueIndex (str,cexpr1,cexpr2) ->
	  let ao1 = compile_cexpr_annotate compile_ctxt cexpr1 in
	  let ao2 = compile_cexpr_annotate compile_ctxt cexpr2 in
	  let op_name = AOEValueIndexDecl str in
	  let indep = OneSub ao1 in
	  let dep = OneSub ao2 in
	    logical_aalgop_decl_mkop op_name indep dep fi

      | CNameIndex cename ->
	  let relem_sym = Namespace_symbols.relem_symbol cename in
	  let op_name = AOENameIndexDecl relem_sym in
	  let indep = NoSub in
	  let dep = NoSub in
	    logical_aalgop_decl_mkop op_name indep dep fi

(**********)
(* Module *)
(**********)

let is_built_in_or_interface_function f =
  match f.pcfunction_def_desc with
  | (_,_,_,CEFunctionBltIn, _) 
  | (_,_,_,CEFunctionInterface, _) -> true
  | _ -> false

let is_interface_var v =
  match v.pcvar_decl_desc with
  | (_,_,CEVarInterface) -> true
  | _ -> false

(*

  Prolog compilation

  The bodies of the declarations for locally defined and imported
  functions and variables are compiled, but the declarations for
  interfaces and externally defined functions and variables are not,
  because their definitions are not available locally.

*)
let compile_prolog compile_context cprolog =
  Debug.print_compile_debug "Starting compilation of prolog";
  let non_builtin_or_interface_functions =
    List.filter (fun f -> not(is_built_in_or_interface_function f)) cprolog.pcprolog_functions
  in
  let non_interface_vars =
    List.filter (fun v -> not(is_interface_var v)) cprolog.pcprolog_vars
  in
  Debug.print_compile_debug "\tCompiling functions";
  let cfuns = List.map (compile_cfunction_def compile_context) non_builtin_or_interface_functions in
  Debug.print_compile_debug "\tCompiling variables";
  let cvars = List.map (compile_cvar_decl compile_context) non_interface_vars in
  Debug.print_compile_debug "\tCompiling indices";
  let cinds = List.map (compile_cindex_def compile_context) cprolog.pcprolog_indices in
  let prolog =
    { palgop_prolog_functions = cfuns;
      palgop_prolog_vars      = cvars;
      palgop_prolog_indices   = cinds }
  in
  Debug.print_compile_debug "Finished compilation of prolog";
  (compile_context,prolog)

let compile_xmodule compile_context cxmodule =
  let (compile_context',prolog') = compile_prolog compile_context cxmodule.pcmodule_prolog in
  let xmod =
    { palgop_module_prolog     = prolog';
      palgop_module_statements = List.map (compile_statement compile_context') cxmodule.pcmodule_statements }
  in
  (compile_context',xmod)

