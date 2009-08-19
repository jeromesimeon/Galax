(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: procmod_compiler.ml,v 1.44 2007/10/16 01:25:35 mff Exp $ *)

(* Module: Procmod_compiler
   Description:
     This module contains top-level operations over the processing
     model. This notably supports various levels of compilations for
     queries.
*)

open Error

open Xquery_common_ast
open Xquery_ast
open Xquery_core_ast
open Xquery_algebra_ast
open Logical_algebra_types
open Algebra_type

open Print_top

open Dm_atomic
open Physical_value

open Processing_context
open Parse_context
open Norm_context
open Typing_context
open Compile_context
open Code_selection_context
open Execution_context
open Monitoring_context

open Monitor
open Procmod_types
open Procmod_phases

open Compiled_program_units

(*******************************************)
(* Type for intermediate compilation steps *)
(*******************************************)

type ginput =
  | IOStatement of Galax_io.input_spec
  | IOProlog of Galax_io.input_spec
  | IOLibraryModule of Galax_io.input_spec
  | IOMainModule of Galax_io.input_spec

type ast =
  | ASTStatement of statement
  | ASTProlog of prolog
  | ASTLibraryModule of library_module
  | ASTMainModule of main_module

type core_ast =
  | CoreASTStatement of acstatement
  | CoreASTProlog of acprolog
  | CoreASTLibraryModule of acxmodule
  | CoreASTMainModule of acxmodule

type logical_plan_statement = logical_algop_expr
type logical_plan_main_module = logical_algop_xmodule

type logical_plan =
  | LogicalPlanStatement of logical_algop_expr
  | LogicalPlanProlog of logical_algop_prolog
  | LogicalPlanLibraryModule of logical_algop_xmodule
  | LogicalPlanMainModule of logical_algop_xmodule

type physical_plan =
  | PhysicalPlanStatement of algop_expr
  | PhysicalPlanProlog of algop_prolog
  | PhysicalPlanLibraryModule of algop_xmodule
  | PhysicalPlanMainModule of algop_xmodule


(********************)
(* External context *)
(********************)

type external_context =
    { external_context_item : item option;
      external_timezone     : atomicDayTimeDuration option;
      external_variables    : (cvname * item list) list }

let create_external_context proc_ctxt opt_ctxt_item opt_timezone var_val_list = 
  let nsenv = get_external_nsenv proc_ctxt in
  let interpret_variable (vname_string,vl) =
    let vname = Namespace_names.uqname_element_of_string vname_string in
    let cvname = Namespace_resolve.resolve_variable_qname nsenv vname in
    (cvname,vl)
  in
  let interpreted_variables = List.map interpret_variable var_val_list in
  { external_context_item = opt_ctxt_item;
    external_timezone     = opt_timezone;
    external_variables    = interpreted_variables }

let default_external_context () =
  { external_context_item = None;
    external_timezone     = None;
    external_variables    = [] }
  
(*
   If a context item is provided in the external context, then 
   we add an external declaration for $fs:dot. 
*)

let fix_prolog_with_external_context_item ext_ctxt_item prolog_ast = 
  if (ext_ctxt_item) then
    let dotvar =
      VarDef (Xquery_ast_util.fmkvar_decl 
		((Namespace_builtin.fs_prefix,"dot"), 
		 Some (Xquery_ast_util.fmksequencetype (ITItem, None) Finfo.bogus), EVarExternal) Finfo.bogus)
    in
    { pprolog_xschemas = prolog_ast.pprolog_xschemas;
      pprolog_contexts = prolog_ast.pprolog_contexts;
      pprolog_funcvars = dotvar :: prolog_ast.pprolog_funcvars;
      pprolog_indices  	= prolog_ast.pprolog_indices;
    } 
  else prolog_ast

let fix_main_module_with_external_context_item ext_ctxt_item main_module_ast =
  if (ext_ctxt_item) then
    let dotvar =
      VarDef (Xquery_ast_util.fmkvar_decl 
		((Namespace_builtin.fs_prefix,"dot"), 
		 Some (Xquery_ast_util.fmksequencetype (ITItem, None) Finfo.bogus), EVarExternal) Finfo.bogus)
    in
    let prolog_ast = main_module_ast.pmain_module_prolog in
    let prolog =
      { pprolog_xschemas = prolog_ast.pprolog_xschemas;
	pprolog_contexts = prolog_ast.pprolog_contexts;
	pprolog_funcvars = dotvar :: prolog_ast.pprolog_funcvars;
	pprolog_indices	= prolog_ast.pprolog_indices }
    in
    { pmain_module_prolog     = prolog;
      pmain_module_statements = main_module_ast.pmain_module_statements }
  else main_module_ast
  

(* set_external_context extends the algebra context with bindings of
   variables to values.  We currently do not have an interface for
   adding external bindings of variables to types. *)

let set_external_context opt_ext_ctxt compiled_prog main_mod =
  let alg_ctxt = compiled_prog.compiled_program_algebra_context in 
  let code_ctxt = main_mod.compiled_prolog_code_selection_context in

  match opt_ext_ctxt with
  | None -> ()
  | Some ext_ctxt -> 
      begin
	(match ext_ctxt.external_context_item with
	| None -> ()
	| Some v -> 
	    ignore(add_external_variable_value code_ctxt Xquery_common_ast.fs_dot [v]));
	(List.iter
	    (fun (vn, v) ->
	      (* item_list *)
	      ignore(add_external_variable_value code_ctxt vn v))
	    ext_ctxt.external_variables);
	(match ext_ctxt.external_timezone with
	  | None -> ()
	  | Some tz -> set_timezone alg_ctxt tz)
      end

(*************************)
(* Statement compilation *)
(*************************)

(* Compilation occurs within the context of a given compiled program. *)

(* Parsing *)

let apply_parsing_phase procmod_ctxt comp_prog ginput =
  (* 1. Obtain context information from the compiled program *)
  let proc_ctxt = processing_context_of_compiled_program comp_prog in

  (* 2. Parse *)
  match ginput with
  | IOStatement gio ->
      let parse_ctxt = build_xquery_parse_context proc_ctxt in
      let statement = procmod_ctxt.parsing_phase.parse_statement parse_ctxt gio in
      ASTStatement statement
  | IOProlog gio ->
      let parse_ctxt = build_xquery_parse_context proc_ctxt in
      let (_,prolog) = procmod_ctxt.parsing_phase.parse_prolog parse_ctxt gio in
      ASTProlog prolog
  | IOLibraryModule gio ->
      let parse_ctxt = build_xquery_parse_context proc_ctxt in
      let (_,library_module) = procmod_ctxt.parsing_phase.parse_library_module parse_ctxt gio in
      ASTLibraryModule library_module
  | IOMainModule gio ->
      let parse_ctxt = build_xquery_parse_context proc_ctxt in
      let (_,main_module) = procmod_ctxt.parsing_phase.parse_main_module parse_ctxt gio in
      ASTMainModule main_module

(* Pre-processing

   Returns transitive closure of imported interfaces and modules that need to be compiled. 
*)
let apply_preprocessing_phase procmod_ctxt comp_prog ast =
  let proc_ctxt = processing_context_of_compiled_program comp_prog in
  match ast with
  | ASTStatement statement -> (Hashtbl.create 1, [])
  | ASTProlog prolog ->
      procmod_ctxt.preprocessing_phase.preprocess_prolog proc_ctxt prolog 
  | ASTLibraryModule library_module ->
      procmod_ctxt.preprocessing_phase.preprocess_library_module proc_ctxt library_module
  | ASTMainModule main_module ->
      procmod_ctxt.preprocessing_phase.preprocess_main_module proc_ctxt main_module

(* Normalization *)

let apply_normalize_phase procmod_ctxt comp_prog comp_mod ast =
   let norm_ctxt = norm_context_of_module comp_mod in 

  (* 2. Normalization *)
  match ast with
  | ASTStatement statement ->
      let cstatement =
	procmod_ctxt.normalization_phase.normalize_statement norm_ctxt statement
      in
      (norm_ctxt, CoreASTStatement cstatement)
  | ASTProlog prolog ->
      let (norm_ctxt',cprolog) =
	procmod_ctxt.normalization_phase.normalize_prolog (comp_prog.compiled_program_interfaces, norm_ctxt) prolog
      in
      (norm_ctxt',CoreASTProlog cprolog)
  | ASTLibraryModule library_module ->
      let (norm_ctxt',cxmodule) =
	procmod_ctxt.normalization_phase.normalize_library_module 
	  (comp_prog.compiled_program_interfaces, norm_ctxt) library_module
      in
      (norm_ctxt',CoreASTLibraryModule cxmodule)
  | ASTMainModule main_module ->
      let (norm_ctxt',cxmodule) =
	procmod_ctxt.normalization_phase.normalize_main_module 
	  (comp_prog.compiled_program_interfaces, norm_ctxt) main_module
      in
      (norm_ctxt',CoreASTMainModule cxmodule)

let apply_rewriting_phase procmod_ctxt comp_mod norm_ctxt core_ast =
  (* 1. Obtain context information from the compiled module *)
  let orig_stat_ctxt = static_context_of_module comp_mod in

  (* 2. Set up the new static context *)
  let new_stat_ctxt =
    replace_norm_context_in_static_context norm_ctxt orig_stat_ctxt
  in
  match core_ast with
  | CoreASTStatement acstatement ->
      let rtcstatement =
	procmod_ctxt.rewriting_phase.rewriting_statement new_stat_ctxt acstatement
      in
      (new_stat_ctxt, CoreASTStatement rtcstatement)
  | CoreASTProlog acprolog ->
      let (new_stat_ctxt',rtcprolog) =
	procmod_ctxt.rewriting_phase.rewriting_prolog new_stat_ctxt acprolog
      in
      (new_stat_ctxt',CoreASTProlog rtcprolog)
  | CoreASTLibraryModule acxmodule ->
      let (new_stat_ctxt',rtcxmodule) =
	procmod_ctxt.rewriting_phase.rewriting_library_module new_stat_ctxt acxmodule
      in
      (new_stat_ctxt',CoreASTLibraryModule rtcxmodule)
  | CoreASTMainModule acxmodule ->
      let (new_stat_ctxt',rtcxmodule) =
	procmod_ctxt.rewriting_phase.rewriting_main_module new_stat_ctxt acxmodule
      in
      (new_stat_ctxt',CoreASTMainModule rtcxmodule)

let apply_factorization_phase procmod_ctxt comp_mod new_stat_ctxt core_ast =
  match core_ast with
  | CoreASTStatement acstatement ->
      let rtcstatement =
	procmod_ctxt.factorization_phase.factorize_statement new_stat_ctxt acstatement
      in
      (new_stat_ctxt,CoreASTStatement rtcstatement)
  | CoreASTProlog acprolog ->
      let (new_stat_ctxt',rtcprolog) =
	procmod_ctxt.factorization_phase.factorize_prolog new_stat_ctxt acprolog
      in
      (new_stat_ctxt',CoreASTProlog rtcprolog)
  | CoreASTLibraryModule acxmodule ->
      let (new_stat_ctxt',rtcxmodule) =
	procmod_ctxt.factorization_phase.factorize_library_module new_stat_ctxt acxmodule
      in
      (new_stat_ctxt',CoreASTLibraryModule rtcxmodule)
  | CoreASTMainModule acxmodule ->
      let (new_stat_ctxt',rtcxmodule) =
	procmod_ctxt.factorization_phase.factorize_main_module new_stat_ctxt acxmodule
      in
      (new_stat_ctxt',CoreASTMainModule rtcxmodule)

let apply_compile_phase procmod_ctxt comp_mod new_rewriting_ctxt rewritten_core_ast =
  (* 1. Obtain context information from the compiled module *)
  let orig_comp_ctxt = compile_context_of_module comp_mod in
  (* 2. Set up the new compilation context. *)
  let new_comp_ctxt =
    replace_static_context_in_compile_context new_rewriting_ctxt orig_comp_ctxt
  in
  match rewritten_core_ast with
  | CoreASTStatement rtcstatement ->
      let statement_plan =
	procmod_ctxt.compile_phase.compile_statement new_comp_ctxt rtcstatement
      in
      (new_comp_ctxt,LogicalPlanStatement statement_plan)
  | CoreASTProlog rtcprolog ->
      let (new_comp_ctxt',prolog_plan) =
	procmod_ctxt.compile_phase.compile_prolog new_comp_ctxt rtcprolog
      in
      (new_comp_ctxt',LogicalPlanProlog prolog_plan)
  | CoreASTLibraryModule rtcxmodule ->
      let (new_comp_ctxt',library_module_plan) =
	procmod_ctxt.compile_phase.compile_library_module new_comp_ctxt rtcxmodule
      in
      (new_comp_ctxt',LogicalPlanLibraryModule library_module_plan)
  | CoreASTMainModule rtcxmodule ->
      let (new_comp_ctxt',main_module_plan) =
	procmod_ctxt.compile_phase.compile_main_module new_comp_ctxt rtcxmodule
      in
      (new_comp_ctxt',LogicalPlanMainModule main_module_plan)

let apply_optimization_phase procmod_ctxt comp_mod new_comp_ctxt logical_plan =
  match logical_plan with
  | LogicalPlanStatement statement_plan ->
      let statement_plan' =
	procmod_ctxt.optimization_phase.optimize_statement new_comp_ctxt statement_plan
      in	
      (new_comp_ctxt,LogicalPlanStatement statement_plan')
  | LogicalPlanProlog prolog_plan ->
      let (new_comp_ctxt',prolog_plan') =
	procmod_ctxt.optimization_phase.optimize_prolog new_comp_ctxt prolog_plan
      in
      (new_comp_ctxt',LogicalPlanProlog prolog_plan')
  | LogicalPlanLibraryModule library_module_plan ->
      let (new_comp_ctxt',library_module_plan') =
	procmod_ctxt.optimization_phase.optimize_library_module new_comp_ctxt library_module_plan
      in
      (new_comp_ctxt',LogicalPlanLibraryModule library_module_plan')
  | LogicalPlanMainModule main_module_plan ->
      let (new_comp_ctxt',main_module_plan') =
	procmod_ctxt.optimization_phase.optimize_main_module new_comp_ctxt main_module_plan
      in
      (new_comp_ctxt',LogicalPlanMainModule main_module_plan')

let apply_code_selection_phase comp_prog comp_mod new_comp_ctxt logical_plan =
  let proc_ctxt = processing_context_of_compiled_program comp_prog in 
  let procmod_ctxt = phase_handler_of_processing_context proc_ctxt in
  (* 1. Obtain context information from the compiled module *)
  let orig_cs_ctxt   = code_selection_context_of_module comp_mod in
    (* 2. Set up the new code selection context. *)
  let code_ctxt =
    replace_compile_context_in_code_selection_context (Cs_annotate.annotate_context new_comp_ctxt)  orig_cs_ctxt
  in
  match logical_plan with
  | LogicalPlanStatement statement_plan ->
      let (code_ctxt',statement_plan') =
	procmod_ctxt.selection_phase.selection_statement (comp_prog, comp_mod.compiled_prolog_uri, code_ctxt) statement_plan
      in
      (code_ctxt', PhysicalPlanStatement statement_plan')
  | LogicalPlanProlog prolog_plan ->
      let (code_ctxt',prolog_plan') =
	procmod_ctxt.selection_phase.selection_prolog (comp_prog, comp_mod.compiled_prolog_uri, code_ctxt) prolog_plan
      in
      (code_ctxt',PhysicalPlanProlog prolog_plan')
  | LogicalPlanLibraryModule library_module_plan ->      
      let (code_ctxt',library_module_plan') =
	procmod_ctxt.selection_phase.selection_library_module (comp_prog, comp_mod.compiled_prolog_uri, code_ctxt) library_module_plan
      in
      (code_ctxt',PhysicalPlanLibraryModule library_module_plan')
  | LogicalPlanMainModule main_module_plan ->
      let (code_ctxt',main_module_plan') =
	procmod_ctxt.selection_phase.selection_main_module (comp_prog, comp_mod.compiled_prolog_uri, code_ctxt) main_module_plan
      in
      (code_ctxt',PhysicalPlanMainModule main_module_plan')

let update_compiled_program_with_compiled_module comp_prog (comp_mod) code_ctxt physical_plan = 
  let uri = comp_mod.compiled_prolog_uri in 
  begin
    update_code_selection_context_in_compiled_prolog comp_mod code_ctxt;
      match physical_plan with
    | PhysicalPlanStatement physical_statement_plan -> ()
    | PhysicalPlanLibraryModule xmodule ->
	begin
	  let (plan, _) = Xquery_algebra_ast_util.split_main_module_plan xmodule in 
	  update_physical_plan_in_compiled_prolog comp_mod plan;
	  Hashtbl.add comp_prog.compiled_program_library_module_table uri (comp_mod)
	end
    | PhysicalPlanProlog plan ->
	begin
	  update_physical_plan_in_compiled_prolog comp_mod plan;
	  comp_prog.compiled_program_main_module <- Some (comp_mod, [])
	end
    | PhysicalPlanMainModule xmodule ->
	begin
	  let (plan, statements) = Xquery_algebra_ast_util.split_main_module_plan xmodule in 
	  update_physical_plan_in_compiled_prolog comp_mod plan;
	  comp_prog.compiled_program_main_module <- Some (comp_mod, statements)
	end
  end


(*************************************)
(* Compositions of compilation steps *)
(*************************************)

let aux_compile_physical_plan_from_ast comp_prog ast =
  let proc_ctxt = processing_context_of_compiled_program comp_prog in 
  let procmod_ctxt = phase_handler_of_processing_context proc_ctxt in
(* print_string("in aux-compile_physical_plan\n");  *)

  let uri, comp_mod = 
    match ast with 
    | ASTStatement _ -> 
         (* For statements, we compile in context of main module *)
	  let (comp_mod, _) = main_module_of_compiled_program comp_prog in
          (main_module_uri, comp_mod)
    | ASTLibraryModule library_module -> 
	begin
	  let (prefix, uri, optint) = library_module.plibrary_module_decl in 
	  (try
            (* If library module is already defined, warn that we're ignoring it *)
	    ignore(Hashtbl.find comp_prog.compiled_program_library_module_table uri);
	    Error.eprintf_warning ("Library module '"^uri^"' multiply defined: Using last one")
	  with
	  | Not_found -> ()); 
	  (uri, default_compiled_module comp_prog uri)
	end
    | ASTProlog _ 
    | ASTMainModule _ -> 
         (* Any number of prologs/modules can be imported, upto the main module. *)
	let (comp_mod, stmts) = main_module_of_compiled_program comp_prog in
        if (stmts != []) then 
          raise (Query(Internal_Error("Main module is already defined")))
	else
          (main_module_uri, comp_mod)
  in

(* print_string("in aux-compile_physical_plan "^uri^"\n");  *)

  (* 4. Normalization *)
  let (new_norm_ctxt,core_ast) =
    apply_normalize_phase procmod_ctxt comp_prog comp_mod ast
  in

  (* 5. Rewriting *)
  let (new_rewriting_ctxt,rewritten_core_ast) =
    apply_rewriting_phase procmod_ctxt comp_mod new_norm_ctxt core_ast
  in

  (* 6. Factorization *)
  let (new_factorized_ctxt,factorized_core_ast) =
    apply_factorization_phase procmod_ctxt comp_mod new_rewriting_ctxt rewritten_core_ast
  in

  (* 7. Compilation *)
  let (new_comp_ctxt,logical_plan) =
    apply_compile_phase procmod_ctxt comp_mod new_factorized_ctxt factorized_core_ast
  in

  (* 8. Optimization *)
  let (new_comp_ctxt,optimized_logical_plan) =
    apply_optimization_phase procmod_ctxt comp_mod new_comp_ctxt logical_plan
  in

  (* 9. Code selection *)  
  let (new_code_ctxt,physical_plan) = 
    apply_code_selection_phase comp_prog comp_mod new_comp_ctxt optimized_logical_plan
  in

  (* 10. If this module is library or main module, add its definition to the compiled_program *)
  update_compiled_program_with_compiled_module comp_prog (comp_mod) new_code_ctxt physical_plan;
(* print_string("EXIT aux-compile_physical_plan "^uri^"\n");  *)
  (comp_prog, physical_plan)
    

(* Compile a physical plan from an AST *)
let compile_physical_plan_from_ast comp_prog ast =
  (* 1. Get processing context *)
  let proc_ctxt = processing_context_of_compiled_program comp_prog in

  (* 2. Get processing phase handler *)
  let procmod_ctxt = phase_handler_of_processing_context proc_ctxt in

(* print_string("before preprocessing\n");  *)

  (* 3. Module pre-processing *)
  let (imported_interface_table, imported_library_modules) = 
   apply_preprocessing_phase procmod_ctxt comp_prog ast 
  in
  (* 4. Add all interfaces to the program's interface table.
        TODO: check whether interface is re-defined.
  *)  
(* print_string("before adding interfaces\n");  *)
  Hashtbl.iter 
    (fun uri i ->
      Hashtbl.add comp_prog.compiled_program_interfaces uri i)
    imported_interface_table;

  (* 5. Cache the names of the library modules in imported order *)
  let imported_module_names = 
    List.map (fun m -> let (prefix, uri, optint) = m.plibrary_module_decl in uri) imported_library_modules 
  in
  comp_prog.compiled_program_library_list <- comp_prog.compiled_program_library_list@imported_module_names; 

(* print_string("before compiling library\n");  *)
  (* 6. Compile all the imported library modules. *)
  List.iter (fun m -> 
    let (_,_) = aux_compile_physical_plan_from_ast comp_prog (ASTLibraryModule m)
    in ()) 
    imported_library_modules;

  (* 7. Compile this AST. *)
  aux_compile_physical_plan_from_ast comp_prog ast

let aux_compile_logical_plan_from_ast comp_prog ast =
  let proc_ctxt = processing_context_of_compiled_program comp_prog in 
  let procmod_ctxt = phase_handler_of_processing_context proc_ctxt in

(* print_string("in aux-compile_physical_plan\n");  *)

  let uri, comp_mod = 
    match ast with 
    | ASTStatement _ -> 
         (* For statements, we compile in context of main module *)
	  let (comp_mod, _) = main_module_of_compiled_program comp_prog in
          (main_module_uri, comp_mod)
    | ASTLibraryModule library_module -> 
	begin
	  let (prefix, uri, optint) = library_module.plibrary_module_decl in 
	  (try
            (* If library module is already defined, warn that we're ignoring it *)
	    ignore(Hashtbl.find comp_prog.compiled_program_library_module_table uri);
	    Error.eprintf_warning ("Library module '"^uri^"' multiply defined: Using last one")
	  with
	  | Not_found -> ()); 
	  (uri, default_compiled_module comp_prog uri)
	end
    | ASTProlog _ 
    | ASTMainModule _ -> 
         (* Any number of prologs/modules can be imported, upto the main module. *)
	let (comp_mod, stmts) = main_module_of_compiled_program comp_prog in
        if (stmts != []) then 
          raise (Query(Internal_Error("Main module is already defined")))
	else
          (main_module_uri, comp_mod)
  in

(* print_string("in aux-compile_physical_plan "^uri^"\n");  *)

  (* 4. Normalization *)
  let (new_norm_ctxt,core_ast) =
    apply_normalize_phase procmod_ctxt comp_prog comp_mod ast
  in

  (* 5. Rewriting *)
  let (new_rewriting_ctxt,rewritten_core_ast) =
    apply_rewriting_phase procmod_ctxt comp_mod new_norm_ctxt core_ast
  in

  (* 6. Factorization *)
  let (new_factorized_ctxt,factorized_core_ast) =
    apply_factorization_phase procmod_ctxt comp_mod new_rewriting_ctxt rewritten_core_ast
  in

  (* 7. Compilation *)
  let (new_comp_ctxt,logical_plan) =
    apply_compile_phase procmod_ctxt comp_mod new_factorized_ctxt factorized_core_ast
  in

  (* 8. Optimization *)
  let (new_comp_ctxt,optimized_logical_plan) =
    apply_optimization_phase procmod_ctxt comp_mod new_comp_ctxt logical_plan
  in

  (* 9. If this module is library or main module, add its definition to the compiled_program *)
(*  update_compiled_program_with_compiled_module comp_prog (comp_mod, uri) new_comp_ctxt optimized_logical_plan; *)
(* print_string("EXIT aux-compile_physical_plan "^uri^"\n");  *)
  (comp_prog, (new_comp_ctxt,optimized_logical_plan))
    

(* Compile a physical plan from an AST *)
let compile_logical_plan_from_ast comp_prog ast =
  (* 1. Get processing context *)
  let proc_ctxt = processing_context_of_compiled_program comp_prog in

  (* 2. Get processing phase handler *)
  let procmod_ctxt = phase_handler_of_processing_context proc_ctxt in

(* print_string("before preprocessing\n");  *)

  (* 3. Module pre-processing *)
  let (imported_interface_table, imported_library_modules) = 
   apply_preprocessing_phase procmod_ctxt comp_prog ast 
  in
  (* 4. Add all interfaces to the program's interface table.
        TODO: check whether interface is re-defined.
  *)  
(* print_string("before adding interfaces\n");  *)
  Hashtbl.iter 
    (fun uri i ->
      Hashtbl.add comp_prog.compiled_program_interfaces uri i)
    imported_interface_table;

  (* 5. Cache the names of the library modules in imported order *)
  let imported_module_names = 
    List.map (fun m -> let (prefix, uri, optint) = m.plibrary_module_decl in uri) imported_library_modules 
  in
  comp_prog.compiled_program_library_list <-
    comp_prog.compiled_program_library_list@imported_module_names; 

(* print_string("before compiling library\n");  *)
  (* 6. Compile all the imported library modules. *)
  List.iter (fun m -> 
    let (_,_) = aux_compile_logical_plan_from_ast comp_prog (ASTLibraryModule m)
    in ()) 
    imported_library_modules;

  (* 7. Compile this AST. *)
  aux_compile_logical_plan_from_ast comp_prog ast

(* Compile a physical plan from an input *)
let compile_physical_plan_from_input comp_prog ginput =
  (* 1. Get processing context *)
  let proc_ctxt = processing_context_of_compiled_program comp_prog in

  (* 2. Create a processing phase handler *)
  let procmod_ctxt = phase_handler_of_processing_context proc_ctxt in

  (* 3. Parsing *)
  let ast =
    apply_parsing_phase procmod_ctxt comp_prog ginput
  in
  compile_physical_plan_from_ast comp_prog ast

(*
   Compile a physical plan from a logical plan. 
*)
let compile_physical_plan_from_logical_plan comp_prog (comp_mod) (new_comp_ctxt,logical_plan) =
  (* 1. Get processing context *)
  let proc_ctxt = processing_context_of_compiled_program comp_prog in

  (* 2. Create a processing phase handler *)
  let procmod_ctxt = phase_handler_of_processing_context proc_ctxt in

  (* 3. Optimization *)
  let (new_comp_ctxt,optimized_logical_plan) =
    apply_optimization_phase procmod_ctxt comp_mod new_comp_ctxt logical_plan
  in

  (* 4. Code selection *)
  let (new_code_ctxt,physical_plan) = 
    apply_code_selection_phase comp_prog comp_mod new_comp_ctxt optimized_logical_plan
  in

  (* 5. If this module is a library or main module, add its definition to the compiled_program *)
  update_compiled_program_with_compiled_module comp_prog (comp_mod) new_code_ctxt physical_plan;
  (comp_prog, physical_plan)

(* 
   Compile a physical plan from a logical plan.
 *)
let compile_physical_plan_from_optimized_logical_plan comp_prog (comp_mod) (new_comp_ctxt,optimized_logical_plan) =
  (* 1. Code selection *)
  let (new_code_ctxt,physical_plan) = 
    apply_code_selection_phase comp_prog comp_mod new_comp_ctxt optimized_logical_plan
  in

  (* 2. If this module is library or main module, add its definition to the compiled_program *)
  update_compiled_program_with_compiled_module comp_prog (comp_mod) new_code_ctxt physical_plan;
  (comp_prog, physical_plan)

(*************************)
(* Statement compilation *)
(*************************)

let logical_compile_statement comp_prog gio =
  (* 0. Get processing context *)
  let proc_ctxt = processing_context_of_compiled_program comp_prog in

  (* 1. Set up the processing context *)
  let procmod_ctxt = phase_handler_of_processing_context proc_ctxt in

  (* 2. Parse the statement *)
  let parse_ctxt = build_xquery_parse_context proc_ctxt in
  let statement = procmod_ctxt.parsing_phase.parse_statement parse_ctxt gio in

  (* 3. Compile the statement *)
  let (comp_prog', (_,plan)) = 
    compile_logical_plan_from_ast comp_prog (ASTStatement statement) 
  in
  match plan with
  | LogicalPlanStatement statement_plan ->
      (comp_prog', statement_plan)
  | _ ->
      raise (Query (Compilation "Statement compilation failed"))

let compile_statement comp_prog gio =
  (* 0. Get processing context *)
  let proc_ctxt = processing_context_of_compiled_program comp_prog in

  (* 1. Set up the processing context *)
  let procmod_ctxt = phase_handler_of_processing_context proc_ctxt in

  (* 2. Parse the statement *)
  let parse_ctxt = build_xquery_parse_context proc_ctxt in
  let statement = procmod_ctxt.parsing_phase.parse_statement parse_ctxt gio in

  (* 3. Compile the statement *)
  let (comp_prog', physical_plan) = 
    compile_physical_plan_from_ast comp_prog (ASTStatement statement) 
  in
  match physical_plan with
  | PhysicalPlanStatement physical_statement_plan ->
      (comp_prog', physical_statement_plan)
  | _ ->
      raise (Query (Compilation "Statement compilation failed"))

(* Statement that is compiled in context of some module *)
let compile_statement_from_logical_plan comp_prog comp_mod (statement_plan) =
  let new_comp_ctxt = compile_context_of_module comp_mod in
  let (comp_prog', physical_plan) =
    compile_physical_plan_from_logical_plan comp_prog 
      (comp_mod) (new_comp_ctxt,(LogicalPlanStatement statement_plan))
  in
  match physical_plan with
  | PhysicalPlanStatement physical_statement_plan ->
      (comp_prog', physical_statement_plan)
  | _ ->
      raise (Query (Compilation "Statement compilation failed"))

(* Statement that is compiled in context of main module *)
(* 
let compile_statement_from_logical_plan comp_prog (statement_plan) =
  let (comp_mod,_) = main_module_of_compiled_program comp_prog in 
  compile_statement_from_logical_plan_in_module comp_prog (comp_mod) statement_plan
*)

let compile_statement_from_optimized_logical_plan comp_prog (optimized_statement_plan) =
  let (comp_mod,_) = main_module_of_compiled_program comp_prog in 
  let new_comp_ctxt = compile_context_of_main_module comp_prog in
  let (comp_prog', physical_plan) =
    compile_physical_plan_from_optimized_logical_plan comp_prog 
      (comp_mod) (new_comp_ctxt,(LogicalPlanStatement optimized_statement_plan))
  in
  match physical_plan with
  | PhysicalPlanStatement physical_statement_plan ->
      (comp_prog', physical_statement_plan)
  | _ ->
      raise (Query (Compilation "Statement compilation failed"))

(**********************)
(* Prolog compilation *)
(**********************)

(* Note:
     This function compiles an XQuery prolog in the context of a given
     compiled module.
   - Jerome
*)

let compile_prolog ext_ctxt_item comp_prog gio =
  (* 0. Get processing context *)
  let proc_ctxt = processing_context_of_compiled_program comp_prog in

  (* 1. Set up the processing context *)
  let procmod_ctxt = phase_handler_of_processing_context proc_ctxt in

  (* 2. Parse the library module *)
  let parse_ctxt = build_xquery_parse_context proc_ctxt in
  let (new_parse_ctxt,prolog) = procmod_ctxt.parsing_phase.parse_prolog parse_ctxt gio in

  (* 3. Fix-up the prolog with external context item *)
  let prolog = fix_prolog_with_external_context_item ext_ctxt_item prolog in

  (* 4. Compile the prolog *)
  let (comp_prog',physical_plan) =
    compile_physical_plan_from_ast comp_prog (ASTProlog prolog)
  in
  comp_prog'

(**********************)
(* Module compilation *)
(**********************)

let compile_library_module comp_prog gio =
  (* 0. Get processing context *)
  let proc_ctxt = processing_context_of_compiled_program comp_prog in

  (* 1. Set up the processing context *)
  let procmod_ctxt = phase_handler_of_processing_context proc_ctxt in

  (* 2. Parse the library module *)
  let parse_ctxt = build_xquery_parse_context proc_ctxt in
  let (new_parse_ctxt,library_module) = procmod_ctxt.parsing_phase.parse_library_module parse_ctxt gio in

  (* 3. Compile the library_module *)
  let (comp_prog', plan) = compile_physical_plan_from_ast comp_prog (ASTLibraryModule library_module) in
  let (prefix, uri, optint) = library_module.plibrary_module_decl in
  (prefix, uri, comp_prog')

let logical_compile_main_module ext_ctxt_item comp_prog gio =
  (* 0. Get processing context *)
  let proc_ctxt = processing_context_of_compiled_program comp_prog in

  (* 1. Set up the processing context *)
  let procmod_ctxt = phase_handler_of_processing_context proc_ctxt in

  (* 2. Parse the main module *)
  let parse_ctxt = build_xquery_parse_context proc_ctxt in
  let (new_parse_ctxt,main_module) = procmod_ctxt.parsing_phase.parse_main_module parse_ctxt gio in

  (* 3. Fix-up the prolog with external context item *)
  let main_module = fix_main_module_with_external_context_item ext_ctxt_item main_module in

  (* 5. Compile the whole module *)
  let (comp_prog',(logical_ctxt,logical_plan)) = 
    compile_logical_plan_from_ast comp_prog (ASTMainModule main_module)
  in

  (* 6. Extract the result *)
  let logical_main_module_plan =
    match logical_plan with
    | LogicalPlanMainModule logical_main_module_plan -> logical_main_module_plan
    | _ -> raise (Query (Compilation "Prolog compilation failed"))
  in

  (* 7. Separate the prolog from the statements *) 
  (comp_prog', (logical_ctxt, logical_main_module_plan))

let compile_main_module ext_ctxt_item comp_prog gio =
  (* 0. Get processing context *)
  let proc_ctxt = processing_context_of_compiled_program comp_prog in

  (* 1. Set up the processing context *)
  let procmod_ctxt = phase_handler_of_processing_context proc_ctxt in

  (* 2. Parse the main module *)
  let parse_ctxt = build_xquery_parse_context proc_ctxt in
  let (new_parse_ctxt,main_module) = procmod_ctxt.parsing_phase.parse_main_module parse_ctxt gio in

  (* 3. Fix-up the prolog with external context item *)
  let main_module = fix_main_module_with_external_context_item ext_ctxt_item main_module in

  (* 5. Compile the whole module *)
  let (comp_prog',physical_plan) =
    compile_physical_plan_from_ast comp_prog (ASTMainModule main_module)
  in

  (* 6. Extract the result *)
  let physical_main_module_plan =
    match physical_plan with
    | PhysicalPlanMainModule physical_main_module_plan -> physical_main_module_plan
    | _ -> raise (Query (Compilation "Prolog compilation failed"))
  in

  (* 7. Separate the prolog from the statements *)
  (comp_prog', physical_main_module_plan.palgop_module_statements)

let compile_main_module_from_logical_plan comp_prog (new_comp_ctxt,logical_plan) =
  (* 0. Get processing context *)
  let comp_mod = 
    if (main_module_defined_in_compiled_program comp_prog) then 
      raise (Query(Internal_Error("Main module is already defined")))
    else default_compiled_module comp_prog main_module_uri
  in
  (* 4. Compile the prolog *)
  let (comp_prog',comp_main_module) =
    compile_physical_plan_from_logical_plan comp_prog (comp_mod) 
      (new_comp_ctxt, LogicalPlanMainModule logical_plan)
  in
  let physical_plan= 
    match comp_main_module with
    | PhysicalPlanMainModule physical_main_module_plan ->
	physical_main_module_plan
    | _ ->
	raise (Query (Compilation "Main module compilation failed"))
  in
  (comp_prog',physical_plan.palgop_module_statements)


let compile_main_module_from_optimized_logical_plan comp_prog (new_comp_ctxt,optimized_logical_plan) =
  let comp_mod = 
    if (main_module_defined_in_compiled_program comp_prog) then 
      raise (Query(Internal_Error("Main module is already defined")))
    else default_compiled_module comp_prog main_module_uri
  in
  (* 1. Compile the prolog *)
  let (comp_prog', compiled_physical_plan) =
    compile_physical_plan_from_optimized_logical_plan comp_prog (comp_mod) 
      (new_comp_ctxt,LogicalPlanMainModule optimized_logical_plan)
  in
  let physical_plan = 
    match compiled_physical_plan with
    | PhysicalPlanMainModule physical_main_module_plan ->
	physical_main_module_plan
    | _ ->
	raise (Query (Compilation "Main module compilation failed"))
  in
  (comp_prog',physical_plan.palgop_module_statements)

(***************************************)
(* Standard library module compilation *)
(***************************************)

let compile_standard_library_module proc_ctxt =
  (* Make sure printing is off for stdlib *)
  let printing = !Conf.print_global in
  Conf.print_global := false;
  if Debug.default_debug()
  then Debug.print_default_debug "Parsing pervasives";
  let stdlib_io = 
    try
      (Galax_io.String_Input !Conf.pervasive_content)
    with
    | (Sys_error _) -> (Galax_io.File_Input "pervasive.xq")
  in

  (* Parse & normalize the standard-library module *)
  let parse_ctxt = Parse_context.build_xquery_parse_context proc_ctxt in
  let (_,std_module) = Parse_top.parse_library_module_from_io parse_ctxt stdlib_io in
  let norm_ctxt = Norm_context.default_norm_context proc_ctxt in 

(* TODO! This should be using the procmod_ctxt to determine which function to call! *)
  let mod_proc_ctxt = Norm_context.module_context_from_norm_context norm_ctxt in 
  mod_proc_ctxt.system <- true;
  let (norm_ctxt, std_cmodule) = 
    Norm_top.normalize_library_module (Hashtbl.create 11, norm_ctxt) std_module
  in

  (* Return a new compiled program and empty main module *)
  let comp_prog = default_compiled_program proc_ctxt norm_ctxt in
  comp_prog.compiled_program_main_module <- 
    Some (default_compiled_module comp_prog main_module_uri, []);
  
    (* Turn printing back to its original setting *)
  Conf.print_global := printing;
  comp_prog

(*****************************)
(* Execute a compiled module *)
(*****************************)

let prepare_compiled_program opt_ext_ctxt comp_prog =
  let proc_ctxt = processing_context_of_compiled_program comp_prog in
  let alg_ctxt = algebra_context_of_compiled_program comp_prog in 
  let procmod_ctxt = phase_handler_of_processing_context proc_ctxt in

  (* Initialize the external context right before execution *)
  let (main_mod,_) = main_module_of_compiled_program comp_prog in
  set_external_context opt_ext_ctxt comp_prog main_mod;

  (* Prepare all the library modules *)
  List.iter 
    (fun mod_name ->  
      let library_mod = module_of_compiled_program comp_prog mod_name in 
      let _ = 
	procmod_ctxt.evaluation_phase.eval_prolog
	  alg_ctxt
	  library_mod.compiled_prolog_plan
      in ()
    ) 
    comp_prog.compiled_program_library_list;

  (* Prepare the main module *)
  let _ =
    procmod_ctxt.evaluation_phase.eval_prolog
      alg_ctxt
      main_mod.compiled_prolog_plan
  in
  comp_prog

(********************************)
(* Execute a compiled statement *)
(********************************)

let execute_compiled_statement prepared_program comp_statement =
  (* 0. Get processing context *)
  let proc_ctxt = processing_context_of_compiled_program prepared_program in
  let procmod_ctxt = phase_handler_of_processing_context proc_ctxt in
  let alg_ctxt = prepared_program.compiled_program_algebra_context in
  procmod_ctxt.evaluation_phase.eval_statement 
    (* TODO Need to send entire prepared_program *) alg_ctxt comp_statement

(* let _ = print_string("Procmod_compiler\n") *)
