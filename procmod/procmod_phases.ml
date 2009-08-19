(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: procmod_phases.ml,v 1.82 2008/02/01 02:45:26 mff Exp $ *)

(* Module: Procmod_phase
   Description:
     This module supports a modular approach to the XQuery processing
     model. A structure is used to describe each processing phase and
     can be used to register the appropriate call for each
     phase. Currently, the signature of the call for each phase is
     *fixed*.
*)

open Format

open Error
open Galax_io

open Print_top

open Monitoring_context
open Processing_context
open Parse_context
open Norm_context
open Typing_context
open Compile_context
open Code_selection_context
open Execution_context

open Monitor

open Xquery_ast
open Xquery_core_ast
open Xquery_algebra_ast

open Algebra_type

open Physical_value_util
open Rewriting_rules_notyping
open Rewriting_rules_typing

open Procmod_types


(************)
(* Printers *)
(************)

let phase_print_flag phase =
  match phase with
  | Parsing_Phase       -> !Conf.print_expr
  | Normalization_Phase -> !Conf.print_core_expr
  | Rewriting_Phase     -> !Conf.print_optimized_expr
  | Factorization_Phase -> !Conf.print_factorized_expr
  | Compile_Phase       -> !Conf.print_logical_algebra
  | Optimization_Phase  -> !Conf.print_optimized_algebra
  | Selection_Phase     -> !Conf.print_physical_algebra
  | Evaluation_Phase    -> false


(* Statement printers *)

let print_statement statement =
  begin
    let s = bprintf_statement "" statement in
    print_escaped_output
      !Conf.expr_formatter
      !Conf.expr_header s
      !Conf.expr_footer;
    flush stdout
  end

let print_cstatement cs =
  begin
    let s = bprintf_acstatement "" cs in
    print_escaped_output
      !Conf.core_expr_formatter
      !Conf.core_expr_header s
      !Conf.core_expr_footer;
    flush stdout
  end

let print_rcstatement cs =
  begin
    let s = bprintf_acstatement "" cs in
    print_escaped_output
      !Conf.optimized_expr_formatter
      !Conf.optimized_expr_header s
      !Conf.optimized_expr_footer;
    flush stdout;
    if (!Conf.print_type) then 
      begin
	let t = bprintf_cxtype "" (Xquery_core_ast_annotation.get_type_annot(cs.pcexpr_annot)) in 
	print_escaped_output
	  !Conf.type_formatter
	  !Conf.type_header t
	  !Conf.type_footer;
	flush stdout
      end
  end

let print_fcstatement cs =
  begin
    let s = bprintf_acstatement "" cs in
    print_escaped_output
      !Conf.factorized_expr_formatter
      !Conf.factorized_expr_header s
      !Conf.factorized_expr_footer;
    flush stdout
  end

let print_plan pl =
  begin
    let s = Print_xquery_algebra.bprintf_logical_algstatement "" pl in
    print_escaped_output
      !Conf.logical_algebra_formatter
      !Conf.logical_algebra_header s
      !Conf.logical_algebra_footer;
    flush stdout
  end

let print_oplan pl =
  begin
    let s = Print_xquery_algebra.bprintf_optimized_algstatement "" pl in
    print_escaped_output
      !Conf.optimized_algebra_formatter
      !Conf.optimized_algebra_header s
      !Conf.optimized_algebra_footer;
    flush stdout
  end

let print_pplan pl =
  begin
    let s = Print_xquery_algebra.bprintf_physical_algstatement "" pl in
    print_escaped_output
      !Conf.physical_algebra_formatter
      !Conf.physical_algebra_header s
      !Conf.physical_algebra_footer;
    flush stdout
  end

let print_result result = ()

(* Prolog printers *)

let print_prolog prolog =
  begin
    let s = bprintf_prolog "" prolog in
    print_escaped_output !Conf.expr_formatter !Conf.expr_header s !Conf.expr_footer;
    flush stdout
  end

let print_cprolog cprolog =
  begin
    let s = bprintf_acprolog "" cprolog in
    print_escaped_output !Conf.core_expr_formatter !Conf.core_expr_header s !Conf.core_expr_footer;
    flush stdout
  end

let print_rcprolog cprolog =
  begin
    let s = bprintf_acprolog "" cprolog in
    print_escaped_output !Conf.optimized_expr_formatter !Conf.optimized_expr_header s !Conf.optimized_expr_footer;
    flush stdout
  end

let print_fcprolog cprolog =
  begin
    let s = bprintf_acprolog "" cprolog in
    print_escaped_output !Conf.factorized_expr_formatter !Conf.factorized_expr_header s !Conf.factorized_expr_footer;
    flush stdout
  end

let print_pprolog pp =
  begin	
    let s = Print_xquery_algebra.bprintf_logical_algprolog "" pp in
    print_escaped_output !Conf.logical_algebra_formatter !Conf.logical_algebra_header s !Conf.logical_algebra_footer;
    flush stdout
  end

let print_opprolog opp =
  begin	
    let s = Print_xquery_algebra.bprintf_optimized_algprolog "" opp in
    print_escaped_output !Conf.optimized_algebra_formatter !Conf.optimized_algebra_header s !Conf.optimized_algebra_footer;
    flush stdout
  end

let print_spprolog spp =
  begin
    let s = Print_xquery_algebra.bprintf_physical_algprolog "" spp in
    print_escaped_output !Conf.physical_algebra_formatter !Conf.physical_algebra_header s !Conf.physical_algebra_footer;
    flush stdout
  end

(* Library module printers *)

let print_lm lm =
  begin
    let s = bprintf_library_module "" lm in
    print_escaped_output !Conf.expr_formatter !Conf.expr_header s !Conf.expr_footer;
    flush stdout
  end

let print_clm clm =
  begin
    let s = bprintf_acmodule "" clm in
    print_escaped_output !Conf.core_expr_formatter !Conf.core_expr_header s !Conf.core_expr_footer;
    flush stdout;
  end

let print_rclm rclm =
  begin
    let s = bprintf_acmodule "" rclm in
    print_escaped_output !Conf.optimized_expr_formatter !Conf.optimized_expr_header s !Conf.optimized_expr_footer;
    flush stdout
  end

let print_fclm fclm =
  begin
    let s = bprintf_acmodule "" fclm in
    print_escaped_output !Conf.factorized_expr_formatter !Conf.factorized_expr_header s !Conf.factorized_expr_footer;
    flush stdout
  end

let print_plm nsenv proc_ctxt plm =
  if !Conf.serialize_logical_algebra then
    begin
      let s = Serialization.bserialize_resolved_xml_stream proc_ctxt
	  (Planio_top.box_logical_algebra_module nsenv plm) in
      print_escaped_output
	!Conf.logical_algebra_formatter
	!Conf.logical_algebra_header s
	!Conf.logical_algebra_footer
    end
  else
    begin	
      let s = Print_xquery_algebra.bprintf_logical_algmodule "" plm in
      print_escaped_output !Conf.logical_algebra_formatter !Conf.logical_algebra_header s !Conf.logical_algebra_footer;
      flush stdout
    end

let print_oplm nsenv proc_ctxt oplm =
  if !Conf.serialize_logical_algebra then
    begin
      let s = Serialization.bserialize_resolved_xml_stream proc_ctxt
	  (Planio_top.box_logical_algebra_module nsenv oplm) in
      print_escaped_output
	!Conf.optimized_algebra_formatter
	!Conf.optimized_algebra_header s
	!Conf.optimized_algebra_footer
    end
  else
    begin	
      let s = Print_xquery_algebra.bprintf_optimized_algmodule "" oplm in
      print_escaped_output !Conf.optimized_algebra_formatter !Conf.optimized_algebra_header s !Conf.optimized_algebra_footer;
      flush stdout
    end

let print_splm nsenv proc_ctxt splm =
  if !Conf.serialize_logical_algebra then
    begin
      let s = Serialization.bserialize_resolved_xml_stream proc_ctxt
	  (Planio_top.box_logical_algebra_module nsenv splm) in
      print_escaped_output
	!Conf.physical_algebra_formatter
	!Conf.physical_algebra_header s
	!Conf.physical_algebra_footer
    end
  else
    begin
      let s = Print_xquery_algebra.bprintf_physical_algmodule "" splm in
      print_escaped_output !Conf.physical_algebra_formatter !Conf.physical_algebra_header s !Conf.physical_algebra_footer;
      flush stdout
    end

(* Main module printers *)

let print_mm mm =
  begin
    let s = bprintf_main_module "" mm in
    print_escaped_output !Conf.expr_formatter !Conf.expr_header s !Conf.expr_footer;
    flush stdout
  end

let print_cmm cmm =
  begin
    let s = bprintf_acmodule "" cmm in
    print_escaped_output !Conf.core_expr_formatter !Conf.core_expr_header s !Conf.core_expr_footer;
    flush stdout
  end

let print_rcmm rcmm =
  begin
    let s = bprintf_acmodule "" rcmm in
    print_escaped_output !Conf.optimized_expr_formatter !Conf.optimized_expr_header s !Conf.optimized_expr_footer;
    flush stdout
  end

let print_fcmm fcmm =
  begin
    let s = bprintf_acmodule "" fcmm in
    print_escaped_output !Conf.factorized_expr_formatter !Conf.factorized_expr_header s !Conf.factorized_expr_footer;
    flush stdout
  end

let print_pmm nsenv proc_ctxt pmm =
  if !Conf.serialize_logical_algebra then
    begin
      let s = Serialization.bserialize_resolved_xml_stream proc_ctxt
	  (Planio_top.box_logical_algebra_module nsenv pmm) in
      print_escaped_output
	!Conf.logical_algebra_formatter
	!Conf.logical_algebra_header s
	!Conf.logical_algebra_footer
    end
  else
    begin
      let s = Print_xquery_algebra.bprintf_logical_algmodule "" pmm in
      print_escaped_output !Conf.logical_algebra_formatter
	!Conf.logical_algebra_header s !Conf.logical_algebra_footer;
      flush stdout
    end

let print_opmm nsenv proc_ctxt opmm =
  if !Conf.serialize_logical_algebra then
    begin
      let s = Serialization.bserialize_resolved_xml_stream proc_ctxt
	  (Planio_top.box_logical_algebra_module nsenv opmm) in
      print_escaped_output
	!Conf.optimized_algebra_formatter
	!Conf.optimized_algebra_header s
	!Conf.optimized_algebra_footer
    end
  else
    begin	
      let s = Print_xquery_algebra.bprintf_optimized_algmodule "" opmm in
      print_escaped_output !Conf.optimized_algebra_formatter
	!Conf.optimized_algebra_header s !Conf.optimized_algebra_footer;
      flush stdout
    end

let print_spmm nsenv proc_ctxt spmm =
  if !Conf.serialize_logical_algebra then
    begin
      let s = Serialization.bserialize_resolved_xml_stream proc_ctxt
	  (Planio_top.box_logical_algebra_module nsenv spmm) in
      print_escaped_output
	!Conf.physical_algebra_formatter
	!Conf.physical_algebra_header s
	!Conf.physical_algebra_footer
    end
  else
    begin
      let s = Print_xquery_algebra.bprintf_physical_algmodule "" spmm in
      print_escaped_output !Conf.physical_algebra_formatter !Conf.physical_algebra_header s !Conf.physical_algebra_footer;
      flush stdout
    end


(************)
(* Wrappers *)
(************)

(* Generic wrappers *)
let wrap_statement_aux phase analyze_fun process_input print_result proc_ctxt context input =
  let analyze_and_process_input context input = 
    let new_context = analyze_fun context input in
    process_input new_context input
  in
  let (result) = wrap_monitor proc_ctxt (PQuery phase) (analyze_and_process_input context) input in
  if (!Conf.print_global && (phase_print_flag phase))
  then print_result result;
  (result)

let wrap_statement_with_context phase analyze_fun process_input print_result proc_ctxt context input =
  let analyze_and_process_input context input = 
    let new_context = analyze_fun context input in
    process_input new_context input
  in
  let (context', result) = wrap_monitor proc_ctxt (PQuery phase) (analyze_and_process_input context) input in
  if (!Conf.print_global && (phase_print_flag phase))
  then print_result result;
  (context', result)

let wrap_prolog_aux phase analyze_fun process_input print_result proc_ctxt context input =
  let analyze_and_process_input context input = 
    let new_context = analyze_fun context input in
    process_input new_context input
  in
  let (context',result) = wrap_monitor proc_ctxt (PQuery phase) (analyze_and_process_input context) input in
  if (!Conf.print_global && !Conf.print_prolog && (phase_print_flag phase))
  then print_result result;
  (context',result)

let wrap_lmodule_aux phase analyze_fun process_input print_result proc_ctxt context input =
  let analyze_and_process_input context input = 
    let new_context = analyze_fun context input in
    process_input new_context input
  in
  let (context',result) = wrap_monitor proc_ctxt (PQuery phase) (analyze_and_process_input context) input in
  if (!Conf.print_global && (phase_print_flag phase))
  then print_result result;
  (context',result)

let wrap_mmodule_aux phase analyze_fun process_input print_result proc_ctxt context input =
  let analyze_and_process_input context input = 
    let new_context = analyze_fun context input in
    process_input new_context input
  in
  let (context',result) = wrap_monitor proc_ctxt (PQuery phase) (analyze_and_process_input context) input in
  if (!Conf.print_global && (phase_print_flag phase))
  then print_result result;
  (context',result)

(* Wrappers for the parsing phase *)

let wrap_parse_statement analyze_statement parse_statement proc_ctxt parse_ctxt gio =
  wrap_statement_aux Parsing_Phase analyze_statement parse_statement print_statement proc_ctxt parse_ctxt gio

let wrap_parse_prolog analyze_prolog parse_prolog proc_ctxt parse_ctxt gio =
  wrap_prolog_aux Parsing_Phase analyze_prolog parse_prolog print_prolog proc_ctxt parse_ctxt gio

let wrap_parse_library_module analyze_lmodule parse_lmodule proc_ctxt parse_ctxt gio =
  wrap_lmodule_aux Parsing_Phase analyze_lmodule parse_lmodule print_lm proc_ctxt parse_ctxt gio

let wrap_parse_main_module analyze_mmodule parse_mmodule proc_ctxt parse_ctxt gio =
  wrap_mmodule_aux Parsing_Phase analyze_mmodule parse_mmodule print_mm proc_ctxt parse_ctxt gio

(* Wrappers for the preprocessing phase: preprocessing is charged to normalization *)

let wrap_preprocess_prolog analyze_prolog preprocess_prolog proc_ctxt norm_ctxt lm =
  wrap_prolog_aux Normalization_Phase analyze_prolog preprocess_prolog print_cprolog proc_ctxt norm_ctxt lm

let wrap_preprocess_library_module analyze_lmodule preprocess_lmodule proc_ctxt norm_ctxt lm =
  wrap_lmodule_aux Normalization_Phase analyze_lmodule preprocess_lmodule print_clm proc_ctxt norm_ctxt lm

let wrap_preprocess_main_module analyze_mmodule preprocess_mmodule proc_ctxt norm_ctxt mm =
  wrap_mmodule_aux Normalization_Phase analyze_mmodule preprocess_mmodule print_cmm proc_ctxt norm_ctxt mm

(* Wrappers for the normalization phase *)

let wrap_normalize_statement analyze_statement normalize_statement proc_ctxt norm_ctxt s =
  wrap_statement_aux  Normalization_Phase analyze_statement normalize_statement print_cstatement proc_ctxt norm_ctxt s

let wrap_normalize_prolog analyze_prolog normalize_prolog proc_ctxt norm_ctxt lm =
  wrap_prolog_aux Normalization_Phase analyze_prolog normalize_prolog print_cprolog proc_ctxt norm_ctxt lm

let wrap_normalize_library_module analyze_lmodule normalize_lmodule proc_ctxt norm_ctxt lm =
  wrap_lmodule_aux Normalization_Phase analyze_lmodule normalize_lmodule print_clm proc_ctxt norm_ctxt lm

let wrap_normalize_main_module analyze_mmodule normalize_mmodule proc_ctxt norm_ctxt mm =
  wrap_mmodule_aux Normalization_Phase analyze_mmodule normalize_mmodule print_cmm proc_ctxt norm_ctxt mm


(* Wrappers for the rewriting phase *)

let wrap_rewrite_statement analyze_statement rewrite_statement proc_ctxt stat_ctxt s =
  wrap_statement_aux Rewriting_Phase analyze_statement rewrite_statement print_rcstatement proc_ctxt stat_ctxt s

let wrap_rewrite_prolog analyze_prolog rewrite_prolog proc_ctxt stat_ctxt lm =
  wrap_prolog_aux Rewriting_Phase analyze_prolog rewrite_prolog print_rcprolog proc_ctxt stat_ctxt lm

let wrap_rewrite_library_module analyze_lmodule rewrite_lmodule proc_ctxt stat_ctxt lm =
  wrap_lmodule_aux Rewriting_Phase analyze_lmodule rewrite_lmodule print_rclm proc_ctxt stat_ctxt lm

let wrap_rewrite_main_module analyze_mmodule rewrite_mmodule proc_ctxt stat_ctxt mm =
  wrap_mmodule_aux Rewriting_Phase analyze_mmodule rewrite_mmodule print_rcmm proc_ctxt stat_ctxt mm

(* Wrappers for the factorization phase *)

let wrap_factorization_statement analyze_statement factorize_statement proc_ctxt stat_ctxt s =
  wrap_statement_aux Factorization_Phase analyze_statement factorize_statement print_fcstatement proc_ctxt stat_ctxt s

let wrap_factorization_prolog analyze_prolog factorize_prolog proc_ctxt stat_ctxt lm =
 wrap_prolog_aux Factorization_Phase analyze_prolog factorize_prolog print_fcprolog proc_ctxt stat_ctxt lm

let wrap_factorization_library_module analyze_lmodule factorize_lmodule proc_ctxt stat_ctxt lm =
  wrap_lmodule_aux Factorization_Phase analyze_lmodule factorize_lmodule print_fclm proc_ctxt stat_ctxt lm

let wrap_factorization_main_module analyze_mmodule factorize_mmodule proc_ctxt stat_ctxt mm =
  wrap_mmodule_aux Factorization_Phase analyze_mmodule factorize_mmodule print_fcmm proc_ctxt stat_ctxt mm

(* Wrappers for the compile phase *)

let wrap_compile_statement analyze_statement compile_statement proc_ctxt comp_ctxt s =
  wrap_statement_aux Compile_Phase analyze_statement compile_statement print_plan proc_ctxt comp_ctxt s

let wrap_compile_prolog analyze_prolog compile_prolog proc_ctxt comp_ctxt lm =
  wrap_prolog_aux Compile_Phase analyze_prolog compile_prolog print_pprolog proc_ctxt comp_ctxt lm

let wrap_compile_library_module analyze_lmodule compile_lmodule proc_ctxt comp_ctxt lm =
  let nsenv =
    Norm_context.nsenv_from_norm_context
      (Compile_context.norm_context_from_compile_context comp_ctxt)
  in
  wrap_lmodule_aux Compile_Phase analyze_lmodule compile_lmodule (print_plm nsenv proc_ctxt) proc_ctxt comp_ctxt lm

let wrap_compile_main_module analyze_mmodule compile_mmodule proc_ctxt comp_ctxt mm =
  let nsenv =
    Norm_context.nsenv_from_norm_context
      (Compile_context.norm_context_from_compile_context comp_ctxt)
  in
  wrap_mmodule_aux Compile_Phase analyze_mmodule compile_mmodule (print_pmm nsenv proc_ctxt) proc_ctxt comp_ctxt mm

(* Wrappers for the optimization phase *)

let wrap_optimization_statement analyze_statement optimize_statement proc_ctxt comp_ctxt s =
 wrap_statement_aux Optimization_Phase analyze_statement optimize_statement print_oplan proc_ctxt comp_ctxt s

let wrap_optimization_prolog analyze_prolog optimize_prolog proc_ctxt comp_ctxt lm =
  wrap_prolog_aux Optimization_Phase analyze_prolog optimize_prolog print_opprolog proc_ctxt comp_ctxt lm

let wrap_optimization_library_module analyze_lmodule optimize_lmodule proc_ctxt comp_ctxt lm =
  let nsenv =
    Norm_context.nsenv_from_norm_context
      (Compile_context.norm_context_from_compile_context comp_ctxt)
  in
  wrap_lmodule_aux Optimization_Phase analyze_lmodule optimize_lmodule (print_oplm nsenv proc_ctxt) proc_ctxt comp_ctxt lm

let wrap_optimization_main_module analyze_mmodule optimize_mmodule proc_ctxt comp_ctxt mm =
  let nsenv =
    Norm_context.nsenv_from_norm_context
      (Compile_context.norm_context_from_compile_context comp_ctxt)
  in
  wrap_mmodule_aux Optimization_Phase analyze_mmodule optimize_mmodule (print_opmm nsenv proc_ctxt) proc_ctxt comp_ctxt mm

(* Wrappers for the code selection phase *)

let wrap_selection_statement analyze_statement selection_statement proc_ctxt (comp_prog, module_uri, code_ctxt) s =
  wrap_statement_with_context Selection_Phase analyze_statement 
    selection_statement print_pplan proc_ctxt (comp_prog, module_uri, code_ctxt) s

let wrap_selection_prolog analyze_prolog selection_prolog proc_ctxt (comp_prog, module_uri, code_ctxt) lm =
  wrap_prolog_aux Selection_Phase analyze_prolog 
    selection_prolog print_spprolog proc_ctxt (comp_prog, module_uri, code_ctxt) lm

let wrap_selection_library_module analyze_lmodule selection_lmodule proc_ctxt (comp_prog, module_uri, code_ctxt) lm =
  let nsenv =
    let comp_ctxt = compile_context_from_code_selection_context code_ctxt in
    Norm_context.nsenv_from_norm_context
      (Compile_context.norm_context_from_compile_context comp_ctxt)
  in
  wrap_lmodule_aux Selection_Phase analyze_lmodule 
    selection_lmodule (print_splm nsenv proc_ctxt) proc_ctxt (comp_prog, module_uri, code_ctxt) lm

let wrap_selection_main_module analyze_mmodule selection_mmodule proc_ctxt (comp_prog, module_uri, code_ctxt) mm =
  let nsenv =
    let comp_ctxt = compile_context_from_code_selection_context code_ctxt in
    Norm_context.nsenv_from_norm_context
      (Compile_context.norm_context_from_compile_context comp_ctxt)
  in
  wrap_mmodule_aux Selection_Phase analyze_mmodule 
    selection_mmodule (print_spmm nsenv proc_ctxt) proc_ctxt (comp_prog, module_uri, code_ctxt) mm

(* Wrappers for the evaluation phase *)

let wrap_eval_statement analyze_statement eval_statement proc_ctxt alg_ctxt s =
  wrap_statement_aux Evaluation_Phase analyze_statement eval_statement print_result proc_ctxt alg_ctxt s

let wrap_eval_prolog analyze_prolog eval_prolog proc_ctxt alg_ctxt lm =
  wrap_prolog_aux Evaluation_Phase analyze_prolog eval_prolog print_result proc_ctxt alg_ctxt lm

let wrap_eval_library_module analyze_lmodule eval_lmodule proc_ctxt alg_ctxt lm =
  wrap_lmodule_aux Evaluation_Phase analyze_lmodule eval_lmodule print_result proc_ctxt alg_ctxt lm

let wrap_eval_main_module analyze_mmodule eval_mmodule proc_ctxt alg_ctxt mm =
  wrap_mmodule_aux Evaluation_Phase analyze_mmodule eval_mmodule print_result proc_ctxt alg_ctxt mm


(**************************************)
(* Implementation for specific phases *) 
(**************************************)

let analysis_id context x = context

let apply_analysis proc_ctxt analysis_fun phase_fun context input =
  let new_context = analysis_fun context input in
  phase_fun proc_ctxt new_context input


(* Parsing handler configuration *)

let build_parsing_handler proc_ctxt (sa,pa,la,ma) (s,p,l,m) =
  { parse_statement      = (wrap_parse_statement sa s proc_ctxt);
    parse_prolog         = (wrap_parse_prolog pa p proc_ctxt);
    parse_library_module = (wrap_parse_library_module la l proc_ctxt);
    parse_main_module    = (wrap_parse_main_module ma m proc_ctxt) }

let create_parsing_handler proc_ctxt =
  let (sa,pa,la,ma) =
    (analysis_id,analysis_id,analysis_id,analysis_id)
  in
  let parse_mod =
    (* Taking syntax into account, XQuery or XQueryX *)
    (* This is only for main modules right now! *)
    if (Conf.is_xquery_syntax())
    then Parse_top.parse_main_module_from_io
    else Parse_xqueryx.parse_xqx_xquery_main_module_from_io
  in
  let (s,p,l,m) =
    (Parse_top.parse_statement_from_io,
     Parse_top.parse_prolog_from_io,
     Parse_top.parse_library_module_from_io,
     parse_mod)
  in
  build_parsing_handler proc_ctxt (sa,pa,la,ma) (s,p,l,m)


(* Pre-processing handlers configuration: these phases aren't wrapped! *)
let build_preprocessing_handler proc_ctxt (pa,la,ma) (p,l,m) =
  { preprocess_prolog         = p;
    preprocess_library_module = l;
    preprocess_main_module    = m }

let create_preprocessing_handler proc_ctxt =
  let (pa,la,ma) =
    (analysis_id,analysis_id,analysis_id)
  in
  let (p,l,m) =
    if proc_ctxt.normalization && proc_ctxt.normalization_ident
    then
      (Norm_ident_top.preprocess_ident_prolog,
       Norm_ident_top.preprocess_ident_library_module,
       Norm_ident_top.preprocess_ident_main_module)
    else
      if proc_ctxt.normalization
      then
	(Norm_top.preprocess_prolog,
	 Norm_top.preprocess_library_module,
	 Norm_top.preprocess_main_module)
      else
	(Norm_top.nopreprocess_prolog,
	 Norm_top.nopreprocess_library_module,
	 Norm_top.nopreprocess_main_module)
  in
  build_preprocessing_handler proc_ctxt (pa,la,ma) (p,l,m)

(* Normalization handlers configuration *)

let build_normalization_handler proc_ctxt (sa,pa,la,ma) (s,p,l,m) =
  { normalize_statement      = (wrap_normalize_statement sa s proc_ctxt);
    normalize_prolog         = (wrap_normalize_prolog pa p proc_ctxt);
    normalize_library_module = (wrap_normalize_library_module la l proc_ctxt);
    normalize_main_module    = (wrap_normalize_main_module ma m proc_ctxt) }

let create_normalization_handler proc_ctxt =
  let (sa,pa,la,ma) =
    (analysis_id,analysis_id,analysis_id,analysis_id)
  in
  let (s,p,l,m) =
    if proc_ctxt.normalization && proc_ctxt.normalization_ident
    then
      (Norm_ident_top.normalize_ident_statement,
       Norm_ident_top.normalize_ident_prolog,
       Norm_ident_top.normalize_ident_library_module,
       Norm_ident_top.normalize_ident_main_module)
    else
      if proc_ctxt.normalization
      then
	(Norm_top.normalize_statement,
	 Norm_top.normalize_prolog,
	 Norm_top.normalize_library_module,
	 Norm_top.normalize_main_module)
      else
	(Norm_top.nonorm_statement,
	 Norm_top.nonorm_prolog,
	 Norm_top.nonorm_library_module,
	 Norm_top.nonorm_main_module)
  in
  build_normalization_handler proc_ctxt (sa,pa,la,ma) (s,p,l,m)


(* Rewriting handler configuration *)

let build_rewriting_handler proc_ctxt (sa,pa,la,ma) (s,p,l,m) =
  { rewriting_statement      = (wrap_rewrite_statement sa s proc_ctxt);
    rewriting_prolog         = (wrap_rewrite_prolog pa p proc_ctxt);
    rewriting_library_module = (wrap_rewrite_library_module la l proc_ctxt);
    rewriting_main_module    = (wrap_rewrite_main_module ma m proc_ctxt) }

let create_rewriting_handler proc_ctxt =
  let (sa,pa,la,ma) =
    begin
      match proc_ctxt.typing_kind with
      | Typing_None   ->
	  (Typing_notyping.notyping_type_cstatement,
	   Typing_notyping.notyping_type_cprolog,
	   Typing_notyping.notyping_type_cxmodule,
	   Typing_notyping.notyping_type_cxmodule)
      | Typing_Weak   ->
	  (Typing_top.typing_type_cstatement,
	   Typing_top.typing_type_cprolog,
	   Typing_top.typing_type_cxmodule,
	   Typing_top.typing_type_cxmodule)
      | Typing_Strong ->
	  (Typing_top.typing_type_cstatement,
	   Typing_top.typing_type_cprolog,
	   Typing_top.typing_type_cxmodule,
	   Typing_top.typing_type_cxmodule)
    end
  in
  let (s,p,l,m) =
    let typing_rewrite_rules =
      begin
	match proc_ctxt.typing_kind with
	| Typing_None   -> []
	| Typing_Weak   -> any_typing_rule_set
	| Typing_Strong -> any_typing_rule_set
      end
    in
    let (sbdo_toplevel_rules, sbdo_toplevel_prolog_rules, sbdo_rewrite_rules) = ([], [], []) 
    in
    let (rewriting_rules_sets, rewriting_prolog_rules_sets) =
      if proc_ctxt.rewriting
      then
	(* The order of rules matters here! Typing rules must precede generic rules *)
	let rewrite_rules = sbdo_rewrite_rules @ typing_rewrite_rules @ generic_rule_set in
	(* let toplevel_rules = sbdo_toplevel_rules @ generic_toplevel_rule_set in  *)
	let toplevel_rules = Rewriting_rules_notyping.generic_toplevel_rule_set in
	((toplevel_rules, rewrite_rules),
	 (toplevel_rules, sbdo_toplevel_prolog_rules, rewrite_rules))
      else
	(([], []), ([],[],[]))
    in
    (Rewriting_top.rewriting_cstatement rewriting_rules_sets,
     Rewriting_top.rewriting_cprolog rewriting_prolog_rules_sets,
     Rewriting_top.rewriting_cxmodule rewriting_prolog_rules_sets,
     Rewriting_top.rewriting_cxmodule rewriting_prolog_rules_sets)
  in
  build_rewriting_handler proc_ctxt (sa,pa,la,ma) (s,p,l,m)


(* Factorization handler configuration *)

let build_factorization_handler proc_ctxt (sa,pa,la,ma) (s,p,l,m) =
  { factorize_statement      = (wrap_factorization_statement sa s proc_ctxt);
    factorize_prolog         = (wrap_factorization_prolog pa p proc_ctxt);
    factorize_library_module = (wrap_factorization_library_module la l proc_ctxt);
    factorize_main_module    = (wrap_factorization_main_module ma m proc_ctxt) }

let create_factorization_handler proc_ctxt =
  let factorization_id stat_ctxt x = x in
  let factorization_cid stat_ctxt x = (stat_ctxt,x) in
  let (sa,pa,la,ma) =
    (Factorize_free_var.annotate_free_vars,analysis_id,analysis_id,analysis_id)
  in
  let (s,p,l,m) =
    if proc_ctxt.factorization
    then
      (Factorize_top.factorize_statement,
       Factorize_top.factorize_prolog,
       Factorize_top.factorize_xmodule,
       Factorize_top.factorize_xmodule)
    else
    (factorization_id,factorization_cid,factorization_cid,factorization_cid)
  in
  build_factorization_handler proc_ctxt (sa,pa,la,ma) (s,p,l,m)


(* Compilation handler configuration *)

let build_compilation_handler proc_ctxt (sa,pa,la,ma) (s,p,l,m) =
  { compile_statement 	   = (wrap_compile_statement sa s proc_ctxt);
    compile_prolog         = (wrap_compile_prolog pa p proc_ctxt);
    compile_library_module = (wrap_compile_library_module la l proc_ctxt);
    compile_main_module    = (wrap_compile_main_module ma m proc_ctxt) }

let create_compilation_handler proc_ctxt =  
  let (sa,pa,la,ma) =
    (analysis_id,analysis_id,analysis_id,analysis_id)
  in
  let (s,p,l,m) =
    (Compile_top.compile_statement,
     Compile_top.compile_prolog,
     Compile_top.compile_xmodule,
     Compile_top.compile_xmodule)
  in
  build_compilation_handler proc_ctxt (sa,pa,la,ma) (s,p,l,m)


(* Optimization handler configuration *)

let build_optimization_handler proc_ctxt (sa,pa,la,ma) (s,p,l,m) =
  { optimize_statement 	    = (wrap_optimization_statement sa s proc_ctxt);
    optimize_prolog         = (wrap_optimization_prolog pa p proc_ctxt);
    optimize_library_module = (wrap_optimization_library_module la l proc_ctxt);
    optimize_main_module    = (wrap_optimization_main_module ma m proc_ctxt) }

let create_optimization_handler proc_ctxt =
  let optimization_id context x = x in
  let optimization_cid context x = (context,x) in
  let (sa,pa,la,ma) =
(*     if proc_ctxt.infer_independence *)
(*     then *)
(*       let wrap_analysis analysis_fun ctxt input = analysis_fun input; ctxt in *)
(* 	    (wrap_analysis Alg_path_analysis.path_analysis_of_logical_algop_expr, *)
(* 	    wrap_analysis Alg_path_analysis.path_analysis_of_logical_algop_prolog, *)
(* 	    wrap_analysis Alg_path_analysis.path_analysis_of_logical_algop_xmodule, *)
(* 	    wrap_analysis Alg_path_analysis.path_analysis_of_logical_algop_xmodule) *)
(*     else *)
      (analysis_id,analysis_id,analysis_id,analysis_id)
  in
  let (s,p,l,m) =
    if proc_ctxt.optimization
    then
      begin
	(Optimization_top.optimize_statement,
	 Optimization_top.optimize_prolog,
	 Optimization_top.optimize_library_module,
	 Optimization_top.optimize_main_module)
      end
    else
      begin
	(optimization_id,optimization_cid,optimization_cid,optimization_cid)
      end
  in
    build_optimization_handler proc_ctxt (sa,pa,la,ma) (s,p,l,m)


(* Code selection handler configuration *)

let build_selection_handler proc_ctxt (sa,pa,la,ma) (s,p,l,m) =
  { selection_statement      = (wrap_selection_statement sa s proc_ctxt);
    selection_prolog         = (wrap_selection_prolog pa p proc_ctxt);
    selection_library_module = (wrap_selection_library_module la l proc_ctxt);
    selection_main_module    = (wrap_selection_main_module ma m proc_ctxt) } 

let create_selection_handler proc_ctxt =
  let code_selection_id f noop ctxt _ = f ctxt noop in
  let (sa,pa,la,ma) =
    if proc_ctxt.streaming || proc_ctxt.infer_independence
    then
      let wrap_analysis analysis_fun ctxt input = analysis_fun input; ctxt in
      (wrap_analysis Alg_path_analysis.path_analysis_of_logical_algop_expr,
       wrap_analysis Alg_path_analysis.path_analysis_of_logical_algop_prolog,
       wrap_analysis Alg_path_analysis.path_analysis_of_logical_algop_xmodule,
       wrap_analysis Alg_path_analysis.path_analysis_of_logical_algop_xmodule)
(*
  Nicola:
  This is incorrect, because the analysis of the xmodule needs the paths
  associated with the global variables from the prolog.
*)
    else
      (analysis_id,analysis_id,analysis_id,analysis_id)
  in
  let (s,p,l,m) =    
    if proc_ctxt.code_selection then 
      (Cs_code_selection_top.code_selection_statement,
       Cs_code_selection_top.code_selection_prolog,
       Cs_code_selection_top.code_selection_module,
       Cs_code_selection_top.code_selection_module)
    else 
      (code_selection_id Cs_code_selection_top.code_selection_statement Xquery_algebra_ast_util.empty_statement, 
       code_selection_id Cs_code_selection_top.code_selection_prolog Xquery_algebra_ast_util.empty_prolog_plan,
       code_selection_id Cs_code_selection_top.code_selection_module Xquery_algebra_ast_util.empty_xmodule,
       code_selection_id Cs_code_selection_top.code_selection_module Xquery_algebra_ast_util.empty_xmodule)
  in
  build_selection_handler proc_ctxt (sa,pa,la,ma) (s,p,l,m)


(* Evaluation handler configuration *)

let build_evaluation_handler proc_ctxt (sa,pa,la,ma) (s,p,l,m) =
  { eval_statement 	= (wrap_eval_statement sa s proc_ctxt);
    eval_prolog         = (wrap_eval_prolog pa p proc_ctxt);
    eval_library_module = (wrap_eval_library_module la l proc_ctxt);
    eval_main_module    = (wrap_eval_main_module ma m proc_ctxt) }

let create_evaluation_handler proc_ctxt =
  let (sa,pa,la,ma) = (analysis_id,analysis_id,analysis_id,analysis_id) in
  let (s,p,l,m) =
    if proc_ctxt.evaluation
    then
      (Evaluation_top.eval_cstatement,
       Evaluation_top.eval_prolog,
       Evaluation_top.eval_library_module,
       Evaluation_top.eval_main_module)
    else
      ((function alg_ctxt -> function tcstatement -> empty_xml_value()),
       (function alg_ctxt -> function prolog -> alg_ctxt,[]),
       (function alg_ctxt -> function tcxmodule -> alg_ctxt,[]),
       (function alg_ctxt -> function tcxmodule -> alg_ctxt,[]))
  in
  build_evaluation_handler proc_ctxt (sa,pa,la,ma) (s,p,l,m)


(***************************)
(* Global handler creation *)
(***************************)

(* Creates a global handler from specific handlers for each phase *)

let build_phase_handler ph pph nh rh fh ch oh sh eh =
  { parsing_phase       = ph;
    preprocessing_phase = pph;
    normalization_phase = nh;
    rewriting_phase     = rh;
    factorization_phase = fh;
    compile_phase       = ch;
    optimization_phase  = oh;
    selection_phase     = sh;
    evaluation_phase    = eh }

(* Creates a phase handler according to a given processing context *)

let phase_handler_of_processing_context proc_ctxt =
  let ph = create_parsing_handler       proc_ctxt in
  let pph = create_preprocessing_handler proc_ctxt in
  let nh = create_normalization_handler proc_ctxt in
  let rh = create_rewriting_handler     proc_ctxt in
  let fh = create_factorization_handler proc_ctxt in
  let ch = create_compilation_handler   proc_ctxt in
  let oh = create_optimization_handler  proc_ctxt in
  let sh = create_selection_handler     proc_ctxt in
  let eh = create_evaluation_handler    proc_ctxt in
  build_phase_handler ph pph nh rh fh ch oh sh eh


