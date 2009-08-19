(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_top.ml,v 1.25 2007/09/19 20:01:50 mff Exp $ *)

(* Module: Factorize_top
   Description:
    This module will put statements into a normal form before their
    are compiled into the algbera. This is to help the optimizer pick
    up logical optimizations.
*)

open Conf
open Error

open Xquery_common_ast
open Xquery_core_ast
open Xquery_core_ast_util


(**************************************************)
(* Exposed                                        *)
(* Normalize the FLWOR Expressions in a statement *)
(**************************************************)

let factorize_expr stat_ctxt expr =
  (* Note: we may want to have several factorization "phases" called here.
     - Jerome *)
  begin
   (* *** Disabled for now 
    * let nexpr1 = Factorize_unique.factorize_unique expr in 
    * let nexpr2 = Factorize_globals.factor_global_expression stat_ctxt nexpr1 in 
    * let nexpr3 = Factorize_flwor.factorize_flwor stat_ctxt nexpr2 in
    * let nexpr4 = Factorize_iteration.factorize_expression stat_ctxt nexpr3 in  
    *)

    let nexpr3 = Factorize_tpnf.factorize_tpnf stat_ctxt expr in
(*    let nexpr3 = expr in
    let nexpr4 = Factorize_flwor.factorize_flwor stat_ctxt nexpr3 in *)
    nexpr3
  end

let factorize_statement stat_ctxt statement =
  factorize_expr stat_ctxt statement

let factorize_function_body stat_ctxt cfunction_body =
  match cfunction_body with
  | CEFunctionInterface
  | CEFunctionImported
  | CEFunctionBltIn -> cfunction_body
  | CEFunctionUser ce -> CEFunctionUser (factorize_expr stat_ctxt ce)
       


let factorize_function stat_ctxt cfunction_def =
  let new_desc =
    match cfunction_def.pcfunction_def_desc with
    | (rfname, cvars, cfunction_signature, cfunction_body, upd) ->
	(rfname, cvars, cfunction_signature, factorize_function_body stat_ctxt cfunction_body, upd)
  in
  fmkcfunction_def new_desc cfunction_def.pcfunction_def_loc

let factorize_var stat_ctxt cvar_decl =
  let new_desc =
    match cvar_decl.pcvar_decl_desc with
    | (vname, model, CEVarUser ce) ->
	(vname, model, CEVarUser (factorize_expr stat_ctxt ce))
    | (vname, model, cevar_body) ->
	(vname, model, cevar_body)
  in
  fmkcvar_decl new_desc cvar_decl.pcvar_decl_loc

let factorize_index stat_ctxt cindex_def =
  let new_desc =
    match cindex_def.pcindex_def_desc with
    | CValueIndex (name, ce1, ce2) ->
	CValueIndex (name, factorize_expr stat_ctxt ce1, factorize_expr stat_ctxt ce2)
    | CNameIndex name ->
	CNameIndex name
  in
  fmkcindex_def new_desc cindex_def.pcindex_def_loc


let factorize_prolog stat_ctxt cprolog =
  let prolog = 
  { pcprolog_functions = List.map (factorize_function stat_ctxt) cprolog.pcprolog_functions;
    pcprolog_vars    = List.map (factorize_var stat_ctxt) cprolog.pcprolog_vars;
    pcprolog_servers = raise (Query(Prototype("server declarations not implemented")));
    pcprolog_indices = List.map (factorize_index stat_ctxt) cprolog.pcprolog_indices }
  in
  (stat_ctxt,prolog)

let factorize_xmodule stat_ctxt cxmodule =
  let factorized_cxmodule =
    { pcmodule_prolog = snd (factorize_prolog stat_ctxt cxmodule.pcmodule_prolog);
      pcmodule_statements = List.map (factorize_statement stat_ctxt) cxmodule.pcmodule_statements }
  in

  (* Stream analysis for streaming XPath evaluation; adds core annotations. - Michael *)
(*  let _ = Stream_analysis.stream_analysis_of_xmodule factorized_cxmodule in *)

    (*****************************************)
    (* why is that called twice, by the way? *)
    (*****************************************)

  (stat_ctxt,factorized_cxmodule)
