(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: evaluation_top.ml,v 1.12 2007/02/01 22:08:46 simeon Exp $ *)

(* Module Eval_top
   Description:
     This module implements evaluation of top-level statements and of
     the XQuery prolog.
*)

open Error

open Xquery_algebra_ast

open Algebra_type
open Execution_context
open Cs_util

open Physical_sequence
open Physical_value_util

open Dm

open Evaluation_expr


(**************)
(* Statements *)
(**************)

let eval_cstatement alg_ctxt st = algebra_execute alg_ctxt st


(**********)
(* Prolog *)
(**********)

(* Note:
     This part of the code deals with the evaluation of the query
     prolog.
   - Jerome *)
    
let eval_alg_decl alg_ctxt (alg_term: algop_decl) =
  let algebra_execute_with_input = Evaluation_expr.algebra_execute alg_ctxt in
  (* 2. evaluate the non-dependant sub-expressions, then call the code *)
  (* 1. Instiantiate higher-order operations *)
  let actual_code =
    match !(alg_term.alg_decl_eval) with      
    | PNoDep f -> f
    | PSomeDep f -> f Evaluation_expr.algebra_execute
  in
  let sub_alg_terms = alg_term.alg_decl_indep in
  match (sub_alg_terms, actual_code) with
  | (NoSub,PAOECUnit f) ->
      f alg_ctxt ()
  | (OneSub alg1,PAOECUnary f) ->
      let input1 = algebra_execute_with_input alg1 in
      f alg_ctxt input1
  | (TwoSub (alg1,alg2),PAOECBinary f) ->
      let input1 = algebra_execute_with_input alg1 in
      let input2 = algebra_execute_with_input alg2 in
      f alg_ctxt input1 input2
  | (ManySub algs , PAOECMany f) ->
      let inputs = Array.map algebra_execute_with_input algs in
      f alg_ctxt inputs
  | _ ->
      raise (Query (Malformed_Algebra_Expr "Number of input sub-algebraic operations does not match code arity"))


(* Prolog *)

let default_eval_prolog alg_ctxt prolog =
  let alg_ctxt' = List.fold_left eval_alg_decl alg_ctxt prolog.palgop_prolog_vars in
  let alg_ctxt'' = List.fold_left eval_alg_decl alg_ctxt' prolog.palgop_prolog_indices in
  alg_ctxt''

let eval_prolog alg_ctxt prolog =
  (default_eval_prolog alg_ctxt prolog,[])

(* Library module *)

let eval_library_module alg_ctxt xmod =
  (default_eval_prolog alg_ctxt xmod.palgop_module_prolog,[])

(* Main module *)

let eval_main_module alg_ctxt xmod =
  let alg_ctxt' = default_eval_prolog alg_ctxt xmod.palgop_module_prolog in
  (alg_ctxt',
   List.map (fun x -> eval_cstatement alg_ctxt' x) xmod.palgop_module_statements)

