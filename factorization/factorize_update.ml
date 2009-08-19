(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_update.ml,v 1.4 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Factorize_update
   Description:
     This module factorizes out code from update expression. This is
     to help the optimizer pick up logical optimizations.
*)

open Xquery_core_ast
open Xquery_core_ast_util

open Factorize_util


(****************************)
(* Factorize simple updates *)
(****************************)

let factorize_insert_location il =
  match il with
  | CUAsLastInto ce ->
      let (l_clause, cvar_expr) = update_replace_fun ce in
      (l_clause,CUAsLastInto cvar_expr)
  | CUAsFirstInto ce ->
      let (l_clause, cvar_expr) = update_replace_fun ce in
      (l_clause, CUAsFirstInto cvar_expr)
  | CUInto ce ->
      let (l_clause, cvar_expr) = update_replace_fun ce in
      (l_clause, CUInto cvar_expr)
  | CUAfter ce ->
      let (l_clause, cvar_expr) = update_replace_fun ce in
      (l_clause, CUAfter cvar_expr)
  | CUBefore ce ->
      let (l_clause, cvar_expr) = update_replace_fun ce in
      (l_clause, CUBefore cvar_expr)

let factorize_simple_update_clause csu =
  let annot = csu.pcexpr_annot in
  let eh = csu.pcexpr_origin in
  let fi = csu.pcexpr_loc in
  match csu.pcexpr_desc with
  | CEInsert (ce,cil) ->
      let (cl_clause1,ce) = update_replace_fun ce in
      let (cl_clause2,cil) = factorize_insert_location cil in
      let cl_clauses = cl_clause1 :: cl_clause2 :: [] in
      (cl_clauses, fmkacexpr (CEInsert (ce, cil)) annot eh fi)
  | CEReplace (vo,e1,e2) ->
      let (l_clause1,ce1) = update_replace_fun e1 in
      let (l_clause2,ce2) = update_replace_fun e2 in
      let l_clauses = l_clause1 :: l_clause2 :: [] in
      (l_clauses, fmkacexpr (CEReplace (vo,ce1,ce2)) annot eh fi)
  | CEDelete e ->
      let (l_clause1,ce) = update_replace_fun e in
      (l_clause1 :: [], fmkacexpr (CEDelete ce) annot eh fi)
  | _ ->
      ([],csu)

let rec factorize_simple_update_clauses simple_update_clauses =
  match simple_update_clauses with
  | [] -> [],[]
  | su :: simple_update_clauses' ->
      let l_clauses1,csu = factorize_simple_update_clause su in
      let l_clauses2,csimple_update_clauses' = factorize_simple_update_clauses simple_update_clauses' in
      (l_clauses1 @ l_clauses2, csu :: csimple_update_clauses')

let apply_update_normal_form cfl_clauses csimple_updates_clauses =
  let (cfl_additional_clauses,csimple_updates_clauses) =
    factorize_simple_update_clauses csimple_updates_clauses
  in
  let cfl_clauses = cfl_clauses @ cfl_additional_clauses in
  (cfl_clauses,csimple_updates_clauses)


(***************************)
(* Normal form for updates *)
(***************************)

let update_in_normal_form fl_clauses return_clause =
  match return_clause.pcexpr_desc with
  | CESnap csimple_update ->
      let (cfl_clauses,csimple_updates_clauses) =
	apply_update_normal_form fl_clauses [csimple_update]
      in
      let annot = return_clause.pcexpr_annot in
      let eh = return_clause.pcexpr_origin in
      let fi = return_clause.pcexpr_loc in
      let return_clause =
	fmkacexpr (CESnap csimple_updates_clauses) annot eh fi
      in
      (cfl_clauses,return_clause)
  | _ ->
      (fl_clauses, return_clause)

