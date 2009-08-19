(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_notyping.ml,v 1.5 2007/02/01 22:08:55 simeon Exp $ *)

(* Module: Typing_notyping
   Description:
     This module contains operations to add dummy type annotations,
     when static typing is off. Eventually, this should be replaced by
     a 'non-safe' static typing analysis phase.
 *)

open Typing_context
open Ast_walker_annotate_context
open Ast_walker_annotate

open Xquery_core_ast
open Xquery_core_ast_annotation
open Xquery_type_core_ast


(*****************************)
(* Type annotation operation *)
(*****************************)

(* NB: Mary. The annotation_map is intended to be purely functional:
   it takes an annotation and returns a new annotation.  In type_map
   below, I follow the same convention: the input annotation is
   copied, then updated with the new annotation component.

   An alternative is for annotation_map to be changed to
   annotation_update, and the annotation argument is updated
   directly. *)

let type_update_fn static_ctxt ce =
  set_type_annot ce.pcexpr_annot Schema_builtin.cxtype_item_star

(*******************************)
(* main type-checking function *)
(*******************************)

let notyping_type_cexpr static_ctxt acexpr =
  let annotation_ctxt = build_annotation_context static_ctxt type_update_fn in
  annotate_cexpr annotation_ctxt acexpr;
  static_ctxt

let notyping_type_cstatement static_ctxt acstatement =
  let annotation_ctxt = build_annotation_context static_ctxt type_update_fn in
  annotate_cstatement annotation_ctxt acstatement;
  static_ctxt

let notyping_type_cprolog static_ctxt acprolog =
  let annotation_ctxt = build_annotation_context static_ctxt type_update_fn in
  annotate_cprolog annotation_ctxt acprolog;
  static_ctxt

let notyping_type_cxmodule static_ctxt acxmodule =
  let annotation_ctxt = build_annotation_context static_ctxt type_update_fn in
  annotate_cxmodule annotation_ctxt acxmodule;
  static_ctxt


