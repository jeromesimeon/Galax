(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_core_ast_annotation.ml,v 1.15 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Xquery_core_ast_annotation
   Description:
     This module contains support for XQuery Core AST annotations.
 *)


open Error
open Xquery_type_core_ast

type ddo_annot =
  | Needs_docorder
  | Needs_distinct
  | Needs_distinct_docorder
  | Needs_none

type scrambling_annot =
  | List
  | Set
  | Bag

type free_var_annot = Xquery_common_ast.cvname list

(** If static typing is enabled, the core is annotated with types
resulting from the static type analysis **)
type type_annot = Xquery_type_core_ast.cxtype

type ast_annot =
    { mutable type_annot       : type_annot option;      (* Static typing *)
      mutable sbdo_optim_annot : ddo_annot option;       (* SBDO analysis *)
      mutable free_var_annot   : free_var_annot option;  (* free variable analysis *) 
      mutable scrambling_annot : scrambling_annot;       (* TPNF normalization *) }

(* Type annotations *)

let empty_ast_annot () = 
  { type_annot       = None;
    sbdo_optim_annot = None;
    free_var_annot   = None;
    scrambling_annot = List; }

let annot_components ast_annot = 
  (ast_annot.type_annot,
   ast_annot.sbdo_optim_annot,
   ast_annot.free_var_annot,
   ast_annot.scrambling_annot)

let copy_annot a =
  let (ta, da, fva, sa) = annot_components a in
  { type_annot       = ta;
    sbdo_optim_annot = da;
    free_var_annot   = fva;
    scrambling_annot = sa }

let set_type_annot ast_annot cxtype = 
  ast_annot.type_annot <- Some cxtype

let get_type_annot ast_annot = 
  match ast_annot.type_annot with
  | None ->
      raise (Query(Annotation_Error("In get_type_annot: Annotation does not contain type")))
  | Some cxtype -> cxtype


(* DDO annotations *)
let set_ddo_annot ast_annot da =
  ast_annot.sbdo_optim_annot <- Some da
  
let get_ddo_annot ast_annot =
  ast_annot.sbdo_optim_annot
  
let print_ddo_annot annot =
  match annot with
      Needs_docorder -> "needs_order"
    | Needs_distinct -> "needs_distinct"
    | Needs_distinct_docorder -> "needs_distinct-docorder"
    | Needs_none -> "needs_none"

(* scrambling annots *)
let get_scrambling_annot annot =
  annot.scrambling_annot

let set_scrambling_annot annot a =
  annot.scrambling_annot <- a

let print_scrambling_annot sa =
  match sa with
  | List -> "list" (* default - ordered semantics *)
  | Set  -> "set"  (* e.g. arguments of ddo-expressions *)
  | Bag  -> "bag"  (* e.g. existential predicates *)

(* Free var annots *)
let set_free_var_annot ast_annot fv =
  ast_annot.free_var_annot <- Some fv

let get_free_var_annot ast_annot = 
  ast_annot.free_var_annot

let rec print_free_var_annot flist = 
  match flist with 
    | [] -> ""
    | x :: [] ->
	(Namespace_names.prefixed_string_of_rqname x) 
    | x :: rest -> 
	(Namespace_names.prefixed_string_of_rqname x) ^ "," ^ print_free_var_annot rest

(* Global setting of a given annotation *)

let set_annotation annot1 annot2 =
  annot1.type_annot <- annot2.type_annot;
  annot1.sbdo_optim_annot <- annot2.sbdo_optim_annot;
  annot1.free_var_annot <- annot2.free_var_annot;
  annot1.scrambling_annot <- annot2.scrambling_annot
