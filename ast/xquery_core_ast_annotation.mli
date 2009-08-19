(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* Module: Xquery_core_ast_annotation
   Description:
     This module contains support for XQuery Core AST annotations.
 *)

(*
   Semantics notes:

     Normalization always yields an XQuery Core AST in which only the
     XPath annotation is set.

     An annotation phase (e.g., static typing, sort-by-document-order,
     etc.) takes an AST in which the corresponding annotation
     components are None, and yields an AST in which all the
     corresponding annotation components have been set.

     Setting an annotation component of an annotation destructively
     modifies the annotation.  Accessing an annotation component in an
     annotation that is not set raises an error.  *)


(***************)
(* Annotations *)
(***************)

type ddo_annot =
  | Needs_docorder
  | Needs_distinct
  | Needs_distinct_docorder
  | Needs_none

(* TPNF scrambling annotations *)
type scrambling_annot =
  | List
  | Set
  | Bag

(* If static typing is enabled, the core is annotated with types
   resulting from the static type analysis *)
type type_annot = Xquery_type_core_ast.cxtype

(* Free variables annotations indicates the variable usage within a
   given core expression *)
type free_var_annot = Xquery_common_ast.cvname list

(* The type for the annotations themselves *)
type ast_annot


(*****************************)
(* Operations on annotations *)
(*****************************)

(* Create an empty AST annotation. 
    @return New AST annotation.*)
val empty_ast_annot : unit -> ast_annot

(* Set an annotation from an existing annotation *)
val set_annotation : ast_annot -> ast_annot -> unit

(* Return all the annotation's components 
    @return Tuple of annotation's components *)
val annot_components :
    ast_annot ->
      type_annot option * ddo_annot option * free_var_annot option * scrambling_annot

(*  Copy an annotation
    @param ast_annotation 
    @return copy of annotation *)
val copy_annot  : ast_annot -> ast_annot


(* Type annotations *)
val set_type_annot : ast_annot -> type_annot -> unit
val get_type_annot : ast_annot -> type_annot

(* DDO annotations *)
val set_ddo_annot   : ast_annot -> ddo_annot -> unit
val get_ddo_annot   : ast_annot -> ddo_annot option
val print_ddo_annot : ddo_annot -> string

(* Free variables annotations *)
val set_free_var_annot   : ast_annot -> free_var_annot -> unit
val get_free_var_annot   : ast_annot -> free_var_annot option
val print_free_var_annot : free_var_annot -> string

(* scrambling annot *)
val set_scrambling_annot   : ast_annot -> scrambling_annot -> unit
val get_scrambling_annot   : ast_annot -> scrambling_annot
val print_scrambling_annot : scrambling_annot -> string
