(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ast_walker_annotate_context.ml,v 1.2 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Ast_walker_annotate_context
   Description:
     Context for the annotation walker. An annotation_context is
     polymorphic because a particular annotator module may
     require/construct an annotation-specific context.
*)

open Xquery_core_ast

type 'a annotation_update_fn = 'a -> acexpr -> unit

type 'a annotation_context =
    { annot_context : 'a;
      annot_update_fn : 'a annotation_update_fn }

let build_annotation_context context update_fn =
  { annot_context        = context;
    annot_update_fn      = update_fn; }

let get_annotation_context annot_context =
  annot_context.annot_context

let get_annotation_update_fn annot_context =
  annot_context.annot_update_fn

