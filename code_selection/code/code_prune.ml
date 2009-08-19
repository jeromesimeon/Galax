(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_prune.ml,v 1.12 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_parse
   Description:
     This module contains code building for operators that implement
     pruning of sequences of XML items.
*)

open Error
open Code_selection_context
open Cs_util_coercion
open Physical_item
open Physical_sequence
open Physical_value
open Physical_value_util
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_common_ast
(***********************)
(* Pruning over tuples *)
(***********************)

let get_pre_post item =
  let node = getNode (Physical_util.get_item (cursor_of_sequence item)) in
  match snd (node#docorder()) with
  | Nodeid.PrePostInt (docid, pre, post) -> (docid, pre, post)
  | _ -> raise (Query (Internal_Error ("Pruning operator does not support NodeId format (code_prune)")))


let prune_descendant input_cursor retrieve_code =
  let buffer = ref None in
  let filter next_tuple =
    let filter_aux b = 
      let docid1, pre1, post1 = get_pre_post b in
      let next_item = (retrieve_code ()) in 
      let docid2, pre2, post2 = get_pre_post next_item in
      
      if (docid1 == docid2) && pre1 < pre2 && post1 > post2 
      then
	false
      else
	let _ = buffer := Some next_item in 
	true
    in
    match !buffer with
    | Some b -> filter_aux b
    | None -> 
        let node = retrieve_code () in
        buffer := Some node;
	filter_aux node
  in
    Cursor.cursor_filter filter input_cursor

(* Only keep the qqdeepest descendant of the first input node *)
let prune_preceding input_cursor retrieve_code =
  let filter t =
    match Cursor.cursor_peek input_cursor with
    | Some _ -> false
    | None -> true
  in
  Cursor.cursor_filter filter input_cursor

(*  Note: semantics of Prune op requires input list to be SBDO, w/o duplicates *)
let build_default_prune_code code_ctxt axis fieldname =
  let _ = build_create_dom_tuple_code code_ctxt fieldname in
  let retrieve_code = build_retrieve_dom_tuple_code code_ctxt fieldname in

  (fun alg_ctxt (input_cursor : tuple_unit Cursor.cursor) -> 
    match axis with
    | Xquery_common_ast.Descendant ->
	prune_descendant input_cursor retrieve_code 
    | Xquery_common_ast.Preceding ->
	prune_preceding input_cursor retrieve_code
    | _ -> raise (Query (Internal_Error ("Axis not supported for pruning")))
  )

let build_prune_code code_ctxt algop (field, axis) =
  let _ = access_onesub algop.psub_expression in
  let _ = access_nosub algop.pdep_sub_expression in
  let fn = build_default_prune_code code_ctxt axis field in
  (coerce_nodep fn coerce_unary_tuple_cursor_to_tuple_cursor), code_ctxt
