(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util_xpath.ml,v 1.7 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_util_xpath
   Description:
     This module contains some auxiliary evaluation code for XPath.
*)

open Error
open Datatypes
open Datatypes_util

open Xquery_ast
open Xquery_algebra_ast
open Xquery_common_ast

open Dm_types
open Dm
open Dm_util

open Norm_context
open Typing_context

open Code_util_matching

(***************************************)
(* Auxiliary functions for XPath steps *)
(***************************************)

(* Note:
     This part of the code deals contains XPath steps related
     functions.
   - Jerome *)

(* Return list of nodes along an axis *)

let eval_axis_node_test static_context axis anode_test (node : node)=
  let cxschema = schema_from_static_context static_context in
  let nt = Some (Some cxschema,anode_test) in
  match axis with
  | Ancestor ->
      node#ancestor nt
  | Ancestor_or_self ->
      node#ancestor_or_self nt
  | Attribute ->
      Cursor.cursor_map (fun n -> (n :> node)) (node#attributes nt)
  | Child ->
      node#children nt
  | Descendant ->
      node#descendant nt
  | Descendant_or_self ->
      node#descendant_or_self nt
  | Following -> 
      node#following nt
  | Preceding -> 
      node#preceding nt
  | Following_sibling -> 
      begin
	match (node#node_kind()) with
	| AttributeNodeKind ->	Cursor.cursor_empty()
	| _ ->
	    begin
	      match node#parent None with
	      | None ->	Cursor.cursor_empty()
	      | Some p -> Cursor.cursor_filter (fun n -> node_follows n node) (p#children nt)
	    end
      end
  | Preceding_sibling -> 
      begin
	match (node#node_kind()) with
	| AttributeNodeKind ->	Cursor.cursor_empty()
	| _ -> 
	    begin
	      match node#parent None with
	      | None -> Cursor.cursor_empty()
	      | Some p -> Cursor.cursor_filter (fun n -> node_precedes n node) (p#children nt)
	    end
      end
  | Parent ->
      begin
	match node#parent nt with
	| None -> Cursor.cursor_empty()
	| Some p -> Cursor.cursor_of_singleton p
      end
  | Self ->
      node#self nt


(* Node tests checking *)

(* Input object:
     On DM: node
     On SAX: event
 
   Access operations:

      get_node_kind
        On DM: node#node_kind()
        On SAX: 
      get_elem_node_name
        On DM: (node#getElementNode())#elemName()
        On SAX:
      get_elem_node_name_with_type
        On DM: let en = node#getElementNode() in (en#elemName(),en#node_type())
        On SAX:
      get_attr_node_name
        On DM: (node#getAttributeNode())#attrName()
        On SAX:
      get_single_element_node
        On DM:
          let c = n#children() in
          let ce = Cursor.cursor_filter (fun x -> x#node_kind() = ElementNodeKind) c in
          let ce1 =
            try
              Cursor.cursor_get_singleton ce
            with
            | _ -> raise_document_element_singleton ()
          in
          ce1#getElementNode()

       get_pi_target
         On DM:
	   let pin = n#getProcessingInstructionNode() in
	   let target = pin#target() in
	   target#getAtomicString()


*)

