(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm_types.mli,v 1.4 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Dm_types
   Description:
   Galax's abstract data model interface.
*)
open Hashtbl

open Namespace_symbols
open Namespace_context

open Cursor


(***************)
(* Type things *)
(***************)

type nilled = bool


(*********)
(* Kinds *)
(*********)

(* There are 6 kinds of nodes *)

type _NodeKind =
  | DocumentNodeKind
  | ElementNodeKind
  | AttributeNodeKind
  | TextNodeKind
  | ProcessingInstructionNodeKind
  | CommentNodeKind


(* Access operations *)

type 'a access_ops =
    { get_node_kind : 'a -> _NodeKind;
      get_elem_node_name : 'a -> relem_symbol;
      get_attr_node_name : 'a -> rattr_symbol;
      get_elem_node_name_with_type : 'a -> relem_symbol * rtype_symbol;
      get_attr_node_name_with_type : 'a -> rattr_symbol * rtype_symbol;
      get_single_element_node : 'a -> 'a;
      get_document_node_children : 'a -> 'a cursor;
      get_element_node_children : 'a -> 'a cursor;
      get_pi_target : 'a -> string }

