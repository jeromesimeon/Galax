(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm_util.ml,v 1.24 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Dm_util
   Description:

   This module implements utility functions on the XQuery 1.0 and
   XPath 2.0 data model.

*)

open Error

open Namespace_names
open Namespace_builtin
open Namespace_symbols

open Datatypes

open Dm_types
open Dm_atomic
open Dm


(*************************)
(* Testing the node kind *)
(*************************)

let isDocumentNode n  		  = n#node_kind() = DocumentNodeKind 
let isElementNode n   		  = n#node_kind() = ElementNodeKind
let isAttributeNode n 		  = n#node_kind() = AttributeNodeKind
let isTextNode n      		  = n#node_kind() = TextNodeKind
let isProcessingInstructionNode n = n#node_kind() = ProcessingInstructionNodeKind
let isCommentNode n               = n#node_kind() = CommentNodeKind


(*****************)
(* Some printing *)
(*****************)

let string_of_node_kind k = 
  match k with
  | DocumentNodeKind -> "document" 
  | ElementNodeKind -> "element" 
  | AttributeNodeKind -> "attribute"
  | TextNodeKind -> "text" 
  | ProcessingInstructionNodeKind -> "processing-instruction"
  | CommentNodeKind -> "comment"

let string_of_atomic_value_kind k =
  Datatypes_util.string_of_atomic_type k


(*******************)
(* Node comparison *)
(*******************)

(* Note:
     nodeid() uses the nodeid method, which may not actually
     require access to document order information. - Jerome
*)

let node_equal n1 n2    = Nodeid.nodeid_is (n1#nodeid()) (n2#nodeid())
let node_precedes n1 n2 = Nodeid.docorder_precedes (n1#docorder()) (n2#docorder())
let node_follows n1 n2 	= Nodeid.docorder_follows (n1#docorder()) (n2#docorder())
let node_compare n1 n2 	= Nodeid.docorder_compare (n1#docorder()) (n2#docorder())

let node_preceding_xpath n1 n2  = Nodeid.is_preceding_xpath (n1#docorder()) (n2#docorder())
let node_ancestor_xpath  n1 n2  = Nodeid.is_ancestor_xpath (n1#docorder()) (n2#docorder())
let node_following_xpath n1 n2  = Nodeid.is_following_xpath (n1#docorder()) (n2#docorder())
let node_descendant_xpath n1 n2 = Nodeid.is_descendant_xpath (n1#docorder()) (n2#docorder())

(*******************)
(* Name operations *)
(*******************)

(* Note:
     The status of the following functions is not clear. They are
     currently used internally by the evaluation engine at various
     places, but should probably be removed at some point.
   - Jerome
 *)

(* For JUNGLE: Operations on the _ElemName things *)

let string_of_ElemName en =
  curly_uri_string_of_rqname (relem_name en)
let string_of_AttrName an =
  curly_uri_string_of_rqname (rattr_name an)

let _ElemName_of_string s =
  let (prefix,uri,name) = parse_curly_uri_string s in
  relem_symbol (prefix,uri,name)
let _AttrName_of_string s =
  let (prefix,uri,name) = parse_curly_uri_string s in
  rattr_symbol (prefix,uri,name)


(* Access to "names" for nodes *)

let get_node_name node =
  let optname = node#node_name() in
  match optname with
  | None -> None
  | Some name -> 
      Some (anon_name ((name :> atomicQName)#getAtomicQName()))

let get_node_qname node =
  let optname = node#node_name() in
  match optname with
  | None -> ""
  | Some name -> 
      anon_prefix_string ((name :> atomicQName)#getAtomicQName())

(* Name indices *)

type name_index = (Namespace_symbols.relem_symbol * Dm.node Dm_atomic_btree_util.full_btree_handler)

