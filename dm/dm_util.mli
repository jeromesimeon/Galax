(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm_util.mli,v 1.22 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Dm_util
   Description:

   This module implements a number of short-cut functions to construct
   and access the XQuery 1.0 and XPath 2.0 data model.

*)

open Datatypes

open Namespace_symbols

open Dm_types
open Dm


(*************************)
(* Testing the node kind *)
(*************************)

val isDocumentNode     		: node -> bool
val isElementNode      		: node -> bool
val isAttributeNode    		: node -> bool
val isTextNode         		: node -> bool
val isProcessingInstructionNode : node -> bool
val isCommentNode      		: node -> bool



(*****************)
(* Some printing *)
(*****************)

val string_of_node_kind 	: _NodeKind -> string
val string_of_atomic_value_kind : Datatypes.atomic_type-> string


(*******************)
(* Node comparison *)
(*******************)

val node_equal    : node -> node -> bool
val node_precedes : node -> node -> bool
val node_follows  : node -> node -> bool
val node_compare  : node -> node -> int

val node_preceding_xpath  :  node -> node -> bool
val node_following_xpath  :  node -> node -> bool
val node_ancestor_xpath   :  node -> node -> bool
val node_descendant_xpath :  node -> node -> bool

(*******************)
(* Name operations *)
(*******************)

(* Note:
     The status of the following functions is not clear. They are
     currently used internally by the evaluation engine at various
     places, but should probably be removed at some point.
   - Jerome
 *)

(* For JUNGLE: Operations on the relem_symbols things *)

val string_of_ElemName : relem_symbol -> string
val string_of_AttrName : rattr_symbol -> string

val _ElemName_of_string : string -> relem_symbol
val _AttrName_of_string : string -> rattr_symbol


(* Access to "names" for nodes *)

val get_node_name    : node  -> Namespace_names.rqname option
val get_node_qname   : node  -> string


(* Name indices *)

type name_index = (Namespace_symbols.relem_symbol * Dm.node Dm_atomic_btree_util.full_btree_handler)

