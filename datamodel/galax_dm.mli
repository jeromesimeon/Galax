(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_dm.mli,v 1.17 2007/08/23 21:01:32 simeon Exp $ *)

(* Module: Galax_dm
   Description:
     This module is a main memory implementation of the XQuery 1.0 and
     XPath 2.0 data model. It is based on direct tree, list-based,
     data structures.
*)

open Namespace_symbols

open Xquery_type_core_ast
open Xquery_algebra_ast

open Dm
open Cursor
open Datatypes


(* Galax Items *)

class virtual galaxNode :
    Galax_nodeid.galax_nodeid ->
  object
    inherit node

    method implementation : unit -> string
	
  (* Axes *)
    method parent               : (cxschema option * anode_test) option -> node option

    method descendant_or_self   : (cxschema option * anode_test) option -> node cursor
    method descendant           : (cxschema option * anode_test) option -> node cursor
    method ancestor_or_self     : (cxschema option * anode_test) option -> node cursor
    method ancestor             : (cxschema option * anode_test) option -> node cursor

    method nodeid          	: unit  -> Nodeid.nodeid
    method docorder        	: unit  -> Nodeid.docorder
    method update_parent   	: node -> unit
    method reset_parent   	: unit -> unit
  end


(* Galax Document Nodes *)

class galaxDocumentNode :
    Galax_nodeid.galax_nodeid -> Dm_atomic.atomicAnyURI option ref ->
      node list -> Encoding.encoding option ->
  object
    inherit document
    inherit galaxNode
    method children 	: (cxschema option * anode_test) option -> node cursor
    method document_uri : unit -> Dm_atomic.atomicString option

    method delete     	 : node -> unit
    method detach     	 : node -> unit
    method insert     	 : node cursor -> node -> unit
    method insert_first  : node cursor -> unit
    method replace    	 : node cursor -> node -> unit
  end


(* Galax Element Nodes *)

class galaxElementNode :
    Galax_nodeid.galax_nodeid -> Dm_atomic.atomicAnyURI option ref ->
      relem_symbol -> Namespace_context.nsenv ->
	attribute list -> node list ->
          Dm_types.nilled ->
	    rtype_symbol -> Dm_atomic.atomicValue list ->
  object
    inherit element
    inherit galaxNode

    method children     : (cxschema option * anode_test) option -> node cursor
    method node_name    : unit -> Dm_atomic.atomicQName option

    method node_type   : unit -> rtype_symbol

    method attributes  : (cxschema option * anode_test) option -> attribute cursor
    method elemName    : unit -> relem_symbol
    method namespace_environment : unit -> Namespace_context.nsenv
    method typed_value : unit -> Dm_atomic.atomicValue cursor
    method export_typed_value : unit -> Dm_atomic.atomicValue list

    method has_element_content : unit -> bool

    method nilled        : unit -> Dm_types.nilled

    method delete     	 : node -> unit
    method detach     	 : node -> unit
    method insert     	 : node cursor -> node -> unit
    method insert_first	 : node cursor -> unit
    method replace    	 : node cursor -> node -> unit
    method replace_value : text -> unit
    method rename        : xs_QName -> unit
  end


(* Galax Attribute Nodes *)

class galaxAttributeNode :
    Galax_nodeid.galax_nodeid -> rattr_symbol ->
      Dm_atomic.atomicString -> rtype_symbol
	-> Dm_atomic.atomicValue list ->
  object
    inherit attribute
    inherit galaxNode

    method node_name : unit -> Dm_atomic.atomicQName option

    method node_type : unit -> rtype_symbol

    method string_value : unit -> Datatypes.xs_string
    method typed_value : unit -> Dm_atomic.atomicValue cursor
    method export_typed_value : unit -> Dm_atomic.atomicValue list

    method attrName : unit -> rattr_symbol

    method replace_value : text -> unit
    method rename        : xs_QName -> unit
  end


(* Galax Text Nodes *)

class galaxTextNode :
    Galax_nodeid.galax_nodeid -> Dm_atomic.atomicString ->
  object
    inherit text
    inherit galaxNode

    method string_value : unit -> Datatypes.xs_string

    method replace_value : text -> unit
  end

class galaxCommentNode :
  Galax_nodeid.galax_nodeid ->
  Dm_atomic.atomicString ->
  object
    inherit comment
    inherit galaxNode

    method content : unit -> Dm_atomic.atomicString
    method string_value : unit -> Datatypes.xs_string

    method replace_value : text -> unit
  end


(* Galax Processing Instruction Nodes *)

class galaxProcessingInstructionNode :
  Galax_nodeid.galax_nodeid ->
  Dm_atomic.atomicString ->
  Dm_atomic.atomicString ->
  object
    inherit processingInstruction
    inherit galaxNode

    method node_name : unit -> Dm_atomic.atomicQName option

    method string_value : unit -> Datatypes.xs_string

    method content : unit -> Dm_atomic.atomicString
    method target : unit -> Dm_atomic.atomicString

    method replace_value : text -> unit
    method rename        : xs_QName -> unit
  end


(* Deep copy hack *)

val register_deep_copy_fun : (Nodeid_context.nodeid_context -> Nodeid.docid -> node cursor -> node list) -> unit


