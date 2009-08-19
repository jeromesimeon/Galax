(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm.mli,v 1.39 2007/08/23 21:01:32 simeon Exp $ *)

(* Module: Dm
   Description:
   Galax's abstract data model interface.
*)

open Hashtbl

open Namespace_symbols
open Namespace_context

open Xquery_common_ast
open Xquery_algebra_ast
open Xquery_type_core_ast

open Cursor

open Datatypes

open Dm_atomic
open Dm_types


(********************************)
(* Generic node class hierarchy *)
(********************************)

class virtual node : 
object
  (* Each node must provide a means to identify the implementation it
     comes from *)
  method virtual implementation : unit -> string

  (* DM access operations *)
  method get_access_ops_dm : node access_ops

  (* Downcasts *)

  method virtual getDocumentNode  	      : unit -> document
  method virtual getElementNode   	      : unit -> element
  method virtual getAttributeNode 	      : unit -> attribute
  method virtual getTextNode      	      : unit -> text
  method virtual getProcessingInstructionNode : unit -> processingInstruction
  method virtual getCommentNode               : unit -> comment

  (* Infoset accessors *)

  method virtual node_name    : unit -> atomicQName option
  method virtual base_uri     : unit -> atomicAnyURI option ref
  method virtual node_kind    : unit -> _NodeKind
  method virtual string_value : unit -> xs_string

  (* PSVI accessors *)

  method virtual typed_value        : unit -> atomicValue cursor

  (* Axes *)

  method virtual parent       : (cxschema option * anode_test) option -> node option
  method virtual children     : (cxschema option * anode_test) option -> node cursor
  method virtual attributes   : (cxschema option * anode_test) option -> attribute cursor

  method self                 : (cxschema option * anode_test) option -> node cursor
  method descendant_or_self   : (cxschema option * anode_test) option -> node cursor
  method descendant           : (cxschema option * anode_test) option -> node cursor
  method ancestor_or_self     : (cxschema option * anode_test) option -> node cursor
  method ancestor             : (cxschema option * anode_test) option -> node cursor

  method following     : (cxschema option * anode_test) option -> node cursor
  method preceding     : (cxschema option * anode_test) option -> node cursor

  (* Node identity *)

  (* nodeids are immutable *)
  method virtual nodeid    : unit -> Nodeid.nodeid

  (* nodeids are mutable : can change if document is updated *)
  method virtual docorder  : unit -> Nodeid.docorder
  method virtual update_parent : node -> unit
  method virtual reset_parent : unit -> unit

  (* Updates *)

  method virtual delete        : node -> unit
  method virtual detach        : node -> unit
  method virtual insert        : node cursor -> node -> unit
  method virtual insert_first  : node cursor -> unit
  method virtual replace       : node cursor -> node -> unit
  method virtual replace_value : text -> unit
  method virtual rename        : xs_QName -> unit

  (* Added for convenience *)
  method virtual node_lang : unit -> xs_string option
end

and virtual document :
      atomicAnyURI option ref ->
object
  inherit node

  (* Downcasts *)
  method getDocumentNode  	      : unit -> document
  method getElementNode   	      : unit -> element
  method getAttributeNode 	      : unit -> attribute
  method getTextNode      	      : unit -> text
  method getProcessingInstructionNode : unit -> processingInstruction
  method getCommentNode 	      : unit -> comment

  (* Infoset accessors *)
  method node_name    : unit -> atomicQName option
  method base_uri     : unit -> atomicAnyURI option ref
  method node_kind    : unit -> _NodeKind
  method string_value : unit -> xs_string

  (* PSVI accessors *)
  method typed_value        : unit -> atomicValue cursor

  (* Axes *)
  method parent     : (cxschema option * anode_test) option -> node option
  method attributes : (cxschema option * anode_test) option -> attribute cursor

  (* Accessors specific to document nodes *)

  (* Infoset accessors *)
  method virtual document_uri : unit -> atomicAnyURI option

  (* Updates *)
  method replace       : node cursor -> node -> unit
  method replace_value : text -> unit
  method rename        : xs_QName -> unit

  (* Added for convenience *)
  method node_lang : unit -> xs_string option
end

and virtual element :
      atomicAnyURI option ref ->
object
  inherit node

  (* Downcasts *)
  method getDocumentNode  	      : unit -> document
  method getElementNode   	      : unit -> element
  method getAttributeNode 	      : unit -> attribute
  method getTextNode      	      : unit -> text
  method getProcessingInstructionNode : unit -> processingInstruction
  method getCommentNode               : unit -> comment

  (* Infoset accessors *)
  method base_uri  	       : unit -> atomicAnyURI option ref
  method node_kind 	       : unit -> _NodeKind
  method string_value          : unit -> xs_string

  (* Accessors specific to element nodes *)

  (* Infoset accessors *)
  method virtual elemName              : unit -> relem_symbol
  method virtual namespace_environment : unit -> nsenv

  (* PSVI accessors *)
  method virtual nilled        : unit -> nilled
  method has_element_content   : unit -> bool
  method virtual node_type          : unit -> rtype_symbol
  method virtual export_typed_value : unit -> atomicValue list

  (* Added for convenience *)
  method node_lang : unit -> xs_string option

  (* Updates *)
  method virtual rename        : xs_QName -> unit
end

and virtual attribute :
object
  inherit node

  (* Downcasts *)

  method getDocumentNode  	      : unit -> document
  method getElementNode   	      : unit -> element
  method getAttributeNode 	      : unit -> attribute
  method getTextNode      	      : unit -> text
  method getProcessingInstructionNode : unit -> processingInstruction
  method getCommentNode               : unit -> comment

  (* Infoset accessors *)

  method base_uri  : unit -> atomicAnyURI option ref
  method node_kind : unit -> _NodeKind

  (* Axes *)

  method children   : (cxschema option * anode_test) option -> node cursor 
  method attributes : (cxschema option * anode_test) option -> attribute cursor

  (* Accessors specific to attribute nodes *)

  (* Infoset accessors *)

  method virtual attrName : unit -> rattr_symbol

  (* PSVI accessors *)
  method virtual node_type          : unit -> rtype_symbol
  method virtual export_typed_value : unit -> atomicValue list

  (* Updates *)

  method delete        : node -> unit
  method detach        : node -> unit
  method insert        : node cursor -> node -> unit
  method insert_first  : node cursor -> unit
  method replace       : node cursor -> node -> unit
  method virtual rename        : xs_QName -> unit

  (* Added for convenience *)
  method node_lang : unit -> xs_string option
end
      
and virtual text : 
object
  inherit node

  (* Downcasts *)
  method getDocumentNode  	      : unit -> document
  method getElementNode   	      : unit -> element
  method getAttributeNode 	      : unit -> attribute
  method getTextNode      	      : unit -> text
  method getProcessingInstructionNode : unit -> processingInstruction
  method getCommentNode               : unit -> comment

  (* Infoset accessors *)
  method base_uri  : unit -> atomicAnyURI option ref
  method node_kind : unit -> _NodeKind
  method node_name : unit -> atomicQName option 

  (* PSVI accessors *)
  method typed_value        : unit -> atomicValue cursor

  (* Axes *)
  method children   : (cxschema option * anode_test) option -> node cursor
  method attributes : (cxschema option * anode_test) option -> attribute cursor

  (* Updates *)
  method delete        : node -> unit
  method detach        : node -> unit
  method insert        : node cursor -> node -> unit
  method insert_first  : node cursor -> unit
  method replace       : node cursor -> node -> unit
  method rename        : xs_QName -> unit

  (* Added for convenience *)
  method node_lang : unit -> xs_string option
end

and virtual comment :
object
  inherit node

  (* Downcasts *)
  method getDocumentNode  	      : unit -> document
  method getElementNode   	      : unit -> element
  method getAttributeNode 	      : unit -> attribute
  method getTextNode      	      : unit -> text
  method getProcessingInstructionNode : unit -> processingInstruction
  method getCommentNode               : unit -> comment

  (* Infoset accessors *)
  method base_uri  : unit -> atomicAnyURI option ref
  method node_kind : unit -> _NodeKind
  method node_name : unit -> atomicQName option

  (* PSVI accessors *)
  method typed_value : unit -> atomicValue cursor

  (* Axes *)
  method children   : (cxschema option * anode_test) option -> node cursor
  method attributes : (cxschema option * anode_test) option -> attribute cursor

  (* Updates *)
  method delete        : node -> unit
  method detach        : node -> unit
  method insert        : node cursor -> node -> unit
  method insert_first  : node cursor -> unit
  method replace       : node cursor -> node -> unit
  method rename        : xs_QName -> unit

  (* Added for convenience *)
  method node_lang : unit -> xs_string option
end

and virtual processingInstruction :
object
  inherit node

  (* Downcasts *)
  method getDocumentNode  	      : unit -> document
  method getElementNode   	      : unit -> element
  method getAttributeNode 	      : unit -> attribute
  method getTextNode      	      : unit -> text
  method getProcessingInstructionNode : unit -> processingInstruction
  method getCommentNode               : unit -> comment

  (* Infoset accessors *)
  method base_uri  : unit -> atomicAnyURI option ref
  method node_kind : unit -> _NodeKind

  (* PSVI accessors *)
  method typed_value        : unit -> atomicValue cursor

  (* Axes *)
  method children   : (cxschema option * anode_test) option -> node cursor
  method attributes : (cxschema option * anode_test) option -> attribute cursor

  (* Accessors specific to processing instruction nodes *)

  (* Infoset accessors *)
  method virtual target  : unit -> atomicString (* atomicString, An NCName *)
  method virtual content : unit -> atomicString (* An NCName *)

  (* Updates *)
  method delete        : node -> unit
  method detach        : node -> unit
  method insert        : node cursor -> node -> unit
  method insert_first  : node cursor -> unit
  method replace       : node cursor -> node -> unit
  method virtual rename  : xs_QName -> unit

  (* Added for convenience *)
  method node_lang : unit -> xs_string option
end

