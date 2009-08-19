(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_store.mli,v 1.11 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Shredded_store
   Description:
     This module encapsulates all the structures and methods needed to
     store an XML document within the berkeley db.
*)

open Shredded_store_sigs
open Error

(* This is the store functor. The actual shredded style storage module is 
   parameterized by a series of indexes. Implementing these indexes *)

(* Preorder needs to be in the datamodel, so it can fetch from the correct place *)
(* module Shredded_Store_Functor :  *)
module type Shredded_Store_Functor_Sig =
  functor (Basetypes : Shredded_Basetypes) ->
    functor (Record  : Node_Record 
                                 with type preorder        = Basetypes.preorder 
                                 with type stored_nodeid   = Basetypes.stored_nodeid
                                 with type record_specific = Basetypes.record_specific
                                 with type record_kind     = Basetypes.record_kind
                                 with type eqnameid        = Basetypes.eqnameid
                                 with type namespaceid     = Basetypes.namespaceid
                                 with type prefixid        = Basetypes.prefixid
                                 with type textid          = Basetypes.textid
                                 with type commentid       = Basetypes.commentid
                                 with type processingid    = Basetypes.processingid
   ) ->
      functor (Shredded_Recno  : Shredded_Recno_Functor_Sig) ->
	functor (Shredded_Btree_Functor : Shredded_Btree_Functor_Sig) ->
	  functor (Shredded_Hash_Functor : Shredded_Hash_Functor_Sig) ->	
sig

  type shredded_store 

  (***************************)
  (* Store maintenance calls *)
  (***************************)
  val create_shredded_store : Nodeid_context.nodeid_context -> string -> string -> int -> shredded_store
  val open_store  : string -> string -> shredded_store
  val close_store : shredded_store -> unit
  val sync_store  : shredded_store -> unit
    
  val implemid : Nodeid.implemid

  (* Broken for now *)
  val print_store : shredded_store -> Nodeid.large_preorder -> unit

  (*************)
  (* Accessors *)
  (*************)
  val get_docid : shredded_store -> Nodeid.docid

  val get_root : shredded_store -> Nodeid.nodeid 

  val get_store_from_docid : Nodeid.docid -> shredded_store

  val preorder_of_nodeid : Nodeid.nodeid -> Nodeid.large_preorder

  val docid_of_nodeid    : Nodeid.nodeid -> Nodeid.docid

  val docid_preorder_of_nodeid : Nodeid.nodeid -> Nodeid.docid * Nodeid.large_preorder

  val get_name_of_docid : Nodeid.docid -> string * string

  (**************************)
  (* Old Children Accessors *)
  (**************************)

  val get_first_child :
    Nodeid.nodeid -> Nodeid.nodeid option 

  val get_next_sibling :
    Nodeid.nodeid -> Nodeid.nodeid option 

  (**********************************)
  (* Navigation and Value Accessors *)
  (**********************************)

  val get_children :     
    Nodeid.nodeid -> (Xquery_type_core_ast.cxschema option * Xquery_algebra_ast.anode_test) option -> Nodeid.nodeid Cursor.cursor

  val get_attributes :
    Nodeid.nodeid -> (Xquery_type_core_ast.cxschema option * Xquery_algebra_ast.anode_test) option -> Nodeid.nodeid Cursor.cursor

  val get_parent :
    Nodeid.nodeid -> (Xquery_type_core_ast.cxschema option * Xquery_algebra_ast.anode_test) option -> Nodeid.nodeid option

  val get_nodekind :
    Nodeid.nodeid -> Basetypes.record_kind

  val get_doc_uri :
    Nodeid.nodeid -> Dm_atomic.atomicString option

  val get_elem_name : Nodeid.nodeid -> Namespace_symbols.relem_symbol
  val set_elem_name : Nodeid.nodeid -> Namespace_symbols.relem_symbol -> unit

  val get_attr_name : Nodeid.nodeid -> Namespace_symbols.rattr_symbol   
  val set_attr_name : Nodeid.nodeid -> Namespace_symbols.rattr_symbol -> unit

  val get_type_name : Nodeid.nodeid -> Namespace_symbols.rtype_symbol option
  val set_type_name : Nodeid.nodeid -> Namespace_symbols.rtype_symbol option -> unit



  val retrieve_typed_element   : Nodeid.nodeid -> (Dm_types.nilled * Dm_atomic.atomicValue list) option
  val retrieve_typed_attribute : Nodeid.nodeid -> (Dm_atomic.atomicValue list) option

  val get_content :
    Nodeid.nodeid -> Datatypes.xs_string


  val get_nsenv :
    Nodeid.nodeid -> Namespace_context.nsenv 

  val get_pi_target :
    Nodeid.nodeid -> Datatypes.xs_string

  val get_pi_value :
    Nodeid.nodeid -> Datatypes.xs_string

  val get_comment_value : 
    Nodeid.nodeid -> Datatypes.xs_string


  val get_last_descendant : Nodeid.nodeid -> Nodeid.nodeid
  val improved_descendant : Nodeid.nodeid -> (Xquery_type_core_ast.cxschema option * Xquery_algebra_ast.anode_test) option -> Nodeid.nodeid Cursor.cursor
  val improved_descendant_or_self : Nodeid.nodeid -> (Xquery_type_core_ast.cxschema option * Xquery_algebra_ast.anode_test) option -> Nodeid.nodeid Cursor.cursor
  (*********************)
  (* Loading Interface *)
  (*********************)
    
  val load_shredded_store_from_resolved_stream : 
    Nodeid_context.nodeid_context -> Streaming_types.resolved_xml_stream -> 
    string -> string -> int -> shredded_store
    (** [load_shredded_store_from_resolved_stream xml_stream dir
	name] builds a new Shred store in directory [dir], with
	logical name [name], populated from the stream
	[xml_stream] *)

  val load_shredded_store_from_ordered_typed_stream : Nodeid_context.nodeid_context -> Streaming_types.ordered_typed_xml_stream -> 
    string -> string -> int -> shredded_store  


  (*****************************************************************)
  (* These are update statements. Insertions, deletes and detaches *)
  (*****************************************************************)

  (* val delete_node : bool -> Nodeid.nodeid -> unit *)
    (* Current note: We support the detach semantic for delete. This
    means that if we delete a node x, then it is no longer reachable
    but is still in the store. These nodes should be 'garbage
    collected' at this point, there could be external references to
    them. For example in the query... *)
  val insert_node : Nodeid.nodeid option (* insert location *) -> Nodeid.nodeid (* parent *) -> Physical_value.item Cursor.cursor -> unit
  val detach_node : Nodeid.nodeid -> unit

    
(***********************************)
(* HACKS FOR PROFILING AND TESTING *)
(***********************************)
  val recno_iter : (Record.record -> unit) -> shredded_store -> unit
end

module Shredded_Store_Functor : Shredded_Store_Functor_Sig
