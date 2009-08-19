(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_load_sigs.mli,v 1.7 2007/02/01 22:08:54 simeon Exp $ *)
(* ad hoc loading structure this prevents nastier signatures 
   in other places of the code. *)

module type Shredded_Load_Store = sig
  type shredded_store
  type namespaceid

  type text 
  val get_store_from_docid : Nodeid.docid -> shredded_store
  val preorder_of_nodeid : Nodeid.nodeid -> Nodeid.large_preorder
  val docid_of_nodeid    : Nodeid.nodeid -> Nodeid.docid

  val invalid_nodeid : Nodeid.nodeid
  val close_store : shredded_store -> unit
  val sync_store  : shredded_store -> unit

  val preorder_of_nodeid : Nodeid.nodeid -> Nodeid.large_preorder

  (* Not implemented! - Can these be bundled more carefully? *)

  val get_name_of_docid : Nodeid.docid -> string * string

  val namespaceid_seed  : namespaceid
    (*   val qname_of_string   : string -> qname     *)

  (* Fix these types (no strings) *)
  val shredded_text_of_xml_attribute : string -> text   
  val xml_attribute_of_shredded_text : text -> string

  val shredded_text_of_text_desc     : string -> text
  val text_desc_of_shredded_text     : text -> string

  val xs_untyped_of_text            : text -> Datatypes.xs_untyped
  val text_of_xs_untyped            : Datatypes.xs_untyped -> text 
    
  val create_shredded_store : Nodeid_context.nodeid_context -> string -> string -> int -> shredded_store
  val get_docid : shredded_store -> Nodeid.docid
    

  val store_document_node : 
    shredded_store -> Nodeid.large_preorder -> Nodeid.nodeid

  val store_element_node :
    shredded_store -> Nodeid.large_preorder -> Nodeid.nodeid -> (* Basetypes.shredded_qname *)
    Namespace_symbols.relem_symbol -> 
    (Namespace_symbols.rtype_symbol * Dm_types.nilled * Dm_atomic.atomicValue list) option
    -> namespaceid -> Nodeid.nodeid

  val store_attribute_node :
    shredded_store -> Nodeid.large_preorder -> (Nodeid.nodeid * namespaceid) -> 
    Namespace_symbols.rattr_symbol -> Datatypes.xs_untyped -> 
    (Dm_atomic.atomicValue list * Namespace_symbols.rtype_symbol) option  
    -> Nodeid.nodeid


  val store_text_node :
    shredded_store -> Nodeid.large_preorder -> Nodeid.nodeid -> Datatypes.xs_untyped -> Nodeid.nodeid

  val store_processing_instruction : 
    shredded_store -> Nodeid.large_preorder -> Nodeid.nodeid -> Namespace_names.ncname * Datatypes.xs_untyped -> Nodeid.nodeid

  val store_comment : 
    shredded_store -> Nodeid.large_preorder -> Nodeid.nodeid -> Datatypes.xs_untyped -> Nodeid.nodeid


  val store_children :
    shredded_store -> Nodeid.nodeid -> Nodeid.nodeid Cursor.cursor -> unit 

  val store_attributes :
    shredded_store -> Nodeid.nodeid -> Nodeid.nodeid Cursor.cursor -> unit 

  val store_nsenv :
    shredded_store -> namespaceid ->  Namespace_context.binding_table -> namespaceid 

  val sync_store : shredded_store -> unit
end
