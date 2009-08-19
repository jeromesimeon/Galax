(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_dm.ml,v 1.19 2007/08/23 21:01:32 simeon Exp $ *)

(* Module: Shredded_dm
   Description:
     This module is the Shred implementation of the DM abstract data
     model.
*)

open Error

open Namespace_context
open Namespace_symbols

open Datatypes

(* Generic data model *)

open Dm_atomic
open Dm
open Dm_util
open Physical_value

(*************)
(* XML Nodes *)
(*************)

(* Shred items *)
open Shredded_store_sigs
open Shredded_store


module Shredded_Datamodel 
  (Store_Functor : Shredded_Store_Functor_Sig)
  (Basetypes : Shredded_Basetypes) 
  (Record_Functor : Node_Record  with type preorder        = Basetypes.preorder 
                                 with type stored_nodeid   = Basetypes.stored_nodeid
                                 with type record_specific = Basetypes.record_specific
                                 with type record_kind     = Basetypes.record_kind
                                 with type eqnameid        = Basetypes.eqnameid
                                 with type namespaceid     = Basetypes.namespaceid
                                 with type prefixid        = Basetypes.prefixid
                                 with type textid          = Basetypes.textid
                                 with type commentid       = Basetypes.commentid
                                 with type processingid    = Basetypes.processingid
   )
  (Shredded_Recno_Func : Shredded_Recno_Functor_Sig)
  (Shredded_Btree_Func : Shredded_Btree_Functor_Sig)
  (Shredded_Hash_Func  : Shredded_Hash_Functor_Sig)
= struct
  (* This the "code" that creates a store *)

  module Shredded_Store = 
    Shredded_Store_Functor 
      (Basetypes) (Record_Functor)
      (Shredded_Recno_Func)
      (Shredded_Btree_Func)
      (Shredded_Hash_Func)
      
  (* Shred nodes *)

  class virtual shredNode id = 
    let _ =  Monitoring_context.increment_node_count () in
  object (self)
    inherit node

    method implementation () = "Shred"
    val mutable node_id : Nodeid.nodeid = id
					    (* Keep docorder and nodeid the same for now - Jerome *) 
    method nodeid () = node_id

    method docorder () =
      let docid,pre = Shredded_Store.docid_preorder_of_nodeid node_id in 
	Shredded_Store.implemid,Nodeid.PreIntPair(docid, pre)

    method update_parent n =
      raise (Query (Prototype "UPDATES ARE NOT SUPPORTED IN SHRED"))

    method reset_parent n =
      raise (Query (Prototype "UPDATES ARE NOT SUPPORTED IN SHRED"))


    method parent nto =
      let parent_nodeid = Shredded_Store.get_parent node_id nto in
	match parent_nodeid with
	  | None -> None
	  | Some pnodeid ->
	      let new_obj =
		begin
		  let parent_node_kind = Shredded_Store.get_nodekind pnodeid in
		    match parent_node_kind with
		      | Basetypes.DocumentRecordKind ->
			  ((new shredDocumentNode pnodeid (Dm_atomic_util.default_no_uri_dm)) :> node)
		      | Basetypes.ElementRecordKind ->
			  ((new shredElementNode pnodeid (Dm_atomic_util.default_no_uri_dm)) :> node)
		      | _ ->
			  raise  (Query (Shredded_Error "Wrong kind of node as parent of a Shred node")) 
			    
		end
	      in (Some new_obj)

  (***********************************************************)
  end

  and shredDocumentNode id docuri = 
  object (self)
    inherit document (docuri) as superdoc
    inherit shredNode (id) as super

    method children nto =         
      let build_new_object kid =
	match Shredded_Store.get_nodekind kid with
	  | Basetypes.ElementRecordKind ->
	      ((new shredElementNode kid Dm_atomic_util.default_no_uri_dm) :> node)
	  | Basetypes.TextRecordKind    ->
	      ((new shredTextNode kid) :> node)
	  | Basetypes.PIRecordKind      ->
	      ((new shredProcessingInstructionNode kid) :> node)
	  | Basetypes.CommentRecordKind ->
	      ((new shredCommentNode kid) :> node)
	  | _ ->
	      raise (Query (Shredded_Error "Wrong kind of node as children of a Shred node"))
      in    
	(* Old children method *)	
	if !(Conf.old_children_method) then begin	  
	  (* NODE TESTS ARE IGNORED!! - JEROME *)
	  let first_child_id = Shredded_Store.get_first_child node_id in
	  let current_id = ref first_child_id in
	  let children_fun x =
	    begin
              match !current_id with
		| None -> None
		| Some id ->
                    current_id := Shredded_Store.get_next_sibling id; Some (build_new_object id)
            end
	  in
	    Cursor.cursor_of_function children_fun
	end else Cursor.cursor_map build_new_object (Shredded_Store.get_children node_id nto)

	  
    method descendant_or_self nto = 
      let build_new_object kid =
	match Shredded_Store.get_nodekind kid with
	  | Basetypes.ElementRecordKind ->
	      ((new shredElementNode kid Dm_atomic_util.default_no_uri_dm) :> node)
	  | Basetypes.TextRecordKind    ->
	      ((new shredTextNode kid) :> node)
	  | Basetypes.PIRecordKind      ->
	      ((new shredProcessingInstructionNode kid) :> node)
	  | Basetypes.CommentRecordKind ->
	      ((new shredCommentNode kid) :> node)
	  | _ ->
	      raise (Query (Shredded_Error "Wrong kind of node as children of a Shred node"))
      in    
	if !Conf.new_descendant_style then
	  begin
	    Cursor.cursor_map build_new_object 
	      (Shredded_Store.improved_descendant_or_self node_id nto)
	  end
	else superdoc#descendant_or_self nto      

    method descendant nto = 
      let build_new_object kid =
	match Shredded_Store.get_nodekind kid with
	  | Basetypes.ElementRecordKind ->
	      ((new shredElementNode kid Dm_atomic_util.default_no_uri_dm) :> node)
	  | Basetypes.TextRecordKind    ->
	      ((new shredTextNode kid) :> node)
	  | Basetypes.PIRecordKind      ->
	      ((new shredProcessingInstructionNode kid) :> node)
	  | Basetypes.CommentRecordKind ->
	      ((new shredCommentNode kid) :> node)
	  | _ ->
	      raise (Query (Shredded_Error "Wrong kind of node as children of a Shred node"))
      in    
	if !Conf.new_descendant_style then 
	  begin
	    Cursor.cursor_map build_new_object 
	      (Shredded_Store.improved_descendant node_id nto)
	  end
	else superdoc#descendant nto


    method document_uri () = Shredded_Store.get_doc_uri node_id

    method insert insert_content insert_location =      
      let item_sequence = Cursor.cursor_map (fun n -> (Physical_value.Item_Node n)) insert_content in 
      let insert_nodeid = Some (insert_location#nodeid ()) in 
      Shredded_Store.insert_node insert_nodeid node_id item_sequence

    method insert_first insert_content =      
      let item_sequence = Cursor.cursor_map (fun n -> (Physical_value.Item_Node n)) insert_content in 
      let insert_nodeid = None in
      Shredded_Store.insert_node insert_nodeid node_id item_sequence

    method detach detach_child = Shredded_Store.detach_node (detach_child#nodeid ())
				   (* DELETE SHOULD *PROBABLY BE FIXED - JEROME *)
    method delete delete_child = Shredded_Store.detach_node (delete_child#nodeid ())

    method replace replace_content replaced_node =
      raise (Query (Prototype "Replace is not supported in Shred [Document]"))
  end

  and shredElementNode id baseuri =
  object (self)
    inherit element(baseuri) as superelem
    inherit shredNode (id) as super

    method descendant_or_self nto = 
      let build_new_object kid =
	match Shredded_Store.get_nodekind kid with
	  | Basetypes.ElementRecordKind ->
	      ((new shredElementNode kid Dm_atomic_util.default_no_uri_dm) :> node)
	  | Basetypes.TextRecordKind    ->
	      ((new shredTextNode kid) :> node)
	  | Basetypes.PIRecordKind      ->
	      ((new shredProcessingInstructionNode kid) :> node)
	  | Basetypes.CommentRecordKind ->
	      ((new shredCommentNode kid) :> node)
	  | _ ->
	      raise (Query (Shredded_Error "Wrong kind of node as children of a Shred node"))
      in    
	if !Conf.new_descendant_style then
	  begin
	    Cursor.cursor_map build_new_object 
	      (Shredded_Store.improved_descendant_or_self node_id nto)
	  end
	else superelem#descendant_or_self nto      

    method descendant nto = 
      let build_new_object kid =
	match Shredded_Store.get_nodekind kid with
	  | Basetypes.ElementRecordKind ->
	      ((new shredElementNode kid Dm_atomic_util.default_no_uri_dm) :> node)
	  | Basetypes.TextRecordKind    ->
	      ((new shredTextNode kid) :> node)
	  | Basetypes.PIRecordKind      ->
	      ((new shredProcessingInstructionNode kid) :> node)
	  | Basetypes.CommentRecordKind ->
	      ((new shredCommentNode kid) :> node)
	  | _ ->
	      raise (Query (Shredded_Error "Wrong kind of node as children of a Shred node"))
      in    
	if !Conf.new_descendant_style then begin
	  Cursor.cursor_map build_new_object (Shredded_Store.improved_descendant node_id nto)
	end
	else superelem#descendant nto

    method children nto =
      (* This is one reason we don't have nice locality, even if our
	 accessors do, we are forced to hit the main record to get the
	 kind - Chris *)
      let build_new_object kid =
	match Shredded_Store.get_nodekind kid with
	  | Basetypes.ElementRecordKind ->
	      ((new shredElementNode kid Dm_atomic_util.default_no_uri_dm) :> node)
	  | Basetypes.TextRecordKind    ->
	      ((new shredTextNode kid) :> node)
	  | Basetypes.PIRecordKind      ->
	      ((new shredProcessingInstructionNode kid) :> node)
	  | Basetypes.CommentRecordKind ->
	      ((new shredCommentNode kid) :> node)
	  | _ ->
	      raise (Query (Shredded_Error "Wrong kind of node as children of a Shred node"))
      in    
	if !(Conf.old_children_method) then begin
	  let first_child_id = Shredded_Store.get_first_child node_id in
	  let current_id = ref first_child_id in
	  let children_fun x =
	    begin
	      match !current_id with
		| None -> None
		| Some id ->
		    current_id := 
		    Shredded_Store.get_next_sibling id; 
		    Some (build_new_object id)
	    end
	  in
	    Cursor.cursor_of_function children_fun
	end else Cursor.cursor_map build_new_object (Shredded_Store.get_children node_id nto)
	  
    method node_type() = 
      match Shredded_Store.get_type_name node_id with
	| Some v -> v
	| None -> Namespace_symbols.anytype
	    (* This is an error, node_type should never be none! *)
	    (* raise (Query (Shredded_Error ("Element type annotation should never be none"))) *)

    method nilled()    = 
      match Shredded_Store.retrieve_typed_element node_id with	
	| None -> false (* Default? *)
	| Some (nilled, av_list) -> nilled
	    

    method node_name ()    = Some (new atomicQName (Shredded_Store.get_elem_name node_id))

    method elemName() = Shredded_Store.get_elem_name node_id
    method namespace_environment () =
      Shredded_Store.get_nsenv node_id 
	
    method attributes nto =
      let build_new_attribute_object kid = 
	match Shredded_Store.get_nodekind kid with
	  | Basetypes.AttributeRecordKind ->
	      ((new shredAttributeNode kid) :> attribute)
	  | _ ->
	      raise (Query (Shredded_Error "Wrong kind of node as attributes of a Shred element"))
      in
      let kid_ids = Shredded_Store.get_attributes node_id nto in
	Cursor.cursor_map build_new_attribute_object kid_ids
	  

    method export_typed_value () =       
      match Shredded_Store.retrieve_typed_element node_id with	
	| None -> (* Cursor.cursor_empty () *) []
	| Some (nilled, av_list) -> 
	    (* Cursor.cursor_of_list *) av_list

    (* This isn't correct!!! *)
    method typed_value () =  
      let elem_value = self#export_typed_value () in      
      let ta = self#node_type () in
	(* Case 1. above: Need to check for mixed content here: *)
	if (ta = Namespace_symbols.untypedsym || ta = Namespace_symbols.anytype) 
	then Cursor.cursor_of_singleton(new atomicUntyped(self#string_value()))
	  (* Case 2. above : NOT IMPLEMENTED COMPLETELY *)
	else if (not (elem_value = []))
	then Cursor.cursor_of_list elem_value
	  (* Case 3. *)
	else if Cursor.cursor_is_empty (self#children None) 
	then Cursor.cursor_empty()
	  (* Case 4. NOT IMPLEMENTED CORRECTLY *)
	else Cursor.cursor_of_singleton (new atomicUntyped (self#string_value()))

    method insert insert_content insert_location =       
      let item_sequence = Cursor.cursor_map (fun n -> (Physical_value.Item_Node n)) insert_content in 
      let insert_nodeid = Some (insert_location#nodeid ()) in
      Shredded_Store.insert_node insert_nodeid node_id item_sequence
	  
    method insert_first insert_content =       
      let item_sequence = Cursor.cursor_map (fun n -> (Physical_value.Item_Node n)) insert_content in 
      let insert_nodeid = None in
      Shredded_Store.insert_node insert_nodeid node_id item_sequence
	  
    method detach detach_child = Shredded_Store.detach_node (detach_child#nodeid ())
				   (* DELETE SHOULD PROBABLY BE FIXED - JEROME *)
    method delete delete_child = Shredded_Store.detach_node (delete_child#nodeid ())
    method replace replace_content replaced_node =
      self#insert replace_content replaced_node;
      self#detach replaced_node

      (* raise (Query (Prototype "REPLACE NOT IN DATAMODEL YET [Element]")) *)
    method replace_value replace_content =
      raise (Query (Prototype "REPLACE VALUE NOT IN DATAMODEL YET [Element]"))
    method rename newName =
      raise (Query (Prototype "RENAME NOT IN THE SHRED DATAMODEL YET [Element]"))

  end

  and shredAttributeNode id = 
  object (self)
    inherit attribute
    inherit shredNode (id) as super

    (* Should default be untyped not none? *)
    method node_type() = 
      match Shredded_Store.get_type_name node_id with
	| None -> Namespace_symbols.anytype
	    (* This is an error, node_type should never be none! *)
	    (* raise (Query (Shredded_Error ("Attribute type annotation should never be none"))) *)
	| Some v -> v

    method node_name() = Some (new atomicQName (Shredded_Store.get_attr_name node_id))

    method attrName() = Shredded_Store.get_attr_name node_id

    method export_typed_value() = (* [new atomicUntyped(Shredded_Store.get_content node_id)] *)
      let ta = self#node_type() in
	if (Namespace_symbols.rtype_equal ta Namespace_symbols.untypedAtomicsym)
	then [(new atomicUntyped(Shredded_Store.get_content node_id))]
	else
	  match Shredded_Store.retrieve_typed_attribute node_id with	
	    | None -> [(new atomicUntyped(Shredded_Store.get_content node_id))]
	    | Some av_list -> av_list 

    method typed_value () = 
      Cursor.cursor_of_list(self#export_typed_value ())

    method string_value () = Shredded_Store.get_content node_id

    method replace_value replace_content =
      raise (Query (Prototype "REPLACE VALUE NOT IN DATAMODEL YET [Attribute]"))
    method rename newName =
      raise (Query (Prototype "RENAME NOT IN THE SHRED DATAMODEL YET [Attribute]"))

  end

  and shredTextNode id = 
  object
    inherit text
    inherit shredNode (id) as super

    method typed_value () = Cursor.cursor_of_singleton (new atomicUntyped (Shredded_Store.get_content node_id))
    method string_value () = Shredded_Store.get_content node_id

    method replace_value replace_content =
      raise (Query (Prototype "REPLACE VALUE NOT IN DATAMODEL YET [TEXT]"))
  end

  and shredCommentNode id = 
  object
    inherit comment
    inherit shredNode (id) as super

    method typed_value () = Cursor.cursor_of_singleton(new atomicUntyped (Shredded_Store.get_comment_value node_id))
    method string_value () = Shredded_Store.get_comment_value node_id
    method content () = new atomicString(Shredded_Store.get_comment_value node_id)

    method replace_value replace_content =
      raise (Query (Prototype "REPLACE VALUE NOT IN DATAMODEL YET [Attribute]"))
  end

  and shredProcessingInstructionNode id =
  object
    inherit processingInstruction
    inherit shredNode (id) as super

    method node_name () =  Some (new atomicQName (anon_symbol (Namespace_names.NSDefaultElementPrefix, Namespace_names.NSUri "", Shredded_Store.get_pi_target node_id)))

    method typed_value () = Cursor.cursor_of_singleton(new atomicUntyped (Shredded_Store.get_pi_value node_id))
    method string_value () = Shredded_Store.get_pi_value node_id
    method target () = new atomicString(Shredded_Store.get_pi_target node_id)
    method content () = new atomicString(Shredded_Store.get_pi_value node_id)

    method replace_value replace_content =
      raise (Query (Prototype "REPLACE VALUE NOT IN DATAMODEL YET [PI]"))
    method rename newName =
      raise (Query (Prototype "RENAME NOT IN THE SHRED DATAMODEL YET [PI]"))
  end
    

  (************************************************)
  (* Provided to ease registration module hassles *)
  (************************************************)
  let registration_store_handle = ref None
  let update_store_handle  h    = 
    match !registration_store_handle with
      | None -> registration_store_handle := Some h
      | Some v -> 
	  raise (Query (Shredded_Error "Store handle already registered!"))
	    
  let open_and_get_root directory name =   
    let shredded_store = Shredded_Store.open_store directory name in
    let ()             = update_store_handle shredded_store in 
    let store_root     = Shredded_Store.get_root shredded_store in
    let doc_node       = new shredDocumentNode store_root Dm_atomic_util.default_no_uri_dm in
      [(Item_Node (doc_node :> node))]

  let implem_name = Basetypes.implem_name

  let close () = 
    match !registration_store_handle with 
      | None -> 
	  (* Format.printf "Never opened..@."; *)
	  () (* Never opened *)
      | Some store -> 
	  Shredded_Store.close_store store

  let sync  () = 
    match !registration_store_handle with 
      | None -> () (* Never opened *)
      | Some store -> 
	  Shredded_Store.sync_store store
end
