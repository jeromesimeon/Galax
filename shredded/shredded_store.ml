(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_store.ml,v 1.17 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Shredded_store
   Description:
     This module encapsulates all the structures and methods needed to
     store an XML document within the berkeley db.
*)

(* A Note About Nodeids:
   --------------------   
   There are three types of nodeids visible at different levels.
   
   o Nodeids: These are the ones visible to Dm.
     The DM interacts with uses these for two things:
     o Identifiying the node
     o Identifying the store, this is clearly necessary
       to identify the node and so must be part site-wide.

     (implem_id, (docid, _))

   o Stored_Nodeids: These are just nodeids, but lack
     the document identifier. This document identifier
     can change at runtime so it is not reasonable to 
     serialize it.  

   In order, to accomodate this split and avoid repeated conversions
   There are external versions (visible) these wrap the external type,
   Nodeid.nodeid, and call internal versions which deal only with
   stored_nodeid. This is necessary because the store needs to call
   some of its own internal functions; in order to construct types we
   need access to the store which is cumbersome to carry around, this
   localizes it.

   Serialization
   -------------

   We need to get the last issued nodeid but we want to avoid storing
   the nodeid of each node in the record, so we are storing it in the
   catalog. It is a key "max nodeid" from where we will start issuing
   nodes. This started because I wanted to demphasize the ordering of
   nodeids, they are a purely logical entitity which correspond only
   with a record number. 

   The only convention we insist on is that the root be at logical id
   1. If this is restrictive, just store it in the metadata.

*)
  
open Error
open Nodeid
open Filename


open Shredded_store_sigs


(* This code has the functor and the sigs to create the modules *)
(* Constant string keys *)
let last_nodeid_metadata_key   = "Last nodeid Id"
let buff_size_metadata_key     = "buff_size"
let hi_max_pre_order_metadata_key = "high_max_pre_order"
let lo_max_pre_order_metadata_key = "high_max_pre_order"

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
    Nodeid_context.nodeid_context -> Streaming_types.xml_stream -> 
    string -> string -> int -> shredded_store
    (** [load_shredded_store_from_resolved_stream xml_stream dir
	name] builds a new Shred store in directory [dir], with
	logical name [name], populated from the stream
	[xml_stream] *)

  val load_shredded_store_from_ordered_typed_stream : Nodeid_context.nodeid_context -> Streaming_types.ordered_xml_stream -> 
    string -> string -> int -> shredded_store  

  (*****************************************************************)
  (* These are update statements. Insertions, deletes and detaches *)
  (*****************************************************************)

  (* val delete_node : bool -> Nodeid.nodeid -> unit *)

  val insert_node : Nodeid.nodeid option -> Nodeid.nodeid -> Physical_value.item Cursor.cursor -> unit
  val detach_node : Nodeid.nodeid -> unit

  val recno_iter : (Record.record -> unit) -> shredded_store -> unit    
end



module Shredded_Store_Functor
  (Basetypes      : Shredded_Basetypes) 
  (Record         : Node_Record with type preorder        = Basetypes.preorder 
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
  (Recno_Functor  : Shredded_Recno_Functor_Sig) 
  (Btree_Functor  : Shredded_Btree_Functor_Sig) 
  (Hash_Functor   : Shredded_Hash_Functor_Sig) 
  = struct
    (***************************)
    (* Call the input functors *)
    (***************************)
    
    (* Create Structures *)
    module Recno          = Recno_Functor (Basetypes) (Record)

    (* Btrees *)
    module Nodeid_Btree   = Btree_Functor (Basetypes.Stored_Nodeid_Module)   (Basetypes.Stored_Preorder_Nodeid_Module)
    module Textid_Btree   = Btree_Functor (Basetypes.Textid_Module)   (Basetypes.Text_Module)

    (* Hashes *)       
    module Metadata_Hash            = Hash_Functor (Basetypes.Metadata_Key_Module) (Basetypes.Metadata_Value_Module)
    module Namespaceid_Binding_Hash = Hash_Functor (Basetypes.Namespaceid_Module)  (Basetypes.Binding_Module)
    module Comment_Hash             = Hash_Functor (Basetypes.Commentid_Module)    (Basetypes.Comment_Module)
    module Processingid_Hash        = Hash_Functor (Basetypes.Processingid_Module) (Basetypes.Processing_Instruction_Module)
    module Typed_Element_Value_Hash = Hash_Functor (Basetypes.Stored_Nodeid_Module) (Basetypes.Stored_Atomic_Value_List_With_Nilled_Module)
    module Typed_Attribute_Value_Hash = Hash_Functor (Basetypes.Stored_Nodeid_Module) (Basetypes.Stored_Atomic_Value_List_Module)

    (* Two Way Mappings *)       
    module Two_Way_Mapping_Functor     = 
      Shredded_qname_mapping.Shredded_Qname_Qnameid_Mapping (Basetypes) (Hash_Functor) 
    module Element_Mapping         = Two_Way_Mapping_Functor (Shredded_qname_mapping.Element_Lookup)
    module Attribute_Mapping       = Two_Way_Mapping_Functor (Shredded_qname_mapping.Attribute_Lookup)
    module Type_Mapping            = Two_Way_Mapping_Functor (Shredded_qname_mapping.Type_Lookup)

    (* Renumber policy *)
    module Renumber_Callback       = struct 
      type preorder = Basetypes.preorder 
      type nodeid   = Basetypes.stored_nodeid
      type handle   = (Recno.recno_handle * Nodeid_Btree.btree_handle)
	  
      (* The performance is unacceptably slow *)
      let update_preorder (rec_h,children_btree_h) nodeid prev_preorder new_preorder = 
	match Recno.recno_get rec_h nodeid with
	  | Some r -> 
	      (* Update the record *)
	      (* This should be done with a cursor over the original database *)
	      let kind   = Record.get_kind r in 
	      let _  = 
		if kind = Basetypes.ElementRecordKind    
		then Record.get_name_eqnameid r
		else -2 
	      in
	      let parent = Record.get_parent r in		 
	      let record = Record.set_preorder r new_preorder in 
		 (* Format.printf "Putting Nodeid %d with preorder %a [old: %a] Qname: %d | %d@."
		  nodeid 
		  Shredded_renumber.Cell_As_Int64.print_int64_pair new_preorder
		  Shredded_renumber.Cell_As_Int64.print_int64_pair prev_preorder		
		  (if kind = Basetypes.ElementRecordKind    
		   then Record.get_name_eqnameid record
		   else -2) old_t;  *)
		Recno.recno_put rec_h nodeid record;

		(* Update the child index *)
		
		begin 
		  match kind with
		      (* Documents and attributes can't be children of other nodes *)
		    | Basetypes.DocumentRecordKind  
		    | Basetypes.AttributeRecordKind ->  ()

		    | Basetypes.CommentRecordKind
		    | Basetypes.ElementRecordKind    
		    | Basetypes.TextRecordKind
		    | Basetypes.PIRecordKind -> 
			begin
			  match parent with
			    | Some parent ->
				begin
				  (* Format.printf "\t [Shredded_Store.Children] Replacing: %d (%a,%d) -> %d (%a,%d)@."
				    parent Shredded_renumber.Cell_As_Int64.print_int64_pair prev_preorder nodeid
				    parent Shredded_renumber.Cell_As_Int64.print_int64_pair new_preorder nodeid;   *)
				  Nodeid_Btree.btree_delete children_btree_h parent (prev_preorder,nodeid);				   
				  (* Store the new value *)
				  
				  Nodeid_Btree.btree_put children_btree_h parent (new_preorder, nodeid)
				end
			    | None -> ()
			end
		end
	  | None -> raise (Query (Shredded_Error ("Could not find stored_id " ^ (string_of_int nodeid))))
	      
    end

    module Renumber_Policy         = Shredded_renumber.Renumber_Module 
      (Basetypes)         (Recno_Functor)
      (Renumber_Callback) (Btree_Functor)               

    (************************)								      
    (* Declare public types *)
    (************************)
    type stored_nodeid   = Nodeid.nodeid

    let invalid_nodeid                 = (Basetypes.implemid, (IntId (-1, -1)))
    let qname_of_string s              = s

    let implemid  = Basetypes.implemid		      
    let get_name_of_docid store = raise (Query (Shredded_Error "Name of Store not implemented"))


    type shredded_store =
	{ 
	  docid                        : Nodeid.docid;


	  (* Generators *)
	  mutable load_nodeid_gen      : Basetypes.Stored_Nodeid_Generator.handle;
	  mutable textid_gen		: Basetypes.Textid_Generator.handle;
	  mutable namespaceid_gen	: Basetypes.Namespaceid_Generator.handle;
	  mutable commentid_gen        : Basetypes.Commentid_Generator.handle;
	  mutable processingid_gen     : Basetypes.Processingid_Generator.handle;

	  (* Keep tracks of all nsenv already created*)
	  nsenv_hash_table 		: Basetypes.nsenv_hash_table_type;

	  (* Indexes *)
	  main_recno_data_db		: Recno.recno_handle;	(* Main DB, stores record, one entry per node *)

	  (* These are preorder -> preorder btrees *)
	  (* Should we switch these to secondary indexes? *)
	  (* First child is leaving *)
	  main_first_child_index_db 	: Nodeid_Btree.btree_handle; (* Stores the preorder of the first child node *)

	  (* Should this be next sibiling or just a bound? *)
	  main_next_sibling_index_db   : Nodeid_Btree.btree_handle; (* Stores the preorder of next sibling *)
	  main_children_index_db       : Nodeid_Btree.btree_handle; (* Store all the children *)
	  main_attr_index_db		: Nodeid_Btree.btree_handle; (* Store the list of preorder of attributes nodes *)

	  (* Stores the text for the text nodes and values for attribute nodes *)
	  main_text_db			: Textid_Btree.btree_handle; 

	  (* Stores the namespace environment defined + inherited by each node *)
	  main_namespace_db		  : Namespaceid_Binding_Hash.hash;   
	  main_comment_db                : Comment_Hash.hash;
	  main_processing_instruction_db : Processingid_Hash.hash;

	  typed_element_value_hash       : Typed_Element_Value_Hash.hash;
	  typed_attribute_value_hash     : Typed_Attribute_Value_Hash.hash;

	  (* This could be string -> int, but it only has two elements.. *)
	  main_meta_data_db		: Metadata_Hash.hash;	(* Store the meta data corresponding to this store *)

	  (* The two mappings *)
	  element_qname_mapping        : Element_Mapping.mapping;
	  attribute_qname_mapping      : Attribute_Mapping.mapping;
	  type_qname_mapping           : Type_Mapping.mapping;

	  (* Reordering Structures *)  
	  renumber_policy               : Renumber_Policy.handle;

	  buff_size			: int;	     	   

	  (* ID of the last node *) 
	  mutable max_preorder		: Nodeid.large_preorder;
	  mutable max_nodeid                   : Basetypes.stored_nodeid;
	}


    
    (* Registration *)
    type store_meta_table_type = (docid,shredded_store) Hashtbl.t
    let (store_meta_table : store_meta_table_type) = Hashtbl.create 167
    let docid_gen        = Id.create 1
    let get_new_docid () = Id.next docid_gen
			     
    let add_store docid store = Hashtbl.add store_meta_table docid store				      
    let get_back_store docid  = Hashtbl.find store_meta_table docid

    let namespaceid_seed = Basetypes.Namespaceid_Generator.seed_value
			     

			     
    let store_meta_data store =
      let buff_size     = string_of_int store.buff_size in 
      let high,low      = Shredded_renumber.Cell_As_Int64.split_into_strings store.max_preorder in
      let max_nodeid    = string_of_int store.max_nodeid in 
	Metadata_Hash.hash_put store.main_meta_data_db buff_size_metadata_key buff_size;
	Metadata_Hash.hash_put store.main_meta_data_db hi_max_pre_order_metadata_key high;
	Metadata_Hash.hash_put store.main_meta_data_db lo_max_pre_order_metadata_key low;
	Metadata_Hash.hash_put store.main_meta_data_db last_nodeid_metadata_key max_nodeid

    let create_shredded_store nodeid_context directory name buffsize =      
      (* Register the document *)
      let docid   = get_new_docid () in 

      (* Metadata *)
      let metadata_db_name = Shredded_file_util.create_metadata_db_name directory name in 
      let metadata_db = Metadata_Hash.hash_open metadata_db_name buffsize false in	 

      (* Open Main Record *)
      let main_db_name = Shredded_file_util.create_main_db_name directory name in 
      let main_recno = Recno.recno_open main_db_name buffsize in

      (* Open Child and Sibling (int -> int) indexes*)
      let first_child_index_db_name = Shredded_file_util.create_first_child_index_db_name directory name in 
      let first_child_index_db = Nodeid_Btree.btree_open first_child_index_db_name buffsize false in

      let next_sibling_index_db_name = Shredded_file_util.create_next_sibling_index_db_name directory name in 
      let next_sibling_index_db = Nodeid_Btree.btree_open next_sibling_index_db_name buffsize false in

      (* New children index *)
      let children_index_db_name = Shredded_file_util.create_children_index_db_name directory name in 
      let children_index_db      = Nodeid_Btree.btree_open children_index_db_name buffsize true in 

      (* Open attr and text (int -> str) btrees *)
      let attr_index_db_name = Shredded_file_util.create_attr_index_db_name directory name in 
      let attr_index_db      = Nodeid_Btree.btree_open attr_index_db_name buffsize true in

      let text_db_name = Shredded_file_util.create_text_db_name directory name in 
      let textdb       = Textid_Btree.btree_open text_db_name buffsize false in

      let ns_db_name = Shredded_file_util.create_namespace_db_name directory name in 
      let ns_hash = Namespaceid_Binding_Hash.hash_open ns_db_name buffsize false in

      let commentdb = Comment_Hash.hash_open 
			(Shredded_file_util.create_comment_db_name directory name) buffsize false in

      let piddb = Processingid_Hash.hash_open 
		    (Shredded_file_util.create_processingid_db_name directory name) buffsize false in      

      let typed_element_value_hash = Typed_Element_Value_Hash.hash_open 
				       (Shredded_file_util.create_typed_attribute_value_db_name directory name) buffsize false in

      let typed_attribute_value_hash = Typed_Attribute_Value_Hash.hash_open 
					 (Shredded_file_util.create_typed_element_value_db_name directory name) buffsize false in

      let element_qname_map   = Element_Mapping.mapping_create 
				  (directory,name) "element" buffsize in 
      let attribute_qname_map = Attribute_Mapping.mapping_create
				  (directory,name) "attribute" buffsize in 
      let type_qname_map      = Type_Mapping.mapping_create
				  (directory,name) "type" buffsize in 

	
      let renumber_handle = Renumber_Policy.renumber_open (directory,name) buffsize (main_recno,children_index_db) in 

      (* Create In memory hash tables *)
      let nsenv_table = Hashtbl.create 200 in 


      let new_store =
	{ docid = docid;

	  load_nodeid_gen  = 
	    Basetypes.Stored_Nodeid_Generator.build_gen Basetypes.Stored_Nodeid_Generator.seed_value;
	  namespaceid_gen  = 
	    Basetypes.Namespaceid_Generator.build_gen Basetypes.Namespaceid_Generator.seed_value;	 
	  textid_gen       = 
	    Basetypes.Textid_Generator.build_gen Basetypes.Textid_Generator.seed_value;
	  commentid_gen    = 
	    Basetypes.Commentid_Generator.build_gen Basetypes.Commentid_Generator.seed_value;	    
	  processingid_gen = 
	    Basetypes.Processingid_Generator.build_gen Basetypes.Processingid_Generator.seed_value;

	  nsenv_hash_table = nsenv_table;
	  main_recno_data_db = main_recno;
	  main_first_child_index_db = first_child_index_db;
	  main_next_sibling_index_db = next_sibling_index_db;
	  main_children_index_db = children_index_db;
	  main_attr_index_db = attr_index_db;

	  main_text_db = textdb;
	  main_namespace_db = ns_hash;
	  main_meta_data_db = metadata_db;
	  element_qname_mapping   = element_qname_map;
	  attribute_qname_mapping = attribute_qname_map;
	  type_qname_mapping      = type_qname_map;
	  typed_element_value_hash = typed_element_value_hash;	   
	  typed_attribute_value_hash = typed_attribute_value_hash;	   

	  buff_size = buffsize;
	  
	  main_comment_db = commentdb;

	  main_processing_instruction_db = piddb;

	  renumber_policy = renumber_handle;
	  max_preorder = Shredded_renumber.Cell_As_Int64.zero;
	  max_nodeid    = 0;
	  
	}
      in
	begin
	  add_store docid new_store;
	  store_meta_data new_store;
	  new_store
	end

    let sync_store store =            
      Metadata_Hash.hash_put store.main_meta_data_db last_nodeid_metadata_key (string_of_int store.max_nodeid);
      (* The rest really are syncs *)
      Recno.recno_sync store.main_recno_data_db;
      Attribute_Mapping.mapping_sync store.attribute_qname_mapping;
      Element_Mapping.mapping_sync   store.element_qname_mapping;
      Type_Mapping.mapping_sync store.type_qname_mapping;
      Nodeid_Btree.btree_sync store.main_first_child_index_db;
      Nodeid_Btree.btree_sync store.main_next_sibling_index_db;
      Nodeid_Btree.btree_sync store.main_attr_index_db;
      Nodeid_Btree.btree_sync store.main_children_index_db;
      Textid_Btree.btree_sync store.main_text_db;
      Namespaceid_Binding_Hash.hash_sync store.main_namespace_db;
      Comment_Hash.hash_sync store.main_comment_db;
      Processingid_Hash.hash_sync store.main_processing_instruction_db;
      Typed_Element_Value_Hash.hash_sync store.typed_element_value_hash;
      Typed_Attribute_Value_Hash.hash_sync store.typed_attribute_value_hash;
      Renumber_Policy.renumber_sync store.renumber_policy;
      Metadata_Hash.hash_sync store.main_meta_data_db

    (* you should not close multiple times *)
    let close_store store =
      (* Format.printf "Closing Store@."; *)
      Metadata_Hash.hash_put store.main_meta_data_db last_nodeid_metadata_key 
	(string_of_int store.max_nodeid);
      (* All are closes *)
      Recno.recno_close store.main_recno_data_db;
      Attribute_Mapping.mapping_close store.attribute_qname_mapping;
      Element_Mapping.mapping_close store.element_qname_mapping;
      Type_Mapping.mapping_close store.type_qname_mapping;
      Nodeid_Btree.btree_close store.main_first_child_index_db;
      Nodeid_Btree.btree_close store.main_next_sibling_index_db;
      Nodeid_Btree.btree_close store.main_attr_index_db;
      Nodeid_Btree.btree_close store.main_children_index_db;
      Textid_Btree.btree_close store.main_text_db;
      Namespaceid_Binding_Hash.hash_close store.main_namespace_db;
      Comment_Hash.hash_close store.main_comment_db;
      Processingid_Hash.hash_close store.main_processing_instruction_db;
      Metadata_Hash.hash_close store.main_meta_data_db;
      Typed_Element_Value_Hash.hash_close store.typed_element_value_hash;
      Typed_Attribute_Value_Hash.hash_close store.typed_attribute_value_hash;
      Renumber_Policy.renumber_close store.renumber_policy



    let get_main_db store = store.main_recno_data_db 
			      
    let get_first_child_index_db store = store.main_first_child_index_db
					   
    let get_next_sibling_index_db store = store.main_next_sibling_index_db

    let get_children_index_db store = store.main_children_index_db

    let get_attr_index_db store = store.main_attr_index_db


    let get_text_db store = store.main_text_db
			      
    let get_comment_db store = store.main_comment_db
				 
    let get_processinginstruction_db store = store.main_processing_instruction_db
					       
    let get_docid store = store.docid
			    
    let get_nodeid_gen store = store.load_nodeid_gen

    let get_max_preorder store = store.max_preorder 

    let get_element_qname_map_db store   = store.element_qname_mapping
    let get_attribute_qname_map_db store = store.attribute_qname_mapping
    let get_type_qname_map_db store = store.type_qname_mapping
    let get_typed_element_value_hash_db store = store.typed_element_value_hash
    let get_typed_attribute_value_hash_db store = store.typed_attribute_value_hash
						    
    let compute_max_preorder main_db = 
      let main_db_cursor = Recno.recno_cursor_open main_db in
	match Recno.recno_cursor_get_last main_db_cursor with
	  | None -> Shredded_renumber.Cell_As_Int64.zero
	  | Some last_record ->
	      Record.get_preorder last_record 


    let uri_of_str uri_str = Namespace_names.NSUri (uri_str)

    (************)
    (* Get Maxs *)
    (************)
    let get_max_textid textdb = 
      let textdb_cursor = Textid_Btree.btree_cursor_open textdb in 
      let ret = 
	match Textid_Btree.btree_cursor_get_last textdb_cursor with
	  | Some (id, _) -> id
	  | None -> -1 
      in
      let () = Textid_Btree.btree_cursor_close textdb_cursor in 
	ret


    let get_max_namespaceid namespacedb = 
      let namespacedb_cursor = Namespaceid_Binding_Hash.hash_cursor_open namespacedb in	
      let ret =
	match Namespaceid_Binding_Hash.hash_cursor_get_last namespacedb_cursor with
	  | Some (id, _) -> id
	  | None -> -1 
      in
      let () = Namespaceid_Binding_Hash.hash_cursor_close namespacedb_cursor in
	ret


    let get_max_commentid comment_db = 
      let comment_cursor = Comment_Hash.hash_cursor_open comment_db in 
      let ret = 
	match Comment_Hash.hash_cursor_get_last comment_cursor with
	  | Some (id,_) -> id
	  | None -> -1 
      in
      let ()  = Comment_Hash.hash_cursor_close comment_cursor in
	ret 

    let get_max_processingid procid_db = 
      let cursor = Processingid_Hash.hash_cursor_open procid_db in 
      let ret = 
	match Processingid_Hash.hash_cursor_get_last cursor with
	  | Some (id,_) -> id
	  | None -> -1 
      in
      let ()  = Processingid_Hash.hash_cursor_close cursor in
	ret 


    let new_textid store = 
      Basetypes.Textid_Generator.new_value store.textid_gen


    let new_commentid store = 
      Basetypes.Commentid_Generator.new_value store.commentid_gen	

    let new_processingid store =
      Basetypes.Processingid_Generator.new_value store.processingid_gen

    let new_namespaceid store =
      Basetypes.Namespaceid_Generator.new_value store.namespaceid_gen	

    (* This depends on loading, essentially we load the nodes, setting
       up parent-child relationships, adding in new qnames,
       storing type information. Then we call the renumber
       module, this takes care of actually setting the
       preorders.  *)
    let dump_children store =
      let print_pair (parent,(p,child)) =
	Format.printf "\t[Debug Shredded Children] (%d,(%a,%d))@."  
	  parent 
	  Shredded_renumber.Cell_As_Int64.print_int64_pair p child
      in
      let cursor = Nodeid_Btree.btree_cursor_open (get_children_index_db store) in 
      let first  = Nodeid_Btree.btree_cursor_get_first cursor in 
	match first with
	  | None -> Format.printf "No children!@.";
	  | Some v ->
	      print_pair v;
	      Cursor.cursor_iter print_pair (Nodeid_Btree.btree_cursor_to_cursor cursor Nodeid_Btree.Next)
		
    (***********************)
    (* Open Shredded Store *)
    (***********************)
    let load_shredded_store_from_ordered_typed_stream_ref = ref 
      (fun a b c d e -> raise (Query (Shredded_Error ("Ordered type stream load reference not updated, should never happen"))))
    let load_shredded_store_from_resolved_stream_ref =  ref
      (fun a b c d e -> raise (Query (Shredded_Error ("Ordered type stream load reference not updated, should never happen"))))

    let open_store directory name = 
      let default_buffsize = 1 lsl 20 in 
	(* if the document is not persistent then load it *)
	if not (Basetypes.is_persistent ()) then
	  begin
	    let input_file      = Filename.concat directory name  in
	    let (_, xml_stream) = Streaming_parse.open_xml_stream_from_io (Galax_io.File_Input input_file) in 
	    let nodeid_context  = Nodeid_context.default_nodeid_context() in
	    let resolved_xml_stream = Streaming_ops.resolve_xml_stream xml_stream in     	  
	      (* Should this change ? *)
	    let typed_load = ref true in 
	      if !typed_load then
		begin
		  let typed_stream = Streaming_ops.typed_of_resolved_xml_stream resolved_xml_stream in 
		  let docid_gen    = (* Shouldn't this be in the store? *)Nodeid.build_docid_gen () in 
		  let ot_stream    = Streaming_ops.ordered_typed_of_typed_stream docid_gen nodeid_context typed_stream in 
		    !load_shredded_store_from_ordered_typed_stream_ref nodeid_context ot_stream directory name default_buffsize
		end
	      else
		begin
		  !load_shredded_store_from_resolved_stream_ref 
		    nodeid_context resolved_xml_stream directory name default_buffsize
		end
	  end
	else begin
	  let metadata_db_name = Shredded_file_util.create_metadata_db_name directory name in 

	  let metadata_db  = Metadata_Hash.hash_open metadata_db_name default_buffsize false in
	  let buffsize_str = Metadata_Hash.hash_get metadata_db buff_size_metadata_key in  
	  let last_nodeid   = 
	    match Metadata_Hash.hash_get metadata_db last_nodeid_metadata_key with
	      | None -> raise (Query (Shredded_Error "No Last docid in store, appears to be corrupted"))
	      | Some d -> int_of_string d
	  in

	  let buffsize = 
	    match !(Conf.jungle_buffsize),buffsize_str with
	      | Some b, _ -> b
	      | None, Some str -> 
		  int_of_string str
	      | None, None ->
		  raise (Query (Shredded_Error ("No Buffersize information Set")))

	  in 
	  let main_db_name = Shredded_file_util.create_main_db_name directory name in 

	  let main_recno = Recno.recno_open main_db_name buffsize in

	  let first_child_index_db_name = Shredded_file_util.create_first_child_index_db_name directory name in 
	  let first_child_index_db = Nodeid_Btree.btree_open first_child_index_db_name buffsize false in

	  let next_sibling_index_db_name = Shredded_file_util.create_next_sibling_index_db_name directory name in 
	  let next_sibling_index_db = Nodeid_Btree.btree_open next_sibling_index_db_name buffsize false in

	  let children_index_db_name = Shredded_file_util.create_children_index_db_name directory name in 
	  let children_index_db      = Nodeid_Btree.btree_open children_index_db_name buffsize true in 
	    
	  let attr_index_db_name = Shredded_file_util.create_attr_index_db_name directory name in 
	  let attr_index_db = Nodeid_Btree.btree_open attr_index_db_name buffsize true in

	  let text_db_name = Shredded_file_util.create_text_db_name directory name in 
	  let textdb = Textid_Btree.btree_open text_db_name buffsize false in

	  let comment_db_name = Shredded_file_util.create_comment_db_name directory name in 
	  let comment_db = Comment_Hash.hash_open comment_db_name buffsize false in

	    
	  let typed_element_value_hash = Typed_Element_Value_Hash.hash_open 
					   (Shredded_file_util.create_typed_attribute_value_db_name directory name) buffsize false in

	  let typed_attribute_value_hash = Typed_Attribute_Value_Hash.hash_open 
					     (Shredded_file_util.create_typed_element_value_db_name directory name) buffsize false in

	  let pid_name = Shredded_file_util.create_processingid_db_name directory name in 
	  let pid_db = Processingid_Hash.hash_open pid_name buffsize false in

	  let ns_db_name = Shredded_file_util.create_namespace_db_name directory name in 
	  let ns_hash = Namespaceid_Binding_Hash.hash_open ns_db_name buffsize false in
	    
	  let element_qname_map   = Element_Mapping.mapping_open 
				      (directory,name) "element" buffsize in 
	  let attribute_qname_map = Attribute_Mapping.mapping_open
				      (directory,name) "attribute" buffsize in 
	  let type_qname_map = Type_Mapping.mapping_open
				 (directory,name) "type" buffsize in 
	    
	  let renumber_handle = Renumber_Policy.renumber_open (directory,name) buffsize (main_recno,children_index_db) in 

	    
	  let nsenv_table = Hashtbl.create 200 in 

	  let docid   = get_new_docid () in 
	    
	  let new_store =
	    { docid = docid;
	      load_nodeid_gen = 
		Basetypes.Stored_Nodeid_Generator.build_gen Basetypes.Stored_Nodeid_Generator.seed_value;
	      namespaceid_gen      = 
		Basetypes.Namespaceid_Generator.build_gen Basetypes.Namespaceid_Generator.seed_value;	 
	      textid_gen           = 
		Basetypes.Textid_Generator.build_gen Basetypes.Textid_Generator.seed_value;
	      commentid_gen = 
		Basetypes.Commentid_Generator.build_gen Basetypes.Commentid_Generator.seed_value;	    
	      processingid_gen = 
		Basetypes.Processingid_Generator.build_gen Basetypes.Processingid_Generator.seed_value;

	      nsenv_hash_table = nsenv_table;
	      main_recno_data_db = main_recno;
	      main_first_child_index_db = first_child_index_db;
	      main_next_sibling_index_db = next_sibling_index_db;
	      main_children_index_db = children_index_db;
	      main_attr_index_db = attr_index_db;
	      main_text_db = textdb;
	      main_comment_db = comment_db;
	      main_processing_instruction_db = pid_db;
	      main_namespace_db = ns_hash;
	      main_meta_data_db = metadata_db;
	      element_qname_mapping   = element_qname_map;
	      attribute_qname_mapping = attribute_qname_map;
	      type_qname_mapping      = type_qname_map;
	      typed_element_value_hash = typed_element_value_hash;
	      typed_attribute_value_hash = typed_attribute_value_hash;

	      buff_size = buffsize;

	      renumber_policy = renumber_handle;

	      max_preorder  = compute_max_preorder main_recno;
	      max_nodeid    = last_nodeid;
	    }
	  in
	    begin	  
	      add_store docid new_store;

	      new_store.load_nodeid_gen <-
	      Basetypes.Stored_Nodeid_Generator.build_gen (last_nodeid + 1);

	      new_store.textid_gen <- 
	      Basetypes.Textid_Generator.build_gen ((get_max_textid textdb)+1);

	      new_store.namespaceid_gen <- 
	      Basetypes.Namespaceid_Generator.build_gen ((get_max_namespaceid ns_hash)+1);	 

	      new_store.commentid_gen   <- 
	      Basetypes.Commentid_Generator.build_gen ((get_max_commentid comment_db)+1);	 

	      new_store.processingid_gen <- 
	      Basetypes.Processingid_Generator.build_gen ((get_max_processingid pid_db)+1);
	      
	      new_store
	    end
	end

    let set_max_preorder store max_pre = 
      store.max_preorder <- max_pre

    let set_nodeid_gen store new_nodeid_gen =
      store.load_nodeid_gen <- new_nodeid_gen

    (**********************************************)
    (* Internal Type Constructors                 *)
    (* This section of code handles the creation  *)
    (* of internal types (ex: textids)            *)
    (**********************************************)
    let new_snodeid store =
      let last_id = Basetypes.Stored_Nodeid_Generator.new_value store.load_nodeid_gen  in
	store.max_nodeid <- last_id;
	last_id

    let get_textid store text = 
      let textdb = get_text_db store in 
      let new_id = new_textid store in 
	(* Format.printf "Storing newid: %d with text %s@." 
	   new_id (Basetypes.string_of_text text); *)
	Textid_Btree.btree_put textdb new_id text; 
	new_id

    let get_commentid store comment = 
      let commentdb = get_comment_db store in 
      let new_id    = new_commentid store  in 
	Comment_Hash.hash_put commentdb new_id comment;
	new_id

    let get_processing_id store pi  =
      let pid_db = get_processinginstruction_db store in 
      let new_id = new_processingid store in
	Processingid_Hash.hash_put pid_db new_id pi; 
	new_id
	  

    let get_text store textid =    
      let text_db = get_text_db store in 
	(* Format.printf "--> %d@." textid; *)
	match Textid_Btree.btree_get text_db textid with
	  | Some content_str -> content_str
	  | None -> 
	      begin 
		print_string ("Error in retreiving text for " ^ string_of_int(textid) ^ "\n");
		flush stdout; 
		raise (Query (Shredded_Error ("Error in retreiving text for " ^ string_of_int(textid) ^ "\n")))
	      end

    let get_comment store commentid = 
      let comment_db = get_comment_db store in 
	match Comment_Hash.hash_get comment_db commentid with
	  | None -> (* Error *)
	      raise (Query (Shredded_Error ("Error in retreiving comment for " ^ string_of_int(commentid) ^ "\n")))
	  | Some content -> content

    let get_processing_instruction store pid = 
      let pid_db = get_processinginstruction_db store in 
	match Processingid_Hash.hash_get pid_db pid with
	  | None -> (* Error *)
	      raise (Query (Shredded_Error ("Error in retreiving processing instruction for " ^ string_of_int(pid) ^ "\n")))
	  | Some content -> content
	      
    (**********************************************************)
    (* Conversion routines for simple internal/external types *)
    (**********************************************************)
    let stored_nodeid_of_nodeid did (_,pn) = 
      match pn with
	| IntId (did_i,sid) -> 
	    if did_i != did 
	    then raise (Query (Shredded_Error "Using incorrect docid"))
	    else sid
	| _ -> raise (Query (Shredded_Error "Using incorrect nodeid"))


    let nodeid_of_stored_nodeid did pn = (implemid, (IntId (did, pn)))

    let nodeid_opt_of_stored_nodeid_opt did opn = 
      match opn with
	| None    -> None
	| Some pn ->
	    Some (nodeid_of_stored_nodeid did pn)

    let get_store_from_docid did = get_back_store did
    let get_store_of_nodeid (_,nid)  = 				 
      let docid = 
	match nid with 
	  | IntId (docid,_) -> docid
	  | IntPairId (docid,_) -> raise (Query (Shredded_Error "This nodeid did not come from a shredded store"))
	      
      in

	get_store_from_docid docid 
	  
    let docid_of_nodeid nid =
      let store    = get_store_of_nodeid nid in 
	get_docid store 

    let preorder_of_snodeid store snid = 
      let recorddb = get_main_db store   in 
      let record   = Recno.recno_get_unsafe recorddb snid in  
	Record.get_preorder record

    let preorder_of_nodeid nid = 
      let store    = get_store_of_nodeid nid in 
      let docid    = docid_of_nodeid nid in 
      let snid     = stored_nodeid_of_nodeid docid nid in 
	preorder_of_snodeid store snid

    let docid_preorder_of_nodeid node_id = 
      (docid_of_nodeid node_id), 
      (preorder_of_nodeid node_id)


    (***************************************)
    (* Convention: Root is always at one.. *)
    (***************************************)
    let get_root store = 
      (* This is awful, what if there isn't a root? *)
      let docid          = get_docid store in 
	nodeid_of_stored_nodeid docid 1



    let insert_children_index store parent node = 
      let child_index_db = get_children_index_db store in 
      let preorder = preorder_of_snodeid store node in 
	Nodeid_Btree.btree_put child_index_db parent (preorder,node)
	  
    let insert_next_sibling_index store current next = 
      let next_sibling_index_db = get_next_sibling_index_db store in 
      let preorder = preorder_of_snodeid store next in 
	Nodeid_Btree.btree_put next_sibling_index_db current (preorder,next)

    let replace_next_sibling_index store current next = 
      let next_sibling_index_db = get_next_sibling_index_db store in 
	(* This isn't necessary if we have trees which properly
	   support the non-duplicate semantic *)
      let () =
	match Nodeid_Btree.btree_get next_sibling_index_db current with
	  | None -> ()
	  | Some v ->
	      Nodeid_Btree.btree_delete next_sibling_index_db current v 
      in
	insert_next_sibling_index store current next

    let insert_first_child_index store current first_child = 
      let first_child_index_db  = get_first_child_index_db store in
      let preorder = preorder_of_snodeid store first_child in 
	Nodeid_Btree.btree_put first_child_index_db current (preorder,first_child)

    (* Here we change so that it inserts 'one at a time' *)
    let insert_attr_index store current attr_list = 
      let attr_index_db = get_attr_index_db store in 
	Cursor.cursor_iter (Nodeid_Btree.btree_put attr_index_db current) attr_list 

	  
    (*******************)
    (* Namespace stuff *)
    (*******************)
    let lookup_nsenv_from_namespaceid store nsid = 
      Hashtbl.find store.nsenv_hash_table nsid 

    let rec get_nsenv_from_namespaceid store nsid = 
      try 
	lookup_nsenv_from_namespaceid store nsid  
      with
	| Not_found ->
	    (* This is the initial namespaceid... 
	       There should be a better way to do this *)
	    if (nsid == 1) 
	    then
	      begin
		let default_namespace = Namespace_context.default_xml_nsenv in 
		  Hashtbl.add store.nsenv_hash_table nsid default_namespace;
		  default_namespace
	      end
	    else 
	      begin
		(* old_nsid -> prev_namespace | new_bindings *)
		match 
		  Namespaceid_Binding_Hash.hash_get 
		    store.main_namespace_db nsid 
		with
		  | None -> 
		      raise (Query (Shredded_Error ("Error retrieving old namespace bindings!")))
		  | Some (old_nsid, new_bindings) ->
		      let prev_namespace = get_nsenv_from_namespaceid store old_nsid in 
		      let real_nsenv = Namespace_context.add_all_ns prev_namespace new_bindings in 
			Hashtbl.add store.nsenv_hash_table nsid real_nsenv;
			real_nsenv		      

	      end


    (******************)
    (* Typed Values   *)
    (******************)
    let erase_av_list =
      List.map (fun av -> ((av#getAtomicValueKind ()), (av#erase_atomic_value ()))) 

    let store_typed_element store nsid nodeid type_sig_opt = 
      match type_sig_opt with
	| None -> () (* Do nothing *)
	| Some (type_name, nilled, av_list) -> 
	    let typed_value_hash = get_typed_element_value_hash_db store in 
	    let av_list = erase_av_list av_list in 
	      Typed_Element_Value_Hash.hash_put typed_value_hash nodeid (nilled,(nsid,av_list))

    let store_typed_attribute store nodeid nsid type_opt = 
      match type_opt with
	| None -> ()
	| Some (av_list, type_name) -> 
	    let typed_value_hash = get_typed_attribute_value_hash_db store in 
	    let av_list = erase_av_list av_list in 
	      Typed_Attribute_Value_Hash.hash_put typed_value_hash nodeid (nsid,av_list)

    (***************************************************************)
    (* Here we have internal and external versions                 *)
    (* The distinction is that the input,output types of internal  *)
    (*   versions are "internal" types like stored nodeids         *)
    (*   or eqname symbols, while the external deal in the         *)
    (*   externally defined equivalents (Nodeid.nodeid or          *)
    (*   symbols). Additionally, external versions are public      *)
    (*   while internal are all 'private', though some are shared  *)
    (*   to the XPath modules.                                     *)
    (***************************************************************)

    (*********************)
    (* Internal versions *)
    (*********************)
		
    let retrieve_typed_element_snodeid store nodeid =
      let typed_value_hash = get_typed_element_value_hash_db store in 
	match Typed_Element_Value_Hash.hash_get typed_value_hash nodeid with
	  | None -> None
	  | Some (nilled,(nsid,av_list)) ->
	      let nsenv = get_nsenv_from_namespaceid store nsid in 
	      let cast_fn (at,untyped) =
		let bt = Datatypes_util.symbol_of_primitive_type at in
		(new Dm_atomic.atomicUntyped untyped)#cast_to nsenv bt at in
	      Some (nilled, List.map cast_fn av_list)

		  
    let retrieve_typed_attribute_snodeid store nodeid =
      let typed_value_hash = get_typed_attribute_value_hash_db store in 
	match Typed_Attribute_Value_Hash.hash_get typed_value_hash nodeid with
	  | None -> None
	  | Some (nsid,av_list) ->
	      let nsenv = get_nsenv_from_namespaceid store nsid in 
	      let cast_fn (at,untyped) =
		let bt = Datatypes_util.symbol_of_primitive_type at in
		(new Dm_atomic.atomicUntyped untyped)#cast_to nsenv bt at in
		Some (List.map cast_fn av_list)


    let get_parent_snodeid store snid = 
      let recorddb = get_main_db store in 
      let record   = Recno.recno_get_unsafe recorddb snid in 
	Record.get_parent record 

    (* Eqnames are used frequently and to avoid
       unnecessary calls to BDB, we have many variations 
       to get portions of the qnames *)
    (* Elemement names *)
    let get_elem_eqnameid_snodeid store snid = 
      let recorddb = get_main_db store in  
      let record   = Recno.recno_get_unsafe recorddb snid in 
	Record.get_name_eqnameid record 

    let get_elem_prefixid_snodeid store snid = 
      let recorddb = get_main_db store in  
      let record   = Recno.recno_get_unsafe recorddb snid in 
	Record.get_name_prefixid record 

    let get_elem_prefixid_eqnameid_snodeid store snid = 
      let recorddb = get_main_db store in  
      let record   = Recno.recno_get_unsafe recorddb snid in 
	(Record.get_name_prefixid record, Record.get_name_eqnameid record)

    (* Attribute Names *)
    let get_attr_eqnameid_snodeid store snid = 
      let recorddb = get_main_db store in  
      let record   = Recno.recno_get_unsafe recorddb snid in 
	Record.get_name_eqnameid record 

    let get_attr_prefixid_snodeid store snid = 
      let recorddb = get_main_db store in  
      let record   = Recno.recno_get_unsafe recorddb snid in 
	Record.get_name_prefixid record 

    let get_attr_prefixid_eqnameid_snodeid store snid = 
      let recorddb = get_main_db store in  
      let record   = Recno.recno_get_unsafe recorddb snid in 
	(Record.get_name_prefixid record, Record.get_name_eqnameid record)

    (* Types Names *)
    let get_type_eqnameid_snodeid store snid = 
      let recorddb = get_main_db store in  
      let record   = Recno.recno_get_unsafe recorddb snid in 
	Record.get_type_eqnameid record 

    let get_type_prefixid_snodeid store snid = 
      let recorddb = get_main_db store in  
      let record   = Recno.recno_get_unsafe recorddb snid in 
	Record.get_type_prefixid record 

    let get_type_prefixid_eqnameid_snodeid store snid = 
      let recorddb = get_main_db store in  
      let record   = Recno.recno_get_unsafe recorddb snid in 
	match (Record.get_type_prefixid record, Record.get_type_eqnameid record) with
	  | (None, None) -> 
	      None
	  | (Some p, Some e) ->
	      Some (p,e)

	  | (Some p, None) -> (* THIS CASE DOESN'T MAKE SENSE *)
	      raise (Query (Shredded_Error ("[Prefix] and Eqname should both be defined!")))
	  | (None, Some e) -> (* THIS CASE DOESN'T MAKE SENSE *)
	      raise (Query (Shredded_Error ("Prefix and [Eqname] should both be defined! " ^ (string_of_int e))))


    let set_name_snodeid store snid symbol = 
      let recorddb = get_main_db store in  
      let record   = Recno.recno_get_unsafe recorddb snid in 
      let record   = Record.set_name_symbol record symbol in 
	Recno.recno_put recorddb snid record

    let set_elem_name_snodeid store snid (prefixid, eqnameid) = 
      set_name_snodeid store snid  (prefixid, eqnameid)

    let set_attr_name_snodeid store snid (prefixid, eqnameid) = 
      set_name_snodeid store snid  (prefixid, eqnameid)

    let set_type_name_snodeid store snid opt_symbol = 
      let recorddb = get_main_db store in  
      let record   = Recno.recno_get_unsafe recorddb snid in 
      let record   = Record.set_type_symbol record opt_symbol in 
	Recno.recno_put recorddb snid record

    (* End Eqnames *)

    (* Content of an attribute *)
    let get_attribute_valueid_snodeid store snid = 
      let recorddb   = get_main_db store in  
      let record     = Recno.recno_get_unsafe recorddb snid in 
	Record.get_attribute_value_id record 
	  
    let get_text_valueid_snodeid store snid = 
      let recorddb   = get_main_db store in  
      let record     = Recno.recno_get_unsafe recorddb snid in 
	Record.get_textid record

    (* This section has accessors for the btrees.  For ordering, the
       btrees store the preorders which is not always useful. For
       convenience, we provide implementations which do not use it.
       Those ..._with_preorder_snodeid return the preorder, else it is
       stripped *)    

    (* get first child *)
    let get_first_child_with_preorder_snodeid store snid = 
      let first_child_index_db = get_first_child_index_db store in 
	Nodeid_Btree.btree_get first_child_index_db snid 

    let get_first_child_snodeid store snid = 
      match get_first_child_with_preorder_snodeid store snid with
	| None       -> None
	| Some (p,n) -> Some n

    (* get next sibling *)
    let get_next_sibling_with_preorder_snodeid store snid = 
      let next_sibling_index_db = get_next_sibling_index_db store in 
	Nodeid_Btree.btree_get next_sibling_index_db snid

    let get_next_sibling_snodeid store snid = 
      match get_next_sibling_with_preorder_snodeid store snid with
	| None       -> None
	| Some (p,n) -> Some n
	    
    let get_children_with_preorder_snodied store snid = 
      let children_index = get_children_index_db store in
	Nodeid_Btree.btree_get_all children_index snid

    (* get children *)
    let get_children_snodeid store snid = 
      Cursor.cursor_map snd
	(get_children_with_preorder_snodied store snid)

	
    let get_attributes_with_preorder_snodeid store snid = 
      let attr_index_db = get_attr_index_db store in 	
	Nodeid_Btree.btree_get_all attr_index_db snid

    (* get attributes *)
    let get_attributes_snodeid store snid = 
      Cursor.cursor_map snd
	(get_attributes_with_preorder_snodeid store snid)


    (* content *)
    let get_content_snodeid store snid = 
      let recorddb   = get_main_db store in  
      let record    = Recno.recno_get_unsafe recorddb snid in 
      let node_kind = Record.get_kind record in 
      let text_value = 
	(* We should use the accessors here but don't because we've
	   already hit the record *)
	match node_kind with 
	  | Basetypes.AttributeRecordKind -> 	  
	      let textid = Record.get_attribute_value_id record in 
		get_text store textid

	  | Basetypes.TextRecordKind -> 	 
	      let textid = Record.get_textid record in 
		get_text store textid	  
	  | _ 		
	    -> raise (Query (Prototype ("Could not retrieve value of node other than attribute and text")))
      in
	Basetypes.string_of_text text_value


    let get_nodekind_snodeid store snid = 
      let recorddb = get_main_db store in  
      let record = Recno.recno_get_unsafe recorddb snid in 
	Record.get_kind record


    let get_nsenv_snodeid store snid = 
      let recorddb = get_main_db store in  
      let record = Recno.recno_get_unsafe recorddb snid in 
      let nsid = Record.get_namespaceid record in 
	get_nsenv_from_namespaceid store nsid 

    let get_descendant_snodeid store nto snid = raise (Query (Prototype ("Store access not implemented yet")))
    let get_doc_uri_snodeid store snid = raise (Query (Prototype ("Store access not implemented yet")))

    let get_pi_id_snodeid store snid     = 
      let recorddb = get_main_db store in  
      let record = Recno.recno_get_unsafe recorddb snid in 
	Record.get_processing_instruction_id record 

    let get_pi_target_snodeid store snid = 
      let pid            = get_pi_id_snodeid store snid in 
      let (target,value) = get_processing_instruction store pid in
	target

    let get_pi_value_snodeid store snid = 	     
      let pid            = get_pi_id_snodeid store snid in 
      let (target,value) = get_processing_instruction store pid in
	value

    let get_commentid_snodeid store snid = 
      let recorddb = get_main_db store in  
      let record = Recno.recno_get_unsafe recorddb snid in 
	Record.get_commentid record 

    let get_comment_value_snodeid store snid = 
      get_comment store (get_commentid_snodeid store snid)

	

    (********************************)
    (* Module for XPath Expressions *)
    (********************************)

    module Store_For_XPath = struct
      type handle        = shredded_store
      type stored_nodeid = Basetypes.stored_nodeid
      type record_kind   = Basetypes.record_kind

      type elem_eqnameid = Basetypes.eqnameid
      type attr_eqnameid = Basetypes.eqnameid
      type type_eqnameid = Basetypes.eqnameid


      let get_record_kind = get_nodekind_snodeid

      let encode_elem_eqnameid store sym = 
	let element_map        = get_element_qname_map_db store in 
	let eqnameid,prefixid  = Element_Mapping.encode_symbol element_map sym in 
	  eqnameid

      let get_elem_eqnameid = get_elem_eqnameid_snodeid 

       let get_elem_name  store snid = 
	 let prefixid, eqnameid = get_elem_prefixid_eqnameid_snodeid store snid in
	 let element_map        = get_element_qname_map_db store in       
	  Element_Mapping.decode_symbol element_map prefixid eqnameid 



      let encode_attr_eqnameid store sym = 
	let attribute_map        = get_attribute_qname_map_db store in 
	let eqnameid,prefixid    = Attribute_Mapping.encode_symbol attribute_map sym in 
	  eqnameid

      let get_attr_eqnameid = get_attr_eqnameid_snodeid 

      let get_attr_name store snid   = 
	(*    Format.printf "Get_attr_name %d@." snid; *)
	let prefixid, eqnameid   = get_attr_prefixid_eqnameid_snodeid store snid in
	let attribute_map        = get_attribute_qname_map_db store in 
	  Attribute_Mapping.decode_symbol attribute_map prefixid eqnameid 


      let encode_type_eqnameid store sym = 
	let type_map           = get_type_qname_map_db store in 
	let eqnameid,prefixid  = Type_Mapping.encode_symbol type_map sym in 
	  eqnameid

      let get_type_eqnameid = get_type_eqnameid_snodeid 
      

      let get_type store snid        = 
	(* Format.printf "Get_type_name %d@." snid; *)
	match get_type_prefixid_eqnameid_snodeid store snid with
	  | None -> None
	  | Some (prefixid, eqnameid) ->
	      let type_map           = get_type_qname_map_db store in 
		Some (Type_Mapping.decode_symbol type_map prefixid eqnameid)

      let get_elem_name_eqnameid_with_type_name store snid =
	let name               = get_elem_eqnameid_snodeid store snid in
	  match get_type_prefixid_eqnameid_snodeid store snid with
	    | None ->  name, None
	    | Some (prefixid,eqnameid) ->
		let type_map           = get_type_qname_map_db store in 
		let type_symbol        = Type_Mapping.decode_symbol type_map prefixid eqnameid in 
		  name, (Some type_symbol)

      let get_attr_name_eqnameid_with_type_name store snid =
	let name               = get_attr_eqnameid_snodeid store snid in
	  match get_type_prefixid_eqnameid_snodeid store snid with
	    | None ->  name, None
	    | Some (prefixid,eqnameid) ->
		let type_map           = get_type_qname_map_db store in 
		let type_symbol        = Type_Mapping.decode_symbol type_map prefixid eqnameid in 
		  name, (Some type_symbol)

      let get_single_element_node store snid = 
	Cursor.cursor_get_singleton
	  (get_children_snodeid store snid)
          
      let get_pi_target = get_pi_target_snodeid
    end

    module XPath_Step_Evaluation_Module = 
      Shredded_dm_step.Shredded_XPath_Step_Functor (Basetypes) (Store_For_XPath)

    let eval_node_test store a nto snid = 
      match nto with 
	| None -> true
	| Some (schema_opt, nt) ->
	    let nt = XPath_Step_Evaluation_Module.shred_node_test_of_anode_test store nt in
	      XPath_Step_Evaluation_Module.eval_node_test_gen store schema_opt a nt snid

    (*********************)
    (* External Versions *)
    (*********************)
    (* Qnames *)
    let get_elem_name nid = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in 
      let snid  = stored_nodeid_of_nodeid docid nid in 
      let prefixid, eqnameid = get_elem_prefixid_eqnameid_snodeid store snid in
      let element_map        = get_element_qname_map_db store in 
	Element_Mapping.decode_symbol element_map prefixid eqnameid 

    let set_elem_name nid name = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in 
      let snid  = stored_nodeid_of_nodeid docid nid in 
      let element_map        = get_element_qname_map_db store in 
      let prefixid, eqnameid = Element_Mapping.encode_symbol element_map name in 
	set_elem_name_snodeid store snid (prefixid, eqnameid)
	  
    let get_attr_name nid = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in 
      let snid  = stored_nodeid_of_nodeid docid nid in 
      let prefixid, eqnameid = get_attr_prefixid_eqnameid_snodeid store snid in
      let attribute_map        = get_attribute_qname_map_db store in 
	Attribute_Mapping.decode_symbol attribute_map prefixid eqnameid 

    let set_attr_name nid name = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in 
      let snid  = stored_nodeid_of_nodeid docid nid in 
      let attribute_map      = get_attribute_qname_map_db store in 
      let prefixid, eqnameid = Attribute_Mapping.encode_symbol attribute_map name in 
	set_attr_name_snodeid store snid (prefixid, eqnameid)


    let get_type_name nid = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in 
      let snid  = stored_nodeid_of_nodeid docid nid in 
	match get_type_prefixid_eqnameid_snodeid store snid with
	  | Some (prefixid, eqnameid) ->
	      let type_map        = get_type_qname_map_db store in 
		Some (Type_Mapping.decode_symbol type_map prefixid eqnameid)
	  | None -> None

    let set_type_name nid name = 
      let docid    = docid_of_nodeid nid in 
      let store    = get_store_from_docid docid in 
      let snid     = stored_nodeid_of_nodeid docid nid in 
      let type_map = get_type_qname_map_db store in 
	match name with
	  | None -> 
	      	set_type_name_snodeid store snid None
	  | Some name -> 
	      let prefixid, eqnameid = Type_Mapping.encode_symbol type_map name in 
		set_type_name_snodeid store snid (Some (prefixid, eqnameid))

    (*************************************)
    (* Typed element/attribute accessors *)
    (*************************************)
    let retrieve_typed_element nid = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in 
      let snid  = stored_nodeid_of_nodeid docid nid in 
	retrieve_typed_element_snodeid store snid 


    let retrieve_typed_attribute nid = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in 
      let snid  = stored_nodeid_of_nodeid docid nid in 
	retrieve_typed_attribute_snodeid store snid

    (**************)
    (* Navigation *)
    (**************)
    let get_parent nid nto = 
      let store    = get_store_of_nodeid nid in 
      let docid    = docid_of_nodeid nid in 
      let snid     = stored_nodeid_of_nodeid docid nid in 
      let parent   = get_parent_snodeid store snid in
	match parent with
	  | None -> None
	  | Some p ->
	      begin
		match nto with
		  | None -> nodeid_opt_of_stored_nodeid_opt docid parent
		  | Some _ -> 
		      if eval_node_test store Xquery_common_ast.Parent nto p then      
			nodeid_opt_of_stored_nodeid_opt docid parent
		      else None
	      end
		

    let get_children nid nto = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in
      let snid  = stored_nodeid_of_nodeid docid nid in 
      let filt  = Cursor.cursor_filter 
		    (eval_node_test store Xquery_common_ast.Child nto) 
		    (get_children_snodeid store snid)
      in
	Cursor.cursor_map
	  (nodeid_of_stored_nodeid docid) filt
	  
	  
    let get_attributes nid nto =
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in
      let snid  = stored_nodeid_of_nodeid docid nid in 
      let filt  = Cursor.cursor_filter 
		    (eval_node_test store Xquery_common_ast.Attribute nto)
		    (get_attributes_snodeid store snid)
      in	
	Cursor.cursor_map
	  (nodeid_of_stored_nodeid docid) filt
	  

    let get_descendant nid nto = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in
      let snid  = stored_nodeid_of_nodeid docid nid in 
	get_descendant_snodeid store nto snid

    (* going to be phased out *)
    let get_first_child nid = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in
      let snid  = stored_nodeid_of_nodeid docid nid in 
	nodeid_opt_of_stored_nodeid_opt docid
	  (get_first_child_snodeid store snid)
	  
    (* going to be phased out *)
    let get_next_sibling nid = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in
      let snid  = stored_nodeid_of_nodeid docid nid in 
	nodeid_opt_of_stored_nodeid_opt docid
	  (get_next_sibling_snodeid store snid)


	  
    let rec get_last_descendant_preorder store snid =
      let _ = get_next_sibling_index_db store in 
      let recorddb = get_main_db store in 
      let unwrap_last lastopt = 
	match lastopt with
	  | None -> raise (Query (Shredded_Error ("Last Preorder is Empty? Is database emtpy?")))
	  | Some v -> v
      in
	match get_next_sibling_snodeid store snid with
	  | None -> 
	      begin
		(* The last descendant of me is the same as my parent *)
		match Recno.recno_get recorddb snid with
		  | Some record ->
		      begin
			match Record.get_parent record with
			  | None -> 
			      (* We are at the root *)
			      unwrap_last (Renumber_Policy.last_entry store.renumber_policy)
			  | Some parent ->
			      get_last_descendant_preorder store parent
		      end
		  | None -> (* We are at the root *)
		      unwrap_last (Renumber_Policy.last_entry store.renumber_policy)  
	      end
	  | Some sib ->
	      (* Format.printf "Found sibling: %d@." sib;  *)
	      let record   = Recno.recno_get_unsafe recorddb sib in 
	      let preorder = Record.get_preorder record in	    
		unwrap_last (Renumber_Policy.previous_entry store.renumber_policy preorder)


    let rec get_last_descendant nid =
      let store    = get_store_of_nodeid nid in   
      let docid    = get_docid store in 
      let snid     = stored_nodeid_of_nodeid docid nid in 
      let pre,snid = get_last_descendant_preorder store snid in 
	nodeid_of_stored_nodeid docid snid
	  

    (* Calculate extent (first_pre, last_pre) -> *)
    (* taf: throw away first, descendant-or-self v. descendant *)	
    let improved_descendant_helper_snodeid store taf nto snid =
      let recorddb = get_main_db store in 
      let last_pre , last_node = get_last_descendant_preorder store snid in  
      let record               = Recno.recno_get_unsafe recorddb snid in 	
      let first_pre            = Record.get_preorder record in
	Cursor.cursor_filter (eval_node_test store Xquery_common_ast.Descendant nto) 
	  (Renumber_Policy.matching_interval store.renumber_policy taf first_pre last_pre)

    (* taf: throw away first *)	
    let improved_descendant_helper taf nid nto =
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in 
      let snid  = stored_nodeid_of_nodeid docid nid in 
	Cursor.cursor_map (nodeid_of_stored_nodeid docid)
	  (improved_descendant_helper_snodeid store taf nto snid)

    let improved_descendant nid nto         = improved_descendant_helper true nid nto
    let improved_descendant_or_self nid nto = improved_descendant_helper false nid nto

	  
    (* End navigation *)
    let get_content nid = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in 
      let snid  = stored_nodeid_of_nodeid docid nid in 
	get_content_snodeid store snid

    let get_nodekind nid = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in 
      let snid  = stored_nodeid_of_nodeid docid nid in 
	get_nodekind_snodeid store snid

    let get_nsenv nid = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in 
      let snid  = stored_nodeid_of_nodeid docid nid in 
	get_nsenv_snodeid store snid


    let get_doc_uri jid = raise (Query (Prototype ("Store access not implemented yet")))

    let get_pi_target nid =
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in 
      let snid  = stored_nodeid_of_nodeid docid nid in 
	get_pi_target_snodeid store snid

    let get_pi_value nid = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in 
      let snid  = stored_nodeid_of_nodeid docid nid in 
	get_pi_value_snodeid store snid


    let get_comment_value nid = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in 
      let snid  = stored_nodeid_of_nodeid docid nid in 
	get_comment_value_snodeid store snid



    (**********************************************************)
    (* Node Storage. This is used internally during loading.  *)
    (* All nodeids should be assigned by this code            *)
    (**********************************************************)
    let store_node store preorder record = 
      let recorddb = get_main_db store in
      let nodeid   = new_snodeid store in
	set_max_preorder store preorder;    
	(* Format.printf "Storing nodeid: %d@." nodeid;  *)
	Renumber_Policy.store_node store.renumber_policy nodeid preorder;
	Recno.recno_put recorddb nodeid record;
	nodeid

    let store_document_node store preorder = 
      let new_record = Record.create_record preorder None Basetypes.DocumentRecord in
      let docid          = get_docid store in 
	nodeid_of_stored_nodeid docid
	  (store_node store preorder new_record)
	  

    let store_element_node store preorder parent qname type_sig_opt nsid  = 
      let element_map        = get_element_qname_map_db store in 
      let name_symbol        = Element_Mapping.encode_symbol element_map qname in 
      let type_map = get_type_qname_map_db store in 
      let type_name_opt      = match type_sig_opt with 
	| None -> None
	| Some (type_name, nilled, av_list) -> 
	    Some (Type_Mapping.encode_symbol type_map type_name)
      in

      let docid              = get_docid store in 
      let parent_snid        = stored_nodeid_of_nodeid docid parent in 
      let new_record = 
	Record.create_record preorder (Some parent_snid) 
	  (Basetypes.ElementRecord (name_symbol, nsid, type_name_opt)) in 
	
      let internal_nodeid    = store_node store preorder new_record in
	store_typed_element store nsid internal_nodeid type_sig_opt;

	nodeid_of_stored_nodeid docid internal_nodeid
	  
	  
    (* Attribute values are currently store in the textdb *)
    let store_attribute_node store preorder (parent,nsid) qname attr_content type_name_opt = 
      let docid              = get_docid store in 
      let parent_snid        = stored_nodeid_of_nodeid docid parent in

      let attribute_map      = get_attribute_qname_map_db store in 
      let attr_symbolid      = Attribute_Mapping.encode_symbol attribute_map qname in 

      let encoded_type_name      = 
	match type_name_opt with 
	  | None -> None
	  | Some (av_list, type_name) -> 
	      let type_map = get_type_qname_map_db store in 
		Some (Type_Mapping.encode_symbol type_map type_name)
      in

      let text_id            = get_textid store (Basetypes.text_of_xml_attribute attr_content) in 

      (* Create and store the record *)
      let new_record         = Record.create_record preorder (Some parent_snid) 
				 (Basetypes.AttributeRecord (attr_symbolid, text_id, encoded_type_name)) in 

      let internal_nodeid    = store_node store preorder new_record in
	store_typed_attribute store internal_nodeid nsid type_name_opt;

      let docid          = get_docid store in 
        nodeid_of_stored_nodeid docid
	  (store_node store preorder new_record)


    let store_text_node store preorder parent text = 
      let text_id = get_textid store (Basetypes.text_of_xs_untyped text) in 
      let docid              = get_docid store in 
      let parent_snid        = stored_nodeid_of_nodeid docid parent in 
      let new_record = Record.create_record preorder (Some parent_snid) (Basetypes.TextRecord text_id) in 
      let docid          = get_docid store in 
	nodeid_of_stored_nodeid docid
	  (store_node store preorder new_record)

    let store_comment store preorder parent text =
      let text_id     = get_commentid store text in 
      let docid       = get_docid store in 
      let parent_snid = stored_nodeid_of_nodeid docid parent in 
      let new_record  = Record.create_record preorder (Some parent_snid) (Basetypes.CommentRecord text_id) in 
      let docid          = get_docid store in 
        nodeid_of_stored_nodeid docid
	  (store_node store preorder new_record)

    let store_processing_instruction store preorder parent pi =
      let pid           = get_processing_id store pi in 
      let docid       = get_docid store in 
      let parent_snid = stored_nodeid_of_nodeid docid parent in 
      let new_record    = Record.create_record preorder (Some parent_snid) (Basetypes.PIRecord pid) in 
      let docid         = get_docid store in 
	nodeid_of_stored_nodeid docid
	  (store_node store preorder new_record)

    (* List is in reverse document order hence the first child is in the last *)
    let store_children_snodeid store parent_pre children = 
      let _ = get_children_index_db store in 
	if (Cursor.cursor_is_empty children) then ()
	else begin
	  
	  (* Redundant *)      
	  (* List is in reverse document order hence the first child
	     is in the last. However since we do preorder sorting, we
	     don't care. *)
	  let add_children_fold cur_node prev_node = 
	    (* Do the insert and return this node *)
	    insert_children_index store parent_pre cur_node;
	    insert_next_sibling_index store prev_node cur_node;
	    prev_node
	  in
	    
	  let last_child  = Cursor.cursor_next children in 
	    
	  let first_child = 
	    Cursor.cursor_fold_left add_children_fold 
	      last_child children
	  in
	    insert_children_index store parent_pre first_child; 
	    insert_first_child_index store parent_pre first_child
	end     
	  
    let store_children store parent children = 
      let docid          = get_docid store in 
      let parent         = stored_nodeid_of_nodeid docid parent in
      let children       = 
	Cursor.cursor_map 
	  (stored_nodeid_of_nodeid docid) children 
      in          
	store_children_snodeid store parent children

    let store_attributes_snodeid store element_snodeid attributes =  
      let append_preorder v = (preorder_of_snodeid store v,v) in 
	insert_attr_index store element_snodeid (Cursor.cursor_map append_preorder attributes)

    let store_attributes store element_nid attributes = 
      let docid          = get_docid store in 
      let element_snid   = stored_nodeid_of_nodeid docid element_nid in
      let attributes     = Cursor.cursor_map 
			     (stored_nodeid_of_nodeid docid) attributes 
      in              
	store_attributes_snodeid store element_snid attributes


    let store_nsenv store old_nsid new_bindings = 
      let namespaceid = new_namespaceid store in  
      let new_binding =  (old_nsid, new_bindings) in
	Namespaceid_Binding_Hash.hash_put 
	  store.main_namespace_db namespaceid new_binding;
	namespaceid	
	  
    (*******************)
    (* Loading Section *)
    (*******************)

    (* This module is to pass to the loading functor so that we can have
       loading per store *)
    type shredded_store_alias = shredded_store
    module This_For_Loading = struct
      type shredded_store = shredded_store_alias
      type namespaceid    = Basetypes.namespaceid
      type text           = Basetypes.text 

      let namespaceid_seed  = namespaceid_seed

      let close_store     = close_store
      let sync_store      = sync_store
			      
      let invalid_nodeid       = invalid_nodeid
      let get_store_from_docid = get_store_from_docid
      let get_name_of_docid    = get_name_of_docid
      let preorder_of_nodeid   = preorder_of_nodeid
      let docid_of_nodeid      = docid_of_nodeid				    
      let preorder_of_nodeid   = preorder_of_nodeid
      let get_docid            = get_docid
				   

      (* Fix these types (no strings) *)
      let shredded_text_of_xml_attribute = Basetypes.text_of_xml_attribute
      let xml_attribute_of_shredded_text = Basetypes.xml_attribute_of_text
					     
      let shredded_text_of_text_desc     = Basetypes.text_of_text_desc
      let text_desc_of_shredded_text     = Basetypes.text_desc_of_text
					     
      let xs_untyped_of_text            = Basetypes.xs_untyped_of_text
      let text_of_xs_untyped            = Basetypes.text_of_xs_untyped
					     
      let create_shredded_store          = create_shredded_store

					     
      let store_document_node            = store_document_node	
      let store_element_node             = store_element_node

      let store_attribute_node           = store_attribute_node
      let store_text_node                = store_text_node

      let store_processing_instruction   = store_processing_instruction
      let store_comment                  = store_comment
					     
      let store_children                 = store_children
      let store_attributes               = store_attributes

      let store_nsenv                    = store_nsenv
    end
    module Shredded_Load = Shredded_load.Shredded_Load_Functor (This_For_Loading)

    let load_shredded_store_from_resolved_stream = Shredded_Load.load_shredded_store_from_resolved_stream
    let load_shredded_store_from_ordered_typed_stream = Shredded_Load.load_shredded_store_from_ordered_typed_stream

    let () = load_shredded_store_from_resolved_stream_ref := Shredded_Load.load_shredded_store_from_resolved_stream
    let () = load_shredded_store_from_ordered_typed_stream_ref := Shredded_Load.load_shredded_store_from_ordered_typed_stream 
    
		     
    let renumber_nodes store preceding_node nodes = 
      let preceding_preorder = preorder_of_nodeid preceding_node in 
      let _  = get_docid store in 
      let first  = Cursor.cursor_peek nodes in 
	(* print_endline "[RENUMBER] INSERT"; *)
      let _ = Renumber_Policy.insert_nodes store.renumber_policy preceding_preorder nodes in
	(* print_endline "[RENUMBER] INSERT DONE"; *)
	first

    let insert_node insert_nodeid_opt parent_nid item_sequence = 
      let docid = docid_of_nodeid parent_nid in 
      let store = get_store_from_docid docid in
	(* Calculate previous nodeid for the insert. *)
      let prev_nid = 
	match insert_nodeid_opt with
	  | None               -> 
	      (* If we have no insert location, we insert as the last one *)	
	      get_last_descendant (parent_nid)

	  | Some insert_nodeid -> 		
	      begin 
		(* If we have an insert location, we insert it before *)
		let snid     = stored_nodeid_of_nodeid docid insert_nodeid in 
		let recorddb = get_main_db store in 
		let record   = Recno.recno_get_unsafe recorddb snid in 
		let preorder = Record.get_preorder record in	    
		  (* Format.printf "Getting Previous => Nodeid: %d preorder %a@."
		    snid
		    Shredded_renumber.Cell_As_Int64.print_int64_pair preorder; *)

		  match Renumber_Policy.previous_entry store.renumber_policy preorder with 
		    | None -> raise (Query (Shredded_Error ("[Shredded Store] Insert Location is root?")))
		    | Some (preorder, snid) ->
			nodeid_of_stored_nodeid docid snid		    
	      end
      in

      (* 1. Exports the data model as a stream *)
      (* Change it to Export/Materialize to main memory *)
      let input_stream = Physical_export.typed_xml_stream_of_datamodel item_sequence in 

      (* 2. Do the erasure *)
      let resolved_input_stream = Streaming_ops.erase_xml_stream_section_3_7_1 input_stream in 

      (* 3. Get the leading attributes in the resolved stream *)
      let _ =
	List.map (fun (n,t,c,d) -> (n, Basetypes.text_of_xs_untyped t,c,d))
	  (Streaming_ops.consume_leading_attribute_events resolved_input_stream)
      in
	
	  (* Format.printf "ABOUT TO RENUMBER 1 | parent: %d prev: %d@." 	  
	  (stored_nodeid_of_nodeid docid parent_nid) 
	  (stored_nodeid_of_nodeid docid prev_nid);
	  dump_children store; *)

      let nodeid_cursor = Shredded_Load.load_an_update_from_resolved_stream store resolved_input_stream parent_nid in 

       (* print_endline "ABOUT TO RENUMBER 2"; 
	 dump_children store;  *)

      (* Renumber the nodes *)
      let first_node = renumber_nodes store prev_nid (Cursor.cursor_map (stored_nodeid_of_nodeid docid) nodeid_cursor) in
	
      (* Add in the correct sibling *)
      let () = 
	match (insert_nodeid_opt,first_node) with 
	  | _   , None
	  | None, _ -> ()
	  | (Some prev_sibling), (Some current_node) ->
	      let prev_snid = stored_nodeid_of_nodeid docid prev_sibling in	  
		replace_next_sibling_index store prev_snid current_node
      in
	(* print_endline "Renumbered 3"; 
	   dump_children store; *)
	()


    (* For creating a node we need 
       + preorder (obtained from the counter jungle_load_context)
       + parent (available from top of the stack incomplete_tuple in jungle_load_context)
       + nodekind (element, identified by the TSAX_event)
       + qnameid (create new if not found in Hashtable)
    *)

    let print_store store last_record =
      print_string("Printing store ...... \n");	
      print_string(".....Main index...... \n");	
      print_endline (" --- print_store needs a cursor --- ");
      flush stdout

    (************************)
    (* New update Functions *)
    (************************)
	
    (****************************************************************)
    (* Delete functions:                                            *)
    (* We have the _single_ varieties which handle removing a given *)
    (* nodeid completely but are not recrusive so, removing a node  *)
    (* does not cascade to its children                             *)
    (****************************************************************)

    (***************************************************************)
    (* This function should handle:                                *)
    (*  o deleting it from the record                              *)
    (*  o reclaim nodeids                                          *)
    (*  o remove from auxilliary indexes (i.e. preorder)           *)
    (***************************************************************)
    let delete_single_snodeid store nodeid = 
      Recno.recno_delete (get_main_db store) nodeid

    let delete_single_element_snodeid store snid = 
      Nodeid_Btree.btree_delete_all (get_first_child_index_db store) snid;
      Nodeid_Btree.btree_delete_all (get_next_sibling_index_db store) snid;
      Nodeid_Btree.btree_delete_all (get_children_index_db store) snid;       
      Nodeid_Btree.btree_delete_all (get_attr_index_db store) snid;
      Typed_Element_Value_Hash.hash_delete (get_typed_element_value_hash_db store) snid;
      delete_single_snodeid store snid
	
    let delete_single_attribute_snodeid store snid = 
      Typed_Attribute_Value_Hash.hash_delete (get_typed_attribute_value_hash_db store) snid;       
      Textid_Btree.btree_delete_all (get_text_db store) (get_attribute_valueid_snodeid store snid);
      delete_single_snodeid store snid

    let delete_single_comment_snodeid store snid =        
      Comment_Hash.hash_delete (get_comment_db store) (get_commentid_snodeid store snid);
      delete_single_snodeid store snid

    let delete_single_processing_instruction_snodeid store snid = 
      Processingid_Hash.hash_delete (get_processinginstruction_db store) (get_pi_id_snodeid store snid);
      delete_single_snodeid store snid

    let delete_single_text_snodeid store snid = 
      Textid_Btree.btree_delete_all (get_text_db store) (get_text_valueid_snodeid store snid);
      delete_single_snodeid store snid
	
    let delete_single_document_snodeid store snid = 
      Nodeid_Btree.btree_delete_all (get_children_index_db store) snid;
      delete_single_snodeid store snid

    let delete_any_single_snodeid store snid =
      match get_nodekind_snodeid store snid with
	| Basetypes.ElementRecordKind   -> delete_single_element_snodeid store snid
	| Basetypes.AttributeRecordKind -> delete_single_attribute_snodeid store snid
	| Basetypes.TextRecordKind      -> delete_single_text_snodeid store snid
	| Basetypes.PIRecordKind        -> delete_single_processing_instruction_snodeid store snid
	| Basetypes.CommentRecordKind   -> delete_single_comment_snodeid store snid
	| Basetypes.DocumentRecordKind  -> delete_single_document_snodeid store snid

    (********************************************************)
    (* Detach versions of delete.                           *)
    (* In these versions, we just remove the child pointer  *)
    (* TODO: Handle document root/nodes correctly.          *)
    (********************************************************)
    let detach_node_snodeid store snid = 
      let recorddb = get_main_db store in
      let record   = Recno.recno_get_unsafe recorddb snid in 
      let parent   = Record.get_parent record in 
      let preorder = Record.get_preorder record in 
	match parent with
	  | None -> raise (Query (Shredded_Error "Trying to delete document node"))
	  | Some parent ->
	      (* Detach the child's parent pointer and the parent's child pointer *)
	      let record = Record.set_parent record None in 
		Recno.recno_put recorddb snid record;
		(* Format.printf "[Internal Delete] Recno Updated@."; *)
		Nodeid_Btree.btree_delete (get_children_index_db store) parent (preorder, snid)	       
		
    let detach_node nid = 
      let docid = docid_of_nodeid nid in 
      let store = get_store_from_docid docid in
      let snid  = stored_nodeid_of_nodeid docid nid in 
	detach_node_snodeid store snid


  (***********************************)
  (* HACKS FOR PROFILING AND TESTING *)
  (***********************************)
    let recno_iter cursor_fn store =
      let recno = get_main_db store in
      let rec_cursor = Recno.recno_cursor_open recno in 
	match Recno.recno_cursor_get_first rec_cursor with
	  | None -> ()
	  | Some record ->
	      cursor_fn record;
	      Cursor.cursor_iter cursor_fn (Recno.recno_cursor_to_cursor rec_cursor Recno.Next );
	      Recno.recno_cursor_close rec_cursor

  end
    

