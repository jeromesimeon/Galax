(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_load.ml,v 1.8 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Shredded_load
   Description:
     This module implements loading of a set of physical Shredded
     indexes from a (typed) XML stream. It is a functor parameterized
     by the store 
*)
 
open Error

open Streaming_types
open Streaming_util

(********************************)
(* Return the proper next event *)
(********************************)

module type Shredded_Load_Functor_Sig = 
  functor (Shredded_store : Shredded_load_sigs.Shredded_Load_Store) ->
sig
  
  val load_shredded_store_from_resolved_stream : 
    Nodeid_context.nodeid_context -> Streaming_types.resolved_xml_stream -> 
    string -> string -> int -> Shredded_store.shredded_store
    (** [load_shredded_store_from_resolved_stream xml_stream dir
	name] builds a new Shred store in directory [dir], with
	logical name [name], populated from the stream
	[xml_stream] *)


  (* Returns a cursor but materializes inside *)
  val load_an_update_from_resolved_stream : 
    Shredded_store.shredded_store -> Streaming_types.resolved_xml_stream -> Nodeid.nodeid -> Nodeid.nodeid Cursor.cursor

  val close_shredded_store : Shredded_store.shredded_store -> unit


  val load_shredded_store_from_ordered_typed_stream : Nodeid_context.nodeid_context -> Streaming_types.ordered_typed_xml_stream -> 
    string -> string -> int -> Shredded_store.shredded_store  

end

module Shredded_Load_Functor 
  (Shredded_store : Shredded_load_sigs.Shredded_Load_Store) 
  = struct
    module Shredded_Load_Context = Shredded_load_context.Shredded_Load_Context_Functor (Shredded_store)

    let rec next_event doc_stream =
      try
	let e = Cursor.cursor_next doc_stream in
	  match e.rse_desc with
	    | RSAX_characters "" ->
		next_event doc_stream
	    | _ -> e
      with
	| Stream.Failure ->
	    raise (Query (Stream_Error "Stream is not well-formed in Loading"))

    (* Note:
       The following function also deals with coalescing text nodes.
       - Jerome
    *)


    let rec next_event_discard doc_stream =
      let rec next_event_discard_aux previous fi =
	match (Cursor.cursor_peek doc_stream) with
	  | Some event ->
	      begin
		match event.rse_desc with
		  | (RSAX_characters s) ->
		      begin
			ignore(Cursor.cursor_next doc_stream);
			next_event_discard_aux (previous ^ s) fi
		      end
		  | _ ->
		      Some (fmkrse_event (RSAX_characters previous) fi)
	      end
	  | _ ->
	      Some (fmkrse_event (RSAX_characters previous) fi)
      in
	try
	  let e = Cursor.cursor_next doc_stream in
	    match e.rse_desc with
	      | RSAX_characters "" ->
		  next_event_discard doc_stream
	      | RSAX_characters s ->
		  next_event_discard_aux s e.rse_loc
	      | _ -> Some e
	with
	  | Stream.Failure ->
	      None

    let next_event_discard_wrap doc_stream () =
      next_event_discard doc_stream


    (****************************)
    (* Builds an attribute node *)
    (****************************)

    let load_sax_xml_attribute_in_shredded_store shredded_load_context parent (rattr_sym, attribute_content) =
      let shredded_store = Shredded_Load_Context.get_shredded_store shredded_load_context in 
      let pre_order      = Shredded_Load_Context.new_preorder shredded_load_context in 
	Shredded_store.store_attribute_node shredded_store pre_order parent rattr_sym attribute_content None
	
    (******************************)
    (* Load the content of a node *)
    (******************************)

    let load_one_event shredded_load_context xml_event =
      let shredded_store = Shredded_Load_Context.get_shredded_store shredded_load_context in 
	match xml_event.rse_desc with
	  | RSAX_startDocument (xml_decl,_,base_uri) ->	    
	      (* Base uri not stored *)
	      let pre_order      = Shredded_Load_Context.new_preorder shredded_load_context in
	      let doc_node       = Shredded_store.store_document_node shredded_store pre_order in
		Shredded_Load_Context.update_current_parent_with_child shredded_load_context doc_node;
		Shredded_Load_Context.become_next_parent shredded_load_context doc_node
		  
	  | RSAX_endDocument ->
	      let current_node   = Shredded_Load_Context.get_current_parent shredded_load_context in 
	      let children       = Shredded_Load_Context.get_current_children shredded_load_context in 
		Shredded_store.store_children shredded_store current_node (Cursor.cursor_of_list children);
		Shredded_Load_Context.no_longer_parent shredded_load_context

	  | RSAX_startElement (relem_sym, attributes, has_element_content, baseuri, delta_bindings) -> 
	      let pre_order      = Shredded_Load_Context.new_preorder shredded_load_context in 
	      let parent         = Shredded_Load_Context.get_current_parent shredded_load_context in 
		(********************************************************************)
		(* Store the namespace env for this node, this a little complicated *) 
		(* There might not be any new bindings here so compare it with the  *)
		(* previous binding                                                 *)
		(********************************************************************)
	      let old_nsenv       = Shredded_Load_Context.get_namespace_env shredded_load_context in 
	      let old_nsid        = Shredded_Load_Context.get_namespace_id shredded_load_context in 
	      let no_new_bindings = Namespace_context.same_nsenv delta_bindings old_nsenv in 
	      let (new_nsid, new_nsenv) =
		if no_new_bindings then
	  	  (* If it is the same binding then return old nsid and old nsenv *)
		  (old_nsid, old_nsenv)
		else 
		  begin
	  	    (* Get the new bindings *)
	  	    let binding_table = Namespace_context.active_bindings delta_bindings in 
	  	    let nsid = Shredded_store.store_nsenv shredded_store old_nsid binding_table in 
		      (nsid, delta_bindings)
		  end
	      in 	
		Shredded_Load_Context.add_namespace_env shredded_load_context new_nsid new_nsenv;

		(* Store the element *)		
		let element_node = Shredded_store.store_element_node 
				     shredded_store pre_order parent relem_sym None new_nsid	
		in
		  (* Store its attributes *)
		let attr_nodeids = 
		  List.map (load_sax_xml_attribute_in_shredded_store shredded_load_context (element_node,new_nsid)) attributes
		in
		  Shredded_store.store_attributes shredded_store element_node (Cursor.cursor_of_list attr_nodeids);
		  (* Store them in the node *)
		  Shredded_Load_Context.update_current_parent_with_child shredded_load_context element_node;
		  Shredded_Load_Context.become_next_parent shredded_load_context element_node;
		  (* We need to 'become the parent' to store them for now *)
		  Shredded_Load_Context.update_attributes shredded_load_context attr_nodeids
		    
	  | RSAX_endElement ->
	      let current_node = Shredded_Load_Context.get_current_parent shredded_load_context in 
	      let children     = Shredded_Load_Context.get_current_children shredded_load_context in 
		Shredded_store.store_children shredded_store current_node (Cursor.cursor_of_list children);
		Shredded_Load_Context.remove_namespace_env shredded_load_context;
		Shredded_Load_Context.no_longer_parent shredded_load_context 

	  | RSAX_processingInstruction (target,content) -> 
	      let pre_order = Shredded_Load_Context.new_preorder shredded_load_context in 
	      let parent    = Shredded_Load_Context.get_current_parent shredded_load_context in 
	      let current   = Shredded_store.store_processing_instruction shredded_store pre_order parent (target,content) in
		Shredded_Load_Context.update_current_parent_with_child shredded_load_context current
		  
	  | RSAX_comment c -> 
	      let pre_order  = Shredded_Load_Context.new_preorder shredded_load_context in 
	      let parent     = Shredded_Load_Context.get_current_parent shredded_load_context in 
	      let current    = Shredded_store.store_comment shredded_store pre_order parent c  in
		Shredded_Load_Context.update_current_parent_with_child shredded_load_context current

	  | RSAX_characters s ->
	      let pre_order  = Shredded_Load_Context.new_preorder shredded_load_context in 
	      let parent     = Shredded_Load_Context.get_current_parent shredded_load_context in 
	      let current    = Shredded_store.store_text_node shredded_store pre_order parent s in 
		Shredded_Load_Context.update_current_parent_with_child shredded_load_context current
		  
	  | RSAX_attribute a -> 
	      (* This should never occur *)
	      raise (Query (Shredded_Error "Does not support loading of streams containing attributes"))

	  | RSAX_atomicValue av ->
	      (* Does not support this yet *)
	      raise (Query (Shredded_Error "Does not support data model loading of non-node streams"))

	  | RSAX_hole ->
	      raise (Query (Shredded_Error "Cannot materialize an XML stream with holes"))
	      
  
    let load_xml_node_content_in_shredded_store_from_stream shredded_load_context doc_stream =
      let cleaned_stream = Cursor.cursor_of_function (next_event_discard_wrap doc_stream) in
	Cursor.cursor_iter (load_one_event shredded_load_context) cleaned_stream


    (*********************************)
    (* Load the document node itself *)
    (*********************************)

    let load_document_node_in_shredded_store_from_sax shredded_load_context doc_stream =
      (* Process the next event *)
      let xml_event = Cursor.cursor_next doc_stream in
	match xml_event.rse_desc with
	  | RSAX_startDocument (xml_decl,_,base_uri) ->
	      let shredded_store = Shredded_Load_Context.get_shredded_store shredded_load_context in 
	      let pre_order      = Shredded_Load_Context.new_preorder shredded_load_context in 
		Shredded_Load_Context.add_namespace_env shredded_load_context 
		Shredded_Load_Context.initial_namespaceid Namespace_context.empty_nsenv;
		let doc_node     = Shredded_store.store_document_node shredded_store pre_order in
		  Shredded_Load_Context.become_next_parent shredded_load_context doc_node;
			
		  (* Process the rest of the document. *)
		  load_xml_node_content_in_shredded_store_from_stream shredded_load_context doc_stream; 
		  
		  (* Return the shredded_store_handle. *)		  
		  shredded_store
		    
	  | RSAX_endDocument
	  | RSAX_startElement _
	  | RSAX_endElement
	  | RSAX_processingInstruction _
	  | RSAX_comment _
	  | RSAX_characters _
	  | RSAX_attribute _
	  | RSAX_atomicValue _ ->
	      raise (Query (Shredded_Error "Not a well-formed XML document"))
	  | RSAX_hole ->
	      raise (Query (Shredded_Error "Cannot materialize an XML stream with holes"))
      

    let load_shredded_store_from_resolved_stream nodeid_context doc_stream directory log_name buff_size =
      let gen = Shredded_renumber.Cell_As_Int64.Generator.create () in 
      let shredded_load_context = Shredded_Load_Context.build_load_context nodeid_context directory log_name gen buff_size in
	load_document_node_in_shredded_store_from_sax shredded_load_context doc_stream


    let load_an_update_from_resolved_stream store doc_stream parent =            
      let shredded_load_context = Shredded_Load_Context.build_load_update_context store in
	Shredded_Load_Context.add_namespace_env shredded_load_context 
	  Shredded_Load_Context.initial_namespaceid Namespace_context.empty_nsenv;	

	(* Notice we store the association of the parent/child here *)
	Shredded_Load_Context.become_next_parent shredded_load_context parent;
	let ()       = load_xml_node_content_in_shredded_store_from_stream shredded_load_context doc_stream in	  
	let children     = Shredded_Load_Context.get_current_children shredded_load_context in 
	  Shredded_store.store_children store parent (Cursor.cursor_of_list children);
	  Shredded_Load_Context.remove_namespace_env shredded_load_context;
	  Shredded_Load_Context.no_longer_parent shredded_load_context;
	  Cursor.cursor_of_list (Shredded_Load_Context.get_nodeids shredded_load_context)


    (*************************)
    (* Ordered Typed Section *)
    (*************************)

    let rec next_ordered_typed_event doc_stream =
      try
	let e = Cursor.cursor_next doc_stream in
	  match e.otse_desc with
	    | OTSAX_characters ("",_) ->
		next_ordered_typed_event doc_stream
	    | _ -> e
      with
	| Stream.Failure ->
	    raise (Query (Stream_Error "Stream is not well-formed in Loading"))

    (* Note:
       The following function also deals with coalescing text nodes.
       - Jerome
    *)


    let rec next_ordered_typed_event_discard doc_stream =
      let rec next_ordered_typed_event_discard_aux (previous,order) annot fi =
	match (Cursor.cursor_peek doc_stream) with
	  | Some event ->
	      begin
		match event.otse_desc with
		  | (OTSAX_characters (s,_)) ->
		      begin
			ignore(Cursor.cursor_next doc_stream);
			next_ordered_typed_event_discard_aux ((previous ^ s),order) annot fi
		      end
		  | _ ->
		      Some (fmkotse_event (OTSAX_characters (previous,order)) annot fi)
	      end
	  | None ->
	      Some (fmkotse_event (OTSAX_characters (previous,order)) annot fi)
      in
	try
	  let e = Cursor.cursor_next doc_stream in
	    match e.otse_desc with
	      | OTSAX_characters ("",_) ->
		  next_ordered_typed_event_discard doc_stream
	      | OTSAX_characters (s,order) ->
		  next_ordered_typed_event_discard_aux (s,order) e.otse_annot e.otse_loc
	      | _ -> Some e
	with
	  | Stream.Failure ->
	      None

    let next_ordered_typed_event_discard_wrap doc_stream () =
      next_ordered_typed_event_discard doc_stream

    let load_ordered_typed_sax_xml_attribute_in_shredded_store shredded_load_context parent 
      ((rattr_sym, attribute_content, type_annotation, atomicValue),(docid,pre_order,post)) =
      let pre_order = Shredded_renumber.Cell_As_Int64.of_int pre_order in
      let shredded_store = Shredded_Load_Context.get_shredded_store shredded_load_context in 
	Shredded_store.store_attribute_node 
	  shredded_store pre_order parent rattr_sym attribute_content (Some (atomicValue, type_annotation))

    (************************************************)
    (* We don't store postorder, dtds or a base_uri *)
    (************************************************)
    let load_one_ordered_typed_event shredded_load_context xml_event =
      let shredded_store = Shredded_Load_Context.get_shredded_store shredded_load_context in 
	match xml_event.otse_desc with	
          | OTSAX_startDocument ((xml_decl,_,base_uri),(docid,pre_order)) -> 	    
	      let doc_node       = Shredded_store.store_document_node shredded_store 
		(Shredded_renumber.Cell_As_Int64.of_int pre_order) in
		Shredded_Load_Context.update_current_parent_with_child shredded_load_context doc_node;
		Shredded_Load_Context.become_next_parent shredded_load_context doc_node
		  
	  | OTSAX_endDocument post ->
	      let current_node   = Shredded_Load_Context.get_current_parent shredded_load_context in 
	      let children       = Shredded_Load_Context.get_current_children shredded_load_context in 
		Shredded_store.store_children shredded_store current_node (Cursor.cursor_of_list children);
		Shredded_Load_Context.no_longer_parent shredded_load_context

	  | OTSAX_startElement ((relem_sym, attributes, has_element_content, baseuri, delta_bindings, nilled, type_annotation, atomicValues), 
				(docid, pre_order)) ->
	      let parent         = Shredded_Load_Context.get_current_parent shredded_load_context in 
		(********************************************************************)
		(* Store the namespace env for this node, this a little complicated *) 
		(* There might not be any new bindings here so compare it with the  *)
		(* previous binding                                                 *)
		(********************************************************************)
	      let old_nsenv       = Shredded_Load_Context.get_namespace_env shredded_load_context in 
	      let old_nsid        = Shredded_Load_Context.get_namespace_id shredded_load_context in 
	      let no_new_bindings = Namespace_context.same_nsenv delta_bindings old_nsenv in 
	      let (new_nsid, new_nsenv) =
		if no_new_bindings then
	  	  (* If it is the same binding then return old nsid and old nsenv *)
		  (old_nsid, old_nsenv)
		else begin
	  	  (* Get the new bindings *)
	  	  let binding_table = Namespace_context.active_bindings delta_bindings in 
	  	  let nsid = Shredded_store.store_nsenv shredded_store old_nsid binding_table in 
		    (nsid, delta_bindings)
		end
	      in 	
		Shredded_Load_Context.add_namespace_env shredded_load_context new_nsid new_nsenv;

		(* Store the element *)		
		let element_node = Shredded_store.store_element_node 
		  shredded_store (Shredded_renumber.Cell_As_Int64.of_int pre_order)
		  parent relem_sym (Some (type_annotation,nilled,atomicValues)) new_nsid 
		in
		  (* Store its attributes *)
		let attr_nodeids = 
		  List.map (load_ordered_typed_sax_xml_attribute_in_shredded_store shredded_load_context (element_node,new_nsid)) 
		    attributes
		in
		  Shredded_store.store_attributes shredded_store element_node (Cursor.cursor_of_list attr_nodeids);
		  (* Store them in the node *);
		  Shredded_Load_Context.update_current_parent_with_child shredded_load_context element_node;
		  Shredded_Load_Context.become_next_parent shredded_load_context element_node
		    
		    
	  | OTSAX_endElement post ->
	      let current_node = Shredded_Load_Context.get_current_parent shredded_load_context in 
	      let children     = Shredded_Load_Context.get_current_children shredded_load_context in 
		Shredded_store.store_children shredded_store current_node (Cursor.cursor_of_list children);
		Shredded_Load_Context.remove_namespace_env shredded_load_context;
		Shredded_Load_Context.no_longer_parent shredded_load_context 

	  | OTSAX_processingInstruction ((target, content), (docid,pre_order,post)) ->
	      let parent    = Shredded_Load_Context.get_current_parent shredded_load_context in 
	      let pre_order = Shredded_renumber.Cell_As_Int64.of_int pre_order in 
	      let current   = Shredded_store.store_processing_instruction shredded_store pre_order parent (target,content) in
		Shredded_Load_Context.update_current_parent_with_child shredded_load_context current

	  | OTSAX_comment (c,(docid,pre_order,post)) ->
	      let pre_order = Shredded_renumber.Cell_As_Int64.of_int pre_order in 
	      let parent     = Shredded_Load_Context.get_current_parent shredded_load_context in 
	      let current    = Shredded_store.store_comment shredded_store pre_order parent c  in
		Shredded_Load_Context.update_current_parent_with_child shredded_load_context current

	  | OTSAX_characters (s, (docid, pre_order, post)) ->
	      let pre_order = Shredded_renumber.Cell_As_Int64.of_int pre_order in 
	      let parent    = Shredded_Load_Context.get_current_parent shredded_load_context in 
	      let current   = Shredded_store.store_text_node shredded_store pre_order parent s in 
		Shredded_Load_Context.update_current_parent_with_child shredded_load_context current

	  (* Additions to the standard SAX events: *)
	  | OTSAX_attribute a ->
	      (* This should never occur *)
	      raise (Query (Shredded_Error "Does not support loading of streams containing attributes"))

	  | OTSAX_atomicValue av ->

	      (* Does not support this yet *)
	      raise (Query (Shredded_Error "Does not support data model loading of non-node streams"))

	  | OTSAX_hole ->
	      raise (Query (Shredded_Error "Cannot materialize an XML stream with holes"))
		(* Enclosed expressions only occur in typed streams and are 
		   eliminated during erasure *)
	  | OTSAX_startEncl 
	  | OTSAX_endEncl ->
	      raise (Query (Shredded_Error "Do not support loading of enclosed expressions"))

    let load_xml_node_content_in_shredded_store_from_ordered_typed_stream shredded_load_context doc_stream =
      let cleaned_stream = Cursor.cursor_of_function (next_ordered_typed_event_discard_wrap doc_stream) in
      Cursor.cursor_iter (load_one_ordered_typed_event shredded_load_context) cleaned_stream

    let load_document_node_in_shredded_store_from_ordered_typed_stream shredded_load_context doc_stream =
      (* Process the next event *)
      let xml_event = Cursor.cursor_next doc_stream in
      match xml_event.otse_desc with
      | OTSAX_startDocument ((xml_decl,_,base_uri),(docid,preorder)) ->
	  let shredded_store = Shredded_Load_Context.get_shredded_store shredded_load_context in 
	  Shredded_Load_Context.add_namespace_env shredded_load_context 
	    Shredded_Load_Context.initial_namespaceid Namespace_context.empty_nsenv;
	  let doc_node     = Shredded_store.store_document_node shredded_store 
	      (Shredded_renumber.Cell_As_Int64.of_int preorder) in
	  Shredded_Load_Context.become_next_parent shredded_load_context doc_node;

	  (* Process the rest of the document. *)
	  load_xml_node_content_in_shredded_store_from_ordered_typed_stream shredded_load_context doc_stream; 

	  (* Return the shredded_store_handle. *)		  
	  shredded_store

      | OTSAX_endDocument _
      | OTSAX_startElement _
      | OTSAX_endElement _
      | OTSAX_processingInstruction _
      | OTSAX_comment _
      | OTSAX_characters _
      | OTSAX_attribute _
      | OTSAX_atomicValue _ ->
	  raise (Query (Shredded_Error "Not a well-formed XML document"))
      | OTSAX_hole ->
	  raise (Query (Shredded_Error "Cannot materialize an XML stream with holes"))
      | OTSAX_startEncl 
      | OTSAX_endEncl ->
	  raise (Query (Shredded_Error "Do not support loading of enclosed expressions"))

    let load_shredded_store_from_ordered_typed_stream nodeid_context doc_stream directory log_name buff_size =
      let gen = Shredded_renumber.Cell_As_Int64.Generator.create () in 
      let shredded_load_context = Shredded_Load_Context.build_load_context nodeid_context directory log_name gen buff_size in
      load_document_node_in_shredded_store_from_ordered_typed_stream shredded_load_context doc_stream
	
    let close_shredded_store handle = 
      (* Format.printf "closing...@."; *)
      Shredded_store.sync_store handle;
      (* Format.printf "Sync'd... @."; *)
      Shredded_store.close_store handle
  end


