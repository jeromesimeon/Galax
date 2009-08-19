(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_load_update.ml,v 1.6 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Shredded_load_update
   Description:
     This module is the implementation of loading the content
	 of inserts and replace in Shred store
*)

open Nodeid 
 
open Error

open Streaming_types
open Streaming_util

exception Shredded_Error_UNCATCHABLE of string

module type Shredded_Load_Update_Functor_Sig = 
  functor (Shredded_Store : Shredded_load_sigs.Shredded_Load_Store) ->
    functor (Shredded_load_context_functor : Shredded_load_context.Shredded_Load_Context_Functor_Sig) ->
      sig

	val shredded_load_insert_content :
	  Dm.node Cursor.cursor ->
	  Shredded_Store.nodeid -> 
	  Shredded_Store.nodeid -> unit 		(* Not sure if there would be a return value for this function : Avinash *) 

end


module Shredded_Load_Update_Functor
  (Shredded_Store : Shredded_load_sigs.Shredded_Load_Store) 
  (Shredded_load_context_functor : Shredded_load_context.Shredded_Load_Context_Functor_Sig) 
  = struct 

    exception Shredded_Update_Stream_End of string
    module Shredded_load_context = Shredded_load_context_functor (Shredded_Store)

    open Shredded_Store
    open Shredded_load_context
     
    type shredded_insert_kind = 
      | ElementInsert of (preorder * preorder * Namespace_symbols.relem_symbol * namespaceid) 
      | AttributeInsert of (preorder * preorder * Namespace_symbols.rattr_symbol * text)
      | TextInsert of (preorder * preorder * text)

	  
    let shredded_update_list = ref []
				 
    let store_update shredded_insert_instance = 
      shredded_update_list := shredded_insert_instance :: !shredded_update_list 	

    let process_all_inserts shredded_store = 
      let apply_updates shredded_insert_instance =  
	match shredded_insert_instance with 
	  | ElementInsert (pre_order, parent_pre, qname, nsid) ->
    	      insert_element_node shredded_store pre_order parent_pre qname nsid 
	  | AttributeInsert (pre_order, parent_pre, attr_qname, attribute_content) ->
	      insert_attribute_node shredded_store  pre_order parent_pre attr_qname attribute_content
	  | TextInsert (pre_order, parent_pre, s) ->
	      insert_text_node shredded_store pre_order parent_pre s
      in 
	List.iter apply_updates (List.rev !shredded_update_list) 

let build_nodeid_context_for_insert store previous_nodeid = 
  let pre_order = next_preorder store (preorder_of_nodeid previous_nodeid) in
    (*  Nodeid_context.build_nodeid_context pre_order min_postorder  *)
    raise (Shredded_Error_UNCATCHABLE
	     ("build_nodeid_context broken, we need to do something about nodeid_contexts and generators"))


let rec peek_event doc_stream =
  let e = Cursor.cursor_peek doc_stream in
  match e with
  | Some ev ->
      begin
	match ev.rse_desc with
	  RSAX_characters "" ->
	    ignore(Cursor.cursor_next doc_stream);
	    peek_event doc_stream
	| _ -> e
      end
  | _ -> e

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
	      ignore(Cursor.cursor_next doc_stream);
	    next_event_discard_aux (previous ^ s) fi
	  | _ ->
	      fmkrse_event (RSAX_characters previous) fi
	end
    | _ ->
	fmkrse_event (RSAX_characters previous) fi
  in
  try
    let e = Cursor.cursor_next doc_stream in
    match e.rse_desc with
    | RSAX_characters "" ->
	next_event_discard doc_stream
    | RSAX_characters s ->
	next_event_discard_aux s e.rse_loc
    | _ -> e
  with
  | Stream.Failure ->
      raise (Shredded_Update_Stream_End "End of stream has reached")

let load_sax_xml_attribute_in_shredded_store shredded_load_context a =
  match a with
    | (rattr_sym, attribute_content) ->
	(* let attr_qname = qname_of_string (Dm_util.string_of_AttrName rattr_sym) in *)
	let shredded_store = get_shredded_store shredded_load_context in 
	let pre_order  = new_preorder shredded_load_context in 
	let parent_pre = get_parent_pre_order shredded_load_context in 
	  (* insert_shredded_attribute_node shredded_store  pre_order parent_pre attr_qname attribute_content *)
	  store_update (AttributeInsert (pre_order, parent_pre, rattr_sym, attribute_content))

let rec load_xml_update_content_in_shredded_store_from_stream shredded_load_context doc_stream = 
  try 
    begin
      let xml_event = next_event_discard doc_stream in 
	match xml_event.rse_desc with
	  | RSAX_startDocument (xml_decl,_,base_uri) ->
    	      (* Does not support this yet *)
    	      raise (Query (Shredded_Error "Insert of document not implemented"))

  	  | RSAX_endDocument ->
    	      (* Does not support this yet *)
    	      raise (Query (Shredded_Error "Insert of document not implemented"))

  	  | RSAX_startElement (relem_sym, attributes, has_element_content, delta_bindings) ->
    	      (* let qname = qname_of_string (Dm_util.string_of_ElemName relem_sym) in *)
    	      let shredded_store = get_shredded_store shredded_load_context in 
    	      let pre_order = new_preorder shredded_load_context in 
    	      let parent_pre = get_parent_pre_order shredded_load_context in 
		(* Dummy call to set up the parent stack in loading context *)
    		store_preorder_to_create_children_index shredded_load_context pre_order;

    		(* insert_shredded_element_node shredded_store pre_order parent_pre qname; *)
		(* inserting a fake nsid here for completing the compilation *)
    		store_update (ElementInsert (pre_order, parent_pre, relem_sym, namespaceid_seed)); 
		
   		(* store attributes *)
	  	
      		List.iter (fun (n,t) -> 
			     load_sax_xml_attribute_in_shredded_store shredded_load_context (n,(text_of_xs_untyped t))) 
		  attributes; 
      		load_xml_update_content_in_shredded_store_from_stream shredded_load_context doc_stream 

  	  | RSAX_endElement ->
	      let shredded_store = get_shredded_store shredded_load_context in 
	 	(* Dummy to pop the top entry of the parent_stack in loading_context *)
	      let pre_order = get_preorder_to_create_children_index shredded_load_context in 
      		load_xml_update_content_in_shredded_store_from_stream shredded_load_context doc_stream 

  	  | RSAX_characters s ->
	      let shredded_store = get_shredded_store shredded_load_context in 
	      let pre_order = new_preorder shredded_load_context in 
	      let parent_pre = get_parent_pre_order shredded_load_context in 
		(* insert_shredded_text_node shredded_store pre_order parent_pre s; *)
		store_update (TextInsert (pre_order, parent_pre, shredded_text_of_text_desc s));
    	  	load_xml_update_content_in_shredded_store_from_stream shredded_load_context doc_stream 
		  
  	  | RSAX_attribute a -> (* This should never occur *)
	      raise (Query (Shredded_Error "No attribute event should appear in middle of the stream"))

  	  | RSAX_processingInstruction (target,content) ->
      	      raise (Query (Shredded_Error "Insert of processing instruction not implemented"))
  	  | RSAX_comment c ->		(* Ignoring Comments for Now *)
     	      raise (Query (Shredded_Error "Insert of comments not implemented"))
  	  | RSAX_atomicValue av ->
      	      raise (Query (Shredded_Error "Insert of atomic value not implemented"))
  	  | RSAX_hole ->
      	      raise (Query (Shredded_Error "Cannot materialize an XML stream with holes"))
    end
  with 
    | Shredded_Update_Stream_End message -> ()

let shredded_load_update shredded_store nodeid_context leading_attributes doc_stream parent_pre prev_pre =
  (* 1. Create the Shred loading context for given shred store
     and nodeid_context *)
  let shredded_load_context = Shredded_load_context.build_shredded_load_update_context shredded_store nodeid_context in 

    (* 2. Update the parent information in the loading context *)
    Shredded_load_context.store_preorder_to_create_children_index shredded_load_context parent_pre;  

    
    (* 3. Insert the attributes before any other node *)
    List.iter (load_sax_xml_attribute_in_shredded_store shredded_load_context) leading_attributes; 

    (* 4. Create a list of insert calls from the attribute free stream *)
    load_xml_update_content_in_shredded_store_from_stream shredded_load_context doc_stream; 

    (* 5. Create required space in the store *)
    let size_of_update = List.length !shredded_update_list in 
      print_string("Size of the update = " ^ string_of_int(size_of_update) ^ "\n"); flush stdout;
      let max_pre_order = Shredded_Store.get_max_pre_order shredded_store in  
      let (last_occuppied_pre_order, new_max_preorder) = 
	Shredded_Store.find_space_for_new_node shredded_store (prev_pre) max_pre_order size_of_update in
	Shredded_Store.renumber_main_index shredded_store 
	  (Shredded_Store.next_preorder shredded_store prev_pre) last_occuppied_pre_order new_max_preorder;
	
	(* 6. Apply the updates from the list created above *)
	process_all_inserts shredded_store;
	
	Shredded_Store.sync_store shredded_store 

(* For attributes do the same as done in eval_util for element_constructor *)
let shredded_load_insert_content node_sequence parent_nodeid previous_nodeid = 
  let item_sequence = Cursor.cursor_map (fun n -> (Dm.Item_Node n)) node_sequence in 

  (* 1. Exports the data model as a stream *)
  (* Change it to Export/Materialize to main memory *)
  let input_stream = Dm_export.typed_xml_stream_of_datamodel item_sequence in 

  (* 2. Do the erasure *)
  let resolved_input_stream = Streaming_ops.erase_xml_stream_section_3_7_1 input_stream in 

  (* 3. Get the leading attributes in the resolved stream *)
  let leading_attributes =
    List.map (fun (n,t) -> (n, text_of_xs_untyped t))
      (Streaming_ops.consume_leading_attribute_events resolved_input_stream)
  in
  let prev_pre = preorder_of_nodeid previous_nodeid in
  let docid    = docid_of_nodeid parent_nodeid in 
  let pre_order = preorder_of_nodeid parent_nodeid in
  let shredded_store = Shredded_Store.get_store_from_docid docid in 
  let nodeid_context = build_nodeid_context_for_insert shredded_store previous_nodeid in 
    shredded_load_update shredded_store nodeid_context leading_attributes resolved_input_stream pre_order prev_pre
      
  end


  
*)
