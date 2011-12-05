(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_load.ml,v 1.28 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Galax_load
   Description:
     This module implements loading of a main-memory data model
     instance from a (typed) XML stream.
*)

open Error

open Streaming_types
open Streaming_util

open Dm_atomic
open Dm
open Physical_value

open Galax_dm
open Galax_load_context
open Dm_atomic_btree_util

(*****************************************)
(* Check the stream is indeed terminated *)
(*****************************************)

let load_fail_from_sax doc_stream =
  try
    ignore(Cursor.cursor_next doc_stream);
    raise (Query (Load_Error "Loading non-terminated stream"))
  with
  | Stream.Failure ->
      ()

(********************************)
(* Return the proper next event *)
(********************************)

let rec peek_event doc_stream =
  Cursor.cursor_peek doc_stream

let rec next_event doc_stream =
  try
    Cursor.cursor_next doc_stream
  with
  | Stream.Failure ->
      raise (Query (Stream_Error "Stream is not well-formed in Loading"))

(* Note:
     The following function also deals with coalescing text nodes.
  - Jerome
*)

let rec next_event_discard doc_stream =
  let rec next_event_discard_aux previous new_nodeid tsa fi =
    match (Cursor.cursor_peek doc_stream) with
    | Some {otse_desc = (OTSAX_characters (s,_))} ->
	begin
	  ignore(Cursor.cursor_next doc_stream);
	  next_event_discard_aux (previous ^ s) new_nodeid tsa fi
	end
    | _ ->
	Streaming_util.fmkotse_event (OTSAX_characters (previous,new_nodeid)) tsa fi
  in
  try
    let e = Cursor.cursor_next doc_stream in
    match e.otse_desc with
    | OTSAX_characters ("",_) ->
	next_event_discard doc_stream
    | OTSAX_characters (s,nodeid) ->
	next_event_discard_aux s nodeid e.otse_annot e.otse_loc
    | _ -> e
  with
  | Stream.Failure ->
      raise (Query (Stream_Error "Stream is not well-formed in Loading (2)"))


(******************************)
(* Update the parent pointers *)
(******************************)


let set_parents self kids =
  List.iter (fun kid -> kid#update_parent self) kids


(****************************)
(* Builds an attribute node *)
(****************************)

let load_sax_xml_attribute load_context a =
  match a with
  | ((uqname, attribute_content, special,rel1, rel2),new_nodeid) ->
      let rattr_sym =
	match !rel1 with
	| None -> raise (Query (Load_Error "Attribute event has not been resolved"))
	| Some r -> r
      in
      let (type_annotation, simple_value) =
	match !rel2 with
	| None -> raise (Query (Load_Error "Attribute event has not been typed"))
	| Some (t,s) -> (t,s)
      in
      let attribute_value = simple_value in
      let attribute_node =
	new galaxAttributeNode
	  new_nodeid
	  rattr_sym
	  (new atomicString attribute_content)
	  type_annotation
	  attribute_value
      in
      (attribute_node :> galaxAttributeNode)

(******************************)
(* Load the content of a node *)
(******************************)

let rec load_xml_node_content_from_stream load_context doc_stream =
  (* Process the next event *)
  let xml_event = next_event_discard doc_stream in
  let label = Sax_annot.get_stream_label_annot xml_event.otse_annot in
  match xml_event.otse_desc with
  | OTSAX_startDocument ((xml_decl,_,base_uri),preorder) ->
      let document_encoding =
	match xml_decl with
	| None -> None
	| Some (_,enc,_) -> enc
      in

      (* Then process the rest of the document. *)
      (* 1. If the label is set to true, then create an entry for the
      result node to be cached *)
      if label
      then Galax_load_context.push_labeled_event load_context
      else Galax_load_context.push_non_labeled_event load_context;
      (* 2. process the sub-tree first *)
      let full_root_forest = load_xml_node_content_from_stream load_context doc_stream in
      let full_root_forest_node = List.map (fun x -> (x :> node)) full_root_forest in
      (* 3. obtain the postorder and id from the context *)
      let postorder = Galax_load_context.get_postorder load_context in
      let nodeid = Nodeid.merge_docorder_from_pre_post preorder postorder in

      (* 4. Create the node itself. *)
      let new_doc_node =
	new galaxDocumentNode
	  nodeid
	  base_uri
	  full_root_forest_node
	  document_encoding
      in
      begin
	(* 5. Pass the node to the context in case it has been labeled *)
	Galax_load_context.pop_event load_context (new_doc_node :> node);
	(* 6. Set the parent pointers *)
	set_parents (new_doc_node :> node) full_root_forest;
	(* 7. return the resulting nodes *)
	(new_doc_node :> galaxNode) :: (load_xml_node_content_from_stream load_context doc_stream)
      end

  | OTSAX_endDocument postorder ->
      Galax_load_context.set_postorder load_context postorder;
      []    (* Terminates recursion for the children of an element.
	       This gives the hand back to the calling element node creation. *)

  | OTSAX_startElement ((uqname, attributes, has_element_content,_,rel1,rel2),preorder) ->
      (* Compute children and attributes *)

      let (relem_sym,baseuri,nsenv) =
	match !rel1 with
	| None -> raise (Query (Load_Error "Element event has not been resolved"))
	| Some (r,b,n) -> (r,b,n)
      in
      let (nilled_flag,type_annotation,simple_value) =
	match !rel2 with
	| None -> raise (Query (Load_Error "Element event has not been typed"))
	| Some (n,t,s) -> (n,t,s)
      in

      (* Note:
           Attribute nodes *must be* computed first for the node-ids
           to be correct.
	 - Jerome *)

      (* 1. If the label is set to true, then create an entry for the
      result node to be cached *)
      if label
      then Galax_load_context.push_labeled_event load_context
      else Galax_load_context.push_non_labeled_event load_context;

      (* 1bis. -- Add dummy node to name or twig index, if required. -- Philippe *)
      let (doc,pre) = preorder in
      let _ = add_pre_order_to_name_index load_context relem_sym pre in

      (* 2. process the attributes *)
      let attributes = List.filter (fun ((_,_,special,_,_),_) -> not !special) attributes in
      let attr_children = List.map (load_sax_xml_attribute load_context) attributes in
      let attr_children_node = List.map (fun x -> (x :> attribute)) attr_children in
      (* 3. process the children *)
      let elem_children = load_xml_node_content_from_stream load_context doc_stream in
      let elem_children_node = List.map (fun x -> (x :> node)) elem_children in

      (* 4. obtain the postorder and id from the context *)
      let postorder = Galax_load_context.get_postorder load_context in
      let nodeid = Nodeid.merge_docorder_from_pre_post preorder postorder in

      (* 5. Compute the element value *)
      let elem_value = simple_value in

      (* 6. Create the node itself. *)
      let new_elem_node =
	new galaxElementNode
	  nodeid
	  baseuri
	  relem_sym
	  nsenv
	  attr_children_node
	  elem_children_node
	  nilled_flag
	  type_annotation
          elem_value
      in
      begin
	(* 7. Pass the node to the context in case it has been labeled *)
	Galax_load_context.pop_event load_context (new_elem_node :> node);
	(* 8. Set the parent pointers *)
	set_parents (new_elem_node :> node) attr_children;
	set_parents (new_elem_node :> node) elem_children;

	(* 9. -- Add node to name or twig index, if required. -- Philippe *)
	let (_,post) = postorder in
(*	let _ = add_element_to_name_index load_context relem_sym (new_elem_node :> node) (doc,pre,post) in *)
	let _ = add_post_order_to_name_index load_context relem_sym (new_elem_node :> node) pre post in
	
	(* 10. return the resulting nodes *)
	(new_elem_node :> galaxNode) :: (load_xml_node_content_from_stream load_context doc_stream)
	
      end

  | OTSAX_endElement postorder ->      
      Galax_load_context.set_postorder load_context postorder;
      []    (* Terminates recursion for the children of an element.
	       This gives the hand back to the calling element node creation. *)

  | OTSAX_processingInstruction ((target,content),nodeid) ->
      let new_pi_node =
	new galaxProcessingInstructionNode
	  nodeid
	  (new atomicString target)
	  (new atomicString content)
      in
      (new_pi_node :> galaxNode) :: (load_xml_node_content_from_stream load_context doc_stream)

  | OTSAX_comment (c,nodeid) ->
      let new_comment_node = new galaxCommentNode nodeid (new atomicString c) in
      (new_comment_node :> galaxNode) :: (load_xml_node_content_from_stream load_context doc_stream)

  | OTSAX_characters (s,nodeid) ->
      let new_text_node = new galaxTextNode nodeid (new atomicString s) in
      (new_text_node :> galaxNode) :: (load_xml_node_content_from_stream load_context doc_stream)

  | OTSAX_attribute a ->
      let new_attr_node = (load_sax_xml_attribute load_context a) in
      (new_attr_node :> galaxNode) :: (load_xml_node_content_from_stream load_context doc_stream)

  | OTSAX_atomicValue _ ->
      raise (Query (Load_Error "Does not support data model loading of a stream with atomic values"))

  | OTSAX_hole ->
      raise (Query (Load_Error "Cannot load stream with a hole!"))

  | OTSAX_startEncl 
  | OTSAX_endEncl -> 
      []


(******************************)
(* Builds a sequence of items *)
(******************************)

(* Note:
     This function is used only for the toplevel items in the stream.
   - Jerome
 *)

let rec load_xml_item_forest_from_sax load_context doc_stream =
  (* Process the next event *)
  match peek_event doc_stream with
  | None ->
      []
  | Some e ->
      begin
	let xml_event = next_event doc_stream in
	match xml_event.otse_desc with
	| OTSAX_startDocument ((xml_decl,_,base_uri),preorder) ->
      (* Get the document encoding *)
	    
	    let document_encoding =
	      match xml_decl with
	      | None -> None
	      | Some (_,enc,_) -> enc
	    in

      (* Then process the rest of the document. *)
	    let full_root_forest = load_xml_node_content_from_stream load_context doc_stream in
	    let postorder = Galax_load_context.get_postorder load_context in
	    let nodeid = Nodeid.merge_docorder_from_pre_post preorder postorder in
	    let full_root_forest_node = List.map (fun x -> (x :> node)) full_root_forest in
	    
      (* Then the node itself. *)
	    let new_doc_node =
	      new galaxDocumentNode
		nodeid
		base_uri
		full_root_forest_node
		document_encoding
	    in
	    let copied_nodes = Galax_load_context.get_result load_context in
	    let copied_nodes = List.map (fun x -> Item_Node x) copied_nodes in
	    begin
	      set_parents (new_doc_node :> node) full_root_forest;
	      (Item_Node (new_doc_node :> node)) :: copied_nodes @ (load_xml_item_forest_from_sax load_context doc_stream)
	    end

	| OTSAX_endDocument postorder ->
	    Galax_load_context.set_postorder load_context postorder;
	    []    (* Terminates recursion for the children of an element.
		     This gives the hand back to the calling element node creation. *)
	      
	| OTSAX_startElement ((uqname, attributes, has_element_content,_,rel1,rel2),preorder) ->
	    
      (* Compute children and attributes *)
	    
      let (relem_sym,baseuri,nsenv) =
	match !rel1 with
	| None -> raise (Query (Load_Error "Element event has not been resolved"))
	| Some (r,b,n) -> (r,b,n)
      in
      let (nilled_flag,type_annotation,simple_value) =
	match !rel2 with
	| None -> raise (Query (Load_Error "Element event has not been typed"))
	| Some (n,t,s) -> (n,t,s)
      in
      (* Note:
           Attribute nodes *must be* computed first for the node-ids
           to be correct.
	 - Jerome *)

	    let attr_children = List.map (load_sax_xml_attribute load_context) attributes in
	    let attr_children_node = List.map (fun x -> (x :> attribute)) attr_children in
	    let elem_children = load_xml_node_content_from_stream load_context doc_stream in
	    let postorder = Galax_load_context.get_postorder load_context in
	    let nodeid = Nodeid.merge_docorder_from_pre_post preorder postorder in
	    let elem_children_node = List.map (fun x -> (x :> node)) elem_children in

      (* Compute the element value *)
	    
	    let elem_value = simple_value in

      (* Finally builds the new element node *)

	    let new_elem_node =
	      new galaxElementNode
		nodeid
		baseuri
		relem_sym
		nsenv
		attr_children_node
		elem_children_node
		nilled_flag
		type_annotation
		elem_value
	    in
	    let copied_nodes = Galax_load_context.get_result load_context in
	    let copied_nodes = List.map (fun x -> Item_Node x) copied_nodes in
	    begin
	      set_parents (new_elem_node :> node) attr_children;
	      set_parents (new_elem_node :> node) elem_children;
	      (Item_Node (new_elem_node :> node)) :: copied_nodes @ (load_xml_item_forest_from_sax load_context doc_stream)
	    end
 
	| OTSAX_endElement postorder ->
	    Galax_load_context.set_postorder load_context postorder;
	    []    (* Terminates recursion for the children of an element.
		     This gives the hand back to the calling element node creation. *)

	| OTSAX_processingInstruction ((target,content),nodeid) ->
	    let new_pi_node = new galaxProcessingInstructionNode nodeid (new atomicString target) (new atomicString content) in
	    (Item_Node (new_pi_node :> node)) :: (load_xml_item_forest_from_sax load_context doc_stream)
	      
	| OTSAX_comment (c,nodeid) ->
	    let new_comment_node = new galaxCommentNode nodeid (new atomicString c) in
	    (Item_Node (new_comment_node :> node)) :: (load_xml_item_forest_from_sax load_context doc_stream)
					   
	| OTSAX_characters (s,nodeid) ->
	    let new_text_node = new galaxTextNode nodeid (new atomicString s) in
	    (Item_Node (new_text_node :> node)) :: (load_xml_item_forest_from_sax load_context doc_stream)
	      
	| OTSAX_attribute a ->
	    let new_attr_node = (load_sax_xml_attribute load_context a) in
	    (Item_Node (new_attr_node :> node)) :: (load_xml_item_forest_from_sax load_context doc_stream)

	| OTSAX_atomicValue av ->
	    (Item_Atomic av) :: (load_xml_item_forest_from_sax load_context doc_stream)

	| OTSAX_hole ->
	    raise (Query (Load_Error "Cannot load stream with a hole!"))
	      
	| OTSAX_startEncl
	| OTSAX_endEncl -> 
	    []
      end

(******************************)
(* Builds a sequence of nodes *)
(******************************)

(* Note:
     This function is used only for the toplevel nodes in the stream.
   - Jerome

   We need to keep track of labeled nodes so that we can return those 

 *)

let rec load_xml_node_forest_from_sax load_context doc_stream =
  (* Process the next event *)
  match peek_event doc_stream with
  | None ->
      []
  | Some _ ->
      begin
	let xml_event = next_event doc_stream in
	match xml_event.otse_desc with
	| OTSAX_startDocument ((xml_decl,_,base_uri),preorder) ->
      (* Get the document encoding *)
	    
	    let document_encoding =
	      match xml_decl with
	      | None -> None
	      | Some (_,enc,_) -> enc
	    in
	    
      (* Then process the rest of the document. *)
	    let full_root_forest = load_xml_node_content_from_stream load_context doc_stream in
	    let postorder = Galax_load_context.get_postorder load_context in
	    let nodeid = Nodeid.merge_docorder_from_pre_post preorder postorder in
	    let full_root_forest_node = List.map (fun x -> (x :> node)) full_root_forest in

      (* Then the node itself. *)
	    let new_doc_node =
	      new galaxDocumentNode
		nodeid
		base_uri
		full_root_forest_node
		document_encoding
	    in
	    let copied_nodes = Galax_load_context.get_result load_context in
	    begin
	      set_parents (new_doc_node :> node) full_root_forest;
	      (new_doc_node :> node) :: copied_nodes @ (load_xml_node_forest_from_sax load_context doc_stream)
	    end

	| OTSAX_endDocument postorder ->
	    []    (* Terminates recursion for the children of an element.
		     This gives the hand back to the calling element node creation. *)
	      
	| OTSAX_startElement ((uqname, attributes, has_element_content,_,rel1,rel2),preorder) ->

      (* Compute children and attributes *)
	    
      let (relem_sym,baseuri,nsenv) =
	match !rel1 with
	| None -> raise (Query (Load_Error "Element event has not been resolved"))
	| Some (r,b,n) -> (r,b,n)
      in
      let (nilled_flag,type_annotation,simple_value) =
	match !rel2 with
	| None -> raise (Query (Load_Error "Element event has not been typed"))
	| Some (n,t,s) -> (n,t,s)
      in
      (* Note:
           Attribute nodes *must be* computed first for the node-ids
           to be correct.
	 - Jerome *)

	    let attr_children = List.map (load_sax_xml_attribute load_context) attributes in
	    let attr_children_node = List.map (fun x -> (x :> attribute)) attr_children in
	    let elem_children = load_xml_node_content_from_stream load_context doc_stream in
	    let postorder = Galax_load_context.get_postorder load_context in
	    let nodeid = Nodeid.merge_docorder_from_pre_post preorder postorder in
	    let elem_children_node = List.map (fun x -> (x :> node)) elem_children in

      (* Compute the element value *)
	    
	    let elem_value = simple_value in
	    
      (* Finally builds the new element node *)

	    let new_elem_node =
	      new galaxElementNode
		nodeid
		baseuri
		relem_sym
		nsenv
		attr_children_node
		elem_children_node
		nilled_flag
		type_annotation
		elem_value
	    in
	    let copied_nodes = Galax_load_context.get_result load_context in
	    begin
	      set_parents (new_elem_node :> node) attr_children;
	      set_parents (new_elem_node :> node) elem_children;
	      (new_elem_node :> node) :: copied_nodes @ (load_xml_node_forest_from_sax load_context doc_stream)
	    end

	| OTSAX_endElement postorder ->
	    Galax_load_context.set_postorder load_context postorder;
	    []    (* Terminates recursion for the children of an element.
		     This gives the hand back to the calling element node creation. *)
	      
	| OTSAX_processingInstruction ((target,content),nodeid) ->
	    let new_pi_node = new galaxProcessingInstructionNode nodeid (new atomicString target) (new atomicString content) in
	    (new_pi_node :> node) :: (load_xml_node_forest_from_sax load_context doc_stream)

	| OTSAX_comment (c,nodeid) ->
	    let new_comment_node = new galaxCommentNode nodeid (new atomicString c) in
	    (new_comment_node :> node) :: (load_xml_node_forest_from_sax load_context doc_stream)

	| OTSAX_characters (s,nodeid) ->
	    let new_text_node = new galaxTextNode nodeid (new atomicString s) in
	    (new_text_node :> node) :: (load_xml_node_forest_from_sax load_context doc_stream)
	      
	| OTSAX_attribute a ->
	    let new_attr_node = (load_sax_xml_attribute load_context a) in
	    (new_attr_node :> node) :: (load_xml_node_forest_from_sax load_context doc_stream)
	      
	| OTSAX_atomicValue _ ->
	    raise (Query (Load_Error "Cannot load an atomic value to a node list"))

	| OTSAX_hole ->
	    raise (Query (Load_Error "Cannot load stream with a hole!"))
	      
	| OTSAX_startEncl
	| OTSAX_endEncl -> 
	    []
      end

(**************************)
(* Load a single document *)
(**************************)

let load_document_node_from_sax load_context doc_stream =
  (* Process the next event *)
  let xml_event = Cursor.cursor_next doc_stream in
  match xml_event.otse_desc with
  | OTSAX_startDocument ((xml_decl,_,base_uri),preorder) ->
      (* Get the document encoding *)    
      let document_encoding =
	match xml_decl with
	| None -> None
	| Some (_,enc,_) -> enc
      in
      (* Then process the rest of the document. *)
      let full_root_forest = load_xml_node_content_from_stream load_context doc_stream in
      let postorder = Galax_load_context.get_postorder load_context in
      let nodeid = Nodeid.merge_docorder_from_pre_post preorder postorder in
      let full_root_forest_node = List.map (fun x -> (x :> node)) full_root_forest in

      (* Then the node itself. *)
      let new_doc_node =
	new galaxDocumentNode
	  nodeid
	  base_uri
	  full_root_forest_node
	  document_encoding
      in
      begin
	set_parents (new_doc_node :> node) full_root_forest;
	let doc_node = (Item_Node (new_doc_node :> node)) in
	let copied_nodes = Galax_load_context.get_result load_context in
	let copied_nodes = List.map (fun x -> Item_Node x) copied_nodes in
	begin
	  load_fail_from_sax doc_stream;
	  doc_node :: copied_nodes
	end
      end

  | OTSAX_endDocument _ 
  | OTSAX_startElement _
  | OTSAX_endElement _
  | OTSAX_processingInstruction _
  | OTSAX_comment _
  | OTSAX_characters _
  | OTSAX_attribute _
  | OTSAX_atomicValue _ 
  | OTSAX_hole 
  | OTSAX_startEncl
  | OTSAX_endEncl ->
      raise (Query (Load_Error "Not a well-formed XML document"))

(*********************************)
(* Load the document node itself *)
(*********************************)

(* Load a data model instance from an XML typed ordered stream *)

(* Loading and building and index *)

let load_xml_value_from_typed_ordered_stream name_index_names typed_ordered_xml_stream =
  let load_context = Galax_load_context.build_load_context name_index_names in
  load_xml_item_forest_from_sax load_context typed_ordered_xml_stream

let load_xml_node_sequence_from_typed_ordered_stream name_index_names typed_ordered_xml_stream =
  let load_context = Galax_load_context.build_load_context name_index_names in
  load_xml_node_forest_from_sax load_context typed_ordered_xml_stream

let load_xml_document_from_typed_ordered_stream name_index_names typed_ordered_xml_stream =
  let load_context = Galax_load_context.build_load_context name_index_names in
  load_document_node_from_sax load_context typed_ordered_xml_stream

(* Registering the GalaxDM load functions as being the DM load functions *)

let _ =
  try 
  (* Register functions without index *)
    let fun0 = Galax_nodeid.new_docid in
    let fun1 = load_xml_value_from_typed_ordered_stream 
	Physical_name_index.no_name_indices in
    let fun2 = load_xml_node_sequence_from_typed_ordered_stream 
	Physical_name_index.no_name_indices in
    let fun3 = load_xml_document_from_typed_ordered_stream 
	Physical_name_index.no_name_indices in
    Physical_load.register_load_functions fun0 fun1 fun2 fun3;
  (* Registed functions with index *)
    let fun0 = Galax_nodeid.new_docid in
    let fun1 = load_xml_value_from_typed_ordered_stream in
    let fun2 = load_xml_node_sequence_from_typed_ordered_stream in
    let fun3 = load_xml_document_from_typed_ordered_stream in
    Physical_index_load.register_load_functions fun0 fun1 fun2 fun3
  with
  | e ->
      begin
	eprintf_error "  " e;
	Format.fprintf (!Conf.glx_err_formatter) "@."; 
      end


