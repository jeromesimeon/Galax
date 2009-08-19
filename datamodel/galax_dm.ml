(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_dm.ml,v 1.33 2007/08/23 21:01:32 simeon Exp $ *)

(* Module: Galax_dm
   Description:
     This module is a main memory implementation of the XQuery 1.0 and
     XPath 2.0 data model. It is based on direct tree, list-based,
     data structures.
*)

open Error

open Namespace_context
open Namespace_symbols

open Datatypes

open Xquery_common_ast

(* Generic data model *)

open Cursor
open Dm_types
open Dm_atomic
open Dm_step
open Dm

(* For the Galax data model *)

open Nodeid

(*************************************)
(* Some useful functions for updates *)
(*************************************)
   
(* Deep copy *)
   
(* Note:
     This hack is necessary to go around a cyclic dependency in
     loading depending on the data model, and the data model needing
     to do deep copy (for updates).
   - Jerome *)

let deep_copy_fun =
  ref (fun x -> raise (Query (Datamodel "Deep-copy function on Galax nodes not registered!")))

let register_deep_copy_fun f =
  deep_copy_fun := f

let deep_copy nodeid_context node =
  !deep_copy_fun nodeid_context node

(* Builds a new nodeid_context *)

let nodeid_context_for_insert previous_nodeid =
  match previous_nodeid with
  | (_,PrePostInt (docid,preorder,postorder)) ->
      let nodeid_context =
	Nodeid_context.build_nodeid_context (preorder+1) min_postorder
      in
      (docid,nodeid_context)
  | _ ->
      raise (Query (Datamodel "Galax node has wrong kind of nodeid!"))

let nodeid_context_for_replace replaced_nodeid =
  match replaced_nodeid with
  | (_,PrePostInt (docid,preorder,postorder)) ->
      let nodeid_context =
	Nodeid_context.build_nodeid_context preorder min_postorder
      in
      (docid,nodeid_context)
  | _ ->
      raise (Query (Datamodel "Galax node has wrong kind of nodeid!"))

(* auxilliary primitives for insert-into *)

let rec insert_content_into_attributes node_parent (node_list: node cursor) elem_attributes =
  match elem_attributes with
    | [] ->
        begin
	      let previous_nodeid = node_parent#docorder() in
	      let (docid,nodeid_context) = nodeid_context_for_insert previous_nodeid in
            (*	let new_node_list = deep_copy nodeid_context docid node_list in *)
          let new_node_list = Cursor.list_of_cursor "insert_content_into_attributes" node_list in (* no need for deep-copy, normalization takes care of this *)
	      let new_attribute_list =
	        List.map (fun x -> match x#node_kind() with AttributeNodeKind -> x#getAttributeNode() | _ -> 
	          raise (Query (Update_Error("Inserting a mix of attributes and elements is not allowed")))) new_node_list
	      in
	        (nodeid_context,new_attribute_list)
        end
    | att :: elem_attributes' ->
        let (nodeid_context,new_attribute_list) = insert_content_into_attributes node_parent node_list elem_attributes' in
          (nodeid_context,att::new_attribute_list)

let update_parent node_parent x =
  x#update_parent (node_parent :> node); x

let rec insert_content_into_children node_parent node_list node_children =
  begin
    let previous_nodeid = node_parent#docorder() in
    let (docid,nodeid_context) = nodeid_context_for_insert previous_nodeid in
    let new_node_list =
      Cursor.list_of_cursor "insert_content_into_children" (Cursor.cursor_map (update_parent node_parent) node_list)
    in
    (nodeid_context,new_node_list@node_children)
  end

(* auxilliary primitives for insert-into-before *)

let rec insert_content_into_children_before_aux node_parent node_list node_children location = 
  match node_children with
  | [] ->
      raise (Query (Update_Error ("1. Insert specifier not a child of the insert location!")))
  | last_node :: [] ->
      raise (Query (Update_Error ("2. Insert specifier not a child of the insert location!")))
  | previous_node :: current_node :: node_children' ->
      if (Dm_util.node_equal current_node location)
      then
	let previous_nodeid = previous_node#docorder() in
	let (docid,nodeid_context) = nodeid_context_for_insert previous_nodeid in
	let new_node_list =
	  Cursor.list_of_cursor
	    "insert_content_into_children_before_aux" (Cursor.cursor_map (update_parent node_parent) node_list)
	in
        (* no need for deep-copy, normalization takes care of this *)
	(nodeid_context, previous_node :: (new_node_list @ (current_node :: node_children')))
      else
	let (nodeid_context,new_node_list) = insert_content_into_children_before_aux node_parent node_list (current_node::node_children') location in
	(nodeid_context,previous_node::new_node_list)

let insert_content_into_children_before node_parent node_list node_children location = 
  match node_children with
  | [] ->
      raise (Query (Update_Error ("1. Insert specifier not a child of the insert location!")))
  | current_node :: node_children' ->
      if (Dm_util.node_equal current_node location)
      then
	let previous_nodeid = node_parent#docorder() in
	let (docid,nodeid_context) = nodeid_context_for_insert previous_nodeid in
	let new_node_list =
	  Cursor.list_of_cursor
	    "insert_content_into_children_before" (Cursor.cursor_map (update_parent node_parent) node_list)
	in
        (* no need for deep-copy, normalization takes care of this *)
	(nodeid_context, (new_node_list @ node_children))
      else
	let (nodeid_context,new_node_list) =
	  insert_content_into_children_before_aux node_parent node_list node_children location
	in
	(nodeid_context,new_node_list)

(* Attribute helpers *)
let compute_attribute_value ta ac av = 
  (* Case 1. if the attribute has typed untypedAtomic *)
  if (Namespace_symbols.rtype_equal ta Namespace_symbols.untypedAtomicsym)
  then [(new atomicUntyped(ac#getAtomicString()))]
    (* Case 2. otherwise *)
  else av

    
(******************)
(* The data model *)
(******************)

(* Galax nodes *)

class virtual galaxNode id = 
  object (self)
    inherit node

    method implementation () = "Galax"

    val mutable node_id : Galax_nodeid.galax_nodeid = id
    method nodeid () =
      match node_id with
      | (docid,pre,post) ->
	  (Galax_nodeid.galax_implemid,IntId(docid,pre))
    method docorder () =
      match node_id with
      |	(docid,pre,post) ->
	  (Galax_nodeid.galax_implemid,PrePostInt(docid,pre,post))

  (********)
  (* Axes *)
  (********)

    (* Note:
         Here is a pretty sensitive part of the code, as the way axis
         are implemented determins a alrge part of the performances of
         the system.
       - Jerome
     *)

    (* parent axis *)

    val mutable node_parent : (node option) = None
    method parent nto =
      match nto with
      | None ->
	  node_parent
      | Some (cx,nt) ->
	  let ntf x = eval_node_test_gen (self#get_access_ops_dm) cx Parent nt x in
	  Cursor.cursor_peek (Cursor.cursor_filter ntf (cursor_of_option node_parent))
    method update_parent (p : node) = node_parent <- (Some p)
    method reset_parent() = node_parent <- None

  end

and galaxDocumentNode id docuri kids optenc = 
  object (self)
    inherit document (docuri) as superdoc
    inherit galaxNode (id) as super

    val mutable doc_uri : atomicAnyURI option ref = docuri
    val mutable doc_encoding : Encoding.encoding option = optenc

    val mutable node_children : (node list) = kids
    method children nto =
      match nto with
      | None ->
	  cursor_of_list node_children
      | Some (cx,nt) ->
	  let ntf x = eval_node_test_gen (self#get_access_ops_dm) cx Child nt x in
	  Cursor.cursor_filter ntf (cursor_of_list node_children)

    method document_uri () = !doc_uri

    method detach detach_child =
      begin
	let old_children = node_children in 
	let new_children =
	  List.filter (fun c -> not(Dm_util.node_equal c detach_child)) old_children
	in
	node_children <- new_children
      end

    method delete delete_child =
      self#detach delete_child
	(* NOTE: Delete is implemented as detach for now... This means
	   further access to deleted nodes may still be possible from
	   e.g., a global variable. *)

    method insert insert_content insert_location =
      match Cursor.cursor_peek insert_content with
      | None -> ()
      | Some node ->
	  begin
	    match node#node_kind() with
	    | DocumentNodeKind ->
		raise (Query (Update_Error("Cannot insert a document node inside another node")))
	    | AttributeNodeKind ->
		raise (Query (Update_Error("Cannot insert an attribute node inside a document node")))
	    | _ ->
		begin
		  let old_children = node_children in
		  let (nodeid_context,new_children) =
		    insert_content_into_children_before self insert_content old_children insert_location
		  in
		  node_children <- new_children
		end
	  end

    method insert_first insert_content =
      match Cursor.cursor_peek insert_content with
      | None -> ()
      | Some node ->
	  begin
	    match node#node_kind() with
	    | DocumentNodeKind ->
		raise (Query (Update_Error("Cannot insert a document node inside another node")))
	    | AttributeNodeKind ->
		raise (Query (Update_Error("Cannot insert an attribute node inside a document node")))
	    | _ ->
		begin
		  let old_children = node_children in
		  let (nodeid_context,new_children) =
		    insert_content_into_children self insert_content old_children
		  in
		  node_children <- new_children
		end
	  end
        
    method replace replace_content replaced_node =
      raise (Query (Prototype "REPLACE IS NOT SUPPORTED"))
  end

and galaxElementNode id elemuri qn nsl atl kids nf ta ev =
  object (self)
    inherit element elemuri as superelem
    inherit galaxNode id as super

    val mutable elem_name    	= qn
    val mutable elem_value   	= ev
    val mutable elem_attributes = atl
    val namespace_env           = nsl

    val mutable node_children : (node list) = kids
    method children nto =
      match nto with
      | None ->
	  cursor_of_list node_children
      | Some (cx,nt) ->
	  let ntf x = eval_node_test_gen (self#get_access_ops_dm) cx Child nt x in
	  Cursor.cursor_filter ntf (cursor_of_list node_children)

    val mutable node_type = ta
    method node_type()    = node_type

    val nilled = nf
    method nilled() = nilled

    method node_name ()    = Some (new atomicQName elem_name)

    method elemName() = elem_name

    method namespace_environment() = namespace_env
    method attributes nto :attribute cursor =
      match nto with
      | None ->
	  cursor_of_list elem_attributes
      | Some (cx,nt) ->
	  let ntf x = eval_node_test_gen ((self :> node)#get_access_ops_dm) cx Attribute nt x in
	  Cursor.cursor_map (fun x -> x#getAttributeNode()) (Cursor.cursor_filter ntf (Cursor.cursor_map (fun x -> (x :> node)) (cursor_of_list elem_attributes)))

    method typed_value () = 

    (* From XQuery: 

       For an element node, the relationship between typed value and
       string value depends on the node's type annotation, as follows:

       1. If the type annotation is xs:untyped or denotes a complex
          type with mixed content (including xs:anyType), then the
          typed value of the node is equal to its string value, as an
          instance of xs:untypedAtomic.

       2. If the type annotation denotes a simple type or a complex
          type with simple content, then the typed value of the node
          is derived from its string value and its type annotation in
          a way that is consistent with schema validation.

       3. If the type annotation denotes a complex type with empty
          content, then the typed value of the node is the empty
          sequence and its string value is the zero-length string.

       4. If the type annotation denotes a complex type with
          element-only content, then the typed value of the node is
          undefined. The fn:data function raises a type error
          [err:XP0007] when applied to such a node.
    *)

      (* Case 1. above: Need to check for mixed content here: *)
      if (ta = Namespace_symbols.untypedsym || ta = Namespace_symbols.anytype) 
      then cursor_of_singleton(new atomicUntyped(self#string_value()))
      (* Case 2. above : NOT IMPLEMENTED COMPLETELY *)
      else if (not (elem_value = []))
      then cursor_of_list elem_value
      (* Case 3. *)
      else if cursor_is_empty (self#children None) 
      then cursor_empty()
      (* Case 4. NOT IMPLEMENTED CORRECTLY *)
      else cursor_of_singleton (new atomicUntyped (self#string_value()))

    method export_typed_value () = elem_value

    method insert insert_content insert_location =
      match Cursor.cursor_peek insert_content with
      | None -> ()
      | Some node ->
	  begin
	    match node#node_kind() with
	    | DocumentNodeKind ->
		raise (Query (Update_Error("Cannot insert a document node inside another node")))
	    | AttributeNodeKind ->
		begin
		  let old_attributes = elem_attributes in
		  let (nodeid_context,new_attributes) =
		    (* Ignore the insert location in the case of attributes - Jerome *)
		    insert_content_into_attributes self insert_content old_attributes
		  in
		  begin
		    (* renumber nodeid_context first_following_node; *)
		    elem_attributes <- new_attributes
		  end
		end
	    | _ ->
		begin
		  let old_children = node_children in
		  let (nodeid_context,new_children) =
		    insert_content_into_children_before self insert_content old_children insert_location
		  in
		  node_children <- new_children
		end
	  end

    method insert_first insert_content =
      match Cursor.cursor_peek insert_content with
      | None -> ()
      | Some node ->
	  begin
	    match node#node_kind() with
	    | DocumentNodeKind ->
		raise (Query (Update_Error("Cannot insert a document node inside another node")))
	    | AttributeNodeKind ->
		begin
		  let old_attributes = elem_attributes in
		  let (nodeid_context,new_attributes) =
		    (* Ignore the insert location in the case of attributes - Jerome *)
		    insert_content_into_attributes self insert_content old_attributes
		  in
		  begin
		    (* renumber nodeid_context first_following_node; *)
		    elem_attributes <- new_attributes
		  end
		end
	    | _ ->
		begin
		  let old_children = node_children in
		  let (nodeid_context,new_children) =
		    insert_content_into_children self insert_content old_children
		  in
		  node_children <- new_children
		end
	  end

    method detach detach_child =
      begin
	let old_children = node_children in
	let old_attributes = elem_attributes in
	let new_children =
	  List.filter (fun c -> not(Dm_util.node_equal c detach_child)) old_children
	in
	let new_attributes =
	  List.filter
	    (fun c -> not(Dm_util.node_equal (c :> node) detach_child))
	    old_attributes
	in
	begin
	  node_children <- new_children;
	  elem_attributes <- new_attributes
	end
      end

    method delete delete_child =
      self#detach delete_child
	(* NOTE: Delete is implemented as detach for now... This means
	   further access to deleted nodes may still be possible from
	   e.g., a global variable. *)

    method replace replace_content replaced_node =
(*       let print_id  nd = match nd#nodeid() with *)
(*           impl, IntId(di, pi) -> Printf.printf "%d %d %d\n" impl di pi *)
(*         | impl, IntPairId(di, _) -> Printf.printf "%d %d \n" impl di *)
(*       in *)
      try
        self#insert (Cursor.cursor_map (fun nd -> (nd#getElementNode() :> node)) replace_content) replaced_node; 
        self#delete (replaced_node :> node)
      with
        | Query (Datamodel _) -> raise (Query (Update_Error "The nodes in the replace list of an element must be element nodes."))
        | Stream.Failure -> ()     
            
                    
    method replace_value t =
	  (* replace all children with a new text node *)
	  t#update_parent (self :> node);
	  List.iter (fun c -> c#reset_parent()) node_children; (* for all former children, unset the parent *)
	  node_children <- [t] (* NB: the attributes should not be affected *)        

    method rename newName = elem_name <- newName
      
  end

and galaxAttributeNode id qn (ac : atomicString) ta av = 
object (self)
    inherit attribute
    inherit galaxNode (id) as super

    val mutable attr_name = qn

   (* From the XQuery Data Model [6.3.4 Construction from a PSVI]:

      The typed-value is calculated as follows:

        * If the attribute is of type xs:untypedAtomic: its
          typed-value is its dm:string-value as an xs:untypedAtomic.

        * Otherwise, a sequence of zero or more atomic values as
          described in 3.3.1.2 Typed Value Determination. The
          relationship between the type-name, typed-value, and
          string-value of an attribute node is consistent with XML
          Schema validation.
    *)

    val mutable attr_value : atomicValue list = compute_attribute_value ta ac av

    val node_type = ta
    method node_type() = node_type

    method node_name() = Some (new atomicQName qn)
    method attrName() : rattr_symbol = attr_name

    method typed_value () = cursor_of_list attr_value
    method export_typed_value () = attr_value

    val mutable string_value = ac#getAtomicString()
    method string_value () = string_value

    method replace_value txt_content =      
      string_value <- txt_content#string_value();
      attr_value <-  [new atomicUntyped(string_value)] (* this might need to be fixed *)
        
    (*       let replace_list = Cursor.list_of_cursor "" replace_content in *)
    (* 	    attr_value <- replace_list; *)
    (* 	    string_value <- List.fold_left (^) "" *)
    (* 	      (List.map (fun x -> x#erase_atomic_value ()) replace_list) *)
          
    method rename nm = attr_name <- nm
      
  end

and galaxTextNode id astr = 
  object
    inherit text
    inherit galaxNode (id) as super
    val mutable text_content : atomicString = astr

  (* From XQuery: For text and document nodes, the typed value of the
     node is the same as its string value, as an instance of the type
     xs:untypedAtomic. *)

    method string_value() = text_content#getAtomicString()

    method replace_value replace_content =
      text_content <- new atomicString (replace_content#string_value())

  end

and galaxCommentNode id astr = 
  object
    inherit comment
    inherit galaxNode (id) as super
    val mutable comment_content : atomicString = astr

    (* The typed value of a comment or processing instruction node is
       the same as its string value. It is an instance of the type
       xs:string. *)
    method content () = comment_content
    method string_value () = comment_content#getAtomicString()

    method replace_value replace_content =
      comment_content <- new atomicString (replace_content#string_value())

  end

and galaxProcessingInstructionNode id target vl = 
  object
    inherit processingInstruction
    inherit galaxNode (id) as super

    (* The typed value of a comment or processing instruction node is
       the same as its string value. It is an instance of the type
       xs:string. *)
    val mutable pi_target : atomicString = target
    val mutable pi_value  : atomicString = vl 

    method node_name () =  Some (new atomicQName (anon_symbol (Namespace_names.NSDefaultElementPrefix,Namespace_names.NSUri "", pi_target#string_value())))

    method string_value () = pi_value#string_value()
    method target() = (pi_target)
    method content() =(pi_value)

    method replace_value replace_content =
      pi_value <- new atomicString (replace_content#string_value())

    method rename nm = 
      pi_target <- new atomicString (Namespace_symbols.anon_prefix_string nm)

  end

