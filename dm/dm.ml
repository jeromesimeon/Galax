(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm.ml,v 1.55 2007/08/23 21:01:32 simeon Exp $ *)

(* Module: Dm
   Description:
   Galax's abstract data model interface.
*)

open Error

open Namespace_builtin
open Namespace_context
open Namespace_symbols

open Decimal
open AnyURI

open Datatypes
open Datatypes_util

open Cursor

open Xquery_common_ast
open Xquery_type_core_ast
open Xquery_algebra_ast

open Dm_types
open Dm_atomic
open Dm_step


(**********)
(* Errors *)
(**********)

let raise_document_element_singleton () =
  raise (Query (Prototype "document node does not contain a singleton children element"))

let xmlns_base = APNameTest (anon_symbol xml_base)

(********************************)
(* Generic node class hierarchy *)
(********************************)

class virtual node =
object (self)
  (* Each node must provide a means to identify the implementation it
     comes from *)
  method virtual implementation : unit -> string

  val access_ops_dm : node access_ops =
    { get_node_kind      	   =
        (fun n -> n#node_kind());
      get_elem_node_name 	   =
        (fun n -> (n#getElementNode())#elemName());
      get_attr_node_name 	   =
        (fun n -> (n#getAttributeNode())#attrName());
      get_elem_node_name_with_type =
        (fun n -> let en = n#getElementNode() in (en#elemName(),en#node_type()));
      get_attr_node_name_with_type =
        (fun n -> let en = n#getAttributeNode() in (en#attrName(),en#node_type()));
      get_document_node_children   =
        (fun n -> (n#getDocumentNode())#children None);
      get_element_node_children    =
        (fun n -> (n#getElementNode())#children None);
      get_single_element_node      =
        (fun n ->
	  let c = n#children None in
	  let ce = Cursor.cursor_filter (fun x -> x#node_kind() = ElementNodeKind) c in
	  let ce1 =
            try
              Cursor.cursor_get_singleton ce
            with
            | _ -> raise_document_element_singleton ()
	  in
	  ce1);
      get_pi_target                =
        (fun n ->
	  let pin = n#getProcessingInstructionNode() in
	  let target = pin#target() in
	  target#getAtomicString())	
    }

  method get_access_ops_dm = access_ops_dm

  (* Downcasts *)

  method virtual getDocumentNode  : unit -> document
  method virtual getElementNode   : unit -> element
  method virtual getAttributeNode : unit -> attribute
  method virtual getTextNode      : unit -> text
  method virtual getProcessingInstructionNode : unit -> processingInstruction
  method virtual getCommentNode               : unit -> comment

  method virtual base_uri  : unit -> atomicAnyURI option ref
  method virtual node_kind : unit -> _NodeKind

  (* The following functions should all be coalesced into node_name *)
  method virtual node_name    : unit -> atomicQName option

  method virtual string_value        : unit -> string
  method virtual typed_value         : unit -> atomicValue cursor

  (* Axes *)
  method virtual parent     : (cxschema option * anode_test) option -> node option
  method virtual children   : (cxschema option * anode_test) option -> node cursor
  method virtual attributes : (cxschema option * anode_test) option -> attribute cursor

  method self nto =
    match nto with
    | None ->
	(Cursor.cursor_of_singleton (self :> node))
    | Some (cx,nt) ->
	let ntf x = eval_node_test_gen access_ops_dm cx Descendant_or_self nt x in
	Cursor.cursor_filter ntf (Cursor.cursor_of_singleton (self :> node))

  method descendant_or_self nto =
    match nto with
    | None ->
	Cursor_descendant.cursor_descendant access_ops_dm
	  (Cursor.cursor_of_singleton (self :> node))
    | Some (cx,nt) ->
	let ntf x = eval_node_test_gen access_ops_dm cx Descendant_or_self nt x in
	Cursor.cursor_filter ntf (Cursor_descendant.cursor_descendant access_ops_dm
	  (Cursor.cursor_of_singleton (self :> node)))

  (* descendant axis  *)

  (* Note:
       03/03/2004. descendant is now implemented using
       descendant-or-self.
       - Jerome
   *)
  method descendant nto =
    cursor_map_concat (fun n -> n#descendant_or_self nto) (self#children None)

  (* ancestor-or-self axis *)

  (* Note:
       03/03/2004. ancestor-or-self requires to accumulate all of
       the ancestors, as it should return things in document
       order. This should not be a big problem as the materialized
       list will be at most of the depth of the tree.
     - Jerome
   *)

  method ancestor_or_self nto =
    let all_ancestors =
      let rec compute_ancestor_or_self current previous =
	let new_previous =
	  match nto with
	  | None -> (current :> node) :: previous
	  | Some (cx,nt) ->
	      if (eval_node_test_gen (self#get_access_ops_dm) cx Ancestor_or_self nt current)
	      then
		(current :> node) :: previous
	      else previous
	in
	let parent = current#parent None in
	match parent with
	| None -> new_previous
	| Some p -> compute_ancestor_or_self p new_previous
      in
      compute_ancestor_or_self (self :> node) []
    in
    cursor_of_list all_ancestors

  (* ancestor axis *)

  (* Note:
       03/03/2004. ancestor is now implemented using
       ancestor-or-self.
       - Jerome
   *)
  method ancestor nto =
    let parent = self#parent None in
    match parent with
    | None -> cursor_empty()
    | Some p -> p#ancestor_or_self nto

  (* full axis feature, following and preceding -- Philippe *)
  method following (nto: (cxschema option * anode_test) option) : node cursor =  
    let rec compute_foll n =
      match n#parent None with
      | Some p ->
	  let siblings =
	    Cursor.cursor_filter 
	      (fun n' -> Nodeid.docorder_follows (n'#docorder()) (n#docorder())) 
	      (p#children None)
	  in
	  let first_followings =
	    Cursor.cursor_map_concat (fun n'' -> n''#descendant_or_self nto) siblings
	  in
	  Cursor.cursor_append  first_followings (compute_foll p)
      | None -> Cursor.cursor_empty ()
    in
    compute_foll (self :> node)

  method preceding (nto: (cxschema option * anode_test) option) : node cursor =
    let ancestors = self#ancestor_or_self None in
    let get_precs n' = 
      match n'#parent None with
      | Some p ->
	  let siblings =
	    Cursor.cursor_filter 
	      (fun n'' -> Nodeid.docorder_precedes (n''#docorder()) (n'#docorder())) 
	      (p#children None)
	  in
	  Cursor.cursor_map_concat (fun i -> i#descendant_or_self nto) siblings
      | None -> Cursor.cursor_empty ()
    in
    Cursor.cursor_map_concat get_precs ancestors

  method virtual nodeid    : unit -> Nodeid.nodeid
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

and virtual document =
    fun init_base_uri ->
object (self)
  inherit node
  method getDocumentNode() : document  = (self :> document)
  method getElementNode()  : element   = raise (Query (Datamodel "Not a element node"))
  method getAttributeNode(): attribute = raise (Query (Datamodel "Not a attribute node"))
  method getTextNode()     : text = raise (Query (Datamodel "Not a text node"))
  method getProcessingInstructionNode() : processingInstruction = raise (Query (Datamodel "Not a processing-instruction node"))
  method getCommentNode() : comment = raise (Query (Datamodel "Not a comment node"))

  val base_uri = init_base_uri
  method base_uri () = base_uri

  method node_kind () = DocumentNodeKind

  method node_name () : atomicQName option = None 

  method parent nto   	: node option = None
  method attributes nto : attribute cursor = cursor_empty ()

  (* From XQuery: For text and document nodes, the typed value of the node is the
     same as its string value, as an instance of the type
     xs:untypedAtomic. The string value of a document node is formed
     by concatenating the string values of all its descendant text
     nodes, in document order. *)
  method string_value() =
    let all_descendants = self#descendant_or_self None in
    let text_nodes_descendants =
      Cursor.cursor_filter (fun n -> n#node_kind() = TextNodeKind) all_descendants
    in
    let string_content_from_node_descendant =
      Cursor.cursor_map (fun n -> (n#getTextNode())#string_value()) text_nodes_descendants
    in
    let all_strings = Cursor.list_of_cursor "Dm.document.string_value()" string_content_from_node_descendant in
    String.concat "" all_strings

  (* PSVI accessors *)

  method typed_value() = cursor_of_singleton (new atomicUntyped(self#string_value()))

  (* Accessors specific to document nodes *)

  (* Infoset accessors *)

  method virtual document_uri : unit -> atomicAnyURI option

  (* Updates *)

  method replace avl =
    raise (Query (Update_Error ("[Update] Replace not supported on document nodes")))
  method replace_value avl =
    raise (Query (Update_Error ("[Update] Replace value not supported on document nodes")))
  method rename s = 
    raise (Query (Update_Error ("[Update] Rename not supported on document nodes")))

  (* Added for convenience *)
  method node_lang () = None
end

and virtual element = 
  fun init_base_uri ->
object (self)
  inherit node
      (* method virtual lang : unit -> xs_string opation *)
  method getElementNode() = (self :> element)
  method getDocumentNode() : document  = raise (Query (Datamodel "Not a document node"))
  method getAttributeNode(): attribute = raise (Query (Datamodel "Not a attribute node"))
  method getTextNode()     : text = raise (Query (Datamodel "Not a text node"))
  method getProcessingInstructionNode() : processingInstruction = raise (Query (Datamodel "Not a processing-instruction node"))
  method getCommentNode() : comment = raise (Query (Datamodel "Not a comment node"))

  val base_uri = init_base_uri
  method base_uri () =
    begin
      match !init_base_uri with
      | None ->
	  begin
	    match (self#parent None) with
	    | None -> ref None
	    | Some n -> n#base_uri()
	  end
      | Some _ ->
	  begin
	    if (Dm_atomic_util.is_absolute_atomicAnyURI base_uri)
	    then base_uri
	    else
	      let basebase =
		begin
		  match (self#parent None) with
		  | None -> ref None
		  | Some n -> n#base_uri()
		end
	      in
	      Dm_atomic_util.resolve_atomicAnyURI basebase base_uri
	  end
    end

  method node_kind()           = ElementNodeKind
  method has_element_content() =
    (cursor_for_all (fun n -> not(n#node_kind() = TextNodeKind)) (self#children None))

  method string_value() =
    let all_descendants = self#descendant_or_self None in
    let text_nodes_descendants =
      Cursor.cursor_filter (fun n -> n#node_kind() = TextNodeKind) all_descendants
    in
    let string_content_from_node_descendant =
      Cursor.cursor_map (fun n -> (n#getTextNode())#string_value()) text_nodes_descendants
    in
    let all_strings = Cursor.list_of_cursor "Dm.element.string_value()" string_content_from_node_descendant in
    String.concat "" all_strings

  method virtual node_type           : unit -> rtype_symbol
  method virtual export_typed_value  : unit -> atomicValue list

  method virtual elemName   : unit -> relem_symbol
  method virtual namespace_environment : unit -> nsenv

  method virtual nilled : unit -> nilled

  (* Added for convenience *)
  method node_lang () = 
    let node_lang = APNameTest (anon_symbol Namespace_builtin.xml_lang) in
    let attrs = self#attributes (Some (None,node_lang)) in
    try
      let ab = cursor_next attrs in
      Some (ab#string_value())
    with
    | Stream.Failure ->
	begin
	  match self#parent None with
	  | None ->
	      None
	  | Some n ->
	      n#node_lang()
	end

  method virtual rename        : xs_QName -> unit

end

and virtual attribute = 
object (self)
  inherit node
  method getAttributeNode() = (self :> attribute)
  method getDocumentNode() : document  = raise (Query (Datamodel "Not a document node"))
  method getElementNode()  : element   = raise (Query (Datamodel "Not a element node"))
  method getTextNode()     : text = raise (Query (Datamodel "Not a text node"))
  method getProcessingInstructionNode() : processingInstruction = raise (Query (Datamodel "Not a processing-instruction node"))
  method getCommentNode() : comment = raise (Query (Datamodel "Not a comment node"))

  method base_uri () : atomicAnyURI option ref =
    match (self#parent None) with
    | None -> ref None
    | Some e -> e#base_uri()

  method node_kind () = AttributeNodeKind
  method children nto : node cursor = cursor_empty()
  method attributes nto : attribute cursor = cursor_empty ()
  method virtual attrName : unit -> rattr_symbol

  method virtual node_type           : unit -> rtype_symbol
  method virtual export_typed_value  : unit -> atomicValue list

  (* Updates *)

  method delete n =
     raise (Query (Update_Error ("Cannot delete into an attribute node")))
  method detach n =
     raise (Query (Update_Error ("Cannot detach into an attribute node")))
  method insert nl nopt =
     raise (Query (Update_Error ("Cannot insert into an attribute node")))
  method insert_first nl =
     raise (Query (Update_Error ("Cannot insert into an attribute node")))
  method replace nl nopt =
     raise (Query (Update_Error ("Cannot replace into an attribute node")))
  method virtual rename        : xs_QName -> unit	 

  (* Added for convenience *)
  method node_lang () =
    begin
      match self#parent None with
      | None -> None
      | Some n -> n#node_lang()
    end
end
      
and virtual text =
object (self)
  inherit node
  method getTextNode() = (self :> text)
  method getDocumentNode() : document  = raise (Query (Datamodel "Not a document node"))
  method getElementNode()  : element   = raise (Query (Datamodel "Not a element node"))
  method getAttributeNode(): attribute = raise (Query (Datamodel "Not a attribute node"))
  method getProcessingInstructionNode() : processingInstruction = raise (Query (Datamodel "Not a processing-instruction node"))
  method getCommentNode() : comment = raise (Query (Datamodel "Not a comment node"))

  method base_uri () = 
    match (self#parent None) with
    | None -> ref None
    | Some e -> e#base_uri()

  method node_kind () = TextNodeKind
  method node_name () : atomicQName option = None 

  method children nto   : node cursor = cursor_empty()
  method attributes nto : attribute cursor = cursor_empty ()

  (* PSVI accessors *)
  method typed_value() = cursor_of_singleton (new atomicUntyped(self#string_value()))

  (* Updates *)
  method delete n =
     raise (Query (Update_Error ("Cannot delete into a text node")))
  method detach n =
     raise (Query (Update_Error ("Cannot detach into a text node")))
  method insert nl nopt =
    raise (Query (Update_Error ("Cannot insert into a text node")))
  method insert_first nl =
    raise (Query (Update_Error ("Cannot insert into a text node")))
  method replace nl n =
    raise (Query (Update_Error ("Cannot replace into a text node")))
  method rename s =
    raise (Query (Update_Error ("Cannot rename a text node")))

  (* Added for convenience *)
  method node_lang () =
    begin
      match self#parent None with
      | None -> None
      | Some n -> n#node_lang()
    end
end

and virtual comment = 
object (self)
  inherit node
  method getCommentNode() = (self :> comment)
  method getDocumentNode() : document  = raise (Query (Datamodel "Not a document node"))
  method getElementNode()  : element   = raise (Query (Datamodel "Not a element node"))
  method getAttributeNode(): attribute = raise (Query (Datamodel "Not a attribute node"))
  method getTextNode()     : text = raise (Query (Datamodel "Not a text node"))
  method getProcessingInstructionNode() : processingInstruction = raise (Query (Datamodel "Not a processing-instruction node"))

  method base_uri () = 
      match (self#parent None) with
      |	None -> ref None
      |	Some e -> e#base_uri()

  method node_kind () = CommentNodeKind
  method node_name () : atomicQName option = None 

  method children nto   : node cursor = cursor_empty()
  method attributes nto : attribute cursor = cursor_empty ()

  (* PSVI accessors *)
  method typed_value() = cursor_of_singleton (new atomicString(self#string_value()))

  (* Updates *)

  method delete n =
     raise (Query (Update_Error ("Cannot delete into a comment node")))
  method detach n =
     raise (Query (Update_Error ("Cannot detach into a comment node")))
  method insert nl nopt =
    raise (Query (Update_Error ("Cannot insert into a comment node")))
  method insert_first nl =
    raise (Query (Update_Error ("Cannot insert into a comment node")))
  method replace nl n =
    raise (Query (Update_Error ("Cannot replace into a comment node")))
  method rename s =
    raise (Query (Update_Error ("Cannot rename a comment node")))

  (* Added for convenience *)
  method node_lang () =
    begin
      match self#parent None with
      | None -> None
      | Some n -> n#node_lang()
    end
end

and virtual processingInstruction = 
object (self)
  inherit node
  method getProcessingInstructionNode() = (self :> processingInstruction)
  method getDocumentNode() : document  = raise (Query (Datamodel "Not a document node"))
  method getElementNode()  : element   = raise (Query (Datamodel "Not a element node"))
  method getAttributeNode(): attribute = raise (Query (Datamodel "Not a attribute node"))
  method getTextNode()     : text = raise (Query (Datamodel "Not a text node"))
  method getCommentNode() : comment = raise (Query (Datamodel "Not a comment node"))

  method base_uri () =
      match (self#parent None) with
      |	None -> ref None
      |	Some e -> e#base_uri()

  method node_kind () = ProcessingInstructionNodeKind

  method children nto : node cursor = cursor_empty()
  method attributes nto : attribute cursor = cursor_empty ()
  method virtual content : unit -> atomicString (* An NCName *)
  method virtual target : unit -> atomicString (* An NCName *)

  (* PSVI accessors *)
  method typed_value() =  cursor_of_singleton ((self#content()))

  (* Updates *)

  method delete n =
     raise (Query (Update_Error ("Cannot delete into a processing-instruction node")))
  method detach n =
     raise (Query (Update_Error ("Cannot detach into a processing-instruction node")))
  method insert nl nopt =
    raise (Query (Update_Error ("Cannot insert into a processing-instruction node")))
  method insert_first nl =
    raise (Query (Update_Error ("Cannot insert into a processing-instruction node")))
  method replace nl n =
    raise (Query (Update_Error ("Cannot replace into a processing-instruction node")))

  method virtual rename        : xs_QName -> unit

  (* Added for convenience *)
  method node_lang () =
    begin
      match (self#parent None) with
      | None -> None
      | Some n -> n#node_lang()
    end
end


