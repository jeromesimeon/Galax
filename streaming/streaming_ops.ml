(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: streaming_ops.ml,v 1.26 2007/10/16 01:25:35 mff Exp $ *)

(* Module: Streaming_ops
   Description:
     Some basic operations on XML streams.
*)

open Error

open Namespace_names
open Namespace_symbols
open Namespace_resolve
open Namespace_context

open Streaming_types
open Streaming_util


(********************)
(* The empty stream *)
(********************)

let empty_xml_stream          () = Cursor.cursor_of_list []
let empty_typed_xml_stream    () = Cursor.cursor_of_list []

(***************************************)
(* Validity checks on stream contents  *)
(***************************************)

let check_valid_processing_instruction target pi_content = 
  let target'= Datatypes_util.ncname_of_untyped target in 
  if (String.lowercase(target') = "xml") then 
    raise (Query(Constructor_Error("Invalid processing-instruction target: contains 'xml'")))
  else if (Regularexp.matches pi_content "\\?>" "") then 
    raise (Query(Constructor_Error("Invalid processing-instruction content: contains '?>'")))
  else (target, pi_content)

let check_valid_comment comment_content = 
  if (Regularexp.matches comment_content "(--)|(-$)" "") then 
    raise (Query(Constructor_Error("Invalid comment content: contains '--' or '-' at end of comment")))
  else true

(***********************)
(* Discarding a stream *)
(***********************)

(* Discards the current XML subtree *)

let rec discard_xml_stream xml_stream =
  try
    while true do
      ignore(Cursor.cursor_next xml_stream)
    done
  with
  | Stream.Failure -> ()

(* Discards the current resolved XML subtree *)

let rec discard_resolved_xml_stream resolved_xml_stream =
  try
    while true do
      ignore(Cursor.cursor_next resolved_xml_stream)
    done
  with
  | Stream.Failure -> ()

(* Discards the current typed XML subtree *)

let rec discard_typed_xml_stream typed_xml_stream =
  try
    while true do
      ignore(Cursor.cursor_next typed_xml_stream)
    done
  with
  | Stream.Failure -> ()

 (***********************)
 (* Removing text nodes *)
 (***********************)

 (**
  * Code to merge text nodes in typed SAX streams
  **)
let merge_xml_text_nodes_in_typed_stream typed_stream = 
  let merge_text_nodes () = 
    (* function for merging adjacent text nodes *)
    let rec next_event_discard previous tsa fi =
      match (Cursor.cursor_peek typed_stream) with
      | Some {se_desc = (SAX_characters (s))} ->
	  begin
	    ignore(Cursor.cursor_next typed_stream);
	    next_event_discard (previous ^ s) tsa fi
	  end
      | _ ->
	  Some (fmkatse_event (SAX_characters previous) tsa fi)
    in
    try
      let e = Cursor.cursor_next typed_stream in
      match e.se_desc with
      | SAX_characters s  -> next_event_discard s e.se_annot e.se_loc
      | _ -> Some e
    with
    | Stream.Failure ->
	None
  in
  Cursor.cursor_of_function merge_text_nodes


 (*******************************************************)
 (* Conversion between well-formed and resolved streams *)
 (*******************************************************)

 (* Turns a well-formed XML stream into an resolved one *)

let resolve_attribute ts_context attribute =
  match attribute with
  | (attr_uqname, _, attr_sym_ref, _) ->
      begin
	match !attr_sym_ref with
        (* It's already been resolved, do nothing *)
	| Some _ -> ()
	(* Awaiting resolution *)
	| None ->
	    (* Look up current namespace environment *)
	    let in_scope_nsenv = Resolve_stream_context.get_nsenv ts_context in
	    let rattr_sym = Resolve_stream_context.resolve_attribute_name ts_context in_scope_nsenv attr_uqname in
	    attr_sym_ref := Some rattr_sym
      end

let resolve_attributes ts_context attributes =
  begin
    (* Resolve the attributes *)
    List.iter (resolve_attribute ts_context) attributes;
    (* Check for duplicates *)
    Streaming_util.check_duplicate_attributes attributes
  end

let update_resolved_element_content resolved_element_content esym nsenv =
  resolved_element_content := Some (esym,nsenv)

let resolved_of_well_formed_event ts_context event =
  begin
    match event.se_desc with
    | SAX_startElement (elem_uqname,sax_attributes,has_element_content,special_attributes,base_uri,relem_desc,rtype) ->
	(* First, extract the namespace attributes *)
	let new_nss = Streaming_util.bindings_of_special_attributes !special_attributes
	in
	(* Second, update the namespace environment *)
	let in_scope_nsenv =
	  begin
	    Resolve_stream_context.push_ns_bindings ts_context new_nss;
	    Resolve_stream_context.get_nsenv ts_context
	  end
	in
	(* Third, resolve the element QName using that new environment *)
	let relem_sym,default =
	  Resolve_stream_context.resolve_element_name ts_context in_scope_nsenv elem_uqname
	in
	let in_scope_nsenv =
	  if default then patch_bindings in_scope_nsenv Namespace_builtin.default_built_in_namespaces else in_scope_nsenv
	in
	begin
	  resolve_attributes ts_context sax_attributes;
	  update_resolved_element_content relem_desc relem_sym in_scope_nsenv
	end
    | SAX_endElement ->
	Resolve_stream_context.pop_nsenv ts_context
    | SAX_attribute sax_xml_attribute ->
	resolve_attribute ts_context sax_xml_attribute
    | SAX_processingInstruction _
    | SAX_comment _
    | SAX_characters _
    | SAX_startDocument _
    | SAX_endDocument
    | SAX_atomicValue _
    | SAX_hole
    | SAX_startEncl
    | SAX_endEncl -> ()
  end

let resolve_event_wrap ts_context xml_stream =
  try
    let next_event = Cursor.cursor_next xml_stream in
    resolved_of_well_formed_event ts_context next_event;
    Some next_event
  with
  | Stream.Failure ->
      None

let next_event_resolved_of_well_formed_xml_stream ts_context xml_stream n =
  resolve_event_wrap ts_context xml_stream

let resolve_xml_stream xml_stream =
  let ts_context = Resolve_stream_context.build_ts_context () in
  Cursor.cursor_of_function (next_event_resolved_of_well_formed_xml_stream ts_context xml_stream)


(* Turns a resolved XML stream back to one with prefixes *)

let prefix_attribute nsenv (rsym,content) =
  let uqname = rattr_uname nsenv rsym in
  (uqname,content)

let prefix_attributes nsenv resolved_attributes =
  List.map (prefix_attribute nsenv) resolved_attributes

let recreate_ns_binding new_bindings x =
  begin
    match x with
    | (NSDefaultFunctionPrefix,_) | (NSWildcardPrefix,_) | (_,NSWildcardUri) |
      (NSInterfacePrefix _,_) | (NSServerPrefix _,_) -> ()
    | (NSDefaultElementPrefix,NSUri uri) -> new_bindings := ((NSDefaultElementPrefix,"xmlns"),uri) :: !new_bindings
    | (NSPrefix ncname,NSUri uri) ->  new_bindings := ((NSPrefix "xmlns",ncname),uri) :: !new_bindings
  end

let recreate_ns_bindings delta_bindings =
  let new_bindings = ref [] in
  begin
    List.iter (recreate_ns_binding new_bindings) delta_bindings;
    !new_bindings
  end

let prefix_event prefix_context resolved_event =
  match resolved_event.se_desc with
  | SAX_startDocument _
  | SAX_endDocument -> ()
  | SAX_startElement (_,_,_,special,_,resolved_element_content,_) ->
      begin
	let nsenv =
	  match !resolved_element_content with
	  | None -> raise (Query(Stream_Error("Trying to unresolve an unresolved stream")))
	  | Some (_,nsenv) -> nsenv
	in
	let ns_bindings = Prefix_context.push_nsenv_in_prefix_context prefix_context nsenv in
	let ns_attributes = recreate_ns_bindings ns_bindings in
	special := ns_attributes;
	(* Just resets the resolved part *)
	resolved_element_content := None
      end
  | SAX_endElement ->
      begin
	Prefix_context.pop_nsenv_from_prefix_context prefix_context
      end
  | SAX_processingInstruction _
  | SAX_comment _
  | SAX_characters _ ->
      ()
  | SAX_attribute (_,_,resolved_attribute_content, _) ->
(* Prior code when we used to rebuild the event. -JS *)
(*
      (* At the top-level, assume an empty namespace environment *)
      let nsenv = empty_nsenv in
      let sax_xml_attribute =
	prefix_attribute nsenv resolved_sax_xml_attribute
      in
      Some (fmkse_event (SAX_attribute sax_xml_attribute) resolved_event.rse_loc)
*)
      begin
	(* Just resets the resolved part *)
	resolved_attribute_content := None
      end
  | SAX_atomicValue _
  | SAX_hole
  | SAX_endEncl
  | SAX_startEncl ->
      ()

let prefix_xml_stream_next_event prefix_context resolved_xml_stream =
  try
    let next_event = Cursor.cursor_next resolved_xml_stream in
    begin
      prefix_event prefix_context next_event;
      Some next_event
    end
  with
  | Stream.Failure ->
      None

let next_event_for_prefix_xml_stream prefix_context resolved_xml_stream n =
  prefix_xml_stream_next_event prefix_context resolved_xml_stream

let prefix_xml_stream resolved_xml_stream =
  let prefix_context = Prefix_context.build_prefix_context () in
  Cursor.cursor_of_function (next_event_for_prefix_xml_stream prefix_context resolved_xml_stream)


 (*************************************************)
 (* Conversion between resolved and typed streams *)
 (*************************************************)

 (* Turns an untyped XML stream into an typed one *)

 (* Note:
      The following adds xs:untyped as type annotations for
      elements, and xs:untypedAtomic for attributes.
    - Jerome *)

let type_attribute attribute =
  match attribute with
  | (rattr_qname,attr_content,attr_sym,attr_type) ->
      begin
	match !attr_sym with
	| None -> raise (Query(Stream_Error("Trying to type an unresolved stream [attribute error]" ^ (Namespace_names.string_of_uqname rattr_qname))))
	| Some rattr_sym ->
	    attr_type := Some (untypedAtomicsym,[])
      end

let type_attributes attributes =
  List.iter type_attribute attributes

let typed_of_resolved_event event =
  match event.se_desc with
  | SAX_startDocument _
  | SAX_endDocument
  | SAX_startEncl
  | SAX_endEncl -> ()
  | SAX_startElement (elem_uqname,sax_attributes,has_element_content,special,base_uri,relem_content,relem_type) ->
      begin
	match !relem_content with
	| None -> raise (Query(Stream_Error("Trying to type an unresolved stream")))
	| Some (relem_sym,nsenv) ->
	    begin
	      (* Add type to the element's attributes *)
	      type_attributes sax_attributes;
	      (* Add type to the element *)
	      relem_type := Some (false,untypedsym,[])
	    end
      end
  | SAX_endElement
  | SAX_processingInstruction _
  | SAX_comment _
  | SAX_characters _ -> ()
  | SAX_attribute sax_xml_attribute ->
      type_attribute sax_xml_attribute
  | SAX_atomicValue _
  | SAX_hole ->
      ()

let typed_event_wrap xml_stream =
  try
    let next_event = Cursor.cursor_next xml_stream in
    begin
      (* Type the event *)
      typed_of_resolved_event next_event;
      (* Return it *)
      Some next_event
    end
  with
  | Stream.Failure ->
      None

let next_event_typed_of_resolved_xml_stream xml_stream n =
  typed_event_wrap xml_stream

let typed_of_resolved_xml_stream xml_stream =
  let s1 =
    Cursor.cursor_of_function (next_event_typed_of_resolved_xml_stream xml_stream)
  in
  merge_xml_text_nodes_in_typed_stream s1

 (* Turns a typed XML stream into a resolved, untyped one *)

let erase_attribute (_,_,_,ratt_type) = ratt_type := None
let erase_attributes typed_attributes = List.iter erase_attribute typed_attributes

let erase_event typed_event =
  match typed_event.se_desc with
  | SAX_startDocument _
  | SAX_endDocument -> ()
  | SAX_startElement (_,typed_sax_xml_attributes,_,_,_,_,relem_type) ->
      begin
	erase_attributes typed_sax_xml_attributes;
	relem_type := None
      end
  | SAX_endElement
  | SAX_processingInstruction _
  | SAX_comment _
  | SAX_characters _ ->
      ()
  | SAX_attribute typed_sax_xml_attribute ->
      erase_attribute typed_sax_xml_attribute
  | SAX_atomicValue _
  | SAX_hole
  | SAX_startEncl 
  | SAX_endEncl ->
      ()

let rec erase_xml_stream_next_event typed_xml_stream =
  try
    let next_event = Cursor.cursor_next typed_xml_stream in
    begin
      erase_event next_event;
      Some next_event
    end
  with
  | Stream.Failure ->
      None

let next_event_for_erase_xml_stream typed_xml_stream n =
  erase_xml_stream_next_event typed_xml_stream

let erase_xml_stream typed_xml_stream =
  Cursor.cursor_of_function (next_event_for_erase_xml_stream typed_xml_stream)

 (* Turns a typed XML stream into a resolved, non-typed one, but also
    turns atomic values into text nodes according to the semantics in
    Section 3.7.1 of the XQuery 1.0 document, and rejects attribute
    events, since they should have been processed before-hand from the
    beginning of the stream. *)

let erase_atomic_event_section_3_7_1 typed_xml_stream av0 =
  let rec erase_atomic_event_section_3_7_1_aux typed_xml_stream =
    let typed_event = (Cursor.cursor_peek typed_xml_stream) in
    match typed_event with
      Some annot_tse ->
	begin
	  match annot_tse.se_desc with
	    (SAX_atomicValue av1) ->
	      begin
		ignore(Cursor.cursor_next typed_xml_stream);
		let avl = erase_atomic_event_section_3_7_1_aux typed_xml_stream in
		(av1 :: avl)
	      end
		(* Start/end enclosed expr are sentinels to end concatenation
		   of atomic values.  They are always consumed. *)
	  | (SAX_startEncl) ->
	      raise (Query(Stream_Error("Start enclosed event cannot follow atomic value event\n")))
	  | (SAX_endEncl) ->
	      ignore(Cursor.cursor_next typed_xml_stream);
	      []
	  | _ ->
	      []
	end
    | _ ->
	[]
  in
  let avl = erase_atomic_event_section_3_7_1_aux typed_xml_stream in
  fmkse_event (SAX_characters (Dm_atomic_util.erase_simple_value (av0 :: avl))) Finfo.bogus

let rec erase_event_section_3_7_1 typed_xml_stream typed_event =
  match typed_event.se_desc with
  | SAX_startDocument _
  | SAX_endDocument -> typed_event
  | SAX_startElement (_,typed_sax_xml_attributes,_,_,_,_,relem_type) ->
      begin
	erase_attributes typed_sax_xml_attributes;
	relem_type := None;
	typed_event
      end
  | SAX_endElement
  | SAX_processingInstruction _
  | SAX_comment _ -> typed_event
	(* From 3.7.1
	   Adjacent text nodes in the content sequence are merged into a
	   single text node by concatenating their contents, with no
	   intervening blanks. After concatenation, any text node whose
	   content is a zero-length string is deleted from the content
	   sequence.
	 *)
  | SAX_characters text_desc ->
      if (text_desc = "") then 
	let next_event = Cursor.cursor_next typed_xml_stream in
	(erase_event_section_3_7_1 typed_xml_stream next_event)
      else
	typed_event
  | SAX_attribute typed_sax_xml_attribute ->
      begin
	erase_attribute typed_sax_xml_attribute;
	typed_event
      end
  | SAX_atomicValue av ->
      erase_atomic_event_section_3_7_1 typed_xml_stream av 
  | SAX_hole ->
      typed_event
  | SAX_startEncl ->
      let next_event = Cursor.cursor_next typed_xml_stream in
      (erase_event_section_3_7_1 typed_xml_stream next_event)
  | SAX_endEncl ->
      let next_event = Cursor.cursor_next typed_xml_stream in
      (erase_event_section_3_7_1 typed_xml_stream next_event)

let erase_xml_stream_next_event_section_3_7_1 typed_xml_stream =
  try
    let next_event = Cursor.cursor_next typed_xml_stream in
    (Some (erase_event_section_3_7_1 typed_xml_stream next_event))
  with
  | Stream.Failure ->
      None

let next_event_for_erase_xml_stream_section_3_7_1 typed_xml_stream n =
  erase_xml_stream_next_event_section_3_7_1 typed_xml_stream

let erase_xml_stream_section_3_7_1 typed_xml_stream =
  Cursor.cursor_of_function (next_event_for_erase_xml_stream_section_3_7_1 typed_xml_stream)


(**********************************************)
(* Conversion between typed and ordered typed *)
(**********************************************)

let order_attribute streaming_ordered_context sax_xml_attribute =
  let attributeid =
    Streaming_ordered_context.new_leaf_docorder streaming_ordered_context
  in
  (sax_xml_attribute, attributeid)

let order_start_element streaming_ordered_context element_desc : ordered_element_desc =
  match element_desc with
  | (uqname,
     sax_xml_attribute_forest,
     has_element_content,
     special,
     base_uri,
     res_elem,
     typed_elem) ->
       let ordered_sax_xml_attribute_forest =
	 List.map (order_attribute streaming_ordered_context) sax_xml_attribute_forest
       in
       (uqname,
	ordered_sax_xml_attribute_forest,
	has_element_content,
	special,
	base_uri,
	res_elem,
	typed_elem)

let ordered_event_of_typed_event streaming_ordered_context next_event =
  let next_ordered_event_desc =
    match next_event.se_desc with
    | SAX_startDocument document_desc ->
	let startdocid =
	  Streaming_ordered_context.new_preorderid streaming_ordered_context
	in
	OTSAX_startDocument (document_desc,startdocid)
    | SAX_endDocument ->
	let enddocid =
	  Streaming_ordered_context.new_postorderid streaming_ordered_context
	in
	OTSAX_endDocument enddocid 
    | SAX_startElement element_desc ->
	let startelementid =
	  Streaming_ordered_context.new_preorderid streaming_ordered_context
	in
	let ordered_element_desc =
	  order_start_element streaming_ordered_context element_desc
	in
	OTSAX_startElement (ordered_element_desc, startelementid)
    | SAX_endElement ->
	let endelementid =
	  Streaming_ordered_context.new_postorderid streaming_ordered_context
	in
	OTSAX_endElement endelementid
    | SAX_processingInstruction pi_desc ->
	let piid =
	  Streaming_ordered_context.new_leaf_docorder streaming_ordered_context
	in
	OTSAX_processingInstruction (pi_desc,piid)
    | SAX_comment comment_desc ->
	let commentid =
	  Streaming_ordered_context.new_leaf_docorder streaming_ordered_context
	in
	OTSAX_comment (comment_desc,commentid)
    | SAX_characters text_desc ->
	let textid =
	  Streaming_ordered_context.new_leaf_docorder streaming_ordered_context
	in
	OTSAX_characters (text_desc,textid)
	  (* Additions to the standard SAX events: *)
    | SAX_attribute sax_xml_attribute ->
	let tsxa =
	  order_attribute streaming_ordered_context sax_xml_attribute
	in
	OTSAX_attribute tsxa
    | SAX_atomicValue av ->
	OTSAX_atomicValue av
    | SAX_hole ->
	raise (Query (Stream_Error "Cannot create node identifier in a stream with a [hole]")) 
    | SAX_startEncl
    | SAX_endEncl ->
	raise (Query (Stream_Error "1: Cannot create a node identified in an XML stream with start/end enclosed expression"))
  in
  fmkotse_event next_ordered_event_desc next_event.se_annot next_event.se_loc

let next_ordered_typed_event streaming_ordered_context typed_xml_stream =
  try
    let next_event = Cursor.cursor_next typed_xml_stream in
    (Some (ordered_event_of_typed_event streaming_ordered_context next_event))
  with
  | Stream.Failure ->
      None

let next_ordered_typed_event_token streaming_ordered_context typed_xml_stream n =
  next_ordered_typed_event streaming_ordered_context typed_xml_stream

let ordered_typed_of_typed_stream_for_docid docid nodeid_context typed_xml_stream =
  let streaming_ordered_context =
    Streaming_ordered_context.build_streaming_ordered_context docid nodeid_context
  in
  Cursor.cursor_of_function (next_ordered_typed_event_token streaming_ordered_context typed_xml_stream)

let ordered_typed_of_typed_stream docid_gen nodeid_context typed_xml_stream =
  let docid = Nodeid.new_docid docid_gen in
  ordered_typed_of_typed_stream_for_docid docid nodeid_context typed_xml_stream


(**********************)
(* Stream composition *)
(**********************)

(* Composes a well-formed stream with holes with a list of well-formed streams *)

let rec next_event_of_compose_xml_streams s0 current_top additional_streams =
  match !current_top with
  | None ->
      begin
	try
	  let event = (Cursor.cursor_next s0) in
	  match event.se_desc with
	  | SAX_hole ->
	      begin
		match !additional_streams with
		| [] ->
		    raise (Query (Stream_Error "Stream composition: less streams than holes in the main stream"))
		| s1 :: sl ->
		    begin
		      current_top := Some s1;
		      additional_streams := sl;
		      next_event_of_compose_xml_streams s0 current_top additional_streams
		    end
	      end
	  | _ ->
	      Some event
	with
	| Stream.Failure ->
	    begin
	      match !additional_streams with
	      | [] ->
		  None
	      | _ ->
		  raise (Query (Stream_Error "Stream composition: more streams than holes in the main stream"))
	    end
      end
  | Some s ->
      try
	Some (Cursor.cursor_next s)
      with
      |	Stream.Failure ->
	  begin
	    (* Switch back to the top-level stream *)
	    current_top := None;
	    next_event_of_compose_xml_streams s0 current_top additional_streams
	  end

let next_event_of_compose_xml_streams_aux s0 current_top additional_streams n =
  next_event_of_compose_xml_streams s0 current_top additional_streams

let compose_xml_streams s0 sl =
  let current_top = ref None in
  let additional_streams = ref sl in
  Cursor.cursor_of_function (next_event_of_compose_xml_streams_aux s0 current_top additional_streams)

(* Composes a resolved stream with holes with a list of resolved streams *)

let rec next_event_of_compose_resolved_xml_streams s0 current_top additional_streams =
  match !current_top with
  | None ->
      begin
	try
	  let event = (Cursor.cursor_next s0) in
	  match event.se_desc with
	  | SAX_hole ->
	      begin
		match !additional_streams with
		| [] ->
		    raise (Query (Stream_Error "Stream composition: less streams than holes in the main stream"))
		| s1 :: sl ->
		    begin
		      current_top := Some s1;
		      additional_streams := sl;
		      next_event_of_compose_resolved_xml_streams s0 current_top additional_streams
		    end
	      end
	  | _ ->
	      Some event
	with
	| Stream.Failure ->
	    begin
	      match !additional_streams with
	      | [] ->
		  None
	      | _ ->
		  raise (Query (Stream_Error "Stream composition: more streams than holes in the main stream"))
	    end
      end
  | Some s ->
      try
	Some (Cursor.cursor_next s)
      with
      |	Stream.Failure ->
	  begin
	    (* Switch back to the top-level stream *)
	    current_top := None;
	    next_event_of_compose_resolved_xml_streams s0 current_top additional_streams
	  end

let next_event_of_compose_resolved_xml_streams_aux s0 current_top add_streams n =
  next_event_of_compose_resolved_xml_streams s0 current_top add_streams

let compose_resolved_xml_streams s0 sl =
  let current_top = ref None in
  let additional_streams = ref sl in
  Cursor.cursor_of_function (next_event_of_compose_resolved_xml_streams_aux s0 current_top additional_streams)

    (* Composes a typed stream with holes with a list of typed streams *)

let rec next_event_of_compose_typed_xml_streams s0 current_top additional_streams =
  match !current_top with
  | None ->
      begin
	try
	  let event = (Cursor.cursor_next s0) in
	  match event.se_desc with
	  | SAX_hole ->
	      begin
		match !additional_streams with
		| [] ->
		    raise (Query (Stream_Error "Stream composition: less streams than holes in the main stream"))
		| s1 :: sl ->
		    begin
		      current_top := Some s1;
		      additional_streams := sl;
		      
		      next_event_of_compose_typed_xml_streams s0 current_top additional_streams
		     end
	       end
	   | _ ->
	       Some event
	 with
	 | Stream.Failure ->
	     begin
	       match !additional_streams with
	       | [] ->
		   None
	       | s::l -> 
		   raise (Query (Stream_Error "4: Stream composition: more streams than holes in the main stream"))
	     end
       end
   | Some s ->
       try
	  Some (Cursor.cursor_next s)
       with
       |	Stream.Failure ->
	   begin
	     (* Switch back to the top-level stream *)
	     current_top := None; 
	     next_event_of_compose_typed_xml_streams s0 current_top additional_streams
	   end

 let next_event_of_compose_typed_xml_streams_aux s0 current_top additional_streams n =
   next_event_of_compose_typed_xml_streams s0 current_top additional_streams

 let compose_typed_xml_streams s0 sl =
   let current_top = ref None in
   let additional_streams = ref sl in
   Cursor.cursor_of_function (next_event_of_compose_typed_xml_streams_aux s0 current_top additional_streams)

 (* Builds a typed XML stream with just one SAX comment event. *)

 let rec simple_value_of_xml_stream input_stream =
   String.concat "" (simple_value_of_xml_stream_aux input_stream)
 and simple_value_of_xml_stream_aux input_stream =
   match Cursor.cursor_peek input_stream with
   | None -> []
   | Some { se_desc = SAX_atomicValue av; se_annot = _; se_loc = _ } -> 
       begin
	 Cursor.cursor_junk input_stream;
	 (av#string_value()) :: 
	 ((match Cursor.cursor_peek input_stream with 
	 | Some ({ se_desc = SAX_atomicValue _; se_annot = _; se_loc = _ }) -> " "
	 | _ -> "") :: 
	  (simple_value_of_xml_stream_aux input_stream))
       end
   | Some { se_desc = SAX_startEncl ; se_annot = _; se_loc = _ } 
   | Some { se_desc = SAX_endEncl ; se_annot = _; se_loc = _ } ->
       Cursor.cursor_junk input_stream;
       (simple_value_of_xml_stream_aux input_stream)
   | _ ->
       raise (Query (Stream_Error "Not a stream of atomic values"))


 (* Builds an XML stream with just one TSAX_attribute event, from a
    stream of atomic values, adding one character whitespace between
    each atomic value. *)

(*
 let resolved_xml_stream_of_attribute sym text =
   Cursor.cursor_of_list [fmkse_event(SAX_attribute(sym,text)) Finfo.bogus]
*)

(***************************)
(* Simple stream accessors *)
(***************************)

(* Returns true is the stream is empty *)

let is_empty_xml_stream xml_stream =
  match Cursor.cursor_peek xml_stream with
  | None -> true
  | Some _ -> false


(* Returns true is the resolved stream is empty *)

let is_empty_resolved_xml_stream xml_stream =
  match Cursor.cursor_peek xml_stream with
  | None -> true
  | Some _ -> false


(* Returns true is the typed stream is empty *)

let is_empty_typed_xml_stream xml_stream =
  match Cursor.cursor_peek xml_stream with
  | None -> true
  | Some _ -> false


(* Return all of the leading attributes in the stream *)

let rec consume_leading_attribute_events resolved_xml_stream =
  match (Cursor.cursor_peek resolved_xml_stream) with
  | Some { se_desc = SAX_attribute attribute_desc; se_loc = _; } ->
      begin
	ignore(Cursor.cursor_next resolved_xml_stream);
	let next_attributes = consume_leading_attribute_events resolved_xml_stream in
	attribute_desc :: next_attributes
      end
  | _ ->
      []
