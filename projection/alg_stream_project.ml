(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: alg_stream_project.ml,v 1.2 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Alg_stream_project
   Description:
     This module implements document projection on an XML stream.
*)

open Error

open Streaming_types
open Streaming_util
open Streaming_ops

open Ast_path_struct
open Alg_path_structutil
open Alg_project_context

(* Projects attributes *)

let project_attributes other_atts pfs' =
  List.filter (one_step_attribute pfs') other_atts

let rec project_next_projection pfs project_context =
  (* Get the next event *)
  let xml_event = get_next_xml_event project_context in

  (* And refill the buffer accordingly *)

  match xml_event.se_desc with
  | SAX_startDocument _ ->
      raise (Query (Projection ("Should not have a start document event")))

  | SAX_endDocument ->
      pop_project_context project_context [xml_event]

  | SAX_startElement (name, attributes, has_element_content, special, relem, telem) ->
      (* Identify which kind of action is required based on the path structure *)
      let action = one_step xml_event pfs in
      begin
	match action with
	| GetSubtree ->

            (* SUBTREE ACTION --

               In the case the required action is to return the whole
               subtree, flip to non-projection parsing using None in
               the path structure stack. - Jerome *)

	    push_project_context_get_subtree project_context xml_event
	      
	| KeepMovingSkipNode pfs' ->
	    
            (* KEEP MOVING SKIP NODE ACTION --
	       
               In that case, we keep talking down the tree until we
               can decide whether we should return the node or not. - Jerome *)
	    
            (* Extract the namespace attributes and update the
	       namespace environment accordingly. *)
	    
	    let projected_attributes = project_attributes attributes pfs' in
	    
	    let new_xml_event = fmkse_event (SAX_startElement (name, projected_attributes, has_element_content, special, relem, telem)) xml_event.se_loc  in
	    
	    if (projected_attributes = []) then
	      begin
		push_project_context_keep_moving_skip_node
		  project_context
		  new_xml_event
		  pfs'
	      end
	    else
	      begin
		push_project_context_keep_moving_preserve_node
		  project_context
		  new_xml_event
		  pfs'
	      end
		
	| KeepMovingPreserveNode pfs' ->
	    
            (* KEEP MOVING PRESERVE NODE ACTION --
	       
               In that case, we return the current event and keep
               talking down the tree. - Jerome *)
	    
	    let projected_attributes = project_attributes attributes pfs' in
	    
	    let new_xml_event = fmkse_event (SAX_startElement (name, projected_attributes, has_element_content, special, relem, telem)) xml_event.se_loc in
	    
	    begin
	      push_project_context_keep_moving_preserve_node
		project_context
		new_xml_event
		pfs'
	    end
	      
	| PreserveNode ->
	    
            (* PRESERVE NODE ACTION --
	       
               In that case, discard the XML stream for the subtree,
               but keep the events for the current node. - Jerome *)
	    
	    let refill_local_buffer = [
	      fmkse_event (SAX_startElement (name, attributes, has_element_content, special, relem, telem)) xml_event.se_loc;
	      fmkse_event (SAX_endElement) xml_event.se_loc
	    ]
	    in
	    push_project_context_preserve_node project_context refill_local_buffer
	      
	| SkipNode ->
	    
            (* SKIP NODE ACTION --
	       
               In that case, discard the XML stream. - Jerome *)
	    
            (* Extract the namespace attributes and update the
	       namespace environment accordingly. *)
	    
	    push_project_context_skip_node project_context
      end
	
  | SAX_endElement ->
      pop_project_context project_context [xml_event]

  | SAX_processingInstruction (target,content) ->
      let action = one_step xml_event pfs in
      begin
	match action with
	| GetSubtree
	| KeepMovingPreserveNode _
	| PreserveNode ->
	    refill_local_buffer project_context [xml_event]
	| KeepMovingSkipNode _
	| SkipNode ->
	    project_next_projection pfs project_context
      end
	
  | SAX_comment c ->
      let action = one_step xml_event pfs in
      begin
	match action with
	| GetSubtree
	| KeepMovingPreserveNode _
	| PreserveNode ->
	    refill_local_buffer project_context [xml_event]
	| KeepMovingSkipNode _
	| SkipNode ->
	    project_next_projection pfs project_context
      end
	
  | SAX_characters _ ->
      let action = one_step xml_event pfs in
      begin
	match action with
	| GetSubtree
	| KeepMovingPreserveNode _
	| PreserveNode ->
	    refill_local_buffer project_context [xml_event]
	| KeepMovingSkipNode _
	| SkipNode ->
	    project_next_projection pfs project_context
      end

  | SAX_attribute a ->
      let action = one_step xml_event pfs in
      begin
	match action with
	| GetSubtree
	| KeepMovingPreserveNode _
	| PreserveNode ->
	    refill_local_buffer project_context [xml_event]
	| KeepMovingSkipNode _
	| SkipNode ->
	    project_next_projection pfs project_context
      end	      

  | SAX_atomicValue _ ->
      (* A path expression never applies to an atomic value *)
      project_next_projection pfs project_context
  | SAX_hole ->
      raise (Query (Projection "Should not apply projection operation on a stream with holes!"))
  | SAX_startEncl
  | SAX_endEncl ->
      raise (Query (Projection "Should not apply projection operation on a stream with enclosed expressions!"))
	
let rec project_next_get_subtree project_context =
  (* Get the next event *)
  let xml_event = get_next_xml_event project_context in
  match xml_event.se_desc with
  | SAX_startDocument _
  | SAX_startElement _ ->
      push_project_context_get_subtree project_context xml_event
  | SAX_endDocument
  | SAX_endElement ->
      pop_project_context project_context [xml_event]
      
  | SAX_processingInstruction _
  | SAX_comment _
  | SAX_characters _
  | SAX_attribute _
  | SAX_atomicValue _ ->
      refill_local_buffer project_context [xml_event]
  | SAX_hole ->
      raise (Query (Projection "Should not apply projection operation on a stream with holes!"))
  | SAX_startEncl
  | SAX_endEncl ->
      raise (Query (Projection "Should not apply projection operation on a stream with enclosed expressions!"))

let rec project_next project_context =
  (* Check wether stream is exhausted _before_ the context is queried
     for projection paths.
     - Michael *)
  begin
    if
      project_stream_is_empty project_context
    then
      raise Stream.Failure
    else
      ()
  end;
  let pfs = get_pfs project_context in
    match pfs with
      | None ->
	  project_next_get_subtree project_context
      | Some pfs ->
	  (* Treat empty paths with subtree flag the same way as no paths at all.
	     Check tail recursion!
	     - Michael *)
	  let rec all_paths_empty_with_subtrees paths =
	    match paths with
	      | [] -> true
	      | (path, subtree) :: rest ->
		  let rest_empty = all_paths_empty_with_subtrees rest in
		    (path = []) && (subtree = Subtree) && rest_empty
	  in
	    if all_paths_empty_with_subtrees pfs
	    then
	      project_next_get_subtree project_context
	    else
		(* previous version *)
	      project_next_projection pfs project_context

(* Stream wrapping function *)

let rec next_project_event_internal project_context =
  match get_next_buffered_sax_event project_context with
  | None ->
      begin
	project_next project_context;
	next_project_event_internal project_context
      end
  | Some event ->
      Some event

let next_project_event project_context n =
  next_project_event_internal project_context

(* Top level stream operation *)

let project_xml_stream_from_document root_uri path_seq xml_stream =

  let first_event = Cursor.cursor_next xml_stream in
  begin
    match first_event.se_desc with
    | SAX_startDocument _ ->
	()
    | _ ->
	raise (Query (Projection ("Was expecting a start document event")))
  end;

  let pfs = inside_document first_event path_seq (AnyURI._string_of_uri root_uri) in

  let project_context =
    build_project_context xml_stream pfs [first_event]
  in

  (Cursor.cursor_of_function (next_project_event project_context))

