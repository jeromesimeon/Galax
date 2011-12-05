(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: stream_project.ml,v 1.12 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Stream_project
   Description:
     This module implements document projection on an XML stream.
*)

open Error

open Streaming_types
open Streaming_util
open Streaming_ops

open Project_context

(* Projects attributes *)

let project_attributes other_atts pfs' =
  List.filter (Path_structutil.one_step_attribute pfs') other_atts

let rec project_next_projection pfs project_context =
  (* Get the next event *)
  let xml_event = Project_context.get_next_xml_event project_context in

  (* And refill the buffer accordingly *)

  match xml_event.se_desc with
  | SAX_startDocument _ ->
      raise (Query (Projection ("Should not have a start document event")))
  | SAX_endEncl|SAX_startEncl ->
      raise (Query (Projection ("Should not have a enclosed expression event")))
  | SAX_endDocument ->
      Project_context.pop_project_context project_context [xml_event]

  | SAX_startElement (relem_sym, attributes, has_element_content, special, baseuri, delta_bindings) ->

      (* Identify which kind of action is required based on the path structure *)
      let action = Path_structutil.one_step xml_event pfs in
      begin
	match action with
	| Path_structutil.GetSubtree ->

            (* SUBTREE ACTION --

               In the case the required action is to return the whole
               subtree, flip to non-projection parsing using None in
               the path structure stack. - Jerome *)

	    Project_context.push_project_context_get_subtree project_context xml_event
	      
	| Path_structutil.KeepMovingSkipNode pfs' ->
	    
            (* KEEP MOVING SKIP NODE ACTION --
	       
               In that case, we keep talking down the tree until we
               can decide whether we should return the node or not. - Jerome *)
	    
            (* Extract the namespace attributes and update the
	       namespace environment accordingly. *)
	    
	    let projected_attributes = project_attributes attributes pfs' in
	    
	    let new_xml_event = fmkse_event (SAX_startElement (relem_sym, projected_attributes, has_element_content, special, baseuri, delta_bindings)) xml_event.se_loc  in
	    
	    if (projected_attributes = []) then
	      begin
		Project_context.push_project_context_keep_moving_skip_node
		  project_context
		  new_xml_event
		  pfs'
	      end
	    else
	      begin
		Project_context.push_project_context_keep_moving_preserve_node
		  project_context
		  new_xml_event
		  pfs'
	      end
		
	| Path_structutil.KeepMovingPreserveNode pfs' ->
	    
            (* KEEP MOVING PRESERVE NODE ACTION --
	       
               In that case, we return the current event and keep
               talking down the tree. - Jerome *)
	    
	    let projected_attributes = project_attributes attributes pfs' in
	    
	    let new_xml_event = fmkse_event (SAX_startElement (relem_sym, projected_attributes, has_element_content, special, baseuri, delta_bindings)) xml_event.se_loc in
	    
	    begin
	      Project_context.push_project_context_keep_moving_preserve_node
		project_context
		new_xml_event
		pfs'
	    end
	      
	| Path_structutil.PreserveNode ->
	    
            (* PRESERVE NODE ACTION --
	       
               In that case, discard the XML stream for the subtree,
               but keep the events for the current node. - Jerome *)
	    
	    let refill_local_buffer = [
	      fmkse_event (SAX_startElement (relem_sym, attributes, has_element_content, special, baseuri, delta_bindings)) xml_event.se_loc;
	      fmkse_event (SAX_endElement) xml_event.se_loc
	    ]
	    in
	    Project_context.push_project_context_preserve_node project_context refill_local_buffer
	      
	| Path_structutil.SkipNode ->
	    
            (* SKIP NODE ACTION --
	       
               In that case, discard the XML stream. - Jerome *)
	    
            (* Extract the namespace attributes and update the
	       namespace environment accordingly. *)
	    
	    Project_context.push_project_context_skip_node project_context
      end
	
  | SAX_endElement ->
      Project_context.pop_project_context project_context [xml_event]

  | SAX_processingInstruction (target,content) ->
      let action = Path_structutil.one_step xml_event pfs in
      begin
	match action with
	| Path_structutil.GetSubtree
	| Path_structutil.KeepMovingPreserveNode _
	| Path_structutil.PreserveNode ->
	    Project_context.refill_local_buffer project_context [xml_event]
	| Path_structutil.KeepMovingSkipNode _
	| Path_structutil.SkipNode ->
	    project_next_projection pfs project_context
      end
	
  | SAX_comment c ->
      let action = Path_structutil.one_step xml_event pfs in
      begin
	match action with
	| Path_structutil.GetSubtree
	| Path_structutil.KeepMovingPreserveNode _
	| Path_structutil.PreserveNode ->
	    Project_context.refill_local_buffer project_context [xml_event]
	| Path_structutil.KeepMovingSkipNode _
	| Path_structutil.SkipNode ->
	    project_next_projection pfs project_context
      end
	
  | SAX_characters _ ->
      let action = Path_structutil.one_step xml_event pfs in
      begin
	match action with
	| Path_structutil.GetSubtree
	| Path_structutil.KeepMovingPreserveNode _
	| Path_structutil.PreserveNode ->
	    Project_context.refill_local_buffer project_context [xml_event]
	| Path_structutil.KeepMovingSkipNode _
	| Path_structutil.SkipNode ->
	    project_next_projection pfs project_context
      end

  | SAX_attribute a ->
      let action = Path_structutil.one_step xml_event pfs in
      begin
	match action with
	| Path_structutil.GetSubtree
	| Path_structutil.KeepMovingPreserveNode _
	| Path_structutil.PreserveNode ->
	    Project_context.refill_local_buffer project_context [xml_event]
	| Path_structutil.KeepMovingSkipNode _
	| Path_structutil.SkipNode ->
	    project_next_projection pfs project_context
      end	      

  | SAX_atomicValue _ ->
      (* A path expression never applies to an atomic value *)
      project_next_projection pfs project_context
  | SAX_hole ->
      raise (Query (Projection "Should not apply projection operation on a stream with holes!"))
	
let rec project_next_get_subtree project_context =
  (* Get the next event *)
  let xml_event = Project_context.get_next_xml_event project_context in
  match xml_event.se_desc with
  | SAX_startDocument _
  | SAX_startElement _ ->
      Project_context.push_project_context_get_subtree project_context xml_event
  | SAX_endDocument
  | SAX_endElement ->
      Project_context.pop_project_context project_context [xml_event]
      
  | SAX_processingInstruction _
  | SAX_comment _
  | SAX_characters _
  | SAX_attribute _
  | SAX_atomicValue _ ->
      Project_context.refill_local_buffer project_context [xml_event]
  | SAX_hole ->
      raise (Query (Projection "Should not apply projection operation on a stream with holes!"))
  | SAX_endEncl|SAX_startEncl ->
      raise (Query (Projection ("Should not have a enclosed expression event")))

let rec project_next project_context =
  (* Check wether stream is exhausted _before_ the context is queried
     for projection paths.
     - Michael *)
  begin
    if
      Project_context.project_stream_is_empty project_context
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
		    (path = []) && (subtree = Path_struct.Subtree) && rest_empty
	  in
	    if all_paths_empty_with_subtrees pfs
	    then
	      project_next_get_subtree project_context
	    else
		(* previous version *)
	      project_next_projection pfs project_context

(* Stream wrapping function *)

let rec next_project_event_internal project_context =
  match Project_context.get_next_buffered_sax_event project_context with
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

  let pfs = Path_structutil.inside_document first_event path_seq (AnyURI._string_of_uri root_uri) in

  let project_context =
    Project_context.build_project_context xml_stream pfs [first_event]
  in

  (Cursor.cursor_of_function (next_project_event project_context))

let project_xml_stream_from_variable vname path_seq xml_stream =

  let first_event = Cursor.cursor_next xml_stream in
  begin
    match first_event.se_desc with
    | SAX_startDocument _ ->
	()
    | _ ->
	raise (Query (Projection ("Was expecting a start document event")))
  end;

  let pfs = Path_structutil.inside_variable first_event path_seq vname in

  let project_context =
    Project_context.build_project_context xml_stream pfs [first_event]
  in

  (Cursor.cursor_of_function (next_project_event project_context))

