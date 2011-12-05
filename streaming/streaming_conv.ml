(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: streaming_conv.ml,v 1.13 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Stream_conv
   Description:
     This module provides data structures, accessor and conversion
     functions for an extended XML token stream model, labeled
     streams, namely.
*)


open Streaming_types

open Namespace_names
open Namespace_symbols

open Error


type typed_labeled_sax_event = sax_event

type typed_labeled_xml_stream = xml_stream

type depth_counter = int ref

type typed_labeled_of_typed_context = depth_counter

type typed_of_labeled_typed_context = {

  (* Buffer *)
  buffer : typed_labeled_sax_event Dynamic_buffer.t;
  
  (* Keeps track of relevant 'starting-points' (tree roots) inside the buffer *)
  position_queue : int Queue.t;

  (* An event in the input is top-level iff toplevel_depth_counter = 0 *)
  toplevel_depth_counter : depth_counter;
  
  (* Tracks the depth of buffered events relative to the buffered tree
     they are part of at the time they are added to the buffer;
     1) the buffer is accessible iff b_i_d_c = 0 ( and ...)
     2) an event has to be buffered iff b_i_d_c != 0 (or ...) *)
  buffer_input_depth_counter : depth_counter;

  (* Tracks the depth of buffered events relative to the buffered tree
     they are part of at the time they are taken out of the buffer;
     a buffered tree has been emitted entirely iff b_o_d_c = 0 *)
  buffer_output_depth_counter : depth_counter;
}


(*****************)
(* Stream access *)
(*****************)

let get_flag labeled_event =
  Sax_annot.get_stream_label_annot labeled_event.se_annot

let set_flag labeled_event =
  Sax_annot.set_stream_label_annot labeled_event.se_annot true

let unset_flag labeled_event =
  Sax_annot.set_stream_label_annot labeled_event.se_annot false


(************************)
(* Context initializers *)
(************************)

let default_typed_labeled_of_typed_context () =
  ref 0

let default_typed_of_typed_labeled_context chunks csize inc =
  let init = Streaming_util.fmkse_event SAX_hole Finfo.bogus in
  unset_flag init; 
  (* num_chunks, chunk_size, increment *)
  (* 20 500 5 *)
  (* 100 50000 5 *)
  (* 1 10000 1 *)
  let buff = Dynamic_buffer.make chunks csize inc init in
  { buffer = buff;
    position_queue = Queue.create ();
    toplevel_depth_counter = ref 0;
    buffer_output_depth_counter = ref 0;
    buffer_input_depth_counter = ref 0 }


(***********************************)
(* 'Generic' depth counter helpers *)
(***********************************)

let decrease_depth_counter depth_counter =
  depth_counter := (!depth_counter - 1)

let increase_depth_counter depth_counter =
  depth_counter := (!depth_counter + 1)

let is_depth_zero depth_counter =
  !depth_counter = 0

let is_depth_one depth_counter =
  !depth_counter = 1

let is_depth_greater_zero depth_counter =
  !depth_counter > 0

let is_depth_greater_one depth_counter =
  !depth_counter > 1


(*********************)
(* Stream conversion *)
(*********************)

(***************)
(* Event level *)
(***************)

(****************)
(* Stream level *)	
(****************)

(* Sets the flag iff the event is at the top-level *)
let typed_labeled_of_typed_xml_stream_internal typed_xml_stream tlt_context =
  let next_event input_stream () =
    try
      let event = Cursor.cursor_next input_stream in
	match event.se_desc with
	  | SAX_startDocument _
	  | SAX_startElement _ ->
	      begin
		increase_depth_counter tlt_context;
		if is_depth_one tlt_context
		then
		  (set_flag event;
		   Some event)
		else
		  (unset_flag event;
		   Some event)
	      end
	  | SAX_endDocument
	  | SAX_endElement ->
	      begin
		decrease_depth_counter tlt_context;
		if is_depth_zero tlt_context
		then
		  (set_flag  event; 
		   Some event)
		else
		  (unset_flag event;
		   Some event)
	      end
	  | _ ->
	      begin
		if is_depth_zero tlt_context
		then
		  (set_flag event; 
		   Some event)
		else
		  (unset_flag event;
		   Some event)
	      end
    with
      | Stream.Failure ->	None
  in
    Cursor.cursor_of_function (next_event typed_xml_stream)
      
(* Returns the next event from the buffer, keeping track of the event's depth
   relative to the buffered fragment it is part of *)
let next_buffered_event_track_depth ttl_context =
  let buffer = ttl_context.buffer in
  let depth_counter = ttl_context.buffer_output_depth_counter in
  let labeled_event = Dynamic_buffer.next buffer in
    begin
      match labeled_event.se_desc with
	| SAX_startElement _ ->
	    increase_depth_counter depth_counter
	| SAX_endElement ->
	    decrease_depth_counter depth_counter
	| SAX_startDocument _
	| SAX_endDocument ->
	    raise (Query (Streaming_XPath "Should not have document events in the buffer."))
	| _ -> ()
    end;
    (* Added label-removal - Michael 05|14|2006 *)
    unset_flag labeled_event;
    labeled_event

(* Returns the next event from the buffer, advancing to the next recorded
   buffer position if necessary *)
let next_buffered_event ttl_context =
  let buffer = ttl_context.buffer in
  let depth_counter = ttl_context.buffer_output_depth_counter in
    begin
      if is_depth_zero depth_counter
      then
	let queue = ttl_context.position_queue in
	  try 
	    let position = Queue.pop queue in
	      Dynamic_buffer.position buffer position
	  with Queue.Empty ->
	    Dynamic_buffer.reset buffer
    end;
    next_buffered_event_track_depth ttl_context
      
(* Records the current buffer position *)
let record_buffer_position ttl_context =
  let position = Dynamic_buffer.get_position ttl_context.buffer in
    Queue.push position ttl_context.position_queue

(* Stores an event in the buffer *)
let buffer labeled_event ttl_context =
(*  print_string ("buffering: " ^ (string_of_typed_sax_event labeled_event.event) ^ "\n");*)
  Dynamic_buffer.add ttl_context.buffer labeled_event
	    	      
(* Buffers a (non-top-level) event iff its flag is set or it is part of a tree that
   is to be buffered; records the buffer position iff a start event's flag is set *)
let consider_buffering labeled_event ttl_context =
  let depth_counter = ttl_context.buffer_input_depth_counter in
    match labeled_event.se_desc with
      | SAX_startElement _ ->
	  let is_flag_set = get_flag labeled_event in
	    if is_depth_zero depth_counter
	    then
	      begin
		if is_flag_set
		then
		  begin
		    increase_depth_counter depth_counter;
		    record_buffer_position ttl_context;
		    buffer labeled_event ttl_context
		  end
	      end
	    else
	      begin
		increase_depth_counter depth_counter;
		if is_flag_set
		then
		  record_buffer_position ttl_context;
		buffer labeled_event ttl_context
	      end

      | SAX_endElement ->
	  if is_depth_greater_zero depth_counter
	  then
	    begin
	      decrease_depth_counter depth_counter;
	      buffer labeled_event ttl_context
	    end

      | SAX_startDocument _
      | SAX_endDocument ->
	  raise (Query (Streaming_XPath "Should never consider buffering document events."))

      | _ ->
	  let is_flag_set = get_flag labeled_event in
	    if is_depth_zero depth_counter
	    then
	      begin
		if is_flag_set
		then
		  begin
		    record_buffer_position ttl_context;
		    buffer labeled_event ttl_context
		  end
	      end
	    else
	      begin
		if is_flag_set
		then
		  record_buffer_position ttl_context;
		buffer labeled_event ttl_context
	      end

(* Returns the next event from the input stream, considering buffering that
   event only in case it is not at the top-level *)
let next_event_track_toplevel input_stream ttl_context =
  try
    let labeled_event = Cursor.cursor_next input_stream in
    let depth_counter = ttl_context.toplevel_depth_counter in
      match labeled_event.se_desc with
	| SAX_startDocument _
	| SAX_startElement _ ->
	    increase_depth_counter depth_counter;
	    if is_depth_greater_one depth_counter
	    then
	      begin
		consider_buffering labeled_event ttl_context;
		(* Added label-removal - Michael 06|12|2006 *)
		unset_flag labeled_event
	      end;
	    Some labeled_event
	| SAX_endDocument
	| SAX_endElement ->
	    decrease_depth_counter depth_counter;
	    if is_depth_greater_zero depth_counter
	    then
	      begin
		consider_buffering labeled_event ttl_context;
		(* Added label-removal - Michael 06|12|2006 *)
		unset_flag labeled_event
	      end;
	    Some labeled_event
	| _ ->
	    if not (is_depth_zero depth_counter)
	    then
	      begin
		consider_buffering labeled_event ttl_context;
		(* Added label-removal - Michael 06|12|2006 *)
		unset_flag labeled_event
	      end;
	    Some labeled_event
  with
    | Stream.Failure -> None

(* The buffer is accessible iff neither does the current event belong to a tree
   that is to be buffered, nor is the current event non-top-level (in case of
   the current event being non-top-level, the tree it is part of must be emitted
   in entirety, first, before starting to process a new tree from the buffer). *)
let is_buffer_accesible ttl_context =
  (is_depth_zero ttl_context.buffer_input_depth_counter) && (is_depth_zero ttl_context.toplevel_depth_counter)
	      
(* If accessible, returns the next event from the buffer; if not, simply passes
   through the next outstanding event from the input stream (possibly involving
   buffering etc.) *)
let next_event input_stream ttl_context () =
  if is_buffer_accesible ttl_context
  then
    begin
      (*print_string "from buffer\n";*)
      try
	Some (next_buffered_event ttl_context)
      with
      | Dynamic_buffer.Exhausted ->
	  begin
	    (*print_string "cancelled\n ";*)
	    next_event_track_toplevel input_stream ttl_context
	  end
    end
  else
    begin
      (*print_string "from input\n";*)
      next_event_track_toplevel input_stream ttl_context
    end
      

(**********************)
(* Context generation *)
(**********************)

let typed_of_typed_labeled_xml_stream_internal typed_labeled_xml_stream ttl_context =
  Cursor.cursor_of_function (next_event typed_labeled_xml_stream ttl_context)


(***********)
(* Exposed *)      
(***********)

let typed_labeled_of_typed_xml_stream typed_xml_stream =
  let tlt_context = default_typed_labeled_of_typed_context () in
    typed_labeled_of_typed_xml_stream_internal typed_xml_stream tlt_context

let typed_of_typed_labeled_xml_stream typed_labeled_xml_stream =
  let chunks = !Conf.buffer_chunks in
  let csize = !Conf.buffer_csize in
  let inc = !Conf.buffer_inc in
  let typed_of_typed_labeled_context = default_typed_of_typed_labeled_context chunks csize inc in
  let typed_xml_stream = 
    typed_of_typed_labeled_xml_stream_internal typed_labeled_xml_stream typed_of_typed_labeled_context
  in
    typed_xml_stream

  (*typed_labeled_xml_stream*)
     
let slice_typed_xml_stream typed_labeled_xml_stream =
  (* Must resolve the labels in order to properly reflect item boundaries. *)
  let typed_xml_stream = typed_of_typed_labeled_xml_stream typed_labeled_xml_stream in

  let finished_slice = ref false in
  let depth = ref 0 in

  let next_slice () =
    match Cursor.cursor_peek typed_xml_stream with
      | None -> None
      | Some _ ->
	  let next_event_current_slice () =
	    if !finished_slice
	    then
	      begin
		finished_slice := false;
		None
	      end
	    else
	      let next_event = Cursor.cursor_next typed_xml_stream in
		begin
		  match next_event.se_desc with
		    | SAX_startDocument _
		    | SAX_startElement _
		    | SAX_startEncl ->
			depth := !depth + 1
		    | SAX_endDocument
		    | SAX_endElement
		    | SAX_endEncl ->
			depth := !depth - 1;
		    | _ -> ()
		end;
		if !depth = 0
		then
		  finished_slice := true;
		Some next_event
	  in
	    Some (Cursor.cursor_of_function next_event_current_slice)
  in
    Cursor.cursor_of_function next_slice

let slice_discard_typed_xml_stream typed_xml_stream =
  Cursor.cursor_map Streaming_ops.discard_typed_xml_stream (slice_typed_xml_stream typed_xml_stream)

let item_count_typed_labeled_xml_stream typed_labeled_xml_stream =
  let item_count = ref 0 in

  let rec count_items () =
    let labeled_event = Cursor.cursor_next typed_labeled_xml_stream in
      if get_flag labeled_event then incr item_count;
      count_items ()
  in
    
  let _ =
    try count_items ()
    with | Stream.Failure -> ()
  in 
    !item_count

let item_range_typed_labeled_xml_stream typed_labeled_xml_stream from_index to_index =
  typed_labeled_xml_stream

let nth_item_typed_labeled_xml_stream typed_labeled_xml_stream n =
 item_range_typed_labeled_xml_stream typed_labeled_xml_stream 0 n

(* NOTE: This behaves like a filter in total analogy to streamed XPath evaluation.
         The rest of the stream following the first item is continously consumed,
         although it is guaranteed to not contribute to the result. This must be
         done in order to ensure that other cursors pending on the same stream
         refer to the correct positions.

         We may want another static analysis capable of determining wether this
         consumption is really needed. - Michael *)
let first_item_typed_labeled_xml_stream typed_labeled_xml_stream =
  (*  nth_item_typed_labeled_xml_stream typed_labeled_xml_stream 1 *)

  let finished_first_item = ref false in
  let depth = ref 0 in

  let rec next_event () =
    match Cursor.cursor_peek typed_labeled_xml_stream with
      | None -> None
      | Some event ->
	  begin
	    Cursor.cursor_junk typed_labeled_xml_stream;
	    if !finished_first_item
	    then
	      next_event ()
	    else
	      begin
		begin
		  match event.se_desc with
		    | SAX_startDocument _
		    | SAX_startElement _
		    | SAX_startEncl ->
			begin
			  if !depth > 0 then unset_flag event;
			  depth := !depth + 1
			end
		    | SAX_endDocument
		    | SAX_endElement
		    | SAX_endEncl ->
			begin
			  depth := !depth - 1;
			  if !depth > 0 then unset_flag event
			end
		    | _ ->
			if !depth > 0 then unset_flag event
		end;
		if !depth = 0
		then
		  finished_first_item := true;
		Some event
	      end
	  end
  in
    Cursor.cursor_of_function next_event
