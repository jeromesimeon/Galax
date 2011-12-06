(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: streaming_xpath.ml,v 1.19 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Streaming_xpath
   Description:
     This module streaming XPath evaluation code, employing labeled
     XML token streams.
   - Michael *)


open Namespace_names
open Namespace_symbols

open Dm_types

open Xquery_common_ast
open Xquery_common_ast_util
open Xquery_algebra_ast

open Streaming_types
open Streaming_conv

open Sxp_context

open Error


(************************)
(* Node test evaluation *)
(************************)

let raise_unexpected_event_error () = raise (Query (Streaming_XPath "Unexpected event type encountered."))

let relem_symbol_of_sax_event sax_event =
  match sax_event.se_desc with
  | SAX_startElement (_, _, _, _, _, reo, _) ->
      begin
	match !reo with
	| None -> raise_unexpected_event_error ()
	| Some (relem_sym,_) -> relem_sym
      end
  | _ ->
      raise_unexpected_event_error ()

let relem_symbol_of_sax_event_and_type sax_event =
  match sax_event.se_desc with
  | SAX_startElement (_, _, _, _, _, reo, ret) ->
      let relem_sym =
	begin
	  match !reo with
	  | None -> raise_unexpected_event_error ()
	  | Some (relem_sym,_) -> relem_sym
	end
      in
      let type_annot =
	begin
	  match !ret with
	  | None -> raise_unexpected_event_error ()
	  | Some (_,ta,_) -> ta
	end
      in
      (relem_sym,type_annot)
  | _ ->
      raise_unexpected_event_error ()

let rattr_symbol_of_sax_event sax_event =
  match sax_event.se_desc with
  | SAX_attribute (_,_,rao,_) ->
      begin
	match !rao with
	| None -> raise_unexpected_event_error ()
	| Some rattr_sym -> rattr_sym
      end
  | _ ->
      raise_unexpected_event_error ()

let rattr_symbol_of_sax_event_and_type sax_event =
  match sax_event.se_desc with
  | SAX_attribute (_ ,_, rao, rat) ->
      let rattr_sym =
	begin
	  match !rao with
	  | None -> raise_unexpected_event_error ()
	  | Some rattr_sym -> rattr_sym
	end
      in
      let type_annotation =
	begin
	  match !rat with
	  | None -> raise_unexpected_event_error ()
	  | Some (ta,_) -> ta
	end
      in
      (rattr_sym,type_annotation)
  | _ ->
      raise_unexpected_event_error ()

let pi_ncname_of_sax_event sax_event =
  match sax_event.se_desc with
  | SAX_processingInstruction (pi_name,_) ->
      pi_name
  | _ ->
      raise_unexpected_event_error ()

let node_kind_of_sax_event sax_event =
  match sax_event.se_desc with
  | SAX_startDocument _
  | SAX_endDocument ->
      DocumentNodeKind
  | SAX_startElement _
  | SAX_endElement ->
      ElementNodeKind
  | SAX_processingInstruction _ ->
      ProcessingInstructionNodeKind
  | SAX_comment _ ->
      CommentNodeKind
  | SAX_characters _ ->
      TextNodeKind
  | SAX_attribute _ ->
      AttributeNodeKind
  | SAX_atomicValue _
  | SAX_startEncl
  | SAX_endEncl
  | SAX_hole ->
      raise_unexpected_event_error ()

open Code_util_matching

let access_ops_sax =
  { get_node_kind      		 = node_kind_of_sax_event;
    get_elem_node_name 		 = relem_symbol_of_sax_event;
    get_attr_node_name 		 = rattr_symbol_of_sax_event;
    get_elem_node_name_with_type = relem_symbol_of_sax_event_and_type;
    get_attr_node_name_with_type = rattr_symbol_of_sax_event_and_type;
    get_single_element_node      = (fun x -> raise (Query (Streaming_XPath "Cannot stream document tests with an element test")));
    get_document_node_children   = (fun x -> raise (Query (Streaming_XPath "Document node children acces not required for streaming")));
    get_element_node_children    = (fun x -> raise (Query (Streaming_XPath "Document node children acces not required for streaming")));
    get_pi_target                = pi_ncname_of_sax_event }

(* Performs the actual node test. *) 
let node_test_matches stat_ctxt axis anode_test ts =
  let cxschema = Typing_context.schema_from_static_context stat_ctxt in
  Dm_step.eval_node_test_gen access_ops_sax (Some cxschema) axis anode_test ts

(*****************************)
(* XPath axes implementation *)
(*****************************)

(*******************************************************)
(* Action dispatchers, one for each kind of XPath axis *)
(*******************************************************)

let dispatch_action_axis_child stat_ctxt labeled_event node_test sxp_context =
  let flag_set = get_flag labeled_event in
  let inside_another_match = is_inside_another_match sxp_context in
  let topmost_depth_is_one = topmost_label_depth_is_one sxp_context in
  let node_test_matches = node_test_matches stat_ctxt Child node_test labeled_event in
  let new_match = topmost_depth_is_one && node_test_matches in
  let new_match_or_inside_another_match = new_match || inside_another_match in
    {
      let_pass = new_match_or_inside_another_match;
      increase_outermost_match_depth = new_match_or_inside_another_match;
      push_label_depth_stack = flag_set;
      set_flag = new_match;
    }

let dispatch_action_axis_descendant stat_ctxt labeled_event node_test sxp_context =
  let flag_set = get_flag labeled_event in
  let inside_another_match = is_inside_another_match sxp_context in
  let topmost_depth_ge_one = topmost_label_depth_ge_one sxp_context in
  let node_test_matches = node_test_matches stat_ctxt Descendant node_test labeled_event in
  let new_match = topmost_depth_ge_one && node_test_matches in
  let new_match_or_inside_another_match = new_match || inside_another_match in
    {
      let_pass = new_match_or_inside_another_match;
      increase_outermost_match_depth = new_match_or_inside_another_match;
      push_label_depth_stack = flag_set;
      set_flag = new_match;
    }

let dispatch_action_axis_descendant_or_self stat_ctxt labeled_event node_test sxp_context =
  let inside_another_match = is_inside_another_match sxp_context in
  let node_test_matches = node_test_matches stat_ctxt Descendant_or_self node_test labeled_event in
  let inside_another_match_or_node_test_matches = inside_another_match || node_test_matches in
    {
      let_pass = inside_another_match_or_node_test_matches;
      increase_outermost_match_depth = inside_another_match_or_node_test_matches;
      push_label_depth_stack = false;
      set_flag = node_test_matches;
    }


(******************************)
(* Recursive event processing *)
(******************************)

let rec next_event input_stream node_test dispatch_action_fun sxp_context () =
(* Discovered a cursor_next without Stream.Failure catch!
   This seems to be the cause of a bug.

   Catching Stream.Failure here is nevertheless impossible,
   since the OCaml compiler is unable to apply tail recursion
   optimization, obviously (Stack Overflow for large documents).
   
   Decided to not catch it here, thus, but in separate functions
   wrapped around this one just below the exposed ones. - Michael 02|17|2006 *)

(* try *)

    let labeled_event = Cursor.cursor_next input_stream in
      
      match labeled_event.se_desc with
	| SAX_startDocument _
	| SAX_startElement _ ->
	    begin
	      let action = dispatch_action_fun labeled_event node_test sxp_context in
	      let _ = record_action action sxp_context in
		execute_action_start_event input_stream labeled_event node_test action dispatch_action_fun sxp_context
	    end
	      
	| SAX_endDocument
	| SAX_endElement ->
	    let action = get_recorded_action sxp_context in
	      execute_action_end_event input_stream labeled_event node_test action dispatch_action_fun sxp_context
		
	| SAX_processingInstruction _
	| SAX_comment _
	| SAX_characters _
	| SAX_attribute _ ->
	    let action = dispatch_action_fun labeled_event node_test sxp_context in
	      execute_action_leaf input_stream labeled_event node_test action dispatch_action_fun sxp_context
		
	| SAX_atomicValue _ ->
	    (* Is that the right kind of exception? *)
	    raise (Query (Streaming_XPath "Cannot apply a tree join on an atomic value."))
	      
	| SAX_hole
	| SAX_startEncl
	| SAX_endEncl ->
	    raise_unexpected_event_error ()
(*	      
  with
    | Stream.Failure -> None
*)


(*************************)
(* Event specific action *)
(*************************)

and execute_action_leaf input_stream labeled_event node_test action dispatch_action_fun sxp_context =
  if action.set_flag
  then
    set_flag labeled_event
  else
    unset_flag labeled_event;
  
  if action.let_pass
  then
    Some labeled_event
  else
    next_event input_stream node_test dispatch_action_fun sxp_context ()
      
and execute_action_start_event input_stream labeled_event node_test action dispatch_action_fun sxp_context =
  if action.increase_outermost_match_depth
  then
    increase_outermost_match_depth sxp_context;

  begin
    if action.set_flag
    then
      set_flag labeled_event
    else
      unset_flag labeled_event
  end;

  increase_topmost_label_depth sxp_context;

  if action.push_label_depth_stack
  then
    push_label_depth_stack sxp_context;

  if action.let_pass
  then
    Some labeled_event
  else
    next_event input_stream node_test dispatch_action_fun sxp_context ()

(* Simply performs actions inverse to those recorded. *)      
and execute_action_end_event input_stream labeled_event node_test action dispatch_action_fun sxp_context =
  if action.increase_outermost_match_depth
  then
    decrease_outermost_match_depth sxp_context;

  if action.push_label_depth_stack
  then
      ignore (
	try
	  pop_label_depth_stack sxp_context
	with Stack.Empty ->
	  begin
	    print_string "execute_action_end_event: Stack.empty\n";
	    raise Stack.Empty
	  end
      );

  decrease_topmost_label_depth sxp_context;

  if action.let_pass
  then
    Some labeled_event
  else
    next_event input_stream node_test dispatch_action_fun sxp_context ()


(* Hackish - does not quite fit into the action/dispatch framework - Michael 05|12|2006 *)
let rec next_event_attribute input_stream node_test stat_ctxt sxp_context () =
    let labeled_event = Cursor.cursor_next input_stream in
      
      match labeled_event.se_desc with
	| SAX_startElement (_, attributes, _, _, _, _, _) 
	  when get_flag labeled_event ->
	    begin
	      let f a = node_test_matches stat_ctxt Attribute node_test a in
	      let mk a = 
		let labeled_event =
		  Streaming_util.fmkse_event (SAX_attribute a) Finfo.bogus in
		let _ = set_flag labeled_event in
		labeled_event
	      in
	      let attribute_events = List.map mk attributes in
	      let matches = List.filter f attribute_events in
		match matches with
		  | hd :: tl -> Some hd
		  | [] -> next_event_attribute input_stream node_test stat_ctxt sxp_context ()
	    end
	| SAX_startElement _
	| SAX_startDocument _
	| SAX_endDocument
	| SAX_endElement
	| SAX_processingInstruction _
	| SAX_comment _
	| SAX_characters _
	| SAX_attribute _ ->
	    next_event_attribute input_stream node_test stat_ctxt sxp_context ()
		
	| SAX_atomicValue _ ->
	    (* Is that the right kind of exception? *)
	    raise (Query (Streaming_XPath "Cannot apply a tree join on an atomic value."))
	      
	| SAX_hole
	| SAX_startEncl
	| SAX_endEncl ->
	    raise_unexpected_event_error ()
  

(**********************)
(* Context generation *)
(**********************)

(* Hackish - does not quite fit into the action/dispatch framework - Michael 05|12|2006 *)
let xpath_axis_attribute_internal stat_ctxt typed_labeled_stream node_test sxp_context =
  let f () = try next_event_attribute typed_labeled_stream node_test stat_ctxt sxp_context () with
    | Stream.Failure -> None
  in
    Cursor.cursor_of_function f


let xpath_axis_descendant_or_self_internal stat_ctxt typed_labeled_stream node_test sxp_context =
  let f () = try next_event typed_labeled_stream node_test (dispatch_action_axis_descendant_or_self stat_ctxt) sxp_context () with
    | Stream.Failure -> None
  in
    Cursor.cursor_of_function f

(* s.a. - Michael 02|17|2006 *)
(* Cursor.cursor_of_function (next_event typed_labeled_stream node_test (dispatch_action_axis_descendant_or_self stat_ctxt) sxp_context) *)

    
let xpath_axis_descendant_internal stat_ctxt typed_labeled_stream node_test sxp_context =
  let f () = try next_event typed_labeled_stream node_test (dispatch_action_axis_descendant stat_ctxt) sxp_context () with
    | Stream.Failure -> None
  in
    Cursor.cursor_of_function f

(* s.a. - Michael 02|17|2006 *)
(* Cursor.cursor_of_function (next_event typed_labeled_stream node_test (dispatch_action_axis_descendant stat_ctxt) sxp_context) *)

    
let xpath_axis_child_internal stat_ctxt typed_labeled_stream node_test sxp_context =
  let f () = try next_event typed_labeled_stream node_test (dispatch_action_axis_child stat_ctxt) sxp_context () with
    | Stream.Failure -> None
  in
    Cursor.cursor_of_function f

(* s.a. - Michael 02|17|2006 *)
(* Cursor.cursor_of_function (next_event typed_labeled_stream node_test (dispatch_action_axis_child stat_ctxt) sxp_context)*)


(***********)
(* Exposed *)
(***********)

let xpath_axis_attribute (stat_ctxt : Typing_context.static_context) typed_labeled_input_stream node_test =
  let sxp_context = default_sxp_context () in
  xpath_axis_attribute_internal stat_ctxt typed_labeled_input_stream node_test sxp_context

let xpath_axis_descendant_or_self (stat_ctxt : Typing_context.static_context) typed_labeled_input_stream node_test =
  let sxp_context = default_sxp_context () in
  xpath_axis_descendant_or_self_internal stat_ctxt typed_labeled_input_stream node_test sxp_context
      
let xpath_axis_descendant stat_ctxt typed_labeled_input_stream node_test =
  let sxp_context = default_sxp_context () in
  xpath_axis_descendant_internal stat_ctxt typed_labeled_input_stream node_test sxp_context
      
let xpath_axis_child stat_ctxt typed_labeled_input_stream node_test =
  let sxp_context = default_sxp_context () in
  xpath_axis_child_internal stat_ctxt typed_labeled_input_stream node_test sxp_context

