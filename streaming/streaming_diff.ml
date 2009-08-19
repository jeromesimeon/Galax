(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: streaming_diff.ml,v 1.4 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Streaming_diff
   Description:
     Compares two XML streams
*)

open Error

open Streaming_types

(***************)
(* Stream diff *)
(***************)

let diff_error () =
  raise (Query (Stream_Error "XML streams differ"))

(* Return the first stream up to the point where it differs with the
   second stream, in which case it raises an error *)

let rec peek_no_whitespace s =
  let e = Cursor.cursor_peek s in
  match e with
  | Some ed ->
      begin
	match ed.tse_desc with
	| (TSAX_characters c) ->
	  if Whitespace.whitespace_only c
	  then
	    begin
	      Cursor.cursor_junk s;
	    peek_no_whitespace s
	    end
	  else
	    e
	| _ -> e
      end
  | _ -> e

let compare_start_docs (xml_decl1,dtd1,baseuri1) (xml_decl2,dtd2,baseuri2) =
  match xml_decl1,xml_decl2 with
  | None,_ -> true
  | _,None -> true
  | (Some (xmlversion1,_,_),Some (xmlversion2,_,_)) ->
      xmlversion1 = xmlversion2

let compare_attribute_names_aux rsym1 rsym2 =
  let (_,uri1,ncname1) = Namespace_symbols.rattr_name rsym1 in
  let (_,uri2,ncname2) = Namespace_symbols.rattr_name rsym2 in
  compare (uri1,ncname1) (uri2,ncname2)

let compare_attribute_names (rsym1,_,_,_) (rsym2,_,_,_) =
  compare_attribute_names_aux rsym1 rsym2

let compare_characters c1 c2 =
  c1 = c2

let compare_attributes (rsym1,c1,_,_) (rsym2,c2,_,_) =
  ((compare_attribute_names_aux rsym1 rsym2) = 0) &&
  (compare_characters c1 c2)

let rec compare_attributes_lists satts1 satts2 =
  match (satts1,satts2) with
  | [],[] -> true
  | att1 :: satts1', att2 :: satts2' ->
      if (compare_attributes att1 att2)
      then
	compare_attributes_lists satts1' satts2'
      else
	false
  | _ -> false

let compare_start_elems (rsym1,atts1,_,_,_,_,_,_) (rsym2,atts2,_,_,_,_,_,_) =
  (Namespace_symbols.relem_equal rsym1 rsym2) &&
  let satts1 = List.sort compare_attribute_names atts1 in
  let satts2 = List.sort compare_attribute_names atts2 in
  (compare_attributes_lists satts1 satts2)

let compare_pis pi1 pi2 =
  pi1 = pi2

let compare_comments c1 c2 =
  c1 = c2

let compare_atomic_values a1 a2 =
  a1#atomic_value_eq a2

let next_diff_event s1 s2 () =
  let e1 = peek_no_whitespace s1 in
  let e2 = peek_no_whitespace s2 in
  let event_compare =
    match e1,e2 with
    | (None,None) ->
	true
    | (Some ed1,Some ed2) ->
	begin
	  match ed1.tse_desc,ed2.tse_desc with
	  | (TSAX_startDocument d1,TSAX_startDocument d2) ->
	      compare_start_docs d1 d2
	  | (TSAX_endDocument,TSAX_endDocument) ->
	      true
	  | (TSAX_startElement e1, TSAX_startElement e2) ->
	      compare_start_elems e1 e2
	  | (TSAX_endElement,TSAX_endElement) ->
	      true
	  | (TSAX_processingInstruction pi1, TSAX_processingInstruction pi2) ->
	      compare_pis pi1 pi2
	  | (TSAX_comment c1, TSAX_comment c2) ->
	      compare_comments c1 c2
	  | (TSAX_characters c1, TSAX_characters c2) ->
	      compare_characters c1 c2
	  | (TSAX_attribute a1, TSAX_attribute a2) ->
	      compare_attributes a1 a2
	  | (TSAX_atomicValue a1, TSAX_atomicValue a2) ->
	      compare_atomic_values a1 a2
	  | (TSAX_hole, TSAX_hole) ->
	      true
	  | (TSAX_startEncl, TSAX_startEncl) ->
	      true
	  | (TSAX_endEncl, TSAX_endEncl) ->
	      true
	  | _ ->
	      false
	end
    | _ ->
	false
  in
  begin
    if event_compare then () else diff_error ();
    Cursor.cursor_junk s1;
    Cursor.cursor_junk s2;
    e1
  end

let stream_diff s1 s2 =
  Cursor.cursor_of_function (next_diff_event s1 s2)

let stream_boolean_diff s1 s2 =
  try
    Streaming_ops.discard_typed_xml_stream (stream_diff s1 s2);
    true
  with
  | (Query (Stream_Error _)) -> false

