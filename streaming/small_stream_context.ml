(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: small_stream_context.ml,v 1.12 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Small_stream_context
   Description:
     This module implements the context used when building a small
     stream from an element-construction expression.
*)

open Error

open Namespace_names
open Namespace_symbols
open Namespace_resolve
open Namespace_context


open Small_stream_ast

open Streaming_types

(****************************)
(* The small stream context *)
(****************************)

type ss_context =
    { mutable current_sexpr_list   : sexpr list;
      mutable remaining_sexpr_list : sexpr list;
      stacked_sexpr_list           : (resolved_sax_event * sexpr list) Stack.t}


(**************************************)
(* Creates a new small stream context *)
(**************************************)

let build_ss_context cel =
  { current_sexpr_list = cel;
    remaining_sexpr_list = [];
    stacked_sexpr_list = Stack.create () }


(******************************************)
(* Operations on the small stream context *)
(******************************************)

(* Access the current expr list *)

let get_current_sexpr_list ss_context =
  ss_context.current_sexpr_list

(* Access the remaining expr list *)

let get_remaining_sexpr_list ss_context =
  ss_context.remaining_sexpr_list

(* Replace the current sequence *)

let replace_current_sexpr_list ss_context new_sexpr_list =
  ss_context.current_sexpr_list <- new_sexpr_list

(* Push a context (getting inside of an element/document node) *)

let convert_sattribute (rattr_sym,content) =
  (rattr_sym,content)

let convert_sattributes sattrs =
  (List.map convert_sattribute sattrs)

let push_elem_to_ss_context ss_context se remaining_list =
  match se with
  | SDocument (base_uri,nse) ->
      begin
	Stack.push (Streaming_util.fmkrse_event RSAX_endDocument Finfo.bogus, remaining_list) ss_context.stacked_sexpr_list;
	(Streaming_util.fmkrse_event (RSAX_startDocument (None,None, base_uri)) Finfo.bogus, nse)
      end
  | SElem (rsym, nsenv, sattributes, base_uri, nse) ->
      begin
	let attributes = convert_sattributes sattributes in
	(* Has element content is tricky here, as part of the content may be computed.
	   So, we are a bit conservative here and only return true if there is no
	   text content and no holes in the content. - Jerome *)
	let has_element_content =
	  List.for_all (fun x -> match x with | SHole | SText _ -> false | _ -> true) nse
	in
	Stack.push (Streaming_util.fmkrse_event RSAX_endElement Finfo.bogus, remaining_list) ss_context.stacked_sexpr_list;
	(Streaming_util.fmkrse_event (RSAX_startElement (rsym,attributes,has_element_content,base_uri,nsenv)) Finfo.bogus, nse)
      end
  | SText content ->
      (Streaming_util.fmkrse_event (RSAX_characters content) Finfo.bogus, remaining_list)
  | SPI (target,content) ->
      (Streaming_util.fmkrse_event (RSAX_processingInstruction (target,content)) Finfo.bogus, remaining_list)
  | SComment content ->
      (Streaming_util.fmkrse_event (RSAX_comment content) Finfo.bogus, remaining_list)
  | SHole ->
      (Streaming_util.fmkrse_event RSAX_hole Finfo.bogus, remaining_list)
      

(* Pop a context (getting outside of an element/document node) *)

let pop_elem_from_ss_context ss_context =
  try
    Some (Stack.pop ss_context.stacked_sexpr_list)
  with
  | Stack.Empty ->
      None

(******************************)
(* Simple stream constructors *)
(******************************)

(* Builds a small XML stream with holes from an AST with element
   construction operations *)

let next_event_of_sexpr ss_context =
  let (event,rest) =
    match get_current_sexpr_list ss_context with
    | [] ->
	begin
	  match pop_elem_from_ss_context ss_context with
	  | Some (event,rest) ->
	      (Some event,rest)
	  | None ->
	      (None,[])
	end
    | sexpr :: rest_of_sexpr_list ->
	let (event,nested_sexpr_list) =
	  push_elem_to_ss_context ss_context sexpr rest_of_sexpr_list
	in
	(Some event, nested_sexpr_list)
  in
  begin
    replace_current_sexpr_list ss_context rest;
    event
  end

let next_event_of_sexpr_aux ss_context n =
  next_event_of_sexpr ss_context

let resolved_xml_stream_of_sexpr sexpr =
  let ss_context = build_ss_context [sexpr] in
  Cursor.cursor_of_function (next_event_of_sexpr_aux ss_context)

(* Builds a sexpr out of an unresolved sexpr *)

let sattribute_of_rsattribute nsenv (rqname,content) =
  let rattr_sym = rattr_symbol rqname in
  (rattr_sym,content)

let rec sexpr_of_rsexpr nsenv rsexpr =
  match rsexpr with
  | Small_stream_ast.RSDocument (base_uri_option, rsexpr_list) ->
      let sexpr_list = List.map (sexpr_of_rsexpr nsenv) rsexpr_list in
      Small_stream_ast.SDocument (base_uri_option, sexpr_list)
  | Small_stream_ast.RSElem (rqname,binding_table,rsattribute_forest,base_uri,rsexpr_list) ->
      let new_nsenv = add_all_ns nsenv binding_table in 
      let relem_sym = relem_symbol rqname in
      let sattribute_forest = List.map (sattribute_of_rsattribute new_nsenv) rsattribute_forest in
      let sexpr_list = List.map (sexpr_of_rsexpr new_nsenv) rsexpr_list in
      Small_stream_ast.SElem (relem_sym,new_nsenv,sattribute_forest,base_uri,sexpr_list)
  | Small_stream_ast.RSText text ->
      Small_stream_ast.SText text
  | Small_stream_ast.RSPI (target,content) ->
      Small_stream_ast.SPI (target,content)
  | Small_stream_ast.RSComment comment ->
      Small_stream_ast.SComment comment
  | Small_stream_ast.RSHole ->
      Small_stream_ast.SHole

