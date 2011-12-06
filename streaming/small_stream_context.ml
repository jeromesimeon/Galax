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
      stacked_sexpr_list           : (sax_event * sexpr list) Stack.t}


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

let push_elem_to_ss_context ss_context se remaining_list =
  match se with
  | SDocument (base_uri,nse) ->
      begin
	Stack.push (Streaming_util.fmkse_event SAX_endDocument Finfo.bogus, remaining_list) ss_context.stacked_sexpr_list;
	(Streaming_util.fmkse_event (SAX_startDocument (None,None, base_uri)) Finfo.bogus, nse)
      end
  | SElem (rqname, _, nsenv, sattributes, base_uri, nse, qname) ->
      begin
	(* Should we do something with attributes? *)
	let attributes = sattributes in
	(* Has element content is tricky here, as part of the content may be computed.
	   So, we are a bit conservative here and only return true if there is no
	   text content and no holes in the content. - Jerome *)
	let has_element_content =
	  List.for_all (fun x -> match x with | SHole | SText _ -> false | _ -> true) nse
	in
	let rsym =
	  match !qname with
	  | None -> raise (Query (Internal_Error "Missing element symbol in Small Stream"))
	  | Some rsym -> rsym
	in
	Stack.push (Streaming_util.fmkse_event SAX_endElement Finfo.bogus, remaining_list) ss_context.stacked_sexpr_list;
	(Streaming_util.fmkse_event
	   (SAX_startElement (uqname_of_rqname rqname,attributes,ref has_element_content, ref [],
			      ref (Some (rsym,base_uri,nsenv)), ref None)) Finfo.bogus, nse)
      end
  | SElem (rqname, Some bt, nsenv, sattributes, base_uri, nse, qname) ->
      raise (Query (Internal_Error "Binding table shouldn't appear during small stream processing"))
  | SText content ->
      (Streaming_util.fmkse_event (SAX_characters content) Finfo.bogus, remaining_list)
  | SPI (target,content) ->
      (Streaming_util.fmkse_event (SAX_processingInstruction (target,content)) Finfo.bogus, remaining_list)
  | SComment content ->
      (Streaming_util.fmkse_event (SAX_comment content) Finfo.bogus, remaining_list)
  | SHole ->
      (Streaming_util.fmkse_event SAX_hole Finfo.bogus, remaining_list)
      

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

let sattribute_of_rsattribute nsenv (uqname,content,special,rqname_opt,typed_opt) =
  raise (Failure "temp")
(*
  let rattr_sym = rattr_symbol rqname in
  (rqname,content,ref (Some rattr_sym)) *)

let rec sexpr_of_rsexpr nsenv rsexpr =
  match rsexpr with
  | SDocument (base_uri_option, rsexpr_list) ->
      let sexpr_list = List.map (sexpr_of_rsexpr nsenv) rsexpr_list in
      SDocument (base_uri_option, sexpr_list)
  | SElem (rqname,Some binding_table,_,rsattribute_forest,base_uri,rsexpr_list,_) ->
      let new_nsenv = add_all_ns nsenv binding_table in 
      let relem_sym = relem_symbol rqname in
      let sattribute_forest = List.map (sattribute_of_rsattribute new_nsenv) rsattribute_forest in
      let sexpr_list = List.map (sexpr_of_rsexpr new_nsenv) rsexpr_list in
      SElem (rqname,None,new_nsenv,sattribute_forest,base_uri,sexpr_list,ref (Some relem_sym))
  | SElem (rqname,None,_,rsattribute_forest,base_uri,rsexpr_list,_) ->
      raise (Query (Internal_Error "Missing Binding Table during small stream processing"))
  | SText _ | SPI _ | SComment _ | SHole ->
      rsexpr

