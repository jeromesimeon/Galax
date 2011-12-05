(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_validation_context.ml,v 1.18 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_validation_context
   Description:

     This module implements the context used during XML Schema
     validation.  
*)

open Error

open Namespace_symbols_util

open Xquery_common_ast
open Xquery_type_core_ast
open Xquery_type_core_ast_util


(**************************)
(* The validation context *)
(**************************)

type validation_context =
    { input_stream            : Streaming_types.xml_stream;        (* The input stream *)
      validation_schema       : cxschema;        	  	   (* The schema itself *)
      previous_content_models : (mixed * bool * cxtype) Stack.t;   (* Stack of remainding content models *)
      validation_nsenv        : Namespace_context.nsenv Stack.t;   (* Stack of namespace environments *)
      mutable buffered_events : Streaming_types.sax_event list }  (* Buffered events *)

(* Events *)

let next_buffered_event valid_ctxt =
  let current_buffered_events = valid_ctxt.buffered_events in
  match current_buffered_events with
  | [] ->
      None
  | event :: remaining_buffered_events ->
      begin
	valid_ctxt.buffered_events <- remaining_buffered_events;
	Some event
      end

let next_validation_event valid_ctxt =
  match next_buffered_event valid_ctxt with
  | Some buffered_event -> buffered_event
  | None -> Cursor.cursor_next valid_ctxt.input_stream
 

(* Push-pop-top on the stack *)  

let top_validate_ctxt valid_ctxt =
  if Stack.is_empty valid_ctxt.previous_content_models
  then
    raise (Query (Schema_Internal "Missing top-level content model during XML Schema validation"))
  else
    Stack.top valid_ctxt.previous_content_models

let push_validate_ctxt cmixed nilled cxtype valid_ctxt =
  Stack.push (cmixed,nilled,cxtype) valid_ctxt.previous_content_models

let pop_validate_ctxt valid_ctxt =
  if
    (Stack.is_empty valid_ctxt.validation_nsenv ||
    Stack.is_empty valid_ctxt.previous_content_models)
  then
    raise (Query (Schema_Internal "Cannot pop content model during XML Schema validation"))
  else
    Stack.pop valid_ctxt.previous_content_models

(* Namespace environment support *)

let get_namespace_env valid_ctxt =
  if Stack.is_empty valid_ctxt.validation_nsenv
  then
    raise (Query (Schema_Internal "Missing top-level content model during XML Schema validation"))
  else
    Stack.top valid_ctxt.validation_nsenv

let push_nsenv valid_ctxt nsenv =
  Stack.push nsenv valid_ctxt.validation_nsenv

let pop_nsenv valid_ctxt =
  ignore(Stack.pop valid_ctxt.validation_nsenv)

(* Get the currently processed content model *)

let get_current_content_model valid_ctxt =
  match (top_validate_ctxt valid_ctxt) with
  | (_,_,cxtype) -> cxtype

let has_mixed_content valid_ctxt =
  match (top_validate_ctxt valid_ctxt) with
  | (Mixed,_,_) -> true
  | (NonMixed,_,_) -> false

let has_been_nilled valid_ctxt =
  match (top_validate_ctxt valid_ctxt) with
  | (_,nilled,_) -> nilled

let get_cxschema valid_ctxt =
  valid_ctxt.validation_schema

(*******************)
(* Document events *)
(*******************)

(* Builds the top-level content model *)

(* Note:
     Inside the document node, the content model is a union of all
     toplevel element declarations.
   - Jerome
 *)

let ename_list cdecl_kind cdecl cenames =
  begin
    match cdecl with
    | (_,(CSubstitutesFor _,_,_)) -> 
	cenames
	  (* Note:
	     The top-level content model SHOULD NOT include
	     elements in a substitution group, otherwise the
	     content model is non deterministic. Those are
	     automatically included through the head of the
	     substitution group.
	     - Jerome
	   *)
    | (cename,(CNonSubstitutesFor,_,_)) ->
	cename :: cenames
  end

let rec build_top_content_model elist =
  match elist with
  | [] -> raise (Query (Validation "Schema should have at least one top-level element declaration"))
  | cename :: [] ->
      fmkcxtype (CElementRef cename) Finfo.bogus
  | cename :: elist' ->
      let ctype = build_top_content_model elist' in
      let etype =
	fmkcxtype (CElementRef cename) Finfo.bogus
      in
      fmkcxtype (CChoice (etype,ctype)) Finfo.bogus

let create_top_content_model celemdecls =
  let elist = SQNameHashtbl.fold ename_list celemdecls [] in
  build_top_content_model elist  

let push_document_event valid_ctxt =
  ()

let pop_document_event valid_ctxt =
  try
    let (cmixed,nilled,cxtype) = pop_validate_ctxt valid_ctxt in
    begin
      if (Stack.is_empty valid_ctxt.previous_content_models)
	  &&
	(Schema_judge.type_contains_empty cxtype)
      then
	()
      else
	raise (Query (Stream_Error "Mal-formed stream: closed document before validation was finished"))
    end
  with
  | _ ->
      raise (Query (Schema_Internal "Missing content model during XML Schema validation"))


(******************)
(* Element events *)
(******************)

let push_complex_element_event valid_ctxt sibling_cxtype cmixed nilled children_cxtype =
  let (sibling_cmixed,sibling_nilled,_) = pop_validate_ctxt valid_ctxt in
  begin
    push_validate_ctxt sibling_cmixed sibling_nilled sibling_cxtype valid_ctxt;
    push_validate_ctxt cmixed nilled children_cxtype valid_ctxt
  end

let push_simple_element_event valid_ctxt sibling_cxtype buffer =
  let (sibling_cmixed,sibling_nilled,_) = pop_validate_ctxt valid_ctxt in
  begin
    push_validate_ctxt sibling_cmixed sibling_nilled sibling_cxtype valid_ctxt;
    (* Simple types should be considered as mixed content as far as validation is concerned as some text nodes may appear in them. - Jerome *)
    push_validate_ctxt Mixed false Schema_util.text_content valid_ctxt;
    valid_ctxt.buffered_events <- buffer;
  end

let pop_element_event valid_ctxt =
  pop_nsenv valid_ctxt;
  let (cmixed,nilled,cxtype) = pop_validate_ctxt valid_ctxt in
  begin
    if (Schema_judge.type_contains_empty cxtype)
    then
      ()
    else
      raise (Query (Validation ("Closed element but content model is incomplete.  Missing: "^(Print_top.bprintf_cxtype "" cxtype))))
  end


(************************************)
(* Creates a new validation context *)
(************************************)

let build_validation_context init_nsenv cxschema sax_stream =
  let valid_ctxt =
    { input_stream            = sax_stream;
      validation_schema       = cxschema;
      previous_content_models = Stack.create ();
      validation_nsenv 	      = Stack.create ();
      buffered_events  	      = [] }
  in
  begin
    let ct = create_top_content_model cxschema.cxschema_element_declarations in
    push_nsenv valid_ctxt init_nsenv;
    push_validate_ctxt NonMixed false ct valid_ctxt;
    valid_ctxt
  end

