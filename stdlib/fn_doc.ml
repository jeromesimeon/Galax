(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: fn_doc.ml,v 1.32 2007/09/07 19:03:40 mff Exp $ *)

(* Module: Fn_doc
   Description:
     This module implements the fn:doc() function.
*)

open Error
open Galax_io

open Datatypes

open Dm

open Monitoring_context
open Processing_context

(* Note:
     This module performs two main tasks: (1) it dispatches URL's to
     the appropriate source (HTTP, file system, back-end store, etc.),
     and (2) if necessary it 'stores' the resulting document locally
     in order to preserve the original node identity of the document.
   - Jerome
 *)


(*************************)
(* Back-end registration *)
(*************************)

type back_end_call =
    Processing_context.processing_context -> string * string option * string -> Physical_value.item list

let back_ends =
  ref []

let is_registered_back_end me =
  List.mem_assoc me !back_ends

let lookup_back_end_fun me =
  try
    List.assoc me !back_ends
  with
  | _ ->
      raise (Query (Internal_Error ("Accessing unregistered backend: " ^ me ^ ", how come?")))

let register_back_end me f =
  if is_registered_back_end me
  then
    raise (Query (Internal_Error ("Backend : " ^ me ^ " already registered")))
  else
    begin
      Galax_url.register_method me;       (* Do not forget to register the backend as a URI method *)
      back_ends := (me,f) :: !back_ends  (* Add the function to the list of registered back-ends *)
    end

(* Alive documents *)

type alive_documents = (Datatypes.xs_string, Physical_value.item list) Hashtbl.t

let build_alive_documents_table () = Hashtbl.create 59

let build_dummy_alive_documents_table () = Hashtbl.create 1
(* Nicola: used only when we want an empty documents table *)

let alive_documents_table_mem alive_documents file =
  Hashtbl.mem alive_documents file

let alive_documents_table_get alive_documents file =
  Hashtbl.find alive_documents file
let alive_documents_table_put alive_documents file doc =
  Hashtbl.add alive_documents file doc

let merge_alive_documents ad1 ad2 =
  Gmisc.merge_hashtable ad1 ad2


(*********************)
(* Http/file sources *)
(*********************)

let lookup_document_from_io_with_index gio name_indices entity_kind alive_ctxt_opt proc_ctxt =
  let apply_load_document () =
    (* 1. Set the document id and nodeid context *)
    let uri_string =
      match gio with
      | File_Input uri_string
      | Http_Input uri_string -> uri_string
      | _ -> raise (Query (Internal_Error "Cannot call the fn:doc() function on something else than a file or URI"))
    in
    let get_data () = 
	  let result_node =
	    (* 1. Open a SAX cursor on the input document *)
	    let (dtd_opt, xml_stream) = Streaming_parse.open_xml_stream_from_io gio (* entity kind *) in
	      (* 2. Resolve namespaces *)
	    let resolved_xml_stream = Streaming_ops.resolve_xml_stream xml_stream in
	      (* 3. Apply type annotations *)
	    let typed_xml_stream = Streaming_ops.typed_of_resolved_xml_stream resolved_xml_stream in
	      (* If the doc id is not found, then build a new one and register it *)
	    let docid = Galax_nodeid.new_docid () in
	    let nodeid_context = Nodeid_context.default_nodeid_context () in
	  (* 4. Load in the data model *)
	      Physical_index_load.load_xml_document_from_typed_stream_for_docid nodeid_context docid name_indices typed_xml_stream
	  in
	    begin
          begin
            match alive_ctxt_opt with
              | None -> ()
              | Some alive_ctxt -> Hashtbl.add alive_ctxt uri_string result_node;
          end;
	      result_node
	    end
    in
      begin
        match alive_ctxt_opt with
          | None -> get_data () 
          | Some alive_ctxt -> 
              try
                Hashtbl.find alive_ctxt uri_string
              with
                | Not_found -> get_data () 
      end
  in
    Monitor.wrap_monitor proc_ctxt (Document_ParsingLoading_Phase (Parse_io.name_of_input_spec gio)) apply_load_document ()
      
let lookup_document_from_io gio =
  let name_indices = Physical_name_index.no_name_indices in
  lookup_document_from_io_with_index gio name_indices Document_entity

let lookup_entity_with_index uri name_indices entity_kind =
  let http_method = Galax_url.glx_decode_url uri in
  match http_method with
  | Galax_url.File _
  | Galax_url.Http _ ->
      lookup_document_from_io_with_index (Galax_io.Http_Input uri) name_indices entity_kind
  | Galax_url.ExternalSource (me,host,port,local) ->
      let back_end_fun = lookup_back_end_fun me in
        (fun alive_ctxt -> fun proc_ctxt -> back_end_fun proc_ctxt (host,port,local))

let lookup_doc_function_with_index uri name_indices ad =
  lookup_entity_with_index uri name_indices Document_entity ad 

let lookup_doc_function uri =
  let name_indices = Physical_name_index.no_name_indices in
  lookup_entity_with_index uri name_indices Document_entity

let lookup_doc_function_no_table uri =
  let ad = build_alive_documents_table () in
  let name_indices = Physical_name_index.no_name_indices in
    lookup_entity_with_index uri name_indices Document_entity (Some ad)

(*
   Collection functions
*)

let lookup_collection_function uri =
  let name_indices = Physical_name_index.no_name_indices in
  lookup_entity_with_index uri name_indices Document_fragment

