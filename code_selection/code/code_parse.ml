(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_parse.ml,v 1.18 2007/09/07 19:03:40 mff Exp $ *)

(* Module: Code_parse
   Description:
     This module contains code building for operators that implement
     parsing of XML documents.
*)

open Error 

open Xquery_common_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_physical_type_ast

open Algebra_type
open Cs_util_coercion

open Compile_context
open Code_selection_context

open Processing_context

open Ast_path_struct

(*****************************)
(* XPath evaluation - Michael*)
(*****************************)

let build_default_parse_stream_code code_ctxt doc_uri =
  if Debug.default_debug() then Debug.print_default_debug "Picking up streaming parse code";
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  (fun alg_ctxt () ->
     let lookup_document_from_io gio =
       fun proc_ctxt ->
	 let apply_load_document () =
	   (* 1. Open a SAX cursor on the input document *)
	   let (dtd_opt, xml_stream) = Streaming_parse.open_xml_stream_from_io gio in
	   (* 2. Resolve namespaces *)
	   let resolved_xml_stream = Streaming_ops.resolve_xml_stream xml_stream in
	   (* 3. Apply type annotations *)
	   let typed_xml_stream = Streaming_ops.typed_of_resolved_xml_stream resolved_xml_stream in
	     typed_xml_stream
	 in
	 (* Does that make sense? *)
	 Monitor.wrap_monitor proc_ctxt (Monitoring_context.Document_ParsingLoading_Phase (Parse_io.name_of_input_spec gio)) apply_load_document ()
     in
     lookup_document_from_io (Galax_io.Http_Input doc_uri) proc_ctxt)

let build_default_parse_code code_ctxt doc_uri name_indices =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  (fun alg_ctxt () -> 
    let alive_documents = Execution_context.alive_documents_from_algebra_context alg_ctxt in
    (Cursor.cursor_of_list 
       (Fn_doc.lookup_doc_function_with_index doc_uri name_indices (Some alive_documents) proc_ctxt)))

let is_streamable proc_ctxt http_method  =
  (*(proc_ctxt.Processing_context.treejoin_log  = Processing_context.TreeJoin ) &&
    (proc_ctxt.Processing_context.treejoin_phys = Processing_context.Streaming) &&*)
  match http_method with
    | Galax_url.ExternalSource _ -> false
    | _ -> true
	
let build_parse_code code_ctxt algop uri = 
  let _ = access_nosub algop.pdep_sub_expression in 
  let physop = Cs_util.get_physical_opname algop in 
    match physop with 
      | Xquery_physical_algebra_ast.POParse_Stream -> 
	  let fn = build_default_parse_stream_code code_ctxt uri in
	    (coerce_nodep fn coerce_unit_to_sax), code_ctxt
      | Xquery_physical_algebra_ast.POParse_Load -> 
	  let name_indices = get_all_name_indices_handler code_ctxt in
	  let fn = build_default_parse_code code_ctxt uri name_indices in
	    (coerce_nodep fn coerce_unit_to_item_cursor), code_ctxt
      | _ -> raise(Query(Code_Selection("Invalid physical operator in build_parse_code")))
	  
(* A streamed variant can be selected if
   1) streaming is enabled
   2) the employed http method allows streaming
   3) path analysis does not reveal prohibitive axis steps.
   - Michael *)
let select_physical_op code_ctxt algop uri =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  if (proc_ctxt.streaming)
  then
    begin
      let http_method = Galax_url.glx_decode_url uri in
      let annot = !(algop.annotation.path_annotation) in
      let is_streaming_prohibitive =
        match annot with 
        | Some a ->
            begin
	      match a.streaming_annot with
	      | Some paths ->
		  Alg_path_analysis.is_streaming_prohibitive paths
	      | None ->
		  raise (Query (Prototype ("Streaming is enabled; should have a path annotation for AOEParse(" ^ uri ^ ").")))
            end
        | None ->  raise (Query (Prototype ("Streaming is enabled; should have a path annotation for AOEParse(" ^ uri ^ ").")))
      in
      if ((is_streamable proc_ctxt http_method) && (not is_streaming_prohibitive))
      then Xquery_physical_algebra_ast.POParse_Stream
      else Xquery_physical_algebra_ast.POParse_Load
    end
  else Xquery_physical_algebra_ast.POParse_Load
