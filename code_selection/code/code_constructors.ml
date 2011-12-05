(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_constructors.ml,v 1.19 2007/08/17 18:28:54 simeon Exp $ *)

(* Module: Code_constructors
   Description:
     This module contains code building for constructors.
*)

open Cs_util_coercion

open Datatypes
open Dm_atomic
open Physical_sequence
open Physical_value

open Processing_context
open Norm_context
open Typing_context
open Code_selection_context

open Algebra_type
open Xquery_physical_algebra_ast

open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_common_ast

open Ast_path_struct

open Error

(* CESeq *)
let build_default_ceseq_code code_ctxt =
  (fun alg_ctxt ae1 ae2 -> ae1 @ ae2)

let build_streaming_ceseq_code code_ctxt =
  (fun alg_ctxt ae1 ae2 -> Streaming_constructors.sequence_constructor ae1 ae2)

let build_default_ceimperativeseq_code code_ctxt =
  (fun alg_ctxt ae1 ae2 -> ae2)

let build_streaming_ceimperativeseq_code code_ctxt =
  (fun alg_ctxt ae1 ae2 -> ae2)

let build_seq_code code_ctxt algop =
  let _ = access_nosub algop.pdep_sub_expression in 
  let physop = Cs_util.get_physical_opname algop in
  match physop with
  | POSeq_Stream ->
      let fn = build_streaming_ceseq_code code_ctxt in
      (coerce_nodep fn coerce_binary_sax_to_sax), code_ctxt
  | POSeq_Materialized ->
      let fn = build_default_ceseq_code code_ctxt in
      (coerce_nodep fn coerce_binary_item_list_to_item_list), code_ctxt
  | _ -> raise(Query(Code_Selection("Invalid physical operator in build_seq_code")))

let build_imperative_seq_code code_ctxt algop =
  let _ = access_nosub algop.pdep_sub_expression in 
  let physop = Cs_util.get_physical_opname algop in
  match physop with
  | POImperativeSeq_Stream ->
      let fn = build_streaming_ceimperativeseq_code code_ctxt in
      (coerce_nodep fn coerce_binary_sax_to_sax), code_ctxt
  | POImperativeSeq_Materialized ->
      let fn = build_default_ceimperativeseq_code code_ctxt in
      (coerce_nodep fn coerce_binary_item_list_to_item_list), code_ctxt
  | _ -> raise(Query(Code_Selection("Invalid physical operator in build_imperative_seq_code")))


(* CEEmpty *)
let build_default_ceempty_code code_ctxt = 
  (fun alg_ctxt () -> [])

let build_empty_code code_ctxt algop =
  let fn = build_default_ceempty_code code_ctxt in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_unit_to_item_list), code_ctxt

(* CEError *)
let build_default_ceerror_code code_ctxt =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  (fun alg_ctxt v ->
    let vl =  Cursor.list_of_cursor "Cs_code.build_default_ceerror_code" (Cursor.cursor_array_fold v) in 
    Fn_error.raise_error proc_ctxt vl
  )

let build_error_code code_ctxt algop =
  let fn = build_default_ceerror_code code_ctxt in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_many_item_cursor_to_physical_value), code_ctxt

(* CEScalar *)
let build_default_cescalar_code code_ctxt s =
  let atomic_value = 
    match s with
    | IntegerLiteral i -> (new atomicInteger i)
    | DecimalLiteral d -> (new atomicDecimal d)
    | DoubleLiteral d -> (new atomicDouble d)
    | StringLiteral s -> (new atomicString s)
    | BooleanLiteral b -> (new atomicBoolean b)
    | URILiteral u -> (new atomicAnyURI u)
  in
  let dm_value = [Item_Atomic atomic_value] in
  (fun alg_ctxt () -> dm_value)

let build_scalar_code code_ctxt algop dmv =
  let fn = build_default_cescalar_code code_ctxt dmv in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_unit_to_item_list), code_ctxt

(* CEDocument *)
let build_default_document_code code_ctxt =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let mod_proc_ctxt = Norm_context.module_context_from_norm_context norm_ctxt in
  let base_uri = Processing_context.get_base_uri mod_proc_ctxt in
  let base_uri_item = Dm_atomic_util.uri_dm_of_uri base_uri in
  (fun alg_ctxt content_stream ->
    Streaming_constructors.document_constructor base_uri_item content_stream)

let build_document_code code_ctxt algop =
  let fn = build_default_document_code code_ctxt in
  let _ = access_nosub algop.pdep_sub_expression in
  (coerce_nodep fn coerce_unary_sax_to_sax), code_ctxt

(* CEPI *)
let build_default_cepi_code code_ctxt target pi_content =
  (fun alg_ctxt () ->
    let dmv = sequence_of_singleton (Item_Atomic (new atomicUntyped pi_content)) in
    let sdmv = Physical_xml_value.sax_value_of_dom_value dmv in
    Streaming_constructors.pi_constructor false target sdmv)

let build_pi_code code_ctxt algop (ncname,str) =
  let fn = build_default_cepi_code code_ctxt ncname str in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_unit_to_sax), code_ctxt

(* CEAnyPI *)
let build_default_cepicomputed_code code_ctxt  =
  (fun alg_ctxt pv1 pv2 ->
    let stat_ctxt = static_context_from_code_selection_context code_ctxt in
    let norm_ctxt = norm_context_from_stat_context stat_ctxt in
    let nsenv     = nsenv_from_norm_context norm_ctxt in
    let target = ((Physical_util.get_singleton_atomic pv1)#cast_to nsenv Namespace_symbols_builtin.xs_string ATString)#getAtomicString() in
    Streaming_constructors.pi_constructor true target pv2)

let build_picomputed_code code_ctxt algop =
  let fn = build_default_cepicomputed_code code_ctxt in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_binary_item_cursor_and_sax_to_sax), code_ctxt

(* CEComment *)
let build_default_cecomment_code code_ctxt c =
  (fun alg_ctxt () ->
    let dmv = sequence_of_singleton  (Item_Atomic (new atomicUntyped c)) in
    let sdmv = Physical_xml_value.sax_value_of_dom_value dmv in
    Streaming_constructors.comment_constructor sdmv)

let build_comment_code code_ctxt algop str =
  let fn = build_default_cecomment_code code_ctxt str in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_unit_to_sax), code_ctxt

let build_default_cecommentcomputed_code code_ctxt =
  (fun alg_ctxt dmv ->
     Streaming_constructors.comment_constructor dmv)

let build_commentcomputed_code code_ctxt algop =
  let fn = build_default_cecommentcomputed_code code_ctxt in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_unary_sax_to_sax), code_ctxt

(* CEText *)
let build_default_cetext_code code_ctxt t =
  (fun alg_ctxt () -> 
     let dmv = (sequence_of_singleton (Item_Atomic (new atomicUntyped t))) in
     let sdmv = Physical_xml_value.sax_value_of_dom_value dmv in
     (Streaming_constructors.text_constructor sdmv))

let build_default_charref_code code_ctxt i =
  (fun alg_ctxt () ->
    (Streaming_constructors.charref_constructor i))

let build_text_code code_ctxt algop text =
  let fn = build_default_cetext_code code_ctxt text in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_unit_to_sax),  code_ctxt

let build_charref_code code_ctxt algop i =
  let fn = build_default_charref_code code_ctxt i in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_unit_to_sax),  code_ctxt

let build_default_cetextcomputed_code code_ctxt =
  (fun alg_ctxt dmv ->
    Streaming_constructors.text_constructor dmv)

let build_textcomputed_code code_ctxt algop =
  let fn = build_default_cetextcomputed_code code_ctxt in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_unary_sax_to_sax), code_ctxt

(* CEElem *)
let build_default_ceelem_code code_ctxt sym nsenv =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let mod_proc_ctxt = Norm_context.module_context_from_norm_context norm_ctxt in
  let base_uri = Processing_context.get_base_uri mod_proc_ctxt in
  let base_uri_item = Dm_atomic_util.uri_dm_of_uri base_uri in
  (fun alg_ctxt content_streams ->
    let merged_stream = Cursor.cursor_array_fold content_streams in
    Streaming_constructors.element_constructor base_uri_item sym nsenv merged_stream)

let build_elem_code code_ctxt algop (relem_sym,nsenv) =
  let fn = build_default_ceelem_code code_ctxt relem_sym nsenv in
  let _ = access_nosub algop.pdep_sub_expression in
  (coerce_nodep fn coerce_many_sax_to_sax), code_ctxt

(* CEAnyElem *)

let build_default_ceanyelem_code code_ctxt nsenv1 nsenv2 = 
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let mod_proc_ctxt = Norm_context.module_context_from_norm_context norm_ctxt in
  let base_uri = Processing_context.get_base_uri mod_proc_ctxt in
  let base_uri_item = Dm_atomic_util.uri_dm_of_uri base_uri in
  (fun alg_ctxt pv1 content_stream -> 
    (*

       If the atomized value of the name expression is of type
       xs:QName, that expanded QName is used as the node-name property
       of the constructed element, retaining the prefix part of the
       QName.

       If the atomized value of the name expression is of type
       xs:string or xdt:untypedAtomic, that value is converted to an
       expanded QName. If the string value contains a namespace
       prefix, that prefix is resolved to a namespace URI using the
       statically known namespaces. If the string value contains no
       namespace prefix, it is treated as a local name in the default
       element/type namespace. The resulting expanded QName is used as
       the node-name property of the constructed element, retaining
       the prefix part of the QName. If conversion of the atomized
       name expression to an expanded QName is not successful, a
       dynamic error is raised [err:XQDY0074].

    *)
   (* Here use the statically-known namespaces *)
   let sym = Code_util.get_computed_node_name nsenv1 pv1 in 
   (* Here use the in-scope namespaces *)
   Streaming_constructors.element_constructor base_uri_item sym nsenv2 content_stream)

let build_anyelem_code code_ctxt algop nsenv1 nsenv2 =
  let fn = build_default_ceanyelem_code code_ctxt nsenv1 nsenv2 in
  let _ = access_nosub algop.pdep_sub_expression in
  (coerce_nodep fn coerce_binary_item_cursor_and_sax_to_sax), code_ctxt

(* CEAttr *)
let build_default_ceattr_code code_ctxt sym nsenv = 
  (fun alg_ctxt content_streams ->
    let merged_stream = Cursor.cursor_array_fold content_streams in
    Streaming_constructors.attribute_constructor sym nsenv merged_stream)

let build_attr_code code_ctxt algop rattr_sym nsenv =
  let fn = build_default_ceattr_code code_ctxt rattr_sym nsenv in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_many_sax_to_sax), code_ctxt

(* CEAnyAttr *)
let build_default_ceanyattr_code code_ctxt nsenv =
  (fun alg_ctxt pv1 content_stream ->
    let sym = Code_util.get_computed_node_name nsenv pv1 in
    Streaming_constructors.attribute_constructor sym nsenv content_stream)

let build_anyattr_code code_ctxt algop nsenv =
  let fn = build_default_ceanyattr_code code_ctxt nsenv in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_binary_item_cursor_and_sax_to_sax), code_ctxt

(* A streamed variant can be selected if
   1) streaming is enabled
   2) path analysis does not reveal prohibitive axis steps.
   - Michael *)
let select_physical_op code_ctxt indep_signature algop =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  (* TEST HERE - JEROME *)
  match algop.palgop_expr_name with
  | AOESeq ->
      begin
	let t = Xquery_physical_type_ast_util.access_two_non_discarded_xml_types indep_signature in
	match t with
	| (Xquery_physical_type_ast.PT_Sax Xquery_physical_type_ast.PT_Stream,Xquery_physical_type_ast.PT_Sax Xquery_physical_type_ast.PT_Stream) -> POSeq_Stream
	| _ -> POSeq_Materialized
      end
  | AOEImperativeSeq -> (* Nicola: Actually, semicolon might allow for more streaming. This part should be rewritten. *)
      begin
	let t = Xquery_physical_type_ast_util.access_two_non_discarded_xml_types indep_signature in
	match t with
	| (Xquery_physical_type_ast.PT_Sax Xquery_physical_type_ast.PT_Stream,Xquery_physical_type_ast.PT_Sax Xquery_physical_type_ast.PT_Stream) -> POImperativeSeq_Stream
	| _ -> POImperativeSeq_Materialized
      end
  | _ ->
      (* END TEST *)
      if (proc_ctxt.streaming)
      then
	begin
	  let annot = !(algop.annotation.path_annotation) in
	  let is_streaming_prohibitive =
            match annot with 
            | Some a ->
		begin
	          match a.streaming_annot with
	          | Some paths ->
	              Alg_path_analysis.is_streaming_prohibitive paths
	          | None ->
	              raise (Query (Prototype "Streaming is enabled; should have a path annotation for constructor."))
		end
            | None ->  raise (Query (Prototype "Streaming is enabled; should have a path annotation for constructor."))
	  in
	  if (not is_streaming_prohibitive)
	  then
	    begin
	      match algop.palgop_expr_name with
	      | AOEDocument -> PODocument_Stream
	      | AOEPI _ -> POPI_Stream
	      | AOEPIComputed -> POPIComputed_Stream
	      | AOEComment _  -> POComment_Stream
	      | AOECommentComputed -> POCommentComputed_Stream
	      | AOEText _ -> POText_Stream
	      | AOECharRef _ -> POText_Stream
	      | AOETextComputed -> POTextComputed_Stream
	      | AOEElem _ -> POElem_Stream
	      | AOEAnyElem _ -> POAnyElem_Stream
	      | AOEAttr _ -> POAttr_Stream
	      | AOEAnyAttr _ -> POAnyAttr_Stream
	      | _ -> 
		  raise (Query (Prototype "Expected palgop_expr_name to be a constructor in Code_constructors.select_physical_op."))
	    end
	  else
	    begin
	      match algop.palgop_expr_name with
	      | AOEDocument -> PODocument_Materialized
	      | AOEPI _ -> POPI_Materialized
	      | AOEPIComputed -> POPIComputed_Materialized
	      | AOEComment _  -> POComment_Materialized
	      | AOECommentComputed -> POCommentComputed_Materialized
	      | AOEText _ | AOECharRef _ -> POText_Materialized
	      | AOETextComputed -> POTextComputed_Materialized
	      | AOEElem _ -> POElem_Materialized
	      | AOEAnyElem _ -> POAnyElem_Materialized
	      | AOEAttr _ -> POAttr_Materialized
	      | AOEAnyAttr _ -> POAnyAttr_Materialized
	      | _ -> 
		  raise (Query (Prototype "Expected palgop_expr_name to be a constructor in Code_constructors.select_physical_op."))
	    end
	end
      else
	(* This mimics the former behaviour of always assuming
	   constructors as being streamed. *)
	begin
	  match algop.palgop_expr_name with
	  | AOEDocument -> PODocument_Stream
	  | AOEPI _ -> POPI_Stream
	  | AOEPIComputed -> POPIComputed_Stream
	  | AOEComment _  -> POComment_Stream
	  | AOECommentComputed -> POCommentComputed_Stream
	  | AOEText _ | AOECharRef _ -> POText_Stream
	  | AOETextComputed -> POTextComputed_Stream
	  | AOEElem _ -> POElem_Stream
	  | AOEAnyElem _ -> POAnyElem_Stream
	  | AOEAttr _ -> POAttr_Stream
	  | AOEAnyAttr _ -> POAnyAttr_Stream
	  | _ -> 
	      raise (Query (Prototype "Expected palgop_expr_name to be a constructor in Code_constructors.select_physical_op."))
	end

