(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_builtin_fn.ml,v 1.14 2007/05/02 19:30:59 mff Exp $ *)

(* Module: Code_builtin_fn
   Description:
     This module contains code building for built-in functions.
*)

open Compile_context
open Code_selection_context
open Processing_context

open Code_util_matching
open Cs_util_coercion

open Datatypes
open Namespace_builtin

open Physical_value
open Physical_item_util

open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_physical_algebra_ast
open Xquery_physical_type_ast
open Xquery_physical_type_ast_util

open Error


let select_physical_op_specialized_builtin_fn input_signature physop =
  let raise_non_non_discarded_xml_type () =
    raise (Query (Physical_Type_Error ("Expected non-discarded XML type")))
  in
    match access_many_non_discarded_xml_types input_signature with
      | xml_type :: [] ->
	  begin
	    match xml_type with
	      | PT_Sax PT_Stream-> physop
	      | PT_Dom _ -> POCallBuiltIn
	      | PT_Sax PT_Discarded -> raise_non_non_discarded_xml_type ()
	  end
      | _ -> raise_non_non_discarded_xml_type ()

(* STREAMING HACK!! *)
let select_physical_op code_ctxt input_signature algop_expr ((cfname, arity), odts, t) =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in
  if (proc_ctxt.streaming)
  then
    begin
	(* fn:count *)
      if Namespace_names.rqname_equal cfname fn_count
      then select_physical_op_specialized_builtin_fn input_signature POCallBuiltIn_Fn_Count_Stream

	(* fs:first-item *)
      else if Namespace_names.rqname_equal cfname fs_first
      then select_physical_op_specialized_builtin_fn input_signature POCallBuiltIn_Fs_First_Stream
	  
	(* fs:item-sequence-to-node-sequence *)
      else if Namespace_names.rqname_equal cfname fs_item_sequence_to_node_sequence
      then select_physical_op_specialized_builtin_fn input_signature POCallBuiltIn_Fs_Item2Node_Stream
	  
	(* other *)
      else POCallBuiltIn
    end
  else POCallBuiltIn
      
let build_default_fn_count_stream_code code_ctxt =
  (fun algt_ctxt args ->
     let input_stream = Args.get_array_param1 args in
     let item_count = Streaming_conv.item_count_typed_labeled_xml_stream input_stream in
     let result_cursor =  Cursor.cursor_of_singleton (_integer (Decimal._integer_of_int item_count)) in
       (Cursor.list_of_cursor "CSCODE FN BUILTIN" result_cursor)
  )

let build_default_fs_first_stream_code code_ctxt =
  (fun algt_ctxt args ->
     let input_stream = Args.get_array_param1 args in
       Streaming_conv.first_item_typed_labeled_xml_stream input_stream
  )

let build_default_fs_item2node_stream_code code_ctxt =
  (fun algt_ctxt args -> Args.get_array_param1 args)

(* Built In function Compilation 
   Should these be moved to their own location? *)
let build_default_builtin_fn_code code_ctxt fn arity optintypes outtype =
  let optin_arity  = Array.length optintypes in
  let compile_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let stat_ctxt    = static_context_from_compile_context compile_ctxt in

  let fn_code = (Code_fn.lookup_bltin_fctn (fn, arity)) code_ctxt in
    (fun alg_ctxt eargs ->
  let arity = Array.length eargs in
  let eargs = Array.map Cursor.cursor_of_list eargs in
  if (optin_arity != arity)
  then
    raise (Query(Parameter_Mismatch("Argument and type count not equal")))
  else
      for i = 0 to optin_arity - 1 do
	eargs.(i) <- 
	  dynamic_opttype_check stat_ctxt optintypes.(i) eargs.(i)
      done;
      (Cursor.list_of_cursor "CSCODE FN BUILTIN" (fn_code alg_ctxt eargs)))

let build_builtin_fn_code code_ctxt algop ((cfname,arity), optintypes, outtype) =
  let _  = access_nosub algop.pdep_sub_expression in 
  let physop = Cs_util.get_physical_opname algop in
    match physop with
      | POCallBuiltIn ->
	  let fn = build_default_builtin_fn_code code_ctxt cfname arity optintypes outtype in
	    (coerce_nodep fn coerce_many_item_list_to_item_list), code_ctxt
      | POCallBuiltIn_Fn_Count_Stream ->
	  let fn = build_default_fn_count_stream_code code_ctxt in
	    (coerce_nodep fn coerce_many_sax_to_item_list), code_ctxt
      | POCallBuiltIn_Fs_First_Stream ->
	  let fn = build_default_fs_first_stream_code code_ctxt in
	    (coerce_nodep fn coerce_many_sax_to_sax), code_ctxt
      | POCallBuiltIn_Fs_Item2Node_Stream ->
        (* The function fs:item-sequence-to-node-sequence is a NO-OP:
           It is only used for typing.  However, if rewriting is
           disabled, an implementation must be provided, both for DOM
           and Token-Stream representations of an item sequence.  We
           can use the same implementation for both.  
	*)
	  let fn = build_default_fs_item2node_stream_code code_ctxt in
	    (coerce_nodep fn coerce_many_sax_to_sax), code_ctxt
      | _ -> raise(Query(Code_Selection("Invalid physical operator in build_builtin_fn_code")))

(* Special functions *)

let build_default_convert_simple_code code_ctxt atomic_type =
  let bt = Datatypes_util.symbol_of_primitive_type atomic_type in
  (fun alg_ctxt p1 ->
   (* fs:convert-simple-operand is used in the definition of function
      calls that take sequences of atomic values as arguments.  The
      function converts each occurrence of an xs:untypedAtomic
      argument to the target type.  *)
    let a1l = Physical_util.get_atomic_cursor p1 in
    Cursor.cursor_map (fun a1 -> 
      match a1#getAtomicValueKind() with
      | ATUntypedAtomic ->
	  (Item_Atomic (a1#cast_to Namespace_context.default_xquery_nsenv bt atomic_type))
      | _ -> (Item_Atomic a1))
      a1l)

let build_default_promote_numeric_code code_ctxt atomic_type =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  begin
    match atomic_type with
    | ATInteger 
    | ATDecimal 
    | ATFloat   
    | ATDouble -> ()
    | _ -> raise(Query(Cast_Error("Attempt to promote to non-numeric type")))
  end;
  let bt = Datatypes_util.symbol_of_primitive_type atomic_type in
  (fun alg_ctxt p1 ->
    (* The algebra promotion functions take an input sequence of
       atomic values and attempt to promote each atomic value in the
       input sequence to the target type. *)
    let _fs_promote_atomic_value_to_type comp_ctxt alg_ctxt a1 = 
      let at = a1#getAtomicValueKind() in
      let atl = Datatypes_util.can_be_promoted_to at in
      if (List.mem atomic_type atl) then
	if (at = atomic_type)
	then (Item_Atomic a1)
	else (Item_Atomic (a1#cast_to Namespace_context.default_xquery_nsenv bt atomic_type))
      else
	raise (Query(Cast_Error("Cannot promote "^(Datatypes_util.string_of_atomic_type at)^" to "^(Datatypes_util.string_of_atomic_type atomic_type))))
    in
    let a1l = Physical_util.get_atomic_cursor p1 in 
    Cursor.cursor_map (_fs_promote_atomic_value_to_type comp_ctxt alg_ctxt) a1l
      (* fs:convert-simple-operand is used in the definition of function
	 calls that take sequences of atomic values as arguments.  The
	 function converts each occurrence of an xs:untypedAtomic
	 argument to the target type.  *)
  )

let build_default_promote_anystring_code code_ctxt =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let atomic_type = ATString in
  let bt = Datatypes_util.symbol_of_primitive_type atomic_type in
  (fun alg_ctxt p1 ->
    (* The algebra promotion functions take an input sequence of
       atomic values and attempt to promote each atomic value in the
       input sequence to the target type. *)
    let _fs_promote_atomic_value_to_type comp_ctxt alg_ctxt a1 = 
      let at = a1#getAtomicValueKind() in
      let atl = Datatypes_util.can_be_promoted_to at in
      if (List.mem atomic_type atl) then
	if (at = atomic_type)
	then (Item_Atomic a1)
	else (Item_Atomic (a1#cast_to Namespace_context.default_xquery_nsenv bt atomic_type))
      else
	raise (Query(Cast_Error("Cannot promote "^(Datatypes_util.string_of_atomic_type at)^" to "^(Datatypes_util.string_of_atomic_type atomic_type))))
    in
    let a1l = Physical_util.get_atomic_cursor p1 in 
    Cursor.cursor_map (_fs_promote_atomic_value_to_type comp_ctxt alg_ctxt) a1l
      (* fs:convert-simple-operand is used in the definition of function
	 calls that take sequences of atomic values as arguments.  The
	 function converts each occurrence of an xs:untypedAtomic
	 argument to the target type.  *)
  )

let build_default_unsafe_promote_numeric_code code_ctxt atomic_type =
  let bt = Datatypes_util.symbol_of_primitive_type atomic_type in
  (fun alg_ctxt p1 ->
    (* fs:unsafe-promote-to-numeric is the same as
       fs:promote-to-numeric but is only applied when static typing
       guarantees that every atomic value in the input sequence can be
       promoted to the target type.  *)
    let a1l = Physical_util.get_atomic_cursor p1 in 
    let a1l' = Cursor.cursor_map (
      fun a1 ->
	let at = a1#getAtomicValueKind() in
	if (at = atomic_type) then (Item_Atomic a1)
	else (Item_Atomic (a1#cast_to Namespace_context.default_xquery_nsenv bt atomic_type))
     ) a1l in
    a1l')

let build_convert_simple_code code_ctxt algop atomic_type = 
  let fn = build_default_convert_simple_code code_ctxt atomic_type in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_unary_item_cursor_to_item_cursor), code_ctxt

let build_promote_numeric_code code_ctxt algop atomic_type =
  let fn = build_default_promote_numeric_code code_ctxt atomic_type in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_unary_item_cursor_to_item_cursor), code_ctxt

let build_promote_anystring_code code_ctxt algop =
  let fn = build_default_promote_anystring_code code_ctxt in
  let _ = access_nosub algop.pdep_sub_expression in 
  (coerce_nodep fn coerce_unary_item_cursor_to_item_cursor), code_ctxt

let build_unsafe_promote_numeric_code code_ctxt algop atomic_type =
  let fn = build_default_unsafe_promote_numeric_code code_ctxt atomic_type in
  let _ = access_nosub algop.pdep_sub_expression in
  (coerce_nodep fn coerce_unary_item_cursor_to_item_cursor), code_ctxt
