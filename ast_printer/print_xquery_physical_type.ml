(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: print_xquery_physical_type.ml,v 1.23 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Xquery_algebra_ast_annotation_util
   Description:
     This module also provides functions access annotations on the algebra AST. 
*)

open Xquery_algebra_ast
open Xquery_physical_type_ast
open Xquery_physical_algebra_ast

open Namespace_names
  
  
let string_of_physical_materialized_type t =
  match t with 
    | PT_CursorSeq -> "CursSeq"
    | PT_ListSeq -> "ListSeq"
	
let string_of_physical_sax_type t =
  match t with 
    | PT_Stream -> "Stream"
    | PT_Discarded -> "Discarded"
	
let string_of_physical_xml_type t =
  match t with 
    | PT_Dom pmt -> "Dom("^(string_of_physical_materialized_type pmt)^")"
    | PT_Sax pst -> "Sax("^(string_of_physical_sax_type pst)^")"
	
let string_of_physical_tuple_type name_type_list = 
    "[" ^ (String.concat ";" (List.map (fun (v,pt) -> (prefixed_string_of_rqname v)^":"^(string_of_physical_xml_type pt)) name_type_list) ) ^ "]"
    
let string_of_physical_type t = 
  match t with 
    | PT_XML pt -> string_of_physical_xml_type pt
    | PT_Table pt -> string_of_physical_tuple_type pt
	
let string_of_input_signature s =
  match s with
    | NoInput -> "()"
    | OneInput pt ->
	"(" ^ (string_of_physical_type pt) ^ ")"
    | TwoInput (pt1,pt2) ->
	"(" ^ (string_of_physical_type pt1) ^ "," ^ (string_of_physical_type pt2) ^ ")"
    | ManyInput pta ->
	let ptl = List.map string_of_physical_type (Array.to_list pta) in
	  "(" ^ (String.concat "," ptl) ^ ")"

let string_of_physop_variable_binding (vn, pt) =
      "$" ^ (prefixed_string_of_rqname vn) ^ ":" ^ (string_of_physical_xml_type pt)

let string_of_physop_tuple_field_binding (tn, pt) =
  string_of_physop_variable_binding (tn, pt)

let string_of_physop_tuple_binding tb =
  let tfbs = List.map string_of_physop_tuple_field_binding tb in
    String.concat "," tfbs

let string_of_project_fields f =
  let pflds = List.map (fun vn -> prefixed_string_of_rqname vn) (Array.to_list f) in
  String.concat "," pflds

let string_of_physop_expr_name op =
  match op with 
  | POIf	-> "POIf"
  | POWhile	-> "POWhile"
  | POLetvar pvb -> "POLetvar[" ^ string_of_physop_variable_binding pvb  ^ "]"
  | POTypeswitch _ -> "POTypeswitch"
  | POVar vn	-> "POVar["^(prefixed_string_of_rqname vn)^"]"
  | POScalar	-> "POScalar"
  | POSeq_Stream -> "POSeq_Stream"
  | POSeq_Materialized -> "POSeq_Materialized"
  | POImperativeSeq_Stream -> "POImperativeSeq_Stream"
  | POImperativeSeq_Materialized -> "POImperativeSeq_Materialized"
  | POEmpty	-> "POEmpty"
  | PODocument_Stream -> "PODocument_Stream"
  | PODocument_Materialized -> "PODocument_Materialized"
  | POPI_Stream -> "POPI_Stream"
  | POPI_Materialized -> "POPI_Materialized"
  | POPIComputed_Stream -> "POPIComputed_Stream"
  | POPIComputed_Materialized -> "POPIComputed_Materialized"
  | POComment_Stream -> "POComment_Stream"
  | POComment_Materialized -> "POComment_Materialized"
  | POCommentComputed_Stream -> "POCommentComputed_Stream"
  | POCommentComputed_Materialized -> "POCommentComputed_Materialized"
  | POText_Stream -> "POText_Stream"
  | POText_Materialized -> "POText_Materialized"
  | POTextComputed_Stream -> "POTextComputed_Stream"
  | POTextComputed_Materialized -> "POTextComputed_Materialized"
  | POElem_Stream -> "POElem_Stream"
  | POElem_Materialized -> "POElem_Materialized"
  | POAnyElem_Stream -> "POAnyElem_Stream"
  | POAnyElem_Materialized -> "POAnyElem_Materialized"
  | POAttr_Stream -> "POAttr_Stream"
  | POAttr_Materialized -> "POAttr_Materialized"
  | POAnyAttr_Stream -> "POAnyAttr_Stream"
  | POAnyAttr_Materialized -> "POAnyAttr_Materialized"
  | POError 	-> "POError"
  | POTreat 	-> "POTreat"
  | POValidate 	-> "POValidate"
  | POCast 	-> "POCast"
  | POCastable	-> "POCastable"
  | POSome pvb	-> "POSome[" ^ string_of_physop_variable_binding pvb  ^ "]"
  | POEvery pvb	-> "POEvery[" ^ string_of_physop_variable_binding pvb  ^ "]"
  | POInputTuple	-> "POInputTuple"
  | POCallBuiltIn 	-> "POCallBuiltIn"
  | POCallBuiltIn_Fn_Count_Stream -> "POCallBuiltIn_Fn_Count_Stream"
  | POCallBuiltIn_Fs_First_Stream -> "POCallBuiltIn_Fs_First_Stream"
  | POCallBuiltIn_Fs_Item2Node_Stream -> "POCallBuiltIn_Fs_Item2Node_Stream"
  | POCallOverloaded 	-> "POCallOverloaded"
  | POCallUserDefined 	-> "POCallUserDefined"
  | POConvertSimple 	-> "POConvertSimple"
  | POPromoteNumeric 	-> "POPromoteNumeric"
  | POPromoteAnyString 	-> "POPromoteAnyString"
  | POUnsafePromoteNumeric -> "POUnsafePromoteNumeric"
  | POServerImplementsTree  -> "POServerImplementsTree"
  | POServerImplementsTuple  -> "POServerImplementsTuple"
  | POForServerCloseTree  -> "POForServerCloseTree"
  | POForServerCloseTuple  -> "POForServerCloseTuple"
  | POEvalClosureTree  -> "POEvalClosureTree"
  | POEvalClosureTuple  -> "POEvalClosureTuple"
  | POExecuteTree       -> "POExecuteTree"
  | POExecuteTuple      -> "POExecuteTuple"
  | POCreateTuple ptb 	-> "POCreateTuple[" ^ string_of_physop_tuple_binding ptb  ^ "]"
  | POAccessTuple  _	-> "POAccessTuple"
  | POConcatTuples 	-> "POConcatTuples"
  | POMapFromItem pvb	-> "POMapFromItem[" ^ string_of_physop_variable_binding pvb  ^ "]"
  | POMapToItem         -> "POMapToItem"
  | POMap               -> "POMap"
  | PONullMap  _	-> "PONullMap"
  | POMapIndex  _	-> "POMapIndex"
  | POMapIndexStep  _	-> "POMapIndexStep"
  | POMapConcat         -> "POMapConcat"
  | POOuterMapConcat  _	-> "POOuterMapConcat"
  | POProduct           -> "POProduct"
  | POSelect 	        -> "POSelect"
  | POJoin_Hash	        -> "POJoin_Hash"
  | POJoin_Sort	        -> "POJoin_Sort"
  | POJoin_NestedLoop 	-> "POJoin_NestedLoop"
  | POLeftOuterJoin_Hash  _	  -> "POLeftOuterJoin_Hash"
  | POLeftOuterJoin_Sort  _	  -> "POLeftOuterJoin_Sort"
  | POLeftOuterJoin_NestedLoop  _ -> "POLeftOuterJoin_NestedLoop"
  | POGroupBy _ -> "POGroupBy"
  | POOrderBy 	-> "POOrderBy"
  | POCopy	-> "POCopy"
  | PODelete	-> "PODelete"
  | POInsert 	-> "POInsert"
  | PORename	-> "PORename"
  | POReplace 	-> "POReplace"
  | POSnap 	-> "POSnap"
  | POSet vn   -> "POSet[" ^ (prefixed_string_of_rqname vn)  ^ "]"
  | POParse_Stream	-> "POParse_Stream"
  | POParse_Load	-> "POParse_Load"
  | POTreeJoin_Sort	-> "POTreeJoin_Sort"
  | POTreeJoin_Stream	-> "POTreeJoin_Stream"
  | POTreeJoin_NestedLoop	       -> "POTreeJoin_NestedLoop"
  | POTupleTreePattern_IndexSortJoin _ -> "POTupleTreePattern_IndexSortJoin"
  | POTupleTreePattern_NestedLoop _    -> "POTupleTreePattern_NestedLoop"
  | POTupleTreePattern_SCJoin _        -> "POTupleTreePattern_SCJoin"
  | POTupleTreePattern_Streaming _     -> "POTupleTreePattern_Streaming"
  | POTupleTreePattern_TwigJoin _      -> "POTupleTreePattern_TwigJoin"
(*
  | PODistinct _	               -> "PODistinct"
  | POPrune  _	                       -> "POPrune"
*)
  | POProject crnames                  -> "POProject["^(string_of_project_fields crnames)^"]"

let string_of_eval_sig (n,s,pt) =
  let pname = string_of_physop_expr_name n in
  let is = string_of_input_signature s in
  let os = string_of_physical_type pt in
    pname ^ is ^ " -> " ^ os
      
