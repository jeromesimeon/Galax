(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_physical_algebra_ast.mli,v 1.27 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Xquery_core_ast_util
   Description:
     This module implements the physical algebra AST.
*)

open Xquery_common_ast open Xquery_physical_type_ast


type physop_variable_binding = physical_variable_type
type physop_tuple_field_binding = physical_tuple_field_type
type physop_tuple_binding = physical_tuple_type

(* Physical operator names *)
type physop_expr_name =
  | POIf
  | POWhile
  | POLetvar of physop_variable_binding
  | POTypeswitch of (cvname option) array
  | POVar of cvname
  | POScalar
  | POEmpty

  (***************)
  (* Constructor *)
  (***************)
  | POSeq_Stream
  | POSeq_Materialized
  | POImperativeSeq_Stream
  | POImperativeSeq_Materialized
  | PODocument_Stream
  | PODocument_Materialized
  | POPI_Stream
  | POPI_Materialized
  | POPIComputed_Stream
  | POPIComputed_Materialized
  | POComment_Stream
  | POComment_Materialized
  | POCommentComputed_Stream
  | POCommentComputed_Materialized
  | POText_Stream
  | POText_Materialized
  | POTextComputed_Stream
  | POTextComputed_Materialized
  | POElem_Stream
  | POElem_Materialized
  | POAnyElem_Stream
  | POAnyElem_Materialized
  | POAttr_Stream
  | POAttr_Materialized
  | POAnyAttr_Stream
  | POAnyAttr_Materialized

  | POError 
  | POTreat 
  | POValidate 
  | POCast 
  | POCastable
  | POSome of physop_variable_binding
  | POEvery of physop_variable_binding

  (* Input tuple *)
  | POInputTuple

  (* Function Calls *)
  | POCallBuiltIn
  | POCallOverloaded
  | POCallUserDefined
  | POConvertSimple
  | POPromoteNumeric
  | POPromoteAnyString
  | POUnsafePromoteNumeric

  (* Specific implementations *)
  | POCallBuiltIn_Fn_Count_Stream
  | POCallBuiltIn_Fs_First_Stream
  | POCallBuiltIn_Fs_Item2Node_Stream

  (* Galax extensions *)
  (* DXQ *)
  | POServerImplementsTree 
  | POServerImplementsTuple
  | POForServerCloseTree 
  | POForServerCloseTuple 
  | POEvalClosureTree  
  | POEvalClosureTuple 
  | POExecuteTree  
  | POExecuteTuple 

  (**************************)
  (* Basic Tuple operations *)
  (**************************)
  | POCreateTuple  of physop_tuple_binding
  | POAccessTuple  of crname
  | POConcatTuples
  | POProject      of crname array
  (**************)
  (* Tuple Maps *)
  (**************)
  | POMapFromItem of physop_variable_binding
  | POMapToItem                          (* Tuple to item iteration *)
  | POMap                                (* Tuple iteration *)
  | PONullMap  of crname
  | POMapIndex  of crname
  | POMapIndexStep  of crname

  (* Map Concats *)      
  | POMapConcat                          (* Map with built-in tuple concatenation *)
  | POOuterMapConcat  of crname

  (******************************)
  (* Products, Joins Selections *)
  (******************************)
  | POProduct                            (* Cartesian product *)
  | POSelect 
  | POJoin_Hash
  | POJoin_Sort
  | POJoin_NestedLoop 
  | POLeftOuterJoin_Hash  of crname
  | POLeftOuterJoin_Sort  of crname
  | POLeftOuterJoin_NestedLoop  of crname     

  (************************************)
  (* Grouping and Ordering operations *)
  (************************************)
  | POGroupBy of cvname list
  | POOrderBy 

  (* Update-related operations *)
  | POCopy
  (* Update operations *)
  | PODelete
  | POInsert 
  | PORename
  | POReplace 
  (* | POSequencing *)
  | POSnap 

  (* Assignment *)
  | POSet of cvname

  (********************)
  (* XPath evaluation *)
  (********************)
  | POParse_Stream
  | POParse_Load
  | POTreeJoin_Sort
  | POTreeJoin_Stream
  | POTreeJoin_NestedLoop

  | POTupleTreePattern_IndexSortJoin of crname list
  | POTupleTreePattern_NestedLoop of crname list
  | POTupleTreePattern_SCJoin of crname list
  | POTupleTreePattern_Streaming of crname list
  | POTupleTreePattern_TwigJoin of crname list

(* Defunct
  | PODistinct of crname
  | POPrune  of crname
*)
