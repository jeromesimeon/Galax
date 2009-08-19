(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_physical_type_ast_util.ml,v 1.10 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Xquery_algebra_ast_annotation_util
   Description:
     This module also provides functions access annotations on the algebra AST. 
*)

open Error

open Xquery_common_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_physical_type_ast

(***********************)
(* Useful constants    *)
(***********************)
let dom_cursor_type = (PT_Dom PT_CursorSeq)
let dom_cursor_xml_type = (PT_XML dom_cursor_type)
let dom_list_type = (PT_Dom PT_ListSeq)
let dom_list_xml_type = (PT_XML dom_list_type)
let sax_stream_type = (PT_Sax PT_Stream)
let sax_stream_xml_type = PT_XML (PT_Sax PT_Stream)

(* Physical type accessors *)
let assert_table_type x = 
  match x with
    | PT_Table y -> x
    | _ -> raise (Query (Physical_Type_Error ("Expected table type")))

let assert_tuple_type x = 
  match x with
    | PT_Table y -> y
    | _ -> raise (Query (Physical_Type_Error ("Expected tuple type")))

let assert_non_discarded_xml_type x = 
  let raise_non_non_discarded_xml_type () = 
    raise (Query (Physical_Type_Error ("Expected non-discarded XML type")))
  in
  match x with
  | PT_XML y ->
      begin
	match y with
	| PT_Dom _ -> y
	| PT_Sax PT_Stream -> y
	| _ -> raise_non_non_discarded_xml_type ()
      end
  | _ -> raise_non_non_discarded_xml_type ()

let access_no_type x = 
  match x with
    | NoInput -> ()
    | _ -> raise (Query (Physical_Type_Error ("Expected no/zero input type")))

let access_one_type x = 
  match x with
    | OneInput y -> y
    | _ -> raise (Query (Physical_Type_Error ("Expected one input type")))

let access_one_non_discarded_xml_type x =
  let t = access_one_type x in
    try assert_non_discarded_xml_type t
    with | (Query (Physical_Type_Error _ )) ->
      raise (Query (Physical_Type_Error ("Expected one non-discarded XML input")))

let access_one_table_type x = 
  match access_one_type x with
    | PT_Table y -> y
    | _ -> raise (Query (Physical_Type_Error ("Expected one table input")))

let access_two_types x = 
  match x with
    | TwoInput(x, y) -> (x,y) 
    | _ -> raise (Query (Physical_Type_Error  ("Expected two input types")))

let access_two_table_types x = 
  match access_two_types x with
    | (PT_Table x, PT_Table y) -> (x,y) 
    | _ -> raise (Query (Physical_Type_Error ("Expected two table inputs")))

let access_two_non_discarded_xml_types x = 
  let (t1, t2) = access_two_types x in
    try (assert_non_discarded_xml_type t1, assert_non_discarded_xml_type t2)
    with | (Query (Physical_Type_Error _ )) ->
      raise (Query (Physical_Type_Error ("Expected two non-discarded XML inputs")))

let access_many_types x =
  match x with
    | ManyInput y -> y
    | _ -> raise (Query (Physical_Type_Error ("Incorrect number of input types (expected ManyInput)")))

let access_many_non_discarded_xml_types x =
  let l = access_many_types x in 
    Array.to_list 
      (Array.map 
	 (fun t ->
	    try assert_non_discarded_xml_type t
	    with | (Query (Physical_Type_Error _ )) ->
	      raise (Query(Physical_Type_Error ("Expected many non-discarded XML inputs"))))
	 l)

let assert_dom_list_xml_type x =
    match x with
      | PT_Dom PT_ListSeq -> x
      | _ -> raise (Query (Physical_Type_Error("Expected Dom List type")))

let assert_sax_stream_xml_type x =
    match x with
      | PT_Sax PT_Stream -> x
      | _ -> raise (Query (Physical_Type_Error("Expected Sax Stream type")))

let access_one_sax_stream_xml_type x =
  let xml_type = access_one_non_discarded_xml_type x in
    assert_sax_stream_xml_type xml_type

let access_one_dom_list_table_type x =
  let tuple_type = access_one_table_type x in
    List.map (fun (tn, xml_type) -> (tn, (assert_dom_list_xml_type xml_type))) tuple_type

let access_two_dom_list_table_types x =
  let (tuple_type1, tuple_type2) = access_two_table_types x in
    (List.map (fun (tn, xml_type) -> (tn, (assert_dom_list_xml_type xml_type))) tuple_type1,
     List.map (fun (tn, xml_type) -> (tn, (assert_dom_list_xml_type xml_type))) tuple_type2)

let least_upper_xml_type pt1 pt2 =
  match (pt1, pt2) with
    | (PT_Sax PT_Stream, PT_Sax PT_Stream) -> (PT_Sax PT_Stream)
    | (PT_Sax PT_Discarded, PT_Sax PT_Discarded) -> (PT_Sax PT_Discarded)
    | (PT_Dom PT_CursorSeq, PT_Dom PT_CursorSeq) -> (PT_Dom PT_CursorSeq)

    | (PT_Sax PT_Discarded, PT_Sax PT_Stream) -> (PT_Sax PT_Discarded)
    | (PT_Sax PT_Stream, PT_Sax PT_Discarded) -> (PT_Sax PT_Discarded)

    | (PT_Sax _, PT_Dom PT_CursorSeq) -> (PT_Dom PT_CursorSeq)
    | (PT_Dom PT_CursorSeq, PT_Sax _) -> (PT_Dom PT_CursorSeq)

    | (_, _) -> dom_list_type
	
let concat_xml_type pt =
  match pt with
    | (PT_Sax PT_Stream) -> (PT_Sax PT_Stream)
    | (PT_Sax PT_Discarded) -> (PT_Sax PT_Discarded)
    | _ -> (PT_Dom PT_CursorSeq)

let materialize_xml_type pt =
  match pt with
    | (PT_Sax _) -> (PT_Dom PT_ListSeq)
    | (PT_Dom PT_ListSeq) -> (PT_Dom PT_ListSeq)
    | _ -> (PT_Dom PT_CursorSeq)
