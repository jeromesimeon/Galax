(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_physical_type_ast_util.mli,v 1.9 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Xquery_physical_type_ast_util
   Description:
     This module also provides functions access annotations on the algebra AST. 
*)

open Xquery_algebra_ast
open Xquery_physical_type_ast

open Ast_logical_algebra_types

val access_no_type                      : Xquery_algebra_ast.input_signature -> unit
val access_one_non_discarded_xml_type   : Xquery_algebra_ast.input_signature -> physical_xml_type
val access_one_table_type               : Xquery_algebra_ast.input_signature -> physical_tuple_type
val access_two_non_discarded_xml_types  : Xquery_algebra_ast.input_signature -> physical_xml_type * physical_xml_type
val access_two_types                    : Xquery_algebra_ast.input_signature -> physical_type * physical_type
val access_two_table_types              : Xquery_algebra_ast.input_signature -> physical_tuple_type * physical_tuple_type
val access_many_non_discarded_xml_types : Xquery_algebra_ast.input_signature -> physical_xml_type list
val access_many_types                   : Xquery_algebra_ast.input_signature -> physical_type array

val access_one_sax_stream_xml_type  :  Xquery_algebra_ast.input_signature -> physical_xml_type
val access_one_dom_list_table_type  : Xquery_algebra_ast.input_signature -> physical_tuple_type
val access_two_dom_list_table_types : Xquery_algebra_ast.input_signature -> physical_tuple_type * physical_tuple_type

val assert_table_type             : physical_type -> physical_type
val assert_tuple_type             : physical_type -> physical_tuple_type
val assert_non_discarded_xml_type : physical_type -> physical_xml_type

val dom_cursor_type     : physical_xml_type  (* A DOM item cursor *)
val dom_cursor_xml_type : physical_type      (* An XML-boxed DOM cursor list *)
val dom_list_type       : physical_xml_type  (* A DOM item list *)
val dom_list_xml_type   : physical_type      (* An XML-boxed DOM item list *)
val sax_stream_type     : physical_xml_type  (* A SAX cursor *)		  
val sax_stream_xml_type : physical_type      (* An XML-boxed SAX cursor *)

val least_upper_xml_type : physical_xml_type -> physical_xml_type -> physical_xml_type

(* Mimics the coercion behaviour of Physical_value_util.concat_xml_value_cursor
   used by Code_item_tuple.build_tuple_to_item_map_code. - Michael *)
val concat_xml_type      : physical_xml_type -> physical_xml_type

(* Mimics the coercion behaviour of Physical_xml_value.dom_value_of_xml_value
   used by Code_util_materialize.generic_materialize_tuple_cursor. - Michael *)
val materialize_xml_type : physical_xml_type -> physical_xml_type
