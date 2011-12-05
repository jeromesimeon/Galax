(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: planio_util.mli,v 1.3 2007/02/01 22:08:51 simeon Exp $ *)

(* 
   Module: Planio_util
   Description:
     This module contains utilities for query plan parsing and
     serialization.
*)

(**********************)
(* Parsing Utilities  *)
(**********************)

val string_id : 'a -> 'a
val int_id : string -> int

val get_multiple : int -> ('a -> 'b) -> 'a -> 'b list
val get_multiple_cursor : int -> ('a -> 'b) -> 'a -> 'b Cursor.cursor
val parse_nested_substream :
  int ->
  Streaming_types.sax_event Stream.t ->
  Streaming_types.sax_event Cursor.cursor

val start_typed_element_get_name_no_consume :
  Streaming_types.xml_stream ->
  Namespace_symbols.relem_symbol *
  Streaming_types.sax_xml_attribute_forest
val start_element_get_name_no_consume :
  Streaming_types.sax_event Stream.t ->
  Namespace_symbols.relem_symbol *
  Streaming_types.sax_xml_attribute_forest
val start_element_get_name :
  Streaming_types.sax_event Stream.t ->
  Namespace_symbols.relem_symbol *
  Streaming_types.sax_xml_attribute_forest
val aux_start_element :
  ('a -> Namespace_symbols.symbol * 'b) ->
  Namespace_symbols.relem_symbol -> 'a -> 'b
val start_element :
  Namespace_symbols.relem_symbol ->
  Streaming_types.sax_event Stream.t ->
  Streaming_types.sax_xml_attribute_forest
val start_typed_element_get_name :
  Streaming_types.xml_stream ->
  Namespace_symbols.relem_symbol *
  Streaming_types.sax_xml_attribute_forest
val start_typed_element :
  Namespace_symbols.relem_symbol ->
  Streaming_types.xml_stream ->
  Streaming_types.sax_xml_attribute_forest
val is_element_end : Streaming_types.sax_event Stream.t -> bool
val consume_end_element : Streaming_types.sax_event Stream.t -> unit
val consume_typed_end_element :
  Streaming_types.sax_event Cursor.cursor -> unit
val check_opt_element :
  Namespace_names.rqname ->
  Streaming_types.sax_event Stream.t -> bool
val element_parser :
  Namespace_names.rqname ->
  (Streaming_types.sax_xml_attribute_forest ->
   Streaming_types.sax_event Stream.t -> 'a) ->
  string -> Streaming_types.sax_event Stream.t -> 'a
(* val parse_attr_helper :
  ('a -> 'b) -> 'c -> (unit -> 'b) -> ('c * 'a) list -> 'b *)
val parse_get_typed_attr_from_attr_list :
  Streaming_types.sax_xml_attribute_forest ->
  (Datatypes.xs_untyped -> 'a) -> Namespace_symbols.rattr_symbol -> 'a
val get_attr_from_attr_list :
  Streaming_types.sax_xml_attribute_forest ->
  (Datatypes.xs_untyped -> 'a) -> Namespace_symbols.rattr_symbol -> 'a
val get_opt_attr_from_attr_list :
  Streaming_types.sax_xml_attribute_forest ->
  (Datatypes.xs_untyped -> 'b) -> Namespace_symbols.rattr_symbol -> 'b option

(****************************)
(* Serialization Utilities  *)
(****************************)

val construct_element_top : 
    Namespace_names.rqname -> 
      Small_stream_ast.sattribute_forest -> 
	Small_stream_ast.sexpr list -> 
	  Small_stream_ast.sexpr 

val construct_element : 
    Namespace_names.rqname -> 
      (Namespace_names.rqname * Datatypes.xs_untyped) list ->
	Small_stream_ast.sexpr list -> 
	  Small_stream_ast.sexpr 

val construct_attribute    : Namespace_names.rqname -> string -> Small_stream_ast.sattribute 

val construct_comment      : string -> string -> Small_stream_ast.sexpr list 

val attribute_of_occurrence: (Occurrence.occurs * Occurrence.occurs) option ->  Small_stream_ast.sattribute

val box_rtype_symbol: Namespace_symbols.rtype_symbol -> Small_stream_ast.sexpr
val box_relem_symbol: Namespace_symbols.relem_symbol -> Small_stream_ast.sexpr
val box_rattr_symbol: Namespace_symbols.rattr_symbol -> Small_stream_ast.sexpr

val box_kind_test     : Xquery_algebra_ast.akind_test -> Small_stream_ast.sexpr
val box_aitemtype     : Xquery_algebra_ast.aitemtype -> Small_stream_ast.sexpr

val box_asequencetype : Xquery_algebra_ast.asequencetype -> Small_stream_ast.sexpr
val box_optasequencetype : Xquery_algebra_ast.asequencetype option -> Small_stream_ast.sexpr list

(* Return attributes encoding kind of subexpression and for Many kind, its arity *)
val dep_subexpr_attrs : ('a, 'b) Xquery_algebra_ast.aalgop_sub_exprs -> Small_stream_ast.sattribute list
val indep_subexpr_attrs : ('a, 'b) Xquery_algebra_ast.aalgop_sub_exprs -> Small_stream_ast.sattribute list

val get_dep_subexpr_kind_arity : Streaming_types.sax_xml_attribute_forest -> Planio_common.algop_kind_moniker -> (int)
val get_indep_subexpr_kind_arity : Streaming_types.sax_xml_attribute_forest  -> Planio_common.algop_kind_moniker -> (int) 
