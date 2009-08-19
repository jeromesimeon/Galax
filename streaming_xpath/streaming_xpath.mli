(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: streaming_xpath.mli,v 1.5 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Streaming_xpath
   Description:
     This module streaming XPath evaluation code, employing labeled
     XML token streams.
   - Michael *)

open Xquery_algebra_ast

open Streaming_conv


val xpath_axis_attribute :
  Typing_context.static_context -> typed_labeled_xml_stream -> anode_test -> typed_labeled_xml_stream

val xpath_axis_descendant_or_self :
  Typing_context.static_context -> typed_labeled_xml_stream -> anode_test -> typed_labeled_xml_stream

val xpath_axis_descendant :
  Typing_context.static_context -> typed_labeled_xml_stream -> anode_test -> typed_labeled_xml_stream

val xpath_axis_child :
  Typing_context.static_context -> typed_labeled_xml_stream -> anode_test -> typed_labeled_xml_stream
