(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: path_structutil.mli,v 1.5 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Path_structutil
   Description:
     This module contains basic operations on path structures.
*)

open Path_struct

(* Turns all subtree annotations on *)

val imposes_subtree : rooted_path_sequence -> rooted_path_sequence

(* Type for actions resulting from path navigation *)

type action =
  | GetSubtree
  | PreserveNode
  | SkipNode
  | KeepMovingPreserveNode of path_fragment_sequence
  | KeepMovingSkipNode of path_fragment_sequence

(* step inside a document *)

val inside_document :
    Streaming_types.sax_event -> rooted_path_sequence -> string -> path_fragment_sequence

(* step inside an external variable *)

val inside_variable :
    Streaming_types.sax_event -> rooted_path_sequence -> Xquery_common_ast.cvname -> path_fragment_sequence

(* step inside an element *)

val one_step :
    Streaming_types.sax_event -> path_fragment_sequence -> action

(* step inside attributes *)

val one_step_attribute :
    path_fragment_sequence -> Streaming_types.sax_xml_attribute -> bool

(* Printing of path structures *)

val print_path_sequence :
    Format.formatter -> rooted_path_sequence -> unit

