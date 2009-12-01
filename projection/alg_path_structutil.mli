(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: alg_path_structutil.mli,v 1.5 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Alg_path_structutil
   Description:
     Utilities for path analysis over the XQuery algebra.
*)

open Ast_path_struct

(* Type for actions resulting from path navigation *)

type action =
  | GetSubtree
  | PreserveNode
  | SkipNode
  | KeepMovingPreserveNode of path_fragment_sequence
  | KeepMovingSkipNode of path_fragment_sequence


val mk_rooted_path_sequence_constructor : int -> rooted_path_sequence
val mk_rooted_path_sequence_document : string -> rooted_path_sequence

(* Turns all subtree annotations on *)
val imposes_subtree : rooted_path_sequence -> rooted_path_sequence

(* Printing of path structures *)
val print_path_sequence :
    Format.formatter -> rooted_path_sequence -> unit

(* step inside a document *)

val inside_document :
    Streaming_types.typed_annotated_sax_event -> rooted_path_sequence -> string -> path_fragment_sequence

(* step inside an external variable *)

(*
val inside_variable :
    Streaming_types.resolved_sax_event -> rooted_path_sequence -> Xquery_common_ast.cvname -> path_fragment_sequence
*)

(* step inside an element *)

val one_step :
    Streaming_types.typed_annotated_sax_event -> path_fragment_sequence -> action

(* step inside attributes *)

val one_step_attribute :
    path_fragment_sequence -> Streaming_types.typed_sax_xml_attribute -> bool

val paths_from_path_annotation : string -> path_annotation -> paths

val rooted_path_equal: rooted_path -> rooted_path -> bool

val rooted_path_sequence_equal: rooted_path_sequence -> rooted_path_sequence -> bool

val mem_rooted_path_sequence: rooted_path -> rooted_path_sequence -> bool

val disjoint_rooted_path_sequence: rooted_path_sequence -> rooted_path_sequence -> bool

val path_sequences_with_disjoint_roots: rooted_path_sequence -> rooted_path_sequence -> bool
