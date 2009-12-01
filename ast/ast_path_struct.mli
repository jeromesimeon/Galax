(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: alg_path_struct.mli,v 1.7 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Alg_path_struct
   Description:
     Data structures used for path analysis over the XQuery algebra.
*)

(* Path analysis for document projection
   over the XQuery algebra.

   Essentially the same data structures as for path analysis
   over the XQuery core; just changed a cnode_test into
   anode_test, and added path_annotation.

    - Michael *)


open Xquery_common_ast
open Xquery_algebra_ast


(* A path is a sequence of steps (axis, nodetest pair). *) 

type path = (axis * anode_test) list

(* Source id *)

type source_id =
  | Document_id of string
  | Constructor_id of int

(* Subtree flag *)

type subtree =
  | Subtree
  | NoSubtree

(* Path fragments *)

type path_fragment = path * subtree

type path_fragment_sequence = path_fragment list

(* Rooted paths *)

type rooted_path = source_id * path_fragment

type rooted_path_sequence = rooted_path list

(* returned paths of an expression, returned paths contained in tuple fields, paths accessed by an expression,
paths modified by an expression*)
type paths = rooted_path_sequence * (crname * rooted_path_sequence) list * rooted_path_sequence * rooted_path_sequence


(* Path annotation *)

type full_annotation = {
    mutable path_analysis : paths option;
    mutable streaming_annot: rooted_path_sequence option
  }

type path_annotation = full_annotation option ref




