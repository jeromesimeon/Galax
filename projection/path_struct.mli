(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: path_struct.mli,v 1.5 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Path_struct
   Description:
     This module contains types for path structures.
*)

open Xquery_common_ast
open Xquery_core_ast


(* A path is a sequence of steps (axis,nodetest pair). *) 

type path = (axis * cnode_test) list

(* Document id *)

type rootid =
  | InputDocument of string
  | InputVariable of cvname

(* Subtree flag *)

type subtree =
  | Subtree
  | NoSubtree

(* Path fragments *)

type path_fragment = path * subtree

type path_fragment_sequence = path_fragment list

(* Rooted paths *)

type rooted_path = rootid * path_fragment

type rooted_path_sequence = rooted_path list

