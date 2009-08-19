(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util.mli,v 1.11 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_util
   Description:
     This module contains some auxiliary evaluation code.
*)

open Namespace_context
open Typing_context

open Xquery_common_ast
open Xquery_algebra_ast

open Datatypes
open Cursor
open Streaming_types
open Physical_value


(***********************************)
(* Auxiliary functions for sorting *)
(***********************************)

val make_atomic_gt : item -> item -> bool

val forest_compare :
    sortkind -> emptysortkind ->
      item cursor -> item cursor ->
	(item -> item -> bool) ->
	  int


(******************************************)
(* Auxiliary functions for function calls *)
(******************************************)

val match_overloaded_function :
    static_context -> Namespace_names.rqname ->
      (cfname * afunction_signature * updating_modifier ) list ->
	(item cursor list -> cfname * item cursor list)

val get_computed_node_name : Namespace_context.nsenv -> item Cursor.cursor -> xs_QName

