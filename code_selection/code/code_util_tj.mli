(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util_tj.mli,v 1.6 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_util_tj
   Description:
     This module contains utilities used by the TwigJoin algorithm.
*)

open Code_selection_context
open Dynamic_stack
open Xquery_algebra_ast
open Xquery_common_ast
open Cursor
open Physical_value
open Physical_name_index

val get_top_item_from_stack : 
    (item sequence * int * int * int) dynamic_stack -> Dm.node

val get_top_pre_from_stack : 
    (item sequence * int * int * int) dynamic_stack -> int

val get_top_post_from_stack : 
    (item sequence * int * int * int) dynamic_stack -> int

(* fixme: merge with get_name_indices *)
val check_available_indices :
    code_selection_context -> twig_pattern -> name_index_handler array

val get_name_indices_array :
    code_selection_context -> twig_pattern -> name_index array

val pre : Dm.node -> int
val post : Dm.node -> int

val print_stack_config : 
    (item sequence * int * int * int) dynamic_stack array -> unit

val print_tuple :
    item sequence array -> unit

val build_restore_array :
    twig_pattern -> code_selection_context -> (item sequence -> unit) array

val restore_tuple :
    (item sequence -> unit) array ->
    item sequence array ->
    tuple_unit

val common_cursor_of_input_cursor :
    tuple_unit cursor ->
    (unit -> item sequence) -> item sequence cursor

val check_axis : code_selection_context -> Dm.node -> axis array -> twig_pattern -> int -> bool

val show_solutions :
    twig_pattern -> 
    axis array -> 
    (item sequence * int * int * int) dynamic_stack array -> 
    int -> 
    int -> 
    item sequence array list

val get_index_window : Physical_value.item Physical_value.sequence Cursor.cursor -> int * int
