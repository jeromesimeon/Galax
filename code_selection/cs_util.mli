(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cs_util.mli,v 1.19 2007/05/23 22:12:59 simeon Exp $ *)

(* Module: Cs_util
   Description:
     This module contains some auxiliary evaluation functions, notably
     for function calls, type declarations, axis, element and
     attribute construction.
*)

open Physical_value

(*************)
(* Constants *)
(*************)

val empty_sequence         : xml_value
val non_empty_sequence     : xml_value 

val empty_dom_sequence     : dom_value
val non_empty_dom_sequence : dom_value 

val empty_tuple            : tuple_unit
val empty_tuple_opt        : tuple_unit option (* Used for tuple returns *)


(* add *)
(* unsafe means that the variable is *not* materialized *)
val build_add_var_xml_value_with_ref :
    Code_selection_context.code_selection_context -> Variable_context_manager.variable_ref -> 
      (xml_value -> unit )

val build_add_var_item_list :
    Code_selection_context.code_selection_context -> Xquery_common_ast.cvname -> 
      (item list -> unit )

val build_add_var_xml_value_unsafe_allowed :
    Code_selection_context.code_selection_context  -> Xquery_common_ast.cvname -> 
      (xml_value -> unit)
val build_add_var_xml_value_safe :
    Code_selection_context.code_selection_context  -> Xquery_common_ast.cvname -> 
      (xml_value -> unit )

(*
val build_add_var_item_cursor_unsafe_allowed :
    Code_selection_context.code_selection_context  -> Xquery_common_ast.cvname -> 
      (item Cursor.cursor -> unit )
*)
val build_add_var_item_cursor_safe :
    Code_selection_context.code_selection_context  -> Xquery_common_ast.cvname -> 
      (item Cursor.cursor -> unit )

(* Retr *)
(*
val build_physical_value_retrieve :
    Code_selection_context.code_selection_context -> Xquery_common_ast.cvname -> 
      (unit -> physical_value)
*)
val build_var_xml_value_retrieve :
    Code_selection_context.code_selection_context -> Xquery_common_ast.cvname -> 
      (unit -> xml_value)

val build_var_item_list_retrieve :
    Code_selection_context.code_selection_context -> Xquery_common_ast.cvname -> 
      (unit -> item list)

val inputs_are_fs_untyped_to_any : Algebra_type.algop_expr -> bool


(**************************************)
(* These should be changed or moved.. *)
(* Deal with type promotion for       *)
(* atomicValue comparison             *)
(**************************************)
val promote_atomicValue_to_highest : Namespace_context.nsenv -> Dm_atomic.atomicValue -> Dm_atomic.atomicValue

val promote_atomicValue_to_all : Namespace_context.nsenv -> Dm_atomic.atomicValue -> Dm_atomic.atomicValue list

val handle_fs_untyped_to_any_semantic : 
  Namespace_context.nsenv -> Dm_atomic.atomicValue -> 
  (* the possibly two types it could be *)
  (Dm_atomic.atomicValue * Dm_atomic.atomicValue option )

val get_physical_opname : Algebra_type.algop_expr -> Xquery_physical_algebra_ast.physop_expr_name

val dxq_demangle_rqname : Namespace_names.rqname ->  Namespace_names.rqname option
