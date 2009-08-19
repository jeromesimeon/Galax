(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_binding.mli,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_binding
   Description:
     This module contains code building for operators that bind
     variable or tuple fields.
*)

(* Variables *)

(* Selects the _smallest_ physical xml type for the binding
   according to current command-line switches, variable use
   counts, independent input signature and type checking
   requirements.

   The physical type returned here may thus be different ('too
   small') from the actual type enforced by means of coercion
   functions. - Michael *)
val select_physical_variable_binding : 
    Code_selection_context.code_selection_context ->
      Xquery_algebra_ast.input_signature ->
        (Xquery_algebra_ast.asequencetype option * Xquery_common_ast.cvname) ->
          Xquery_physical_algebra_ast.physop_variable_binding

val build_bind_item_cursor_to_variable_code :
    Code_selection_context.code_selection_context ->
      Xquery_common_ast.cvname ->
        (Physical_value.item Cursor.cursor -> unit)

val build_bind_type_checked_item_cursor_to_variable_code :
    Code_selection_context.code_selection_context ->
      Xquery_algebra_ast.asequencetype ->
        Xquery_common_ast.cvname ->
          (Physical_value.item Cursor.cursor -> unit)

val build_bind_item_list_to_variable_code :
    Code_selection_context.code_selection_context ->
      Xquery_common_ast.cvname ->
        (Physical_value.item list-> unit)

(* This is _not_ a misnomer. Although the function's argument
   is an item cursor, a _list_ is bound to the variable. *)
val build_bind_type_checked_item_list_to_variable_code :
    Code_selection_context.code_selection_context ->
      Xquery_algebra_ast.asequencetype ->
        Xquery_common_ast.cvname ->
          (Physical_value.item Cursor.cursor -> unit)

val build_bind_sax_value_to_variable_code :
    Code_selection_context.code_selection_context ->
      Xquery_common_ast.cvname ->
        (Streaming_types.typed_xml_stream -> unit)


(* Tuples *)

(* Selects the _smallest_ physical xml type for the binding
   according to current command-line switches, tuple field use
   counts and independent input signature and type checking
   requirements.

   The physical type returned here may thus be different ('too
   small') from the actual type enforced by means of coercion
   functions. - Michael *)
val select_physical_tuple_binding : 
    Code_selection_context.code_selection_context ->
      Xquery_algebra_ast.input_signature ->
        (Xquery_algebra_ast.asequencetype option * Xquery_common_ast.crname) array ->
          Xquery_physical_algebra_ast.physop_tuple_binding

val build_bind_sax_value_to_tuple_field_code  :
    Code_selection_context.code_selection_context ->
      Xquery_common_ast.crname ->
        (Streaming_types.typed_xml_stream -> unit)

val build_bind_item_cursor_to_tuple_field_code  :
    Code_selection_context.code_selection_context ->
      Xquery_common_ast.crname ->
        (Physical_value.item Cursor.cursor -> unit)

val build_bind_type_checked_item_cursor_to_tuple_field_code  :
    Code_selection_context.code_selection_context ->
      Xquery_algebra_ast.asequencetype ->
        Xquery_common_ast.crname ->
          (Physical_value.item Cursor.cursor -> unit)

val build_bind_item_list_to_tuple_field_code  :
    Code_selection_context.code_selection_context ->
      Xquery_common_ast.crname ->
        (Physical_value.item list-> unit)

(* This is _not_ a misnomer. Although the function's argument
   is an item cursor, a _list_ is bound to the tuple field. *)
val build_bind_type_checked_item_list_to_tuple_field_code  :
    Code_selection_context.code_selection_context ->
      Xquery_algebra_ast.asequencetype ->
        Xquery_common_ast.crname ->
          (Physical_value.item Cursor.cursor -> unit)
