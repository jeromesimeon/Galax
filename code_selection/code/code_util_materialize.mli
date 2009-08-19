(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util_materialize.mli,v 1.8 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_util_materialize
   Description:
     This module contains code to handle the materialization of tuple
     cursors into an array of physical xml value arrays (the
     individual tuples)

     Additionally it returns a function which stores one of these
     tuples without names into the "current" tuple slot destructively.
 *)

open Xquery_common_ast

open Physical_value

type sequence_index   = int
type restore_function = dom_tuple -> unit

(*******************************************
 Simple exporting(importing) of the INPUT tuple 
 to/from a tuple cursor Used in serialization 
 of a table.
 *******************************************)
(* Currently, export_input_tuple_as_tuple_cursor materializes in horizontal dimension *)
val export_input_tuple_as_tuple_cursor :
    Code_selection_context.code_selection_context -> 
      Xquery_physical_type_ast.physical_tuple_type -> 
	Physical_value.tuple_unit Cursor.cursor ->
	  Physical_value.tuple Cursor.cursor

val import_tuple_cursor_as_input_tuple :
    Code_selection_context.code_selection_context -> 
      Xquery_physical_type_ast.physical_tuple_type -> 
	Physical_value.tuple Cursor.cursor ->
	  Physical_value.tuple_unit Cursor.cursor

type array_materialize_fun =
    Algebra_type.eval_fun ->
      Execution_context.algebra_context ->
	tuple_unit Cursor.cursor -> dom_table

(* Get back the array of tuples *)
val materialize_cursor_to_dom_value_array :
  (* Code selection context is needed to get the "needed names" or the
     tuple field names which are returned as the last portion of the
     triplet *)
  Code_selection_context.code_selection_context ->
    Xquery_algebra_ast.free_variable_desc -> unit
      -> array_materialize_fun * restore_function * cvname array

(* Materialize to a hash table, which maps items (computed by the
   provided expression) -> their sequence number can also be used for
   just mem if not in doc_order*)

type hash_materialize_fun =
    Algebra_type.eval_fun -> Execution_context.algebra_context -> 
  dom_table -> 
  int Dm_atomic.AtomicValueHash.t

val materialize_array_to_hash :
    Code_selection_context.code_selection_context ->
      Xquery_algebra_ast.free_variable_desc
      -> restore_function (* for the array *)
	-> (Code_util_predicates.predicate_branch * Namespace_context.nsenv)
	  -> hash_materialize_fun * restore_function * cvname array

(* Sorted materialization functions *)
type sort_array_materialize_fun  = 
    Algebra_type.eval_fun -> Execution_context.algebra_context 
      -> dom_table
	-> Code_util_ridlist.rid Dm_atomic_btree.btree (* in sorted order *)

(* Get back the array of tuples *)
val materialize_array_to_sorted_array_index :
  (* Code selection context is needed to get the "needed names" or the
     tuple field names which are returned as the last portion of the
     triplet *)
  Code_selection_context.code_selection_context ->
    Xquery_algebra_ast.free_variable_desc
    -> restore_function (* for the array *)
      -> (Code_util_predicates.predicate_branch * Namespace_context.nsenv)
	-> sort_array_materialize_fun


(* Materializes a cursor. Used when materialization is needed in a statement with side effects *)
val build_materialize_table_code : 
  ('a, 'b) Xquery_algebra_ast.aalgop_expr -> 
  Code_selection_context.code_selection_context -> 
  Algebra_type.eval_fun ->
  (Execution_context.algebra_context -> tuple_unit Cursor.cursor -> tuple_unit Cursor.cursor)

(* The decision concerning materialization is stored in annotations that need to be computed beforehand *)
val annotate_materialization : Code_selection_context.code_selection_context -> Algebra_type.algop_expr -> unit

val should_materialize : Algebra_type.algop_expr -> bool

val produces_a_table : Algebra_type.algop_expr -> bool
