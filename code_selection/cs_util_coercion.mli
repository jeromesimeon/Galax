(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cs_util_coercion.mli,v 1.19 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Cs_util_coercion
   Description:
     This module implements coercion operation for algebraic operators
     input and output.
*)

open Xquery_algebra_ast

open Execution_context
open Algebra_type
open Cursor

open Streaming_types

open Physical_value



(* Coercion functions for the data models.
   These are characterized by:
   - Number of dependant algebraic parameters      
   - Number of independant algebraic parameters
   - Data Model of incoming parameters
       * Assumed to be homogenous unless otherwise stated
   - Data Model of outcoming parameter

   * Note: dependant expressions are not coerced so should
   really not be an axis that we differentiate on *)

(* The following are coercions that have to deal with
   dependant expressions. No coercision actually happens here.
   They serve to wrap up the types correctly 
   
   For typical usage see algebra_compile.ml
*)

val coerce_nodep:
  'a -> ('a -> alg_eval_code) -> alg_eval_code_dep

val coerce_unitdep:
  (unit -> eval_fun -> 'a) -> unit ->
  ('a -> alg_eval_code) -> alg_eval_code_dep 

val coerce_onedep:
  (algop_expr -> eval_fun -> 'a) -> algop_expr ->
  ('a -> alg_eval_code ) -> alg_eval_code_dep 

val coerce_twodep:
  ((algop_expr * algop_expr) -> eval_fun -> 'a) -> (algop_expr * algop_expr) ->
  ('a -> alg_eval_code ) -> alg_eval_code_dep 

val coerce_manydep:
  ((algop_expr array) -> eval_fun -> 'a) -> algop_expr array ->
  ('a -> alg_eval_code ) -> alg_eval_code_dep 


val coerce_nodep_prolog:
  'a -> ('a -> alg_eval_code_prolog) -> alg_eval_code_dep_prolog 

val coerce_onedep_prolog:
  (algop_expr -> eval_fun -> 'a) -> algop_expr ->
  ('a -> alg_eval_code_prolog) -> alg_eval_code_dep_prolog



(* These are coercions for the physical data model
   The nomenclature is given n input parameters are of type x 
   and the return type of type y. coerce_n_x_to_y. 
   
   Parameters are assumed to be of the same type unless
   otherwise stated (coerce_binary_xml_and_sax_to_sax for example). *)

val coerce_unit_to_xml:
  (algebra_context -> unit -> xml_value)
  -> (alg_eval_code)

val coerce_unit_to_sax:
  (algebra_context -> unit -> typed_xml_stream)
  -> (alg_eval_code)

val coerce_unit_to_item_cursor:
  (algebra_context -> unit -> item cursor)
  -> alg_eval_code

val coerce_unit_to_item_list:
  (algebra_context -> unit -> item list)
  -> alg_eval_code

val coerce_unit_to_physical_value:
  (algebra_context -> unit -> physical_value)
  -> alg_eval_code

val coerce_unit_to_tuple:
  (algebra_context -> unit -> tuple_unit)
  -> alg_eval_code

val coerce_unit_to_item:
  (algebra_context -> unit -> item)
  -> alg_eval_code

val coerce_unary_sax_to_sax:
  (algebra_context -> typed_xml_stream -> typed_xml_stream)
  -> alg_eval_code  

val coerce_binary_sax_to_sax:
  (algebra_context -> typed_xml_stream -> typed_xml_stream -> typed_xml_stream)
  -> alg_eval_code  

val coerce_unary_sax_to_item_list:
  (algebra_context -> typed_xml_stream -> item list)
  -> alg_eval_code  

val coerce_unary_sax_to_item_cursor:
  (algebra_context -> typed_xml_stream -> item cursor)
  -> alg_eval_code  

val coerce_unary_sax_to_tuple_cursor:
  (algebra_context -> typed_xml_stream -> tuple_unit cursor)
  -> alg_eval_code  

val coerce_unary_sax_to_physical_value:
  (algebra_context -> typed_xml_stream -> physical_value)
  -> alg_eval_code  

val coerce_unary_xml_to_xml:
  (algebra_context -> xml_value -> xml_value)
  -> alg_eval_code

val coerce_unary_xml_to_physical_value:
  (algebra_context -> xml_value -> physical_value)
  -> alg_eval_code

val coerce_unary_xml_to_item_list:
  (algebra_context -> xml_value -> item list)
  -> alg_eval_code

val coerce_unary_xml_to_tuple_cursor:
  (algebra_context -> xml_value -> tuple_unit cursor)
  -> alg_eval_code

val coerce_unary_item_cursor_to_tuple_cursor:
  (algebra_context -> item cursor -> tuple_unit cursor)
  -> alg_eval_code

val coerce_unary_tuple_cursor_to_item_cursor:
  (algebra_context -> tuple_unit cursor -> item cursor)
  -> alg_eval_code

val coerce_unary_tuple_cursor_to_tuple_cursor:
  (algebra_context -> tuple_unit cursor -> tuple_unit cursor)
  -> alg_eval_code

val coerce_unary_tuple_cursor_to_xml:
  (algebra_context -> tuple_unit cursor -> xml_value)
  -> alg_eval_code

val coerce_binary_tuple_cursor_to_tuple_cursor:
  (algebra_context -> tuple_unit cursor -> tuple_unit cursor -> tuple_unit cursor)
  -> alg_eval_code

val coerce_binary_tuple_to_tuple:
  (algebra_context -> tuple_unit -> tuple_unit -> tuple_unit)
  -> alg_eval_code

val coerce_binary_item_cursor_to_item_cursor : 
  (algebra_context -> item cursor -> item cursor -> item cursor)
  -> alg_eval_code

val coerce_binary_item_cursor_to_item_list : 
  (algebra_context -> item cursor -> item cursor -> item list)
  -> alg_eval_code

val coerce_binary_item_cursor_to_tuple_cursor :
    (algebra_context -> item cursor -> item cursor -> tuple_unit cursor) -> alg_eval_code

val coerce_unary_item_cursor_to_item_cursor:
  (algebra_context -> item cursor -> item cursor)
  -> alg_eval_code

val coerce_unary_item_list_to_item_list:
  (algebra_context -> item list -> item list)
  -> alg_eval_code

val coerce_unary_item_cursor_to_item_list :
    (algebra_context -> item cursor -> item list) -> alg_eval_code

val coerce_unary_item_list_to_xml :
    (algebra_context -> item list -> xml_value) -> alg_eval_code

val coerce_unary_item_list_to_physical_value :
    (algebra_context -> item list -> physical_value) -> alg_eval_code

val coerce_unary_item_list_to_tuple_cursor :
    (algebra_context -> item list -> tuple_unit cursor) -> alg_eval_code

val coerce_unary_item_cursor_to_physical_value:
  (algebra_context -> item cursor -> physical_value) -> alg_eval_code

val coerce_unary_item_cursor_to_xml:
  (algebra_context -> item cursor -> xml_value)
  -> alg_eval_code

val coerce_unary_tuple_cursor_to_physical_value:
  (algebra_context -> tuple_unit cursor -> physical_value)
  -> alg_eval_code

val coerce_binary_item_list_to_item_list :
  (algebra_context -> item list -> item list -> item list)
  -> alg_eval_code

val coerce_binary_xml_and_sax_to_sax :
  (algebra_context -> xml_value -> typed_xml_stream -> typed_xml_stream) -> alg_eval_code

val coerce_binary_item_cursor_and_sax_to_sax :
    (algebra_context -> item cursor -> typed_xml_stream -> typed_xml_stream) -> alg_eval_code

val coerce_binary_xml_and_xml_to_sax :
  (algebra_context -> xml_value -> xml_value -> typed_xml_stream)
  -> alg_eval_code

val coerce_many_sax_to_sax :
  (algebra_context -> typed_xml_stream array -> typed_xml_stream)
  -> alg_eval_code

val coerce_many_sax_to_item_list :
  (algebra_context -> typed_xml_stream array -> item list)
  -> alg_eval_code

val coerce_many_xml_to_tuple:
  (algebra_context -> xml_value array -> tuple_unit)
  -> alg_eval_code

val coerce_many_item_cursor_to_item_cursor:
  (algebra_context -> item cursor array -> item cursor)
  -> alg_eval_code 

val coerce_many_item_list_to_item_list:
  (algebra_context -> item list array -> item list)
  -> alg_eval_code 

val coerce_many_item_cursor_to_physical_value:
  (algebra_context -> item cursor array -> physical_value)
  -> alg_eval_code

val coerce_binary_item_cursor_tuple_cursor_to_tuple_cursor:
  (algebra_context -> item cursor -> tuple_unit cursor -> tuple_unit cursor)
  -> alg_eval_code

(* Prolog coercions *)

val coerce_unary_item_cursor_to_algebra_context :
    (algebra_context -> item cursor -> algebra_context)
  -> alg_eval_code_prolog

val coerce_unit_to_algebra_context :
    (algebra_context -> unit -> algebra_context)
  -> alg_eval_code_prolog
