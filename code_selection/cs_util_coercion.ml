(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cs_util_coercion.ml,v 1.21 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Cs_util_coercion
   Description:
     This module implements coercion operation for algebraic operators
     input and output.
*)


open Xquery_algebra_ast
open Xquery_physical_type_ast
open Algebra_type
open Execution_context 

open Physical_value_util
open Physical_value
open Error

(* Module: Cs_util_coercion
   Description:
    Implementation of code to perform coercisions between differing datamodels
    and their helper functions 
*)


(* Signatures to clarify the types of some of the things
   below. *)

type 'a input_coercion_function = physical_value -> 'a 
type 'a return_coercion_function = 'a -> physical_value


(* val coerce_unit: 
  unit -> 'a return_coercion_function ->
  (Execution_context.algebra_context -> unit -> 'a)
  -> Algebra_type.alg_eval_code

val coerce_unary :
  'a input_coercion_function -> 'b return_coercion_function ->
  (Execution_context.algebra_context -> 'a -> 'b)
  -> Algebra_type.alg_eval_code

val coerce_binary : 
  'a input_coercion_function -> 'b return_coercion_function ->
  (Execution_context.algebra_context -> 'a -> 'a -> 'b)
  -> Algebra_type.alg_eval_code

val coerce_many :
  'a input_coercion_function -> 'b return_coercion_function ->
  (Execution_context.algebra_context -> 'a list -> 'b)
  -> Algebra_type.alg_eval_code
*)

(* These are helper functions (signatures above)
   their purpose is to apply the physical coercision
   functions 
   coerce: coercion (or fc1,fc2)
   rc: return coercion
   f: the function to be wrapped

   They return the constructed type (alg_eval_code) 
*)

let coerce_unit () rc f = 
  AOECUnit(fun alg_ctxt () -> 
    let f' = f alg_ctxt () in
    rc f')

let coerce_unary coerce rc f =
  AOECUnary(fun alg_ctxt to_coerce ->
    rc (f alg_ctxt (coerce to_coerce)))

let coerce_binary coerce rc f = 
  AOECBinary (fun alg_ctxt c1 c2 ->
    let x1 = coerce c1 in  (* Forces evaluation order *)
    let x2 = coerce c2 in
    rc (f alg_ctxt x1 x2))

let coerce_binary_hetero fc1 fc2 rc f = 
  AOECBinary (fun alg_ctxt c1 c2 ->
    let x1 = fc1 c1 in
    let x2 = fc2 c2 in
    rc (f alg_ctxt x1 x2))

let coerce_many coerce rc f = 
  AOECMany (fun alg_ctxt c_args ->
    rc (f alg_ctxt (Array.map coerce c_args)))


(* Same for the prolog operations *)

let coerce_unit_prolog () rc f = 
  PAOECUnit(fun alg_ctxt () -> 
    let (f':'a) = f alg_ctxt () in
    rc f')

let coerce_unary_prolog coerce rc f =
  PAOECUnary(fun alg_ctxt to_coerce ->
    rc (f alg_ctxt (coerce to_coerce)))

let coerce_binary_prolog coerce rc f = 
  PAOECBinary (fun alg_ctxt c1 c2 ->
    let x1 = coerce c1 in
    let x2 = coerce c2 in
    rc (f alg_ctxt x1 x2))

let coerce_binary_hetero_prolog fc1 fc2 rc f = 
  PAOECBinary (fun alg_ctxt c1 c2 ->
    let x1 = fc1 c1 in
    let x2 = fc2 c2 in
    rc (f alg_ctxt x1 x2))

let coerce_many_prolog coerce rc f = 
  PAOECMany (fun alg_ctxt c_args ->
		rc (f alg_ctxt (Array.map coerce c_args)))


(* for physical_model -> physical_model coercisions *)
let coerce_id x = x (* odd - explain this *)


(* Below here is exposed in the module *)
(* Parameter Coercions - for dependant parameters 
   see algebra_coercion.mli *)

let coerce_nodep input_code coercion_fun =
  NoDep ((fun ef -> coercion_fun input_code), None)

let coerce_unitdep input_code () coercion_fun =
  SomeDep ((fun ef ->
    let f' = input_code () ef in
    coercion_fun f'), None)
      
let coerce_onedep input_code dep_op coercion_fun =
  SomeDep ((fun  ef -> 
    let f' = input_code dep_op ef in		  
    coercion_fun f'), None)

let coerce_twodep input_code (ae1,ae2) coercion_fun =
  SomeDep ((fun ef -> 
    let f' = input_code (ae1,ae2) ef in
    coercion_fun f'), None)

let coerce_manydep input_code ae_array coercion_fun =
  SomeDep ((fun ef -> 
    let f' = input_code ae_array ef in
    coercion_fun f'), None)

let coerce_nodep_prolog input_code coercion_fun =
  PNoDep (coercion_fun input_code)

let coerce_onedep_prolog input_code dep_op coercion_fun =
  PSomeDep (fun ef ->
    let f' = input_code dep_op ef in
    coercion_fun f')


(* Signature helpers 
let make_signature_unit in_sig out_sig              = NoInput, out_sig
let make_signature_unary in_sig out_sig             = (OneInput in_sig), out_sig
let make_signature_binary (in_sig1,in_sig2) out_sig = (TwoInput (in_sig1, in_sig2)), out_sig 
let make_signature_many in_sig out_sig              = (ManyInput in_sig), out_sig
*)

(* Actual Coercision functions as explained in algebra_coercion.mli *)
let coerce_unit_to_xml f   = coerce_unit () physical_value_of_xml_value f

let coerce_unit_to_sax f   = coerce_unit () physical_value_of_sax_value f

let coerce_unit_to_item_cursor f   = coerce_unit () physical_value_of_item_cursor f

let coerce_unit_to_item_list f   = coerce_unit () physical_value_of_item_list f

let coerce_unit_to_physical_value f   = coerce_unit () coerce_id f

let coerce_unit_to_tuple f   = coerce_unit () physical_value_of_tuple f

let coerce_unit_to_item f   = coerce_unit () physical_value_of_item f

let coerce_unary_sax_to_sax f   = coerce_unary sax_value_of_physical_value physical_value_of_sax_value f

let coerce_binary_sax_to_sax f   = coerce_binary sax_value_of_physical_value physical_value_of_sax_value f

let coerce_unary_sax_to_item_list f   = coerce_unary sax_value_of_physical_value physical_value_of_item_list f

let coerce_unary_sax_to_item_cursor f   = coerce_unary sax_value_of_physical_value physical_value_of_item_cursor f

let coerce_unary_sax_to_tuple_cursor f   = coerce_unary sax_value_of_physical_value physical_value_of_tuple_cursor f

let coerce_unary_sax_to_physical_value f   = coerce_unary sax_value_of_physical_value coerce_id f

let coerce_unary_xml_to_xml f   = coerce_unary xml_value_of_physical_value physical_value_of_xml_value f

let coerce_unary_xml_to_item_list f   = coerce_unary xml_value_of_physical_value physical_value_of_item_list f

let coerce_unary_xml_to_tuple_cursor f   = coerce_unary xml_value_of_physical_value physical_value_of_tuple_cursor f

let coerce_unary_item_cursor_to_item_cursor f = 
 coerce_unary item_cursor_of_physical_value physical_value_of_item_cursor f

let coerce_unary_item_list_to_xml f = coerce_unary item_list_of_physical_value physical_value_of_xml_value f

let coerce_unary_item_cursor_to_xml f = coerce_unary item_cursor_of_physical_value physical_value_of_xml_value f

let coerce_unary_item_list_to_physical_value f = coerce_unary item_list_of_physical_value coerce_id f

let coerce_unary_item_list_to_item_list f   = coerce_unary item_list_of_physical_value physical_value_of_item_list f

let coerce_unary_item_list_to_tuple_cursor f   = coerce_unary item_list_of_physical_value physical_value_of_tuple_cursor f

let coerce_unary_item_cursor_to_item_list f   = coerce_unary item_cursor_of_physical_value physical_value_of_item_list f

let coerce_unary_tuple_cursor_to_tuple_cursor f = coerce_unary tuple_cursor_of_physical_value physical_value_of_tuple_cursor f

let coerce_unary_item_cursor_to_tuple_cursor f = coerce_unary item_cursor_of_physical_value physical_value_of_tuple_cursor f

let coerce_unary_tuple_cursor_to_item_cursor f = coerce_unary tuple_cursor_of_physical_value physical_value_of_item_cursor f

let coerce_unary_tuple_cursor_to_xml f = coerce_unary tuple_cursor_of_physical_value physical_value_of_xml_value f

let coerce_binary_tuple_cursor_to_tuple_cursor f = coerce_binary tuple_cursor_of_physical_value physical_value_of_tuple_cursor f

let coerce_binary_tuple_to_tuple f = coerce_binary (fun x -> Cursor.cursor_get_singleton (tuple_cursor_of_physical_value x)) (fun x -> physical_value_of_tuple_cursor (Cursor.cursor_of_singleton x)) f

let coerce_binary_item_cursor_to_item_cursor  f    = coerce_binary item_cursor_of_physical_value physical_value_of_item_cursor f

let coerce_binary_item_cursor_to_item_list  f   = coerce_binary item_cursor_of_physical_value physical_value_of_item_list f
let coerce_binary_item_cursor_to_tuple_cursor f = coerce_binary item_cursor_of_physical_value  physical_value_of_tuple_cursor f

let coerce_unary_tuple_cursor_to_physical_value f   = coerce_unary tuple_cursor_of_physical_value coerce_id f

let coerce_unary_item_cursor_to_physical_value f   = coerce_unary item_cursor_of_physical_value coerce_id f

let coerce_unary_xml_to_physical_value f = coerce_unary xml_value_of_physical_value coerce_id f

let coerce_binary_item_list_to_item_list f   = coerce_binary item_list_of_physical_value physical_value_of_item_list f

let coerce_binary_xml_and_sax_to_sax f   = 
  coerce_binary_hetero xml_value_of_physical_value sax_value_of_physical_value physical_value_of_sax_value f

let coerce_binary_item_cursor_and_sax_to_sax f   = 
  coerce_binary_hetero item_cursor_of_physical_value sax_value_of_physical_value physical_value_of_sax_value f

let coerce_binary_xml_and_xml_to_sax f   = 
  coerce_binary_hetero xml_value_of_physical_value xml_value_of_physical_value physical_value_of_sax_value f

let coerce_many_sax_to_sax f   = coerce_many sax_value_of_physical_value physical_value_of_sax_value f

let coerce_many_sax_to_item_list f   = coerce_many sax_value_of_physical_value physical_value_of_item_list f

let coerce_many_xml_to_tuple f = coerce_many xml_value_of_physical_value physical_value_of_tuple f

let coerce_many_item_cursor_to_item_cursor f = coerce_many item_cursor_of_physical_value physical_value_of_item_cursor f

let coerce_many_item_list_to_item_list f = coerce_many item_list_of_physical_value physical_value_of_item_list f

let coerce_many_item_cursor_to_physical_value f   = coerce_many item_cursor_of_physical_value coerce_id f

let coerce_binary_item_cursor_tuple_cursor_to_tuple_cursor f =
  coerce_binary_hetero item_cursor_of_physical_value tuple_cursor_of_physical_value physical_value_of_tuple_cursor f

(* Prolog coercions *)

let coerce_unary_item_cursor_to_algebra_context f =
    coerce_unary_prolog item_cursor_of_physical_value coerce_id f
let coerce_unit_to_algebra_context f =
    coerce_unit_prolog () coerce_id f

