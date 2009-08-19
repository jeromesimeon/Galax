(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_errors.ml,v 1.22 2007/05/02 19:31:02 mff Exp $ *)

(* Module: Typing_errors
   Description:
     This module implements supports for static typing errors.
 *)

open Format

open Error
open Occurrence

open Namespace_names
open Namespace_builtin

open Norm_context
open Processing_context 

open Datatypes

open Xquery_core_ast
open Xquery_ast_util

open Xquery_type_core_ast

open Schema_simplification
open Schema_util

open Typing_context

open Print_top


(***************)
(* Type errors *)
(***************)

(* Wrong expected type error *)

(* Note:
     A 'wrong expected type error' is raised when a specific type is
     expected and the actual type is not a subtype of it.
   - Jerome *)

(* Note:
     This error is effectively raised only when strong typing is
     on. In case of weak typing, the expected type is returned. The
     rationale is that *if* evaluation returns a value, *then* it will
     be of the corresponding type. This requires some synchronization
     with evaluation to make sure a dynamic error is raised if that is
     not the case.
   - Jerome *)

let raise_wrong_expected_type_error_aux stat_ctxt msg actual_type expected_type = 
  let schema = schema_from_static_context stat_ctxt in
  let norm_ctxt = (norm_context_from_stat_context stat_ctxt) in
  let proc_ctxt = (processing_context_from_norm_context norm_ctxt) in
  if (is_weak_typing proc_ctxt)
  then
    (* Note: in case of weak typing, resulting type is bottomed to the
       expected type. - Jerome *)
    expected_type
  else
    let simplified_actual_type = simplify_ty schema actual_type in
    raise (Query (Static_Type_Error ((msg^"\nExpecting type: "^(Print_top.bprintf_cxtype "" expected_type)^" but expression has type: "^(Print_top.bprintf_cxtype "" simplified_actual_type)))))

let raise_wrong_expected_type_error stat_ctxt actual_type expected_type = 
  raise_wrong_expected_type_error_aux stat_ctxt "" actual_type expected_type

(* Empty type error *)

let raise_empty_type_error stat_ctxt e actual_type =
  let norm_ctxt = (norm_context_from_stat_context stat_ctxt) in
  let proc_ctxt = (processing_context_from_norm_context norm_ctxt) in
  let estr = 
    match e.pcexpr_origin with
    | Some exp -> (bprintf_expr "" exp)
    | None -> "???"
  in
  if (is_weak_typing proc_ctxt)
  then
    (* Note: in case of weak typing, resulting type is bottomed to the
       actual type. - Jerome *)
    (eprintf_warning ("\nExpression '"^estr^"' at "^(Finfo.finfo_to_string e.pcexpr_loc) ^" has empty type, but is not the empty expression\n"); 
    actual_type)
  else
    raise (Query (Static_Type_Error ("Expression '"^estr^"' has empty type, but is not the empty expression")))   

(* None type error *)

let raise_none_type_error stat_ctxt e actual_type  =
  let norm_ctxt = (norm_context_from_stat_context stat_ctxt) in
  let proc_ctxt = (processing_context_from_norm_context norm_ctxt) in
  let estr = 
    match e.pcexpr_origin with
    | Some exp -> (bprintf_expr "" exp)
    |	None -> "???"
  in
  if (is_weak_typing proc_ctxt)
  then
    (* Note: in case of weak typing, resulting type is bottomed to the
       actual type. - Jerome *)
    (eprintf_warning ("\nExpression ' "^estr^"' has none type, but is not the error expression"); 
    actual_type)
  else
    raise (Query (Static_Type_Error ("Expression ' "^estr^"' has none type, but is not the error expression")))

let raise_axis_type_error stat_ctxt axis t = 
  raise_wrong_expected_type_error_aux stat_ctxt ("Applying axis "^(Print_common.string_of_axis axis)^" to non-node type:\n") t
  Schema_builtin.cxtype_node

(* 
   Class 1: check that the input type is smaller than an expected type
   and return that expected type.  In this case, weak typing by-passes
   the subtyping check, and a type assertion may occur at run-time.

   Example:

   In a user-defined function call, or in a type assertion.

     let $x as xs:integer :=
       if (true()) then 1 else "1"
     return $x

   check_type_replace
     actual type    : Type1
     expected type : Type2
        STRONG : if Type1 <: Type2 then Type2 else TYPE_ERROR
        WEAK   : Type2
*)
let check_type_replace stat_ctxt actual_type expected_type  =
  let schema = schema_from_static_context stat_ctxt in
  let norm_ctxt = (norm_context_from_stat_context stat_ctxt) in
  let proc_ctxt = (processing_context_from_norm_context norm_ctxt) in
  if (is_weak_typing proc_ctxt) then 
    expected_type 
  else if (Subtyping_top.is_subtype_of schema actual_type expected_type)
  then
    (* Here, replace by the expected type *)
    expected_type
  else
    raise_wrong_expected_type_error stat_ctxt actual_type expected_type 

(* 
   Class 2: check that the input type is smaller than an expected type
   and return the input type.  In this case, weak typing by-passes the
   subtyping check and returns the input type.

   Example: In constructors:

   <a>{if (true()) then //b else (<b/>,attribute b {})}</a>

   Typing rule:

   check_type_discard
     actual type   : Type1
     expected type : Type2
        STRONG : if Type1 <: Type2 return Type1 else TYPE_ERROR
        WEAK   : Type1
*)
let check_type_discard stat_ctxt actual_type expected_type  =
   let schema = schema_from_static_context stat_ctxt in
  let norm_ctxt = (norm_context_from_stat_context stat_ctxt) in
  let proc_ctxt = (processing_context_from_norm_context norm_ctxt) in
   if (is_weak_typing proc_ctxt) then 
       actual_type
   else if (Subtyping_top.is_subtype_of schema actual_type expected_type)
   then actual_type
   else
     raise_wrong_expected_type_error stat_ctxt actual_type expected_type 

(*

   Class 3: check that the input type is smaller than an expected type
   an return nothing. In that case weak typing is a no-op.

   Example: Check that the content of a validate expression is an
   element or document node.

   validate { if (true()) then <a/> else attribute a {} }

   Typing rule:

   check_type_ignore
     actual type    : Type1
     expected type : Type2
        STRONG : if Type1 <: Type2 then () else TYPE_ERROR
        WEAK   : ()
*)

let check_type_ignore stat_ctxt actual_type expected_type  =
  let schema = schema_from_static_context stat_ctxt in
  let norm_ctxt = (norm_context_from_stat_context stat_ctxt) in
  let proc_ctxt = (processing_context_from_norm_context norm_ctxt) in
  if (is_weak_typing proc_ctxt) then ()
  else if (Subtyping_top.is_subtype_of schema actual_type expected_type) then ()
  else 
   let _ = raise_wrong_expected_type_error stat_ctxt actual_type expected_type 
   in ()

(*

  Class 4: Check whether the input type is a subtype of the expected
  type and return the input type; however, in the case of weak typing,
  the input type is returned if the subtyping check succeeds or it
  branches out to the expected type.

   Example: This is used primarily for special built-in function for
   which the output type depends on the type of the input.

   let $x :=
     if (true())
     then //a
    else (1,2,3)
   return
     fs:distinct-docorder-or-atomic-sequence($x)

   Typing rule:

   check_type_branch
     actual type   : Type1
     expected type : Type2
       STRONG : if Type1 <: Type2 then Type1 else TYPE_ERROR
       WEAK   : if Type1 <: Type2 then Type1 else Type3
*)
let check_type_branch stat_ctxt actual_type expected_type weak_return_type =
   let schema = schema_from_static_context stat_ctxt in
   if (Subtyping_top.is_subtype_of schema actual_type expected_type)
   then actual_type 
   else raise_wrong_expected_type_error stat_ctxt actual_type weak_return_type

(* Check for inferred empty types for non-trivial expressions.  *)

(* Note:
     The following cases are considered as static type errors: 

       e : () and e is not trivially the () expression, or fn:data()
           or any fs: function applied to an expression with empty
           type.
       e : 0  and e is not trivially the error expression

   - Jerome *)

(*
   check_empty_type
     Expr : Type1
     if Type1 <: empty & (Expr not Empty) then Type1 else TYPE_ERROR
     if Type1 <: none  & (Expr not Error) then Type1 else TYPE_ERROR
*)
let check_empty_type stat_ctxt result_type e  =
  let schema = schema_from_static_context stat_ctxt in
  if (Subtyping_top.is_subtype_of_empty_sequence schema result_type)
  then
    match e.pcexpr_desc with
    | CESnap _ -> result_type
    | CEEmpty -> result_type
    | CECall (fname, _, _, _, _) -> 
	let  (prefix, uri, ncname) = fname in 
	if (fname = fn_data || uri = fs_uri) then result_type
	else raise_empty_type_error stat_ctxt e result_type 
    | _ -> raise_empty_type_error stat_ctxt e result_type 
  else
    if (Subtyping_top.is_subtype_of_empty_choice schema result_type) then
      match e.pcexpr_desc with
      | CEError _ -> result_type
      | _         -> raise_none_type_error stat_ctxt e result_type 
    else
      result_type

(* Check type declaration *)
let check_type_declaration stat_ctxt actual_type odt  =
  match odt with
  | None ->
      actual_type
  | Some (dt, cty) -> 
      check_type_replace stat_ctxt actual_type cty

