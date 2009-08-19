(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_util.ml,v 1.40 2007/09/25 15:12:43 mff Exp $ *)

(* Module: Schema_util
   Description:
     This modules implements some useful basic operations on schemas.
 *)

open Error

open Datatypes

open Namespace_symbols
open Namespace_symbols_builtin
open Namespace_symbols_util

open Xquery_common_ast

open Xquery_core_ast
open Xquery_core_ast_util

open Xquery_type_core_ast
open Xquery_type_core_ast_util

open Xquery_algebra_ast
open Xquery_algebra_ast_util


(* Empty types *)
let cxtype_empty = fmkcxtype_builtin CEmpty 
let cxtype_none =   fmkcxtype_builtin CNone

(******************)
(* Some utilities *)
(******************)

let is_xs_anytype ctname1 =
  symbol_equal ctname1 xs_anyType

let is_empty_cxtype cxtype =
  match cxtype.pcxtype_desc with
  | CEmpty -> true
  | _ -> false

let rec is_really_empty_cxtype cxtype =
  match cxtype.pcxtype_desc with
  | CAtomicRef _
  | CElementRef _
  | CElementLocal _
  | CAttributeRef _
  | CAttributeLocal _
  | CDocument _
  | CText
  | CPI _
  | CComment -> false
  | CBound (cxtype1,bound1,bound2) ->
      (is_really_empty_cxtype cxtype1)
  | CSequence (cxtype1,cxtype2) ->
      (is_really_empty_cxtype cxtype1) && (is_really_empty_cxtype cxtype2)
  | CEmpty -> true
  | CChoice (cxtype1,cxtype2) ->
      (is_really_empty_cxtype cxtype1) && (is_really_empty_cxtype cxtype2)
  | CNone -> true
  | CInterleave (cxtype1,cxtype2) ->
      (is_really_empty_cxtype cxtype1) && (is_really_empty_cxtype cxtype2)

let is_none_cxtype m =
  match m.pcxtype_desc with
  | CNone -> true
  | _ -> false

(* Flattens a choice into its components *)

let rec list_of_choice cxtype =
  match cxtype.pcxtype_desc with
  | CChoice (cxtype1,cxtype2) ->
      (list_of_choice cxtype1) @ (list_of_choice cxtype2)
  | _ ->
      cxtype :: []

let rec choice_of_list ms =
  let ms' = List.filter (fun f -> not(is_none_cxtype f)) ms in
  match ms' with
  | [m] -> m
  | m :: ms'' ->
      fmkcxtype (CChoice (m,choice_of_list ms'')) Finfo.bogus
  | [] ->
      cxtype_none

(*************)
(* Factoring *)
(*************)

let rec factor_aux m =
  match m.pcxtype_desc with
  | CAtomicRef _
  | CAttributeRef _
  | CElementRef _
  | CAttributeLocal _
  | CElementLocal _
  | CDocument _
  | CText
  | CPI _
  | CComment ->
      (m :: [], Occurrence.occurs 1, Occurrence.occurs 1)
  | CBound (m',i,j) ->
      let (m'',i',j') = factor_aux m' in
      (m'', Occurrence.ub_mult i' i, Occurrence.ub_mult j' j)
  | CInterleave (m1,m2) (* Treat interleave like sequence - Jerome *)
  | CSequence (m1, m2) ->
      let (m1', i1, j1) = factor_aux m1 in
      let (m2', i2, j2) = factor_aux m2 in
      ((m1' @ m2'), Occurrence.ub_add i1 i2, Occurrence.ub_add j1 j2)
  | CEmpty ->
      ((fmkcxtype CNone Finfo.bogus) :: [],Occurrence.occurs 0,Occurrence.occurs 0)
  | CChoice (m1, m2) ->
      let (m1', i1, j1) = factor_aux m1 in
      let (m2', i2, j2) = factor_aux m2 in
      ((m1' @ m2'), Occurrence.ub_min i1 i2, Occurrence.ub_max j1 j2)
  | CNone ->
      ((fmkcxtype CNone Finfo.bogus) :: [],Occurrence.UNBOUNDED, Occurrence.occurs 0)

(* Factor *)

let factor_with_units m =
  let (mts,i,j) = factor_aux m in
  let (i',j') = Occurrence.approximate_occurrences (i,j) in
  (mts, i', j')

let factor_with_units m =
  if (is_really_empty_cxtype m)
  then
    ([cxtype_empty],  Occurrence.UP_INT 0,  Occurrence.UP_INT 0)
  else
    let (mts,i,j) = factor_aux m in
    let (i',j') = Occurrence.approximate_occurrences (i,j) in
    (mts, i', j')

let factor m =
  if (is_really_empty_cxtype m)
  then
    (cxtype_empty,  Occurrence.UP_INT 0,  Occurrence.UP_INT 0)
  else
    let (mts,i,j) = factor_aux m in
    let (i',j') = Occurrence.approximate_occurrences (i,j) in
    (choice_of_list mts, i', j')

(* Turns a factored type back into a type *)
let defactor (m,i,j) = 
  fmkcxtype (CBound (m, i, j)) m.pcxtype_loc

(**********************)
(* Type Constructors  *)
(**********************)

let make_sequence_cxtypes cxtype1 cxtype2 =
  if (is_empty_cxtype cxtype1)
  then cxtype2
  else if (is_empty_cxtype cxtype2)
  then cxtype1
  else fmkcxtype (CSequence (cxtype1, cxtype2)) cxtype1.pcxtype_loc

(* No need to check for empty on builtin types *)
let make_builtin_sequence_cxtypes cxtype1 cxtype2 =
  fmkcxtype_builtin (CSequence (cxtype1, cxtype2))

(* We do not support interleaved types in Subtyping module yet so we
   approximate them with factored types. *)
let make_interleave_cxtypes cxtype1 cxtype2 =
  if (is_empty_cxtype cxtype1)
  then cxtype2
  else if (is_empty_cxtype cxtype2)
  then cxtype1
  else defactor (factor (fmkcxtype (CInterleave (cxtype1, cxtype2)) Finfo.bogus))

let make_builtin_interleave_cxtypes cxtype1 cxtype2 =
  let t = make_interleave_cxtypes cxtype1 cxtype2 
  in t.pcxtype_simplified <- true; 
  t

let make_choice_cxtypes cxtype1 cxtype2 =
  if (is_none_cxtype cxtype1)
  then cxtype2
  else if (is_none_cxtype cxtype2)
  then cxtype1
  else fmkcxtype (CChoice (cxtype1, cxtype2)) Finfo.bogus

(* No need to check for empty on builtin types *)
let make_builtin_choice_cxtypes cxtype1 cxtype2 =
  fmkcxtype_builtin (CChoice (cxtype1, cxtype2)) 

let make_builtin_atomic_type ctname =
  fmkcxtype_builtin (CAtomicRef ctname)

(* Type lookup *)  

let make_atomic_type cxschema (ctname)  =
  try
    let decl = SQNameHashtbl.find cxschema.cxschema_type_declarations (ctname) in
    match decl.ctypedecl_cxtype with
    | None ->
	fmkcxtype (CAtomicRef ctname) Finfo.bogus
    | Some t -> t
  with
  | Not_found -> fmkcxtype (CAtomicRef ctname) Finfo.bogus

let make_optional_type cxtype =
  fmkcxtype (CBound (cxtype,Occurrence.occurs_zero,Occurrence.occurs_one)) Finfo.bogus

let make_builtin_optional_type cxtype =
  fmkcxtype_builtin (CBound (cxtype,Occurrence.occurs_zero,Occurrence.occurs_one))

let make_zeroormore_type cxtype =
  fmkcxtype (CBound (cxtype,Occurrence.occurs_zero,Occurrence.unbounded)) Finfo.bogus

let make_builtin_zeroormore_type cxtype =
  fmkcxtype_builtin (CBound (cxtype,Occurrence.occurs_zero,Occurrence.unbounded))

let make_oneormore_type cxtype =
  fmkcxtype (CBound (cxtype,Occurrence.occurs_one,Occurrence.unbounded)) Finfo.bogus

let make_builtin_oneormore_type cxtype =
  fmkcxtype_builtin (CBound (cxtype,Occurrence.occurs_one,Occurrence.unbounded)) 

let make_builtin_opt_attribute_ref an =
  make_builtin_optional_type (fmkcxtype_builtin (CAttributeRef an))

let mixed_content = make_builtin_zeroormore_type (fmkcxtype_builtin CText)

let text_content = mixed_content

let rec is_simple_cxtype cxtype =
  match cxtype.pcxtype_desc with
  | CAtomicRef _ ->
      true
  | CElementRef _ ->
      false
  | CElementLocal _ ->
      false
  | CAttributeRef _ ->
      false
  | CAttributeLocal _ ->
      false
  | CDocument _ ->
      false
  | CText ->
      false
  | CPI _ ->
      false
  | CComment ->
      false
  | CBound (cxtype1,_,_) ->
      is_simple_cxtype cxtype1
  | CSequence (cxtype1, cxtype2) ->
      ((is_simple_cxtype cxtype1) && (is_simple_cxtype cxtype2))
  | CEmpty ->
      true
  | CChoice (cxtype1, cxtype2) ->
      ((is_simple_cxtype cxtype1) && (is_simple_cxtype cxtype2))
  | CNone ->
      true
  | CInterleave (cxtype1, cxtype2) ->
      ((is_simple_cxtype cxtype1) && (is_simple_cxtype cxtype2))


let rec attribute_only cxtype =
  match cxtype.pcxtype_desc with
  | CAtomicRef _ ->
      false
  | CElementRef _ ->
      false
  | CElementLocal _ ->
      false
  | CAttributeRef _ ->
      true
  | CAttributeLocal _ ->
      true
  | CDocument _ ->
      false
  | CText ->
      false
  | CPI _ ->
      false
  | CComment ->
      false
  | CBound (cxtype1,_,_) ->
      attribute_only cxtype1
  | CSequence (cxtype1, cxtype2) ->
      ((attribute_only cxtype1) && (attribute_only cxtype2))
  | CEmpty ->
      true
  | CChoice (cxtype1, cxtype2) ->
      ((attribute_only cxtype1) && (attribute_only cxtype2))
  | CNone ->
      true
  | CInterleave (cxtype1, cxtype2) ->
      ((attribute_only cxtype1) && (attribute_only cxtype2))


let rec no_attribute cxtype =
  match cxtype.pcxtype_desc with
  | CAtomicRef _ ->
      true
  | CElementRef _ ->
      true
  | CElementLocal _ ->
      true
  | CAttributeRef _ ->
      false
  | CAttributeLocal _ ->
      false
  | CDocument _ ->
      true
  | CText ->
      true
  | CPI _ ->
      true
  | CComment ->
      true
  | CBound (cxtype1,_,_) ->
      no_attribute cxtype1
  | CSequence (cxtype1, cxtype2) ->
      ((no_attribute cxtype1) && (no_attribute cxtype2))
  | CEmpty ->
      true
  | CChoice (cxtype1, cxtype2) ->
      ((no_attribute cxtype1) && (no_attribute cxtype2))
  | CNone ->
      true
  | CInterleave (cxtype1, cxtype2) ->
      ((no_attribute cxtype1) && (no_attribute cxtype2))

let separate_attributes_from_content cxtype =
  if no_attribute cxtype
  then
    (cxtype_empty,cxtype)
  else
    if attribute_only cxtype
    then
      (cxtype, cxtype_empty)
    else
      match cxtype.pcxtype_desc with
      | CSequence (cxtype1, cxtype2) ->
	  if (attribute_only cxtype1) &&  (no_attribute cxtype2)
	  then
	    (cxtype1, cxtype2)
	  else
	    raise (Query (Schema_Internal ("Mal-formed element content")))
      | _ ->
	  raise (Query (Schema_Internal ("Mal-formed element content")))

let rec is_built_in_atomic_type cxschema ctname =
  try
    ignore(Datatypes_util.lookup_bltin_type ctname);
    true
  with
  | _ ->
      false

(***********************)
(* Content model kinds *)
(***********************)

type simple_kind =
  | AtomicKind of rtype_symbol
  | UnionKind of rtype_symbol list
  | ListKind of rtype_symbol list  (* It can be a list in case this is a list over a union *)

type complex_kind = cxtype

type content_kind =
  | ComplexKind of complex_kind
  | SimpleKind of simple_kind


let extends_attribute_content cattribute_content2 cattribute_content =
  match (cattribute_content2,cattribute_content) with
  | None,a -> a
  | a,None -> a
  | (Some cxtype1,Some cxtype2) ->
      Some (make_interleave_cxtypes cxtype1 cxtype2)

let extends_element_content cxtype1 cxtype2 =
  make_sequence_cxtypes cxtype1 cxtype2

