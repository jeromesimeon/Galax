(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_norm_util.ml,v 1.3 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_norm_util
   Description:
     This module implements operations on schema which are used during
     XQuery normalization.
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


(*******************************)
(* Utilities for normalization *)
(*******************************)

let atomic_type_of_csequencetype cxschema csequencetype =
  match csequencetype.pcsequencetype_desc with
  | (CITAtomic a,_) ->
      let a = Namespace_symbols.rtype_symbol a in
      Schema_judge.atomic_type_of_typename cxschema a
  | _ -> ATAnyAtomic

let is_simple_type_csequencetype cxschema csequencetype =
  match csequencetype.pcsequencetype_desc with
  | (CITNumeric,_) -> true
  | (CITAtomic _,_) -> true
  | _ -> false

let check_optional_atomic_type_csequencetype cxschema csequencetype =
  match csequencetype.pcsequencetype_desc with
  | (CITAtomic a,oocc) ->
      begin
	if Namespace_names.rqname_equal a Namespace_builtin.xs_anyAtomicType then
	  raise (Query (Schema ("Atomic type may not be xs:anyAtomicType")))
	else
	  let a = Namespace_symbols.rtype_symbol a in
	  if (Schema_judge.derives_from cxschema a xs_anyAtomicType)
	  then
	    ()
	  else
	    raise (Query (Schema_Internal ("Typename "^(Namespace_symbols.symbol_prefix_string a ^" is not an atomic type"))));
	begin
	  match oocc with
	  | None -> ()
	  | Some occ ->
	      if Occurrence.is_optional occ
	      then
		()
	      else if Occurrence.is_one occ then
		()
	      else
		raise (Query (Schema ("Should be an optional atomic type")))
	end
      end
  | _ ->
      raise (Query (Schema ("Should be an optional atomic type")))

let get_optional_atomic_type_asequencetype cxschema asequencetype =
  match asequencetype.pasequencetype_desc with
  | (AITAtomic a,None) ->
      (a,false)
  | (AITAtomic a,Some occurrence_indicator) ->
      if Occurrence.is_optional occurrence_indicator
      then
	(a,true)
      else if Occurrence.is_one occurrence_indicator then
	(a,false)
      else
	raise (Query (Schema ("Should be an optional atomic type")))
  | _ ->
      raise (Query (Schema ("Should be an optional atomic type")))

let is_atomic_type_csequencetype cxschema csequencetype =
  match csequencetype.pcsequencetype_desc with
  | (CITNumeric,_) -> true
  | (CITAtomic a,_) ->
      let a = Namespace_symbols.rtype_symbol a in
      (Schema_judge.derives_from cxschema a xs_anyAtomicType)
	&& not(Schema_judge.derives_from cxschema xs_anyAtomicType a)
  | _ -> false

let is_numeric_type_csequencetype cxschema csequencetype =
  match csequencetype.pcsequencetype_desc with
  | (CITNumeric,_) -> true
  | (CITAtomic ty,_) ->
      let a = Namespace_symbols.rtype_symbol ty in
      let a = Schema_judge.atomic_type_of_typename cxschema a in
      Datatypes_util.atomic_is_numeric a
  | _ -> false

