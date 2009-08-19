(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_type_core_ast_annotation.mli,v 1.3 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Xquery_type_core_ast_annotation
   Description:
     This module implements some annotations on the type AST.
*)

open Namespace_symbols
open Namespace_symbols_util
open Xquery_common_ast

type atomic_letter_mapping = (int ref) * int SQNameHashtbl.t
type cename_letter_mapping = (int ref) * (rtype_symbol option * nillable * int) SQNameHashtbl.t
type caname_letter_mapping = (int ref) * (rtype_symbol option * int) SQNameHashtbl.t
type piname_letter_mapping = (int ref) * (string option, int) Hashtbl.t
type letter_mappings =
    atomic_letter_mapping * cename_letter_mapping * caname_letter_mapping * piname_letter_mapping

val type_letter_map : letter_mappings -> atomic_letter_mapping
val elem_letter_map : letter_mappings -> cename_letter_mapping
val attr_letter_map : letter_mappings -> caname_letter_mapping
val pi_letter_map   : letter_mappings -> piname_letter_mapping

val create_letter_mappings : unit -> letter_mappings
val copy_letter_mappings   : letter_mappings -> letter_mappings

