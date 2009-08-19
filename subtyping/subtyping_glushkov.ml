(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: subtyping_glushkov.ml,v 1.7 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Subtyping_glushkov
   Description:
     This module implements type inclusion and intersection, by
     compiling regular expressions into automatas using the glushkov
     construction.
*)

open Error
open Occurrence

open Namespace_names

open Xquery_common_ast
open Xquery_type_core_ast
open Subtyping_letter

module OrderedNamePairs =
  struct
    type t = actual_letter

    let compare l1 l2 = compare_letters l1 l2
  end

module TypeName_Glushkov = Regexp.MakeGlushkovType(OrderedNamePairs)

