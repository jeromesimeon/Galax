(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: subtyping_build_regexp.mli,v 1.4 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Subtyping_build_regexp
   Description:
     This module implements type inclusion and intersection, by
     compiling core types into automatas using the glushkov
     construction.
*)

open Xquery_type_core_ast
open Subtyping_glushkov
open Subtyping_letter

(* First argument determines whether an occurrence of a type should be expanded to include all its derived type *)
val normalize_regexp : 
    bool -> Xquery_type_core_ast_annotation.letter_mappings -> cxschema -> cxtype -> TypeName_Glushkov.Regexp.regexp

