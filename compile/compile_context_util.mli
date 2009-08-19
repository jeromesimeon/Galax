(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: compile_context_util.mli,v 1.3 2007/02/01 22:08:45 simeon Exp $ *)

(* This function has been excides from Compile_context
   in order to resolve the cyclic dependency
   Logical_algebra_types -> Compile_context -> Logical_algebra_types.

    - Michael *)

open Compile_context

open Logical_algebra_types


(* More expensive, strip functions of their annotations *)
val copy_strip_functions   : ('a,'b) compile_context -> logical_compile_context
