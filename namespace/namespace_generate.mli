(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_generate.mli,v 1.1 2007/02/12 21:15:33 simeon Exp $ *)

(* Module: Namespace_generate
   Description:
     This module contains support for QName creation.
*)

(* Note:
     The compiler makes intensive use of this, during normalization,
     algebraic compilation, andoptimization.
     This module consolidate bits of pieces of code scattered accross
     the compiler, and making sure the result of compilation is stable.
  - Jerome
*)

open Namespace_names

type name_gen

val create_name_generator : prefix -> uri -> string -> name_gen

val reset_name_generator : name_gen -> unit

val generate_name : name_gen -> rqname
val generate_name_with_prefix : name_gen -> string -> rqname

