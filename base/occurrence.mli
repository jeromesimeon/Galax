(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: occurrence.mli,v 1.8 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Occurrence
   Description:
     Manipulations of occurrences, i.e., an integer or
     'unbounded'. Used for XML Schema minOccurs, maxOccurs, and XQuery
     occurrence indicators.
*)

(* the type 'occurs' corresponds to minOccur and maxOccur *)

type occurs =
  | UP_INT of int
  | UNBOUNDED

type occurrence_indicator=
    (occurs * occurs)

(* operations on bounds *)

val occurs    : int -> occurs

val unbounded   : occurs
val occurs_zero : occurs
val occurs_one  : occurs

val ub_max  : occurs -> occurs -> occurs
val ub_min  : occurs -> occurs -> occurs

val ub_add  : occurs -> occurs -> occurs
val ub_mult : occurs -> occurs -> occurs

val mult    : occurs -> occurs -> occurs
val minus   : int -> occurs -> occurs
val equal   : occurs -> occurs -> bool
val le      : occurs -> occurs -> bool

(* prints bounds *)

val string_of_occurs : occurs -> string


(* Approximate occurrence indicators as used in XQuery *)

val one       : occurrence_indicator  (* Exactly one *)
val optional  : occurrence_indicator  (* Zero or one '?' *)
val star      : occurrence_indicator  (* Zero or more '*' *)
val plus      : occurrence_indicator  (* One or more '+' *)

val is_one       : occurrence_indicator -> bool
val is_optional  : occurrence_indicator -> bool
val is_star      : occurrence_indicator -> bool
val is_plus      : occurrence_indicator -> bool

val mult_occurrences : occurrence_indicator -> occurrence_indicator -> occurrence_indicator
val seq_occurrences : occurrence_indicator -> occurrence_indicator -> occurrence_indicator

(* Computes an approximate occurrence indicator *)

val approximate_occurrences : occurrence_indicator -> occurrence_indicator

