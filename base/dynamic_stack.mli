(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dynamic_stack.mli,v 1.2 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Dynamic_stack
     Implements a dynamic stack with direct access to the elements
     needed for e.g. twig joins

 *)

type 'a dynamic_stack = {
    mutable capacity : int;
    mutable size : int;
    mutable stack : 'a array;
    default : 'a;
  }

val make  : int -> 'a -> 'a dynamic_stack
val push  : 'a dynamic_stack -> 'a -> unit
val empty : 'a dynamic_stack -> bool
val pop   : 'a dynamic_stack -> 'a
val top   : 'a dynamic_stack -> 'a
