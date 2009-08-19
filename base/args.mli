(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: args.mli,v 1.11 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Args
   Description:
     This module implements extraction of function arguments for
     various arities.
*)

(* Arguments extraction *)

val get_param0 :  'a list -> unit
val get_param1 :  'a list -> 'a
val get_param2 :  'a list -> ('a * 'a)
val get_param3 :  'a list -> ('a * 'a * 'a)
val get_param4 :  'a list -> ('a * 'a * 'a * 'a)

val get_array_param0 :  'a array -> unit
val get_array_param1 :  'a array -> 'a
val get_array_param2 :  'a array -> ('a * 'a)
val get_array_param3 :  'a array -> ('a * 'a * 'a)
val get_array_param4 :  'a array -> ('a * 'a * 'a * 'a)

