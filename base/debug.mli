(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: debug.mli,v 1.7 2007/09/13 18:36:42 simeon Exp $ *)

(* Module: Debug
   Description:
     This module implements basic operations used for debugging.
*)

type debug_flag =
  | JoinDebug
  | TypeDebug
  | DefaultDebug
  | CompileDebug
  | StaticDebug
  | DxqDebug
  | MaterializationDebug

val print_join_debug : string -> unit
val print_dxq_debug : string -> unit
val print_materialization_debug : string -> unit
val print_typing_debug : string -> unit
val print_compile_debug : string -> unit
val print_static_debug : string -> unit
val print_default_debug : string -> unit
val sprintf_default_debug : ('a -> string, unit, string) format -> 'a -> unit

val set_debug : debug_flag list -> unit

val join_debug : unit -> bool
val typing_debug : unit -> bool
val dxq_debug : unit -> bool
val compile_debug : unit -> bool
val static_debug : unit -> bool
val materialization_debug : unit -> bool
val default_debug : unit -> bool

val debug_flag_of_string : string -> debug_flag

