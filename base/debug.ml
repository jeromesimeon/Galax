(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: debug.ml,v 1.10 2007/09/13 18:36:42 simeon Exp $ *)

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

let debug_flag_of_string s =
  match s with
  | "join" -> JoinDebug
  | "typing" -> TypeDebug
  | "default" -> DefaultDebug
  | "compile" -> CompileDebug
  | "static" -> StaticDebug
  | "dxq" -> DxqDebug
  | "materialization" -> MaterializationDebug
  | _ -> raise Not_found

let string_of_flag f =
  match f with
  | JoinDebug -> " JOIN"
  | TypeDebug -> " TYPING"
  | DxqDebug -> " DXQ"
  | StaticDebug -> " STATIC"
  | CompileDebug -> " COMPILE"
  | MaterializationDebug -> "MATERIALIZATION"
  | DefaultDebug -> ""

let debug_flags = ref []

let print_debug flag msg =
  let fs = string_of_flag flag in
  Format.fprintf (!Conf.glx_err_formatter) "[DEBUG%s] %s@.%!" fs msg;
  Format.pp_print_flush (!Conf.glx_err_formatter) ()

let join_debug ()    = List.exists (fun x -> x = JoinDebug) !debug_flags
let dxq_debug ()     = List.exists (fun x -> x = DxqDebug) !debug_flags
let typing_debug ()  = List.exists (fun x -> x = TypeDebug) !debug_flags
let compile_debug ()  = List.exists (fun x -> x = CompileDebug) !debug_flags
let static_debug ()  = List.exists (fun x -> x = StaticDebug) !debug_flags
let materialization_debug ()  = List.exists (fun x -> x = MaterializationDebug) !debug_flags
let default_debug () = List.exists (fun x -> x = DefaultDebug) !debug_flags

let print_join_debug msg =
  if join_debug() then
  print_debug JoinDebug msg

let print_typing_debug msg =
  if typing_debug() then
  print_debug TypeDebug msg

let print_compile_debug msg =
  if compile_debug() then
  print_debug CompileDebug msg

let print_default_debug msg =
  if default_debug() then 
  print_debug DefaultDebug msg

let print_dxq_debug msg =
  if dxq_debug() then 
  print_debug DxqDebug msg

let print_static_debug msg =
  if static_debug() then 
  print_debug StaticDebug msg

let print_materialization_debug msg =
  if materialization_debug() then 
  print_debug MaterializationDebug msg

let sprintf_default_debug ff x =
  let s = Format.sprintf ff x in
  print_default_debug s

let set_debug df = debug_flags := df


