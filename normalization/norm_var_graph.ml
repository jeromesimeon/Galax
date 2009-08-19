(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: norm_var_graph.ml,v 1.3 2007/07/31 17:34:08 simeon Exp $ *)

(* Module: Norm_var_graph
   Description:
     This module implements support for keeping track of global
     variable dependencies.
*)

open Error

open Namespace_names
open Namespace_util

open Xquery_common_ast
open Xquery_core_ast

open Processing_context


(* Types for the var graph *)

type var_or_fun =
  | GlobalVariable of rqname
  | FunctionDeclaration of rqname

type var_graph =
    { var_map : var_or_fun RQNameHashtbl.t;
      fun_map : var_or_fun RQNameHashtbl.t }

(* Init a var graph *)

let build_var_graph () = { var_map = RQNameHashtbl.create 167; fun_map = RQNameHashtbl.create 167 }
let reset_var_graph t = RQNameHashtbl.clear t.var_map; RQNameHashtbl.clear t.fun_map

(* Operations on the var graph *)

let var_eq v1 vof2 =
  match vof2 with
  | GlobalVariable v2 -> Namespace_names.rqname_equal v1 v2
  | FunctionDeclaration _ -> false

let fun_eq f1 vof2 =
  match vof2 with
  | GlobalVariable _ -> false
  | FunctionDeclaration f2 -> Namespace_names.rqname_equal f1 f2

let var_or_fun_eq vof1 vof2 =
  match (vof1,vof2) with
  | GlobalVariable v1,GlobalVariable v2 -> Namespace_names.rqname_equal v1 v2
  | FunctionDeclaration f1, FunctionDeclaration f2 -> Namespace_names.rqname_equal f1 f2
  | _ -> false

let not_var_dep t vn1 v2 =
  let prev = RQNameHashtbl.find_all t.var_map vn1 in
  not(List.exists (fun x -> var_or_fun_eq x v2) prev)

let not_fun_dep t fn1 v2 =
  let prev = RQNameHashtbl.find_all t.fun_map fn1 in
  not(List.exists (fun x -> var_or_fun_eq x v2) prev)

let add_dependency t v1 v2 =
  match v1 with
  | GlobalVariable vn1 ->
      if not_var_dep t vn1 v2
      then
	RQNameHashtbl.add t.var_map vn1 v2
  | FunctionDeclaration fn1 ->
      if not_fun_dep t fn1 v2
      then
	RQNameHashtbl.add t.fun_map fn1 v2

(* Check for cyclic dependencies *)

let rec split_var_funcs funcs l =
  match l with
  | [] -> ([],[])
  | (GlobalVariable v) :: l' ->
      let (vs,fs) = split_var_funcs funcs l' in
      (v::vs,fs)
  | (FunctionDeclaration f) :: l' ->
      let (vs,fs) = split_var_funcs funcs l' in
      if (RQNameHashtbl.mem funcs f) then (vs,fs) else (vs,f::fs)

let rec get_next_vars_from_funcs t funcs f1 =
  match f1 with
  | [] -> []
  | f :: f1' ->
      let next_from_fun = RQNameHashtbl.find_all t.fun_map f in
      let (v2,f2) = split_var_funcs funcs next_from_fun in
      v2@(get_next_vars_from_funcs t (RQNameHashtbl.add funcs f (); funcs) (f1'@f2))

let get_next_vars t var currentvars =
  let next_from_var = RQNameHashtbl.find_all t.var_map var in
  let funcs = RQNameHashtbl.create 167 in
  let (v1,f1) = split_var_funcs funcs next_from_var in
  v1 @ get_next_vars_from_funcs t funcs f1

let index = ref 0

let print_prevvars prevvars var =
  incr index;
  Printf.printf "Checking for variable: %s\n" (prefixed_string_of_rqname var);
  Printf.printf "Iteration: %i\n" !index;
  flush stdout;
  Printf.printf "prevvars:";
  RQNameHashtbl.iter (fun x -> fun y -> Printf.printf " %s;" (prefixed_string_of_rqname x)) prevvars;
  Printf.printf "\n";
  flush stdout

let rec no_cycle t prevvars var =
  if (Debug.static_debug()) then print_prevvars prevvars var;
  if (RQNameHashtbl.mem prevvars var)
  then
    raise (Query (Static_Error ("[err:XQST0054] Variable "^(prefixed_string_of_rqname var)^" depends on itself.]")))
  else
    let nextvars = get_next_vars t var [] in
    begin
      (* List.iter (fun x -> RQNameHashtbl.add prevvars x ()) nextvars; *)
      let prevvars = RQNameHashtbl.copy prevvars in
      RQNameHashtbl.add prevvars var ();
      List.iter (no_cycle t prevvars) nextvars;
    end

let print_var rqname =
  Printf.printf "$%s" (prefixed_string_of_rqname rqname)

let print_fun rqname =
  Printf.printf "%s()" (prefixed_string_of_rqname rqname)

let print_var_or_fun vof =
  match vof with
  | GlobalVariable v -> print_var v
  | FunctionDeclaration f -> print_fun f

let print_var_dep x y =
  Printf.printf "\t";
  print_var x;
  Printf.printf "  -->  ";
  print_var_or_fun y;
  Printf.printf "\n";
  flush stdout

let print_fun_dep x y =
  Printf.printf "\t";
  print_fun x;
  Printf.printf "  -->  ";
  print_var_or_fun y;
  Printf.printf "\n";
  flush stdout

let print_var_deps t =
  RQNameHashtbl.iter print_var_dep t

let print_fun_deps t =
  RQNameHashtbl.iter print_fun_dep t

let print_var_graph t =
  Printf.printf "-----------------------------\n";
  Printf.printf "Checking variable dependencies\n";flush stdout;
  Printf.printf "-----------------------------\n";
  Printf.printf "Var graph\n";
  Printf.printf "---------\n";
  print_var_deps t.var_map;
  Printf.printf "---------\n";
  Printf.printf "Fun graph\n";
  Printf.printf "---------\n";
  print_fun_deps t.fun_map;
  Printf.printf "-----------------------------\n"

let print_success () =
  Printf.printf "SUCCEEDED!\n";flush stdout;
  Printf.printf "-----------------------------\n"

let check_cyclic_variables t =
  if (Debug.static_debug()) then print_var_graph t;
  let prevvars = RQNameHashtbl.create 167 in
  RQNameHashtbl.iter (fun x -> fun y -> index := 0; no_cycle t prevvars x; RQNameHashtbl.clear prevvars) t.var_map;
  if (Debug.static_debug()) then print_success ()

