(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: alg_analysis_context.ml,v 1.5 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Alg_analysis_context
   Description:
     Path analysis for document projection over the XQuery algebra.
*)

open Error

open Xquery_common_ast

open Alg_path_struct


(* returned paths of an expression, returned paths contained in tuple fields, used paths of an expression, paths modified by an expression  *)
type paths = rooted_path_sequence * (Xquery_common_ast.crname * rooted_path_sequence) list * rooted_path_sequence * rooted_path_sequence

(* paths bound to environmental variables, paths bound to the input tuple *)
type analysis_context =
    (cvname * rooted_path_sequence) list * (cvname * rooted_path_sequence) list


let build_analysis_context () =
  ([], [])

let add_var_paths analysis_ctxt var paths =
  match analysis_ctxt with
    | (var_paths, it_paths) -> ((var, paths) :: var_paths, it_paths)

let get_var_paths analysis_ctxt var =
  match analysis_ctxt with
    | (var_paths, _) ->
	begin
	  try
	    List.assoc var var_paths
	  with
	    | _ -> raise (Query (Prototype ("Cannot find variable $"^(Namespace_names.prefixed_string_of_rqname var)^" in alg_analysis_context.")))
	end

(* Mirrors the implicit input tuple for algebra operations *)	  
let set_input_tuple_paths analysis_ctxt paths =
  match analysis_ctxt with
    | (var_paths, it_paths) ->
	(*begin
	  match paths with
	    | [] -> print_string "Bound empty input tuple paths."
	    | (cvname, path) :: tl -> print_string "SET INPUT TUPLE PATH: "; print_string (Namespace_names.prefixed_string_of_rqname cvname); print_string "\n"
	end;*)
	  (var_paths, paths)
  
let get_input_tuple_paths analysis_ctxt =
  match analysis_ctxt with
    | (_, it_paths) ->
	(*begin
	  match it_paths with
	    | [] -> print_string "Retrieving empty input tuple paths."
	    | (cvname, path) :: tl -> print_string "GET INPUT TUPLE PATH: "; print_string (Namespace_names.prefixed_string_of_rqname cvname); print_string "\n"
	end;*)
	  it_paths
