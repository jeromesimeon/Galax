(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optim_vars.ml,v 1.5 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Optim_vars
   Description:
     This module implements operations on variable for optimized
     projection.
*)

open Error

open Xquery_common_ast


(* Type representing a set of optimization variables *)

type optim_vars = 
  | AllVars
  | VarList of cvname list


(* The set of all optimization variables *)

let all_optimvars = AllVars


(* The empty set *)

let empty_optimvars = VarList []


(* Creates a singleton set with one optimization variable inside *)

let single_optimvars vname = VarList [vname]


(* Is a given variable in the given set of optimization variables ? *)

let rec member elmt lst =
 match lst with
 | AllVars -> true
 | VarList list ->
     match list with 
     |  [] -> false
     | h::t -> h = elmt || (member elmt (VarList t));;


(* The union of two sets of optimization variables *)

let rec union lst1 lst2 =
 match lst1 with
 | AllVars -> AllVars
 | VarList list ->
     match list with 
     | [] -> lst2
     | h::t -> 
	 match lst2 with 
	 | AllVars -> AllVars
	 | VarList list2 ->
	     match list2 with 
	     | [] -> lst1
	     | a::b ->
		 if (member h lst2) then (union (VarList t) lst2)
		 else 
		   let varlist = (union (VarList t) lst2) in
		   match varlist with 
		   | VarList uniontail ->
		       (VarList (h::uniontail))
		   | _ -> raise (Query (Prototype "Should not have AllVars here"));;


(* The intersection between two sets of optimization variables *)

let rec intersection lst1 lst2 =
  match lst1 with
 | AllVars -> lst2
 | VarList list ->
     match list with 
     | [] -> (VarList [])
     | h::t ->
	 match lst2 with
	 | AllVars -> lst1
	 | VarList list2 ->
	     match list2 with 
	     | [] -> (VarList [])
	     | _ -> 
		 if (member h lst2) then 
		   let varlist = (intersection (VarList t) lst2) in 
		   match varlist with 
		   | VarList intertail ->
		       (VarList (h::intertail))
		   | _ -> raise (Query (Prototype "Should not have AllVars here"))
		 else (intersection (VarList t) lst2);;


(* Removes one variable from a set of optimization variables *)

let rec remove elmt lst =
 match lst with
 | AllVars -> AllVars
 | VarList list ->
     match list with 
     | [] ->(VarList [])  
     | h::t ->
	 if h = elmt
	 then (VarList t)
	 else 
	   let varlist = remove elmt (VarList t) in
	   match varlist with 
	   | VarList tail -> (VarList (h::tail))
	   | _ -> raise (Query (Prototype "Should not have AllVars here"));;

let rec remove_list elmt_lst lst =
	match elmt_lst with 
	| [] -> lst
	| h::t ->
	if (member h lst) 
		then
		let new_lst = remove h lst in
		remove_list t new_lst
		else
		remove_list t lst
