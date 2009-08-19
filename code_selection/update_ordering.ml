(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: update_ordering.ml,v 1.6 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Update_ordering 
     Description: This module contains a structure to assist with
     ordering for the deterministic order semantic.
*)

(******************************************************)
(* Delta type (atomic update) as in the semantic note *)
(******************************************************)
type delta = 
  | Insert       of Dm.node * Dm.node option * Dm.node list
  | Delete       of Dm.node * Dm.node
  | ReplaceValue of Dm.node * Dm.text
  | Replace      of Dm.node * Dm.node * Dm.node list
  | Rename       of Dm.node * Datatypes.xs_QName

type update_holder = 
  | Concrete_update of delta
  | PlaceHolder of update_holder list ref

exception Update_Order_Error of string

let make_fresh_place_holder () = PlaceHolder (ref [])

(* Allocate a place holder *)
(* let internal_calls = ref 0 *)
let allocate_update_holder uh = 
  (* incr internal_calls;
     Format.printf "Allocating Place Holder [%d calls]@." !internal_calls; *)
  match uh with
    | Concrete_update _ -> 
	raise (Update_Order_Error "Attempting to allocate space underneath a concrete update?")
    | PlaceHolder l ->
	let new_place_holder = make_fresh_place_holder () in 
	l := !l @ [new_place_holder]; 
	  new_place_holder

let add_concrete_update uh delta = 
  match uh with
    | Concrete_update _ -> 
	raise (Update_Order_Error "Attempting to allocate space underneath a concrete update?")
    | PlaceHolder l ->
	l := !l @ [Concrete_update delta]

let rec collect_all_updates uh = 
  match uh with
    | Concrete_update c -> [c]
    | PlaceHolder p ->
	List.concat (List.map collect_all_updates !p)
