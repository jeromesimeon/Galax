(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm_atomic_btree_util.ml,v 1.4 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Dm_atomic_btree_util
   Description:
     This module contains utilities used for BTrees over XML items.
*)

open Error

(***************)
(* BTree types *)
(***************)

(* BTree Handlers *)

type 'a btree_handler =
  | Loading of 'a Dm_atomic_btree.bulk_loader
  | Loaded of 'a Dm_atomic_btree.btree

type int_btree_handler = int btree_handler

type 'a full_btree_handler =
    { mutable n_items      : int ref;
      mutable current_size : int;
      mutable main_array   : 'a array;
      btree_handlers       : (int_btree_handler ref) array }

(* Final BTrees *)

type int_btree           = int Dm_atomic_btree.btree
type 'a full_btree_index = 'a array * int_btree array


(********************************)
(* Operations on Btree handlers *)
(********************************)

(* Integer comparison *)
let compare i i' = i - i'

(* Create a new btree handler *)
let create_btree_handler index =
  ref (Loading (Dm_atomic_btree.init compare))

let create_full_btree_handler btree_count =
    { n_items        = (ref 0);
      current_size   = 0;
      main_array     = [||];
      btree_handlers = Array.init btree_count create_btree_handler }

(* Get the main array from a btree handler *)
let get_main_array fbth =
  fbth.main_array

(* Set the main array in a btree handler *)
let set_main_array fbth newarray =
  fbth.main_array <- newarray

(* Load a btree with a given index *)
let get_loading bth =
  match !bth with
  | Loading x -> x
  | Loaded _ -> raise (Query (Prototype "CANNOT LOAD BTREE ANYMORE!"))

let get_loading_on_index fbth index =
  get_loading fbth.btree_handlers.(index)

let single_add_to_btree fbth index values =
  Dm_atomic_btree.single_add (get_loading_on_index fbth index) values

let bulk_add_to_btree fbth index values =
  Dm_atomic_btree.bulk_add (get_loading_on_index fbth index) values


(* Access to btrees *)
let finalize_btree_handler bth =
  match !bth with
  | Loading bl ->
      begin
	let x = Dm_atomic_btree.finalize bl in
	bth := Loaded x;
	x
      end
  | Loaded x -> x

let finalize_sorted_btree_handler bth =
  match !bth with
  | Loading bl ->
      begin
	let x = Dm_atomic_btree.finalize_sorted bl in
	bth := Loaded x;
	x
      end
  | Loaded x -> x


let get_loaded_btree bth =
  match !bth with
  | Loading _ -> finalize_btree_handler bth
  | Loaded x -> x

let get_loaded_btree_sorted bth =
  match !bth with
  | Loading _ -> finalize_sorted_btree_handler bth
  | Loaded x -> x

(* Finalize a given loaded btree on index *)
let finalize_btree_on_index fbth index =
  ignore(get_loaded_btree (fbth.btree_handlers.(index)))

(* Get a given loaded btree on index *)
let get_loaded_btree_on_index fbth index =
  get_loaded_btree (fbth.btree_handlers.(index))

(* Get the final index *)
let full_btree_index_of_btree_handler fbth =
  let arr = fbth.main_array in
  let btrees = Array.map get_loaded_btree fbth.btree_handlers in
  (arr,btrees)

(* Get the final index - but assume it is already sorted - name_index support *)
let full_btree_index_of_sorted_btree_handler fbth =
  let arr = fbth.main_array in
  let btrees = Array.map get_loaded_btree_sorted fbth.btree_handlers in
  (arr,btrees)


let full_btree_index_add_to_main_array fbth item =
  let n_items_ref = fbth.n_items in
  let cur_index = !n_items_ref in
  if cur_index = 0 then
    begin
      fbth.main_array <- [| item |];
      fbth.current_size <- 1;
    end
  else if ((!n_items_ref + 1) >= fbth.current_size) then
    begin 
      let new_size  = fbth.current_size * 2 in (* Multiplicative instead? *)
      let old_array = fbth.main_array in
      fbth.main_array <- Array.create new_size fbth.main_array.(0); (* There has to be a better way to do this *)
      fbth.current_size <- new_size;
      (* Copy over the items *)
      Array.blit old_array 0 fbth.main_array 0 !n_items_ref;
    end;
  (* Here we may assume the input table is of the right size *)          
  fbth.main_array.(!n_items_ref) <- (item);
  incr n_items_ref;
  cur_index

let full_btree_index_update_in_main_array fbth rid item =
  let arr = fbth.main_array in
  arr.(rid) <- item
