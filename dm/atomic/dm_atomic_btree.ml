(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm_atomic_btree.ml,v 1.5 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Dm_atomic_btree
   Description:
     This module contains support for BTrees over XML items.
*)

(*************************************************************)
(*   This section of the code deals with searching the table *)
(*   with respsect to different operators                    *)
(*************************************************************)

open Dm_atomic

type key       = atomicValue
type position  = int
type 'a pair   = (key * 'a)
type 'a btree  = 'a pair array



(*******************************************************)
(* Comparison between points and elements of the table *)
(*******************************************************)
               
(* These use the total order over types for comparison operations *)
let value_equal b (x,_)   = (Dm_atomic_util.total_order_compare b x) = 0

let value_less b (x,_)    = (Dm_atomic_util.total_order_compare b x) < 0
let value_leq  b (x,_)    = (Dm_atomic_util.total_order_compare b x) <= 0
 
let value_greater b (x,_) = (Dm_atomic_util.total_order_compare b x) > 0 
let value_geq  b (x,_)    = (Dm_atomic_util.total_order_compare b x) >= 0

(*** Since we define a total order for types, we need to find the
     smallest possible value for this type.  Eventually these tables
     should be segregated by type *)
let min_typed_value tbl point =
  let typed = point#getAtomicValueKind () in
  let rec find_point_helper lo hi =
    let mid = (lo + hi) / 2 in
    if (lo=hi) then lo
    else
      if (Datatypes_util.compare_types typed ((fst (tbl.(mid)))#getAtomicValueKind ()) > 0)
      then find_point_helper (mid+1) hi
      else find_point_helper lo mid
  in
  find_point_helper 0 (Array.length tbl)

let max_typed_value tbl point =
  let typed = point#getAtomicValueKind () in
  let rec find_point_helper (lo:int) (hi:int) =
    let mid = (lo + hi) / 2 in
    if (lo=hi) then lo
    else if (lo+1) = hi then 
      begin
	if (Datatypes_util.compare_types typed ((fst tbl.(lo))#getAtomicValueKind ()) < 0)
	then (lo - 1)
	else
	  if (Datatypes_util.compare_types typed ((fst tbl.(hi))#getAtomicValueKind ()) > 0)
	  then (hi + 1)
	      (* We know here that 
		 tbl.(lo) <= typed <= tbl.(hi)
	       
		 if typed < tbl.(hi) then lo
		 else (* tbl.(hi) = typed *) hi
	       *)
	else
	    if (Datatypes_util.compare_types typed ((fst tbl.(hi))#getAtomicValueKind ()) < 0)
	    then lo	      
	    else hi
      end
    else
      if (Datatypes_util.compare_types typed ((fst tbl.(mid))#getAtomicValueKind ()) < 0)
      then find_point_helper lo mid
      else find_point_helper mid hi
  in
  let max_index = ((Array.length tbl) - 1) in
  find_point_helper 0 max_index 


(*************************************************)
(* NOTE: Assumes the input is in ascending order.*)
(*************************************************)

(*

  find_low_point: given a value b return the index of the point equal
  in value to b that comes first in sequence order.

  If no point is equal to b. Find the index following where it should
  occur.

  If point is smaller than all indexes in the table then 0 is returned

  If point is larger than all indexes in the table then (Array.length
  tbl) is returned

  [0;2] find_low_point(1)  = 1 

*)

let find_low_point tbl point =
  let rec find_point_helper lo hi =
    let mid = (lo + hi) / 2 in
    if (lo=hi) then lo
    else if value_greater point tbl.(mid) then
      find_point_helper (mid+1) hi
    else
      find_point_helper lo mid
  in  
  find_point_helper 0 (Array.length tbl)

(*
   find_high_point: given a value b
   return the index of the point equal in value to b 
   that comes last in sequence order
   
   If no point is equal to b. Find the index before where it should occur.

   If point is smaller than all indexes in the table then
      -1 will be returned

   If point is larger than all indexes then 
      (Array.length - 1) will be returned

   find_high_point(1) = 0
*)
let find_high_point tbl point =
  let rec find_point_helper (lo:int) (hi:int) =
    let mid = (lo + hi) / 2 in
      if (lo=hi) then lo
      else if (lo+1) = hi then 
	begin
	  if value_less point tbl.(lo) then
	    (lo - 1)
	  else if value_geq point tbl.(hi) then
	    hi
	  else lo	    
	end
      else if value_less point tbl.(mid) then
	find_point_helper lo mid
      else
	find_point_helper mid hi
  in
  let max_index = ((Array.length tbl) - 1) in
  find_point_helper 0 max_index 

(* previous_highest_value tbl b index 
   return max(i) 
   such that i <= index
   and tbl.(i) < b *)
let previous_highest_value tbl b index = 
  let index = 
    if (Array.length tbl) >= index 
    then (Array.length tbl) - 1
    else index 
  in
  let cur = ref index in      
  (*      while tbl.(!cur) >= b do  *)
  while !cur >= 0 &&
    value_leq b tbl.(!cur) do
    decr cur
  done;
  !cur

(* next_lowest_value b index 
   return min(i) 
   such that i >= index
   and tbl.(i) > b *)

let next_lowest_value tbl b index =
  let index  = if index < 0 then 0 else index in 
  let cur    = ref index in
  let length = Array.length tbl in
  (*     while tbl.(!cur) <= b do *)      
  while !cur < length &&
    value_geq b tbl.(!cur) do
    incr cur
  done;
  !cur

let max_position tbl = Array.length tbl - 1
let min_position tbl = 0

let position_before p1 p2  = p1 < p2
let position_after  p1 p2  = p1 > p2

let max tbl   = Array.length tbl - 1
let first tbl = fst (tbl.(0))
let last tbl  = fst (tbl.(max tbl))

let sub tbl lo hi = Array.sub tbl lo (hi-lo+1)
let to_list tbl = Array.to_list (Array.map snd tbl)

type 'a bulk_loader =
    { mutable table          : 'a option btree;
      mutable current_size   : int;
      compare                : 'a -> 'a -> int;
      n_items                : int ref }

let sort_atomic_value_compare compare (d,i) (d',i') =
  let cmp = Dm_atomic_util.total_order_compare d d' in
  if cmp = 0
  then compare i i'
  else cmp

let initial_size =  1024
let dummy_value  =  (new atomicString("INVALID"), None)

let init compare =
  { table        = Array.make initial_size dummy_value;
    current_size = initial_size;
    compare      = compare;
    n_items      = ref 0 }

let finalize_sorted bl =
  (* We need to trim this table for the return now, to keep out invalid values *)
  let n_items   = !(bl.n_items) in
  let ret_array = Array.sub bl.table 0 n_items in
  let ret_array = Array.map (function (x,None) -> raise (Failure "INVALID BTREE") | (x,Some y) -> (x,y)) ret_array in
  (if Debug.default_debug()
   then Debug.sprintf_default_debug "Ret Array %i \n%!" (Array.length ret_array);
   ret_array)

let finalize bl =
  let ret_array = finalize_sorted bl in
  Array.sort (sort_atomic_value_compare bl.compare) ret_array;
  ret_array

let sort_add bl (av, index) =
  let n_items_ref = bl.n_items in
  (******************************)
  (* This grows the input table *)
  (******************************)
  if ((!n_items_ref + 1) >= bl.current_size) then
    begin 
      let new_size  = bl.current_size * 2 in (* Multiplicative instead? *)
      let old_table = bl.table in
      bl.table <- Array.make new_size bl.table.(0); (* There has to be a better way to do this *)
      bl.current_size <- new_size;
      (* Copy over the items *)
      Array.blit old_table 0 bl.table 0 !n_items_ref;
    end;
  (* Here we may assume the input table is of the right size *)          
  bl.table.(!n_items_ref) <- (av,index);
  incr n_items_ref

let single_add bl (x,y) =
  sort_add bl (x,Some y)

let bulk_add bl value_seq =
  List.iter (single_add bl) value_seq

let reset bl =
  bl.table <- Array.make initial_size dummy_value;
  bl.current_size <- initial_size;
  bl.n_items := 0

