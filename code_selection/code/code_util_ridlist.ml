(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util_ridlist.ml,v 1.3 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_util_ridlist
   Description:
     This module contains operations on record-id lists (ridlists).
*)

type rid = int

(*************)
(* Rid lists *)
(*************)
(* probably should change to something more performanct (bitmaps maybe) *)
(* WE ASSUME THE LISTS ARE IN SORTED ORDER AND DUPLICATE FREE *)

type rid_list = 
  | Full_RidList
  | Empty_RidList
  | Regular_RidList of rid list

let is_empty_rid_list rl =
  match rl with 
    | Empty_RidList -> true
    | _ -> false

let is_full_rid_list rl =
  match rl with
    | Full_RidList -> true
    | _ -> true

(* This function *REVERSES* the order, which is then reversed above
   again The reason is that :: is faster than the constructor
   @ [...] idiom.  *)
let remove_sorted_duplicates_and_reverse dup_list =
  if (List.length dup_list) = 0 then
    begin
      []
    end
  else (* List.hd safe *)
    begin
      let head = List.hd dup_list in
      let tail = List.tl dup_list in 
      let helper_fn (cur_list,last_seen) cur =
	if cur = last_seen then
	  (cur_list, last_seen)
	else
	  (cur :: cur_list, cur)
      in  
	   (fst
	      (List.fold_left helper_fn
		 ([head], head) tail))
    end

let intersect_rid_list r1 r2 =
  match (r1, r2) with
    | (Full_RidList, x)    | (x, Full_RidList ) -> 
	x
    | (Empty_RidList, _)   | (_, Empty_RidList) -> Empty_RidList
    | (Regular_RidList r1, Regular_RidList r2 ) ->

	(* This function *REVERSES* the order, which is then reversed above again 
	   The reason is that :: is faster than the constructor @ [...] idiom.
	*)
	let rec merge_intersect_lists intersected_list (full_left: int list) (full_right: int list) =
	  match (full_left, full_right) with
	      (head_left :: tail_left), (head_right :: tail_right) ->
		begin
		  if (head_left = head_right) then 
		    merge_intersect_lists (head_left :: intersected_list) tail_left tail_right
		  else if (head_left < head_right) then
		    (* throw away only the left side *)
		    merge_intersect_lists intersected_list tail_left full_right
		  else (* head_left > head_right *)
		    merge_intersect_lists intersected_list full_left tail_right
		end
	    | ([], _) | (_, []) -> (* inteserct with empty -> empty *)
		intersected_list
	in
	  (* This list contains duplicates which we must remove *)
	let dup_free = remove_sorted_duplicates_and_reverse 
			 (merge_intersect_lists [] r1 r2)  in
	  if dup_free = [] then Empty_RidList
	  else Regular_RidList dup_free
	      

let union_rid_list r1 r2 =
  match (r1, r2) with
    | (Full_RidList, _)    | (_, Full_RidList ) -> Full_RidList
    | (Empty_RidList, x)   | (x, Empty_RidList) -> x
    | (Regular_RidList r1, Regular_RidList r2 ) ->
	(* This function *REVERSES* the order, which is then reversed above again 
	   The reason is that :: is faster than the constructor @ [...] idiom.
	*)
	let rec union_lists unioned_list full_left full_right =
	  match (full_left, full_right) with
	      (head_left :: tail_left), (head_right :: tail_right) ->
		begin
		  if (head_left = head_right) then 
		    union_lists unioned_list full_left tail_right
		  else if (head_left < head_right) then
		    union_lists (head_left :: unioned_list) tail_left full_right
		  else union_lists (head_right :: unioned_list ) full_left tail_right
		end
	    | ([], x) | (x, []) -> (* inteserct with empty -> empty *)
		unioned_list @ x
	in
	let dup_free = remove_sorted_duplicates_and_reverse 
			 (union_lists [] r1 r2)  in
	  if dup_free = [] then Empty_RidList
	  else Regular_RidList dup_free

let rid_compare p1 p2 = p2 - p1

let cursor_of_rid rid_list len =
  match rid_list with
  | Empty_RidList -> Cursor.cursor_empty ()
  | Full_RidList ->
      begin
	let offset = ref 0 in
	let next () =
	  if !offset < len then
	    let off = !offset in incr offset; Some off
	  else
	    None
	in
	Cursor.cursor_of_function next
      end
  | Regular_RidList list_of_indexes ->
      Cursor.cursor_of_list list_of_indexes

let unwrap_rid_list_cursor materialized_array array_restore_fun rid_list =
  let max = Array.length materialized_array in
  let cid = cursor_of_rid rid_list max in
  let next () =
    try
      begin
	let offset = Cursor.cursor_next cid in
	(* Restore, increment and return the tuple *)
	array_restore_fun materialized_array.(offset);
	Cs_util.empty_tuple_opt
      end
    with
    | Stream.Failure ->
	None
  in
  Cursor.cursor_of_function next

