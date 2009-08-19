(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_name_index.ml,v 1.15 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_name_index
   Description:
     This module contains support for building name indexes at
     document loading time.
*)

(* ********************************************************************************** *)
(*  The statement  "declare nameindex qname;" in the prolog will create an index      *)
(*  over all elements with the name qname. This name index is structured as           *)
(*  follows:                                                                          *)
(*   - A main array that contains all elements in preorder. The index of an           *)
(*     element in this array is called a record-id (RID)                              *)
(*   - Next, two btrees are defined mapping the preorder, respectively the            *)
(*     postorder to these RIDs                                                        *)
(*  In this way, you can navigate to the first followerof a node (i.e., the first     *)
(*  node with a greater post order) in log(|indexsize|). We use these indices for     *)
(*  both twig joins and staircase joins, but they can probably be used in other       *)
(*  places too.                                                                       *)
(* ********************************************************************************** *)


open Error
open Dm_atomic_btree
open Dm_atomic_btree_util
open Physical_item
open Physical_sequence

(* Name indices *)

type name_index = Physical_value.dom_tuple full_btree_index
type name_index_handler = Physical_value.dom_tuple full_btree_handler

(* Collection of name indices *)

type name_indices = (Namespace_symbols.relem_symbol * name_index) list
type name_indices_handler = (Namespace_symbols.relem_symbol * name_index_handler) list

type name_indices_hash =
    (Namespace_symbols.relem_symbol, Physical_value.dom_tuple full_btree_handler) Hashtbl.t

let init_name_indices_hash () =
  Hashtbl.create 7

let build_name_indices_hash name_indices =
  let indices = Hashtbl.create (List.length name_indices) in
  List.iter (fun (x,y) -> Hashtbl.add indices x y) name_indices;
  indices

let get_name_index name_indices ename =
  if Hashtbl.mem name_indices ename
  then
    Hashtbl.find name_indices ename
  else
    raise (Query (Wrong_Args ("No existing name index for element " ^ (Namespace_symbols.relem_string ename))))

let get_opt_name_index name_indices ename =
  if Hashtbl.mem name_indices ename
  then
    Some (Hashtbl.find name_indices ename)
  else
    None

let get_all_name_indices name_indices =
  Gmisc.all_of_hashtable name_indices

(* Create a name index *)
(* Note:
   - There are 2 b-trees for document 
   - WE ASSUME THAT ALL NODES ARE FROM THE SAME DOCUMENT FOR NOW.
     Eventually, we will want to lookup the index based on
     qname+docid, but that requires more support for docid
     identification during code selection.
   - Note: this is actually safe now because the BTree loader only
     works if you try and load a single time!
  - Jerome and Philippe
*)

let create_name_index () = create_full_btree_handler 2

let add_item_to_name_index name_index (new_elem_node : Dm.node) order =
   let (_, pre, post) = order in
   (* let atomic_doc = new Datatypes_atomic.atomicInteger
   (Big_int.big_int_of_int docid) in *)
   let atomic_pre =
     new Dm_atomic.atomicInteger (Big_int.big_int_of_int pre)
   in
   let atomic_post =
     new Dm_atomic.atomicInteger (Big_int.big_int_of_int  post)
   in
   let rid = full_btree_index_add_to_main_array name_index [| (Physical_sequence.sequence_of_singleton (Physical_value.Item_Node new_elem_node)) |] in
   begin
     (* single_add_to_btree name_index 0 (atomic_doc,  rid); *)
     single_add_to_btree name_index 0 (atomic_pre,  rid);
     single_add_to_btree name_index 1 (atomic_post, rid)
   end

let add_pre_order_item_to_name_index (name_index:name_index_handler) pre =
  let atomic_pre = new Dm_atomic.atomicInteger (Big_int.big_int_of_int pre) in
  let rid = full_btree_index_add_to_main_array name_index [||] in
  let _ = single_add_to_btree name_index 0 (atomic_pre,  rid) in
  rid

let add_post_order_item_to_name_index (name_index:name_index_handler) rid new_elem_node post =
  let tup = [| Physical_sequence.sequence_of_singleton (Physical_value.Item_Node new_elem_node) |] in
  let atomic_post = new Dm_atomic.atomicInteger (Big_int.big_int_of_int  post) in
  let _ = full_btree_index_update_in_main_array name_index rid tup in
  let _ = single_add_to_btree name_index 1 (atomic_post,  rid) in
  rid

(* Empty name_indices *)
let no_name_indices = []

let add_new_name_index name_indices x y =
  Gmisc.cond_add name_indices x y

let merge_name_indices name_indices1 name_indices2 =
  Gmisc.merge_hashtable name_indices1 name_indices2

(* ********************************************* *)
(* Create a pre-order walking cursor starting at *)
(* the node with pre-order index > 1             *)
(* ********************************************* *)
(* DEPRECATED
let pre_cursor_of_name_index (ni: name_index): (Physical_value.item Physical_value.sequence) Cursor.cursor =
  try
    let pre_btree = (snd ni).(0) in
    let rid_table = (fst ni) in
(*	let _ = Printf.printf "1st array access %i / %i \n%!" 
	    (min_position pre_btree)(max_position pre_btree) in
*)
    (* record state of cursor *)
    let posref = ref (-1) in
    let keyref = ref None in
    
    (* the cursor function *)
    let pre_curs_fun () =
      if !posref = (max_position pre_btree) then
	None
      else if !posref < 0 then
	let _ = posref := min_position pre_btree in
	let (key,rid) = pre_btree.(!posref) in
	let _ = keyref := Some key in
	Some (rid_table.(rid).(0))
      else
	let k = 
	  match !keyref with 
	  | Some r -> r 
	  | None -> raise (Query (Internal_Error 
				    "Illegal index cursor state (dm_name_index.pre_cursor_of_name_index)"))
	in
	let _ = posref := next_lowest_value pre_btree k !posref in
	let (key, rid) = pre_btree.(!posref) in
	    let _ = keyref := Some key in
	    Some ((rid_table.(rid)).(0))
    in
    Cursor.cursor_of_function pre_curs_fun
  with Invalid_argument _ ->
    raise (Query (Internal_Error 
		    "Array index out of bounds (dm_name_index.pre_cursor_of_name_index)"))
*)

(* ********************************************* *)
(* Create a pre-order walking cursor starting at *)
(* the first node with pre-order index > pre     *)
(* ********************************************* *)
let pre_cursor_of_name_index_at_pos (ni: name_index) (pre:int) 
      : (Physical_value.item Physical_value.sequence) Cursor.cursor =
  try
    let pre_btree = (snd ni).(0) in
    let rid_table = (fst ni) in
    let cur_rid = ref (-1) in
    
    (**************************************************************************)
    (* WARNING: this assumes the main table contains items in pre-order -- Ph *)
    (**************************************************************************)
    (* the cursor function *)
    let pre_curs_fun () =
      if !cur_rid >= (Array.length pre_btree) -1 then None
      else if !cur_rid < 0 then
	begin
	  let atomic_pre = new Dm_atomic.atomicInteger (Big_int.big_int_of_int pre) in
	  begin
	    if Debug.default_debug()
	    then Debug.sprintf_default_debug "Finding low point for %i\n" pre
	  end;
	  let posref = find_low_point pre_btree atomic_pre in
	  
	  if posref < (Array.length pre_btree) && posref >= 0 then
	    let _ = cur_rid := snd pre_btree.(posref) in
	    Some (rid_table.(!cur_rid).(0))
	  else None
	end
      else
	let _ = incr(cur_rid) in
	let res = Some ((rid_table.(!cur_rid)).(0)) in
	res
    in
    Cursor.cursor_of_function pre_curs_fun
  with Invalid_argument _ ->
    raise (Query (Internal_Error 
		    "Array index out of bounds (dm_name_index.pre_cursor_of_name_index)"))


(* ********************************************* *)
(* Create a pre-order walking cursor starting at *)
(* the first node with post-order index > post   *)
(* ********************************************* *)
let pre_cursor_of_name_index_from_post (ni: name_index) (post:int) 
      : (Physical_value.item Physical_value.sequence) Cursor.cursor =
  try
    let post_btree = (snd ni).(1) in
    let rid_table = (fst ni) in
    let cur_rid = ref (-1) in
    
    (**************************************************************************)
    (* WARNING: this assumes the main table contains items in pre-order -- Ph *)
    (**************************************************************************)
    (* the cursor function *)
    let pre_curs_fun () =
      if !cur_rid >= (Array.length post_btree) -1 then None
      else if !cur_rid < 0 then
	begin
	  let atomic_post =
	    new Dm_atomic.atomicInteger (Big_int.big_int_of_int post)
	  in
	  begin
	    if Debug.default_debug()
	    then Debug.sprintf_default_debug "Finding low point for %i\n" post
	  end;
	  let posref = find_low_point post_btree atomic_post in

	  if posref < (Array.length post_btree) && posref >= 0 then
	    let _ = cur_rid := snd post_btree.(posref) in
	    Some (rid_table.(!cur_rid).(0))
	  else None
	end
      else
	let _ = incr(cur_rid) in
	let res = Some ((rid_table.(!cur_rid)).(0)) in
	res
    in
    Cursor.cursor_of_function pre_curs_fun
  with Invalid_argument _ ->
    raise (Query (Internal_Error 
		    "Array index out of bounds (dm_name_index.pre_cursor_of_name_index)"))



(* ********************************************* *)
(* Create a pre-order walking cursor starting at *)
(* the first node with pre-order index > pre     *)
(* and stopping at the first node with a         *)
(* pre-order index > post                        *)
(* ********************************************* *)
let pre_cursor_of_name_index_from_window (ni: name_index) (pre:int) (post:int)
      : (Physical_value.item Physical_value.sequence) Cursor.cursor =
  try
    let pre_btree = (snd ni).(0) in
    let rid_table = (fst ni) in

    (**************************************************************************)
    (* WARNING: this assumes the main table contains items in pre-order -- Ph *)
    (**************************************************************************)

    (* input  = pre, post *)
    let atomic_pre  = new Dm_atomic.atomicInteger (Big_int.big_int_of_int pre) in
    let pre_pos = find_low_point pre_btree atomic_pre in
    begin
      if Debug.default_debug()
      then
	begin
	  let msg = Format.sprintf "Windowed cursor ]%i;%i[:Lowpoint for %i = %i/%i\n" pre post pre pre_pos (max_position pre_btree) in
	  Debug.print_default_debug msg
	end
    end;
    if pre_pos <= (max_position pre_btree) 
    then 
      let cur_rid = ref (snd pre_btree.(pre_pos)) in
      let rec f () = 
	begin
	  if Debug.default_debug()
	  then 
	    Debug.sprintf_default_debug "Call to cursor-scanner, current rid is %i\n" !cur_rid
	end;
	if !cur_rid > (max_position pre_btree) then
	  None
	else
	  let cur_item = ref (rid_table.(!cur_rid)).(0) in
	  let n = getNode (Physical_util.get_item (cursor_of_sequence ( !cur_item ))) in
	  match n#docorder() with
	  | (_,Nodeid.PrePostInt(_,pre',post')) ->
	      if post' < post then
		begin
		  begin
		    if Debug.default_debug()
		    then 
		      begin
			let msg = Format.sprintf "Outputting node (%i-%i) at rid %i\n" pre' post' !cur_rid
			in
			Debug.print_default_debug msg
		      end
		  end;
		  let _ = incr(cur_rid) in 
		  Some !cur_item
		end
	      else if post' = post
	      then
		begin
		  begin
		    if Debug.default_debug()
		    then 
		      let msg = Format.sprintf "Skipping self node (%i-%i) at rid %i\n" pre' post' !cur_rid
		      in
		      Debug.print_default_debug msg
		  end;
		  let _ = incr(cur_rid) in f ()
		end
	      else
		begin
		  begin
		    if Debug.default_debug()
		    then 
		      let msg = Format.sprintf "Reached end of window ]%i;%i[ with rid %i\n" pre post !cur_rid
		      in
		      Debug.print_default_debug msg
		  end;
		  None
		end
	  | _ -> None
      in Cursor.cursor_of_function f
		else Cursor.cursor_of_function (fun () -> None)
  with Invalid_argument _ ->
    raise (Query 
	     (Internal_Error 
		"Array index out of bounds (pre_cursor_of_name_index_from_window)"))
      
