(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_renumber.ml,v 1.6 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: shredded_renumber 
   Description: 
     This module implements renumbering of preorders such as those
     that result from insertions. This module is responsible for
     updating a mapping from preorders -> nodeids correctly and for
     keeping those records in the logical record up to date.

     This is currently the only place that we can get preorder in
     sorted order. Perhaps a better title for this module is
     "shredded_preorder_manager". Because implementations for
     descendant (and deletes).
 *)
(* TODO
   
 2. Add delete and replace

*)

(**************************************************)
(* We need to be able to update the logical store *)
(**************************************************)
module type Renumber_Callback = sig
  type preorder
  type nodeid
  type handle    
  (* val update_preorder : handle -> nodeid -> preorder option -> preorder -> unit *)
  val update_preorder : handle -> nodeid -> preorder -> preorder -> unit 
end


module Cell_As_Int64 = struct
  open Nodeid  
  type t = large_preorder
      
  (*****************)
  (* Magic Numbers *)
  (*****************)
  let zero = {gId=Int64.of_int 0;mId=Int64.of_int 0}
  let invalid_preorder = {gId=(Int64.of_int (-1));mId=(Int64.of_int (-1))}
  let is_invalid_preorder {gId=x;mId=_} = (Int64.compare x Int64.zero) < 0      

      
  (**********************************)
  (* This is the preorder Generator *)
  (**********************************)
  module Generator = struct
    type handle = (Int64.t * Int64.t) ref
    let incr_amount = Int64.of_int 1

    let next c_ref  = 
      let (h,l) = !c_ref in 
      let (h,l) as next_val = 
	if l > Int64.sub Int64.max_int incr_amount then
	  (Int64.succ h, Int64.zero)
	else
	  (h, Int64.add l incr_amount)
      in
	c_ref := next_val;
	{gId=h;mId=l}

    let create () = ref (Int64.zero, Int64.zero)
  end


  let of_int i = {gId=Int64.zero;mId=(Int64.of_int i)}
  let print_int64_pair ff {gId=g;mId=m} =
    Format.fprintf ff "{%s%s}"
      (Int64.to_string g)
      (Int64.to_string m)

  exception Distance_Is_Negative
			
  let split_into_strings {gId=g;mId=m} = 
    ((Int64.to_string g), (Int64.to_string m))
    
  let divide {gId=g;mId=m} c =
    let as_i64  = Int64.of_int c in 
    let big     = Int64.div g as_i64 in 
    let a       = Int64.rem g as_i64 in 
    let small_1 = Int64.mul a (Int64.div Int64.max_int as_i64) in
    let r       = Int64.rem Int64.max_int as_i64 in 
    let small_0 = Int64.div m as_i64 in 
      (big, (Int64.add small_1  (Int64.add small_0  (Int64.div (Int64.mul r a) as_i64))))

  let calc_gap ({gId=a0;mId=a1}) ({gId=b0;mId=b1}) count = 
    let dist = 
      if a0 > b0 then
	if a1 > b1 then
	  {gId=(Int64.sub a0 b0);mId=(Int64.sub a1 b1)}
	else
	  {gId=(Int64.sub (Int64.sub a0 b0) (Int64.of_int 1));mId=(Int64.sub a1 b1)}
      else if a0 = b0 then
	if a1 > b1 then
	  {gId = Int64.zero; mId=(Int64.sub a1 b1)}
	else
	  begin
	    (* Format.printf "--> %a to %a@."
	      print_int64_pair a
	      print_int64_pair b; *)
	    raise Distance_Is_Negative
	  end
      else
 	begin
	  (* Format.printf "--> %a to %a@."
	    print_int64_pair a
	    print_int64_pair b; *)
	  raise Distance_Is_Negative
	end
    in
      divide dist count

  let add  ({gId=a0;mId=a1}) ({gId=b0;mId=b1}) =    
    let g, m = 
      let low_order = Int64.add a1 b1 in 
	if low_order < Int64.zero then
	  begin
	    (* Carry Here *)
	    let m_ret = Int64.add Int64.max_int low_order in 
	    let g_ret = Int64.succ (Int64.add a0 b0) in
	      g_ret, m_ret
	  end
	else (* Not carry *)
	  begin
	    let g_ret = Int64.add a0 b0 in 
	      g_ret, low_order
	  end
    in
      {gId=g;mId=m}

  let mk_cell (a0,a1) = {gId=a0;mId=a1}
  let in_same_cell ({gId=a0;mId=a1}) ({gId=b0;mId=b1}) current_level =
    let in_cell =
      if current_level < 64 then
	begin
	  let a_0 = Int64.shift_right_logical a1 current_level in 
	  let b_0 = Int64.shift_right_logical b1 current_level in 
	    (a_0 = b_0) && (a0 = b0)
	end
      else
	begin
	  let a_0 = Int64.shift_right_logical a0 (current_level - 64) in 
	  let b_0 = Int64.shift_right_logical b0 (current_level - 64) in 
	    (a_0 = b_0)
	end
    in   
      (* Format.printf "--> Cell [%a,%a] %b @ %d@."
	print_int64_pair a
	print_int64_pair b in_cell current_level; *)
      in_cell
	  
end

(**************************************************)
(* This module handles the assignment of preorder *)
(**************************************************)
module Renumber_Module
  (Basetypes     : Shredded_store_sigs.Shredded_Basetypes)
  (Recno_Functor : Shredded_store_sigs.Shredded_Recno_Functor_Sig)
  (Callback      : Renumber_Callback with type nodeid = Basetypes.stored_nodeid with type preorder = Nodeid.large_preorder)
  (Btree_Functor : Shredded_store_sigs.Shredded_Btree_Functor_Sig) 
  = struct
    open Basetypes
    open Nodeid
    exception Renumber_Exception of string

      
    (**************************)
    (* The structures we need *)
    (**************************)
    module Preorder_To_Cell  = Btree_Functor (Preorder_Module) (Basetypes.Stored_Nodeid_Module)
    module Recno = Recno_Functor (Basetypes) (Basetypes.Stored_Preorder_Nodeid_Record_Module)

    (*****************)
    (* Magic Numbers *)
    (*****************)
    let root_nodeid      = 1 
	  
    let min_preorder     = {gId=(Int64.of_int 0);mId=(Int64.of_int 0)}
    let max_preorder     = {gId=(Int64.max_int);mId=(Int64.max_int)}
			 
    let _T = 1.5 (* The T parameter from Bender. *)
    let rhs = 2.0/._T
    let max_power = 130
    let generate_powers v =
      let a = Array.make (max_power + 1) 1.0 in
	for i = 1 to max_power do
	  a.(i) <- a.(i-1) *. v
	done; a

    let powers = generate_powers rhs
		   
    (**********************************************************)
    (* Want to know when nodes_in_area < (2/_T)^current_level *)
    (**********************************************************)

    let lower_then_threshold_density current_level nodes_in_area =
      let rhs_value = 
	if current_level <= max_power 
	then powers.(current_level)
	else raise (Renumber_Exception ("Threshold power too high: " ^ (string_of_int current_level)))
      in
	(* Format.printf "Threshold: %d %d@."
	  nodes_in_area  (int_of_float rhs_value); *)
	nodes_in_area < (int_of_float rhs_value)

    type cursor_desc = 
      | Reached_End
      | Not_Set
      | Value of (Preorder_Module.t * Basetypes.Stored_Nodeid_Module.t) 

    type handle = {
      recno_handle : Callback.handle;
      btree_handle : Preorder_To_Cell.btree_handle;

      right_cursor : Preorder_To_Cell.btree_cursor;
      mutable right_cursor_current : cursor_desc;
      right_temp   : Recno.recno_handle;
      right_index  : int ref;

      left_cursor  : Preorder_To_Cell.btree_cursor;
      mutable left_cursor_current  : cursor_desc;
      left_temp    : Recno.recno_handle;      
      left_index   : int ref
    }
	
    let renumber_open (directory,name) buffsize rec_handle =
      let renumber_name   = Shredded_file_util.create_renumber_db_name directory name in 
      let right_temp_name = Shredded_file_util.create_renumber_right_temp_db_name directory name in
      let left_temp_name  = Shredded_file_util.create_renumber_left_temp_db_name  directory name in

      (* No need for duplicates *)
      let handle          = Preorder_To_Cell.btree_open renumber_name buffsize false in 
      let right_cursor    = Preorder_To_Cell.btree_cursor_open handle in
      let right_temp      = Recno.recno_open right_temp_name buffsize in 

      let left_cursor     = Preorder_To_Cell.btree_cursor_open handle in
      let left_temp       = Recno.recno_open left_temp_name buffsize  in 

	(* DEBUG PRINT *)
	(* begin
	   let first  = Preorder_To_Cell.btree_cursor_get_first right_cursor in 
	    match first with 
	      | None -> Format.printf "No Preorders Stored@.";
	      | Some v -> 
		  let print_pair (p,n) =
		    Format.printf "\t[Debug Shredded Renumber] (%a,%d))@."  
		      Cell_As_Int64.print_int64_pair p n
		  in
		  let cursor = Preorder_To_Cell.btree_cursor_to_cursor right_cursor Preorder_To_Cell.Next  in 
		    print_pair v;
		    Cursor.cursor_iter print_pair cursor
	end; *)
	
	{ recno_handle = rec_handle; 
	  btree_handle = handle;

	  left_cursor = left_cursor;
	  left_cursor_current = Not_Set;
	  left_temp   = left_temp;
	  left_index  = ref 0;

	  right_cursor = right_cursor;
	  right_cursor_current = Not_Set;
	  right_temp   = right_temp;
	  right_index  = ref 0
	}

    let renumber_close handle = 
      Recno.recno_close_no_sync handle.right_temp;
      Recno.recno_close_no_sync handle.left_temp;
      handle.right_index := 0;
      handle.left_index  := 0;
      Preorder_To_Cell.btree_close handle.btree_handle

    let renumber_sync  handle = 
      Preorder_To_Cell.btree_sync handle.btree_handle
	      
    (* We do not actually close the cursors, just mark them *)
    let close_internal_handle handle = 
      handle.right_cursor_current <- Not_Set;
      handle.left_cursor_current <- Not_Set;
      handle

    (*********************************************************)
    (* We don't store invalid preorders (ex: during updates) *)
    (*********************************************************)
    let store_node handle snid preorder = 
      if Cell_As_Int64.is_invalid_preorder preorder 
      then ()
      else  
	Preorder_To_Cell.btree_put handle.btree_handle preorder snid
	  
	
      
    let unsafe_cursor_start cursor pre_order =
      match Preorder_To_Cell.btree_cursor_get_set cursor pre_order with
	| None -> 
	    (* Format.printf "Failing: %a@." Cell_As_Int64.print_int64_pair pre_order;  *)
	    raise (Renumber_Exception "Unsafe Get Failed in Renumbering!!")
	| Some v -> v

    (*************************)
    (* Tempstorage functions *)
    (*************************) 
    let reset_temp_index handle = 
      handle.left_index  := 1;
      handle.right_index := 1

    let store_on_right handle nodeid = 
      (* Store in the new list *)
      Recno.recno_put handle.right_temp !(handle.right_index) nodeid;
      incr handle.right_index
		
    let store_on_left handle nodeid = 
      (* Store in the new list *)
      Recno.recno_put handle.left_temp !(handle.left_index) nodeid;
      incr handle.left_index

    (**********************************)
    (* Handle scanning left and right *)
    (**********************************)
    (* The logic is: 

       o If they are in the same cell, we remove the entry from the index
         and place it on the appropriate temp list. 
	 
       o If they are not in the same cell, then our scan is done
         for this level.
    *)    
    let move_cursor_left handle =
      match Preorder_To_Cell.btree_cursor_get_prev handle.left_cursor with
	| None -> 
	    handle.left_cursor_current <- Reached_End			
	| Some (pre, cell) ->
	    handle.left_cursor_current <- Value (pre,cell)

    let move_cursor_right handle = 
      match Preorder_To_Cell.btree_cursor_get_next handle.right_cursor  with		      
	| None -> 
	    handle.right_cursor_current <- Reached_End
	| Some (pre, cell) ->
	    handle.right_cursor_current <- Value (pre,cell)

    let rec scan_left handle current_cell_id current_level nodes_in_area =      
      match handle.left_cursor_current with
	| Reached_End -> 
	    (* Format.printf "[Left] Reached End?@."; *)
	    (min_preorder, nodes_in_area) 
	| Not_Set -> raise (Renumber_Exception "Scan handle not set?")
	| Value (cell_id, nodeid) ->
	    if Cell_As_Int64.in_same_cell current_cell_id cell_id current_level 
	    then
	      begin 
		(* Format.printf "[Left] Moving to Temp: %d@." nodeid; *)
		store_on_left handle (cell_id,nodeid)
		;  
		(* Delete from the btree *)
		Preorder_To_Cell.btree_cursor_del handle.left_cursor;
		(* Format.printf "[Left] Delete Completed@."; *)
		let () = move_cursor_left handle in 
		(* Format.printf "[Left] Move Cursor Left Completed@."; *)
		  scan_left handle current_cell_id current_level (nodes_in_area + 1)
	      end
	    else (cell_id, nodes_in_area)

    let rec scan_right handle current_cell_id current_level nodes_in_area =      
      match handle.right_cursor_current with
	| Reached_End -> (max_preorder, nodes_in_area) 
	| Not_Set -> raise (Renumber_Exception "Scan handle not set?")
	| Value (cell_id, nodeid) ->
	    if Cell_As_Int64.in_same_cell current_cell_id cell_id current_level 
	    then 
	      begin 
		(* Format.printf "[Right] Moving to Temp: %d@." nodeid;   *)
		(* Store it *)
		store_on_right handle (cell_id,nodeid);
		(* Delete from the btree *)
		Preorder_To_Cell.btree_cursor_del handle.right_cursor;
		(* This looks wrong - it should advance after a delete automatically... - Chris *)
		let () = move_cursor_right handle in
		  scan_right handle current_cell_id current_level (nodes_in_area + 1)
	      end
	    else (cell_id, nodes_in_area)
	      
	      
    (***************************************************************************)
    (* Internal helper function to actually perform the renumbering            *)
    (* The idea is that the scans have stored in temporary files,              *)
    (* left_list and right_list the nodeids of those that are to be renumbered.*)
    (* We then calculate the gap we should renumber with and then perform this *)
    (* evenly spaced renumbering on all those nodes                            *)
    (***************************************************************************)
    (* We take the prev_pre (the preorder of this node before renumbering)
       so that we can remove it from the children index in the store.
              
    *)
    let renumber_node handle prev_pre nodeid preorder = 
  (*
  let opt_int64 ff v = 
	if Cell_As_Int64.is_invalid_preorder v 
	then Format.fprintf ff "Root"
	else  Format.fprintf ff "%a" 
		Cell_As_Int64.print_int64_pair v

  (* match v with 
	  | None -> Format.fprintf ff "Root"
	  | Some v ->
	      Format.fprintf ff "%a" 
		Cell_As_Int64.print_int64_pair v  *)
      in
 *)
       (* Format.printf "\t Renumber %d -> %a [old: %a]@."
	nodeid 
	Cell_As_Int64.print_int64_pair preorder
	opt_int64  prev_pre  
	; *)
      let () = store_node handle nodeid preorder in
	Callback.update_preorder handle.recno_handle nodeid prev_pre preorder
	  

    (* Clean this up *)
    let opt_preorder v = v
      (* if v = invalid_preorder 
	 then None
	 else Some v *)

    (* The two functions below implement the renumber by moving
       through the list *)
    let renumber_left_list handle start_index gap = 
      let cursor = Recno.recno_cursor_open handle.left_temp in 
      let rec renumber_helper current_index = 
	match Recno.recno_cursor_get_prev cursor with
	  | None -> current_index
	  | Some (prev_pre,nodeid) ->
	      renumber_node handle (opt_preorder prev_pre) nodeid current_index;
	      renumber_helper (Cell_As_Int64.add gap current_index)
      in
      let index = !(handle.left_index) - 1 in
	(* Format.printf "[Left] Setting the cursor: %d@." index;  *)
	match Recno.recno_cursor_get_set cursor index with
	  | None   -> start_index
	  | Some (prev_pre,nodeid) -> 
	      renumber_node handle (opt_preorder prev_pre) nodeid start_index;
	      renumber_helper (Cell_As_Int64.add gap start_index)

    let renumber_right_list handle current_index gap = 
      let cursor     = Recno.recno_cursor_open handle.right_temp in 
      let last_entry = !(handle.right_index)  in
      let rec renumber_helper current_index entry_number = 
	if entry_number = last_entry 
	then current_index
	else 
	  begin
	    match Recno.recno_cursor_get_next cursor with
	      | None -> raise (Renumber_Exception "Number of entries on right temp is incorrect!")
	      | Some (prev_pre,nodeid) ->
		  renumber_node handle (opt_preorder prev_pre) nodeid current_index;
		  renumber_helper (Cell_As_Int64.add gap current_index) (entry_number + 1)
	  end
      in      
	(* Format.printf "[Right] Setting the cursor: %d@." 1;  *)
	match Recno.recno_cursor_get_set cursor 1 with
	  | None   -> current_index
	  | Some (prev_pre,nodeid) -> 
	      renumber_node handle (opt_preorder prev_pre) nodeid current_index;
	      renumber_helper (Cell_As_Int64.add gap current_index) 2

    let perform_renumber handle (left_index:Cell_As_Int64.t) right_index (preceeding_pre, nodes) total_node_count =
      (* End point issues + 2 *)
      let gap           = Cell_As_Int64.calc_gap right_index left_index (total_node_count + 2) in 
      let cell          = Cell_As_Int64.mk_cell gap in 
	(* We need to delete all those in the range *)
	(* Format.printf "Renumbering left index: %a right index: %a with %d nodes in between: gap %a@."	  
	  Cell_As_Int64.print_int64_pair left_index 
	  Cell_As_Int64.print_int64_pair right_index
	  total_node_count
	  Cell_As_Int64.print_int64_pair cell
	; *)

      let current_index = renumber_left_list  handle (Cell_As_Int64.add left_index cell) cell in 
      let _ = renumber_right_list handle current_index cell in 
      close_internal_handle handle
	  
    (*********************************************************************************************)
    (* val insert_node : handle -> preorder (* of the node preceeding *) -> nodeid list  -> unit *)
    (* This will update the preorders of the corresponding node ids                              *)
    (*********************************************************************************************)   
    let insert_nodes handle preceding_pre nodes =
      (* Accessors *)
      let set_left_current v = 
	match handle.left_cursor_current with 
	  | Not_Set ->
	      handle.left_cursor_current <- v
	  | _ -> raise (Renumber_Exception "Unsafe Setting previously set cursor!")
      in
      let set_right_current v = 
	match handle.right_cursor_current with 
	  | Not_Set ->
	      handle.right_cursor_current <- v
	  | _ -> raise (Renumber_Exception "Unsafe Setting previously set cursor!")
      in

      let () = reset_temp_index handle in 
      let _ = set_left_current (Value (unsafe_cursor_start handle.right_cursor preceding_pre)) in
      let _ = set_right_current (Value (unsafe_cursor_start handle.left_cursor preceding_pre)) in
	(* Need to advance the right cursor because the cursors delete
	   the underlying item. *)
	move_cursor_right handle;

	(* Store the nodes in doc order *)
	(* We could do this in reverse order *)
	let number_of_nodes = ref 0 in 
	let count_and_store n =
	  incr number_of_nodes;
	  store_on_right handle (Cell_As_Int64.invalid_preorder,n)
	in
	Cursor.cursor_iter count_and_store nodes;

      (******************************************)
      (* Scan left and right to find the extent *)
      (******************************************)
      let rec perform_scan current_level current_cell_id nodes_in_area =
	let left_index,nodes_in_area  = scan_left  handle current_cell_id current_level nodes_in_area in 	  
	let right_index,nodes_in_area = scan_right handle current_cell_id current_level nodes_in_area in 
	  if lower_then_threshold_density current_level nodes_in_area 
	  then (left_index, right_index, current_level, nodes_in_area)
	  else perform_scan (current_level + 1) current_cell_id nodes_in_area
      in

      let current_cell_id  = 
	match handle.left_cursor_current with
	  | Value (cellid,_) -> cellid
	  | Reached_End -> 
	      (* Could happen on an empty store *)
	      raise (Renumber_Exception "Empty inserts/Appends not yet handled") 
	  | Not_Set -> raise (Renumber_Exception "Cursor not set?") 
      in (*  ((List.length nodes) + 1) *)
      let left,right,level,nodes_in_area = perform_scan 1 current_cell_id !number_of_nodes in 
	(* We need to trap the "whole renumber case" *)
	perform_renumber handle left right  (preceding_pre,nodes) nodes_in_area

    (*********************)
    (* Previous Preorder *)
    (*********************)
    let internal_previous_entry cursor =
	Preorder_To_Cell.btree_cursor_get_prev cursor 	

    let previous_entry handle preorder =
      let cursor = Preorder_To_Cell.btree_cursor_open handle.btree_handle in 
      let _      = unsafe_cursor_start cursor preorder       in
	internal_previous_entry cursor

    let last_entry handle = 
      let cursor = Preorder_To_Cell.btree_cursor_open handle.btree_handle     in 
	Preorder_To_Cell.btree_cursor_get_last cursor 

    let last_preorder handle  =
      match last_entry handle with
	| None -> None
	| Some (preorder,_) -> Some preorder

    let last_preorder_unsafe handle  =
      match last_entry handle with
	| None -> raise (Renumber_Exception ("Last preorder unsafe used (on empty db?)"))
	| Some (preorder,_) -> preorder

    (* SEMANTICS IS 
       if bthrow_away_first (first,last]
       else [first,last] *)
    let matching_interval handle bthrow_away_first first last = 
      let cursor = Preorder_To_Cell.btree_cursor_open handle.btree_handle in 
	(* WE THROW AWAY THE FIRST VALUE FOR NOW!!! *)
      let pre,fnid      = unsafe_cursor_start cursor first in 
      let cursor_active = ref true in
      let cursor_function () = 
	if !cursor_active then begin
	  match Preorder_To_Cell.btree_cursor_get_next cursor with
	    | None -> 
		Preorder_To_Cell.btree_cursor_close cursor;
		cursor_active := false; 
		None
		  
	    | Some (pre,nodeid) ->
		if pre <= last 
		then Some nodeid
		else begin 
		  Preorder_To_Cell.btree_cursor_close cursor;
		  cursor_active := false; 
		  None 
		end
	end else None
      in
	if bthrow_away_first 
	then Cursor.cursor_of_function cursor_function
	else 
	  Cursor.cursor_append
	    (Cursor.cursor_of_singleton fnid) 
	    (Cursor.cursor_of_function cursor_function)
    
  end
