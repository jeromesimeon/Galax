(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_main_memory_btree.ml,v 1.7 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Shredded_main_memory_btree
   Description:
     This module describes a functorial interface for in memory
     btrees. The btree is paramterized by its key and value modules.
     The naming conventions (and the semantics they imply) are present
     in shredded_store_sigs.mli.
*)

(* open Shredded_jungle_common *)

(* TODO:
   o bulk loading
   o push: Only push left right now
     - rename borrow to push or something

   o Delete: Only a leaf operation now
     o Merge?
     o No borrow
   x Cursors

   o Interace work
     o external cursors
     x btree_cursor_del
     o btree_cursor_put
    

   o Extend types to have 'printing' function for debugging

Comments and tests:
   o Duplicates that span pages?
   o After massive deletes the performance may degrade.
   o Change root during cursor operations.

   o Cursor Tests!

*** PROBLEM ***

Cursors need access to the root. The root may change during execution.
If external access had:

solution:
type btree_handle = { .. root : Page ref  .. }

Now when we update the root we write,
x.root := new_value

we have passed the same refernce everywhere.
answers: Why is there a reference inside the root. 

*)

(************************)
(* Btree Module Functor *)
(************************)
(* Functor over the types of key and value *)



(* These contain references so they are mutable, arrays are mutable *)

module Int_Ordered = struct
  type t = int
  type encoded_type = char array
  let encode x = raise Not_found
  let decode x = raise Not_found
  let compare = compare
  let fprintf_value ff v = Format.fprintf ff "%d" v
end


module type Main_Memory_Btree_Functor_Sig =
  functor (Key:Shredded_store_sigs.Shredded_OrderedType) ->
    functor (Value:Shredded_store_sigs.Shredded_OrderedType) ->
sig 

  type btree_handle
  type btree_key   = Key.t
  type btree_value = Value.t
  type btree_cursor
  type cursor_direction = Next | Prev


  val empty_tree : unit -> btree_handle

  (* Get *)
  val btree_get_set        : btree_handle -> btree_key -> (btree_key * btree_value) option
  val btree_get_set_range  : btree_handle -> btree_key -> (btree_key * btree_value) option

  val btree_get_both       : btree_handle -> (btree_key * btree_value) -> (btree_key * btree_value) option
  val btree_get_both_range : btree_handle -> (btree_key * btree_value) -> (btree_key * btree_value) option

  val btree_get_all        : btree_handle -> btree_key -> btree_value Cursor.cursor

  (* put *)
  val btree_put            : btree_handle -> btree_key -> btree_value -> unit

  (* Delete *)
  val btree_delete         : btree_handle -> btree_key -> btree_value -> unit
  val btree_delete_all     : btree_handle -> btree_key -> unit    	  


  val btree_sync           : btree_handle -> unit
  val btree_close          : btree_handle -> unit
  val btree_get            : btree_handle -> btree_key -> btree_value option
  val btree_open           : string -> int -> bool -> btree_handle

  (* Cursor operations *)

  val btree_cursor_open      : btree_handle -> btree_cursor
  val btree_cursor_to_cursor : btree_cursor -> cursor_direction -> (btree_key * btree_value) Cursor.cursor 

  val btree_cursor_put       : btree_cursor -> btree_key -> btree_value -> unit 
  val btree_cursor_get_first : btree_cursor -> (btree_key * btree_value) option 
  val btree_cursor_get_last  : btree_cursor -> (btree_key * btree_value) option 

  val btree_cursor_get_next  : btree_cursor -> (btree_key * btree_value) option
  val btree_cursor_get_prev  : btree_cursor -> (btree_key * btree_value) option

  val btree_cursor_get_set : btree_cursor -> btree_key -> (btree_key * btree_value) option
  val btree_cursor_get_both      : btree_cursor -> btree_key * btree_value -> (btree_key * btree_value) option 
  val btree_cursor_get_set_range : btree_cursor -> btree_key -> (btree_key * btree_value) option
  val btree_cursor_get_both_range: btree_cursor -> btree_key * btree_value -> (btree_key * btree_value) option 

  val btree_cursor_del   : btree_cursor -> unit 
  val btree_cursor_close : btree_cursor -> unit
end

(* The functor matching the above signature *)
module Main_Memory_Btree_Functor
  (Key:Shredded_store_sigs.Shredded_OrderedType) 
  (Value:Shredded_store_sigs.Shredded_OrderedType) 
= struct
  type btree_key   = Key.t
  type btree_value = Value.t

  exception Main_Memory_Btree_Error of string

  (* Make these parameters *)
  (* let leaf_size = 4    
     let internal_size = 4 *)
  let leaf_size     = (* 256 *) 16384
  let internal_size = 256


  (* Internal Data structures for btrees:
     leaf entries, leaf_pages
     internal_entries, internal_pages *)
  type leaf_entry = (btree_key * btree_value)  
  type cursor_direction = Next | Prev

  type page =
    | Internal of internal_content ref 
    | Leaf     of leaf_content ref 
    | Invalid  (* This could be removed becaus we store options and don't have dummy values.*)

  and page_entry = (btree_key * btree_value) * page      
      (* These content structures are heavyweight, the pages are light *)
  and leaf_pointer =
    | Pointer of leaf_content ref
    | Null
  and internal_pointer = 
    | IPointer of internal_content ref
    | INull 

  and leaf_content     = { 
    leaf_id      : int;
    mutable prev : leaf_pointer;
    mutable next : leaf_pointer;
    mutable n_elements : int;
    contents     : leaf_entry option array;
  }
  and internal_content =  { 
    internal_id       : int;
    mutable iprev     : internal_pointer;
    mutable inext     : internal_pointer;
    mutable n_count   : int;

    (* contents *)
    mutable low_page  : page;
    internal_contents : page_entry option array;
  }

   (**********************)
   (* Printing functions *)
   (**********************)
   let fprintf_leaf_pointer ff pointer =
     match pointer with
       | Null -> Format.fprintf ff "Null"
       | Pointer l -> Format.fprintf ff "%d" !l.leaf_id

   let fprintf_internal_pointer ff pointer =
     match pointer with
       | INull -> Format.fprintf ff "INull"
       | IPointer ic -> Format.fprintf ff "%d" !ic.internal_id

   let rec printf_leaf_content ff lc =
     let print_contents ff contents =      
       Array.iter (fun v -> 
		     match v with
		       | Some (k,v) -> 
			   Format.fprintf ff "@[ (%a,%a)@,@]" 
			     Key.fprintf_value k
			     Value.fprintf_value v
		       | None -> ()
		  ) contents
     in
       Format.fprintf ff "@[[@<3>leaf: %d [%d]@, @[@<3>| prev: %a next: %a@] |@, @[@,%a@]]@]" 
	 lc.leaf_id
	 lc.n_elements
	 fprintf_leaf_pointer lc.prev
	 fprintf_leaf_pointer lc.next
	 print_contents lc.contents

   and printf_internal_content ff ic = 
     let print_contents ff contents = 
       Array.iter  (fun v -> 
		      match v with
			| Some ((k,v),page) -> 
			    Format.fprintf ff "@[{ (%a,%a) -> @[%a@,@]}@,@]" 
			      Key.fprintf_value k
			      Value.fprintf_value v
			      printf_page page
			| None -> ()
		   ) contents
     in
       Format.fprintf ff "@[@<2>[internal: %d [%d]@,@[@<2>| prev: %a next: %a@]|@, @[@,{ low -> %a}@,%a@]]@]" 
	 ic.internal_id ic.n_count	
	 fprintf_internal_pointer ic.iprev
	 fprintf_internal_pointer ic.inext
	 printf_page ic.low_page
	 print_contents ic.internal_contents 

   and printf_page ff page = 
     match page with 
       | Invalid -> Format.fprintf ff "%s" "INVALID"
       | Leaf l  -> Format.fprintf ff "%a" printf_leaf_content !l
       | Internal i -> Format.fprintf ff "%a" printf_internal_content !i

   (****************)
   (* Constructors *)
   (****************)

   let unique_id = ref 0 (* For debugging (printing) *)

   let mk_leaf_node id p n c ne = 
     { leaf_id = id; 
       prev = p; 
       next = n; 
       contents = c; 
       n_elements = ne  }

   let mk_internal_node id l nc iprev inext contents = 
     { internal_id = id;
       n_count     = nc;
       inext       = inext; iprev = iprev;
       low_page    = l;
       internal_contents = contents;
     } 

   let make_fresh_leaf_page () = 
     incr unique_id;
     mk_leaf_node !unique_id Null Null (Array.create leaf_size None) 0


   let make_fresh_internal_page low = 
     incr unique_id;
     mk_internal_node !unique_id low 1 INull INull (Array.create internal_size None)


   type btree_handle = { tree_root : page ref; }

   let empty_tree () = { tree_root = ref (Leaf (ref (make_fresh_leaf_page ())))}
   let (btree_handles:(string, btree_handle) Hashtbl.t) = Hashtbl.create 1 

   let btree_open f i b = 
     if Hashtbl.mem btree_handles f 
     then Hashtbl.find btree_handles f
     else begin
       let et = empty_tree () in
	 Hashtbl.add btree_handles f et;
	 et
     end

   let btree_close h = ()
   let btree_sync  h = ()

   (******************)
   (* Getter/Setters *)
   (******************)
   let is_valid_leaf_entry (leaf,index) = 
     (0 <= index && index < leaf.n_elements)     
       (* && (leaf.contents.(index) <> None) *)

   let is_valid_leaf_ref (leaf_ref,index) = is_valid_leaf_entry (!leaf_ref,index)
					      
   (* Assertions - should flag these to compile away. Chris *)
   let assert_valid_leaf_ref_entry str ((leaf,index) as li) = 
     if not (is_valid_leaf_ref li) then
       raise (Main_Memory_Btree_Error ("Assertion Failed! Invalid Leaf Entry: " ^ str) )

   let assert_valid_leaf_entry str ((leaf,index) as li) = 
     if not (is_valid_leaf_entry li) then
       raise (Main_Memory_Btree_Error ("Assertion Failed! Invalid Leaf Entry: " ^ str) )

   let get_internal_low_page inode = inode.low_page      

   let set_internal_low_page inode page = inode.low_page <- page

   let get_internal_entry inode index = 
       try 
	 match inode.internal_contents.(index) with
	   | None -> 
	       raise (Main_Memory_Btree_Error "[Internal] Index is invalid - None?")
	   | Some (k,v) -> (k,v)
       with Invalid_argument _ ->
	 raise (Main_Memory_Btree_Error "[Internal] Index is invalid - cardinality?")

   let set_internal_entry inode index entry = 
     (* ASSERT THIS IS VALID! *)
     inode.internal_contents.(index) <- (Some entry)

   let get_internal_page inode index = 
     let key,page = get_internal_entry inode index in
       page

   let set_internal_page inode index page = 
     let key, _ = get_internal_entry inode index in
     let entry  = key, page in
       (* assert_valid_internal_entry "set_internal_page"; *) (* Not necessary, getter does this check *)
       inode.internal_contents.(index) <- (Some entry)

   let set_internal_key inode index new_key =
     let key,page = get_internal_entry inode index in
     let entry    = new_key, page in 
       (* assert_valid_internal_entry "set_internal_key"; *) (* Not necessary, getter does this check *)
       inode.internal_contents.(index) <- (Some entry)

   (* specialized getters/setters for the highest index *)
   let highest_index inode = inode.n_count - 2

   let get_internal_highest_page inode = 
     get_internal_page inode (highest_index inode)

   let set_internal_high_page inode page = 
     set_internal_page inode (highest_index inode) page

   let set_internal_high_key inode key = 
     set_internal_key inode (highest_index inode) key

   let set_internal_high_entry inode entry = 
     set_internal_entry inode (highest_index inode) entry
   let set_internal_new_high_entry inode entry = 
     set_internal_entry inode ((highest_index inode)+1) entry


   (******************)
   (* Leaf functions *)
   (******************)
   let get_leaf_contents leaf = leaf.contents
   let get_next_leaf leaf     = leaf.next      
   let get_prev_leaf leaf     = leaf.prev
   let get_leaf_count leaf = leaf.n_elements



   (* Binary searches a pair array (k,v) and

      finds the entry that either equals it or if it should be between
      two others, the higher of the two.

      It should also find the first in duplicates
   *)
   (* Binary Search Section *)
   type binary_return = 
     | Higher
     | Lower
     | Inside of int

   (************************)
   (* Comparison functions *)
   (************************)
   let compare_key_value_pair (k0,v0) (k1,v1) = 
     let k_comp = Key.compare k0 k1 in
       if k_comp = 0 then (* lt *)
	 Value.compare v0 v1
       else k_comp	  

   let lte_key       k0 k1 = (Key.compare k0 k1) <= 0 
   let lte_key_value p0 p1 = (compare_key_value_pair p0 p1) <= 0

   (* Not symmetric *)
   let leaf_compare opt r = 
     match opt with
       | None -> raise (Main_Memory_Btree_Error "Comparing empty base element? [leaf]");
       | Some l -> 
	   compare_key_value_pair l r

   let leaf_compare_single opt k1 = 
     match opt with
       | None -> raise (Main_Memory_Btree_Error "Comparing empty base element? [leaf opt]");
       | Some (k0,_) -> 
	   let k_comp = Key.compare k0 k1 in
	     if k_comp = 0 then 
	       1 (* tie goes to the search key... *)
	     else k_comp	  

   let internal_compare opt (rk,rv) = 
     match opt with
       | None -> raise (Main_Memory_Btree_Error "Comparing empty base element? [internal]");
       | Some ((lk,lv),_) -> 
	   let comp = Key.compare lk rk in
	     if comp = 0 
	     then Value.compare lv rv
	     else comp

   let internal_compare_single opt rk = 
     match opt with
       | None -> raise (Main_Memory_Btree_Error "Comparing empty base element? [internal opt]");
       | Some ((lk,lv),_) -> 
	   let comp = Key.compare lk rk in
	     if comp = 0 then 1
	     else comp

   (*******************************)
   (* Main Binary Search Function *)
   (*******************************)
   (* Duplicates: finds the first duplicate *)
   (* Bias: when number is not found, choose which direction *)
   type bias = 
     | Bias_Higher
     | Bias_Lower

   let binary_search compare_function bias array initial_hi key_value =
     let less_than    a b = (compare_function a b) < 0  in
     let greater_than a b = (compare_function a b) > 0  in

     (* Invariant: lo <= key,value <= hi *)
     let rec search_help lo hi = 
       if lo > hi then raise (Main_Memory_Btree_Error "lo greater than hi? nope.");      
       if lo = hi then Inside lo
       else 
	 begin
	   if lo + 1 >= hi then
	     begin	
	       match bias with
		 | Bias_Higher -> 		
		     (* if the lo entry is too small *)
		     if less_than array.(lo) key_value 
		     then Inside hi
		     else Inside lo		
		 | Bias_Lower -> 
		     if greater_than array.(hi) key_value 
		     then Inside lo
		     else Inside hi 
	     end
	   else
	     begin
	       (* let mid = (lo + hi) / 2 in *)
	       let mid = (lo + hi) lsr 1 in 
		 (* If the mid point is at least as large as the key, go left *)
		 if not (less_than array.(mid) key_value) then
		   search_help lo mid
		 else
		   search_help mid hi	    	    
	     end
	 end
     in
       try
	 (* Here we select the default which is to point to the next value *)
	 if initial_hi < 0 then Higher 
	 else if greater_than array.(0) key_value
	 then Lower
	 else if less_than array.(initial_hi) key_value 
	 then Higher
	 else search_help 0 initial_hi
       with Invalid_argument _ ->
	 raise (Main_Memory_Btree_Error "Invalid Argument in BTree Search?")
	 

   (*********************************************************************)
   (* Binary search wrappers for different nodes and different searches *)
   (*********************************************************************)
   let binary_search_internal_node_single internal_node k =
     binary_search internal_compare_single Bias_Lower internal_node.internal_contents (internal_node.n_count-2) k

   let binary_search_internal_node internal_node k =
     binary_search internal_compare Bias_Lower internal_node.internal_contents (internal_node.n_count-2) k

   let binary_search_leaf_node_single leaf_node k =
     binary_search leaf_compare_single Bias_Higher leaf_node.contents (leaf_node.n_elements-1) k

   let binary_search_leaf_node leaf_node k =
     binary_search leaf_compare Bias_Higher leaf_node.contents (leaf_node.n_elements-1) k

   (* End Search Section *)


   (* Update replace entries *)
   let update_current_entry internal_node key_loc (key,new_child) = 
     match key_loc with
       | Lower -> set_internal_low_page !internal_node new_child
       | Inside i ->
	   set_internal_entry !internal_node i (key, new_child)
       | Higher -> 
	   set_internal_high_entry !internal_node (key, new_child)

   let update_current_page internal_node key_loc new_child = 
     let () = 
       match key_loc with
	 | Lower ->
	     set_internal_low_page !internal_node new_child
	 | Inside i ->
	     set_internal_page !internal_node i new_child
	 | Higher -> 
	     set_internal_high_page !internal_node new_child
     in internal_node

   let update_low_key ic key_loc new_key = 
     let () = 
       match key_loc with
	 | Lower -> () (* no key to update *)
	 | Inside i -> 
	     set_internal_key ic i new_key
	 | Higher -> (* this is an error *)
	     set_internal_high_key ic new_key
     in ()

   (* Get the first leaf page that k is on *)
   let rec get_leaf_page page (k:Key.t) = 
     match page with
       | Internal internal_content ->
	   begin	    
	     match binary_search_internal_node_single !internal_content k with
	       | Lower    -> get_leaf_page (get_internal_low_page !internal_content) k 
	       | Higher   -> get_leaf_page (get_internal_highest_page !internal_content) k
	       | Inside i -> get_leaf_page (get_internal_page !internal_content i) k
	   end
       | Leaf v -> v
       | Invalid -> raise (Main_Memory_Btree_Error "Found invalid page on traversal to Leaf?")

   let rec get_leaf_page_both page (k,v) = 
     match page with
       | Internal internal_content -> 
	   begin
	     match binary_search_internal_node !internal_content (k,v) with
	       | Lower    -> get_leaf_page (get_internal_low_page !internal_content) k 
	       | Higher   -> get_leaf_page (get_internal_highest_page !internal_content) k
	       | Inside i -> get_leaf_page (get_internal_page !internal_content i) k
	   end
       | Leaf v -> v
       | Invalid -> raise (Main_Memory_Btree_Error "Found invalid page on traversal to Leaf? [both] ")

   (* If you search for an entry that is higher than all others, you
      get None as a return. Let's not rely on this semantic and if you
      do make it a union type. - Chris *)
   let rec get_leaf_entry page (k:Key.t) = 
     let leaf  = get_leaf_page page k in
       match binary_search_leaf_node_single !leaf k with
	 | Lower    -> Some (leaf, 0)
	 | Inside i -> Some (leaf, i)
	 | Higher   -> 
	     (* it's higher than anything on this page, and our semantic is to 
		point to the next highest *)
	     begin	      
	       match !leaf.next with
		 | Null -> None
		 | Pointer leaf ->
		     get_leaf_entry (Leaf leaf) k
	     end

   let rec get_leaf_entry_both page (k,v) = 
     let leaf  = get_leaf_page_both page (k,v) in
       match binary_search_leaf_node !leaf (k,v) with
	 | Lower    -> Some (leaf, 0)
	 | Inside i -> Some (leaf, i)
	 | Higher   -> 
	     (* it's higher than anything on this page, and our semantic is to 
		point to the next highest *)
	     begin	      
	       match !leaf.next with
		 | Null -> None
		 | Pointer leaf ->
		     get_leaf_entry_both (Leaf leaf) (k,v)
	     end  

   (* get the first entry corresponding to k, if k is not present find
      the next value *)
   let get_first_value root k = 
     match get_leaf_entry root k with
       | None -> None
       | Some ((leaf,index) as li) -> 
	   assert_valid_leaf_ref_entry "get_first_value" li;
	   match !leaf.contents.(index) with
	     | None -> raise (Main_Memory_Btree_Error "Invalid leaf entry! Index is corrupt")
	     | Some v -> Some v


   let get_first_value_both root ((k:Key.t),(v:Value.t)) =
       match get_leaf_entry_both root (k,v) with
	 | None -> None
	 | Some ((leaf,index) as li) -> 
	     assert_valid_leaf_ref_entry "get_first_value_both" li ;
	     match !leaf.contents.(index) with
	       | None -> raise (Main_Memory_Btree_Error "Invalid leaf entry! Index is corrupt [both]")
	       | Some v -> Some v

   (* End Get *)

   (* Delete Section *)
   let delete_entry_in_leaf (leaf,index) = 
     (* Format.printf "[Delete Entry] %d of %d@." index !leaf.n_elements; *)
     if index < (!leaf.n_elements - 1)
     then (* if highest no copy needed *)
       begin
	 Array.blit !leaf.contents (index+1) !leaf.contents index (!leaf.n_elements - index);
       end
     ;
     !leaf.contents.(!leaf.n_elements - 1) <- None;
     (* Format.printf "\t[Delete Entry] State Changed...@."; *)
     !leaf.n_elements <- !leaf.n_elements - 1

   (**************************************************)      
   (* This is the start of the 'put' section of code *)
   (**************************************************)
   type put_return =

     (* These two should be in order < *)
     | Split of ((Key.t * Value.t) * page) * ((Key.t * Value.t) * page)
     | NoSplit of page
     | Borrow of (Key.t * Value.t)

   (*****************************************************************)
   (* SPLIT SECTION, MODULES FOR FUNCTOR                            *)
   (* Splitting is written as a functor since the code is similiar  *)
   (* for both cases                                                *)
   (*****************************************************************)
   type cyclic_rename_key = btree_key
   type cyclic_rename_value = btree_value

   module type Node_Accessors_Sig = sig  
     type node
     type node_key 
     type node_value
     type node_entry = node_key * node_value

     val wrap_page        : node -> page
     val exceeds_max_size : node -> bool
     val has_extra_space  : node -> bool

     (* Partition the node and set the internal pointers correctly *)
     val partition_node   : node -> (node * node)

     val calc_min_node    : node -> node_key * node_value
     val calc_max_node    : node -> node_key * node_value

     (* Entry *)
     val calc_min_entry   : node_entry -> node_entry
     val calc_max_entry   : node_entry -> node_entry

     (* accessors *)
     val get_left_sibling       : node -> node option
     val remove_left_most_entry : node -> node_entry


     (* THIS SHOULD BE GUARANTEED TO SUCCEED *)
     val add_entry        : node -> node_entry -> node
     val lte              : node_entry -> node_entry -> bool
     val convert_to_split : node_key -> node_value -> (Key.t * Value.t)
   end


   (* Accessors for leaf node *)
   module Leaf_Node_Accessors = struct
     type node  = leaf_content      
     type node_key   = cyclic_rename_key
     type node_value = cyclic_rename_value
     type node_entry = btree_key * btree_value

     let wrap_page n = Leaf (ref n)

     let exceeds_max_size leaf = 
       (Array.length (get_leaf_contents leaf)) <= ((get_leaf_count leaf) + 1)

     let has_extra_space leaf =
       (Array.length (get_leaf_contents leaf)) > ((get_leaf_count leaf) + 1)


     let get_left_sibling leaf = 
       match leaf.prev with
	 | Null -> None
	 | Pointer lc -> Some !lc

     let remove_left_most_entry leaf = 
       (* Should be non-empty! *)
       let contents = get_leaf_contents leaf in 
	 
	 match contents.(0) with
	   | None -> raise (Main_Memory_Btree_Error "Index is invalid!");
	   | Some v -> 	
	       let count_minus_one = (get_leaf_count leaf)-1 in 
		 Array.blit contents 1 contents 0 count_minus_one;
		 contents.(count_minus_one) <- None;
		 leaf.n_elements <- leaf.n_elements - 1;
		 v

     let partition_node leaf = 
       (* assert count > 2 *)
       let left  = make_fresh_leaf_page () in 
       let right = make_fresh_leaf_page () in 
	 (* Setup the pointers *)
       let left_leaf  = (Pointer (ref left)) in 
       let right_leaf = (Pointer (ref right)) in
	 left.next <- right_leaf;  
	 left.prev <- leaf.prev;	

	 right.next <- leaf.next;
	 right.prev <- left_leaf;

	 (* Set the previous to be correct *)
	 let () = 
	   match leaf.prev with 
	     | Null -> ()
	     | Pointer l -> !l.next <- left_leaf
	 in
	 let () = 
	   match leaf.next with
	     | Null -> ()
	     | Pointer l -> !l.prev <- right_leaf
	 in

	   (*********************)
	   (* Copy the contents *)
	   (*********************)
	   let count = get_leaf_count leaf in 
	   let half_way = (get_leaf_count leaf)/1 in 
	     (* copy to the right *)
	   let copy_counter = ref 0 in 
	     for i = 0 to (half_way / 2) - 1 do
	       left.contents.(!copy_counter) <- leaf.contents.(i);
	       incr copy_counter
	     done;
	     left.n_elements <- !copy_counter;

	     (* copy to the right *)
	     copy_counter := 0; 
	     for i = (half_way / 2) to count - 1 do
	       right.contents.(!copy_counter) <- leaf.contents.(i);
	       incr copy_counter
	     done;

	     right.n_elements <- !copy_counter;
	     (* Format.printf "Partition: @.%a | %a @."
		printf_leaf_content left
		printf_leaf_content right; *)
	     left, right

     let calc_min_node leaf = 
       match leaf.contents.(0) with
	 | None -> (* this is a failure *)
	     raise (Main_Memory_Btree_Error "Attempting to find minimium of empty leaf?");
	 | Some (k,v) -> k,v

     (* let calc_min_node leaf = fst (calc_min_node_value leaf) *)

     let calc_max_node leaf = 
       match leaf.contents.((get_leaf_count leaf) - 1) with
	 | None -> (* this is a failure *)
	     raise (Main_Memory_Btree_Error "Attempting to find minimium of empty leaf?");
	 | Some (k,v) -> k,v

     (* let calc_max_node leaf = fst (calc_max_node_value leaf) *)

     (* These are individual values *)
     let calc_min_entry (key,entry) = key,entry
     let calc_max_entry (key,entry) = key,entry

     let increment_elements lc = 
       lc.n_elements <- lc.n_elements + 1; lc


     let add_entry leaf (key_entry: node_entry) = 
       (* When the value is not present, when ((1,1),(1,3)) and inserting (1,2), it
	  will point to 3. *)

       let n_entries    = get_leaf_count leaf in 
	 if n_entries = 0 then	    
	     leaf.contents.(0) <- Some key_entry	  
	 else
	   begin
	       (* assert current_index is valid to write *)
	     match binary_search_leaf_node leaf key_entry with
	       | Inside found_index ->
		   (* move them up *)
		   Array.blit leaf.contents found_index leaf.contents (found_index + 1) (n_entries - found_index);
		   leaf.contents.(found_index) <- Some key_entry            

	       | Lower ->
		   Array.blit leaf.contents 0 leaf.contents 1 n_entries;
		   leaf.contents.(0) <- Some key_entry            

	       | Higher ->
		     (* No copy needed *)
		   leaf.contents.(n_entries) <- Some key_entry            

	   end;
	 increment_elements leaf

     let convert_to_split k v = (k,v)
     let lte = lte_key_value
   end (* : Node_Accessors_Sig *)

 module Internal_Node_Accessors = struct
   type node  = internal_content
   type node_key   = cyclic_rename_key * cyclic_rename_value
   type node_value = page
   type node_entry = node_key * node_value

   let wrap_page n = Internal (ref n)

   let exceeds_max_size node = 
     (* -1 because there is the low pointer as part of the count *)
     (Array.length node.internal_contents) <= ((node.n_count) - 1)


   let has_extra_space node = 
     (* it has extra space, if the node_count = Array.length because
	there is one hidden, so node_count -1 are stored in the array *)
     (Array.length node.internal_contents) <= (node.n_count) 



   (* Stats *)
   let rec calc_min_node node = 
     match node.low_page with
       | Invalid -> 
	   raise (Main_Memory_Btree_Error "low page points to invalid node?")
       | Internal internal_node ->
	   calc_min_node !internal_node
       | Leaf leaf -> (Leaf_Node_Accessors.calc_min_node !leaf, Invalid)

   let rec calc_max_node node = 
     let contents = 
       if node.n_count = 1 then
	 Some (node.low_page)
       else
	 let last_valid = node.n_count - 2 in 
	   match node.internal_contents.(last_valid) with
	     | None -> None
	     | Some (_,v) -> Some v
     in
       match contents  with
	 | None ->
	     raise (Main_Memory_Btree_Error "Index corrupt: hi page is invalid?")
	 | Some (Invalid) ->	
	     raise (Main_Memory_Btree_Error "high page points to invalid node?")
	 | Some (Internal internal_node) ->
	     calc_max_node !internal_node
	 | Some (Leaf leaf) -> (Leaf_Node_Accessors.calc_max_node !leaf, Invalid)

   let convert_to_split k n = k

   let calc_min_entry ((key:node_key),(page:node_value)) = 
     match page with 
       | Leaf leaf -> (Leaf_Node_Accessors.calc_min_node !leaf),page
       | Internal internal_node -> calc_min_node !internal_node
       | Invalid -> 	    
	   raise (Main_Memory_Btree_Error "calc min entry passed invalid page?") 

   let rec calc_max_entry (key,page) =  
     match page with 
       | Leaf leaf -> (Leaf_Node_Accessors.calc_min_node !leaf),page
       | Internal internal_node -> calc_max_node !internal_node
       | Invalid -> raise (Main_Memory_Btree_Error "calc max entry passed invalid page?") 

   let remove_left_most_entry node =
     let left     = node.low_page in 
     let contents = node.internal_contents in 
     let low_key = 
       begin
	 match left with
	   | Invalid -> raise (Main_Memory_Btree_Error "invalid node in remove_left_most_entry?")
	   | Leaf lc -> Leaf_Node_Accessors.calc_min_node !lc
	   | Internal ic -> fst (calc_min_node !ic)
       end
     in
       match contents.(0) with
	 | None -> 
	     raise (Main_Memory_Btree_Error ("Removing only node on borrow?"))
	     (* low_key, left *)
	 | Some (k,new_left) ->	    	    
	     (* -1 for the leaving, and -1 for the special low *)
	     Array.blit contents 1 contents 0 (node.n_count-2);
	     node.n_count <- node.n_count - 1;
	     set_internal_low_page node new_left;
	     (low_key,left)

   let get_left_sibling node =
     match node.iprev with
       | INull -> None
       | IPointer ic ->
	   Some !ic

   let increment_count node = 
     mk_internal_node node.internal_id node.low_page (node.n_count + 1) node.iprev node.inext node.internal_contents

   (* Internal Nodes *)
   let move_entries_to_internal_fresh_page contents page start_index end_index =
     let page_contents = page.internal_contents in
     let page_offset = ref 0 in 
       for i = start_index to end_index do
	 page_contents.(!page_offset) <- contents.(i);
	 incr page_offset
       done;
       (* +1 because the page has a low branch already *)
       page.n_count <- !page_offset + 1;
       page


   let partition_node node = 
     let { low_page=low;n_count=n_count;internal_contents=contents} = node in     
       (* assert n_count > 2 *)
     let half_way      = n_count / 2  in (* low page always goes to the left *)
     let left_page     = make_fresh_internal_page low in 
     let left_page     = move_entries_to_internal_fresh_page contents left_page 0 (half_way-1) in 

     let right_low     =
       match contents.(half_way) with
	 | None -> 
	     raise (Main_Memory_Btree_Error ("Right low Does not exits? " ^ (string_of_int n_count)));
	 | Some (k,content) -> content
     in
     let right_page    = make_fresh_internal_page right_low in 
     let right_page    = move_entries_to_internal_fresh_page contents right_page (half_way + 1) (highest_index node) in 
       (* Setup the pointers *)
     let left_pointer  = IPointer (ref left_page) in
     let right_pointer = IPointer (ref right_page) in 

	 left_page.inext <- right_pointer;  
	 left_page.iprev <- node.iprev;	

	 right_page.inext <- node.inext;
	 right_page.iprev <- left_pointer;

	 (* Set the previous to be correct *)
	 let () = 
	   match node.iprev with 
	     | INull -> ()
	     | IPointer n -> !n.inext <- left_pointer
	 in
	 let () = 
	   match node.inext with
	     | INull -> ()
	     | IPointer n -> !n.iprev <- right_pointer
	 in


       left_page, right_page

   let lte (a,_) (b,_) = lte_key_value a b

   let add_entry node ((key,(value:node_value)) as entry) = 
     let n_entries    = node.n_count in 
       (* Format.printf "-- %d@." n_entries; *)
       let search_result =       
	 if n_entries = 1 then Lower
	 else binary_search_internal_node node key
       in
       let () = 
	 match search_result with
	   | Lower -> 
	       (* Format.printf "Lower?@."; *)
	       (* Shift over all the entries and store the new one in the low *)
	       let k,v = calc_min_node node in 	      
	       let min_key = convert_to_split k v in

		 if lte_key_value key min_key then
		   begin
		     (* If the new node is smaller, make it the low *)
		     Array.blit node.internal_contents 0 node.internal_contents 1 (n_entries - 1);
		     set_internal_entry node 0 (min_key, (get_internal_low_page node));
		     set_internal_low_page node value
		   end
		 else (* Leave the lower undisturbed *)
		   begin
		     Array.blit node.internal_contents 0 node.internal_contents 1 (n_entries - 1);
		     set_internal_entry node 0 (key,value);
		   end
	   | Higher -> 
	       (* Format.printf "Higher@."; *)
	       set_internal_new_high_entry node entry
	   | Inside i -> 
	       (* The node to which i points could may come before the one we are inserting.*)		      
	       (* Format.printf "Inside %d - %d@." i ((n_entries - 1) - i); *)
	       Array.blit node.internal_contents (i+1) node.internal_contents (i+2) ((n_entries -2) - i);
	       (* Format.printf "Inside %d - %d@." (i+1) (n_entries - 1 - i); *)
	       set_internal_entry node (i+1) entry	    
       in increment_count node
 end


 (* This is the functor that factors out common code between the split
    operations.  Eventually it should have support for borrowing and
    merging as well.
 *)
 module Split_Module_Functor 
   (PageT : Node_Accessors_Sig) =
 struct
   (* The common split operation for both internal and leaf nodes is
      to split and add a new node *)
   let split_routine current_node new_pair =
     if PageT.exceeds_max_size current_node then
       begin
	 (* First see if we can borrow *)
	 let left_sibling = PageT.get_left_sibling current_node in 
	 let can_borrow   = 
	   match left_sibling with 
	     | Some opt -> PageT.has_extra_space opt
	     | None -> false 
	 in
	   if can_borrow then
	     (* Push the left-most to it *)
	     begin
	       (* Format.printf "BORROWING!@."; *)
	       let left_sibling    = 
		 match left_sibling with 
		   | Some v -> v 
		   | None -> raise (Main_Memory_Btree_Error "How could this happen? Did the guard get removed?")
	       in
	       (* Currently only borrow left *)
		 (* Format.printf "Pre Borrow Dump: @.%a@.%a@."
		    printf_page (PageT.wrap_page current_node)
		    printf_page (PageT.wrap_page left_sibling); *)

		 (* The new node should go on this page, which means that the old node should be moved *)
	       let left_most_entry = PageT.remove_left_most_entry current_node in 
	       let _               = PageT.add_entry left_sibling left_most_entry in 
	       let current_node    = PageT.add_entry current_node new_pair in 
	       let min,min_value   = PageT.calc_min_node current_node in 
		 (* Format.printf "Post Borrow Dump: @.%a@.%a@."
		    printf_page (PageT.wrap_page current_node)
		    printf_page (PageT.wrap_page left_sibling);  *)

		 Borrow (PageT.convert_to_split min min_value)
	     end
	   else
	     begin
	       (*******************)
	       (* Split the nodes *)
	       (*******************)
	       let left_node, right_node = PageT.partition_node current_node in 

		 (* Format.printf "Post partition  @[ %a -=- %a @] @."
		    printf_page (PageT.wrap_page left_node) 
		    printf_page (PageT.wrap_page right_node);   *)


	       (* Calculate some stats about the entry *)
	       let (left_max_key,l_max_value)  as left_max   = PageT.calc_max_node left_node  in 
	       let (left_min_key,l_min_value)                = PageT.calc_min_node left_node  in 
	       let (right_min_key,r_min_value) as right_min  = PageT.calc_min_node right_node in

	       let insert_max = PageT.calc_max_entry new_pair  in 
	       let insert_min = PageT.calc_min_entry new_pair  in

	       (***************************************)
	       (* Figure out where the new entries go *)
	       (***************************************)
	       let left_node, right_node = 
		 if PageT.lte insert_max right_min then
		   begin
		     (* Format.printf "\tAdding Left@."; *)
		     (* This means that all the new entry are smaller than
			the right page *)
		     let left_node = PageT.add_entry left_node new_pair in 
		       left_node, right_node
		   end
		 else 
		   begin
		     (* This means that at least one inserted value is higher
			than the new min value and since the update pages are
			contiguous (they resulted from a split), it should be
			the case that all those on the new case are bigger
			than any in the left page.*)
		     (* Format.printf "\tAdding Right@."; *)
		     (* Remove this... *)
		     if (left_max > insert_min) then raise 
		       (Main_Memory_Btree_Error "Btree split straddles pages? ");
		     let right_node = PageT.add_entry right_node new_pair in
		       left_node, right_node
		   end			      			      
	       in

	       let left_page  = PageT.wrap_page left_node in 
	       let right_page = PageT.wrap_page right_node in 

		 (* Format.printf "Exiting split @[ %a -=- %a @] @."
		    printf_page left_page 
		    printf_page right_page; *)

	       let left_min_key  = PageT.convert_to_split left_min_key  l_min_value in 
	       let right_min_key = PageT.convert_to_split right_min_key r_min_value in 
		 Split ((left_min_key, left_page),
			(right_min_key, right_page))
	     end
       end
     else
       (* There is room! *)
       begin
	 (* Format.printf "No Split@.";  *)
	 NoSplit (PageT.wrap_page (PageT.add_entry current_node new_pair))
       end
 end
   (* Instantiations *)

 module Leaf_Split_Module     = Split_Module_Functor (Leaf_Node_Accessors)
 module Internal_Split_Module = Split_Module_Functor (Internal_Node_Accessors)


 let rec internal_tree_put current_page k v =    
   match current_page with
     | Invalid -> 
	 raise (Main_Memory_Btree_Error "Encountered Invalid Node during put?")
     | Internal internal_node ->
	 begin
	   let key_loc = binary_search_internal_node !internal_node (k,v) in
	   let return_value = 
	     match key_loc with
	       | Lower -> 
		   internal_tree_put (get_internal_low_page !internal_node) k v 
	       | Inside i ->
		   internal_tree_put (get_internal_page !internal_node i) k v 
	       | Higher -> 
		   internal_tree_put (get_internal_highest_page !internal_node) k v
	   in	      
	     match return_value with
	       | NoSplit child_page -> 
		   (* just update the location, we are stopping here *)
		   NoSplit (Internal (update_current_page internal_node key_loc child_page))
	       | Split ((low_key,low_page),(hi_key,hi_page)) ->
		   (* The location we get in a split is a split of the
		      old page, so the low is still good *)
		   let _ = update_current_entry internal_node key_loc (low_key,low_page) in 
		   Internal_Split_Module.split_routine !internal_node (hi_key,hi_page)

	       | Borrow (new_low_value) ->
		   update_low_key !internal_node key_loc new_low_value;
		   NoSplit (Internal internal_node) 
	 end
     | Leaf leaf -> 
	 Leaf_Split_Module.split_routine !leaf (k,v)

 let internal_put root_ref k v = 
   let root = !root_ref in 
     match internal_tree_put root k v with
       | Borrow _ -> () (* no need to do anything *) 
	   
       | NoSplit child -> 
	   root_ref := child

       | Split ((lk, lp), (hk, hp)) ->
	   let new_root = make_fresh_internal_page lp in 
	     set_internal_entry new_root 0 (hk,hp);
	     new_root.n_count <- 2;
	     root_ref := Internal (ref new_root)	
	

   (********************)
   (* Internal CURSORS *)
   (********************)

   type internal_cursor = (leaf_content * int) option (* index *)

   let build_internal_cursor (leaf,index) =
     Some (!leaf, index)

   (***********************************************************************)
   (* Internal cursor move functions                                      *)
   (* Internal cursor functions are functional, external cursor functions *)
   (* are not                                                             *)
   (***********************************************************************)
   let cursor_get_safe icursor = 
     match icursor with
       | None -> None
       | Some ((leaf,index) as li) ->
	   begin
	     if is_valid_leaf_entry li then 
	       match leaf.contents.(index) with
		 | None -> raise (Main_Memory_Btree_Error "Index is invalid in Get!")
		 | (Some return) as ret -> ret
	     else None
	   end

   let cursor_get_unsafe icursor = 
     match icursor with
       | None -> raise (Main_Memory_Btree_Error "get on an invalid cursor")
       | Some ((leaf,index) as li) ->
	   begin
	     if is_valid_leaf_entry li then 
	       match leaf.contents.(index) with
		 | None -> raise (Main_Memory_Btree_Error "Index is invalid in cursor_get_unsafe!")
		 | Some return -> return
	     else raise (Main_Memory_Btree_Error "Index is invalid in cursor_get_unsafe!")
	   end

   let move_cursor_forward icursor = 
     match icursor with 
       | None -> (* raise (Main_Memory_Btree_Error "Move on an invalid internal cursor [forward]") *) None
       | Some (leaf,index) ->
	   if leaf.n_elements == 0 then None
	   else
	     if leaf.n_elements <= (index+1) then 
	       begin (* advance to the next page *)
		 match get_next_leaf leaf with
		   | Null -> None
		   | Pointer leaf ->
		     Some (!leaf,0)
	       end
	     else 
	       Some (leaf, index+1)

   (* Move the cursor state to the previous entry *)
   let move_cursor_backward icursor = 
     match icursor with
       | None -> raise (Main_Memory_Btree_Error "Move on an invalid internal cursor [backward]") (* None *)
       | Some (leaf,index) ->
	   (* Format.printf "ICursor is on index %d@."; *)
	   if index <= 0 then 
	     begin (* advance to the next page *)
	       match get_prev_leaf leaf with
		 | Null -> None
		 | Pointer leaf ->
		     Some (!leaf,!leaf.n_elements - 1)
	     end
	   else 
	     Some (leaf, (index-1))

   (* 
      Cursor delete. After deletion:
      1. If the "next" item is valid, point to this item.
      2. If the "next" item is invalid then try to advance to the next page.
      3. If there is no next page, then return a cursor on which any operation
         save a move_previous operation, will fail. (i.e. it will be (leaf,contents + 1).

      This is so if you delete the last element, and then move_prev it will work.
   *)
   let cursor_delete icursor =
     match icursor with
       | None -> raise (Main_Memory_Btree_Error "Delete on invalid cursor")
       | Some (leaf,index) -> 
	   begin
	     let () = delete_entry_in_leaf ((ref leaf),index) in
	       (* Format.printf "[MMBT] Cursor Delete on index %d@." index; *)
	       if is_valid_leaf_entry (leaf,index) then
		 icursor
	       else 
		 (* Advance to the next leaf if possible *)
		 match move_cursor_forward icursor with
		   | None   -> icursor 
		   | (Some _) as r -> r
		  
	   end      

   (* Align forward with store_sigs.mli *)



   (* These delete functions rely on internal cursors for duplicate handling *)
   let delete_leaf_entry root k v = 
     match get_leaf_entry root k with
       | None -> raise (Main_Memory_Btree_Error "delete key that is not present?")
       | Some (leaf,index) ->	  
	   let rec delete_map icursor = 
	     match cursor_get_safe icursor with
	       | None -> ()
	       | Some (k',v') ->
		   let icursor = if k' = k && v' = v then cursor_delete icursor else icursor in 
		   if k' = k then delete_map (move_cursor_forward icursor)
	   in delete_map (build_internal_cursor (leaf,index))
		 
(* This looks like Dead Code *)
		(*
   let delete_leaf_entry_both root k v = 
     match get_leaf_entry_both root (k,v) with
       | None -> raise (Main_Memory_Btree_Error "delete key that is not present?")
       | Some ((leaf,index) as li) ->	 
	   assert_valid_leaf_ref_entry "delete_leaf_entry_both" li;
	   match !leaf.contents.(index) with
	     | None -> ()
	     | Some (k',v) ->
		 if k' = k then
		   delete_entry_in_leaf (leaf,index)
		 else ()

		*)

   (* Construct an external cursor *)
   let build_external_cursor direction early_terminate icursor =
     let current_state = ref icursor in 
     (* Move the cursor function *)
     let advance_cursor cursor_state = 
       match direction with
	 | Next  -> move_cursor_forward cursor_state
	 | Prev -> move_cursor_backward cursor_state
     in

     let cursor_function () =      
       if !current_state = None 
       then None
       else
	 begin
	   let return = cursor_get_unsafe !current_state in 
	     current_state := advance_cursor !current_state;
	     (* Check termination condition *)
	     if early_terminate return 
	     then begin current_state := None; None end
	     else Some return			    
	 end
     in
       Cursor.cursor_of_function cursor_function

   (**************************)
   (* Bulk Loading functions *)
   (**************************)
   type fill_density =
     | Default
     | Filled
     | Fraction of float

   let bulk_load_sorted density key_value_array = 
     let n_entries = Array.length key_value_array in 
       (* Figure out how many leaves we need *)

     let leaf_capacity  = 
       match density with
	 | Default  -> leaf_size / 2
	 | Filled   -> leaf_size
	 | Fraction f -> 
	     if 0.0 < f && f <= 1.0 then
	       int_of_float ((float_of_int leaf_size) *. f)
	     else
	       raise (Main_Memory_Btree_Error ("Invalid density!"))
     in

     (* Calculate the number of needed leaves *)
     let needed_leaves = n_entries / leaf_capacity in 
     let _ = 
       if n_entries mod leaf_capacity = 0 
       then needed_leaves 
       else needed_leaves + 1 
     in

       ()

   (**********************)
   (* External functions *)
   (**********************)
   (* 
      Get the value corresponding to k, 
      if not present None
   *)
   (* Handles wrapping values and the test when we don't want _range behavior *)
   let internal_get_exact_opt (orig_key, value_opt) key_value_opt = 
     match key_value_opt with
       | None -> None
       | Some (key,value) -> 
	   if orig_key = key
	   then begin
	     match value_opt with
	       | None -> Some (key,value)
	       | Some orig_value ->
		   if orig_value = value 
		   then Some (key,value)
		   else None 
	   end else None

   let btree_get_set root k = 
     internal_get_exact_opt (k, None) 
       (get_first_value !(root.tree_root) k)

   let btree_get root k = 
     match btree_get_set root k with
       | None -> 
	   (* Format.printf "None DUMP [Key %a]@.:%a@."  
	     Key.fprintf_value k 
	     printf_page !(root.tree_root); *)
	   None
       | Some (k,v) -> Some v
			    

   let btree_get_both root (k,v) = 
     internal_get_exact_opt (k, (Some v)) 
       (get_first_value !(root.tree_root) k)

   let btree_get_set_range root k      = get_first_value !(root.tree_root) k 
   let btree_get_both_range root (k,v) = get_first_value_both !(root.tree_root) (k,v)

   (* Build and return an external cursor *)
   let btree_get_all root k = 
     match get_leaf_entry !(root.tree_root) k with
       | None -> Cursor.cursor_empty ()
       | Some (leaf,index)->
	   let icursor = build_internal_cursor (leaf,index) in
	     Cursor.cursor_map snd 
	       (build_external_cursor Next 
		  (fun (k',_) -> k' <> k) icursor)

   (***************************)
   (* Put (k,v) into the tree *)
   (***************************)
   let btree_put tree k v = 
     internal_put tree.tree_root k v
	 
   (********************)
   (* Delete functions *)
   (********************)
   let btree_delete tree k v = 
     let root = !(tree.tree_root) in 
       delete_leaf_entry root k v 

   let btree_delete_all tree k = 
     match get_leaf_entry !(tree.tree_root) k with
       | None -> ()
       | Some (leaf,index) ->
	   let icursor = build_internal_cursor (leaf,index) in 
	   let rec delete_iterate icursor = 
	     let (key,_) = cursor_get_unsafe icursor in 
	       if key = k 
	       then begin delete_iterate  (cursor_delete icursor) end
	       else ()
	   in
	     delete_iterate icursor

   (********************)
   (* cursor functions *)
   (********************)
   type btree_cursor = { mutable cursor_state: internal_cursor;
			 root : page ref ; }

   let mk_btree_cursor ic r = 
     { cursor_state = ic; root = r }
   
   let btree_cursor_to_cursor ecursor cursor_direction =
     build_external_cursor cursor_direction (fun x -> true) ecursor.cursor_state

   let btree_cursor_open btree = 
     mk_btree_cursor None btree.tree_root  

   let btree_cursor_get_next ecursor =
     let icursor = ecursor.cursor_state in 
     let icursor = move_cursor_forward icursor  in 
       ecursor.cursor_state <- icursor;
       cursor_get_safe icursor

   let btree_cursor_get_prev ecursor = 
     let icursor = ecursor.cursor_state in 
     let icursor = move_cursor_backward icursor in 
       ecursor.cursor_state <- icursor;
       cursor_get_safe icursor

	 

   let btree_cursor_get_set_range ecursor k = 
     match get_leaf_entry !(ecursor.root) k with
       | None -> None
       | Some (leaf,index) ->
	   let icursor = build_internal_cursor (leaf,index) in 
	     ecursor.cursor_state <- icursor; (* toss the old reference *)
	     cursor_get_safe icursor

   let btree_cursor_get_set ecursor k = 
     internal_get_exact_opt (k, None) 
       (btree_cursor_get_set_range ecursor k)

   let btree_cursor_get_both_range ecursor (k,v) = 
     match get_leaf_entry_both !(ecursor.root) (k,v) with
       | None -> None
       | Some (leaf,index) ->
	   let icursor = build_internal_cursor (leaf,index) in 
	     ecursor.cursor_state <- icursor; (* toss the old reference *)
	     cursor_get_safe icursor

   let btree_cursor_get_both ecursor (k,v) = 
     internal_get_exact_opt (k, (Some v)) 
       (btree_cursor_get_both_range ecursor (k,v))
	   
   (* Walk the tree, then realize that entire leaves could be empty
      because of our lack of merge *)
   let btree_cursor_get_first ecursor =
     (* walk the tree *)
     let rec get_left_most_leaf page = 
       match page with 
	 | Invalid    -> raise (Main_Memory_Btree_Error ("Get_left_most_leaf encountered Invalid Page"))
	 | Leaf l     -> l 
	 | Internal i -> get_left_most_leaf (get_internal_low_page !i)
     in
     let rec get_first_entry leaf = 
       if !leaf.n_elements > 0
       then begin 
	 match !leaf.contents.(0) with
	   | None -> raise (Main_Memory_Btree_Error ("Get_left_most_leaf encountered Invalid Leaf"))
	   | (Some v) as ret -> ret
       end
       else begin
	 match !leaf.next with
	   | Null -> None (* Empty tree. *)
	   | Pointer lc -> 
	       get_first_entry lc
       end
     in
     let leaf = get_left_most_leaf !(ecursor.root) in 
       get_first_entry leaf

   let btree_cursor_get_last ecursor =
     (* walk the tree *)
     let rec get_right_most_leaf page = 
       match page with 
	 | Invalid    -> raise (Main_Memory_Btree_Error ("Get_left_most_leaf encountered Invalid Page"))
	 | Leaf l     -> l 
	 | Internal i -> get_right_most_leaf (get_internal_low_page !i)
     in
     let rec get_last_entry leaf = 
       if !leaf.n_elements > 0
       then begin 
	 match !leaf.contents.(!leaf.n_elements - 1) with
	   | None -> raise (Main_Memory_Btree_Error ("Get_left_most_leaf encountered Invalid Leaf"))
	   | (Some v) as ret -> ret
       end
       else begin
	 match !leaf.next with
	   | Null -> None (* Empty tree. *)
	   | Pointer lc -> 
	       get_last_entry lc
       end
     in
     let leaf = get_right_most_leaf !(ecursor.root) in 
       get_last_entry leaf
       
   let btree_cursor_put ecursor k v = 
     internal_put ecursor.root k v
          	 
   (* btree_cursor_del *)
   let btree_cursor_del ecursor = 
     ecursor.cursor_state <- cursor_delete ecursor.cursor_state

   (* On close we invalidate the terms *)
   let btree_cursor_close ecursor = 
     ecursor.cursor_state <- None     
     
       
 end

 (* Hard coded Int test *)
 
 module Int_Test = Main_Memory_Btree_Functor (Int_Ordered) (Int_Ordered) 
 let items = [(1,2);(1,1);(1,1);(1,4);(1,5);(1,7);(2,2);(2,2);(2,4);(601,2);(10,1);(1,0);(1,23);(2,2);(2,4);(601,2);(10,1);(1,0);(1,23);]



 open Int_Test

 let fprintf_opt_kv ff okv =
   match okv with
    | None -> Format.fprintf ff "NONE";
    | Some (k,v) ->
	Format.fprintf ff "(%d,%d)" k v

let get_all_dump test key = 
  Format.printf "--- Get all dump ---@.";
  Cursor.cursor_iter 
    (fun v -> 
       Format.printf "%d@,@." v) 
    (Int_Test.btree_get_all test key);
  Format.printf "--- End dump ---@."

(*
let test () = 
  let iter_put h (k,v) = 
    Format.printf ">> %d,%d@." k v;
    Int_Test.btree_put h k v;
    Format.printf "DUMP@.:%a@." printf_page !(h.tree_root)
  in 
  let test = Int_Test.empty_tree () in
    List.iter (iter_put test) items;
    Format.printf "SET!@.";
    get_all_dump test 1;
    get_all_dump test 2;
    get_all_dump test 3;
    get_all_dump test 0;
    Format.printf "DUMP@.:%a@." printf_page !(test.tree_root);
    delete_leaf_entry !(test.tree_root) 1;
    delete_leaf_entry !(test.tree_root) 1;
    Format.printf "DUMP@.:%a@." printf_page !(test.tree_root);
    delete_leaf_entry !(test.tree_root) 1;
    Format.printf "DUMP@.:%a@." printf_page !(test.tree_root);

    Format.printf "--- DUP TEST@.---@.";
    List.iter (iter_put test) [(1,1);(1,1);(1,1);(1,1);];
    get_all_dump test 1;
    Format.printf "DUMP@.:%a@." printf_page !(test.tree_root);
    get_all_dump test 10;

    Format.printf "set: %a set_range: %a both: %a@."
      fprintf_opt_kv (btree_get_set test 1) 
      fprintf_opt_kv (btree_get_set_range test 1)
      fprintf_opt_kv (btree_get_both test (1,2))
    ;
    btree_delete_all test 1; 

    Format.printf "DUMP@.:%a@." printf_page !(test.tree_root);


    Format.printf "set: %a set_range: %a both: %a@."
      fprintf_opt_kv (btree_get_set test 1) 
      fprintf_opt_kv (btree_get_set_range test 1)
      fprintf_opt_kv (btree_get_both test (1,2))
    ;

    List.iter (iter_put test) [(1,2);(1,3);(1,4);(1,5);];

    Format.printf "DUMP@.:%a@." printf_page !(test.tree_root)
*)
      
let test2 () =
  let test = Int_Test.empty_tree () in
    for i = 0 to 10000 do
      Int_Test.btree_put test i (i+1);
(*       Format.printf "First all@.:%a@." printf_page !(test.tree_root); *)
    done;
    Format.printf "DUMP@.:%a@.@.@." printf_page !(test.tree_root)
      
;;


(* test ();  *)
(*  Format.printf "TEST 2@."; test2 () *)


