(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util_materialize.ml,v 1.16 2008/02/01 02:45:26 mff Exp $ *)

(* Module: Code_util_materialize
   Description:
   This module contains code to handle the materialization of tuple cursors into
   an array of physical xml value arrays (the individual tuples)

   Additionally it returns a function which stores one of these tuples without names
   into the "current" tuple slot destructively.
*)

open Compile_context

open Datatypes
open Datatypes_util
open Dm_atomic
open Dm

open Error
open Array
open Cs_util
open Code_selection_context
open Physical_sequence

open Norm_context
open Processing_context

open Xquery_algebra_ast_annotation_util
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_physical_algebra_ast

open Algebra_type
open Processing_context

type sequence_index = int

(*******************************************

   Simple exporting(importing) of the INPUT tuple to/from a tuple
   cursor Used in serialization of a table

*******************************************)

let export_input_tuple_as_tuple_cursor code_ctxt phys_type input_cursor =
  let annot = retrieve_annotation "export_input_tuple_as_tuple_cursor" code_ctxt in
  (* This gets all the names that need to be there in the return. *)
  let needed_names = Array.of_list (Xquery_algebra_ast_annotation_util.get_returned_fields annot) in
  (* For each name build retrieve and restore code *)
  let retrieve_code = Array.map (build_retrieve_tuple_code code_ctxt) needed_names in
  let make_tuple () = Array.map (fun f -> f ()) retrieve_code in
  Cursor.cursor_map (fun () -> make_tuple()) input_cursor 

let import_tuple_cursor_as_input_tuple code_ctxt phys_type input_cursor =
  let needed_names = List.map fst phys_type in
  let tuple_arity = List.length phys_type in 
  (* For each name build restore code *)
  let restore_code  = Array.map (build_create_tuple_code code_ctxt) (Array.of_list needed_names) in
  let store_tuple (vl:Physical_value.xml_value array) =
    for i = 0 to tuple_arity - 1 do
      restore_code.(i) vl.(i)
    done
  in
  Cursor.cursor_map (fun tup -> store_tuple tup; Cs_util.empty_tuple) input_cursor

(***************************************)
(* Local type for materialization code *)
(***************************************)

type ('init,'return,'handle) materialize_handle = {
  init        : 'init   -> 'handle;
  add_tuple   : 'handle -> Algebra_type.eval_fun -> 
		            Execution_context.algebra_context -> 
		            Physical_value.dom_value array -> unit;
  get_return  : 'handle -> 'return;
  reset_table : 'handle -> unit}

let generic_materialize_tuple_cursor mh code_ctxt c_annot init_params = 
  (* This gets all the names that need to be there in the return. *)
  let needed_names = Array.of_list (get_returned_fields c_annot) in

  (* For each name build retrieve and restore code *)
  let retrieve_code = Array.map (build_retrieve_dom_tuple_code code_ctxt) needed_names in
  let restore_code  = Array.map (build_create_dom_tuple_code code_ctxt) needed_names in

  let tuple_arity   = Array.length restore_code in
  let make_tuple () = Array.map (fun f -> f ()) retrieve_code in

  (* Possible forget about name association since these tuples are not accessed any
     where... *)
    (* We should use arrays here instead *)
  let store_tuple (vl:Physical_value.dom_value array) =
    for i = 0 to tuple_arity - 1 do
      restore_code.(i) vl.(i)
    done
  in

  (* take in a cursor -> and then get all its stuff *)
  let tbl        = mh.init init_params in
  let add_fn     = mh.add_tuple tbl    in

  let materialize_fun =
    (fun eval alg_ctxt tc ->
      Debug.print_materialization_debug ">>> Starting to materialize the right branch";
       let materializing = ref 0 in
	 try
	   mh.reset_table tbl;
	   while true do
	     (* This cursor is just called for its side-effects *)
	     Cursor.cursor_next tc;
	     incr materializing;
	     add_fn eval alg_ctxt (make_tuple ())
	   done;
	   Debug.print_materialization_debug ">>> After materialize";
	   mh.get_return tbl
	 with Stream.Failure ->
	   mh.get_return tbl
    ) 
  in
    
    materialize_fun, store_tuple, needed_names



(*********************************)
(* Generic materialize array ref *)
(*********************************)

let generic_materialize_array mh code_ctxt array_restore_fn c_annot init_params = 
  (* This gets all the names that need to be there in the return. *)
  let needed_names = Array.of_list (get_returned_fields c_annot) in

  (* For each name build retrieve and restore code *)
  let retrieve_code = Array.map (build_retrieve_dom_tuple_code code_ctxt) needed_names in
  let restore_code  = Array.map (build_create_dom_tuple_code code_ctxt) needed_names in

  let n_functions   = Array.length restore_code in
  let make_tuple () = Array.map (fun f -> f ()) retrieve_code in

  (* Possible forget about name association since these tuples are not accessed any
     where... *)
  (* We should use arrays here instead *)
  let store_tuple (vl:Physical_value.dom_value array) =
    for i = 0 to n_functions - 1 do
      restore_code.(i) vl.(i)
    done
  in

  (* take in a cursor -> and then get all its stuff *)
  let tbl        = mh.init init_params in
  let add_fn     = mh.add_tuple tbl    in

  let materialize_fun =
    (fun eval alg_ctxt array_to_materialize ->                      
       let materializing = ref 0 in
       let array_length  = Array.length array_to_materialize in 

	 mh.reset_table tbl;

	 while !materializing < array_length do
	   array_restore_fn array_to_materialize.(!materializing);
	   add_fn eval alg_ctxt (make_tuple ());
	   incr materializing
	 done;

	 mh.get_return tbl
    ) 
  in
  materialize_fun, store_tuple, needed_names


(*************************************************************************************)
(*************************************************************************************)
(****************************** SPECIFIC MATERIALIZATION *****************************)
(*************************************************************************************)
(*************************************************************************************)

(******************)
(* Specific array *)
(******************)

type array_materialize_fun  = 
    Algebra_type.eval_fun -> Execution_context.algebra_context ->
    Physical_value.tuple_unit Cursor.cursor -> Physical_value.dom_value array array

type restore_function = Physical_value.dom_value array -> unit

let initial_size    =  1024
let dummy_value     =  Array.create 1 (materialized_of_list [])
type array_handle   =
    { mutable table          : Physical_value.dom_value array array;
      mutable current_size   : int;
      n_items        : int ref}

let array_init_table () =  
  { table = Array.create initial_size dummy_value;
    current_size = initial_size;
    n_items      = ref 0;}

let array_reset tbl =
  tbl.n_items := 0

(* Seperate out this structure for materialization
   and optimize it. Could even put in cardinality estimates in the stream
   Such estimates would be known when we compiled the original trees into  
   tuples. 
 *)
let array_add_tuple tbl eval alg_ctxt t =
  let n_items_ref = tbl.n_items in
    try
  (* If it is time to grow the table *)
  if ((!n_items_ref + 1) >= tbl.current_size) then
    begin 
      let new_size  = tbl.current_size * 2 in (* Multiplicative instead? *)
      Debug.print_materialization_debug("new_size = "^(string_of_int(new_size)));
      let old_table = tbl.table in
      tbl.table <- Array.create new_size tbl.table.(0); (* There has to be a better way to do this *)
      tbl.current_size <- new_size;

      (* Copy over the items *)
      Debug.print_materialization_debug("!n_items_ref = "^(string_of_int(!n_items_ref)));
      Array.blit old_table 0 tbl.table 0 !n_items_ref;

    end
      ;
      (* Here the array is sure to have the proper size *)

      tbl.table.(!n_items_ref) <- t;
      incr n_items_ref

    with Invalid_argument x ->
      begin
	raise (Query (Code_Selection ("Materialization Out of bounds")))
      end

let array_helper_get_items tbl = !(tbl.n_items)
let array_get_return       tbl = 
  
  (* We need to trim this table for the return now, to keep out invalid values *)
  let n_items   = array_helper_get_items tbl in
  let ret_array = Array.create n_items tbl.table.(0) in (* need something better here *)
    Array.blit tbl.table 0 ret_array 0 n_items;
    ret_array

let array_materialize_handler = {
  init        = array_init_table;
  add_tuple   = array_add_tuple;
  get_return  = array_get_return;
  reset_table = array_reset;
}

(*********************)
(* Hash Table Arrays *)
(*********************)

type hash_materialize_fun    =
    Algebra_type.eval_fun -> Execution_context.algebra_context 
      -> Physical_value.dom_value array array
      -> int AtomicValueHash.t

type string_hash_table_handle = {
  predicate_branch     : Code_util_predicates.predicate_branch;
  ht                   : int (* the sequence order/reference into the array *) AtomicValueHash.t;
  seq_order            : int ref;
  nsenv                : Namespace_context.nsenv;  
}

let hash_init (pb,nsenv) = 
  { predicate_branch     = pb;
    ht                   = AtomicValueHash.create 1;
    seq_order            = ref 0;
    nsenv                = nsenv;
  }


(* Adding tuple:
   if there are untypedAtomics in the 
   stream, then we handle their promotion according
   to fs:untyped-to-any (assuming all options)

   o If the operand can be cast -> a double then 
     we hash and sore it as a double
   
   o The operand is cast to a string -> 
     and hashed.

   --- both are done ---

   this involves calling promote_to_higest_numeric op
   
   Also all numerics are promoted before they are hashed
   to type double.
   
*)
let hash_add_tuple 
  (hh: string_hash_table_handle) 
  (eval: Algebra_type.eval_fun)
  (alg_ctxt: Execution_context.algebra_context) current_tup =

  (**********************)
  (* Wow, this is nasty *)
  (**********************)

  let cur_store = !(hh.seq_order) in 

  (******************************)
  (* Get the sequence of values to which the some_expr would have been bound,
     if there is no some expr -> this is None -> we are all done. *)
  let value_seq = Code_util_predicates.evaluate_predicate_branch hh.predicate_branch eval alg_ctxt hh.nsenv in        
    List.iter
      (fun key -> AtomicValueHash.add hh.ht key cur_store)
      value_seq ;
  
    incr hh.seq_order

let hash_get_return mh = mh.ht
let hash_reset mh = AtomicValueHash.clear mh.ht 

let hash_materialize_handler () = 
  { init        = hash_init;
    add_tuple   = hash_add_tuple;
    get_return  = hash_get_return;
    reset_table = hash_reset }


(********************)
(* Sort materialize *)
(********************)

type sort_array_materialize_fun  = 
    Algebra_type.eval_fun -> Execution_context.algebra_context 
      -> (Physical_value.dom_value array array)
	-> Code_util_ridlist.rid Dm_atomic_btree.btree (* in sorted order *)

type sort_array_handle   =
    { mutable bulk_loader    : Code_util_ridlist.rid Dm_atomic_btree.bulk_loader;
      predicate_branch       : Code_util_predicates.predicate_branch;
      nsenv                  : Namespace_context.nsenv;
      seq_order              : int ref }

let sort_array_init_table (pb,nsenv) =
  let compare i i' = i - i' in
  { bulk_loader          = Dm_atomic_btree.init compare;
    predicate_branch     = pb;
    nsenv                = nsenv;
    seq_order            = ref 0 }

(* Seperate out this structure for materialization
   and optimize it. Could even put in cardinality estimates in the stream
   Such estimates would be known when we compiled the original trees into  
   tuples. 
 *)
let sort_array_add_tuple tbl eval alg_ctxt t =
  let value_seq =
    Code_util_predicates.evaluate_predicate_branch
      tbl.predicate_branch
      eval
      alg_ctxt
      tbl.nsenv
  in
  let index = !(tbl.seq_order) in
  Dm_atomic_btree.bulk_add tbl.bulk_loader (List.map (fun x -> (x,index)) value_seq);
  incr tbl.seq_order

let sort_array_get_return tbl =
  Dm_atomic_btree.finalize tbl.bulk_loader

let sort_array_reset_table tbl =
  Dm_atomic_btree.reset tbl.bulk_loader

let sort_array_materialize_handler () = {
  init        = sort_array_init_table;
  add_tuple   = sort_array_add_tuple;
  get_return  = sort_array_get_return;
  reset_table = sort_array_reset_table;
}


(**********************)
(* Exported Functions *)
(**********************)

let materialize_cursor_to_dom_value_array code_ctxt c_annot () =
  Debug.print_materialization_debug ">>>> [materialize_cursor_to_dom_value_array]";
  generic_materialize_tuple_cursor
    array_materialize_handler code_ctxt c_annot ()

let materialize_array_to_sorted_array_index code_ctxt c_annot restore_fn init =  
  Debug.print_materialization_debug ">>>> [materialize_array_to_sorted_array_index]";
  let (x,_,_) =
    generic_materialize_array
      (sort_array_materialize_handler ()) code_ctxt restore_fn c_annot init
  in x

let materialize_array_to_hash code_ctxt c_annot restore_fn init =
  Debug.print_materialization_debug ">>>> [materialize_array_to_hash]";
  generic_materialize_array
    (hash_materialize_handler ()) code_ctxt restore_fn c_annot init

let pproc = Processing_context.default_processing_context()

let print_cell (t:Physical_value.dom_value) =
  let tc = Physical_sequence.cursor_of_sequence t in
  let ts = Serialization.bserialize_datamodel pproc tc in
    Printf.printf "%s|" ts

let print_row (t:Physical_value.dom_value array) =
  Printf.printf "|";
  Array.iter print_cell t;
  Printf.printf "\n";
  flush stdout

let print_materialized m =
  let len = Array.length m in
    Printf.printf "Materializing table of size: %i\n" len;
(*     Printf.printf "---------------\n"; *)
(*     Array.iter print_row m; *)
(*     Printf.printf "\n---------------\n"; *)
    Printf.printf "\n";
    flush stdout

let print_annotation annot algop_name = 
  Debug.print_materialization_debug ("code_util_materialize: annotation for " ^ (Xquery_algebra_ast_util.string_of_algop_expr_name algop_name));
  Debug.print_materialization_debug "accessed_fields:";
  List.iter (fun x ->   Debug.print_materialization_debug (Namespace_names.prefixed_string_of_rqname x)) annot.accessed_fields;
  Debug.print_materialization_debug "returned_fields:";
  List.iter (fun x -> Debug.print_materialization_debug (Namespace_names.prefixed_string_of_rqname x)) annot.returned_fields;
  flush stdout
  

let build_materialize_table_code algop code_ctxt =
  let annot = retrieve_annotation "build_default_materialize_table_code" code_ctxt in 
(*     if Debug.materialization_debug() then *)
(*       print_annotation annot algop.palgop_expr_name; *)
    let materialize_fun, restore_fn, needed_names = materialize_cursor_to_dom_value_array code_ctxt annot () in
      (* *REALLY* Should be a common operation.. *)
    let our_cursor m = 
      let offset = ref 0 in
      let len = Array.length m in
        (fun () ->
	      let res = 
	        if !offset < len then	   
	          begin
	            restore_fn m.(!offset);
	            empty_tuple_opt
	          end
	        else
	          None
	      in
	        incr offset;
	        res)
    in
      (fun eval alg_ctxt input_cursor ->
        if Debug.materialization_debug() then
          Debug.print_materialization_debug ("Code generated by build_materialize_table: "^
                                                "Materializing cursor for "^ 
                                                (Xquery_algebra_ast_util.string_of_algop_expr_name algop.palgop_expr_name)^".\n");
        let materialized = materialize_fun eval alg_ctxt input_cursor in
          if Debug.materialization_debug() then
            begin
              print_materialized materialized
            end;
          Cursor.cursor_of_function (our_cursor materialized)
      )
      

let ops_commute op1 op2 = 
  let (_,_,access1,modif1) = Alg_path_structutil.paths_from_path_annotation "Code_util_materialize.commute_physical" op1.annotation.path_annotation in
  let (_,_,access2,modif2) = Alg_path_structutil.paths_from_path_annotation "Code_util_materialize.commute_physical" op2.annotation.path_annotation in
    (Alg_path_structutil.path_sequences_with_disjoint_roots modif1 access2) && (Alg_path_structutil.path_sequences_with_disjoint_roots modif2 access1) &&
      (Alg_path_structutil.path_sequences_with_disjoint_roots modif1 modif2)      

(* conservative test that two expressions commute, modulo the order of the values in the result *)
let commute_physical code_ctxt op1 op2 = 
  let comp_ctxt = compile_context_from_code_selection_context code_ctxt in 
  let norm_ctxt = norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = (processing_context_from_norm_context norm_ctxt) in

  if Conf.is_xquery() then
    true (* expressions without side effects always commute, modulo order of values in the result *)
  else
    if not (proc_ctxt.infer_independence) then
        false
    else
      ops_commute op1 op2

let commute_physical_with_array code_ctxt op op_array = 
  if Conf.is_xquery() then
    true (* expressions without side effects always commute, modulo order of values in the result *)
  else
    let comp_ctxt = compile_context_from_code_selection_context code_ctxt in 
    let norm_ctxt = norm_context_from_compile_context comp_ctxt in
    let proc_ctxt = (processing_context_from_norm_context norm_ctxt) in
    if not (proc_ctxt.infer_independence) then
      false
    else
      let comm = ref true in
      let cont = ref true in
      let len = Array.length op_array in
      let i = ref 0 in 
      while !cont && !i<len do              
        if not (ops_commute op op_array.(!i)) then
          begin
            comm := false;
            cont := false
          end;
        i := !i + 1
      done;
      !comm


(* tests whether the first operator may modify the paths accessed by op2 *)
let modified1_disjoint_from_accessed2 code_ctxt op1 op2 =
  if Conf.is_xquery() then true
  else    
    let comp_ctxt = compile_context_from_code_selection_context code_ctxt in 
    let norm_ctxt = norm_context_from_compile_context comp_ctxt in
    let proc_ctxt = (processing_context_from_norm_context norm_ctxt) in
      if not (proc_ctxt.infer_independence) then
        false
      else
        let (_,_,access1,modif1) = Alg_path_structutil.paths_from_path_annotation "Code_util_materialize.commute_physical" op1.annotation.path_annotation in
        let (_,_,access2,modif2) = Alg_path_structutil.paths_from_path_annotation "Code_util_materialize.commute_physical" op2.annotation.path_annotation in
          (Alg_path_structutil.path_sequences_with_disjoint_roots modif1 access2) 


let print_debug_message algop can_pipeline = 
  if Debug.materialization_debug() then
    let log_opname = string_of_algop_expr_name algop.palgop_expr_name in
    let phys_opname = Print_xquery_physical_type.string_of_physop_expr_name (Xquery_algebra_ast_util.pname_of_algop algop) in
    if can_pipeline then 
      Debug.print_materialization_debug ("Pipelining input of " ^ log_opname ^ "(" ^ phys_opname ^ ") operator ")
    else 
      Debug.print_materialization_debug ("Unable to pipeline input of " ^ log_opname ^ "(" ^ phys_opname ^ ") operator ")
  
(* By default, the materialization flag is set to true,
   We reset whenever we find out that pipelining can be done instead.
 *)

let annotate_materialization code_ctxt algop =
  if Conf.get_materialize_tables() = Conf.Analysis && not (Conf.is_xquery ()) then
    begin
    match algop.palgop_expr_name with
    | AOEMapToItem -> 
        let indep = access_onesub algop.psub_expression in
        let dep =  access_onesub algop.pdep_sub_expression in
        if modified1_disjoint_from_accessed2 code_ctxt dep indep then
          indep.annotation.materialize_tuple_stream <- false
	      
    | AOEProduct -> 
        let op1,op2 = access_twosub algop.psub_expression in
        let can_pipeline = 
          if op2.annotation.has_side_effect(* op2 must be pure *)
          then false
          else commute_physical code_ctxt op1 op2
        in
        begin
          if Debug.materialization_debug() then
            if can_pipeline then 
              Debug.print_materialization_debug ("Pipelining left branch of Product")
            else 
              Debug.print_materialization_debug ("Unable to pipeline left branch of Product");
          op2.annotation.materialize_tuple_stream <- false; (* it is materialized anyway inside the code *)
          if can_pipeline then
            op1.annotation.materialize_tuple_stream <- false
        end                        

    | AOEOrderBy _ -> (* order-by materializes its input anyway, so we want to avoid double materialization *)
        let indep = access_onesub algop.psub_expression in
        indep.annotation.materialize_tuple_stream <- false

    | AOEGroupBy _ -> (* same for GroupBy; just that this one might also have dep. subops. that manipulate tables *)
        let indep = access_onesub algop.psub_expression in
        let deps = access_manysub algop.pdep_sub_expression in
        indep.annotation.materialize_tuple_stream <- false;
        Array.iter (fun x -> x.annotation.materialize_tuple_stream <- false) deps
          
    | AOESelect _ ->
        let indep = access_onesub algop.psub_expression in
        let deps =  access_manysub algop.pdep_sub_expression in
        let can_pipeline = ref true in
        let cont = ref true in
        let len = Array.length deps in
        let i = ref 0 in 
        while !cont && !i<len do              
          if not (modified1_disjoint_from_accessed2 code_ctxt deps.(!i) indep) then
            begin
              can_pipeline := false;
              cont := false
            end;
          i := !i + 1
        done;
        print_debug_message algop !can_pipeline;
        if !can_pipeline then
          indep.annotation.materialize_tuple_stream <- false          
              
    | AOEMapIndexStep _  | AOEMapIndex _  ->  (* same for MapIndex always goes tuple by tuple *)
        let indep = access_onesub algop.psub_expression in
        indep.annotation.materialize_tuple_stream <- false
            
    | AOEMap 
    | AOEMapConcat       
    | AOEOuterMapConcat _ ->
        let indep = access_onesub algop.psub_expression in
        let dep = access_onesub algop.pdep_sub_expression in
        let can_pipeline = commute_physical code_ctxt indep dep in
        begin
          print_debug_message algop can_pipeline;
          if can_pipeline then
            begin
              dep.annotation.materialize_tuple_stream <- false;
              indep.annotation.materialize_tuple_stream <- false
            end
        end                        
          
    | AOENullMap _ ->  (* OMap, in this implementation, only checks for empty
                          values. If we align it to the paper, it should 
                          be similar to Map *)                          
        let indep = access_onesub algop.psub_expression in
        indep.annotation.materialize_tuple_stream <- false

      (* need to finish the "real" analysis here!! *)
    | AOEJoin _ -> 
        begin 
          match Xquery_algebra_ast_util.pname_of_algop algop with     
          | POJoin_NestedLoop ->                   
              let op1,op2 = access_twosub algop.psub_expression in
              let deps = access_manysub algop.pdep_sub_expression in
              let can_pipeline = 
                commute_physical_with_array code_ctxt op1 deps &&
                commute_physical code_ctxt op1 op2 
              in
              begin
                print_debug_message algop can_pipeline;
                op2.annotation.materialize_tuple_stream <- false; (* this one is materialized anyway inside the code *)
                if can_pipeline then
                  op1.annotation.materialize_tuple_stream <- false
              end                                            

          | POJoin_Hash 
          | POJoin_Sort ->  
              let op1,op2 = access_twosub algop.psub_expression in
              let can_pipeline = 
                commute_physical code_ctxt op1 op2 
              in
              begin
                print_debug_message algop can_pipeline;
                op2.annotation.materialize_tuple_stream <- false; (* this one is materialized anyway inside the code *)
                if can_pipeline then
                  op1.annotation.materialize_tuple_stream <- false
              end                                            

          | _ -> raise(Query(Internal_Error("Code_util_materialize.should_materialize: Invalid physical operator for logical join operator")))
        end
          
    | AOELeftOuterJoin _ -> 
        begin
          match Xquery_algebra_ast_util.pname_of_algop algop with                   
          | POLeftOuterJoin_NestedLoop _ ->
              let op1,op2 = access_twosub algop.psub_expression in
              let deps = access_manysub algop.pdep_sub_expression in
              let can_pipeline = 
                commute_physical_with_array code_ctxt op1 deps &&
                commute_physical code_ctxt op1 op2 
              in
              begin
                print_debug_message algop can_pipeline;
                op2.annotation.materialize_tuple_stream <- false; (* this one is materialized anyway inside the code *)
                if can_pipeline then
                  op1.annotation.materialize_tuple_stream <- false
              end                                            

          | POLeftOuterJoin_Hash _
          | POLeftOuterJoin_Sort _ ->
              let op1,op2 = access_twosub algop.psub_expression in
              let can_pipeline = 
                commute_physical code_ctxt op1 op2 
              in
              begin
                print_debug_message algop can_pipeline;
                op2.annotation.materialize_tuple_stream <- false; (* this one is materialized anyway inside the code *)
                if can_pipeline then
                  op1.annotation.materialize_tuple_stream <- false
              end                                            
          | _ -> raise(Query(Internal_Error("Code_util_materialize.should_materialize: Invalid physical operator for logical outer join operator")))
                
        end
    | _ -> ()
    end
  else ()

let should_materialize algop =
  match Conf.get_materialize_tables() with
    | Conf.Always -> true
    | Conf.Never   -> false
    | Conf.Analysis ->
        if Conf.is_xquery () then false
        else algop.annotation.materialize_tuple_stream

let produces_a_table algop = match algop.palgop_expr_name with
  | AOEGroupBy _
  | AOEOrderBy _ 
  | AOEMapFromItem _
  | AOEMap          
  | AOENullMap _
  | AOEMapIndex _
  | AOEMapIndexStep _
  | AOEMapConcat       
  | AOEOuterMapConcat _
  | AOEProduct                 
  | AOESelect _
  | AOEJoin _
  | AOELeftOuterJoin _ -> true
  | _ -> false

