(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util_tj.ml,v 1.9 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_util_tj
   Description:
     This module contains utilities used by the TwigJoin algorithm.
*)

open Error
open Dynamic_stack

open Dm_types
open Dm_atomic
open Dm
open Dm_util

open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Algebra_type

open Physical_item
open Physical_table
open Physical_sequence
open Cursor

open Code_selection_context

let get_top_item_from_stack stack =
  try
    let (item, ptr,_,_) = top stack in
    getNode (Physical_util.get_item (cursor_of_sequence ( item )))
  with _ -> raise (Query(Internal_Error("Attempt to get top of stack failed")))

let get_top_pre_from_stack stack =
  try
    let (_,_,p,_) = top stack in 
    p
  with _ -> raise (Query(Internal_Error("Attempt to get top of stack failed")))


let get_top_post_from_stack stack =
  try
    let (_,_,_,p) = top stack in 
    p
  with _ -> raise (Query(Internal_Error("Attempt to get top of stack failed")))


(*
 * Test wether all node tests in a tree pattern
 * are qname node tests and wehter they all
 * have an index associated to them
 *)
let no_nt_found_err = 
  Query(Internal_Error("No name test found for twig node (cs_code_twigjoin)"))
and no_index_found_err =
  Query(Internal_Error("No name index found for symbol (cs_code_twigjoin)"))
and not_supported_err =
  Query(Internal_Error("Only support for name tests in twigjoin (cs_code_twigjoin)"))

let check_available_indices code_ctxt pattern =
  let pattern' = Array.sub pattern 1 (Array.length pattern -1) in
  let get_name_index_for_twig twignode =
    let nt =
      match twignode.node_test with
      | Some nt -> nt
      | _ -> raise no_nt_found_err
    in
    match nt with
    | APNameTest symbol ->
	begin
	  match Code_selection_context.get_name_index_handler code_ctxt symbol with
	    | Some index -> index
	    | None -> raise no_index_found_err
	  end
      | _ -> raise not_supported_err
    in
    let name_indices_handler_arr = Array.map get_name_index_for_twig pattern' in
    name_indices_handler_arr

let get_name_indices_array code_ctxt pattern =
  try 
    let pattern' = Array.sub pattern 1 (Array.length pattern -1) in
    let get_name_index_for_twig twignode =
      let nt =
	match twignode.node_test with
	| Some nt -> nt
	| _ -> raise no_nt_found_err
      in
      match nt with
      | APNameTest symbol ->
	  begin
	    match Code_selection_context.get_name_index code_ctxt symbol with
	    | Some index -> index
	    | None -> raise no_index_found_err
	  end
      | _ -> raise not_supported_err
    in
    let name_indices_handler_arr = Array.map get_name_index_for_twig pattern' in
    name_indices_handler_arr
  with  Invalid_argument _ ->
      raise (Query (Internal_Error "Array index out of bounds (cs_code_twigjoin.verify_name_indices)"))

(* get the pre order position of a node *)
let pre n = 
  match n#docorder() with
  | (_,Nodeid.PrePostInt(_,p,_)) -> p
  | _ -> raise (Query (Internal_Error "No full pre/post order description available (cs_code_twigjoin.pre)"))

(* get the post order position of a node *)
let post n = 
  match n#docorder() with
  | (_,Nodeid.PrePostInt(_,_,p)) -> p
  | _ -> raise (Query (Internal_Error "No full pre/post order description available (cs_code_twigjoin.post)"))

(* Debugging utility to print current stack configuration *)
let print_stack_config stacks =
(*  if Debug.misc_debug() then *)
  begin
    try
      Printf.printf "-+-+- BEGIN STACKCONFIG -+-+- (%i stacks)\n%!" (Array.length stacks);
      let max_size = ref 0 in
      let max_size_index = ref 0 in
      for i = 0 to (Array.length stacks) -1 do
	if stacks.(i).size > !max_size then
	  begin
	    max_size_index := i;
	    max_size := stacks.(i).size;
	  end
      done;
      for p = 0 to !max_size -1 do
	for q = 0 to (Array.length stacks) -1 do
	  if (stacks.(q).size > p) then
	    let (item, ptr, pre, post) = stacks.(q).stack.(p) in
	    Printf.printf "%i,(%i,%i); %!" ptr pre post
	  else
	    Printf.printf " ,( , ); %!"
	done;
	Printf.printf "\n";
      done;
      Printf.printf "-+-+- END STACKCONFIG -+-+-\n%!";
    with Invalid_argument _ ->
      raise (Query (Internal_Error "Array index out of bounds (cs_code_twigjoin._print_stack_config)"))
  end

(* prints a tuple's pre-order values  *)
let print_tuple t =
  begin
    Printf.printf " Tuple [ ";
    for i = 0 to (Array.length t) -1 do
      if not (t.(i) = sequence_empty()) then
	let node = getNode (Physical_util.get_item (cursor_of_sequence  t.(i) )) in
	Printf.printf " %i " (pre node)
      else
	Printf.printf " -1 "
    done;
    Printf.printf " ]\n";
  end

(*
 * creates an array of restore functions 
 * for each node in a tree pattern
 *)
let build_restore_array pattern code_ctxt =
(*  let arr = Array.make (Array.length pattern) in *)
  let create_restore_fun twig_node =
    match twig_node.out with
    | Some fld 
      when twig_node.restore_out ->
	build_create_dom_tuple_code code_ctxt fld
    | _ -> (fun x -> ())
  in
  Array.map create_restore_fun pattern

(*
 * Restore an entire tuple
 *)
let restore_tuple restore_array tup =
  begin
    let _ =
      for i = 0 to (Array.length tup) -1 do 
	try
	  let actual_item = Physical_util.get_item (cursor_of_sequence tup.(i)) in
	  restore_array.(i) (materialized_of_list [actual_item]) 
	with _ -> ()
      done
    in
    empty_tuple
  end


(* Make the input available as a common cursor *)
let common_cursor_of_input_cursor (input_cursor: Physical_value.tuple_unit cursor) retrieve_code =
  let fn () =
    try 
      let _ = Cursor.cursor_next input_cursor in
      Some (retrieve_code ())
    with Stream.Failure -> None
  in
  Cursor.cursor_of_function fn

let check_axis code_ctxt node axes pattern src =
  let chldrn = get_child_node_indices pattern pattern.(src) in
  let check_one_axis b s =
    if axes.(s) = Xquery_common_ast.Child then
      begin
	let nt = 
	  match pattern.(s).node_test with
	  | Some nt -> nt
	  | _ -> raise no_nt_found_err
	in
	let stat_ctxt = static_context_from_code_selection_context code_ctxt in
	let res = Code_util_xpath.eval_axis_node_test 
	    stat_ctxt Xquery_common_ast.Child nt node in
	b && not(Cursor.cursor_is_empty res)
      end
    else 
      b
  in
  List.fold_left check_one_axis true chldrn


(* How expensive is this? -- Ph *)
(* Differentiates between anc-desc and child-parent edges *)
let check_tuple axes t =
  let r = ref true in
  for pos = 0 to (Array.length t) -1 do
    if axes.(pos) = 1 then
      begin
	  let n = getNode (Physical_util.get_item (cursor_of_sequence ( t.(pos) ))) in
	  begin
	    match n#parent (Some (None, APNodeKindTest(AAnyKind))) with
	    | Some node1 -> 
		let node2 = getNode (Physical_util.get_item (cursor_of_sequence ( t.(pos -1) ))) in
		r := node_equal node1 node2
	    | None -> r:=false
	  end
      end
  done;
  !r

let rec show_solutions pattern axes stacks stack_number stack_pos =
  let len = Array.length stacks in
  let sp_ind = Array.make len 0 in

  let rec show_solutions_aux _SN _SP =
    let _ = sp_ind.(_SN) <- _SP in
    if _SN = 0 then (* we are in the root *)
      begin
	let output = Array.make len (sequence_empty()) in
	for i = 0 to len -1 do
	  if (stacks.(i).size > sp_ind.(i)) then
	    let (item, j, pre, post) = stacks.(i).stack.(sp_ind.(i)) in
	    output.(i) <- item
	done;
(*	if check_tuple axes output then [output] else []  *)
	[output] 
      end
    else (* recursive call *)
      begin
	let (_,parent_pointer,_,_) = stacks.(_SN).stack.(sp_ind.(_SN)) in
	let output_list = ref [] in
	let parent_SN = get_parent_node_index pattern pattern.(_SN) in
	for i = 0 to parent_pointer -1 do
	  output_list := !output_list @ (show_solutions_aux (parent_SN) i)
	done;
	!output_list
      end
  in
  try 
    show_solutions_aux stack_number stack_pos 
  with Invalid_argument _ ->
      raise (Query (Internal_Error "Array index out of bounds (cs_code_twigjoin.show_solutions)"))


let get_index_window input_source =
  match Cursor.cursor_npeek 2 input_source with
  | [item] ->
      begin
	let n = getNode (Physical_util.get_item (cursor_of_sequence item)) in
	match n#docorder() with
	| (_,Nodeid.PrePostInt(_,pre,post)) -> pre, post
	| _ -> raise (Query (Internal_Error "No full pre/post order description available (cs_code_twigjoin.pre)"))
      end
  | [] -> 0, 0 (* in case of empty input, return nilled window *)
  | _ -> -1, -1
      



let apply_twigjoin code_ctxt treepattern =
  false

