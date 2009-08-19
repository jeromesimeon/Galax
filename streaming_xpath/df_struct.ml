(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: df_struct.ml,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Df_struct
   Description:
     This module provides data structures together with corresponding creation
   and manipulation functions for a generic class of data flow graphs.

   - Michael *)

open Format


exception Dferror of string


(* nodes that do not have successors *)
type dfsink_kind =
  | DFSerialize
  | DFControl

(* nodes that have exactly one successor *)
type dfpass_kind =
    
  (* signals wether order of a node relative to any other is relevant *)
  | DFOrdered of int
  | DFUnordered

  (* end point of multiple arcs *)
  | DFMerge
      
  | DFImmediate

type dfnode_kind =
  | DFSink of dfsink_kind
  | DFPass of dfpass_kind
  | DFFork
      

type dfnode_id = int


(* node kind, payload, affiliation (including the node itself), node identity, visited flag, intermediate result storage *)
type ('a, 'b) dfnode = DFNode of  (('a, 'b) dfkind * 'a * ('a, 'b) dfnode list ref * dfnode_id * bool ref * 'b option ref) | DFNil

and ('a, 'b) dfkind = DFNone of dfnone | DFSingle of ('a, 'b) dfsingle | DFMultiple of ('a , 'b) dfmultiple

and dfnone = dfsink_kind

and ('a, 'b) dfsingle = dfpass_kind * ('a, 'b) dfnode ref

and ('a, 'b) dfmultiple = ('a, 'b) dfnode list ref

(* data flow sources, terminal *)
type ('a, 'b) dfgraph = ('a, 'b) dfnode list * ('a, 'b) dfnode


(* node identity *)
let dfnode_id = ref 0


(*****************)
(* node creation *)
(*****************)

let inc_id id =
  let old_id = !id in
    id := old_id + 1;
    old_id

let mkdfnode_id () =
  inc_id dfnode_id

let mkdfnode dfnode_kind a =
  let dfnode_id = mkdfnode_id () in
    DFNode (dfnode_kind, a, ref [], dfnode_id, ref false, ref None)
      
let mkdfnode_dfsink dfsink_kind a =
  mkdfnode (DFNone dfsink_kind) a

let mkdfnode_dfpass dfpass_kind a =
  mkdfnode (DFSingle (dfpass_kind, ref DFNil)) a
    
let mkdfnode_dffork a =
  mkdfnode (DFMultiple (ref [])) a


(******************)
(* graph creation *)
(******************)

let mkdfgraph_empty () =
  ([], DFNil)
    
let mkdfgraph_singleton dfnode =
  ([dfnode], dfnode)


(***************)
(* node access *)
(***************)

let get_dfnode_value dfnode =
  match dfnode with
    | DFNil -> raise (Dferror "Tried to get value from DFNil.")
    | DFNode (_, a, _, _, _, _) -> a

let get_dfnode_kind dfnode =
  match dfnode with
    | DFNode (dfkind, _, _, _, _, _) ->
	begin
	  match dfkind with
	    | DFNone dfsink_kind -> DFSink dfsink_kind
	    | DFSingle (dfpass_kind, _) -> DFPass dfpass_kind
	    | DFMultiple _ -> DFFork
	end
    | DFNil -> raise (Dferror "Tried to get node kind from DFNil.")

let get_dfnode_id dfnode =
  match dfnode with
    | DFNode (_, _, _, dfnode_id, _, _) -> dfnode_id
    | DFNil -> raise (Dferror "Tried to get node id from DFNil.")      

let is_dfsink dfsink_kind dfnode =
  match dfnode with
    | DFNode (DFNone DFControl, _, _, _, _, _) when (dfsink_kind = DFControl) -> true
    | DFNode (DFNone DFSerialize, _, _, _, _, _) when (dfsink_kind = DFSerialize) -> true	
    | _ -> false

let is_dfpass dfpass_kind dfnode =
  match dfnode with
    | DFNode (DFSingle ((DFOrdered id), _), _, _, _, _, _) when (dfpass_kind = DFOrdered id) -> true
    | DFNode (DFSingle (DFUnordered, _), _, _, _, _, _) when (dfpass_kind = DFUnordered) -> true
    | DFNode (DFSingle (DFMerge, _), _, _, _, _, _) when (dfpass_kind = DFMerge) -> true
    | DFNode (DFSingle (DFImmediate, _), _, _, _, _, _) when (dfpass_kind = DFImmediate) -> true
    | _ -> false

let is_dffork dfnode =
  match dfnode with
    | DFNode (DFMultiple _, _, _, _, _, _) -> true
    | _ -> false


(********************)
(* node affiliation *)
(********************)

let affiliate_dfnode dfnodes dfnode =
  match dfnode with
    | DFNode (_, _, affiliates, _, _, _) -> affiliates := dfnodes
    | DFNil -> raise (Dferror "Tried to affiliate DFNil.")

let affiliate_unary dfnode =
  affiliate_dfnode [dfnode] dfnode

let affiliate_many dfnodes =
  List.iter (affiliate_dfnode dfnodes) dfnodes

let affiliate_binary dfnode1 dfnode2 =
  affiliate_many [dfnode1; dfnode2]

let get_affiliates dfnode =
  match dfnode with
    | DFNil -> raise (Dferror "Tried to get affiliates of DFNil.")
    | DFNode (_, _, affiliates, _, _, _) -> !affiliates

    
(*********************)
(* graph composition *)
(*********************)

(* eliminates duplicates based on physical equality *)
let merge_dfnode dfnode dfnodes =
  if not (List.memq dfnode dfnodes) then [dfnode] else []

let rec merge_dfnodes dfnodes1 dfnodes2 =
  match dfnodes1 with
    | [] -> dfnodes2
    | hd :: tl -> (merge_dfnode hd dfnodes2) @ (merge_dfnodes tl dfnodes2)

let mkarc dfnode1 dfnode2 =
  match dfnode1 with
    | DFNode (DFSingle (_, dfnode_ref), _,  _, _, _, _) ->
	dfnode_ref := dfnode2
    | DFNode (DFMultiple lst_ref, _, _, _, _, _) ->
	lst_ref := dfnode2 :: !lst_ref
    | _ ->
	raise (Dferror "Tried to start an arc at a dfnode that should not have any successors.")

let terminate_unary dfnode dfgraph =
  match dfgraph with
    | (sources, DFNil) ->
	(sources, dfnode)
    | (sources, terminal) ->
	mkarc terminal dfnode;
	(sources, dfnode)

let terminate_many dfnode dfgraphs =
  match dfgraphs with
    | [] -> (mkdfgraph_singleton dfnode)
    | _ ->
	let terminated_dfgraphs = List.map (terminate_unary dfnode) dfgraphs in
	let (sources, terminals) = List.split terminated_dfgraphs in
	let flattened_sources = List.flatten sources in
	let merged_sources = List.fold_left merge_dfnodes flattened_sources [] in
	  (merged_sources, dfnode)
	    
let terminate_binary dfnode dfgraph1 dfgraph2 =
  terminate_many dfnode [dfgraph1; dfgraph2]

let merge_dfgraphs dfgraph1 dfgraph2 =
  let (sources1, terminal) = dfgraph1 in
  let (sources2, _) = dfgraph2 in
  let merged_sources = merge_dfnodes sources1 sources2 in
    (merged_sources, terminal)


(*******************)
(* graph iteration *)
(*******************)

(* helpers *)
let ctxt_wrapper f = (fun n ctxt -> f n; ctxt)

let nop n = ()

let touch dfnode =
  match dfnode with
    | DFNil -> true
    | DFNode (_, _, _, _, bool_ref, _) ->
	if !bool_ref then true
	else
	  begin
	    bool_ref := true;
	    false
	  end

let touch_reset dfnode =
  match dfnode with
    | DFNil -> true
    | DFNode (_, _, _, _, bool_ref, _) ->
	bool_ref := false;
	false

let rec iter_dfs_rec touch_fun f ctxt n =
  if not (touch_fun n)
  then
    let ctxt' = f n ctxt in
      match n with
	| DFNode (DFSingle (_, dfnode_ref), _, _, _, _, _) ->
	    iter_dfs_rec touch_fun f ctxt' !dfnode_ref
	| DFNode (DFMultiple lst_ref, _, _, _, _, _) ->
	    List.iter (iter_dfs_rec touch_fun f ctxt') !lst_ref
	| _ -> ()
	
let rec iter_dfs_sources touch_fun f ctxt sources =
  match sources with
    | [] -> ()
    | source :: rest ->
	let _ = iter_dfs_rec touch_fun f ctxt source in
	  iter_dfs_sources touch_fun f ctxt rest

let iter_dfs_internal touch_fun f ctxt dfgraph =
  let (sources, _) = dfgraph in
    iter_dfs_sources touch_fun f ctxt sources

let iter_dfs f ctxt dfgraph =
  iter_dfs_internal touch f ctxt dfgraph;
  iter_dfs_internal touch_reset (ctxt_wrapper nop) () dfgraph

let rec reset_mem dfnode =
  match dfnode with
    | DFNode (_, _, _, _, _, mem_ref) ->
	begin
	  match !mem_ref with
	    | Some mem ->
		mem_ref := None;
		begin
		  match dfnode with
		    | DFNode (DFSingle (_, dfnode_ref), _, _, _, _, _) ->
			reset_mem !dfnode_ref
		    | DFNode (DFMultiple lst_ref, _, _, _, _, _) ->
			List.iter reset_mem !lst_ref
		    | _ -> ()
		end
	    | None -> ()
	end
    | DFNil -> raise (Dferror "Tried to reset intermediate storage on DFNil.")  

let rec fold_left_dfs_rec f g a ctxt dfnode =
  match dfnode with
    | DFNode (_, _, _, _, _, mem_ref) ->
	begin
	  match !mem_ref with
	      
	    (* In case the result has already been calculated, skip. *)
	    | Some mem -> mem
	    | None ->
		
		(* Applying g yields a new context. *)
		let ctxt' = g dfnode ctxt in

		(* Recursively process this dfnode's children using that context. *)
		let a_lst =
		  match dfnode with
		    | DFNode (DFSingle (_, dfnode_ref), _, _, _, _, _) ->
			[fold_left_dfs_rec f g a ctxt' !dfnode_ref]
		    | DFNode (DFMultiple lst_ref, _, _, _, _, _) ->
			List.map (fold_left_dfs_rec f g a ctxt') !lst_ref
		    | _ -> [a]
		in

		(* Calculate and store the result of f for this dfnode, using the old context. *)
		let res = f a_lst dfnode ctxt in
		  mem_ref := Some res;
		  res
	end
    | DFNil -> raise (Dferror "Tried to apply folding on DFNil.")  

let fold_left_dfs f g a ctxt dfnode =
  let res = fold_left_dfs_rec f g a ctxt dfnode in
    reset_mem dfnode;
    res


(*******************)
(* graph searching *)
(*******************)

let rec find_all_rec touch_fun pred n =
  if not (touch_fun n)
  then
    let findings =
      match n with
	| DFNode (DFSingle (_, dfnode_ref), _, _, _, _, _) ->
	    find_all_rec touch_fun pred !dfnode_ref
	| DFNode (DFMultiple lst_ref, _, _, _, _, _) ->
	    List.flatten (List.map (find_all_rec touch_fun pred) !lst_ref)
	| _ -> []
    in
      if pred n then (n :: findings)
      else findings
  else []
	
let rec find_all_sources touch_fun pred sources =
  match sources with
    | [] -> []
    | source :: rest ->
	let findings_source = find_all_rec touch_fun pred source in
	  findings_source @ (find_all_sources touch_fun pred rest)

let find_all pred dfgraph =
  let (sources, _) = dfgraph in
  let findings = find_all_sources touch pred sources in
    iter_dfs_internal touch_reset (ctxt_wrapper nop) () dfgraph;
    findings


(**************)
(* dot output *)
(**************)

let string_of_dfnode_id dfnode_id =
  string_of_int dfnode_id

let dot_nodename_of_dfnode dfnode =
  match dfnode with
    | DFNil -> "nil"
    | DFNode (_, _, _, id, _, _) -> (string_of_dfnode_id id)

let dot_nodelabel_of_dfnode dfnode =
  let dot_nodename = dot_nodename_of_dfnode dfnode in
  let dot_nodelabel =
    match dfnode with
      | DFNil -> "nil"
      | DFNode (DFNone DFSerialize, _, _, _, _, _) -> "ser"
      | DFNode (DFNone DFControl, _, _, _, _, _) -> "con"
      | DFNode (DFSingle (DFOrdered i, _), _, _, _, _, _) -> "ord" ^ (string_of_int i)
      | DFNode (DFSingle (DFUnordered, _), _, _, _, _, _) -> "unord"
      | DFNode (DFSingle (DFMerge, _), _, _, _, _, _) -> "merge"
      | DFNode (DFSingle (DFImmediate,_ ), _, _, _, _, _) -> "imm"
      | DFNode (DFMultiple _, _, _, _, _, _) -> "fork"
  in
    dot_nodename ^ " [label=" ^ dot_nodelabel ^ "];\n"
	  
let print_dot_nodename ff dfnode =
  fprintf ff "%s" (dot_nodename_of_dfnode dfnode)

let print_dot_nodelabel ff dfnode =
  fprintf ff "%s" (dot_nodelabel_of_dfnode dfnode)

let cluster_num = ref 0

let print_cluster_num ff x =
  fprintf ff "%s" (string_of_int !cluster_num)

(* Hack: prints redundant information luckily ignored by dot. *)
let print_dot_clusters ff dfnode =
  let affiliates = get_affiliates dfnode in
    fprintf ff "subgraph cluster%a {\n" print_cluster_num ();
    List.iter (print_dot_nodelabel ff) affiliates;
    fprintf ff "}\n";
    cluster_num := !cluster_num + 1

let print_dot_arcs ff dfnode =
  let print_arrow () = fprintf ff " -> " in
  let print_newline () = fprintf ff ";\n" in
  let print_arc dfnode1 dfnode2 =
    print_dot_nodename ff dfnode1;
    print_arrow ();
    print_dot_nodename ff dfnode2;
    print_newline ()
  in
    begin
      match dfnode with
	| DFNil -> ()
	| DFNode (dfnode_kind, _, _, _, _, _) ->
	    begin
	      match dfnode_kind with
		| DFNone _ -> ()
		| DFSingle (_, dfnode_ref) ->
		    begin
		      match !dfnode_ref with
			| DFNil -> ()
			| _ -> print_arc dfnode !dfnode_ref
		    end
		| DFMultiple lst_ref ->
		    begin
		      match !lst_ref with
			| [] -> ()
			| _ ->List.iter (print_arc dfnode) !lst_ref
		    end
	    end
    end
	    
let print_dot_dfgraph_custom ff f dfgraph =
  let f_wrapper dfnode =
    let dfnode_kind = get_dfnode_kind dfnode in
    let dfnode_id = get_dfnode_id dfnode in
    let a = get_dfnode_value dfnode in
      f ff dfnode_kind dfnode_id a
  in

  fprintf ff "digraph G {\n";
  
  iter_dfs (ctxt_wrapper f_wrapper) () dfgraph;
  iter_dfs (ctxt_wrapper (print_dot_arcs ff)) () dfgraph;
  
  fprintf ff "}\n"

let print_dot_dfgraph ff dfgraph =
  fprintf ff "digraph G {\n";
  
  iter_dfs (ctxt_wrapper (print_dot_nodelabel ff)) () dfgraph;
  iter_dfs (ctxt_wrapper (print_dot_arcs ff)) () dfgraph;
  
  fprintf ff "}\n"

let print_dot_clustered_dfgraph ff dfgraph =
  fprintf ff "digraph G {\n";
  
  iter_dfs (ctxt_wrapper (print_dot_clusters ff)) () dfgraph;
  iter_dfs (ctxt_wrapper (print_dot_arcs ff)) () dfgraph;
  
  fprintf ff "}\n"
