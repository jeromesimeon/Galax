(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: alg_path_structutil.ml,v 1.7 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Alg_path_structutil
   Description:
     Utilities for path analysis over the XQuery algebra.
*)

open Format

open Error

open Namespace_names
open Namespace_symbols

open Dm_types
open Dm

open Xquery_common_ast
open Xquery_common_ast_util

open Xquery_algebra_ast

open Streaming_types

open Ast_path_struct

open Xquery_algebra_ast_util

type action =
  | GetSubtree
  | PreserveNode
  | SkipNode
  | KeepMovingPreserveNode of path_fragment_sequence
  | KeepMovingSkipNode of path_fragment_sequence


let mk_rooted_path_sequence_constructor i =
  [(Constructor_id i, ([], NoSubtree))]

let mk_rooted_path_sequence_document str =
  [(Document_id str, ([], NoSubtree))]


(* Turns all subtree annotations on *)

let imposes_subtree path_seq =
  let subtree_fun (did, (path, subtree)) =
    (did,(path,Subtree))
  in
    List.map subtree_fun path_seq


(* -----------------------------------------------------------------------------
   Print functions
----------------------------------------------------------------------------- *)

let string_node n =
  Print_xquery_algebra.bprintf_anode_test "" n
(*
  match n with
  | APNameTest anon_symbol ->
      anon_prefix_string anon_symbol
  | APNodeKindTest akind_test ->
      raise (Query (Prototype "APNodeKindTest not supported."))
*)

let rec string_path p = 
  let path_string =
    match p with
    | [] -> " "
    | (axis,node)::p_rest ->
	let axis_string =
	  match axis with
	  | Ancestor ->
	      "/ancestor::"
	  | Ancestor_or_self ->
	      "/ancestor-or-self::"
	  | Attribute ->
	      "/attribute::"
	  | Child ->
	      "/"
	  | Descendant ->
	      "//"
	  | Descendant_or_self ->
	      "/descendant-or-self::"
	  | Following_sibling ->
	      "/following-sibilng::"
	  | Preceding_sibling ->
	      "/preceding-sibling::"
	  | Parent ->
	      "/parent::"
	  | Self ->
	      "/self::"
	  | Following ->
	      "/following::"
	  | Preceding ->
	      "/preceding::"
	  in axis_string ^ string_node node ^ string_path p_rest
  in path_string
 
let rec string_path_sequence ps =
  let string_subtree subtree =
    match subtree with
      | Subtree -> "# \n"
      | NoSubtree ->  "\n"
  in
    match ps with
      | [] -> ""
      | (Document_id docid,(path,subtree))::ps_rest ->
	  ("document(" ^ docid ^ ")" ^ string_path path ^ string_subtree subtree ^ string_path_sequence ps_rest)
      | (Constructor_id i,(path,subtree))::ps_rest ->
	  ("constructor(" ^ (string_of_int i) ^ ")" ^ string_path path ^ string_subtree subtree ^ string_path_sequence ps_rest)
	    
let print_path_sequence ff ps = 
  let string_ps = string_path_sequence ps in
  fprintf ff "%s" string_ps


(***************************************)
(* BEGIN - Copied from Streaming_xpath *)
(***************************************)

let raise_unexpected_event_error () = raise (Query (Projection "Unexpected event type encountered."))

let relem_symbol_of_sax_event sax_event =
  match sax_event.se_desc with
  | SAX_startElement (_,_,_,_,eo,_) ->
      let relem_sym =
	match !eo with
	| None -> raise_unexpected_event_error ()
	| Some (rel,_,_) -> rel
      in
      relem_sym
  | _ ->
      raise_unexpected_event_error ()

let relem_symbol_of_sax_event_and_type sax_event =
  match sax_event.se_desc with
  | SAX_startElement (_,_,_,_,eo,et) ->
      let relem_sym =
	match !eo with
	| None -> raise_unexpected_event_error ()
	| Some (rel,_,_) -> rel
      in
      let type_annot =
	match !et with
	| None -> raise_unexpected_event_error ()
	| Some (_,t,_) -> t
      in
      (relem_sym,type_annot)
  | _ ->
      raise_unexpected_event_error ()

let rattr_symbol_of_sax_event sax_event =
  match sax_event.se_desc with
  | SAX_attribute (_,_,s,ao,_) ->
      let rattr_sym =
	match !ao with
	| None -> raise_unexpected_event_error ()
	| Some rat -> rat
      in
      rattr_sym
  | _ ->
      raise_unexpected_event_error ()

let rattr_symbol_of_sax_event_and_type sax_event =
  match sax_event.se_desc with
  | SAX_attribute (_,_,s,ao,at) ->
      let rattr_sym =
	match !ao with
	| None -> raise_unexpected_event_error ()
	| Some rat -> rat
      in
      let type_annotation =
	match !at with
	| None -> raise_unexpected_event_error ()
	| Some (t,_) -> t
      in
      (rattr_sym,type_annotation)
  | _ ->
      raise_unexpected_event_error ()

let pi_ncname_of_sax_event sax_event =
  match sax_event.se_desc with
  | SAX_processingInstruction (pi_name,_) ->
      pi_name
  | _ ->
      raise_unexpected_event_error ()

let node_kind_of_sax_event sax_event =
  match sax_event.se_desc with
  | SAX_startDocument _
  | SAX_endDocument ->
      DocumentNodeKind
  | SAX_startElement _
  | SAX_endElement ->
      ElementNodeKind
  | SAX_processingInstruction _ ->
      ProcessingInstructionNodeKind
  | SAX_comment _ ->
      CommentNodeKind
  | SAX_characters _ ->
      TextNodeKind
  | SAX_attribute _ ->
      AttributeNodeKind
  | SAX_atomicValue _
  | SAX_startEncl
  | SAX_endEncl
  | SAX_hole ->
      raise_unexpected_event_error ()

open Code_util_matching

let access_ops_sax =
  { get_node_kind      		 = node_kind_of_sax_event;
    get_elem_node_name 		 = relem_symbol_of_sax_event;
    get_attr_node_name 		 = rattr_symbol_of_sax_event;
    get_elem_node_name_with_type = relem_symbol_of_sax_event_and_type;
    get_attr_node_name_with_type = rattr_symbol_of_sax_event_and_type;
    get_single_element_node      = (fun x -> raise (Query (Projection "Cannot stream document tests with an element test")));
    get_document_node_children   = (fun x -> raise (Query (Projection "Document node children acces not required for streaming")));
    get_element_node_children    = (fun x -> raise (Query (Projection "Document node children acces not required for streaming")));
    get_pi_target                = pi_ncname_of_sax_event }

(* Performs the actual node test. *) 
let eval_node_test (*stat_ctxt*) axis anode_test ts =
  (*let cxschema = Typing_context.schema_from_static_context stat_ctxt in*)
  Dm_step.eval_node_test_gen access_ops_sax (*(Some cxschema)*) None axis anode_test ts

(*************************************)
(* END - Copied from Streaming_xpath *)
(*************************************)


let merge_actions a1 a2 =
  match (a1,a2) with
  | (GetSubtree,_) -> GetSubtree
  | (_,GetSubtree) -> GetSubtree

  | (KeepMovingSkipNode p1,KeepMovingSkipNode p2)     	  -> KeepMovingSkipNode (p1@p2)
  | (KeepMovingSkipNode p1,KeepMovingPreserveNode p2) 	  -> KeepMovingPreserveNode (p1@p2)
  | (KeepMovingPreserveNode p1,KeepMovingSkipNode p2) 	  -> KeepMovingPreserveNode (p1@p2)
  | (KeepMovingPreserveNode p1,KeepMovingPreserveNode p2) -> KeepMovingPreserveNode (p1@p2)

  | (KeepMovingSkipNode p1, PreserveNode)     -> KeepMovingPreserveNode p1
  | (PreserveNode, KeepMovingPreserveNode p2) -> KeepMovingPreserveNode p2
  | (KeepMovingPreserveNode p1, PreserveNode) -> KeepMovingPreserveNode p1
  | (PreserveNode, KeepMovingSkipNode p2)     -> KeepMovingPreserveNode p2
  | (PreserveNode,PreserveNode)               -> PreserveNode
  
  | (KeepMovingSkipNode p1, SkipNode) 	  -> KeepMovingSkipNode p1
  | (SkipNode, KeepMovingSkipNode p2) 	  -> KeepMovingSkipNode p2
  | (SkipNode, KeepMovingPreserveNode p2) -> KeepMovingPreserveNode p2
  | (KeepMovingPreserveNode p1, SkipNode) -> KeepMovingPreserveNode p1

  | (PreserveNode,SkipNode) -> PreserveNode
  | (SkipNode,PreserveNode) -> PreserveNode
  
  | (SkipNode,SkipNode) -> SkipNode


(* Actual movement in the path structure *)

let rec one_step_one_path sax_event (path, subtree) =
  match path with
  | [] -> (SkipNode,[])
  | (axis1,nt1) :: [] ->
      begin
	match axis1 with
	| Ancestor ->
	    raise (Query (Prototype "Cannot perform projection with ancestor axis involved"))
	| Ancestor_or_self ->
	    raise (Query (Prototype "Cannot perform projection with ancestor-or-self axis involved"))
	| Following_sibling ->
	    raise (Query (Prototype "Cannot perform projection with following-sibling axis involved"))
	| Preceding_sibling ->
	    raise (Query (Prototype "Cannot perform projection with preceding-sibling axis involved"))

	| Preceding -> raise (Query (Prototype "Cannot perform projection with preceding axis involved"))
	| Following -> raise (Query (Prototype "Cannot perform projection with following axis involved"))


	| Attribute ->
	    (* Don't care about attributes in the projection *)
	    (SkipNode,[])
	| Child ->
	    begin
	      if eval_node_test axis1 nt1 sax_event
	      then
		match subtree with
		| Subtree -> (GetSubtree,[])
		| NoSubtree -> (PreserveNode,[])
	      else
		(SkipNode,[])
	    end
	| Descendant ->
	    begin
	      if eval_node_test axis1 nt1 sax_event
	      then
		match subtree with
		| Subtree -> (GetSubtree,[])
		| NoSubtree -> (PreserveNode,[([(Descendant,nt1)],subtree)])
	      else
		(SkipNode,[([(Descendant,nt1)],subtree)])
	    end
	| Descendant_or_self ->
	    begin
	      if eval_node_test axis1 nt1 sax_event
	      then
		match subtree with
		| Subtree -> (GetSubtree,[])
		| NoSubtree -> (PreserveNode,[([(Descendant,nt1)],subtree)])
	      else
		(SkipNode,[([(Descendant,nt1)],subtree)])
	    end
	| Parent ->
	    raise (Query (Prototype "Cannot perform projection with parent axis involved"))
	| Self ->
	    (SkipNode,[])
      end
  | (axis1,nt1) :: path' ->
      begin
	match axis1 with
	| Ancestor ->
	    raise (Query (Prototype "Cannot perform projection with ancestor axis involved"))
	| Ancestor_or_self ->
	    raise (Query (Prototype "Cannot perform projection with ancestor-or-self axis involved"))
	| Following_sibling ->
	    raise (Query (Prototype "Cannot perform projection with following-sibling axis involved"))
	| Preceding_sibling ->
	    raise (Query (Prototype "Cannot perform projection with preceding-sibling axis involved"))
	| Preceding -> raise (Query (Prototype "Cannot perform projection with preceding axis involved"))
	| Following -> raise (Query (Prototype "Cannot perform projection with following axis involved"))

	| Attribute ->
	    (* Don't care about attributes in the projection *)
	    (SkipNode,[])
	| Child ->
	    begin
	      if eval_node_test axis1 nt1 sax_event
	      then
		(KeepMovingSkipNode [(path',subtree)],[])
	      else
		(SkipNode,[])
	    end
	| Descendant ->
	    begin
	      if eval_node_test axis1 nt1 sax_event
	      then
		(KeepMovingSkipNode [(path',subtree)],[((Descendant,nt1)::path',subtree)])
	      else
		(SkipNode,[((Descendant,nt1)::path',subtree)])
	    end
	| Descendant_or_self ->
	    begin
	      if eval_node_test axis1 nt1 sax_event
	      then
		(KeepMovingSkipNode [(path',subtree)],[((Descendant,nt1)::path',subtree)])
	      else
		(SkipNode,[((Descendant,nt1)::path',subtree)])
	    end
	| Parent ->
	    raise (Query (Prototype "Cannot perform projection with parent axis involved"))
	| Self ->
	    (SkipNode,[])
      end

let rec one_step_remove_self sax_event (path, subtree) previous_action plus1 =
  match path with
  | [] -> (previous_action,plus1)
  | (axis1,nt1) :: [] ->
      begin
	match axis1 with
	| Ancestor ->
	    (previous_action,plus1)
	| Ancestor_or_self ->
	    (previous_action,plus1)
	| Following_sibling ->
	    raise (Query (Prototype "Cannot perform projection with following-sibling axis involved"))
	| Preceding_sibling ->
	    raise (Query (Prototype "Cannot perform projection with preceding-sibling axis involved"))
	| Preceding -> raise (Query (Prototype "Cannot perform projection with preceding axis involved"))
	| Following -> raise (Query (Prototype "Cannot perform projection with following axis involved"))

	| Attribute ->
	    (previous_action,plus1)
	| Child ->
	    (previous_action,plus1)
	| Descendant ->
	    (previous_action,plus1)
	| Descendant_or_self ->
	    begin
	      let (new_action,plus) =
		if eval_node_test axis1 nt1 sax_event
		then
		  match subtree with
		  | Subtree -> (GetSubtree,plus1)
		  | NoSubtree -> (PreserveNode, [([(Descendant,nt1)],subtree)])
		else
		  (SkipNode, [([(Descendant,nt1)],subtree)])
	      in
	      (new_action,plus1@plus)
	    end
	| Parent ->
	    (previous_action,plus1)
	| Self ->
	    begin
	      let new_action =
		if eval_node_test axis1 nt1 sax_event
		then
		  match subtree with
		  | Subtree -> GetSubtree
		  | NoSubtree -> PreserveNode
		else
		  SkipNode
	      in
	      (new_action, plus1)
	    end
      end
  | (axis1,nt1) :: path' ->
      begin
	match axis1 with
	| Ancestor ->
	    (previous_action,plus1)
	| Ancestor_or_self ->
	    (previous_action,plus1)
	| Following_sibling ->
	    raise (Query (Prototype "Cannot perform projection with following-sibling axis involved"))
	| Preceding_sibling ->
	    raise (Query (Prototype "Cannot perform projection with preceding-sibling axis involved"))
	| Preceding -> raise (Query (Prototype "Cannot perform projection with preceding axis involved"))
	| Following -> raise (Query (Prototype "Cannot perform projection with following axis involved"))

	| Attribute ->
	    (previous_action,plus1)
	| Child ->
	    (previous_action,plus1)
	| Descendant ->
	    (previous_action,plus1)
	| Descendant_or_self ->
	    begin
	      let (new_action,plus) =
		if eval_node_test axis1 nt1 sax_event
		then
		  

		  (KeepMovingSkipNode [(path',subtree)], [((Descendant,nt1)::path',subtree)])
		else
		  (SkipNode, [((Descendant,nt1)::path',subtree)])
	      in
	      one_step_remove_self sax_event (path', subtree) new_action (plus1@plus)
	    end
	| Parent ->
	    (previous_action,plus1)
	| Self ->
	    begin
	      let new_action =
		if eval_node_test axis1 nt1 sax_event
		then
		  KeepMovingSkipNode [(path',subtree)]
		else
		  SkipNode
	      in
	      one_step_remove_self sax_event (path', subtree) new_action plus1
	    end
      end

let compose_steps sax_event (path, subtree) =
  let (action1,plus1) = one_step_one_path sax_event (path, subtree) in
  match action1 with
  | GetSubtree -> (action1,plus1)
  | PreserveNode -> (action1,plus1)
  | SkipNode -> (action1,plus1)
  | KeepMovingPreserveNode path_fragment_sequence ->
      let (npath,nsubtree) = List.nth path_fragment_sequence 0 in
      one_step_remove_self sax_event (npath,nsubtree) action1 plus1
  | KeepMovingSkipNode path_fragment_sequence ->
      let (npath,nsubtree) = List.nth path_fragment_sequence 0 in
      one_step_remove_self sax_event (npath,nsubtree) action1 plus1

let one_step sax_event (path_fragment_seq : path_fragment_sequence) =
  let stage1 = List.map (compose_steps sax_event) path_fragment_seq in
  let (stage1_actions, stage1_plus) = List.split stage1 in
  let stage2_actions = List.fold_left merge_actions SkipNode stage1_actions in
  let stage3_actions =
    let all_add_paths = List.concat stage1_plus in
    match all_add_paths with
    | [] -> SkipNode
    | _ -> KeepMovingSkipNode all_add_paths
  in
  merge_actions stage2_actions stage3_actions

(* attributes *)

let rec one_step_document_remove_self sax_event (path, subtree) =
  match path with
  | [] -> [(path, subtree)]
  | (axis1,nt1) :: path' ->
      begin
	match axis1 with
	| Ancestor ->
	    [(path, subtree)]
	| Ancestor_or_self ->
	    [(path, subtree)]
	| Following_sibling ->
	    raise (Query (Prototype "Cannot perform projection with following-sibling axis involved"))
	| Preceding_sibling ->
	    raise (Query (Prototype "Cannot perform projection with preceding-sibling axis involved"))
	| Preceding -> raise (Query (Prototype "Cannot perform projection with preceding axis involved"))
	| Following -> raise (Query (Prototype "Cannot perform projection with following axis involved"))
	| Attribute ->
	    [(path, subtree)]
	| Child ->
	    [(path, subtree)]
	| Descendant ->
	    [(path, subtree)]
	| Descendant_or_self ->
	    begin
	      if eval_node_test axis1 nt1 sax_event
	      then
		(one_step_document_remove_self sax_event (path', subtree)) @ [((Descendant,nt1)::path',subtree)]
	      else
		[((Descendant,nt1)::path',subtree)]
	    end
	| Parent ->
	    [(path, subtree)]
	| Self ->
	    begin
	      if eval_node_test axis1 nt1 sax_event
	      then
		(one_step_document_remove_self sax_event (path', subtree))
	      else
		[]
	    end
      end

let compose_document_steps sax_event (path, subtree) =
  one_step_document_remove_self sax_event (path,subtree)

let one_document_step sax_event (path_fragment_seq : path_fragment_sequence) =
  List.concat (List.map (compose_document_steps sax_event) path_fragment_seq)

let get_start_pfs path_seq docid =
  let rec select_path path_seq =
    match path_seq with
    | [] -> []
    | (docid1, (path1, subtree1)) :: path_seq' ->
	if (docid1 = docid)
	then
	  (path1, subtree1) :: (select_path path_seq')
	else
	  (select_path path_seq')
  in
  select_path path_seq

let inside_document sax_event path_seq docid =
  let start_pfs = get_start_pfs path_seq (Document_id docid) in
  one_document_step sax_event start_pfs

(*
let inside_variable sax_event path_seq vname =
  let start_pfs = get_start_pfs path_seq (InputVariable vname) in
  one_document_step sax_event start_pfs
*)

(* attributes *)

let one_step_attribute_check rsym (path, subtree) =
  match path with
  | [] -> false
  | (axis1,nt1) :: _ ->
      begin
      	match axis1 with
	| Attribute ->
	    true
	| _ ->
	    false
      end

let one_step_attribute pfl (_,_,s,ao,_) =
  let rsym =
    match !ao with
    | None -> raise_unexpected_event_error ()
    | Some rat -> rat
  in
  List.exists (one_step_attribute_check rsym) pfl


let paths_from_path_annotation msg p_annot =
  match !p_annot with
    | None -> 
        raise (Query
                  (Internal_Error (msg^": Attempt to retrieve path annotation,"
				                    ^ " but no path annotation set")))
    | Some record ->
        match record.path_analysis with
          | None ->
              raise (Query
                        (Internal_Error (msg^": Attempt to retrieve path analysis results,"
				                          ^ " but no path analysis recorded")))
          | Some panlz -> panlz


let rec path_equal p1 p2 = match (p1,p2) with
  | [],[] -> true
  | _,[] -> false
  | [],_ -> false
  | (ax1,t1)::r1, (ax2,t2)::r2 -> 
      if (ax1 = ax2) && (anode_test_equal t1 t2) then
        path_equal r1 r2
      else
        false
          
let path_fragment_equal (p1,t1) (p2,t2) = 
  (t1 = t2) &&  (path_equal p1 p2) 
  
let rooted_path_equal (id1,pf1) (id2,pf2) = 
  id1 = id2 && (path_fragment_equal pf1 pf2)

let rec rooted_path_sequence_equal rps1 rps2 = match (rps1, rps2) with
  | [], [] -> true
  | [], _ -> false
  | _, [] -> false
  | rp1::seq1, rp2::seq2 ->
      if not (rooted_path_equal rp1 rp2) then
        false
      else
        rooted_path_sequence_equal seq1 seq2


let rec mem_rooted_path_sequence rp rpseq = match rpseq with 
  | [] -> false
  | p::rest -> 
      if (rooted_path_equal rp p) then true
      else mem_rooted_path_sequence rp rest

let rec disjoint_rooted_path_sequence seq1 seq2 = match seq1 with
  | [] -> true
  | p::rest1 -> 
      if mem_rooted_path_sequence p seq2 then false
      else disjoint_rooted_path_sequence rest1 seq2

let root_of_path_is_in_path_sequence ((id1,_) as rp) rpseq = match rpseq with
  | [] -> false
  | (id2,_)::rest -> 
      if id1 = id2 then true
      else mem_rooted_path_sequence rp rest

let rec path_sequences_with_disjoint_roots seq1 seq2 = match seq1 with
  | [] -> true
  | p::rest1 -> 
      if root_of_path_is_in_path_sequence p seq2 then false
      else path_sequences_with_disjoint_roots rest1 seq2


        
