(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: path_structutil.ml,v 1.20 2007/02/01 22:08:52 simeon Exp $ *)

(* Module: Path_structutil
   Description:
     This module contains basic operations on path structures.
*)

open Format

open Error

open Namespace_names
open Namespace_symbols

open Dm_types
open Dm

open Xquery_common_ast
open Xquery_common_ast_util

open Xquery_core_ast
open Xquery_core_ast_util

open Streaming_types

open Path_struct


(* Turns all subtree annotations on *)

let imposes_subtree path_seq =
  let subtree_fun (did, (path, subtree)) =
    (did,(path,Subtree))
  in
  List.map subtree_fun path_seq

(* Actions and related operations *)

type action =
  | GetSubtree
  | PreserveNode
  | SkipNode
  | KeepMovingPreserveNode of path_fragment_sequence
  | KeepMovingSkipNode of path_fragment_sequence

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

(* Navigation functions *)

let node_sym_of_sax_event sax_event =
  match sax_event.rse_desc with
  | RSAX_startDocument (sax_xml_decl,_,_) ->
      None
  | RSAX_endDocument ->
      None
  | RSAX_startElement (relem_sym,_,_,_,_) ->
      Some (RUnitElemSymbol relem_sym)
  | RSAX_endElement ->
      None
  | RSAX_processingInstruction pi_desc ->
      None
  | RSAX_comment comment_desc ->
      None
  | RSAX_characters text_desc ->
      None
  | RSAX_attribute (rattr_sym,_) ->
      Some (RUnitAttrSymbol rattr_sym)
  | RSAX_atomicValue _ ->
      raise (Query (Projection "Should not apply projection operation on an atomic value!"))
  | RSAX_hole ->
      raise (Query (Projection "Should not apply projection operation on a stream with holes!"))

let pi_sym_of_sax_event sax_event =
  match sax_event.rse_desc with
  | RSAX_startDocument _ ->
      None
  | RSAX_endDocument ->
      None
  | RSAX_startElement _ ->
      None
  | RSAX_endElement ->
      None
  | RSAX_processingInstruction (pi_name,_) ->
      Some pi_name
  | RSAX_comment _ ->
      None
  | RSAX_characters _ ->
      None
  | RSAX_attribute _ ->
      None
  | RSAX_atomicValue _ ->
      raise (Query (Projection "Should not apply projection operation on an atomic value!"))
  | RSAX_hole ->
      raise (Query (Projection "Should not apply projection operation on a stream with holes!"))

let node_kind_of_sax_event sax_event =
  match sax_event.rse_desc with
  | RSAX_startDocument _ ->
      DocumentNodeKind
  | RSAX_endDocument ->
      DocumentNodeKind
  | RSAX_startElement _ ->
      ElementNodeKind
  | RSAX_endElement ->
      ElementNodeKind
  | RSAX_processingInstruction _ ->
      ProcessingInstructionNodeKind
  | RSAX_comment _ ->
      CommentNodeKind
  | RSAX_characters _ ->
      TextNodeKind
  | RSAX_attribute _ ->
      AttributeNodeKind
  | RSAX_atomicValue _ ->
      raise (Query (Projection "Should not apply projection operation on an atomic value!"))
  | RSAX_hole ->
      raise (Query (Projection "Should not apply projection operation on a stream with holes!"))

(* Whether a node test holds on a given sax event under a particular axis or not *)

let eval_node_test a nt sax_event =
  let nodesym1 = node_sym_of_sax_event sax_event in
  let nodekind1 = node_kind_of_sax_event sax_event in
  match nt with
  | CPNameTest qn 
  | CPNodeKindTest (CElementKind (CElementTest (Some (qn,None))), _) ->
      begin
	match nodesym1 with 
	| None -> false
	| Some sym -> 
	    let qnsym =
	      begin
		match a with
		| Attribute -> RUnitAttrSymbol (rattr_symbol qn)
		| _ -> RUnitElemSymbol (relem_symbol qn)
	      end
	    in
	    let pnk = principal_node_kind a in
	    let pnk = Dm_step.node_kind_of_principal_node_kind pnk in
            pnk = nodekind1 &&
	    Namespace_symbols.subtag sym qnsym
      end
  | CPNodeKindTest (node_kind, _) ->
      begin
	match node_kind with
	| CPIKind pik ->
	    nodekind1 = ProcessingInstructionNodeKind
	      &&
	    begin
	      match pik with
	      |	None -> true
	      |	Some name ->
		  let pi_targ = pi_sym_of_sax_event sax_event in
		  begin
		    match pi_targ with
		    | None -> false
		    | Some pitargreal -> pitargreal = name
		  end
	    end
	| CCommentKind ->
	    nodekind1 = CommentNodeKind
	| CTextKind ->
	    nodekind1 = TextNodeKind
	(* all the other cases bottom to true - This may be improved
	   in the future - Jerome *)
	| CDocumentKind _ | CElementKind _ | CAttributeKind _ | CAnyKind ->
	    true
      end

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
  let start_pfs = get_start_pfs path_seq (InputDocument docid) in
  one_document_step sax_event start_pfs

let inside_variable sax_event path_seq vname =
  let start_pfs = get_start_pfs path_seq (InputVariable vname) in
  one_document_step sax_event start_pfs

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

let one_step_attribute pfl (rsym,_) =
  List.exists (one_step_attribute_check rsym) pfl
    

(* -----------------------------------------------------------------------------
   Print functions
----------------------------------------------------------------------------- *)

let string_node n =
  match n with
  | CPNameTest name ->
      prefixed_string_of_rqname name
  | CPNodeKindTest (ckind_test, cty) ->
      Print_top.bprintf_cnode_kind "" ckind_test

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
	  | Following ->
	      "/following::"
	  | Preceding ->
	      "/preceding::"
	  | Parent ->
	      "/parent::"
	  | Self ->
	      "/self::"
	  in axis_string ^ string_node node ^ string_path p_rest
  in path_string
 
let rec string_path_sequence ps =
  match ps with
  | [] -> ""
  | (InputDocument docid,(path,subtree))::ps_rest ->
      let string_subtree subtree =
	match subtree with
	| Subtree -> "# \n"
	| NoSubtree ->  "\n"
      in ("document(" ^ docid ^ ")" ^ string_path path ^ string_subtree subtree ^ string_path_sequence ps_rest)
  | (InputVariable vname,(path,subtree))::ps_rest ->
      let string_subtree subtree =
	match subtree with
	| Subtree -> "# \n"
	| NoSubtree ->  "\n"
      in ("$" ^ (prefixed_string_of_rqname vname) ^ "" ^ string_path path ^ string_subtree subtree ^ string_path_sequence ps_rest)


let print_path_sequence ff ps = 
  let string_ps = string_path_sequence ps in
  fprintf ff "%s" string_ps

