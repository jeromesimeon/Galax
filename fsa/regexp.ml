(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: regexp.ml,v 1.10 2007/02/01 22:08:46 simeon Exp $ *)

(* Module Regexp
   Description:
     This module implements basic regular expressions over an alphabet
     of letters, and the Glushkov construction for both NFA's and
     DFA's over the same alphabet. The Glushkov module is a functor
     parameterized by a letter types.
*)

open Occurrence
open Error
open Nfa
open Dfa

(***********************)
(* Regular expressions *)
(***********************)

module type RegexpType =
  sig
    (* Positions within a regular expression *)
    type position = int    (* a symbol is denoted with its position
			      - each symbol as a unique position *)

    module Position : Set.OrderedType with type t = position

    module PositionSet : Set.S with type elt = position

    type letter

    (* Regular expressions *)

    type regexp = 
      | RSym of (letter * position)
      | RSeq of (regexp * regexp) 
      | REmpty
      | RChoice of (regexp * regexp) 
      | RNone
      | RPlus of regexp
      | RStar of regexp
      | RAmp of (regexp * regexp)

    (* helper function on Glushkov construction *)

    val nullable : regexp -> bool

    (* returns a set of position of the expression *)

    val pos : regexp -> PositionSet.t

    val first : regexp -> PositionSet.t
    val last : regexp -> PositionSet.t
    val follow : regexp -> position -> PositionSet.t

    (* the chi function: it takes a regular expression and a position (integer). *)
    (* returns the label at that position. *)

    val chi : regexp -> position -> letter

    (* Creates a regular expression in star normal form *)
    (* it's a two step process *)

    val star_normalize_aux : regexp -> regexp
    val star_normalize : regexp -> regexp

  end

module MakeRegexpType (L : Set.OrderedType) : RegexpType with type letter = L.t =
  struct
    type position = int    (* a symbol is denoted with its position
			      - each symbol as a unique position *)

    module Position =
      struct
	type t = position
	let compare = compare
      end

    module PositionSet = Set.Make(Position)

    type letter = L.t

    (* Regular expressions *)

    type regexp = 
      | RSym of (letter * position)
      | RSeq of (regexp * regexp) 
      | REmpty
      | RChoice of (regexp * regexp) 
      | RNone
      | RPlus of regexp
      | RStar of regexp
      | RAmp of (regexp * regexp)

    (***********************)
    (* Auxiliary functions *)
    (***********************)

    (* Note: these are used for the Glushkov construction *)
	    
    (* Is () in the language defined by the regular expression ? *)

    let rec nullable e = 
      match e with
      | REmpty ->
	  true
      | RNone ->
	  false
      | RSym _ ->
	  false
      | RChoice (f,g) ->
	  ((nullable f) || (nullable g))
      | RSeq (f,g) ->
	  ((nullable f) && (nullable g))
      | RPlus f ->
	  nullable f
      | RStar f ->
	  true
      | RAmp (f,g) ->
	  ((nullable f) && (nullable g))

    (* Returns the set of position in a regular expression *)

    let rec pos e =
      match e with
      | RNone ->
	  PositionSet.empty
      | REmpty ->
	  PositionSet.empty
      | RSym (_,p) ->
	  PositionSet.singleton p
      | RChoice (f, g) ->
	  PositionSet.union (pos f) (pos g)
      | RSeq (f, g) ->
	  PositionSet.union (pos f) (pos g)
      | RPlus f ->
	  pos f
      | RStar f ->
	  pos f
      | RAmp (f, g) ->
	  PositionSet.union (pos f) (pos g)

    (* val first : reg_expr -> PosSet *)

    let rec first e = 
      match e with
      | RNone -> PositionSet.empty
      | REmpty -> PositionSet.empty
      | RSym (_,p) -> PositionSet.singleton p
      | RChoice (f, g) -> PositionSet.union (first f) (first g)
      | RSeq (f, g) ->
	  begin
	    match (nullable f) with
	    | true -> PositionSet.union (first f) (first g)
	    | _ -> first f
	  end
      | RPlus f -> first f
      | RStar f -> first f
      | RAmp (f, g) -> PositionSet.union (first f) (first g)

    let rec last e =
      match e with
      | RNone -> PositionSet.empty
      | REmpty -> PositionSet.empty
      | RSym (_,p) -> PositionSet.singleton p
      | RChoice (f, g) -> PositionSet.union (last f) (last g)
      | RSeq (f, g) -> 
	  begin
	    match (nullable g) with
	    | true -> PositionSet.union (last f) (last g)
	    | _ -> last g
	  end 
      | RPlus f -> last f
      | RStar f -> last f
      | RAmp (f, g) ->
	  PositionSet.union (last f) (last g)

    let rec follow e (x: int) (* position of the expression *) = 
      match e with
      | RNone -> PositionSet.empty (* expression has no position *)
      | REmpty -> PositionSet.empty (* expression has no position *)
      | RSym (_,p) -> if (x = p) then PositionSet.empty else raise (Query (Internal_Error "Follow: invalid position input"))
      | RChoice (f, g) -> 
	  if (PositionSet.mem x (pos f))
	  then 
	    follow f x
	  else if (PositionSet.mem x (pos g))
	  then 
	    follow g x
	  else
	    raise (Query (Internal_Error "Follow: Invalid position input"))
      | RSeq (f, g) ->
	  let posF = pos f
	  and posG = pos g
	  and lastF = last f in
	  if ((PositionSet.mem x posF) && (not(PositionSet.mem x lastF)))
	  then 
	    follow f x
	  else if (PositionSet.mem x (lastF))
	  then 
	    PositionSet.union (follow f x) (first g)
	  else if (PositionSet.mem x posG)
	  then
	    follow g x
	  else
	    raise (Query (Internal_Error "Follow: Invalid position input"))
      | RPlus f ->
	  let posF = pos f
	  and lastF = last f in
	  if ((PositionSet.mem x posF) && (not(PositionSet.mem x lastF)))
	  then 
	    follow f x
	  else if (PositionSet.mem x (lastF))
	  then 
	    PositionSet.union (follow f x) (first f)
	  else
	    raise (Query (Internal_Error "Follow: Invalid position input"))
      | RStar f ->
	  let posF = pos f
	  and lastF = last f in
	  if ((PositionSet.mem x posF) && (not(PositionSet.mem x lastF)))
	  then 
	    follow f x
	  else if (PositionSet.mem x (lastF))
	  then 
	    PositionSet.union (follow f x) (first f)
	  else
	    raise (Query (Internal_Error "Follow: Invalid position input"))
      | _ ->
	  raise (Query (Prototype "All groups not supported yet"))

    (* the chi function: it takes a regular expression and a position (integer). *)
    (* returns the label at that position. *)

    let rec chi r y =
      match r with
      | RNone -> raise (Query (Internal_Error "Chi: Invalid position input"))
      | REmpty -> raise (Query (Internal_Error "Chi: Invalid position input"))
      | RSym (a, p) -> 
	  if (p = y)
	  then
	    a
	  else
	    raise (Query (Internal_Error "Chi: Invalid position input"))
      | RChoice (f, g) -> 
	  begin
	    try (chi f y) with 
	    | (Query Internal_Error _) -> 
		chi g y
	  end
      | RSeq (f, g) -> 
	  begin
	    try (chi f y) with 
	    | (Query Internal_Error _) -> 
		chi g y
	  end
      | RPlus f -> chi f y
      | RStar f -> chi f y
      | RAmp (f, g) -> 
	  begin
	    try (chi f y) with 
	    | (Query Internal_Error _) -> 
		chi g y
	  end

    (* Creates a regular expression in star normal form *)
    (* it's a two step process *)

    let rec star_normalize_aux e = 
      match e with
      | RNone -> RNone
      | REmpty -> REmpty
      | RSym _ -> e
      | RChoice (f, g) -> RChoice (star_normalize_aux f, star_normalize_aux g)
      | RSeq (f, g) ->
	  begin
	    match (nullable f, nullable g) with
	    | (false,false) -> RSeq (f,g)
	    | (false,true) -> RSeq (star_normalize_aux f,g)
	    | (true,false) -> RSeq (f,star_normalize_aux g)
	    | (true,true) -> RChoice (star_normalize_aux f, star_normalize_aux g) (* (!) *)
	  end
      | RPlus f ->
	  if (nullable f) then
	    star_normalize_aux (RStar f)
	  else
	    RPlus (star_normalize_aux f)
      | RStar f ->
	  star_normalize_aux f
      | _ ->
	  raise (Query (Prototype "All groups not supported yet"))

    let rec star_normalize e =
      match e with
      | RSym s -> RSym s
      | RSeq (f,g) -> RSeq (star_normalize f, star_normalize g)
      | REmpty -> REmpty
      | RChoice (f,g) -> RChoice (star_normalize f, star_normalize g)
      | RNone -> RNone 
      | RPlus f -> 
	  if (nullable f) then star_normalize (RStar f)
	  else RPlus (star_normalize f)
      | RStar f -> RStar (star_normalize (star_normalize_aux f))
      | _ ->
	  raise (Query (Prototype "All groups not supported yet"))

  end

module type GlushkovType =
  sig
    module Regexp : RegexpType
    module NFA : NFA
    module DFA : DFA

    module NFAPair : NFA

    val build_glushkov : Regexp.regexp -> NFA.nfa
    val build_dtm_glushkov : Regexp.regexp -> DFA.dfa

    val negates_in_alphabet_space : DFA.dfa -> DFA.Alphabet.t -> DFA.dfa

    val intersects : NFA.nfa -> NFA.nfa -> NFAPair.nfa
    val intersects_dtm : NFA.nfa -> DFA.dfa -> NFAPair.nfa
    val print_intersection :
	(NFAPair.Alphabet.elt -> unit) ->
	  (NFAPair.StateSet.elt -> unit) -> NFAPair.nfa -> unit
  end

module MakeGlushkovType (Letter : Set.OrderedType) =
  struct
    module Regexp = MakeRegexpType(Letter)

    module NFA = MakeNFA(Regexp.Position)(Letter)
    module DFA = MakeDFA(Regexp.Position)(Letter)

    module PairState = Fsautil.MakeOrderedSetPair(Regexp.Position)
    module NFAPair = MakeNFA(PairState)(Letter)

(*************************************)
(* Construction of Glushkov automata *)
(*************************************)

    (* Assumption: each symbol in a regular expression is assigned to a unique position *)
    let build_glushkov e = 
      let me = NFA.fresh_nfa_empty () in  (* me: the Glushkov automata to be constructed *)
      let e' = Regexp.star_normalize e in 
      let poss = Regexp.pos e' in                (* poss is all the positions in the expression e'*)
      
      let q1 = try (Regexp.PositionSet.min_elt poss) - 1 with | Not_found -> 0 in (* start state: set_of_q1: {q1} *)
      let _ = q1 - 1 in
      
      (* add states *)
      Regexp.PositionSet.iter
	(NFA.add_state me) 
	(Regexp.PositionSet.add q1 poss);
      
      (* add start states *)
      NFA.set_start_state me q1;
  
      (* add transitions here *)
  (* delta_E(si) = first (E) (from the paper) *)
      let first_poss_list = Regexp.PositionSet.elements (Regexp.first e') in
      let first_symbols_list = List.map (Regexp.chi e') first_poss_list in
      NFA.add_transitions me q1 (List.combine first_symbols_list first_poss_list);
      
      (* delta_E(x,a) = {y | y isMemberOf follow(E,x), chi(y) = a}, for x isMemberOf pos(E), a isMemberOf Alphabet. *)  
      let pos_list = Regexp.PositionSet.elements (poss) in           (* for all pos *)
      for i=0 to ((List.length pos_list) - 1) do              (* iterates *)
	let x = List.nth pos_list i in
	let follow_list = Regexp.PositionSet.elements (Regexp.follow e' x) in       (* convert follow(E,x) to a list *)
	let follow_letter_list = List.map (Regexp.chi e') follow_list in      (* getting the corresponding a's *)
	let aux automata start (letter, dest) =                     (* helper function *)
	  NFA.add_transitions automata start [(letter, dest)]
	in
	List.iter (aux me x) (List.combine follow_letter_list follow_list)
      done;
      
      (* add final states here *)
      let fin = if (Regexp.nullable e')           (* constructing the final states *)
      then 
	Regexp.PositionSet.add q1 (Regexp.last e')
      else 
	Regexp.last e'
      in
      let all_final_states = Regexp.PositionSet.elements fin in
      List.iter (NFA.add_final_state me) all_final_states;
      me

    let build_dtm_glushkov e = 
      let me = DFA.fresh_dfa_empty () in  (* me: the Glushkov automata to be constructed *)
      let e' = Regexp.star_normalize e in 
      let poss = Regexp.pos e' in                (* poss is all the positions in the expression e'*)

      let q1 = try (Regexp.PositionSet.min_elt poss) - 1 with | Not_found -> 0 in (* start state: set_of_q1: {q1} *)
      let _ = q1 - 1 in

      (* add states *)
      Regexp.PositionSet.iter
	(DFA.add_state me) 
	(Regexp.PositionSet.add q1 poss);

      (* add start states *)
      DFA.set_start_state me q1;
  
      (* add transitions here *)
      (* delta_E(si) = first (E) (from the paper) *)
      let first_poss_list = Regexp.PositionSet.elements (Regexp.first e') in
      let first_symbols_list = List.map (Regexp.chi e') first_poss_list in
      DFA.add_transitions me q1 (List.combine first_symbols_list first_poss_list);
  
      (* delta_E(x,a) = {y | y isMemberOf follow(E,x), chi(y) = a}, for x isMemberOf pos(E), a isMemberOf Alphabet. *)  
      let pos_list = Regexp.PositionSet.elements (poss) in           (* for all pos *)
      for i=0 to ((List.length pos_list) - 1) do              (* iterates *)
	let x = List.nth pos_list i in
	let follow_list = Regexp.PositionSet.elements (Regexp.follow e' x) in       (* convert follow(E,x) to a list *)
	let follow_letter_list = List.map (Regexp.chi e') follow_list in      (* getting the corresponding a's *)
	let aux automata start (letter, dest) =                     (* helper function *)
	  DFA.add_transitions automata start [(letter, dest)]
	in
	List.iter (aux me x) (List.combine follow_letter_list follow_list)
      done;

      (* add final states here *)
      let fin = if (Regexp.nullable e')           (* constructing the final states *)
      then 
	Regexp.PositionSet.add q1 (Regexp.last e') 
      else 
	Regexp.last e'
      in
      let all_final_states = Regexp.PositionSet.elements fin in
      List.iter (DFA.add_final_state me) all_final_states;
      me

    (* operations on dfa *)

    (* this negation function only works on deterministic regular expressions *)
    (* should have a clone function *)
    let negates_in_alphabet_space input_dfa aspace =
      (* 1. Get a fresh DFA *)
      let neg_dfa = DFA.fresh_dfa_empty () in
      (* 2. Copy the alphabet in the output DFA *)
      let alphabet = DFA.get_dfa_alphabet input_dfa in
      (* 2.b Fix the alphabet with additional input letters *)
      let alphabet = DFA.Alphabet.union alphabet aspace in
      (DFA.add_all_letters neg_dfa) alphabet;
      (* 3. Copy the original states in the output DFA *)
      let states = DFA.get_dfa_states input_dfa in
      let special_state = (DFA.StateSet.max_elt states) + 1 in
      let states = DFA.StateSet.add special_state states in
      DFA.StateSet.iter (DFA.add_state neg_dfa) states;
      (* 4. Add the special new state for missing transitions *)
      DFA.add_state neg_dfa special_state;
      (* 5. Set the start state in the output DFA *)
      DFA.set_start_state neg_dfa (DFA.get_dfa_start_state input_dfa);
      (* 6. Set the final states *)
      let final = DFA.get_dfa_final_states input_dfa in
      let not_in_final state = not (DFA.StateSet.mem state final) in
      let (final', _) = DFA.StateSet.partition not_in_final states in
      (* 6.b note that the special state is always final *)
      let final' = DFA.StateSet.add special_state final' in
      (DFA.set_final_states neg_dfa) final';
      (* 7. Add the transitions *)
      (* Note: make sure that missing transitions for a given letter
      are mapped to the special state *)
      let transition_by_a_state input_state =
	(* member test *)
	try
	  let add_one_transition letter =
	    let output_state =
	      try
		let tmap = DFA.get_TransitionMap input_dfa input_state in
		if DFA.TransitionMap.mem letter tmap
		then
		  DFA.TransitionMap.find letter tmap
		else
		  special_state
	      with
		Not_found ->
		  special_state
	    in
	    DFA.add_transitions neg_dfa input_state [(letter,output_state)]
	  in
	  DFA.Alphabet.iter add_one_transition alphabet
	with
	  Not_found ->
	     ()
      in
      DFA.StateSet.iter transition_by_a_state states; neg_dfa

(* Obsolete... additional alphabet is handled like the missing
transitions in the input DFA - Jerome 2005/07/12
    let negates_in_alphabet_space dfa aspace =
      let dfa' = negates dfa in
      let this_aspace = DFA.get_dfa_alphabet dfa' in
      let delta_aspace = DFA.Alphabet.diff aspace this_aspace in
      let add_aux d l =
	let new_state = DFA.StateSet.max_elt (DFA.get_dfa_states dfa') + 1 in
	DFA.add_transitions d (DFA.get_dfa_start_state d) [(l, new_state)];
	DFA.add_transitions d new_state [(l, new_state)];
	DFA.set_final_states d 
	  (DFA.StateSet.add new_state (DFA.get_dfa_final_states d)) 
      in
      DFA.Alphabet.iter (add_aux dfa') delta_aspace; 
      dfa'
*)
     
(* given two sets, return the set of the cross product of the two sets *)
	
    let cross_product statelist1 statelist2 =
      let statelist' = Fsautil.cross_product statelist1 statelist2 in
      let set_of_list l = 
	List.fold_right
	  (NFAPair.StateSet.add) l (NFAPair.StateSet.empty)
      in
      set_of_list statelist'
	
(********************************************************************************)
(* helper function for one_step_walk_aux                                        *)
(* cur_pos is a pair of state, state1 and state2.                               *)
(* the automata2 will take letter2 and it transits from state 2 to a set of    *)
(* destination dest2                                                            *)
(*                                                                              *)
(*                                                                              *)
(* starting at cur_pos                                                          *)
(* examine a particular letter at one automata (letter2)                        *)
(* get a list of states (State * State) should be visited in the next recursion *)
(* append it to the nextlist                                                    *)
(********************************************************************************)
let intersect_by_a_letter nfa cur_pos tmap1 letter2 dest2 nextlist =
  if(NFA.TransitionMap.mem letter2 tmap1)
  then
    begin
      let dest1 = NFA.TransitionMap.find letter2 tmap1 in
      let dest' = (cross_product (NFA.StateSet.elements dest1) (NFA.StateSet.elements dest2)) in
      let dest'' = (NFAPair.StateSet.elements) dest' in
      let letter2_list = Fsautil.list_of_self (List.length dest'') letter2 in
      let arrows = List.combine letter2_list dest'' in
      (* update the nfa *)
      NFAPair.add_transitions nfa cur_pos arrows;  
      nextlist  @ dest'' 
    end
  else
    nextlist

(* helper function for one_step_parellel_walk *)
let one_step_walk_aux a1 a2 pa vlist pair =
  if(not(List.mem pair vlist)) then   (* if we do not visit there before, go ahead *)
    begin
      let (s1, s2) = pair in
      let tmap1 = 
	if (NFA.StateToTransitionMap.mem s1 (NFA.get_nfa_transitions a1)) then
	  NFA.get_TransitionMap a1 s1
	else
	  NFA.TransitionMap.empty
      in
      let tmap2 = 
	if (NFA.StateToTransitionMap.mem s2 (NFA.get_nfa_transitions a2)) then
	  NFA.get_TransitionMap a2 s2
	else
	  NFA.TransitionMap.empty
      in
      (* give us a list of states to be visited in the next recursion *)
      NFA.TransitionMap.fold (intersect_by_a_letter pa pair tmap1) tmap2 []
    end
  else 
    []

(************************************************************************)
(* curlist is the list of states that will visit in this recusrion
step *) (* visited_list is the list of states that we have visited *)
(* it returns a list of states to be visited in the next step *)
(************************************************************************)
let rec one_step_parallel_walk nfa1 nfa2 pnfa visited_list curlist =
  (* in each step, we examine each element in curlist list, try to
     move forward, find the next states to visit *)
  let visit_in_next_step =
    Gmisc.remove_duplicates (List.flatten (List.map (one_step_walk_aux nfa1 nfa2 pnfa visited_list) curlist))
  in
  if ((List.length visit_in_next_step) = 0)
  then () (* fixpoint is reached *)
  else    (* not a fixpoint, take one more step *)
    one_step_parallel_walk nfa1 nfa2 pnfa (Gmisc.remove_duplicates (visited_list @ curlist)) visit_in_next_step 


(*************************************************************************)
(* walk through nfa1 and nfa2 at the same time                           *)
(* Side effect: added the intersect transitions of nfa1 and nfa2 to pnfa *)
(*************************************************************************)
let parallel_walk nfa1 nfa2 pnfa =
  let start1 = NFA.get_nfa_start_state nfa1 in
  let start2 = NFA.get_nfa_start_state nfa2 in
  match (start1, start2) with
  (* do parallel_walk on both automatons until a fixpoint on visited_list is reached *)
  | (Some s1, Some s2) -> one_step_parallel_walk nfa1 nfa2 pnfa [] [(s1,s2)]
  | _ -> raise (Query (Internal_Error "An input NFA does not have a start state"))


(* the toplevel function *)
let intersects (nfa1 : NFA.nfa) (nfa2 : NFA.nfa) =
  let pnfa = NFAPair.fresh_nfa_empty () in

  (* setting the start state *)
  let start1 = NFA.get_nfa_start_state nfa1 and
      start2 = NFA.get_nfa_start_state nfa2 in
  let setstart_aux a s1 s2 =
    begin
      match (start1, start2) with 
      | (Some s1, Some s2) ->
	  (NFAPair.set_start_state) a (s1, s2);
      | _ -> raise (Query (Internal_Error "An input NFA does not have a start state"))
    end
  in
  setstart_aux pnfa start1 start2;

  (* setting the alphabets *)
(*  let alph1 = NFA.get_nfa_alphabet nfa1 and
      alph2 = NFA.get_nfa_alphabet nfa2 in
  NFAPair.add_all_letters pnfa (NFA.Alphabet.inter alph1 alph2); *)

  (* setting the final states *)
  let final1 = NFA.StateSet.elements (NFA.get_nfa_final_states nfa1) and
      final2 = NFA.StateSet.elements (NFA.get_nfa_final_states nfa2) in
  (* constructing th product of the final states *)
  (NFAPair.set_final_states) pnfa (cross_product final1 final2); 

  (* walk through the two automatons, will add states, alphabets, transitions to pnfa as a side effect *)
  parallel_walk nfa1 nfa2 pnfa; 
  pnfa 

let nfa_of_dfa (dfa : DFA.dfa) = 
  let nfa = NFA.fresh_nfa_empty () in
  DFA.StateSet.iter (NFA.add_state nfa) (DFA.get_dfa_states dfa);
  let all_letters = DFA.Alphabet.elements (DFA.get_dfa_alphabet dfa) in
  List.iter (NFA.add_letter nfa) all_letters;
  let start_state = (DFA.get_dfa_start_state dfa) in
  NFA.set_start_state nfa start_state;
  let all_final_states = DFA.StateSet.elements (DFA.get_dfa_final_states dfa) in
  List.iter (NFA.add_final_state nfa) all_final_states;
  
  let transition_by_a_state nfa' dfa' s =
    (* membership test *)
    try
      let tmap = DFA.get_TransitionMap dfa' s in
      let arrows = DFA.TransitionMap.fold (fun letter state ll -> (letter, state)::ll) tmap [] in
      NFA.add_transitions nfa' s arrows
    with
      Not_found -> ()
  in
  let nfa_states_of_dfa_states dfa_states =
    let dfa_states_list = DFA.StateSet.elements dfa_states in
    let nfa_states = NFA.StateSet.empty in
    List.fold_right NFA.StateSet.add dfa_states_list nfa_states
  in
  NFA.StateSet.iter (transition_by_a_state nfa dfa) (nfa_states_of_dfa_states (DFA.get_dfa_states dfa)); nfa

let intersects_dtm (nfa1 : NFA.nfa) (dfa2 : DFA.dfa) =
  intersects nfa1 (nfa_of_dfa dfa2)

(*
let print_pair_int (i, j) =
  print_string "("; print_int i; print_string ", "; print_int j; print_string ")"
*)

let print_intersection print_label print_state_pair i = NFAPair.print_automata i print_label print_state_pair


  end

