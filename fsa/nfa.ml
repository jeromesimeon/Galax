(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: nfa.ml,v 1.4 2007/02/01 22:08:46 simeon Exp $ *)

(* Module Nfa
   Description:
     This module implements Non-deterministic finite States
     Automatas. NFA is a functor parameterized by an letter and a
     state types.
*)

open Format
open Error

module type NFA =
  sig
    type state
    type letter

    module StateSet : Set.S
    module Alphabet : Set.S
    module StateToTransitionMap : Map.S
    module TransitionMap : Map.S

    type nfa = {
        mutable nfa_states : StateSet.t;
        mutable nfa_alphabet : Alphabet.t;
        mutable nfa_start_state : StateSet.elt option;
        mutable nfa_final_states : StateSet.t;
        mutable nfa_transitions :
          StateSet.t TransitionMap.t StateToTransitionMap.t;
      } 

    val nfa_empty : nfa
    val fresh_nfa_empty : unit -> nfa
    val get_nfa_alphabet : nfa -> Alphabet.t
    val add_letter : nfa -> Alphabet.elt -> unit
    val add_all_letters : nfa -> Alphabet.t -> unit
    val get_nfa_states : nfa -> StateSet.t
    val get_nfa_start_state : nfa -> StateSet.elt option
    val get_nfa_final_states : nfa -> StateSet.t
    val add_state : nfa -> StateSet.elt -> unit
    val set_start_state : nfa -> StateSet.elt -> unit
    val set_final_states : nfa -> StateSet.t -> unit
    val add_final_state : nfa -> StateSet.elt -> unit
    val get_nfa_transitions :
        nfa -> StateSet.t TransitionMap.t StateToTransitionMap.t
    val get_TransitionMap :
        nfa -> StateToTransitionMap.key -> StateSet.t TransitionMap.t
    val get_destStateSet : 'a TransitionMap.t -> TransitionMap.key -> 'a
    val get_destStateSet_s :
        nfa -> StateToTransitionMap.key -> TransitionMap.key -> StateSet.t
    val add_transitions_aux :
        nfa ->
          StateToTransitionMap.key -> Alphabet.elt * StateSet.elt -> unit
    val add_transitions :
        nfa -> StateSet.elt -> (Alphabet.elt * StateSet.elt) list -> unit
    val print_automata :
        nfa -> (Alphabet.elt -> unit) -> (StateSet.elt -> unit) -> unit

    val empty : nfa -> bool
  end

module MakeNFA (State : Set.OrderedType) (Letter : Set.OrderedType) =
  struct
    type state = State.t
    type letter = Letter.t

    module StateSet = Set.Make(State)
    module Alphabet = Set.Make(Letter)

    module StateToTransitionMap = Map.Make(State)
    module TransitionMap = Map.Make(Letter) (*key: Letter, data: StateSet.t*)
	
    type nfa =
	{ mutable nfa_states: StateSet.t;
	  mutable nfa_alphabet: Alphabet.t;
	  mutable nfa_start_state: StateSet.elt option;
	  mutable nfa_final_states: StateSet.t;
	  mutable nfa_transitions: StateSet.t TransitionMap.t StateToTransitionMap.t }

    let nfa_empty =
      { nfa_states = StateSet.empty;
	nfa_alphabet = Alphabet.empty;
	nfa_start_state = None;
	nfa_final_states =  StateSet.empty;
	nfa_transitions = StateToTransitionMap.empty }

    (* get a fresh copy of nfa_empty *)
    let fresh_nfa_empty () =
      { nfa_states = StateSet.empty;
	nfa_alphabet = Alphabet.empty;
	nfa_start_state = None;
	nfa_final_states =  StateSet.empty;
	nfa_transitions = StateToTransitionMap.empty }
	
    (* accessor, construction function of alphabet for an automata*)
    let get_nfa_alphabet nfa = nfa.nfa_alphabet

    let add_letter nfa l =
      let letters = nfa.nfa_alphabet in
      nfa.nfa_alphabet <- Alphabet.add l letters

    let add_all_letters nfa ls =
      Alphabet.iter (add_letter nfa) ls

    (* accessor, construction function of state for an automata*)
    let get_nfa_states nfa = nfa.nfa_states
    let get_nfa_start_state nfa = nfa.nfa_start_state
    let get_nfa_final_states nfa = nfa.nfa_final_states

    let add_state nfa x =
      let states = nfa.nfa_states in
      nfa.nfa_states <- StateSet.add x states

    let set_start_state nfa s =
      add_state nfa s;
      nfa.nfa_start_state <- Some s

    let set_final_states nfa ss =
      StateSet.iter (add_state nfa) ss;
      nfa.nfa_final_states <- ss

    let add_final_state nfa s =
      add_state nfa s;
      let fstates = nfa.nfa_final_states in
      nfa.nfa_final_states <- StateSet.add s fstates

    (* accessor, construction function of transition for an automata*)
    let get_nfa_transitions nfa = nfa.nfa_transitions

    let get_TransitionMap nfa start =
      StateToTransitionMap.find start nfa.nfa_transitions

    let get_destStateSet tmap letter = 
      TransitionMap.find letter tmap

    let get_destStateSet_s nfa start letter =
      let tmap = get_TransitionMap nfa start in
      TransitionMap.find letter tmap

    (* val add_transitions_aux :
      nfa -> StateSet.elt -> TransitionMap.key * StateSet.elt -> unit *)  
    let rec add_transitions_aux nfa start arrow =
      let (l, dest) = arrow in 
      add_state nfa dest;
      add_letter nfa l;
      let tmap = get_TransitionMap nfa start in
      let tmap' = 
	if(not(TransitionMap.mem l tmap)) then
	  TransitionMap.add l StateSet.empty tmap
	else
	  tmap
      in
      let ts = 
	if (TransitionMap.mem l tmap') then get_destStateSet tmap' l 
	else StateSet.empty 
      in
      if (StateSet.mem dest ts) 
      then ()
      else 
	nfa.nfa_transitions <- StateToTransitionMap.add start (TransitionMap.add l (StateSet.add dest ts) tmap') nfa.nfa_transitions

     (* val add_transitions :
      nfa -> StateSet.elt -> (TransitionMap.key * StateSet.elt) list -> unit *)
    let add_transitions nfa start arrows =
      add_state nfa start;
      if (not(StateToTransitionMap.mem start nfa.nfa_transitions)) 
      then
	nfa.nfa_transitions <- StateToTransitionMap.add start TransitionMap.empty nfa.nfa_transitions
      else
	(); 
      List.iter (add_transitions_aux nfa start) arrows

     (* operations on nfa *)

    (* this negation function only works on deterministic regular expressions *)
    (*let negates nfa = 
      let states = nfa.nfa_states and
	  final = nfa.nfa_final_states in
      let not_in_final state = not (StateSet.mem state final) in
      let (final', _) = StateSet.partition not_in_final states in
      set_final_states nfa final'; nfa *)

    (* BChoi: print function: good for debugging *)
    let print_automata automata print_alphabet print_state = 
      begin
	printf "%s" "Alphabet: "; 
	Alphabet.iter (fun x -> print_alphabet x; printf " ") (automata.nfa_alphabet);
	printf "%s" "\n";
	flush stdout;
      
	printf "%s" "States: ";
	StateSet.iter (fun x -> print_state x; printf " ") (automata.nfa_states);
	printf "%s" "\n";
	flush stdout;

	printf "%s" "Start state: "; 
	print_state 
	  begin
	    match automata.nfa_start_state with
	    | None -> raise (Query (Automata "Printing an empty automata"))
	    | Some s -> s
	  end;
	printf "%s" "\n";
	flush stdout;

	printf "%s" "Final states: "; 
	StateSet.iter (fun x -> print_state x; printf " ") (automata.nfa_final_states);
	printf "%s" "\n";
	flush stdout;

	printf "%s" "Transitions: \n";
      
	let print_start s = 
	  let tmap = 
	    if (StateToTransitionMap.mem s automata.nfa_transitions) then  
	      StateToTransitionMap.find s automata.nfa_transitions 
	    else 
	      TransitionMap.empty
	  in
	  let print_start_letter_dest s' l' dests = 
	    let print_start_letter_dest_aux s'' l'' t'' =  	  
              (* print the whole thing *)
	      print_state s''; printf "%s" " - "; print_alphabet l''; printf "%s" " -> "; print_state t''; printf "%s" "\n" 
	    in
	    StateSet.iter (print_start_letter_dest_aux s' l') dests
	  in
	  TransitionMap.iter (print_start_letter_dest s) tmap
	in
	StateSet.iter print_start (automata.nfa_states);
	flush stdout
      end

	(*****************)
	(* FSA emptiness *)
	(*****************)

	(* Note:
	   Emptiness of the automata is checked by reachability of some
	   final state.
	   - Jerome
	 *)
	
(* Returns all the states directly reachable from a given state *)
	
    let next_states nfa state =
      let tmap = 
	if (StateToTransitionMap.mem state nfa.nfa_transitions) 
	then get_TransitionMap nfa state
	else TransitionMap.empty
      in
      TransitionMap.fold (fun k d s -> (StateSet.union d s)) tmap StateSet.empty
	
(* Walking function *)

    let rec one_step_walk nfa final_states visited_states current_states =
      (* If there is no more current state, then stops walking and
         returns false *)
      if (StateSet.is_empty current_states)
      then false
      (* If one of the current states is final, then stops walking and
	 returns true *)
      else if not(StateSet.is_empty (StateSet.inter current_states final_states))
      then true
      else
      (* If not, get all the states immediately reachable from any of
	 the current states *)
	let all_next_states =
	  StateSet.fold (fun s y -> StateSet.union (next_states nfa s) y) current_states StateSet.empty
	in
	(* The new visited states are all the old ones, plus current ones *)
	let visited_states' = StateSet.union visited_states current_states in
	(* The new new states are all the immediate next states minus the visited ones *)
	let new_states' = StateSet.diff all_next_states visited_states' in
	one_step_walk nfa final_states visited_states' new_states'
	  
(****************)
(* Reachability *)
(****************)
(* Assumption: each symbol in a regular expression is assigned to a unique position *)

    let reachable nfa =
      let start_state = 
	match (get_nfa_start_state nfa) with
	| None ->
	    raise (Query (Automata "Finding reachability of an empty NFA."))
	| Some start ->
	    start
      and final_states =
	get_nfa_final_states nfa
      in
      (* If there is no final state, then stop walking return false *)
      if StateSet.is_empty final_states then false else
      (* Else perform the reachability test *)
      one_step_walk nfa final_states StateSet.empty (StateSet.singleton start_state)

    let empty nfa = not(reachable nfa)

  end

