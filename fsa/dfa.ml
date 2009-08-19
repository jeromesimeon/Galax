(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dfa.ml,v 1.5 2007/02/01 22:08:46 simeon Exp $ *)

(* Module Dfa
   Description:
     This module implements Deterministic finite States Automatas. DFA
     is a functor parameterized by an letter and a state types.
*)

open Format
open Error

module type DFA =
    sig
      type state
      type letter

      module StateSet : Set.S
      module Alphabet : Set.S
      module StateToTransitionMap : Map.S
      module TransitionMap : Map.S

      type dfa = {
          mutable dfa_states : StateSet.t;
          mutable dfa_alphabet : Alphabet.t;
          mutable dfa_start_state : StateSet.elt option;
          mutable dfa_final_states : StateSet.t;
          mutable dfa_transitions :
            StateSet.elt TransitionMap.t StateToTransitionMap.t;
        } 

      val dfa_empty : dfa
      val fresh_dfa_empty : unit -> dfa
      val get_dfa_alphabet : dfa -> Alphabet.t
      val add_letter : dfa -> Alphabet.elt -> unit
      val add_all_letters : dfa -> Alphabet.t -> unit
      val get_dfa_states : dfa -> StateSet.t
      val get_dfa_start_state : dfa -> StateSet.elt
      val get_dfa_final_states : dfa -> StateSet.t
      val add_state : dfa -> StateSet.elt -> unit
      val set_start_state : dfa -> StateSet.elt -> unit
      val set_final_states : dfa -> StateSet.t -> unit
      val add_final_state : dfa -> StateSet.elt -> unit
      val get_dfa_transitions :
          dfa -> StateSet.elt TransitionMap.t StateToTransitionMap.t
      val get_TransitionMap :
          dfa -> StateToTransitionMap.key -> StateSet.elt TransitionMap.t
      val get_destStateSet :
          StateSet.elt TransitionMap.t -> TransitionMap.key -> StateSet.t
      val get_destStateSet_s :
          dfa -> StateToTransitionMap.key -> TransitionMap.key -> StateSet.t
      val add_transitions_aux :
          dfa ->
            StateToTransitionMap.key -> Alphabet.elt * StateSet.elt -> unit
      val add_transitions :
          dfa -> StateSet.elt -> (Alphabet.elt * StateSet.elt) list -> unit
      val print_automata :
          dfa -> (Alphabet.elt -> unit) -> (StateSet.elt -> unit) -> unit
    end


module MakeDFA (State : Set.OrderedType) (Letter : Set.OrderedType) =
  struct
    type state = State.t
    type letter = Letter.t

    module StateSet = Set.Make(State)
    module Alphabet = Set.Make(Letter)

    module StateToTransitionMap = Map.Make(State)
    module TransitionMap = Map.Make(Letter) (*key: Letter, data: StateSet.t*)
	
    type dfa =
	{ mutable dfa_states: StateSet.t;
	  mutable dfa_alphabet: Alphabet.t;
	  mutable dfa_start_state: StateSet.elt option;
	  mutable dfa_final_states: StateSet.t;
	  mutable dfa_transitions: StateSet.elt TransitionMap.t StateToTransitionMap.t }

    let dfa_empty =
      { dfa_states = StateSet.empty;
	dfa_alphabet = Alphabet.empty;
	dfa_start_state = None;
	dfa_final_states =  StateSet.empty;
	dfa_transitions = StateToTransitionMap.empty }

    (* get a fresh copy of dfa_empty *)
    let fresh_dfa_empty () =
      { dfa_states = StateSet.empty;
	dfa_alphabet = Alphabet.empty;
	dfa_start_state = None;
	dfa_final_states =  StateSet.empty;
	dfa_transitions = StateToTransitionMap.empty }
	
    (* accessor, construction function of alphabet for an automata*)
    let get_dfa_alphabet dfa = dfa.dfa_alphabet

    let add_letter dfa l =
      let letters = dfa.dfa_alphabet in
      dfa.dfa_alphabet <- Alphabet.add l letters

    let add_all_letters dfa ls =
      Alphabet.iter (add_letter dfa) ls

    (* accessor, construction function of state for an automata*)
    let get_dfa_states dfa = dfa.dfa_states
    let get_dfa_start_state dfa =
      match dfa.dfa_start_state with
      | None -> raise (Query (Internal_Error "DFA does not have a start state"))
      | Some s -> s
    let get_dfa_final_states dfa = dfa.dfa_final_states

    let add_state dfa x =
      let states = dfa.dfa_states in
      dfa.dfa_states <- StateSet.add x states

    let set_start_state dfa s =
      add_state dfa s;
      dfa.dfa_start_state <- Some s

    let set_final_states dfa ss =
      StateSet.iter (add_state dfa) ss;
      dfa.dfa_final_states <- ss

    let add_final_state dfa s =
      add_state dfa s;
      let fstates = dfa.dfa_final_states in
      dfa.dfa_final_states <- StateSet.add s fstates

    (* accessor, construction function of transition for an automata*)
    let get_dfa_transitions dfa = dfa.dfa_transitions

    let get_TransitionMap dfa start =
      StateToTransitionMap.find start dfa.dfa_transitions

    let get_destStateSet tmap letter = 
      StateSet.singleton (TransitionMap.find letter tmap)

    let get_destStateSet_s dfa start letter =
      let tmap = get_TransitionMap dfa start in
	StateSet.singleton (TransitionMap.find letter tmap)

    (* val add_transitions_aux :
      dfa -> StateSet.elt -> TransitionMap.key * State.t -> unit *)  
    let rec add_transitions_aux dfa start arrow =
      let (l, dest) = arrow in 
      add_state dfa dest;
      add_letter dfa l;
      let tmap = get_TransitionMap dfa start in
      let tmap' = 
	if(TransitionMap.mem l tmap) then
	  raise (Query (Automata "Constructing a DFA from a non-deterministic regular expression"))
	else 
	  TransitionMap.add l dest tmap in
      dfa.dfa_transitions <- StateToTransitionMap.add start tmap' dfa.dfa_transitions

     (* val add_transitions :
      dfa -> StateSet.elt -> (TransitionMap.key * State.t) list -> unit *)
    let add_transitions dfa start arrows =
      add_state dfa start;
      if (not(StateToTransitionMap.mem start dfa.dfa_transitions)) 
      then
	dfa.dfa_transitions <- StateToTransitionMap.add start TransitionMap.empty dfa.dfa_transitions
      else
	(); 
      List.iter (add_transitions_aux dfa start) arrows

    (* BChoi: print function: good for debugging *)
    let print_automata automata print_alphabet print_state = 
      begin
	printf "%s" "Alphabet: "; 
	Alphabet.iter (fun x -> print_alphabet x; printf "%s" " ") (automata.dfa_alphabet);
	printf "%s" "\n";
	flush stdout;

	printf "%s" "States: ";
	StateSet.iter (fun x -> print_state x; printf "%s" " ") (automata.dfa_states);
	printf "%s" "\n";
	flush stdout;

	printf "%s" "Start state: "; 
	print_state 
	  begin
	    match automata.dfa_start_state with
	    | None -> raise (Query (Automata "Printing an empty automata"))
	    | Some s -> s
	  end;
	printf "%s" "\n";
	flush stdout;

	printf "%s" "Final states: "; 
	StateSet.iter (fun x -> print_state x; printf "%s" " ") (automata.dfa_final_states);
	printf "%s" "\n";
	flush stdout;

	printf "%s" "Transitions: \n";
      
	let print_start s = 
	  let tmap = 
	    if (StateToTransitionMap.mem s automata.dfa_transitions) then  
	      StateToTransitionMap.find s automata.dfa_transitions 
	    else 
	      TransitionMap.empty
	  in
	  let print_start_letter_dest s' l' dest = 
	    print_state s'; printf "%s" " - "; print_alphabet l'; printf "%s" " -> "; print_state dest; printf "%s" "\n" 
	  in
	  TransitionMap.iter (print_start_letter_dest s) tmap
	in
	StateSet.iter print_start (automata.dfa_states);
	flush stdout
      end

  end


