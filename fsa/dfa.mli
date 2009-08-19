(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dfa.mli,v 1.5 2007/02/01 22:08:46 simeon Exp $ *)

(* Module Dfa
   Description:
     This module implements Deterministic finite States Automatas. DFA
     is a functor parameterized by an letter and a state types.
*)

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

module MakeDFA (State : Set.OrderedType) (Letter : Set.OrderedType) :
    DFA with
       type state = State.t
       and type letter = Letter.t
       and type StateSet.elt = State.t
       and type Alphabet.elt = Letter.t
       and type StateToTransitionMap.key = State.t
       and type TransitionMap.key = Letter.t

